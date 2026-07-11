#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include <vector>
#include <limits>
#include "Vector.H"
#include "computation/operation.H"
#include "util/myexception.H"
#include "util/utf8.H"
#include "computation/machine/graph_register.H"
#include "computation/machine/args.H"

using std::vector;

namespace
{
    // Keep registers discovered by dependent reads rooted until a native
    // operation has installed them in its returned boxed Vector.
    class stacked_register_roots
    {
        OperationArgs& args_;
        std::size_t count_ = 0;

    public:
        explicit stacked_register_roots(OperationArgs& args): args_(args) {}

        // Push one discovered register onto the evaluator's GC-root stack.
        void add(int reg)
        {
            args_.stack_push(reg);
            count_++;
        }

        // Remove the temporary roots in stack order on normal and exceptional
        // exits; all nested evaluator stack entries have already been removed.
        ~stacked_register_roots()
        {
            while(count_ > 0)
            {
                args_.stack_pop();
                count_--;
            }
        }
    };

    // Construct a boxed Vector whose elements refer to the supplied machine
    // registers without forcing the values stored in those registers.
    closure make_boxed_vector(closure::Env_t registers)
    {
        if (registers.size() > static_cast<std::size_t>(std::numeric_limits<int>::max()))
            throw myexception()<<"Boxed Vector length exceeds the Int range";
        int length = static_cast<int>(registers.size());
        std::vector<Runtime::Exp> elements(length);
        for(int i = 0; i < length; i++)
            elements[i] = Runtime::IndexVar(length - 1 - i);

        closure result;
        result.Env = std::move(registers);
        result.set_code(Runtime::ConstructorApp("Vector", length,
                                                std::move(elements)));
        return result;
    }

    bool is_clist_nil(const R::Exp& xs)
    {
        // c_nil is represented by an integer sentinel, and the legacy walker
        // stopped on any integer.
        return xs.is_int();
    }

    R::Exp clist_first(const R::Exp& xs)
    {
        auto pair = xs.to<R::RPair>();
        assert(pair);
        return pair->first;
    }

    R::Exp clist_second(const R::Exp& xs)
    {
        auto pair = xs.to<R::RPair>();
        assert(pair);
        return pair->second;
    }
}

// Generate a lazy boxed Vector by retaining one application of the supplied
// function for every index from zero to the requested length minus one.
extern "C" closure builtin_function_boxedGenerate(OperationArgs& Args)
{
    int length = Args.evaluate_slot_to_value(0).as_int();
    if (length < 0)
        throw myexception()<<"Data.Vector.generate: negative length "<<length;

    int function_reg = Args.reg_for_slot(1);
    closure::Env_t elements;
    elements.reserve(length);
    for(int i = 0; i < length; i++)
    {
        int index_reg = Args.allocate(i);
        int element_reg = Args.allocate(
            closure(Runtime::apply(Runtime::IndexVar(1), {Runtime::IndexVar(0)}),
                    {function_reg, index_reg}));
        elements.push_back(element_reg);
    }
    return make_boxed_vector(std::move(elements));
}

// Replicate one lazy element register directly, avoiding per-index function
// applications while retaining sharing between every result position.
extern "C" closure builtin_function_boxedReplicate(OperationArgs& Args)
{
    int length = Args.evaluate_slot_to_value(0).as_int();
    if (length < 0)
        throw myexception()<<"Data.Vector.replicate: negative length "<<length;
    if (length == 0)
        return make_boxed_vector(closure::Env_t{});

    int value_reg = Args.reg_for_slot(1);
    return make_boxed_vector(closure::Env_t(length, value_reg));
}

// Traverse a complete Haskell list spine and retain each lazy head register in
// a boxed Vector, without recursively indexing the original list.
extern "C" closure builtin_function_boxedFromList(OperationArgs& Args)
{
    int xs = Args.evaluate_slot_use(0);
    closure::Env_t elements;
    stacked_register_roots roots(Args);

    while(true)
    {
        const closure& xs_closure = Args.memory().closure_at(xs);
        auto list_cell = xs_closure.get_code().to<Runtime::ConstructorApp>();
        if (not list_cell)
            throw myexception()<<"Data.Vector.fromList: expected a list constructor, but got "
                               <<xs_closure.get_code().print();

        const auto& tag = list_cell->head;
        if (tag.name() == "[]" and tag.n_args() == 0 and
            list_cell->args.empty())
            return make_boxed_vector(std::move(elements));
        if (tag.name() != ":" or tag.n_args() != 2 or
            list_cell->args.size() != 2)
            throw myexception()<<"Data.Vector.fromList: expected ':' or '[]', but got "
                               <<tag.print();
        if (elements.size() == static_cast<std::size_t>(std::numeric_limits<int>::max()))
            throw myexception()<<"Data.Vector.fromList: length exceeds the Int range";

        auto head_reg = xs_closure.reg_for_code(list_cell->args[0]);
        auto tail_reg = xs_closure.reg_for_code(list_cell->args[1]);
        if (not head_reg or not tail_reg)
            throw myexception()<<"Data.Vector.fromList: malformed ':' constructor";

        // Capture both registers before evaluating the tail: evaluation may
        // grow the machine heap and invalidate the xs_closure reference.
        roots.add(*head_reg);
        elements.push_back(*head_reg);
        xs = Args.evaluate_reg_dependent_use(*tail_reg);
    }
}

// Fill exactly the requested number of boxed Vector slots from a Haskell
// list, retaining one shared default register for any unfilled suffix.
extern "C" closure builtin_function_boxedFromListNDefault(OperationArgs& Args)
{
    int length = Args.evaluate_slot_to_value(0).as_int();
    if (length < 0)
        throw myexception()<<"Data.Vector.Internal.fromListNDefault: negative length "
                           <<length;

    // A zero-length result must not inspect the list argument at all.
    if (length == 0)
        return make_boxed_vector(closure::Env_t{});

    int default_reg = Args.reg_for_slot(1);
    closure::Env_t elements(length, default_reg);
    stacked_register_roots roots(Args);
    int xs = Args.evaluate_slot_use(2);

    for(int index = 0; index < length; index++)
    {
        const closure& xs_closure = Args.memory().closure_at(xs);
        auto list_cell = xs_closure.get_code().to<Runtime::ConstructorApp>();
        if (not list_cell)
            throw myexception()<<"Data.Vector.Internal.fromListNDefault: expected a list "
                               <<"constructor, but got "
                               <<xs_closure.get_code().print();

        const auto& tag = list_cell->head;
        if (tag.name() == "[]" and tag.n_args() == 0 and
            list_cell->args.empty())
            return make_boxed_vector(std::move(elements));
        if (tag.name() != ":" or tag.n_args() != 2 or
            list_cell->args.size() != 2)
            throw myexception()<<"Data.Vector.Internal.fromListNDefault: expected ':' or "
                               <<"'[]', but got "<<tag.print();

        auto head_reg = xs_closure.reg_for_code(list_cell->args[0]);
        auto tail_reg = xs_closure.reg_for_code(list_cell->args[1]);
        if (not head_reg or not tail_reg)
            throw myexception()<<"Data.Vector.Internal.fromListNDefault: malformed ':' "
                               <<"constructor";

        roots.add(*head_reg);
        elements[index] = *head_reg;

        // Do not inspect the tail after filling the final result slot.
        if (index + 1 == length)
            return make_boxed_vector(std::move(elements));

        xs = Args.evaluate_reg_dependent_use(*tail_reg);
    }

    return make_boxed_vector(std::move(elements));
}

// Build a boxed Vector from linear (Int,value) associations, retaining lazy
// value registers and letting each later duplicate overwrite the earlier one.
extern "C" closure builtin_function_boxedFromIndexedList(OperationArgs& Args)
{
    int length = Args.evaluate_slot_to_value(0).as_int();
    if (length < 0)
        throw myexception()<<"Data.Vector.Internal.fromIndexedList: negative length "
                           <<length;

    int default_reg = Args.reg_for_slot(1);
    closure::Env_t elements(length, default_reg);
    stacked_register_roots roots(Args);
    int xs = Args.evaluate_slot_use(2);

    while(true)
    {
        const closure& xs_closure = Args.memory().closure_at(xs);
        auto list_cell = xs_closure.get_code().to<Runtime::ConstructorApp>();
        if (not list_cell)
            throw myexception()<<"Data.Vector.Internal.fromIndexedList: expected a list "
                               <<"constructor, but got "
                               <<xs_closure.get_code().print();

        const auto& list_tag = list_cell->head;
        if (list_tag.name() == "[]" and list_tag.n_args() == 0 and
            list_cell->args.empty())
            return make_boxed_vector(std::move(elements));
        if (list_tag.name() != ":" or list_tag.n_args() != 2 or
            list_cell->args.size() != 2)
            throw myexception()<<"Data.Vector.Internal.fromIndexedList: expected ':' or "
                               <<"'[]', but got "<<list_tag.print();

        auto association_reg = xs_closure.reg_for_code(list_cell->args[0]);
        auto tail_reg = xs_closure.reg_for_code(list_cell->args[1]);
        if (not association_reg or not tail_reg)
            throw myexception()<<"Data.Vector.Internal.fromIndexedList: malformed ':' "
                               <<"constructor";

        // Capture the list registers before evaluating the association, since
        // that evaluation can invalidate the xs_closure reference.
        int association = Args.evaluate_reg_dependent_use(*association_reg);
        const closure& association_closure = Args.memory().closure_at(association);
        auto pair = association_closure.get_code().to<Runtime::ConstructorApp>();
        if (not pair or pair->head.name() != "(,)" or
            pair->head.n_args() != 2 or pair->args.size() != 2)
            throw myexception()<<"Data.Vector.Internal.fromIndexedList: expected an "
                               <<"(Int,value) pair";

        auto index_reg = association_closure.reg_for_code(pair->args[0]);
        auto value_reg = association_closure.reg_for_code(pair->args[1]);
        if (not index_reg or not value_reg)
            throw myexception()<<"Data.Vector.Internal.fromIndexedList: malformed pair "
                               <<"constructor";

        // Root the lazy value before evaluating the index; evaluating it here
        // would make array construction unnecessarily strict in elements.
        roots.add(*value_reg);
        int evaluated_index = Args.evaluate_reg_dependent_use(*index_reg);
        const auto& index_value = Args.memory().closure_at(evaluated_index).get_code();
        if (not index_value.is_int())
            throw myexception()<<"Data.Vector.Internal.fromIndexedList: association index "
                               <<"is not an Int";
        int index = index_value.as_int();
        if (index < 0 or index >= length)
            throw myexception()<<"Data.Vector.Internal.fromIndexedList: index "<<index
                               <<" is outside vector length "<<length;

        elements[index] = *value_reg;
        xs = Args.evaluate_reg_dependent_use(*tail_reg);
    }
}

// Copy a boxed Vector once and replace indexed slots from a complete Haskell
// association list, retaining each replacement value as an unevaluated reg.
extern "C" closure builtin_function_boxedReplaceIndexed(OperationArgs& Args)
{
    const closure& base = Args.evaluate_reg_to_closure(Args.reg_for_slot(0));
    const auto& base_elements = boxed_vector_element_regs(base);
    int length = static_cast<int>(base_elements.size());
    closure::Env_t elements(base_elements.begin(), base_elements.end());

    // The copied base registers are local C++ state, so root each one before
    // evaluating the independently supplied association list.
    stacked_register_roots roots(Args);
    for(int reg: elements)
        roots.add(reg);

    int xs = Args.evaluate_slot_use(1);
    while(true)
    {
        const closure& xs_closure = Args.memory().closure_at(xs);
        auto list_cell = xs_closure.get_code().to<Runtime::ConstructorApp>();
        if (not list_cell)
            throw myexception()<<"Data.Vector.Internal.replaceIndexed: expected a list "
                               <<"constructor, but got "
                               <<xs_closure.get_code().print();

        const auto& list_tag = list_cell->head;
        if (list_tag.name() == "[]" and list_tag.n_args() == 0 and
            list_cell->args.empty())
            return make_boxed_vector(std::move(elements));
        if (list_tag.name() != ":" or list_tag.n_args() != 2 or
            list_cell->args.size() != 2)
            throw myexception()<<"Data.Vector.Internal.replaceIndexed: expected ':' or "
                               <<"'[]', but got "<<list_tag.print();

        auto association_reg = xs_closure.reg_for_code(list_cell->args[0]);
        auto tail_reg = xs_closure.reg_for_code(list_cell->args[1]);
        if (not association_reg or not tail_reg)
            throw myexception()<<"Data.Vector.Internal.replaceIndexed: malformed ':' "
                               <<"constructor";

        // Capture the list registers before evaluating the association, since
        // that evaluation can invalidate the xs_closure reference.  Keep the
        // tail rooted through index evaluation before following it.
        roots.add(*tail_reg);
        int association = Args.evaluate_reg_dependent_use(*association_reg);
        const closure& association_closure = Args.memory().closure_at(association);
        auto pair = association_closure.get_code().to<Runtime::ConstructorApp>();
        if (not pair or pair->head.name() != "(,)" or
            pair->head.n_args() != 2 or pair->args.size() != 2)
            throw myexception()<<"Data.Vector.Internal.replaceIndexed: expected an "
                               <<"(Int,value) pair";

        auto index_reg = association_closure.reg_for_code(pair->args[0]);
        auto value_reg = association_closure.reg_for_code(pair->args[1]);
        if (not index_reg or not value_reg)
            throw myexception()<<"Data.Vector.Internal.replaceIndexed: malformed pair "
                               <<"constructor";

        // Keep the lazy replacement alive while evaluating its index and all
        // later associations, without evaluating the replacement itself.
        roots.add(*value_reg);
        int evaluated_index = Args.evaluate_reg_dependent_use(*index_reg);
        const auto& index_value = Args.memory().closure_at(evaluated_index).get_code();
        if (not index_value.is_int())
            throw myexception()<<"Data.Vector.Internal.replaceIndexed: association index "
                               <<"is not an Int";
        int index = index_value.as_int();
        if (index < 0 or index >= length)
            throw myexception()<<"Data.Vector.Internal.replaceIndexed: index "<<index
                               <<" is outside vector length "<<length;

        elements[index] = *value_reg;
        xs = Args.evaluate_reg_dependent_use(*tail_reg);
    }
}

// Copy a boxed Vector and accumulate indexed associations in source order,
// forcing every combine result before it can feed a later duplicate index.
extern "C" closure builtin_function_boxedAccumIndexed(OperationArgs& Args)
{
    int combine_reg = Args.reg_for_slot(0);
    const closure& base = Args.evaluate_reg_to_closure(Args.reg_for_slot(1));
    const auto& base_elements = boxed_vector_element_regs(base);
    int length = static_cast<int>(base_elements.size());
    closure::Env_t elements(base_elements.begin(), base_elements.end());

    // Root the copied base registers before association and combine evaluation
    // can grow or collect the machine heap.
    stacked_register_roots roots(Args);
    for(int reg: elements)
        roots.add(reg);

    int xs = Args.evaluate_slot_use(2);
    while(true)
    {
        const closure& xs_closure = Args.memory().closure_at(xs);
        auto list_cell = xs_closure.get_code().to<Runtime::ConstructorApp>();
        if (not list_cell)
            throw myexception()<<"Data.Vector.Internal.accumIndexed: expected a list "
                               <<"constructor, but got "
                               <<xs_closure.get_code().print();

        const auto& list_tag = list_cell->head;
        if (list_tag.name() == "[]" and list_tag.n_args() == 0 and
            list_cell->args.empty())
            return make_boxed_vector(std::move(elements));
        if (list_tag.name() != ":" or list_tag.n_args() != 2 or
            list_cell->args.size() != 2)
            throw myexception()<<"Data.Vector.Internal.accumIndexed: expected ':' or "
                               <<"'[]', but got "<<list_tag.print();

        auto association_reg = xs_closure.reg_for_code(list_cell->args[0]);
        auto tail_reg = xs_closure.reg_for_code(list_cell->args[1]);
        if (not association_reg or not tail_reg)
            throw myexception()<<"Data.Vector.Internal.accumIndexed: malformed ':' "
                               <<"constructor";

        // Capture the list registers before evaluating the association, since
        // that evaluation can invalidate the xs_closure reference.  Keep the
        // tail rooted through index and combine evaluation before following it.
        roots.add(*tail_reg);
        int association = Args.evaluate_reg_dependent_use(*association_reg);
        const closure& association_closure = Args.memory().closure_at(association);
        auto pair = association_closure.get_code().to<Runtime::ConstructorApp>();
        if (not pair or pair->head.name() != "(,)" or
            pair->head.n_args() != 2 or pair->args.size() != 2)
            throw myexception()<<"Data.Vector.Internal.accumIndexed: expected an "
                               <<"(Int,value) pair";

        auto index_reg = association_closure.reg_for_code(pair->args[0]);
        auto value_reg = association_closure.reg_for_code(pair->args[1]);
        if (not index_reg or not value_reg)
            throw myexception()<<"Data.Vector.Internal.accumIndexed: malformed pair "
                               <<"constructor";

        // Root the new value without forcing it before evaluating the index.
        // The combine function determines how much of old and new it needs.
        roots.add(*value_reg);
        int evaluated_index = Args.evaluate_reg_dependent_use(*index_reg);
        const auto& index_value = Args.memory().closure_at(evaluated_index).get_code();
        if (not index_value.is_int())
            throw myexception()<<"Data.Vector.Internal.accumIndexed: association index "
                               <<"is not an Int";
        int index = index_value.as_int();
        if (index < 0 or index >= length)
            throw myexception()<<"Data.Vector.Internal.accumIndexed: index "<<index
                               <<" is outside vector length "<<length;

        int old_reg = elements[index];
        // Args.allocate keeps the application as a temporary machine root for
        // the remainder of this native operation.
        int apply_reg = Args.allocate(
            closure(Runtime::apply(Runtime::IndexVar(2),
                                   {Runtime::IndexVar(1), Runtime::IndexVar(0)}),
                    {combine_reg, old_reg, *value_reg}));

        // Keep the application register, rather than only the evaluator's
        // result register, so its creator step stays reachable.  Forcing now
        // gives accum and accumArray their per-association WHNF strictness.
        Args.evaluate_reg_force(apply_reg);
        elements[index] = apply_reg;
        xs = Args.evaluate_reg_dependent_use(*tail_reg);
    }
}

// Return the number of lazy element registers stored by a boxed Vector.
extern "C" closure builtin_function_boxedLength(OperationArgs& Args)
{
    const closure& value = Args.evaluate_reg_to_closure(Args.reg_for_slot(0));
    return {static_cast<int>(boxed_vector_element_regs(value).size())};
}

// Return one lazy boxed Vector element after validating its zero-based index.
extern "C" closure builtin_function_boxedIndex(OperationArgs& Args)
{
    extern long total_index_op;
    total_index_op++;

    int index = Args.evaluate_slot_to_value(1).as_int();
    const closure& value = Args.evaluate_reg_to_closure(Args.reg_for_slot(0));
    const auto& elements = boxed_vector_element_regs(value);
    int length = static_cast<int>(elements.size());
    if (index < 0 or index >= length)
        throw myexception()<<"Data.Vector.!: index "<<index
                           <<" is outside vector length "<<length;
    return closure(Runtime::IndexVar(0), {elements[index]});
}

// Copy a contiguous range of lazy element registers into a new boxed Vector.
extern "C" closure builtin_function_boxedSlice(OperationArgs& Args)
{
    int start = Args.evaluate_slot_to_value(0).as_int();
    int count = Args.evaluate_slot_to_value(1).as_int();
    int value_reg = Args.evaluate_slot_use(2);
    const closure& value = Args.memory().closure_at(value_reg);
    const auto& value_elements = boxed_vector_element_regs(value);
    int length = static_cast<int>(value_elements.size());
    if (start < 0 or count < 0 or start > length or count > length - start)
        throw myexception()<<"Data.Vector.slice: invalid slice ("<<start<<","<<count
                           <<") for vector length "<<length;
    if (start == 0 and count == length)
        return closure(Runtime::IndexVar(0), {value_reg});

    closure::Env_t elements(value_elements.begin() + start,
                            value_elements.begin() + start + count);
    return make_boxed_vector(std::move(elements));
}

// Concatenate two boxed Vectors by copying their lazy element register
// references into one result environment.
extern "C" closure builtin_function_boxedAppend(OperationArgs& Args)
{
    int left_reg = Args.evaluate_slot_use(0);
    std::size_t left_size = boxed_vector_element_regs(
        Args.memory().closure_at(left_reg)).size();

    int right_reg = Args.evaluate_slot_use(1);
    std::size_t right_size = boxed_vector_element_regs(
        Args.memory().closure_at(right_reg)).size();

    if (right_size > static_cast<std::size_t>(std::numeric_limits<int>::max()) - left_size)
        throw myexception()<<"Data.Vector.++: result length exceeds the Int range";
    if (left_size == 0)
        return closure(Runtime::IndexVar(0), {right_reg});
    if (right_size == 0)
        return closure(Runtime::IndexVar(0), {left_reg});

    // Reacquire both closures after the final evaluator call so heap growth
    // cannot invalidate the references used while copying their environments.
    const auto& left = boxed_vector_element_regs(
        Args.memory().closure_at(left_reg));
    const auto& right = boxed_vector_element_regs(
        Args.memory().closure_at(right_reg));

    closure::Env_t elements;
    elements.reserve(left_size + right_size);
    elements.insert(elements.end(), left.begin(), left.end());
    elements.insert(elements.end(), right.begin(), right.end());
    return make_boxed_vector(std::move(elements));
}

int vector_value_size(const R::Exp& value)
{
    if (auto v = value.to<R::RVector>())
        return v->size();
    else
        return value.as_<R::RVector>().size();
}

R::Exp vector_value_at(const R::Exp& value, int index)
{
    if (auto v = value.to<R::RVector>())
        return (*v)[index];
    else
        return value.as_<R::RVector>()[index];
}

extern "C" R::Exp simple_function_sizeOfString(vector<R::Exp>& args)
{
    return (int)get_arg(args).as_string().size();
}

extern "C" R::Exp simple_function_getStringElement(vector<R::Exp>& args)
{
    auto arg0 = get_arg(args);
    const std::string& s = arg0.as_string();
    int i = get_arg(args).as_int();

    // Compatibility/raw-byte helper.  Text callers should use
    // decodeUtf8CharAt rather than interpreting this byte as a Char.
    return static_cast<char32_t>(static_cast<unsigned char>(s[i]));
}

// Decode one UTF-8 scalar from a CPPString and return the scalar plus the next
// byte offset, so Haskell can lazily unpack a String without bulk allocation.
extern "C" R::Exp simple_function_decodeUtf8CharAt(vector<R::Exp>& args)
{
    auto arg0 = get_arg(args);
    const std::string& s = arg0.as_string();
    int offset = get_arg(args).as_int();

    if (offset < 0)
        throw myexception()<<"decodeUtf8CharAt: negative byte offset "<<offset;

    auto decoded = utf8::decode_next(s, static_cast<std::size_t>(offset));
    if (not decoded)
        throw myexception()<<"decodeUtf8CharAt: invalid UTF-8 at byte offset "<<offset;

    if (decoded->next_byte > static_cast<std::size_t>(std::numeric_limits<int>::max()))
        throw myexception()<<"decodeUtf8CharAt: next byte offset exceeds Int range.";

    return R::RPair(decoded->code_point, static_cast<int>(decoded->next_byte));
}

// Return a byte-indexed CPPString slice, preserving the original register for
// full-string slices and rejecting invalid Haskell Int offsets explicitly.
extern "C" closure builtin_function_cppSubString(OperationArgs& Args)
{
    int offset = Args.evaluate_slot_to_value(1).as_int();
    int length = Args.evaluate_slot_to_value(2).as_int();
    std::string s = Args.evaluate_slot_to_value(0).as_string();

    if (offset < 0)
        throw myexception()<<"cppSubString: negative offset "<<offset<<".";

    if (length < 0)
        throw myexception()<<"cppSubString: negative length "<<length<<".";

    auto start = static_cast<std::size_t>(offset);
    auto n = static_cast<std::size_t>(length);

    if (start > s.size())
        throw myexception()<<"cppSubString: offset "<<offset<<" is past string size "<<s.size()<<".";

    if (n > s.size() - start)
        throw myexception()<<"cppSubString: length "<<length<<" exceeds string size "<<s.size()<<" at offset "<<offset<<".";

    if (start == 0 and n == s.size())
	return {R::IndexVar(0),{Args.reg_for_slot(0)}};
    else
	return {s.substr(start,n)};
}

extern "C" R::Exp simple_function_vector_size(vector<R::Exp>& args)
{
    auto arg0 = get_arg(args);
    return vector_value_size(arg0);
}

extern "C" closure builtin_function_set_vector_index(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    int i = Args.evaluate_slot_to_value(1).as_int();
    auto x = Args.evaluate_slot_to_value(2);

    if (auto v = arg0.to<R::RVector>())
    {
        R::RVector* vv = const_cast<R::RVector*>(v);
        (*vv)[i] = std::move(x);
    }
    else
    {
        const R::RVector& legacy_vector = arg0.as_<R::RVector>();
        const R::RVector* vv = &legacy_vector;
        R::RVector* vvv = const_cast<R::RVector*>(vv);
        (*vvv)[i] = std::move(x);
    }

    return closure(R::ConstructorApp("()", 0, {}));
}

extern "C" R::Exp simple_function_get_vector_index(vector<R::Exp>& args)
{
    auto arg0 = get_arg(args);
    int i = get_arg(args).as_int();
    return vector_value_at(arg0, i);
}

extern "C" closure builtin_function_clist_to_vector(OperationArgs& Args)
{
    R::Exp xs = Args.evaluate_slot_to_value(0);

    object_ptr<R::RVector> v (new R::RVector);

    for(; not is_clist_nil(xs); xs = clist_second(xs))
        v->push_back(clist_first(xs));

    return v;
}

// Convert a Haskell list directly to an EVector, recording dynamic USE edges
// for list cells and values discovered while walking the spine.
extern "C" closure builtin_function_list_to_vector(OperationArgs& Args)
{
    object_ptr<R::RVector> v (new R::RVector);

    int xs = Args.evaluate_slot_use(0);

    while(true)
    {
        const closure& xs_closure = Args.memory().closure_at(xs);
        auto list_cell = xs_closure.get_code().to<Runtime::ConstructorApp>();
        if (not list_cell)
            throw myexception()<<"list_to_vector: expected a list constructor, but got "
                               <<xs_closure.get_code().print();

        const auto& tag = list_cell->head;
        if (tag.name() == "[]" and tag.n_args() == 0)
            return v;

        if (tag.name() != ":" or tag.n_args() != 2)
            throw myexception()<<"list_to_vector: expected ':' or '[]', but got "
                               <<tag.print();

        int x = xs_closure.reg_for_constructor_slot(0);
        int xs_tail = xs_closure.reg_for_constructor_slot(1);

        int value_reg = Args.evaluate_reg_dependent_use(x);
        v->push_back(Args.memory().closure_at(value_reg).get_code());

        xs = Args.evaluate_reg_dependent_use(xs_tail);
    }
}

// Convert a Haskell [Char] directly to a CPPString, recording dynamic USE edges
// for list cells and character values discovered while walking the spine.
extern "C" closure builtin_function_list_to_string(OperationArgs& Args)
{
    std::string s;

    int xs = Args.evaluate_slot_use(0);

    while(true)
    {
        const closure& xs_closure = Args.memory().closure_at(xs);
        auto list_cell = xs_closure.get_code().to<Runtime::ConstructorApp>();
        if (not list_cell)
            throw myexception()<<"list_to_string: expected a list constructor, but got "
                               <<xs_closure.get_code().print();

        const auto& tag = list_cell->head;
        if (tag.name() == "[]" and tag.n_args() == 0)
            return s;

        if (tag.name() != ":" or tag.n_args() != 2)
            throw myexception()<<"list_to_string: expected ':' or '[]', but got "
                               <<tag.print();

        int c = xs_closure.reg_for_constructor_slot(0);
        int xs_tail = xs_closure.reg_for_constructor_slot(1);

        int value_reg = Args.evaluate_reg_dependent_use(c);
        s += utf8::encode(Args.memory().closure_at(value_reg).get_code().as_char());

        xs = Args.evaluate_reg_dependent_use(xs_tail);
    }
}

extern "C" closure builtin_function_emptyString(OperationArgs& /*Args*/)
{
    std::string s;

    return s;
}

extern "C" closure builtin_function_showObject(OperationArgs& Args)
{
    auto arg = Args.evaluate_slot_to_value(0);
    return arg.print();
}
