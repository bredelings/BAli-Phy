#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include <vector>
#include <limits>
#include "computation/operation.H"
#include "util/myexception.H"
#include "util/matrix.H"
#include "util/utf8.H"
#include "computation/machine/graph_register.H"
#include "computation/machine/args.H"

using boost::dynamic_pointer_cast;
using std::vector;

namespace
{
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

extern "C" closure builtin_function_clist_to_string(OperationArgs& Args)
{
    R::Exp xs = Args.evaluate_slot_to_value(0);

    std::string s;

    for(; not is_clist_nil(xs); xs = clist_second(xs))
        s += utf8::encode(clist_first(xs).as_char());

    return s;
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

extern "C" closure builtin_function_fromVectors(OperationArgs& Args)
{
    // This doesn't distinguish between a 0x0, 2x0 or 2x0 matrix.

    // If I really want something like the Haskell matrix, I could use an RVector of RVectors.
    // Then I could get a matrix of anything -- integers, doubles, log_doubles, etc.

    auto arg = Args.evaluate_slot_to_value(0);
    auto& V = arg.as_<R::RVector>();
    int I = V.size();
    if (I <= 0)
        return Box<Matrix>();

    int J = V[0].as_<R::RVector>().size();
    if (J <= 0)
        return Box<Matrix>();

    auto M = new Box<Matrix>(I,J);
    for(int i=0;i<I;i++)
        for(int j=0;j<J;j++)
            (*M)(i,j) = V[i].as_<R::RVector>()[j].as_double();

    return M;
}

extern "C" closure builtin_function_matrixToVector(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& M = arg0.as_<Box<Matrix>>();

    object_ptr<R::RVector> Vptr = new R::RVector;
    auto& V = *Vptr;

    for(int i=0;i<M.size1();i++)
        for(int j=0;j<M.size2();j++)
            V.push_back(M(i,j));

    return V;
}

extern "C" closure builtin_function_vectorToMatrix(OperationArgs& Args)
{
    int s1 = Args.evaluate_slot_to_value(0).as_int();
    int s2 = Args.evaluate_slot_to_value(1).as_int();
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto& V = arg2.as_<R::RVector>();

    if (V.size() != s1*s2)
        throw myexception()<<"vectorToMatrix: size = ("<<s1<<", "<<s2<<") so expected "<<s1*s2<<" elements, but got "<<V.size()<<"!";

    auto Mptr = new Box<Matrix>(s1, s2);
    auto& M = *Mptr;

    int k=0;
    for(int i=0;i<s1;i++)
        for(int j=0;j<s2;j++)
            M(i,j) = V[k++].as_double();

    return Mptr;
}
