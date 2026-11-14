#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "builtins/haskell-list-input.hh"
#include "computation/machine/args.hh"
#include "computation/haskell/ids.hh"
#include "dp/2way.hh"
#include "alignment/alignment.hh"
#include "util/myexception.hh"

#include <boost/dynamic_bitset.hpp>
#include <limits>
#include <bit>
#include <cstdint>
#include "computation/haskell/Integer.hh"
#include <vector>

using std::vector;

typedef Box<boost::dynamic_bitset<>> bitvector;

namespace
{

// These operations are commutative, so copy the longer owner, truncate it to
// the common prefix, and combine it with the shorter owner.  This preserves the
// upstream truncation rule while copying only the returned bit vector.
template <typename Apply>
closure truncated_commutative_bitwise(OperationArgs& Args, Apply apply)
{
    auto left_arg = Args.evaluate_slot_to_value(0);
    auto right_arg = Args.evaluate_slot_to_value(1);
    const auto& left = left_arg.as_<bitvector>();
    const auto& right = right_arg.as_<bitvector>();
    bool copy_left = left.size() >= right.size();
    const auto& result_source = copy_left ? left : right;
    const auto& shorter = copy_left ? right : left;
    auto result = result_source;
    result.resize(shorter.size());
    apply(result, shorter);
    return result;
}

// Decode one U.Vector Bit constructor and retain its exact packed owner.
// Bit slices already normalize to offset zero, so the cached length must equal the owner size.
object_ptr<const bitvector> read_unboxed_bit_vector(OperationArgs& Args, int vector_reg,
                                                    EdgeContingency contingency)
{
    auto vector = Args.evaluate_reg_use_with_contingency(vector_reg, contingency);
    const closure& vector_closure = Args.memory().closure_at(vector.value_reg);
    auto constructor = vector_closure.get_code().to<Runtime::ConstructorApp>();
    if (not constructor or get_unqualified_name(constructor->head.name()) != "V_Bit" or
        constructor->head.n_args() != 2 or constructor->args.size() != 2)
        throw myexception()<<"Data.Vector.Unboxed.concat: expected V_Bit, but got "
                           <<vector_closure.get_code().print();

    int length_reg = vector_closure.reg_for_constructor_slot(0);
    int owner_reg = vector_closure.reg_for_constructor_slot(1);
    int length_value_reg = Args.evaluate_reg_use(length_reg, vector.edge_contingency);
    auto length_value = Args.memory().closure_at(length_value_reg).get_code();
    if (not length_value.is_int())
        throw myexception()<<"Data.Vector.Unboxed.concat: V_Bit length is not an Int";

    int owner_value_reg = Args.evaluate_reg_use(owner_reg, vector.edge_contingency);
    const auto& owner_value = Args.memory().closure_at(owner_value_reg).get_code();
    auto object_value = owner_value.to<Runtime::ObjectValue>();
    object_ptr<const bitvector> owner;
    if (object_value)
        owner = boost::dynamic_pointer_cast<const bitvector>(object_value->value);
    if (not owner)
        throw myexception()<<"Data.Vector.Unboxed.concat: V_Bit owner has the wrong native representation";
    if (length_value.as_int() < 0 or static_cast<std::size_t>(length_value.as_int()) != owner->size())
        throw myexception()<<"Data.Vector.Unboxed.concat: V_Bit length does not match its native owner";
    return owner;
}

// Append packed inputs at their logical high end, removing each final block's padding.
// The supplied cached length is checked against the exact packed owners encountered.
closure concat_unboxed_bit_vectors(OperationArgs& Args)
{
    auto count_arg = Args.evaluate_slot_to_value_with_contingency(0);
    if (not count_arg.value.is_int())
        throw myexception()<<"Data.Vector.Unboxed.concat: result length is not an Int";
    int expected_count = count_arg.value.as_int();
    if (expected_count < 0)
        throw myexception()<<"Data.Vector.Unboxed.concat: negative result length "<<expected_count;

    std::size_t total = static_cast<std::size_t>(expected_count);
    bitvector result;
    result.reserve(total);
    std::vector<bitvector::block_type> blocks;
    auto xs = Args.evaluate_reg_use_with_contingency(Args.reg_for_slot(1), count_arg.edge_contingency);

    // Copy each owner immediately while retaining the shared walker's list and field dependencies.
    // Resizing after append removes unused high bits before the next owner is added.
    for_each_haskell_list_element(Args, xs, "Data.Vector.Unboxed.concat",
        [&](int vector_reg, EdgeContingency contingency)
        {
            auto input = read_unboxed_bit_vector(Args, vector_reg, contingency);
            if (input->size() > total - result.size())
                throw myexception()<<"Data.Vector.Unboxed.concat: input lengths exceed cached result length";
            auto old_size = result.size();
            blocks.resize(input->num_blocks());
            boost::to_block_range(input->value(), blocks.begin());
            result.append(blocks.begin(), blocks.end());
            result.resize(old_size + input->size());
        });

    if (result.size() != total)
        throw myexception()<<"Data.Vector.Unboxed.concat: input lengths do not match cached result length";
    return result;
}

}

extern "C" closure builtin_function_empty_bitvector(OperationArgs& Args)
{
    int n = Args.evaluate_slot_to_value(0).as_int();
    if (n < 0)
        throw myexception()<<"bit vector: negative length "<<n;

    return { bitvector(n) };
}

// Pack exactly the requested number of Boolean list elements, leaving an
// excess tail unevaluated and rejecting a list that is too short.
extern "C" closure builtin_function_sized_bitvector_from_list(OperationArgs& Args)
{
    auto size_arg = Args.evaluate_slot_to_value_with_contingency(0);
    int expected_size = size_arg.value.as_int();
    if (expected_size < 0)
        throw myexception()<<"bit vector: negative length "<<expected_size;

    bitvector result(expected_size);
    if (expected_size == 0)
        return result;

    auto xs = Args.evaluate_reg_use_with_contingency(Args.reg_for_slot(1), size_arg.edge_contingency);
    for(int index = 0; index < expected_size; index++)
    {
        const closure& xs_closure = Args.memory().closure_at(xs.value_reg);
        auto list_cell = xs_closure.get_code().to<Runtime::ConstructorApp>();
        if (not list_cell)
            throw myexception()<<"bit vector: expected a list constructor, but got "<<xs_closure.get_code().print();

        const auto& tag = list_cell->head;
        if (tag.name() == "[]" and tag.n_args() == 0)
            throw myexception()<<"bit vector: expected "<<expected_size<<" elements, but got "<<index;
        if (tag.name() != ":" or tag.n_args() != 2)
            throw myexception()<<"bit vector: expected ':' or '[]', but got "<<tag.print();

        int element = xs_closure.reg_for_constructor_slot(0);
        auto value_reg = Args.evaluate_reg_use(element, xs.edge_contingency);
        const auto& value = Args.memory().closure_at(value_reg).get_code();
        if (R::has_constructor(value, bool_true_name))
            result.set(index);
        else if (not R::has_constructor(value, bool_false_name))
            throw myexception()<<"bit vector: expected a Boolean, but got "<<value.print();

        if (index + 1 < expected_size)
        {
            int tail = xs_closure.reg_for_constructor_slot(1);
            xs = Args.evaluate_reg_use_with_contingency(tail, xs.edge_contingency);
        }
    }
    return result;
}

// Concatenate exact packed bit-vector owners without unpacking individual bits.
extern "C" closure builtin_function_concat_bitvectors(OperationArgs& Args)
{
    return concat_unboxed_bit_vectors(Args);
}

// Native index zero is the vector's first element.  Shifting the requested
// prefix away and then resizing therefore leaves exactly the logical slice in
// a new offset-zero owner; Haskell has already validated the source range.
extern "C" closure builtin_function_slice(OperationArgs& Args)
{
    auto value = Args.evaluate_slot_to_value(0);
    int start = Args.evaluate_slot_to_value(1).as_int();
    int count = Args.evaluate_slot_to_value(2).as_int();

    auto result = value.as_<bitvector>();
    result >>= start;
    result.resize(count);
    return result;
}

extern "C" closure builtin_function_complement(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);

    bitvector v2 = ~arg0.as_<bitvector>();

    return { v2 };
}


extern "C" closure builtin_function_bitwise_or(OperationArgs& Args)
{
    return truncated_commutative_bitwise(Args, [](auto& left, const auto& right) { left |= right; });
}


extern "C" closure builtin_function_bitwise_and(OperationArgs& Args)
{
    return truncated_commutative_bitwise(Args, [](auto& left, const auto& right) { left &= right; });
}

extern "C" closure builtin_function_bitwise_xor(OperationArgs& Args)
{
    return truncated_commutative_bitwise(Args, [](auto& left, const auto& right) { left ^= right; });
}

extern "C" R::Exp simple_function_size(vector<R::Exp>& args)
{
    return (int)get_arg(args).as_<bitvector>().size();
}

extern "C" closure builtin_function_popcount(OperationArgs& Args)
{
    auto arg0 =Args.evaluate_slot_to_value(0);

    int s = arg0.as_<bitvector>().count();

    return { s };
}

extern "C" R::Exp simple_function_test_bit(vector<R::Exp>& args)
{
    auto arg0 = get_arg(args);
    int n = get_arg(args).as_int();

    return arg0.as_<bitvector>().test(n);
}

extern "C" closure builtin_function_set_bit(OperationArgs& Args)
{
    auto arg0 =Args.evaluate_slot_to_value(0);
    auto x = arg0.as_<bitvector>();

    int n = Args.evaluate_slot_to_value(1).as_int();
    x.set(n);

    return { x };
}

extern "C" closure builtin_function_clear_bit(OperationArgs& Args)
{
    auto arg0 =Args.evaluate_slot_to_value(0);
    auto x = arg0.as_<bitvector>();

    int n = Args.evaluate_slot_to_value(1).as_int();
    x.set(n, false);

    return { x };
}

// Construct the variable-width value of bit n: indices below n are zero and
// the result ends at its single set bit.
extern "C" closure builtin_function_single_bit(OperationArgs& Args)
{
    int index = Args.evaluate_slot_to_value(0).as_int();
    if (index < 0 or index == std::numeric_limits<int>::max())
        throw myexception()<<"bit vector: invalid bit index "<<index;

    bitvector result(index + 1);
    result.set(index);
    return result;
}

// Native index zero is the start of the bit sequence.  Growing before a left
// shift makes room for a zero prefix without losing high bits; shifting right
// before shrinking discards the requested prefix and retains the exact suffix.
extern "C" closure builtin_function_shift(OperationArgs& Args)
{
    auto value = Args.evaluate_slot_to_value(0);
    int amount = Args.evaluate_slot_to_value(1).as_int();
    auto result = value.as_<bitvector>();

    if (amount > 0)
    {
        auto max_size = static_cast<std::size_t>(std::numeric_limits<int>::max());
        if (result.size() > max_size or static_cast<std::size_t>(amount) > max_size - result.size())
            throw myexception()<<"bit-vector shift result exceeds the Haskell Int range";
        result.resize(result.size() + amount);
        result <<= amount;
    }
    else if (amount < 0)
    {
        auto removed = static_cast<std::size_t>(-static_cast<long long>(amount));
        if (removed >= result.size())
            return bitvector(0);
        result >>= removed;
        result.resize(result.size() - removed);
    }
    return result;
}

// Normalize the signed rotation within the fixed owner length.  The left and
// right Boost shifts contain disjoint wrapped pieces, so their union preserves
// every input bit while retaining the exact original length.
extern "C" closure builtin_function_rotate(OperationArgs& Args)
{
    auto value = Args.evaluate_slot_to_value(0);
    int amount = Args.evaluate_slot_to_value(1).as_int();
    const auto& input = value.as_<bitvector>();
    if (input.empty())
        return input;

    auto size = input.size();
    auto signed_size = static_cast<long long>(size);
    auto rotation = static_cast<std::size_t>((static_cast<long long>(amount) % signed_size + signed_size) % signed_size);
    if (rotation == 0)
        return input;

    bitvector result = (input << rotation) | (input >> (size - rotation));
    return result;
}

extern "C" closure builtin_function_alignment_row_to_presence_bitvector(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& A = arg0.as_<Box<alignment>>().value();
    int row = Args.evaluate_slot_to_value(1).as_int();

    bitvector v(A.length());
    for(int col=0; col<A.length(); col++)
	v.set(col, alphabet::is_character(A(col,row)));

    return { v };
}

extern "C" closure builtin_function_pairwise_alignment_from_bits(OperationArgs& Args)
{
    auto arg0 =Args.evaluate_slot_to_value(0);
    auto& v1 = arg0.as_checked<bitvector>();
    auto arg1 =Args.evaluate_slot_to_value(1);
    auto& v2 = arg1.as_checked<bitvector>();

    object_ptr<Box<pairwise_alignment_t>> a = new Box<pairwise_alignment_t>;
    if (v1.size() != v2.size())
	throw myexception()<<"Can't make a pairwise alignment from bitvectors of different length!";

    for(int i=0;i<v1.size();i++)
	a->push_back(v1.test(i), v2.test(i));

    return a;
}

// Scalar Bits instances. Unsigned arithmetic preserves the native Int bit pattern without
// signed left-shift overflow. C++20 signed right shift supplies arithmetic sign extension.
namespace
{
constexpr unsigned int_bits = std::numeric_limits<unsigned>::digits;

// Counts at or beyond the word width produce zero (left) or the sign extension (right).
int shift_int(int x, std::int64_t count)
{
    if (count >= int_bits) return 0;
    if (count <= -std::int64_t(int_bits)) return x < 0 ? -1 : 0;
    if (count < 0) return x >> -count;
    return std::bit_cast<int>(static_cast<unsigned>(x) << count);
}

// Reject negative directional counts; signed shift/rotate handle direction separately.
int directional_count(vector<R::Exp>& args)
{
    int count = get_arg(args).as_int();
    if (count < 0) throw myexception()<<"Data.Bits: negative directional shift count";
    return count;
}

// cpp_int uses sign/magnitude storage but its unchecked bitwise operations implement
// infinite two's-complement semantics, including arithmetic right shift of negative values.
integer shift_integer(integer x, std::int64_t count)
{
    if (count >= 0) x <<= static_cast<std::uint64_t>(count);
    else x >>= static_cast<std::uint64_t>(-count);
    return x;
}
}

// Combine the two Int bit patterns.
extern "C" R::Exp simple_function_andInt(vector<R::Exp>& args)
{
    int x = get_arg(args).as_int();
    int y = get_arg(args).as_int();
    return std::bit_cast<int>(static_cast<unsigned>(x) & static_cast<unsigned>(y));
}

// Combine the two Int bit patterns.
extern "C" R::Exp simple_function_orInt(vector<R::Exp>& args)
{
    int x = get_arg(args).as_int();
    int y = get_arg(args).as_int();
    return std::bit_cast<int>(static_cast<unsigned>(x) | static_cast<unsigned>(y));
}

// Combine the two Int bit patterns.
extern "C" R::Exp simple_function_xorInt(vector<R::Exp>& args)
{
    int x = get_arg(args).as_int();
    int y = get_arg(args).as_int();
    return std::bit_cast<int>(static_cast<unsigned>(x) ^ static_cast<unsigned>(y));
}

// Complement every bit, including sign extension for Integer.
extern "C" R::Exp simple_function_complementInt(vector<R::Exp>& args)
{
    int x = get_arg(args).as_int();
    return std::bit_cast<int>(~static_cast<unsigned>(x));
}

// Apply a signed shift, widening the count before negating it.
extern "C" R::Exp simple_function_shiftInt(vector<R::Exp>& args)
{
    int x = get_arg(args).as_int();
    std::int64_t count = get_arg(args).as_int();
    return shift_int(x, count);
}

// Apply a directional shift, widening the count before negating it.
extern "C" R::Exp simple_function_shiftLInt(vector<R::Exp>& args)
{
    int x = get_arg(args).as_int();
    std::int64_t count = directional_count(args);
    return shift_int(x, count);
}

// Apply a directional shift, widening the count before negating it.
extern "C" R::Exp simple_function_shiftRInt(vector<R::Exp>& args)
{
    int x = get_arg(args).as_int();
    std::int64_t count = directional_count(args);
    return shift_int(x, -count);
}

// Combine the two Integer bit patterns.
extern "C" R::Exp simple_function_andInteger(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    integer y = get_arg(args).as_integer();
    return integer(x & y);
}

// Combine the two Integer bit patterns.
extern "C" R::Exp simple_function_orInteger(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    integer y = get_arg(args).as_integer();
    return integer(x | y);
}

// Combine the two Integer bit patterns.
extern "C" R::Exp simple_function_xorInteger(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    integer y = get_arg(args).as_integer();
    return integer(x ^ y);
}

// Complement every bit, including sign extension for Integer.
extern "C" R::Exp simple_function_complementInteger(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    return integer(~x);
}

// Apply a signed shift, widening the count before negating it.
extern "C" R::Exp simple_function_shiftInteger(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    std::int64_t count = get_arg(args).as_int();
    return shift_integer(x, count);
}

// Apply a directional shift, widening the count before negating it.
extern "C" R::Exp simple_function_shiftLInteger(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    std::int64_t count = directional_count(args);
    return shift_integer(x, count);
}

// Apply a directional shift, widening the count before negating it.
extern "C" R::Exp simple_function_shiftRInteger(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    std::int64_t count = directional_count(args);
    return shift_integer(x, -count);
}

// Apply a signed shift, widening the count before negating it.
extern "C" R::Exp simple_function_rotateRInteger(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    std::int64_t count = get_arg(args).as_int();
    return shift_integer(x, -count);
}

// Rotate the finite bit pattern; std::rotl also handles negative and oversized counts.
extern "C" R::Exp simple_function_rotateInt(vector<R::Exp>& args)
{
    auto x = static_cast<unsigned>(get_arg(args).as_int());
    int count = get_arg(args).as_int();
    return std::bit_cast<int>(std::rotl(x, count));
}

// Rotate the finite bit pattern; std::rotr also handles negative and oversized counts.
extern "C" R::Exp simple_function_rotateRInt(vector<R::Exp>& args)
{
    auto x = static_cast<unsigned>(get_arg(args).as_int());
    int count = get_arg(args).as_int();
    return std::bit_cast<int>(std::rotr(x, count));
}

extern "C" R::Exp simple_function_popCountInt(vector<R::Exp>& args)
{
    return std::popcount(static_cast<unsigned>(get_arg(args).as_int()));
}

extern "C" R::Exp simple_function_countLeadingZerosInt(vector<R::Exp>& args)
{
    return std::countl_zero(static_cast<unsigned>(get_arg(args).as_int()));
}

extern "C" R::Exp simple_function_countTrailingZerosInt(vector<R::Exp>& args)
{
    return std::countr_zero(static_cast<unsigned>(get_arg(args).as_int()));
}

extern "C" R::Exp simple_function_intBitSize(vector<R::Exp>&)
{
    return static_cast<int>(int_bits);
}

// For negative x, ~x is nonnegative and has the opposite value at every bit position.
extern "C" R::Exp simple_function_testBitInteger(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    int index = get_arg(args).as_int();
    if (index < 0) throw myexception()<<"Data.Bits.testBit: negative bit index";
    bool negative = x < 0;
    if (negative) x = ~x;
    return boost::multiprecision::bit_test(x, index) != negative;
}

// Match GHC's signed population count of the magnitude, scanning whole native words.
extern "C" R::Exp simple_function_popCountInteger(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    bool negative = x < 0;
    if (negative) x = -x;
    vector<std::uint64_t> words;
    boost::multiprecision::export_bits(x, std::back_inserter(words), 64);
    int count = 0;
    for (auto word : words) count += std::popcount(word);
    return negative ? -count : count;
}
