#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "dp/2way.H"
#include "computation/expression/bool.H"
#include "computation/expression/constructor.H"
#include <boost/dynamic_bitset.hpp>
#include "alignment/alignment.H"

#include <boost/dynamic_bitset.hpp>

using std::vector;

typedef Box<boost::dynamic_bitset<>> bitvector;

extern "C" closure builtin_function_empty_bitvector(OperationArgs& Args)
{
    int n = Args.evaluate(0).as_int();

    return { bitvector(n) };
}

extern "C" closure builtin_function_complement(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);

    bitvector v2 = ~arg0.as_<bitvector>();

    return { v2 };
}


extern "C" closure builtin_function_bitwise_or(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);

    bitvector v2 = arg0.as_<bitvector>() | arg1.as_<bitvector>();

    return { v2 };
}


extern "C" closure builtin_function_bitwise_and(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);

    bitvector v2 = arg0.as_<bitvector>() & arg1.as_<bitvector>();

    return { v2 };
}

extern "C" closure builtin_function_eq(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);

    bitvector v2 = arg0.as_<bitvector>() | arg1.as_<bitvector>();

    return { arg0.as_<bitvector>() == arg1.as_<bitvector>() };
}


extern "C" closure builtin_function_neq(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);

    return { arg0.as_<bitvector>() != arg1.as_<bitvector>() };
}


extern "C" closure builtin_function_bitwise_xor(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);

    bitvector v2 = arg0.as_<bitvector>() ^ arg1.as_<bitvector>();

    return { v2 };
}

extern "C" expression_ref simple_function_size(vector<expression_ref>& args)
{
    return (int)get_arg(args).as_<bitvector>().size();
}

extern "C" closure builtin_function_popcount(OperationArgs& Args)
{
    auto arg0 =Args.evaluate(0);

    int s = arg0.as_<bitvector>().count();

    return { s };
}

extern "C" expression_ref simple_function_test_bit(vector<expression_ref>& args)
{
    auto arg0 = get_arg(args);
    int n = get_arg(args).as_int();

    return arg0.as_<bitvector>().test(n);
}

extern "C" closure builtin_function_set_bit(OperationArgs& Args)
{
    auto arg0 =Args.evaluate(0);
    auto x = arg0.as_<bitvector>();

    int n = Args.evaluate(1).as_int();
    x.set(n);

    return { x };
}

extern "C" closure builtin_function_clear_bit(OperationArgs& Args)
{
    auto arg0 =Args.evaluate(0);
    auto x = arg0.as_<bitvector>();

    int n = Args.evaluate(1).as_int();
    x.set(n, false);

    return { x };
}

extern "C" closure builtin_function_alignment_row_to_presence_bitvector(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& A = arg0.as_<Box<alignment>>().value();
    auto &a = A.get_alphabet();

    int row = Args.evaluate(1).as_int();

    bitvector v(A.length());
    for(int col=0; col<A.length(); col++)
	v.set(col, a.is_feature(A(col,row)));

    return { v };
}

extern "C" closure builtin_function_pairwise_alignment_from_bits(OperationArgs& Args)
{
    auto arg0 =Args.evaluate(0);
    auto& v1 = arg0.as_checked<bitvector>();
    auto arg1 =Args.evaluate(1);
    auto& v2 = arg1.as_checked<bitvector>();

    object_ptr<Box<pairwise_alignment_t>> a = new Box<pairwise_alignment_t>;
    if (v1.size() != v2.size())
	throw myexception()<<"Can't make a pairwise alignment from bitvectors of different length!";

    for(int i=0;i<v1.size();i++)
	a->push_back(v1.test(i), v2.test(i));

    return a;
}
