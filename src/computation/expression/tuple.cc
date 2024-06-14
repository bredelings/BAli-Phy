#include "tuple.H"
#include "lambda.H"
#include "var.H"

#include "haskell/ids.H"

using std::vector;
using std::string;

template<> expression_ref get_tuple<>(const vector<expression_ref>& S)
{
    if (S.size() == 0) return constructor("()",0);

    if (S.size() == 1) return S[0];

    constructor H = tuple_head(S.size());

    if (not S.size()) return H;

    return new expression(H,S);
}

constructor tuple_head(int n)
{
    assert(n != 1);

    string s = tuple_name(n);
    return constructor(s,n);
}

expression_ref Tuple(int n)
{
    assert(n >= 0);
    return lambda_n( tuple_head(n), n );
}

expression_ref fst  = var("Data.Tuple.fst");
expression_ref snd  = var("Data.Tuple.snd");
