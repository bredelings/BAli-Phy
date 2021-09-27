#include "tuple.H"
#include "lambda.H"
#include "var.H"

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

bool is_tuple_name(const string& s)
{
    if (s.size() < 3) return false;
    return s == tuple_name(s.size()-1);
}

int tuple_arity(const string& s)
{
    if (s == "()") return 0;
    assert(is_tuple_name(s));
    int n = s.size()-1;
    assert(s == tuple_name(n));
    return n;
}

string tuple_name(int n)
{
    if (n == 0)
	return "()";

    if (n == 1)
	std::abort();

    string s;
    s.resize(n+1);
    s[0] = '(';
    for(int i=1;i<n;i++)
	s[i] = ',';
    s[n] = ')';
    return s;
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
    return lambda_expression( tuple_head(n) );
}

expression_ref fst  = var("Data.Tuple.fst");
expression_ref snd  = var("Data.Tuple.snd");
