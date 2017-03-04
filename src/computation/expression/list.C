#include "list.H"
#include "expression.H"
#include "constructor.H"

using std::vector;
using std::string;

expression_ref Cons = lambda_expression( right_assoc_constructor(":",2) );

expression_ref ListEnd = lambda_expression( constructor("[]",0) );

template<> expression_ref get_list<>(const vector<expression_ref>& v)
{
    expression_ref E = ListEnd;

    for(int i=v.size()-1;i>=0;i--)
	E = v[i]&E;

    return E;
}

expression_ref char_list(const string& s)
{
    vector<expression_ref> letters;
    for(char c: s)
	letters.push_back(c);
    return get_list(letters);
}

vector<expression_ref> get_ref_vector_from_list(const expression_ref& E)
{
    vector<expression_ref> V;

    expression_ref E2 = E;
    while(has_constructor(E2,":"))
    {
	assert(E2.size() == 2);
	V.push_back(E2.sub()[0]);
	E2 = E2.sub()[1];
    }
    assert(has_constructor(E2,"[]"));

    return V;
}

