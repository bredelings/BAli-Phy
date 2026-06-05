#include "list.H"
#include "constructor.H"
#include "lambda.H"

using std::vector;
using std::optional;
using std::string;

expression_ref List() {return constructor("[]",0);}

expression_ref cons(const expression_ref& head,const expression_ref& tail)
{
    return  constructor(":",2)+head+tail;
}

template<> expression_ref get_list<>(const vector<expression_ref>& v)
{
    expression_ref E = List();

    for(int i=v.size()-1;i>=0;i--)
	E = cons(v[i],E);

    return E;
}

expression_ref char_list(const string& s)
{
    vector<expression_ref> letters;
    for(char c: s)
	letters.push_back(c);
    return get_list(letters);
}

