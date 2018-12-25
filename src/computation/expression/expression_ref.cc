// #define DEBUG_OPTIMIZE

#include "util/util.H"
#include "expression_ref.H"
#include "lambda.H"
#include "apply.H"
#include "let.H"
#include "case.H"
#include "trim.H"
#include "tuple.H"
#include "list.H" // for char_list
#include "bool.H"
#include "expression.H"
#include "computation/operations.H"
#include <set>
#include <iterator>
#include <map>
#include <cctype>

using std::pair;
using std::vector;
using std::string;
using std::set;
using std::multiset;
using std::unique_ptr;

using boost::dynamic_pointer_cast;

string print_list(const expression_ref& E)
{
    if (not has_constructor(E,":")) std::abort();

    vector<string> V;
    string S;

    expression_ref E2 = E;
    while(has_constructor(E2,":"))
    {
	assert(E2.size() == 2);
	auto x = E2.sub()[0];
	if (x.is_char())
	    S += x.as_char();
	V.push_back(x.print());
	E2 = E2.sub()[1];
    }
    if (not S.empty() and S.size() == V.size())
	return "\"" + S + "\"";
    else if (has_constructor(E2,"[]"))
	return "["+join(V,", ")+"]";
    else {
	V.push_back(E2.print());
	return join(V,":");
    }

}

// How do I make constructor-specific methods of printing data expressions?
// Can I move to defining the print function using an expression?
string expression::print() const 
{
    string result;
    assert(head);

    // The head should not have parts.
    // assert(not is_a<expression>());

    //  if (false)
    {
	vector<expression_ref> vars;
	vector<expression_ref> bodies;
	expression_ref T;

	if (head.is_a<lambda2>())
	{
	    result = sub[0].print();
	    if (sub[0].head().is_a<lambda2>())
		result = "/\\" + result;
	    else
		result = "/\\." + result;
	    return result;
	}

	if (head.is_a<lambda>())
	{
	    expression_ref body = new expression(*this);
	    vector<string> vars;
	    while (body.head().is_a<lambda>())
	    {
		vars.push_back(body.sub()[0].print());
		// Keep a reference 'body.sub()[1]' here, so it is not destroyed!
		expression_ref tmp = body.sub()[1];
		body = tmp;
	    }
	    result = "\\" + join(vars,' ') + " -> "+ body.print();
	    return result;
	}

	expression_ref body;
	vector<pair<var, expression_ref>> decls;
	if (parse_let_expression(*this, decls, body))
	{
	    result = "let {";
	    vector<string> parts;
	    for(int i=0;i<decls.size();i++)
		parts.push_back(decls[i].first.print() + " = " + decls[i].second.print());
	    result += join(parts,", ");
	    result += "} in " + body.print();
	    return result;
	}

	if (parse_indexed_let_expression(*this, bodies, T))
	{
	    result = "let {";
	    result += join(bodies,", ");
	    result += "} in " + T.print();
	    return result;
	}

	if (head.is_a<Trim>())
	{
	    auto& V = sub[0].as_<Vector<int>>();

	    result = "Trim {"+join(V,",")+"} " + sub[1].print();
	    return result;
	}

	if (parse_case_expression(*this, T, vars, bodies))
	{
	    result = "case " + T.print() + " of {";
	    vector<string> parts;
	    for(int i=0;i<vars.size();i++)
		parts.push_back( vars[i].print() + " -> " + bodies[i].print() );
	    result += join(parts,',');
	    result += "}";
	    return result;
	}
    }

    // Print the (unparenthesized) sub-expressions
    vector<string> args(1+size());
    args[0] = head.print();
    for(int i=0;i<size();i++)
	args[1+i] = sub[i].print();

    vector<string> pargs = args;
    for(int i=1;i<pargs.size();i++)
    {
	if (not sub[i-1].size()) continue;

	if (sub[i-1].head().is_a<Operator>())
	{
	    auto& O = sub[i-1].head().as_<Operator>();

	    // Don't parenthesize tuple arguments.
	    if (is_tuple_name(O.name()) and sub[i-1].size() == O.n_args()) continue;

	    // Don't parenthesize list arguments.
	    if (O.name() == ":") continue;
	}

	pargs[i] = "(" + args[i] + ")";
    }
  
    if (head.is_a<Operator>())
    {
	auto& O = head.as_<Operator>();

	string O_name = O.name();
	if (head.is_a<Apply>())
	{
#ifndef DEBUG_OPTIMIZE
	    pargs.erase(pargs.begin());
#endif
	    return O.print_expression( pargs );
	}
	else if (O.name() == ":" and size() == 2)
	{
	    return print_list(*this);
	}
	else if (O.precedence() > -1 and size() == 2)
	{
	    if (sub[0].size())
	    {
		if (sub[0].head() == O and O.associativity()==assoc_left)
		    pargs[1] = args[1];
		else if (sub[0].head().is_a<Operator>())
		    if (sub[0].head().as_<Operator>().precedence() > O.precedence())
			pargs[1] = args[1];
	    }
	    if (sub[1].size())
	    {
		if (sub[1].head() == O and O.associativity()==assoc_right)
		    pargs[2] = args[2];
		else if (sub[1].head().is_a<Operator>())
		    if (sub[1].head().as_<Operator>().precedence() > O.precedence())
			pargs[2] = args[2];
	    }
	    return pargs[1] + O_name + pargs[2];
	}
	else if (is_tuple_name(O.name()) and size() == O.n_args())
	{
	    // Should Tuple's parenthesis sub-expressions?
	    vector<string> sub_names;
	    for(int i=0;i<size();i++)
		sub_names.push_back( args[1+i] );
	    return "(" + join(sub_names,", ") + ")";
	}
      
	return O.print_expression( pargs );
    }

    return print_operator_expression( pargs );
}

bool expression::operator==(const expression& E) const
{
    if (head != E.head) return false;

    for(int i=0;i<size();i++) 
	if (sub[i] != E.sub[i]) return false;

    return true;
}

bool expression::operator==(const Object& o) const 
{
    const expression* E = dynamic_cast<const expression*>(&o);
    if (not E) 
	return false;

    return operator==(*E);
}

expression::expression(const expression_ref& H)
    :head(H)
{ 
    assert(H.is_atomic());
}

expression::expression(const expression_ref& H, const std::initializer_list< expression_ref > S)
    :expression(H,std::vector<expression_ref>(S))
{
    assert(H.is_atomic());
}

expression::expression(const expression_ref& H, const std::vector< expression_ref >& S)
    :head(H),sub(S)
{ 
    assert(H.is_atomic());
}

expression_ref::expression_ref(const bool& b)
    :expression_ref(b?bool_true:bool_false)
{}

expression_ref::expression_ref(const char* s)
    :expression_ref(char_list(s))
{}

expression_ref::expression_ref(const std::string& s)
    :expression_ref(expression_ref{var("Foreign.Vector.listFromString"),String(s)})
{}

expression_ref::expression_ref(const index_var& iv):i(iv.index),type_(index_var_type) {}

expression_ref::expression_ref(const std::initializer_list<expression_ref>& es)
{
    for(auto& e: es)
    {
	if (not (*this))
	    (*this) = e;
	else
	    (*this) = apply((*this),e);
    }
}

unique_ptr<expression> operator+(const expression_ref& E1, const expression_ref&E2)
{
    expression* E3 = new expression(E1.head());
    if (not E1.is_atomic())
	E3->sub = E1.sub();
    E3->sub.push_back(E2);
    return unique_ptr<expression>(E3);
}

unique_ptr<expression> operator+(const expression& E1, const expression_ref& E2)
{
    auto E3 = E1.clone();
    E3->sub.push_back(E2);
    return unique_ptr<expression>(E3);
}

expression_ref error(const std::string& s)
{
    expression_ref error = var("Compiler.Base.error");
    expression_ref msg = s;
    return {error,msg};
}

EVector::operator vector<double>() const
{
    vector<double> v2(size());
    for(int i=0;i<v2.size();i++)
	v2[i] = (*this)[i].as_double();
    return v2;
}

EVector::operator vector<int>() const
{
    vector<int> v2(size());
    for(int i=0;i<v2.size();i++)
	v2[i] = (*this)[i].as_int();
    return v2;
}

EVector::EVector(const vector<double>& v1)
{
    resize(v1.size());
    for(int i=0;i<v1.size();i++)
	(*this)[i] = v1[i];
}

EVector::EVector(const vector<int>& v1)
{
    resize(v1.size());
    for(int i=0;i<v1.size();i++)
	(*this)[i] = v1[i];
}
