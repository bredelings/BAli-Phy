#include "lambda.H"
#include "var.H"

#include "range/v3/all.hpp"
namespace views = ranges::views;

using std::string;
using std::vector;

string lambda::print() const {
    return "lambda";
}

bool lambda::operator==(const Object& o) const 
{
    return dynamic_cast<const lambda*>(&o);
}

string lambda2::print() const {
    return "/\\";
}

bool lambda2::operator==(const Object& o) const 
{
    return dynamic_cast<const lambda2*>(&o);
}

expression_ref lambda_quantify(const expression_ref& var, const expression_ref& R)
{
    return new expression(lambda(),{var, R});
}

expression_ref lambda_quantify(int var_index, const expression_ref& R)
{
    return lambda_quantify(var(var_index), R);
}

expression_ref lambda_quantify(const vector<expression_ref>& vars, expression_ref& body)
{
    auto L = body;
    for(auto& v: vars | views::reverse)
        L = lambda_quantify(v, L);
    return L;
}

expression_ref lambda_quantify(const std::vector<var>& vars, const expression_ref& body)
{
    auto L = body;
    for(auto& v: vars | views::reverse)
        L = lambda_quantify(v, L);
    return L;
}

expression_ref lambda_n(const Object& O, int n)
{
    expression_ref R;
    if (n == 0)
	R = expression_ref(O.clone());
    else
    {
	expression* E = new expression(O);
	for(int i=0;i<n;i++)
	    E->sub.push_back(expression_ref(var(i)));
	R = expression_ref(E);
    }
  
    for(int i=n-1;i>=0;i--) 
	R = lambda_quantify(i,R);
  
    return R;
}


expression_ref lambda_quantify(int var_index, const expression_ref& E);
expression_ref lambda_quantify(const expression_ref& var, const expression_ref& E);

