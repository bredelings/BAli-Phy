#include "lambda.H"
#include "dummy.H"

using std::string;
using std::vector;

string lambda::print() const {
    return "lambda";
}

tribool lambda::compare(const Object& o) const 
{
    return dynamic_cast<const lambda*>(&o);
}

string lambda2::print() const {
    return "/\\";
}

tribool lambda2::compare(const Object& o) const 
{
    return dynamic_cast<const lambda2*>(&o);
}

expression_ref lambda_quantify(const expression_ref& dummy, const expression_ref& R)
{
    return new expression(lambda(),{dummy, R});
}

expression_ref lambda_quantify(int dummy_index, const expression_ref& R)
{
    return lambda_quantify(dummy(dummy_index), R);
}

expression_ref lambda_expression(const Operator& O)
{
    int n = O.n_args();
    assert(n != -1);
  
    expression_ref R;
    if (n == 0)
	R = expression_ref(O.clone());
    else
    {
	expression* E = new expression(O);
	for(int i=0;i<n;i++)
	    E->sub.push_back(expression_ref(dummy(i)));
	R = expression_ref(E);
    }
  
    for(int i=n-1;i>=0;i--) 
	R = lambda_quantify(i,R);
  
    return R;
}

expression_ref lambda_expression(const Operator& O);
expression_ref lambda_quantify(int dummy_index, const expression_ref& E);
expression_ref lambda_quantify(const expression_ref& dummy, const expression_ref& E);

