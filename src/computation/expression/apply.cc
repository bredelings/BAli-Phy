#include "apply.H"

#include "computation/operations.H"

using std::vector;

expression_ref apply_expression(const expression_ref& R,const expression_ref& arg)
{
    assert(R);
    assert(arg);
    if (R.head().is_a<Apply>())
	return R+arg;
    else
	return Apply() + R + arg;
}

expression_ref apply_expression(const expression_ref& E,
				const vector< expression_ref >& args)
{
    expression_ref E2 = E;
    for(int i=0;i<args.size();i++)
	E2 = apply_expression(E2,args[i]);
    return E2;
}

expression_ref apply_expression(const expression_ref& E,
				const vector< var >& args)
{
    expression_ref E2 = E;
    for(int i=0;i<args.size();i++)
	E2 = apply_expression(E2,args[i]);
    return E2;
}


// When applying Lx.M to N, we need to make sure that no occurrence of x has the free variables in N bound.
// At each occurence of x, we need to know 
// (i) what are the lambda's that class with the free variables of N
// (ii) what free variables of M are 
expression_ref apply(const expression_ref& E,const expression_ref& arg)
{
    return apply_expression(E,arg);
}

expression_ref apply(const expression_ref& E, const vector< expression_ref >& args)
{
    expression_ref E2 = E;
    for(int i=0;i<args.size();i++)
	E2 = apply(E2,args[i]);
    return E2;
}


// Multiple-arg version
expression_ref apply(const expression_ref& E, const std::vector< var >& args)
{
    expression_ref E2 = E;
    for(int i=0;i<args.size();i++)
	E2 = apply(E2,args[i]);
    return E2;
}



