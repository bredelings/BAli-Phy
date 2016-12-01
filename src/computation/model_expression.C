#include "model_expression.H"
#include "context.H"
#include "computation/module.H"
#include "computation/operations.H"
#include "parser/AST.H"

using std::vector;
using std::set;
using std::string;

bool is_model_expression(const expression_ref& E)
{
    return is_AST(E,"model");
}

bool is_prefix_expression(const expression_ref& E)
{
    return is_AST(E,"prefix");
}

bool contains_model_expression(const expression_ref& E)
{
    if (not E.is_expression()) return false;

    if (is_model_expression(E)) return true;

    for(const auto F: E.sub())
	if (contains_model_expression(F))
	    return true;

    return false;
}

expression_ref perform_exp(const expression_ref& F)
{
    expression_ref E = F;
    if (contains_model_expression(F))
    {
	E = translate_model(E);
	E = (identifier("gen_model"),E);
	E = (identifier("unsafePerformIO'"),E);
	E = (identifier("evaluate"),-1,E);
    }
    return E;
}

expression_ref perform_exp(const expression_ref& F, const string& prefix)
{
    expression_ref E = F;
    if (contains_model_expression(F))
    {
	E = translate_model(E);
	E = (identifier("add_prefix"),prefix,E);
	E = (identifier("gen_model"),E);
	E = (identifier("unsafePerformIO'"),E);
	E = (identifier("evaluate"),-1,E);
    }
    return E;
}

expression_ref result(const expression_ref& E, const module_loader& L)
{
    return result(E, L,vector<Module>{});
}

expression_ref result(const expression_ref& E, const module_loader& L, const vector<Module>& Ps)
{
    context C(L, Ps);
    return C.evaluate_expression(perform_exp(E));
}

expression_ref result(const expression_ref& E, const module_loader& L, const vector<string>& module_names)
{
    context C(L, module_names);
    return C.evaluate_expression(perform_exp(E));
}

expression_ref model_expression(const vector<expression_ref>& es)
{
    return expression_ref(AST_node("model"),es);
}

expression_ref model_expression(const expression_ref& E)
{
    vector<expression_ref> es;
    if (not E.is_expression())
	es = {E};
    else {
	if (E.head().is_a<Apply>())
	    es = E.sub();
	else
	    throw myexception()<<"Cannot make model expression from "<<E<<" with head "<<E.head();
    }
    return model_expression(es);
}

// prefix_action s a = Prefix s a
expression_ref prefix(const expression_ref& s, const expression_ref& E)
{
    return expression_ref(AST_node("prefix"),{s,E});
}

// log_action s a = do { x <- A ; log s x ; return x }
expression_ref add_logger(const string& s, const expression_ref& E)
{
    return model_expression({identifier("add_logger"),s,E});
}

expression_ref translate_model(const expression_ref& E)
{
    if (E.is_expression() and contains_model_expression(E))
    {
	if (is_prefix_expression(E))
	{
	    assert(E.size() == 2);
	    auto prefix = E.sub()[0];
	    auto E2      = E.sub()[1];

	    return (identifier("@@"), E.sub()[0], translate_model(E2));
	}
	int index = 0;
	for(const auto& F: E.sub())
	    index = std::max(index,get_safe_binder_index(F));

	expression_ref E2 = E.sub()[0];
	int A = E.size()-1;
	vector<int> needs_translation(A,0);
	for(int i=0;i<A;i++)
	{
	    needs_translation[i] = contains_model_expression(E.sub()[i+1]);
	    if (needs_translation[i])
		E2 = (E2,dummy(index+i));
	    else
		E2 = (E2, E.sub()[i+1]);
	}
	if (not is_AST(E,"model"))
	    E2 = (identifier("return"),E2);

	for(int i=A-1;i>=0;i--)
	    if (needs_translation[i])
	    {
		expression_ref arg = translate_model(E.sub()[i+1]);
		E2 = (identifier(">>="),arg,dummy(index+i)^E2);
	    }
	return E2;
    }
    else
	return ((identifier("return"),E));
}

