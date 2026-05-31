#include "computation/object.H"
#include "computation/operation.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/indexify.H"
#include "computation/expression/trim.H"
#include "computation/runtime/ast.H"
#include "util/string/join.H" // for join( )
#include <utility>

using std::vector;
using std::string;

void closure::clear()
{
    exp.clear();
    runtime_exp = {};
    Env.clear();
}

string closure::print() const
{
    string result = exp.print();
    if (Env.size())
	result += " {" + join(Env,", ") + "}";
    return result;
}

void closure::set_runtime_expression(Runtime::Exp E)
{
    Runtime::check_translated(E);
    runtime_exp = std::move(E);
    exp = Runtime::to_expression_ref(runtime_exp);
    check_runtime_expression();
}

void closure::set_legacy_expression(expression_ref E)
{
    exp = std::move(E);
    clear_runtime_expression();
}

void closure::clear_runtime_expression()
{
    runtime_exp = {};
}

void closure::check_runtime_expression() const
{
#ifndef NDEBUG
    if (runtime_exp)
        assert(Runtime::to_expression_ref(runtime_exp) == exp);
#endif
}

int closure::runtime_n_args() const
{
    check_runtime_expression();

    if (runtime_exp)
    {
        if (auto app = runtime_exp.to<Runtime::App>())
            return app->args.size();
        else
            return 0;
    }
    else
        return exp.size();
}

Runtime::Exp closure::runtime_arg_for_slot(int i) const
{
    check_runtime_expression();

    if (runtime_exp)
    {
        auto app = runtime_exp.to<Runtime::App>();
        assert(app);
        assert(i < app->args.size());

        auto& E = app->args[i];
        if (auto index_var = E.to<Runtime::IndexVar>())
            return Runtime::RegRef(lookup_in_env(index_var->index));
        else
            return E;
    }

    auto E = arg_for_slot(i);
    if (E.is_reg_var())
        return Runtime::RegRef(E.as_reg_var());
    else
        return Runtime::ObjectValue(E);
}

int closure::runtime_reg_for_slot(int i) const
{
    check_runtime_expression();

    if (runtime_exp)
    {
        auto app = runtime_exp.to<Runtime::App>();
        assert(app);
        assert(i < app->args.size());

        auto& E = app->args[i];
        if (auto index_var = E.to<Runtime::IndexVar>())
            return lookup_in_env(index_var->index);
        else if (auto reg_ref = E.to<Runtime::RegRef>())
            return reg_ref->target;
        else
            std::abort();
    }

    return reg_for_slot(i);
}

closure get_trimmed(const closure& C)
{
    closure C2 = C;
    return get_trimmed(std::move(C2));
}

void do_trim(closure& C)
{
    if (C.exp.head().type() == type_constant::trim_type)
    {
	expression_ref old = C.exp;
	const vector<int>& keep = old.sub()[0].as_<Vector<int>>();

        if (auto runtime_trim = C.runtime_exp.to<Runtime::Trim>())
        {
            assert(runtime_trim->indices == keep);
            C.set_runtime_expression(runtime_trim->body);
        }
        else
            C.set_legacy_expression(C.exp.sub()[1]);

	// Since environments are indexed backwards
	for(int i=0;i<keep.size();i++)
	{
	    int k = keep[keep.size()-1-i];
	    C.Env[i] = C.lookup_in_env(k);
	}
	C.Env.resize(keep.size());

	// Should this ever happen?
	assert(not C.exp.head().is_a<Trim>());
    }
}

closure get_trimmed(closure&& C)
{
    do_trim(C);

    return std::move(C);
}

expression_ref deindexify(const closure& C)
{
    vector<expression_ref> variables;
    for(int R: C.Env)
	variables.push_back(reg_var(R));
  
    return deindexify(C.exp, variables);
}

closure trim_unnormalize(const closure& C)
{
    closure C2 = C;
    C2.set_legacy_expression(trim_unnormalize(C2.exp));
    return C2;
}
