#include "computation/object.H"
#include "computation/operation.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/indexify.H"
#include "computation/expression/lambda.H"
#include "computation/expression/trim.H"
#include "computation/runtime/ast.H"
#include "util/string/join.H" // for join( )
#include <cmath>
#include <iostream>
#include <optional>
#include <utility>

using std::vector;
using std::string;

namespace
{
    bool runtime_cache_equal(const expression_ref& E1, const expression_ref& E2)
    {
        if (E1 == E2)
            return true;

        if (E1.type() != E2.type())
            return false;

        if (E1.is_double())
            return std::isnan(E1.as_double()) and std::isnan(E2.as_double());

        if (E1.is_log_double())
            return std::isnan(E1.as_log_double().log()) and std::isnan(E2.as_log_double().log());

        if (E1.is_expression())
        {
            if (not E2.is_expression())
                return false;

            if (not runtime_cache_equal(E1.head(), E2.head()))
                return false;

            if (E1.size() != E2.size())
                return false;

            for(int i=0; i<E1.size(); i++)
                if (not runtime_cache_equal(E1.sub()[i], E2.sub()[i]))
                    return false;

            return true;
        }

        return false;
    }

    Runtime::Exp resolve_runtime_slot(Runtime::Exp E, const closure::Env_t& Env)
    {
        if (auto index_var = E.to<Runtime::IndexVar>())
        {
            assert(0 <= index_var->index);
            assert(index_var->index < Env.size());
            return Runtime::RegRef(lookup_in_env(Env, index_var->index));
        }
        else
            return E;
    }

    std::optional<Runtime::Exp> runtime_slot(const Runtime::Exp& E, int i)
    {
        if (auto app = E.to<Runtime::App>())
        {
            assert(0 <= i);
            assert(i < app->args.size());
            return app->args[i];
        }
        else if (auto case_ = E.to<Runtime::Case>())
        {
            // The slot helpers expose the legacy expression_ref child layout.
            // For Case, only the scrutinee is runtime code; the alternatives
            // object remains an expression_ref value during the transition.
            if (i == 0)
                return case_->object;
        }
        else if (auto trim = E.to<Runtime::Trim>())
        {
            // Trim slot 0 is the keep-vector object; slot 1 is the body.
            if (i == 1)
                return trim->body;
        }

        return {};
    }
}
void closure::clear()
{
    exp_cache_.clear();
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
    exp_cache_ = Runtime::to_expression_ref(runtime_exp);
    check_runtime_expression();
}

void closure::set_legacy_expression(expression_ref E)
{
    if (E)
        set_runtime_expression(Runtime::ObjectValue(std::move(E)));
    else
        clear();
}

void closure::check_runtime_expression() const
{
#ifndef NDEBUG
    if (runtime_exp)
    {
        auto runtime_ref = Runtime::to_expression_ref(runtime_exp);
        if (not runtime_cache_equal(runtime_ref, exp))
        {
            std::cerr<<"closure runtime expression mismatch\n";
            std::cerr<<"  exp         = "<<exp.print()<<"\n";
            std::cerr<<"  runtime ref = "<<runtime_ref.print()<<"\n";
            std::cerr<<"  runtime ast = "<<Runtime::print(runtime_exp)<<"\n";
        }
        assert(runtime_cache_equal(runtime_ref, exp));
    }
#endif
}

void closure::peel_lambdas(int n)
{
    check_runtime_expression();

    if (has_structured_runtime_expression())
    {
        Runtime::Exp E = runtime_exp;
        bool peeled_runtime = true;
        for(int i=0; i<n; i++)
        {
            auto L = E.to<Runtime::Lambda>();
            assert(L);
            if (not L)
            {
                peeled_runtime = false;
                break;
            }
            E = L->body;
        }

        if (peeled_runtime)
        {
            set_runtime_expression(std::move(E));
            return;
        }
    }

    expression_ref E2 = exp;
    for(int i=0; i<n; i++)
    {
        assert(E2.head().type() == type_constant::lambda2_type);
        E2 = E2.sub()[0];
    }
    set_legacy_expression(std::move(E2));
}

int closure::runtime_n_args() const
{
    check_runtime_expression();

    if (auto app = runtime_exp.to<Runtime::App>())
        return app->args.size();
    else
        return exp.size();
}

Runtime::Exp closure::runtime_arg_for_slot(int i) const
{
    check_runtime_expression();

    if (runtime_exp)
    {
        if (auto E = runtime_slot(runtime_exp, i))
            return resolve_runtime_slot(std::move(*E), Env);
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
        auto E = runtime_slot(runtime_exp, i);
        if (E)
        {
            if (auto index_var = E->to<Runtime::IndexVar>())
                return lookup_in_env(index_var->index);
            else if (auto reg_ref = E->to<Runtime::RegRef>())
                return reg_ref->target;
            else
                std::abort();
        }
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
