#include "computation/object.H"
#include "computation/operation.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/indexify.H"
#include "computation/expression/trim.H"
#include "computation/runtime/trim.H"
#include "util/string/join.H" // for join( )
#include <cstdlib>
#include <utility>

using std::vector;
using std::string;

namespace
{
    const Runtime::Exp& runtime_app_slot(const Runtime::Exp& E, int i)
    {
        auto app = E.to<Runtime::App>();
        if (not app)
            std::abort();

        assert(0 <= i);
        assert(i < app->args.size());
        return app->args[i];
    }
}

void closure::set_code(Runtime::Exp c)
{
    code = std::move(c);
    exp_cache = Runtime::to_expression_ref(code);
}

void closure::set_legacy_exp(expression_ref e)
{
    code = {};
    exp_cache = std::move(e);
}

void closure::clear()
{
    exp_cache = {};
    code = {};
    Env.clear();
}

string closure::print() const
{
    string result = exp.print();
    if (Env.size())
	result += " {" + join(Env,", ") + "}";
    return result;
}

int closure::runtime_n_slots() const
{
    assert(has_code());

    auto app = code.to<Runtime::App>();
    if (not app)
        std::abort();

    return app->args.size();
}

Runtime::Exp closure::runtime_slot(int i) const
{
    const auto& E = runtime_app_slot(code, i);

    if (auto index_var = E.to<Runtime::IndexVar>())
        return Runtime::RegRef(lookup_in_env(index_var->index));
    else
        return E;
}

int closure::runtime_reg_for_slot(int i) const
{
    const auto& E = runtime_app_slot(code, i);

    if (auto index_var = E.to<Runtime::IndexVar>())
        return lookup_in_env(index_var->index);
    else if (auto reg_ref = E.to<Runtime::RegRef>())
        return reg_ref->target;
    else
        std::abort();
}

closure get_trimmed(const closure& C)
{
    closure C2 = C;
    return get_trimmed(std::move(C2));
}

void do_trim(closure& C)
{
    vector<int> keep;
    if (const auto* trim = C.get_code().to<Runtime::Trim>())
    {
        keep = trim->indices;
        C.set_code(trim->body);

        assert(not C.get_code().to<Runtime::Trim>());
    }
    else if (C.exp.head().type() == type_constant::trim_type)
    {
        expression_ref old = C.exp;
        keep = old.sub()[0].as_<Vector<int>>();

        C.set_legacy_exp(old.sub()[1]);

        assert(not C.exp.head().is_a<Trim>());
    }
    else
        return;

    // Since environments are indexed backwards
    for(int i=0;i<keep.size();i++)
    {
        int k = keep[keep.size()-1-i];
        C.Env[i] = C.lookup_in_env(k);
    }
    C.Env.resize(keep.size());


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
    if (C2.has_code())
        C2.set_code(Runtime::trim_unnormalize(C2.get_code()));
    else
        C2.set_legacy_exp(trim_unnormalize(C2.exp));
    return C2;
}
