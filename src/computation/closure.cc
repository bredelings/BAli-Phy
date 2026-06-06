#include "computation/object.H"
#include "computation/operation.H"
#include "computation/preprocess.H"
#include "computation/runtime/trim.H"
#include "util/string/join.H" // for join( )
#include <cstdlib>
#include <utility>

using std::vector;
using std::string;

void closure::set_code(Runtime::Exp c)
{
    code = std::move(c);
}

void closure::clear()
{
    code = {};
    Env.clear();
}

string closure::print() const
{
    string result = deindexify(*this).print();
    if (Env.size())
	result += " {" + join(Env,", ") + "}";
    return result;
}

int closure::n_slots() const
{
    assert(has_code());

    if (auto app = code.to<Runtime::App>())
        return app->args.size();
    else if (code.to<Runtime::Case>())
        return 1;
    else
        std::abort();
}

Runtime::Exp closure::slot(int i) const
{
    const auto& E = code.slot_ref(i);

    if (auto index_var = E.to<Runtime::IndexVar>())
        return Runtime::RegRef(lookup_in_env(index_var->index));
    else
        return E;
}

int closure::reg_for_slot(int i) const
{
    const auto& E = code.slot_ref(i);

    if (auto index_var = E.to<Runtime::IndexVar>())
        return lookup_in_env(index_var->index);
    else if (auto reg_ref = E.to<Runtime::RegRef>())
        return reg_ref->target;
    else
        std::abort();
}

closure get_trimmed(const Runtime::Exp& code, const closure::Env_t& Env)
{
    assert(not code.empty());

    if (const auto* trim = code.to<Runtime::Trim>())
    {
        closure::Env_t trimmed_env;
        trimmed_env.resize(trim->indices.size());

        // Since environments are indexed backwards.
        for(int i=0;i<trim->indices.size();i++)
        {
            int k = trim->indices[trim->indices.size()-1-i];
            trimmed_env[i] = lookup_in_env(Env, k);
        }

        return closure(trim->body, trimmed_env);
    }
    else
        return closure(code, Env);
}

closure trim_unnormalize(const closure& C)
{
    assert(C.has_code());

    closure C2 = C;
    C2.set_code(Runtime::trim_unnormalize(C2.get_code()));
    return C2;
}
