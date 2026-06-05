#include "computation/object.H"
#include "computation/operation.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/indexify.H"
#include "computation/expression/trim.H"
#include "computation/runtime/trim.H"
#include "util/string/join.H" // for join( )
#include <cstdlib>
#include <type_traits>
#include <utility>

using std::vector;
using std::string;

namespace
{
    const Runtime::Exp& runtime_slot_ref(const Runtime::Exp& E, int i)
    {
        auto app = E.to<Runtime::App>();
        assert(app);
        assert(0 <= i);
        assert(i < app->args.size());
        return app->args[i];
    }

    Runtime::Exp deindexify_code(const Runtime::Exp& E, const closure::Env_t& Env, int depth)
    {
        return E.visit([&](const auto& e) -> Runtime::Exp
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, Runtime::IndexVar>)
            {
                if (e.index < depth)
                    return e;

                int free_index = e.index - depth;
                if (free_index < Env.size())
                    return Runtime::RegRef(lookup_in_env(Env, free_index));
                else
                    return Runtime::IndexVar(e.index - Env.size());
            }
            else if constexpr (std::is_same_v<T, Runtime::Lambda>)
            {
                return Runtime::Lambda(deindexify_code(e.body, Env, depth + 1));
            }
            else if constexpr (std::is_same_v<T, Runtime::Let>)
            {
                int n = e.binds.size();
                vector<Runtime::Exp> binds;
                binds.reserve(n);
                for(const auto& bind: e.binds)
                    binds.push_back(deindexify_code(bind, Env, depth + n));

                return Runtime::Let(std::move(binds), deindexify_code(e.body, Env, depth + n));
            }
            else if constexpr (std::is_same_v<T, Runtime::Case>)
            {
                vector<Runtime::Alt> alts;
                alts.reserve(e.alts.size());
                for(const auto& alt: e.alts)
                {
                    int n = Runtime::pattern_arity(alt.pattern);
                    alts.push_back(Runtime::Alt(alt.pattern, deindexify_code(alt.body, Env, depth + n)));
                }

                return Runtime::Case(deindexify_code(e.object, Env, depth), std::move(alts));
            }
            else if constexpr (std::is_same_v<T, Runtime::App>)
            {
                vector<Runtime::Exp> args;
                args.reserve(e.args.size());
                for(const auto& arg: e.args)
                    args.push_back(deindexify_code(arg, Env, depth));

                return Runtime::App(e.head, std::move(args));
            }
            else if constexpr (std::is_same_v<T, Runtime::Trim>)
            {
                return deindexify_code(Runtime::trim_unnormalize(E), Env, depth);
            }
            else
                return e;
        });
    }
}

void closure::set_code(Runtime::Exp c)
{
    code = std::move(c);
    if (code.empty())
        exp_cache = expression_ref{};
    else
        exp_cache = std::nullopt;
}

const expression_ref& closure::legacy_exp() const
{
    if (not exp_cache)
    {
        assert(has_code());
        exp_cache = Runtime::to_expression_ref(code);
    }
    return *exp_cache;
}

void closure::clear()
{
    exp_cache = expression_ref{};
    code = {};
    Env.clear();
}

string closure::print() const
{
    string result = legacy_exp().print();
    if (Env.size())
	result += " {" + join(Env,", ") + "}";
    return result;
}

int closure::runtime_n_slots() const
{
    assert(has_code());

    if (auto app = code.to<Runtime::App>())
        return app->args.size();
    else if (code.to<Runtime::Case>())
        return 1;
    else
        std::abort();
}

Runtime::Exp closure::runtime_slot(int i) const
{
    const auto& E = runtime_slot_ref(code, i);

    if (auto index_var = E.to<Runtime::IndexVar>())
        return Runtime::RegRef(lookup_in_env(index_var->index));
    else
        return E;
}

int closure::runtime_reg_for_slot(int i) const
{
    const auto& E = runtime_slot_ref(code, i);

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
    assert(C.has_code());

    vector<int> keep;
    if (const auto* trim = C.get_code().to<Runtime::Trim>())
    {
        keep = trim->indices;
        C.set_code(trim->body);

        assert(not C.get_code().to<Runtime::Trim>());
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

Runtime::Exp deindexify(const closure& C)
{
    return deindexify_code(C.get_code(), C.Env, 0);
}

closure trim_unnormalize(const closure& C)
{
    assert(C.has_code());

    closure C2 = C;
    C2.set_code(Runtime::trim_unnormalize(C2.get_code()));
    return C2;
}
