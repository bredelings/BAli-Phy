#include "graph_register.H"
#include "computation/runtime/ast.H"
#include "util/myexception.H"

using std::vector;

Runtime::Exp reg_heap::capture_local_reg_refs(const Runtime::Exp& E, closure::Env_t& Env, int depth)
{
    // Compatibility bridge for expression_ref-only VM/API entry points.
    // Some callers build executable expressions with reg_var atoms because they
    // cannot pass a closure environment alongside an expression_ref.  Convert
    // those local RegRefs to IndexVars before trimming so Trim nodes can remap
    // the corresponding closure slots.  Long term, prefer APIs that accept a
    // Runtime expression plus Env directly, avoiding local RegRefs here.
    return E.visit([&](const auto& e) -> Runtime::Exp
    {
        using T = std::decay_t<decltype(e)>;

        if constexpr (std::is_same_v<T, Runtime::RegRef>)
        {
            if (reg_is_pinned(e.target))
                return e;

            int index = depth + Env.size();
            Env.insert(Env.begin(), e.target);
            return Runtime::IndexVar(index);
        }
        else if constexpr (std::is_same_v<T, Runtime::Int> or
                           std::is_same_v<T, Runtime::Double> or
                           std::is_same_v<T, Runtime::LogDouble> or
                           std::is_same_v<T, Runtime::Char> or
                           std::is_same_v<T, Runtime::String> or
                           std::is_same_v<T, Runtime::Integer> or
                           std::is_same_v<T, Runtime::Constructor> or
                           std::is_same_v<T, Runtime::IndexVar> or
                           std::is_same_v<T, Runtime::GlobalVar>)
        {
            return e;
        }
        else if constexpr (std::is_same_v<T, Runtime::Lambda>)
        {
            return Runtime::Lambda(capture_local_reg_refs(e.body, Env, depth + 1));
        }
        else if constexpr (std::is_same_v<T, Runtime::Let>)
        {
            int n = e.binds.size();

            vector<Runtime::Exp> binds;
            for(const auto& bind: e.binds)
                binds.push_back(capture_local_reg_refs(bind, Env, depth + n));

            return Runtime::Let(binds, capture_local_reg_refs(e.body, Env, depth + n));
        }
        else if constexpr (std::is_same_v<T, Runtime::Case>)
        {
            vector<Runtime::Alt> alts;
            for(const auto& alt: e.alts)
                alts.push_back(Runtime::Alt(alt.pattern, capture_local_reg_refs(alt.body, Env, depth + Runtime::pattern_arity(alt.pattern))));

            return Runtime::Case(capture_local_reg_refs(e.object, Env, depth), alts);
        }
        else if constexpr (std::is_same_v<T, Runtime::App>)
        {
            vector<Runtime::Exp> args;
            for(const auto& arg: e.args)
                args.push_back(capture_local_reg_refs(arg, Env, depth));

            return Runtime::App(e.head, args);
        }
        else if constexpr (std::is_same_v<T, Runtime::Trim>)
        {
            auto env_size = Env.size();
            auto body = capture_local_reg_refs(e.body, Env, depth);

            if (Env.size() != env_size)
                throw myexception()<<"Cannot capture non-pinned register reference inside already-trimmed runtime expression";

            return Runtime::Trim(e.indices, body);
        }
        else
            std::abort();
    });
}

Runtime::Exp reg_heap::translate_refs(const Runtime::Exp& E, closure::Env_t& Env, int depth)
{
    return E.visit([&](const auto& e) -> Runtime::Exp
    {
        using T = std::decay_t<decltype(e)>;

        if constexpr (std::is_same_v<T, Runtime::GlobalVar>)
        {
            int r = reg_for_id(e);
            if (not reg_is_pinned(r))
                throw myexception()<<"Global variable '"<<e.name<<"' resolved to unpinned register "<<r;

            return Runtime::RegRef(r);
        }
        else if constexpr (std::is_same_v<T, Runtime::RegRef>)
        {
            if (reg_is_pinned(e.target))
                return e;

            int index = depth + Env.size();
            Env.insert(Env.begin(), e.target);
            return Runtime::IndexVar(index);
        }
        else if constexpr (std::is_same_v<T, Runtime::Int> or
                           std::is_same_v<T, Runtime::Double> or
                           std::is_same_v<T, Runtime::LogDouble> or
                           std::is_same_v<T, Runtime::Char> or
                           std::is_same_v<T, Runtime::String> or
                           std::is_same_v<T, Runtime::Integer> or
                           std::is_same_v<T, Runtime::Constructor> or
                           std::is_same_v<T, Runtime::IndexVar>)
        {
            return e;
        }
        else if constexpr (std::is_same_v<T, Runtime::Lambda>)
        {
            return Runtime::Lambda(translate_refs(e.body, Env, depth + 1));
        }
        else if constexpr (std::is_same_v<T, Runtime::Let>)
        {
            int n = e.binds.size();

            vector<Runtime::Exp> binds;
            for(const auto& bind: e.binds)
                binds.push_back(translate_refs(bind, Env, depth + n));

            return Runtime::Let(binds, translate_refs(e.body, Env, depth + n));
        }
        else if constexpr (std::is_same_v<T, Runtime::Case>)
        {
            vector<Runtime::Alt> alts;
            for(const auto& alt: e.alts)
                alts.push_back(Runtime::Alt(alt.pattern, translate_refs(alt.body, Env, depth + Runtime::pattern_arity(alt.pattern))));

            return Runtime::Case(translate_refs(e.object, Env, depth), alts);
        }
        else if constexpr (std::is_same_v<T, Runtime::App>)
        {
            vector<Runtime::Exp> args;
            for(const auto& arg: e.args)
                args.push_back(translate_refs(arg, Env, depth));

            return Runtime::App(e.head, args);
        }
        else if constexpr (std::is_same_v<T, Runtime::Trim>)
        {
            auto env_size = Env.size();
            auto body = translate_refs(e.body, Env, depth);

            if (Env.size() != env_size)
                throw myexception()<<"Cannot translate non-pinned register reference inside trimmed runtime expression";

            return Runtime::Trim(e.indices, body);
        }
        else
            std::abort();
    });
}
