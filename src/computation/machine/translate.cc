#include "graph_register.H"
#include "computation/runtime/ast.H"
#include "util/myexception.H"

using std::vector;

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
            bool pinned = reg_is_pinned(e.target);
            assert(pinned);
            if (not pinned)
                throw myexception()<<"Runtime RegRef refers to unpinned register "<<e.target;

            return e;
        }
        else if constexpr (std::is_same_v<T, Runtime::Int> or
                           std::is_same_v<T, Runtime::Double> or
                           std::is_same_v<T, Runtime::LogDouble> or
                           std::is_same_v<T, Runtime::Char> or
                           std::is_same_v<T, Runtime::String> or
                           std::is_same_v<T, Runtime::Integer> or
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
            if (auto nonrec = e.to_nonrec())
                return Runtime::Let(Runtime::NonRec{translate_refs(nonrec->rhs, Env, depth)},
                                    translate_refs(e.body, Env, depth + 1));

            const auto& rhss = e.to_rec()->rhss;
            int n = rhss.size();
            vector<Runtime::Exp> translated;
            for(const auto& rhs: rhss)
                translated.push_back(translate_refs(rhs, Env, depth + n));
            return Runtime::Let(Runtime::Rec(std::move(translated)),
                                translate_refs(e.body, Env, depth + n));
        }
        else if constexpr (std::is_same_v<T, Runtime::Case>)
        {
            vector<Runtime::Alt> alts;
            for(const auto& alt: e.alts)
                alts.push_back(Runtime::Alt(alt.pattern, translate_refs(alt.body, Env, depth + Runtime::pattern_arity(alt.pattern))));

            return Runtime::Case(translate_refs(e.object, Env, depth), alts);
        }
        else if constexpr (std::is_same_v<T, Runtime::FunctionApp>)
        {
            auto head = translate_refs(e.head, Env, depth);

            vector<Runtime::Exp> args;
            for(const auto& arg: e.args)
                args.push_back(translate_refs(arg, Env, depth));

            return Runtime::FunctionApp(head, args);
        }
        else if constexpr (std::is_same_v<T, Runtime::ConstructorApp>)
        {
            vector<Runtime::Exp> args;
            for(const auto& arg: e.args)
                args.push_back(translate_refs(arg, Env, depth));

            return Runtime::ConstructorApp(e.head, args);
        }
        else if constexpr (std::is_same_v<T, Runtime::OperationApp>)
        {
            vector<Runtime::Exp> args;
            for(const auto& arg: e.args)
                args.push_back(translate_refs(arg, Env, depth));

            return Runtime::OperationApp(e.head, e.lib_name, e.func_name, e.call_conv, args);
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
