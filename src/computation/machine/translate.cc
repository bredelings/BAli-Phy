#include "graph_register.H"
#include "computation/runtime/ast.H"
#include "util/myexception.H"

using std::vector;

// Translates references inside a structural trim without changing its projection.
Runtime::TrimmedExp reg_heap::translate_refs(const Runtime::TrimmedExp& E)
{
    return {E.indices, translate_refs(E.body)};
}

Runtime::Exp reg_heap::translate_refs(const Runtime::Exp& E)
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
            return Runtime::Lambda(translate_refs(e.body));
        }
        else if constexpr (std::is_same_v<T, Runtime::Let>)
        {
            vector<Runtime::Bind> binds;
            for(const auto& bind: e.binds)
            {
                if (auto nonrec = std::get_if<Runtime::NonRec>(&bind))
                    binds.push_back(Runtime::NonRec{translate_refs(nonrec->rhs)});
                else
                {
                    vector<Runtime::TrimmedExp> rhss;
                    for(const auto& rhs: std::get<Runtime::Rec>(bind).rhss)
                        rhss.push_back(translate_refs(rhs));
                    binds.push_back(Runtime::Rec(std::move(rhss)));
                }
            }
            return Runtime::Let(std::move(binds), translate_refs(e.body));
        }
        else if constexpr (std::is_same_v<T, Runtime::Case>)
        {
            vector<Runtime::Alt> alts;
            for(const auto& alt: e.alts)
                alts.push_back(Runtime::Alt(alt.pattern, translate_refs(alt.body)));

            return Runtime::Case(translate_refs(e.object), alts);
        }
        else if constexpr (std::is_same_v<T, Runtime::FunctionApp>)
        {
            auto head = translate_refs(e.head);

            vector<Runtime::Exp> args;
            for(const auto& arg: e.args)
                args.push_back(translate_refs(arg));

            return Runtime::FunctionApp(head, args);
        }
        else if constexpr (std::is_same_v<T, Runtime::ConstructorApp>)
        {
            vector<Runtime::Exp> args;
            for(const auto& arg: e.args)
                args.push_back(translate_refs(arg));

            return Runtime::ConstructorApp(e.head, args);
        }
        else if constexpr (std::is_same_v<T, Runtime::OperationApp>)
        {
            vector<Runtime::Exp> args;
            for(const auto& arg: e.args)
                args.push_back(translate_refs(arg));

            return Runtime::OperationApp(e.head, e.lib_name, e.func_name, e.call_conv, args);
        }
        else
            std::abort();
    });
}
