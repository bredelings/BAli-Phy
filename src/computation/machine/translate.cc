#include "graph_register.H"
#include "computation/runtime/ast.H"

using std::vector;

Runtime::ExpPtr reg_heap::translate_refs(const Runtime::ExpPtr& E, closure::Env_t& Env, int depth)
{
    return std::visit([&](const auto& e) -> Runtime::ExpPtr
    {
        using T = std::decay_t<decltype(e)>;

        if constexpr (std::is_same_v<T, Runtime::GlobalVar>)
        {
            int r = reg_for_id(e.name);
            if (reg_is_pinned(r))
                return Runtime::make(Runtime::RegRef{r});

            int index = depth + Env.size();
            Env.insert(Env.begin(), r);
            return Runtime::make(Runtime::IndexVar{index});
        }
        else if constexpr (std::is_same_v<T, Runtime::RegRef>)
        {
            if (reg_is_pinned(e.target))
                return Runtime::make(e);

            int index = depth + Env.size();
            Env.insert(Env.begin(), e.target);
            return Runtime::make(Runtime::IndexVar{index});
        }
        else if constexpr (std::is_same_v<T, Runtime::IntLiteral> or
                           std::is_same_v<T, Runtime::DoubleLiteral> or
                           std::is_same_v<T, Runtime::LogDoubleLiteral> or
                           std::is_same_v<T, Runtime::CharLiteral> or
                           std::is_same_v<T, Runtime::StringLiteral> or
                           std::is_same_v<T, Runtime::IntegerLiteral> or
                           std::is_same_v<T, Runtime::ConstructorValue> or
                           std::is_same_v<T, Runtime::IndexVar>)
        {
            return Runtime::make(e);
        }
        else if constexpr (std::is_same_v<T, Runtime::Lambda>)
        {
            return Runtime::make(Runtime::Lambda{translate_refs(e.body, Env, depth + 1)});
        }
        else if constexpr (std::is_same_v<T, Runtime::Let>)
        {
            int n = e.binds.size();

            vector<Runtime::ExpPtr> binds;
            for(const auto& bind: e.binds)
                binds.push_back(translate_refs(bind, Env, depth + n));

            return Runtime::make(Runtime::Let{binds, translate_refs(e.body, Env, depth + n)});
        }
        else if constexpr (std::is_same_v<T, Runtime::Case>)
        {
            vector<Runtime::Alt> alts;
            for(const auto& alt: e.alts)
                alts.push_back({alt.pattern, translate_refs(alt.body, Env, depth + Runtime::pattern_arity(alt.pattern))});

            return Runtime::make(Runtime::Case{translate_refs(e.object, Env, depth), alts});
        }
        else if constexpr (std::is_same_v<T, Runtime::App>)
        {
            vector<Runtime::ExpPtr> args;
            for(const auto& arg: e.args)
                args.push_back(translate_refs(arg, Env, depth));

            return Runtime::make(Runtime::App{e.head, args});
        }
        else if constexpr (std::is_same_v<T, Runtime::Trim>)
        {
            return Runtime::make(Runtime::Trim{e.indices, translate_refs(e.body, Env, depth)});
        }
        else
            std::abort();
    }, E->value);
}
