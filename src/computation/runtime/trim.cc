#include "trim.H"

using std::vector;

namespace Runtime
{
namespace
{
    // Removes locally bound indices and shifts the remaining sorted free indices.
    vector<int> pop_vars(int n, vector<int> vars)
    {
        assert(n >= 0);
        if (n == 0) return vars;

        for(int& var: vars)
            var -= n;
        while(vars.size() and vars[0] < 0)
            vars.erase(vars.begin());
        return vars;
    }

    // Merges two sorted sets of free indices without introducing duplicates.
    vector<int> merge_vars(const vector<int>& v1, const vector<int>& v2)
    {
        int i = 0;
        int j = 0;
        vector<int> result;

        while(i < v1.size() or j < v2.size())
        {
            if (i >= v1.size())
                result.push_back(v2[j++]);
            else if (j >= v2.size())
                result.push_back(v1[i++]);
            else if (v1[i] < v2[j])
                result.push_back(v1[i++]);
            else if (v1[i] > v2[j])
                result.push_back(v2[j++]);
            else
            {
                result.push_back(v1[i]);
                i++;
                j++;
            }
        }

        return result;
    }

    // Rebuilds an application after remapping both its head and arguments.
    Exp rebuild_app(const FunctionApp&, Exp head, vector<Exp> args)
    {
        return FunctionApp(std::move(head), std::move(args));
    }

    // Rebuilds a constructor application after remapping its arguments.
    Exp rebuild_app(const ConstructorApp& app, vector<Exp> args)
    {
        return ConstructorApp(app.head, std::move(args));
    }

    // Rebuilds an operation application after remapping its arguments.
    Exp rebuild_app(const OperationApp& app, vector<Exp> args)
    {
        return OperationApp(app.head, app.lib_name, app.func_name,
                            app.call_conv, std::move(args));
    }

    vector<int> get_free_index_vars(const Exp&);

    // Exposes a trimmed boundary's original free indices and checks its dense body.
    vector<int> get_free_index_vars(const TrimmedExp& E)
    {
#ifndef NDEBUG
        auto vars = get_free_index_vars(E.body);
        assert(E.indices.size() == vars.size());
        for(int i = 0; i < vars.size(); i++)
            assert(vars[i] == i);
#endif
        return E.indices;
    }

    // Finds the sorted free-index set while respecting each structural trim boundary.
    vector<int> get_free_index_vars(const Exp& E)
    {
        // This visitor combines free indices according to each Runtime scope.
        return E.visit([](const auto& e) -> vector<int>
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, Int> or
                          std::is_same_v<T, Double> or
                          std::is_same_v<T, LogDouble> or
                          std::is_same_v<T, Char> or
                          std::is_same_v<T, String> or
                          std::is_same_v<T, Integer> or
                          std::is_same_v<T, ObjectValue> or
                          std::is_same_v<T, GlobalVar> or
                          std::is_same_v<T, RegRef>)
            {
                return {};
            }
            else if constexpr (std::is_same_v<T, IndexVar>)
            {
                return {e.index};
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                return pop_vars(1, get_free_index_vars(e.body));
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                auto vars = get_free_index_vars(e.body);
                for(auto bind = e.binds.rbegin(); bind != e.binds.rend(); ++bind)
                {
                    if (auto nonrec = std::get_if<NonRec>(&*bind))
                        vars = merge_vars(get_free_index_vars(nonrec->rhs),
                                          pop_vars(1, std::move(vars)));
                    else
                    {
                        const auto& rhss = std::get<Rec>(*bind).rhss;
                        for(const auto& rhs: rhss)
                            vars = merge_vars(vars, get_free_index_vars(rhs));
                        vars = pop_vars(rhss.size(), std::move(vars));
                    }
                }
                return vars;
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                auto vars = get_free_index_vars(e.object);
                for(const auto& alt: e.alts)
                {
                    int n = pattern_arity(alt.pattern);
                    vars = merge_vars(vars,
                                      pop_vars(n, get_free_index_vars(alt.body)));
                }
                return vars;
            }
            else if constexpr (std::is_same_v<T, FunctionApp> or
                               std::is_same_v<T, ConstructorApp> or
                               std::is_same_v<T, OperationApp>)
            {
                vector<int> vars;
                if constexpr (std::is_same_v<T, FunctionApp>)
                    vars = get_free_index_vars(e.head);
                for(const auto& arg: e.args)
                    vars = merge_vars(vars, get_free_index_vars(arg));
                return vars;
            }
            else
                std::abort();
        });
    }

    // Constructs a trim whose body uses exactly the dense indices it declares.
    TrimmedExp make_trimmed(Exp E, vector<int> indices)
    {
#ifndef NDEBUG
        auto vars = get_free_index_vars(E);
        assert(indices.size() == vars.size());
        for(int i = 0; i < vars.size(); i++)
            assert(vars[i] == i);
#endif
        return {std::move(indices), std::move(E)};
    }

    // Remaps only a structural boundary's projection; its body remains dense.
    TrimmedExp remap_free_indices(const TrimmedExp& E,
                                  const vector<int>& mapping, int depth)
    {
        auto indices = E.indices;
        for(int& index: indices)
        {
            int delta = index - depth;
            if (delta >= 0)
            {
                assert(delta < mapping.size());
                assert(mapping[delta] != -1);
                index = depth + mapping[delta];
            }
        }
        return {std::move(indices), E.body};
    }

    // Remaps free indices through an expression while preserving local binders.
    Exp remap_free_indices(const Exp& E, const vector<int>& mapping, int depth)
    {
        // This visitor advances depth through each Runtime binding construct.
        return E.visit([&](const auto& e) -> Exp
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, Int> or
                          std::is_same_v<T, Double> or
                          std::is_same_v<T, LogDouble> or
                          std::is_same_v<T, Char> or
                          std::is_same_v<T, String> or
                          std::is_same_v<T, Integer> or
                          std::is_same_v<T, ObjectValue> or
                          std::is_same_v<T, GlobalVar> or
                          std::is_same_v<T, RegRef>)
            {
                return E;
            }
            else if constexpr (std::is_same_v<T, IndexVar>)
            {
                int delta = e.index - depth;
                if (delta < 0)
                    return E;

                assert(delta < mapping.size());
                assert(mapping[delta] != -1);
                return IndexVar(depth + mapping[delta]);
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                return Lambda(remap_free_indices(e.body, mapping, depth + 1));
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                vector<Bind> binds;
                int bind_depth = depth;
                for(const auto& bind: e.binds)
                {
                    if (auto nonrec = std::get_if<NonRec>(&bind))
                    {
                        binds.push_back(NonRec{remap_free_indices(
                            nonrec->rhs, mapping, bind_depth)});
                        bind_depth++;
                    }
                    else
                    {
                        const auto& rhss = std::get<Rec>(bind).rhss;
                        int n = rhss.size();
                        vector<TrimmedExp> remapped;
                        for(const auto& rhs: rhss)
                            remapped.push_back(remap_free_indices(
                                rhs, mapping, bind_depth + n));
                        binds.push_back(Rec(std::move(remapped)));
                        bind_depth += n;
                    }
                }
                return Let(std::move(binds), remap_free_indices(
                    e.body, mapping, bind_depth));
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                vector<Alt> alts;
                for(const auto& alt: e.alts)
                {
                    int n = pattern_arity(alt.pattern);
                    alts.push_back(Alt(alt.pattern, remap_free_indices(
                        alt.body, mapping, depth + n)));
                }
                return Case(remap_free_indices(e.object, mapping, depth),
                            std::move(alts));
            }
            else if constexpr (std::is_same_v<T, FunctionApp> or
                               std::is_same_v<T, ConstructorApp> or
                               std::is_same_v<T, OperationApp>)
            {
                vector<Exp> args;
                for(const auto& arg: e.args)
                    args.push_back(remap_free_indices(arg, mapping, depth));

                if constexpr (std::is_same_v<T, FunctionApp>)
                    return rebuild_app(e, remap_free_indices(
                        e.head, mapping, depth), std::move(args));
                else
                    return rebuild_app(e, std::move(args));
            }
            else
                std::abort();
        });
    }
}

    // Projects an expression's free variables into a dense closure environment.
    TrimmedExp trim(const Exp& E)
    {
        auto indices = get_free_index_vars(E);
        vector<int> mapping;

        if (indices.size())
        {
            mapping = vector<int>(indices.back() + 1, -1);
            for(int i = 0; i < indices.size(); i++)
                mapping[indices[i]] = i;
        }

        return make_trimmed(remap_free_indices(E, mapping, 0),
                            std::move(indices));
    }
}
