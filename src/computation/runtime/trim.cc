#include "trim.H"

using std::vector;

namespace Runtime
{
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

    vector<int> merge_vars(const vector<int>& v1, const vector<int>& v2)
    {
        int i = 0;
        int j = 0;
        vector<int> v3;

        while(i < v1.size() or j < v2.size())
        {
            if (i >= v1.size())
                v3.push_back(v2[j++]);
            else if (j >= v2.size())
                v3.push_back(v1[i++]);
            else if (v1[i] < v2[j])
                v3.push_back(v1[i++]);
            else if (v1[i] > v2[j])
                v3.push_back(v2[j++]);
            else
            {
                assert(v1[i] == v2[j]);
                v3.push_back(v1[i]);
                i++;
                j++;
            }
        }

        assert(v3.size() >= v1.size());
        assert(v3.size() >= v2.size());
        return v3;
    }

    vector<int> get_free_index_vars(const Exp& E)
    {
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
                          std::is_same_v<T, Constructor>)
            {
                return {};
            }
            else if constexpr (std::is_same_v<T, IndexVar>)
            {
                return {e.index};
            }
            else if constexpr (std::is_same_v<T, GlobalVar> or std::is_same_v<T, RegRef>)
            {
                return {};
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                return pop_vars(1, get_free_index_vars(e.body));
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                auto vars = get_free_index_vars(e.body);

                for(const auto& bind: e.binds)
                    vars = merge_vars(vars, get_free_index_vars(bind));

                return pop_vars(e.binds.size(), vars);
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                auto vars = get_free_index_vars(e.object);

                for(const auto& alt: e.alts)
                {
                    int n = pattern_arity(alt.pattern);
                    vars = merge_vars(vars, pop_vars(n, get_free_index_vars(alt.body)));
                }

                return vars;
            }
            else if constexpr (std::is_same_v<T, App>)
            {
                vector<int> vars;
                for(const auto& arg: e.args)
                    vars = merge_vars(vars, get_free_index_vars(arg));
                return vars;
            }
            else if constexpr (std::is_same_v<T, Trim>)
            {
#ifndef NDEBUG
                auto vars = get_free_index_vars(e.body);
                assert(e.indices.size() == vars.size());
                for(int i = 0; i < vars.size(); i++)
                    assert(vars[i] == i);
#endif
                return e.indices;
            }
            else
                std::abort();
        });
    }

    Exp make_trim(const Exp& E, const vector<int>& indices)
    {
#ifndef NDEBUG
        auto vars = get_free_index_vars(E);
        assert(indices.size() == vars.size());
        for(int i = 0; i < vars.size(); i++)
            assert(vars[i] == i);
#endif
        return Trim(indices, E);
    }

    Exp remap_free_indices(const Exp& E, const vector<int>& mapping, int depth)
    {
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
                          std::is_same_v<T, Constructor>)
            {
                return E;
            }
            else if constexpr (std::is_same_v<T, IndexVar>)
            {
                int delta = e.index - depth;
                if (delta >= 0)
                {
                    assert(delta < mapping.size());
                    assert(mapping[delta] != -1);
                    return IndexVar(depth + mapping[delta]);
                }

                return E;
            }
            else if constexpr (std::is_same_v<T, GlobalVar> or std::is_same_v<T, RegRef>)
            {
                return E;
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                return Lambda(remap_free_indices(e.body, mapping, depth + 1));
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                int n = e.binds.size();

                vector<Exp> binds;
                for(const auto& bind: e.binds)
                    binds.push_back(remap_free_indices(bind, mapping, depth + n));

                return Let(binds, remap_free_indices(e.body, mapping, depth + n));
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                vector<Alt> alts;

                for(const auto& alt: e.alts)
                {
                    int n = pattern_arity(alt.pattern);
                    alts.push_back(Alt(alt.pattern, remap_free_indices(alt.body, mapping, depth + n)));
                }

                return Case(remap_free_indices(e.object, mapping, depth), alts);
            }
            else if constexpr (std::is_same_v<T, App>)
            {
                vector<Exp> args;
                for(const auto& arg: e.args)
                    args.push_back(remap_free_indices(arg, mapping, depth));

                return App(e.head, args);
            }
            else if constexpr (std::is_same_v<T, Trim>)
            {
                auto indices = e.indices;

                for(auto& index: indices)
                {
                    int delta = index - depth;
                    if (delta >= 0)
                    {
                        assert(delta < mapping.size());
                        assert(mapping[delta] != -1);
                        index = depth + mapping[delta];
                    }
                }

                return make_trim(e.body, indices);
            }
            else
                std::abort();
        });
    }

    Exp trim(const Exp& E)
    {
        auto indices = get_free_index_vars(E);

        vector<int> mapping;

        if (indices.size())
        {
            mapping = vector<int>(indices.back() + 1, -1);
            for(int i = 0; i < indices.size(); i++)
                mapping[indices[i]] = i;
        }

        return make_trim(remap_free_indices(E, mapping, 0), indices);
    }

    Exp untrim(const Exp& E)
    {
        if (auto trim = E.to<Trim>())
            return remap_free_indices(trim->body, trim->indices, 0);
        else
            return E;
    }

    Exp trim_normalize(const Exp& E)
    {
        return E.visit([](const auto& e) -> Exp
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, Int> or
                          std::is_same_v<T, Double> or
                          std::is_same_v<T, LogDouble> or
                          std::is_same_v<T, Char> or
                          std::is_same_v<T, String> or
                          std::is_same_v<T, Integer> or
                          std::is_same_v<T, ObjectValue> or
                          std::is_same_v<T, Constructor>)
            {
                return e;
            }
            else if constexpr (std::is_same_v<T, IndexVar>)
            {
                return e;
            }
            else if constexpr (std::is_same_v<T, GlobalVar> or std::is_same_v<T, RegRef>)
            {
                return e;
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                return Lambda(trim_normalize(e.body));
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                vector<Exp> binds;
                for(const auto& bind: e.binds)
                    binds.push_back(trim(trim_normalize(bind)));

                return Let(binds, trim(trim_normalize(e.body)));
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                vector<Alt> alts;
                for(const auto& alt: e.alts)
                    alts.push_back(Alt(alt.pattern, trim(trim_normalize(alt.body))));

                return Case(e.object, alts);
            }
            else if constexpr (std::is_same_v<T, App>)
            {
                vector<Exp> args;
                for(const auto& arg: e.args)
                    args.push_back(trim_normalize(arg));

                return App(e.head, args);
            }
            else if constexpr (std::is_same_v<T, Trim>)
            {
                return Trim(e.indices, trim_normalize(e.body));
            }
            else
                std::abort();
        });
    }

    Exp trim_unnormalize(const Exp& E)
    {
        return E.visit([](const auto& e) -> Exp
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, Int> or
                          std::is_same_v<T, Double> or
                          std::is_same_v<T, LogDouble> or
                          std::is_same_v<T, Char> or
                          std::is_same_v<T, String> or
                          std::is_same_v<T, Integer> or
                          std::is_same_v<T, ObjectValue> or
                          std::is_same_v<T, Constructor> or
                          std::is_same_v<T, IndexVar> or
                          std::is_same_v<T, GlobalVar> or
                          std::is_same_v<T, RegRef>)
            {
                return e;
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                return Lambda(trim_unnormalize(untrim(e.body)));
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                vector<Exp> binds;
                for(const auto& bind: e.binds)
                    binds.push_back(trim_unnormalize(untrim(bind)));

                return Let(binds, trim_unnormalize(untrim(e.body)));
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                vector<Alt> alts;
                for(const auto& alt: e.alts)
                    alts.push_back(Alt(alt.pattern, trim_unnormalize(untrim(alt.body))));

                return Case(e.object, alts);
            }
            else if constexpr (std::is_same_v<T, App>)
            {
                vector<Exp> args;
                for(const auto& arg: e.args)
                    args.push_back(trim_unnormalize(untrim(arg)));

                return App(e.head, args);
            }
            else if constexpr (std::is_same_v<T, Trim>)
            {
                return trim_unnormalize(remap_free_indices(e.body, e.indices, 0));
            }
            else
                std::abort();
        });
    }
}
