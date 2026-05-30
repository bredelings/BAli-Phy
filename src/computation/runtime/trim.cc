#include "trim.H"
#include "computation/expression/constructor.H"
#include "computation/expression/index_var.H"

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

    int pattern_arity(const expression_ref& pattern)
    {
        if (pattern.head().is_a<constructor>())
            return pattern.head().as_<constructor>().n_args();
        else
            return 0;
    }

    vector<int> get_free_index_vars(const ExpPtr& E)
    {
        return std::visit([](const auto& e) -> vector<int>
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, Atom>)
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
        }, E->value);
    }

    ExpPtr make_trim(const ExpPtr& E, const vector<int>& indices)
    {
#ifndef NDEBUG
        auto vars = get_free_index_vars(E);
        assert(indices.size() == vars.size());
        for(int i = 0; i < vars.size(); i++)
            assert(vars[i] == i);
#endif
        return make(Trim{indices, E});
    }

    ExpPtr remap_free_indices(const ExpPtr& E, const vector<int>& mapping, int depth)
    {
        return std::visit([&](const auto& e) -> ExpPtr
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, Atom>)
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
                    return make(IndexVar{depth + mapping[delta]});
                }

                return E;
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                return make(Lambda{remap_free_indices(e.body, mapping, depth + 1)});
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                int n = e.binds.size();

                vector<ExpPtr> binds;
                for(const auto& bind: e.binds)
                    binds.push_back(remap_free_indices(bind, mapping, depth + n));

                return make(Let{binds, remap_free_indices(e.body, mapping, depth + n)});
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                vector<Alt> alts;

                for(const auto& alt: e.alts)
                {
                    int n = pattern_arity(alt.pattern);
                    alts.push_back({alt.pattern, remap_free_indices(alt.body, mapping, depth + n)});
                }

                return make(Case{remap_free_indices(e.object, mapping, depth), alts});
            }
            else if constexpr (std::is_same_v<T, App>)
            {
                vector<ExpPtr> args;
                for(const auto& arg: e.args)
                    args.push_back(remap_free_indices(arg, mapping, depth));

                return make(App{e.head, args});
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
        }, E->value);
    }

    ExpPtr trim(const ExpPtr& E)
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

    ExpPtr trim_normalize(const ExpPtr& E)
    {
        return std::visit([](const auto& e) -> ExpPtr
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, Atom>)
            {
                return make(e);
            }
            else if constexpr (std::is_same_v<T, IndexVar>)
            {
                return make(e);
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                return make(Lambda{trim_normalize(e.body)});
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                vector<ExpPtr> binds;
                for(const auto& bind: e.binds)
                    binds.push_back(trim(trim_normalize(bind)));

                return make(Let{binds, trim(trim_normalize(e.body))});
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                vector<Alt> alts;
                for(const auto& alt: e.alts)
                    alts.push_back({alt.pattern, trim(trim_normalize(alt.body))});

                return make(Case{e.object, alts});
            }
            else if constexpr (std::is_same_v<T, App>)
            {
                vector<ExpPtr> args;
                for(const auto& arg: e.args)
                    args.push_back(trim_normalize(arg));

                return make(App{e.head, args});
            }
            else if constexpr (std::is_same_v<T, Trim>)
            {
                return make(Trim{e.indices, trim_normalize(e.body)});
            }
            else
                std::abort();
        }, E->value);
    }
}
