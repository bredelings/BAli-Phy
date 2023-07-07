#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/expression/index_var.H"
#include "computation/expression/constructor.H"
#include "computation/expression/bool.H"

#include "computation/machine/gcobject.H"
#include "immer/set.hpp"
#include <map>

typedef Box<immer::set<int>> IntSet;

typedef Box<std::map<int,expression_ref>> EIntMap;

extern "C" closure builtin_function_empty(OperationArgs& Args)
{
    Args.evaluate(0);

    IntMap m;

    return {m};
}

extern "C" closure builtin_function_singleton(OperationArgs& Args)
{
    int key = Args.evaluate(0).as_int();

    int v_reg = Args.current_closure().reg_for_slot(1);

    IntMap m;

    m.insert(key,v_reg);

    return {m};
}

extern "C" closure builtin_function_size(OperationArgs& Args)
{
    auto& m = Args.evaluate(0).as_<IntMap>();

    int s = m.size();

    return {s};
}

extern "C" closure builtin_function_has_key(OperationArgs& Args)
{
    int key = Args.evaluate(0).as_int();

    auto& m = Args.evaluate(1).as_<IntMap>();

    int result = m.has_key(key)?1:0;

    return {result};
}

extern "C" closure builtin_function_subscript(OperationArgs& Args)
{
    int key = Args.evaluate(1).as_int();

    auto& m = Args.evaluate(0).as_<IntMap>();

    if (not m.has_key(key))
        throw myexception()<<"IntMap.!: key "<<key<<" not found in map of size "<<m.size();

    int result_reg = m[key];

    return {index_var(0), {result_reg}};
}

extern "C" closure builtin_function_map(OperationArgs& Args)
{
    const closure& C = Args.current_closure();

    int f_reg = C.reg_for_slot(0);

    auto& m = Args.evaluate(1).as_<IntMap>();

    expression_ref apply_E;
    {
	expression_ref fE = index_var(1);
	expression_ref argE = index_var(0);
	apply_E = {fE, argE};
    }

    IntMap m2;

    for(auto& [k,r1]: m)
    {
        int r2 = Args.allocate({apply_E,{f_reg,r1}});
        m2.insert(k,r2);
    }

    return m2;
}

extern "C" closure builtin_function_delete(OperationArgs& Args)
{
    int key = Args.evaluate(0).as_int();

    auto m = Args.evaluate(1).as_<IntMap>();

    m.erase(key);

    return m;
}


extern "C" closure builtin_function_insert(OperationArgs& Args)
{
    int key = Args.evaluate(0).as_int();

    int v_reg = Args.current_closure().reg_for_slot(1);

    auto m = Args.evaluate(2).as_<IntMap>();

    m.insert(key,v_reg);

    return m;
}

extern "C" closure builtin_function_insertWith(OperationArgs& Args)
{
    auto& C = Args.current_closure();

    int f_reg = C.reg_for_slot(0);
    int key   = Args.evaluate(1).as_int();
    int v2_reg = C.reg_for_slot(2);

    auto m = Args.evaluate(3).as_<IntMap>();

    if (m.has_key(key))
    {
        int v1_reg = m[key];
        expression_ref E =  {index_var(2), index_var(1), index_var(0)};
        int v3_reg = Args.allocate({E,{f_reg,v1_reg,v2_reg}});
        m.insert(key,v3_reg);
    }
    else 
        m.insert(key, v2_reg);

    return m;
}

extern "C" closure builtin_function_keys(OperationArgs& Args)
{
    auto& m = Args.evaluate(0).as_<IntMap>();

    EVector V;

    for(auto& [k,v]: m)
        V.push_back(k);

    return V;
}

extern "C" closure builtin_function_union(OperationArgs& Args)
{
    auto& m1 = Args.evaluate(0).as_<IntMap>();
    auto& m2 = Args.evaluate(1).as_<IntMap>();

    // Loop over the smaller map
    if (m2.size() < m1.size())
    {
        // Prefer the left value on collisions
        auto m3 = m1;
        for(auto& [k,v]: m2)
        {
            if (not m3.has_key(k))
                m3.insert(k,v);
        }
        return m3;
    }
    else
    {
        // Prefer the left value on collisions
        auto m3 = m2;
        for(auto& [k,v]: m1)
            m3.insert(k,v);
        return m3;
    }
}

extern "C" closure builtin_function_unionWith(OperationArgs& Args)
{
    auto& C = Args.current_closure();

    int f_reg = C.reg_for_slot(0);
    auto& m1 = Args.evaluate(1).as_<IntMap>();
    auto& m2 = Args.evaluate(2).as_<IntMap>();

    // optimize: loop over whichever of m1 or m2 is smaller
    if (m2.size() < m1.size())
    {
        auto m3 = m1;
        for(auto& [key, v2_reg]: m2)
        {
            if (m3.has_key(key))
            {
                int v1_reg = m3[key];
                expression_ref E =  {index_var(2), index_var(1), index_var(0)};
                int v3_reg = Args.allocate({E, {f_reg, v1_reg, v2_reg}});
                m3.insert(key, v3_reg);
            }
            else
                m3.insert(key, v2_reg);
        }

        return m3;
    }
    else
    {
        auto m3 = m2;
        for(auto& [key, v1_reg]: m1)
        {
            if (m3.has_key(key))
            {
                int v2_reg = m3[key];
                expression_ref E =  {index_var(2), index_var(1), index_var(0)};
                int v3_reg = Args.allocate({E, {f_reg, v1_reg, v2_reg}});
                m3.insert(key, v3_reg);
            }
            else
                m3.insert(key, v1_reg);
        }

        return m3;
    }
}

extern "C" closure builtin_function_difference(OperationArgs& Args)
{
    auto& m1 = Args.evaluate(0).as_<IntMap>();
    auto& m2 = Args.evaluate(1).as_<IntMap>();

    // Loop over the smaller map
    if (m1.size() < m2.size())
    {
        IntMap m3;
        for(auto& [k,v]: m1)
        {
            if (not m2.has_key(k))
                m3.insert(k,v);
        }
        return m3;
    }
    else
    {
        // Prefer the left value on collisions
        auto m3 = m1;
        for(auto& [k,v]: m2)
            m3.erase(k);
        return m3;
    }
}

extern "C" closure builtin_function_disjoint(OperationArgs& Args)
{
    auto& m1 = Args.evaluate(0).as_<IntMap>();
    auto& m2 = Args.evaluate(1).as_<IntMap>();

    // Loop over the smaller map
    expression_ref E = 1;
    if (m1.size() < m2.size())
    {
        for(auto& [k,v]: m1)
            if (m2.has_key(k))
                E = 0;
    }
    else
    {
        for(auto& [k,v]: m2)
            if (m1.has_key(k))
                E = 0;
    }
    return E;
}

extern "C" closure builtin_function_intersection(OperationArgs& Args)
{
    auto& m1 = Args.evaluate(0).as_<IntMap>();
    auto& m2 = Args.evaluate(1).as_<IntMap>();

    // Left-biased intersection - use the value from the left map

    // Loop over the smaller map
    IntMap m3;
    if (m1.size() < m2.size())
    {
        for(auto& [k,v1]: m1)
            if (m2.has_key(k))
                m3.insert(k,v1);
    }
    else
    {
        for(auto& [k,v2]: m2)
            if (m1.has_key(k))
            {
                int v1 = m1[k];
                m3.insert(k,v1);
            }
    }
    return m3;
}

extern "C" closure builtin_function_intersectionWith(OperationArgs& Args)
{
    int f_reg = Args.current_closure().reg_for_slot(0);
    auto& m1 = Args.evaluate(1).as_<IntMap>();
    auto& m2 = Args.evaluate(2).as_<IntMap>();

    expression_ref E =  {index_var(2), index_var(1), index_var(0)};

    // Loop over the smaller map
    IntMap m3;
    if (m1.size() < m2.size())
    {
        for(auto& [k,v1_reg]: m1)
        {
            if (m2.has_key(k))
            {
                int v2_reg = m2[k];
                int v3_reg = Args.allocate({E, {f_reg, v1_reg, v2_reg}});
                m3.insert(k, v3_reg);
            }
        }

    }
    else
    {
        for(auto& [k,v2_reg]: m2)
        {
            if (m1.has_key(k))
            {
                int v1_reg = m1[k];
                int v3_reg = Args.allocate({E, {f_reg, v1_reg, v2_reg}});
                m3.insert(k, v3_reg);
            }
        }

    }
    return m3;
}

extern "C" closure builtin_function_fromSet(OperationArgs& Args)
{
    const closure& C = Args.current_closure();

    int f_reg = C.reg_for_slot(0);

    auto& S = Args.evaluate(1).as_<IntSet>();

    expression_ref apply_E;
    {
	expression_ref fE = index_var(1);
	expression_ref argE = index_var(0);
	apply_E = {fE, argE};
    }

    IntMap m;
    for(auto& k: S)
    {
        int r1 = Args.allocate(expression_ref(k));
        int r2 = Args.allocate({apply_E,{f_reg,r1}});
        m.insert(k,r2);
    }

    return m;
}

extern "C" closure builtin_function_keysSet(OperationArgs& Args)
{
    auto& m = Args.evaluate(0).as_<IntMap>();

    IntSet keys;

    for(auto& [k,v]: m)
        keys = keys.insert(k);

    return keys;
}

extern "C" closure builtin_function_exportIntMap(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& map1 = arg0.as_<IntMap>();

    auto result = object_ptr<EIntMap>(new EIntMap);
    auto& map2 = *result;

    for(auto& [key,r_value]: map1)
    {
        auto value = Args.evaluate_reg_to_object(r_value);
        map2.insert({key, value});
    }

    return result;
}

extern "C" closure builtin_function_esubscript(OperationArgs& Args)
{
    int key = Args.evaluate(1).as_int();

    auto& m = Args.evaluate(0).as_<EIntMap>();

    return m.at(key);
}

extern "C" closure builtin_function_ekeysSet(OperationArgs& Args)
{
    auto& m = Args.evaluate(0).as_<EIntMap>();

    IntSet keys;

    for(auto& [k,v]: m)
        keys = keys.insert(k);

    return keys;
}
