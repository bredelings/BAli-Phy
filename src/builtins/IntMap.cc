#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/operation.H"
#include "computation/runtime/ast.H"

#include "computation/machine/gcobject.H"
#include "immer/set.hpp"
#include <map>
#include <memory>

#include "range/v3/all.hpp"

namespace views = ranges::views;

using std::vector;

typedef Box<immer::set<int>> IntSet;

extern "C" R::Exp simple_function_empty(vector<R::Exp>& /*args*/)
{
    IntMap m;

    return m;
}

extern "C" closure builtin_function_singleton(OperationArgs& Args)
{
    int key = Args.evaluate_slot_to_value(0).as_int();

    int v_reg = Args.reg_for_slot(1);

    IntMap m;

    m.insert(key,v_reg);

    return {m};
}

extern "C" R::Exp simple_function_size(vector<R::Exp>& args)
{
    auto arg0 = get_arg(args);
    auto& m = arg0.as_<IntMap>();


    return (int)m.size();
}

extern "C" R::Exp simple_function_member(vector<R::Exp>& args)
{
    int key = get_arg(args).as_int();

    auto arg1 = get_arg(args);
    auto& m = arg1.as_<IntMap>();

    return m.has_key(key);
}

extern "C" closure builtin_function_subscript(OperationArgs& Args)
{
    int key = Args.evaluate_slot_to_value(1).as_int();

    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& m = arg0.as_<IntMap>();

    if (not m.has_key(key))
        throw myexception()<<"IntMap.!: key "<<key<<" not found in map of size "<<m.size();

    int result_reg = m[key];

    return {R::IndexVar(0), {result_reg}};
}

extern "C" closure builtin_function_map(OperationArgs& Args)
{
    int f_reg = Args.reg_for_slot(0);

    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& m = arg1.as_<IntMap>();

    IntMap m2;

    for(auto& [k,r1]: m)
    {
        int r2 = Args.allocate(closure(R::apply(R::IndexVar(1), {R::IndexVar(0)}),
                                       {f_reg, r1}));
        m2.insert(k,r2);
    }

    return m2;
}

extern "C" closure builtin_function_mapWithKey(OperationArgs& Args)
{
    int f_reg = Args.reg_for_slot(0);

    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& m = arg1.as_<IntMap>();

    IntMap m2;

    for(auto& [k,r_val]: m)
    {
        int r_key = Args.allocate({k});
        int r_val2 = Args.allocate(closure(R::apply(R::IndexVar(2), {R::IndexVar(1), R::IndexVar(0)}),
                                           {f_reg, r_key, r_val}));
        m2.insert(k,r_val2);
    }

    return m2;
}

extern "C" closure builtin_function_delete(OperationArgs& Args)
{
    int key = Args.evaluate_slot_to_value(0).as_int();

    auto m = Args.evaluate_slot_to_value(1).as_<IntMap>();

    m.erase(key);

    return m;
}


extern "C" closure builtin_function_insert(OperationArgs& Args)
{
    int key = Args.evaluate_slot_to_value(0).as_int();

    int v_reg = Args.reg_for_slot(1);

    auto m = Args.evaluate_slot_to_value(2).as_<IntMap>();

    m.insert(key,v_reg);

    return m;
}

extern "C" closure builtin_function_insertWith(OperationArgs& Args)
{
    int f_reg = Args.reg_for_slot(0);
    int key   = Args.evaluate_slot_to_value(1).as_int();
    int v2_reg = Args.reg_for_slot(2);

    auto m = Args.evaluate_slot_to_value(3).as_<IntMap>();

    if (m.has_key(key))
    {
        int v1_reg = m[key];
        int v3_reg = Args.allocate(closure(R::apply(R::IndexVar(2), {R::IndexVar(1), R::IndexVar(0)}),
                                           {f_reg, v1_reg, v2_reg}));
        m.insert(key,v3_reg);
    }
    else 
        m.insert(key, v2_reg);

    return m;
}

extern "C" closure builtin_function_keys(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& m = arg0.as_<IntMap>();

    R::RVector V;

    for(auto& [k,v]: m)
        V.push_back(k);

    return V;
}

extern "C" closure builtin_function_union(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);

    auto& m1 = arg0.as_<IntMap>();
    auto& m2 = arg1.as_<IntMap>();

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
    int f_reg = Args.reg_for_slot(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto& m1 = arg1.as_<IntMap>();
    auto& m2 = arg2.as_<IntMap>();

    // optimize: loop over whichever of m1 or m2 is smaller
    if (m2.size() < m1.size())
    {
        auto m3 = m1;
        for(auto& [key, v2_reg]: m2)
        {
            if (m3.has_key(key))
            {
                int v1_reg = m3[key];
                int v3_reg = Args.allocate(closure(R::apply(R::IndexVar(2), {R::IndexVar(1), R::IndexVar(0)}),
                                                   {f_reg, v1_reg, v2_reg}));
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
                int v3_reg = Args.allocate(closure(R::apply(R::IndexVar(2), {R::IndexVar(1), R::IndexVar(0)}),
                                                   {f_reg, v1_reg, v2_reg}));
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
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& m1 = arg0.as_<IntMap>();
    auto& m2 = arg1.as_<IntMap>();

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
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& m1 = arg0.as_<IntMap>();
    auto& m2 = arg1.as_<IntMap>();

    // Loop over the smaller map
    int E = 1;
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
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& m1 = arg0.as_<IntMap>();
    auto& m2 = arg1.as_<IntMap>();

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
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);

    int f_reg = Args.reg_for_slot(0);
    auto& m1 = arg1.as_<IntMap>();
    auto& m2 = arg2.as_<IntMap>();

    // Loop over the smaller map
    IntMap m3;
    if (m1.size() < m2.size())
    {
        for(auto& [k,v1_reg]: m1)
        {
            if (m2.has_key(k))
            {
                int v2_reg = m2[k];
                int v3_reg = Args.allocate(closure(R::apply(R::IndexVar(2), {R::IndexVar(1), R::IndexVar(0)}),
                                                   {f_reg, v1_reg, v2_reg}));
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
                int v3_reg = Args.allocate(closure(R::apply(R::IndexVar(2), {R::IndexVar(1), R::IndexVar(0)}),
                                                   {f_reg, v1_reg, v2_reg}));
                m3.insert(k, v3_reg);
            }
        }

    }
    return m3;
}

extern "C" closure builtin_function_fromSet(OperationArgs& Args)
{
    int f_reg = Args.reg_for_slot(0);

    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& S = arg1.as_<IntSet>();

    IntMap m;
    for(auto& k: S)
    {
        int r1 = Args.allocate(k);
        int r2 = Args.allocate(closure(R::apply(R::IndexVar(1), {R::IndexVar(0)}),
                                       {f_reg, r1}));
        m.insert(k,r2);
    }

    return m;
}

extern "C" closure builtin_function_keysSet(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& m = arg0.as_<IntMap>();

    IntSet keys;

    for(auto& [k,v]: m)
        keys = keys.insert(k);

    return keys;
}


extern "C" closure builtin_function_restrictKeys(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& map0 = arg0.as_<IntMap>();

    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& keys = arg1.as_<IntSet>();

    object_ptr<IntMap> result = new IntMap;;

    for(auto& k: keys)
	if (map0.has_key(k))
	    result->insert(k, map0[k]);

    return result;
}

// Build a vector from selected IntMap values directly, recording each value
// discovered through the map as a dynamic USE edge on this step.
extern "C" closure builtin_function_restrictKeysToVector(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    const auto& map0 = arg0.as_<IntMap>();

    // The order for a specific keys object should remain unchanged.
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& keys = arg1.as_<IntSet>();

    object_ptr<R::RVector> result = new R::RVector;
    result->reserve(keys.size());

    for(int key: keys)
    {
        int r = map0[key];
        int value_reg = Args.evaluate_reg_dependent_use(r);
        result->push_back(Args.memory().closure_at(value_reg).get_code());
    }
    return result;
}

extern "C" closure builtin_function_withoutKeys(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto result = arg0.as_<IntMap>();

    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& keys = arg1.as_<IntSet>();

    for(auto& k: keys)
	result.erase(k);

    return result;
}

extern "C" closure builtin_function_esubscript(OperationArgs& Args)
{
    int key = Args.evaluate_slot_to_value(1).as_int();

    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& m = arg0.as_<R::RIntMap>();

    return m.at(key);
}

extern "C" closure builtin_function_ekeysSet(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& m = arg0.as_<R::RIntMap>();

    IntSet keys;

    for(auto& [k,v]: m)
        keys = keys.insert(k);

    return keys;
}

extern "C" closure builtin_function_forceAll(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& m = arg0.as_<IntMap>();

    for(auto& [_,r]: m)
	Args.evaluate_reg_dependent_force(r);

    return closure(R::ConstructorApp("()", 0, {}));
}

extern "C" closure builtin_function_exportIntMap(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& m = arg0.as_<IntMap>();

    object_ptr<R::RIntMap>  m2(new R::RIntMap);

    // The local IntMap object is not a machine GC root, so pin all entry regs
    // while evaluating values discovered through the map.
    std::vector<int> tmp;
    for(auto& [_,reg]: m)
    {
        Args.stack_push(reg);
        tmp.push_back(reg);
    }

    // Compute the values
    for(auto& [key,reg]: m)
    {
        int value_reg = Args.evaluate_reg_dependent_use(reg);
        auto value = Args.memory().closure_at(value_reg).get_code();
	m2->insert({key, value});
    }

    // Unreserve the value regs
    for(auto& reg: tmp | views::reverse)
    {
        Args.stack_pop(reg);
    }

    return m2;
}

extern "C" closure builtin_function_toVector(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& m = arg0.as_<IntMap>();

    object_ptr<R::RVector>  v(new R::RVector);

    // The local IntMap object is not a machine GC root, so pin all entry regs
    // while evaluating values discovered through the map.
    std::vector<int> tmp;
    for(auto& [_,reg]: m)
    {
        Args.stack_push(reg);
        tmp.push_back(reg);
    }

    // Compute the values
    for(auto& [key,reg]: m)
    {
        int value_reg = Args.evaluate_reg_dependent_use(reg);
        v->push_back( Args.memory().closure_at(value_reg).get_code() );
    }

    // Unreserve the value regs
    for(auto& reg: tmp | views::reverse)
    {
        Args.stack_pop(reg);
    }

    return v;
}
