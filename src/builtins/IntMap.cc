#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/expression/index_var.H"
#include "computation/expression/constructor.H"
#include "computation/expression/bool.H"

#include "computation/machine/gcobject.H"

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

extern "C" closure builtin_function_erase(OperationArgs& Args)
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

        m.erase(key);
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

