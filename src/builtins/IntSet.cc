#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/expression/index_var.H"
#include "computation/expression/constructor.H"
#include "computation/expression/bool.H"

#include "immer/set.hpp"

typedef Box<immer::set<int>> IntSet;

extern "C" closure builtin_function_empty(OperationArgs& Args)
{
    IntSet S;

    return {S};
}

extern "C" closure builtin_function_singleton(OperationArgs& Args)
{
    int key = Args.evaluate(0).as_int();

    IntSet S;

    S = S.insert(key);

    return {S};
}

extern "C" closure builtin_function_size(OperationArgs& Args)
{
    auto& S = Args.evaluate(0).as_<IntSet>();

    int s = S.size();

    return {s};
}

extern "C" closure builtin_function_member(OperationArgs& Args)
{
    int e= Args.evaluate(0).as_int();

    auto& S = Args.evaluate(1).as_<IntSet>();

    if (S.find(e))
        return bool_true;
    else
        return bool_false;
}

extern "C" closure builtin_function_delete(OperationArgs& Args)
{
    int e = Args.evaluate(0).as_int();

    auto S = Args.evaluate(1).as_<IntSet>();

    S = S.erase(e);

    return S;
}


extern "C" closure builtin_function_insert(OperationArgs& Args)
{
    int e = Args.evaluate(0).as_int();

    auto S = Args.evaluate(1).as_<IntSet>();

    S = S.insert(e);

    return S;
}

extern "C" closure builtin_function_keys(OperationArgs& Args)
{
    auto& S = Args.evaluate(0).as_<IntSet>();

    EVector V;

    for(auto& k: S)
        V.push_back(k);

    return V;
}

extern "C" closure builtin_function_union(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& S1 = arg0.as_<IntSet>();
    auto arg1 = Args.evaluate(1);
    auto& S2 = arg1.as_<IntSet>();

    // Loop over the smaller map
    if (S2.size() < S1.size())
    {
        auto S3 = S1;
        for(auto& k: S2)
            S3 = S3.insert(k);
        return S3;
    }
    else
    {
        auto S3 = S2;
        for(auto& k: S1)
            S3 = S3.insert(k);
        return S3;
    }
}

extern "C" closure builtin_function_difference(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& S1 = arg0.as_<IntSet>();
    auto arg1 = Args.evaluate(1);
    auto& S2 = arg1.as_<IntSet>();

    // Loop over the smaller map
    if (S1.size() < S2.size())
    {
        IntSet S3;
        for(auto& k: S1)
            if (not S2.find(k))
                S3 = S3.insert(k);
        return S3;
    }
    else
    {
        auto S3 = S1;
        for(auto& k: S2)
            S3 = S3.erase(k);
        return S3;
    }
}

extern "C" closure builtin_function_isSubsetOf(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& S1 = arg0.as_<IntSet>();
    auto arg1 = Args.evaluate(1);
    auto& S2 = arg1.as_<IntSet>();

    for(auto k: S1)
        if (not S2.find(k))
            return bool_false;

    return bool_true;
}


extern "C" closure builtin_function_disjoint(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& S1 = arg0.as_<IntSet>();
    auto arg1 = Args.evaluate(1);
    auto& S2 = arg1.as_<IntSet>();

    // Loop over the smaller map
    bool disjoint = true;
    if (S1.size() < S2.size())
    {
        for(auto& k: S1)
            if (S2.find(k))
                disjoint = false;
    }
    else
    {
        for(auto& k: S2)
            if (S1.find(k))
                disjoint = false;
    }

    if (disjoint)
        return bool_true;
    else
        return bool_false;
}

extern "C" closure builtin_function_intersection(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& S1 = arg0.as_<IntSet>();
    auto arg1 = Args.evaluate(1);
    auto& S2 = arg1.as_<IntSet>();

    // Optimized for the case when one set is very small.
    IntSet S3;
    if (S1.size() < S2.size())
    {
        for(auto& k: S1)
            if (S2.find(k))
                S3 = S3.insert(k);
    }
    else
    {
        for(auto& k: S2)
            if (S1.find(k))
                S3 = S3.insert(k);
    }
    return S3;
}

extern "C" closure builtin_function_mapNegate(OperationArgs& Args)
{
    auto& S1 = Args.evaluate(0).as_<IntSet>();

    IntSet S2;
    for(int b: S1)
	S2 = S2.insert(-b);

    return S2;
}
