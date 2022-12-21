#include "computation/machine/args.H"

#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include <map>
#include "computation/expression/index_var.H"
#include "computation/expression/constructor.H"
#include "computation/expression/bool.H"
#include "computation/machine/graph_register.H"
#include "util/rng.H"

using std::vector;
using std::map;
using std::pair;

// PROBLEM: OK the problem here is that the step which computes the updated
//          set in mapUnordered re-uses regs allocated by the previous step.
//          The regs in both steps have a kind of shared ownership.
//          Only the newly allocated regs are uniquely owned by the new step.
//          The deleted output regs are then owned only by the old step.

//          The step is therefore a kind of delta-step. It only does the difference
//          in the work, and it only creates the difference in the regs.

typedef Box<map<int,vector<int>>> Unordered;

extern "C" closure builtin_function_mkUnordered(OperationArgs& Args)
{
    // 1. Compute the number of elements
    int n = Args.evaluate(0).as_int();
    assert(n >= 0);

    // 2. Get a reference to the initial object
    int u_reg = Args.reg_for_slot(1);

    // 3. Create unordered object
    object_ptr<Unordered> U = new Unordered;
    for(int i=0;i<n;i++)
        U->insert({i,{u_reg}});

    return U;
}

int unorderedSize(const Unordered& u)
{
    int N = 0;
    for(auto& [key,value]: u)
        N += value.size();
    return N;
}

extern "C" closure builtin_function_unorderedSize(OperationArgs& Args)
{
    auto u = Args.evaluate(0);

    int N = unorderedSize(u.as_<Unordered>());

    return {N};
}

template <typename T>
void remove_random_entry(vector<T>& v)
{
    int i = myrandom(v.size());
    if (i+1 < v.size())
        std::swap(v[i], v.back());
    v.pop_back();
}

void remove_random_entries(int count, int key, Unordered& U)
{
    auto iter = U.find(key);
    assert(iter != U.end());
    auto& values = iter->second;
    if (values.size() == count)
        U.erase(key);
    else
        for(int i=0;i<count;i++)
            remove_random_entry(values);
}

extern "C" closure builtin_function_unorderedMap(OperationArgs& Args)
{
    /**
     ** NOTE: Because the initial object is created with an expression that looks at environment
     **       slots in order, we don't need the change the expression.
     **/

    // 1. Get the location of the function
    int f_reg = Args.reg_for_slot(0);

    // 2. Get a copy of the input array
    auto arg1 = Args.evaluate(1);
    auto& u_input = arg1.as_<Unordered>();

    // 3. Create the expression part of an "apply <1> <0>" closure.
    //    Indexing is from the end, which is why <1> comes before <1>.
    expression_ref apply_1_0 = {index_var(1),index_var(0)};

    // 4. Replace each x with (f x)

    object_ptr<Unordered> U;
    if (auto r = Args.previous_result_for_reg())
    {
        // Copy the previous value
        auto u = Args.memory().expression_at(*r);
        U = new Unordered(u.as_<Unordered>());

        // What is the delta

        // Look up how many times each input reg occurs
        std::map<int,int> in_reg_count;
        for(auto& [key, values]: u_input)
            for(auto& in_reg: values)
                in_reg_count[in_reg]++;

        // Look up how many times each input reg used to occur.
        std::map<int,int> prev_in_reg_count;
        for(auto& [in_reg, values]: *U)
            prev_in_reg_count[in_reg] = values.size();

        // Determine how many additions we need to do
        std::vector<pair<int,int>> add;
        for(auto& [key,count]: in_reg_count)
        {
            if (not prev_in_reg_count.count(key))
                add.push_back({key,count});
            else
            {
                int prev_count = prev_in_reg_count.at(key);
                if (prev_count < count)
                    add.push_back({key, count - prev_count});
            }
        }

        // Determine how many removals we need to do
        std::vector<pair<int,int>> remove;
        for(auto& [key,prev_count]: prev_in_reg_count)
        {
            if (not in_reg_count.count(key))
                remove.push_back({key, prev_count});
            else
            {
                int count = in_reg_count.at(key);
                if (prev_count > count)
                    remove.push_back({key, prev_count - count});
            }
        }

        // Remove
        for(auto& [key, count]: remove)
            remove_random_entries(count, key, *U);

        // Add
        for(auto& [key, count]: add)
        {
            int in_reg = key;
            for(int i=0;i<count;i++)
            {
                // Issue: the initial step will be the creator of the regs.
                int out_reg = Args.allocate({apply_1_0, {f_reg, in_reg}});

                auto iter = U->find(in_reg);
                if (iter == U->end())
                {
                    U->insert({in_reg,{}});
                    iter = U->find(in_reg);
                }
                assert(iter != U->end());
                iter->second.push_back(out_reg);
            }
        }
    }
    else
    {
        U = new Unordered;
        for(auto& [key, values]: u_input)
            for(auto& in_reg: values)
            {
                // Issue: the initial step will be the creator of the regs.
                int out_reg = Args.allocate({apply_1_0, {f_reg, in_reg}});

                auto iter = U->find(in_reg);
                if (iter == U->end())
                {
                    U->insert({in_reg,{}});
                    iter = U->find(in_reg);
                }
                assert(iter != U->end());
                iter->second.push_back(out_reg);
            }
    }

    return U;
}
