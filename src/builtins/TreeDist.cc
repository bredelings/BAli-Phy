#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include <map>
#include <vector>
#include "computation/machine/args.H"
#include "computation/machine/gcobject.H"
#include "computation/runtime/ast.H"

using std::map;
using std::vector;

enum class EventType {RateChange, NewLeaf, Coalescent};

struct Event
{
    EventType type;
    double time;
    double population_size = 0;
};

// Read each node time once through an edge controlled by the IntMap, then use
// detached doubles with the population schedule and tree topology.
extern "C" closure builtin_function_rawCoalescentTreePr(OperationArgs& Args)
{
    using enum EventType;

    auto pop_sizes_value = Args.evaluate_slot_to_value(0);
    const auto& pop_sizes = pop_sizes_value.as_<R::RVector>();

    auto node_times_arg = Args.evaluate_slot_use_with_contingency(1);

    auto topology_value = Args.evaluate_slot_to_value(2);
    const auto& topology = topology_value.as_<R::RVector>();

    const auto& node_time_regs = Args.memory().closure_at(node_times_arg.value_reg)
                                     .get_code().as_<IntMap>();
    vector<int> node_ids;
    node_ids.reserve(node_time_regs.size());
    for(const auto& [node, _]: node_time_regs)
        node_ids.push_back(node);

    map<int, double> node_times;
    for(int node: node_ids)
    {
        // Evaluation can move the heap, so reacquire the IntMap before lookup.
        const auto& current_node_time_regs =
            Args.memory().closure_at(node_times_arg.value_reg).get_code().as_<IntMap>();
        int value_reg = Args.evaluate_reg_use(
            current_node_time_regs[node], node_times_arg.edge_contingency);
        node_times[node] = Args.memory().closure_at(value_reg).get_code().as_double();
    }

    if (topology.size() != node_times.size())
        throw myexception()<<"Coalescent topology has "<<topology.size()
                           <<" nodes, but its time map has "<<node_times.size();

    std::vector<Event> events;
    events.reserve(pop_sizes.size() + topology.size());

    // 1. Add popSize changes
    for(const auto& pop_size: pop_sizes)
    {
        double time = R::rpair_first(pop_size).as_double();
        double population_size = R::rpair_second(pop_size).as_double();
        events.push_back({RateChange, time, population_size});
    }

    // 2. Add tree nodes
    for(const auto& node_entry: topology)
    {
        int node = R::rpair_first(node_entry).as_int();
        auto node_time = node_times.find(node);
        if (node_time == node_times.end())
            throw myexception()<<"Coalescent topology node "<<node
                               <<" has no node time";
        const auto& child_nodes = R::rpair_second(node_entry).as_<R::RVector>();

        // Check that parent is not younger than children.
        for(const auto& child_value: child_nodes)
        {
            int child = child_value.as_int();
            auto child_time = node_times.find(child);
            if (child_time == node_times.end())
                throw myexception()<<"Coalescent child node "<<child
                                   <<" has no node time";
            if (not (node_time->second >= child_time->second))
            {
                log_double_t Pr;
                return { Pr };
            }
        }

        // Classify events based on number of children.
        if (child_nodes.empty())
            events.push_back({NewLeaf, node_time->second});
        else if (child_nodes.size() == 2)
            events.push_back({Coalescent, node_time->second});
        else
            throw myexception()<<"Coalescent tree node has "<<child_nodes.size()
                               <<" children -- not allowed!";
    }

    // 3. Sort events
    std::sort(events.begin(), events.end(),
              [](const Event& e1, const Event& e2) {return e1.time < e2.time;});

    // 4. Get the initial population size and skip NewLeaf events before
    double N = 0;
    int i=0;
    int n = 0;
    for(;i<events.size();i++)
    {
        if (events[i].type == EventType::RateChange)
        {
            N = events[i].population_size;
            i++;
            break;
        }
        else if (events[i].type == EventType::NewLeaf)
        {
            n++;
        }
        else
        {
            std::abort();
        }
    }

    assert(i > 0);
    assert(events[i-1].type == EventType::RateChange);

    // 5. Compute the probability
    log_double_t Pr = 1;
    for(;i<events.size();i++)
    {
        double t1 = events[i-1].time;
        double t2 = events[i].time;
        assert(t1 <= t2);
        
        double nChoose2 = n*(n-1)/2;
        double rate = 1/N;
        auto PrInterval = exp_to_log_space( -rate * nChoose2 * (t2-t1) );
        
        auto type = events[i].type;
        if (type == RateChange)
        {
            N = events[i].population_size;
        }
        else if (type == NewLeaf)
        {
            n++;
            assert(n >= 1);
        }
        else if (type == Coalescent)
        {
            PrInterval *= rate;
            n--;
            assert(n >= 1);
        }

        Pr *= PrInterval;
    }

    assert(n == 1);

    return { Pr };
}
