#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include <vector>
#include <string>
#include "computation/machine/args.H"
//#include "util/myexception.H"
#include "computation/expression/expression_ref.H"

//using boost::dynamic_pointer_cast;
using std::vector;
using std::string;

//typedef Box<std::map<int,expression_ref>> EIntMap;

enum class EventType {RateChange, NewLeaf, Coalescent};

struct Event
{
    EventType type;
    int index;
};

// coalescentTreePr :: [(Double,Double)] -> Tree -> LogDouble
// rawCoalescent
extern "C" closure builtin_function_rawCoalescentTreePr(OperationArgs& Args)
{
    using enum EventType;
    
    // population sizes: EVector (EPair Double Double)
    auto arg0 = Args.evaluate(0);
    auto& popSizes = arg0.as_<EVector>();

    // nodeTimes :: EVector (EPair Double (EVector Double))
    auto arg1 = Args.evaluate(1);
    auto& nodeTimes = arg1.as_<EVector>();

    // Define sorting function.
    auto event_time = [&](const Event& e) {
        if (e.type == EventType::RateChange)
            return popSizes[e.index].as_<EPair>().first.as_double();
        else
            return nodeTimes[e.index].as_<EPair>().first.as_double();
    };

    auto event_compare = [&](const Event& e1, const Event& e2)
        {
            return event_time(e1) < event_time(e2);
        };

    std::vector<Event> events;

    // 1. Add popSize changes
    for(int i=0;i<popSizes.size();i++)
        events.push_back({EventType::RateChange, i});

    // 2. Add tree nodes
    for(int i=0;i<nodeTimes.size();i++)
    {
        // Check that parent is not younger than children.
        auto nodeTime = nodeTimes[i].as_<EPair>().first.as_double();
        auto& childTimes = nodeTimes[i].as_<EPair>().second.as_<EVector>();
        for(auto& childTime: childTimes)
            if (not (nodeTime >= childTime.as_double()))
            {
                log_double_t Pr;
                return { Pr };
            }

        // Classify events based on number of children.
        int n = childTimes.size();
        if (n == 0)
            events.push_back({EventType::NewLeaf, i});
        else if (n == 2)
            events.push_back({EventType::Coalescent, i});
        else
            throw myexception()<<"Coalescent tree node has "<<n<<" children -- not allowed!";
    }

    // 3. Sort events
    std::sort(events.begin(), events.end(), event_compare);

    // 4. Get the initial population size and skip NewLeaf events before
    double N = 0;
    int i=0;
    int n = 0;
    for(;i<events.size();i++)
    {
        if (events[i].type == EventType::RateChange)
        {
            N = popSizes[events[i].index].as_<EPair>().second.as_double();
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
        double t1 = event_time(events[i-1]);
        double t2 = event_time(events[i]);
        assert(t1 <= t2);
        
        double nChoose2 = n*(n-1)/2;
        double rate = 1/N;
        auto PrInterval = exp_to<log_double_t>( -rate * nChoose2 * (t2-t1) );
        
        auto type = events[i].type;
        if (type == RateChange)
        {
            N = popSizes[events[i].index].as_<EPair>().second.as_double();
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
