#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include <vector>
#include <string>
#include "computation/machine/args.H"
//#include "util/myexception.H"
#include "computation/runtime/ast.H"

//using boost::dynamic_pointer_cast;
using std::vector;
using std::string;


namespace
{
    object_ptr<R::RVector> runtime_vector_from_value(const R::Exp& value)
    {
        object_ptr<R::RVector> result(new R::RVector);

        if (const auto* runtime_vector = value.to<R::RVector>())
        {
            result->assign(runtime_vector->begin(), runtime_vector->end());
            return result;
        }

        throw myexception()<<"Expected coalescent tree vector to be an RVector or R::RVector, but got "<<value.print();
    }
}

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
    
    // population sizes: RVector (RPair Double Double)
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto popSizes = runtime_vector_from_value(arg0);

    // nodeTimes :: RVector (RPair Double (RVector Double))
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto nodeTimes = runtime_vector_from_value(arg1);

    // Define sorting function.
    auto event_time = [&](const Event& e) {
        if (e.type == EventType::RateChange)
            return R::rpair_first((*popSizes)[e.index]).as_double();
        else
            return R::rpair_first((*nodeTimes)[e.index]).as_double();
    };

    auto event_compare = [&](const Event& e1, const Event& e2)
        {
            return event_time(e1) < event_time(e2);
        };

    std::vector<Event> events;

    // 1. Add popSize changes
    for(int i=0;i<popSizes->size();i++)
        events.push_back({EventType::RateChange, i});

    // 2. Add tree nodes
    for(int i=0;i<nodeTimes->size();i++)
    {
        // Check that parent is not younger than children.
        auto nodeTime = R::rpair_first((*nodeTimes)[i]).as_double();
        auto childTimes = runtime_vector_from_value(R::rpair_second((*nodeTimes)[i]));
        for(auto& childTime: *childTimes)
            if (not (nodeTime >= childTime.as_double()))
            {
                log_double_t Pr;
                return { Pr };
            }

        // Classify events based on number of children.
        int n = childTimes->size();
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
            N = R::rpair_second((*popSizes)[events[i].index]).as_double();
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
        auto PrInterval = exp_to_log_space( -rate * nChoose2 * (t2-t1) );
        
        auto type = events[i].type;
        if (type == RateChange)
        {
            N = R::rpair_second((*popSizes)[events[i].index]).as_double();
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
