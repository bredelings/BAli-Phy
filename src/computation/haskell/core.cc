#include "core.H"

#include "expression/tuple.H"

using std::vector;

namespace Core
{

    // Expression stuff

    Exp Tuple(const std::vector<Exp>& es)
    {
        return get_tuple(es);
    }


    // wrapper stuff

    wrapper wrapper_id = [](const Exp& x) {return x;};

    wrapper operator*(const wrapper& w1, const wrapper& w2)
    {
        return [=](const Exp& x)
        {
            return w1(w2(x));
        };
    }
}
