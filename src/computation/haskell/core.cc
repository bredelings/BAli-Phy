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

}


Core::wrapper operator*(const Core::wrapper& w1, const Core::wrapper& w2)
{
    return [=](const Core::Exp& x)
        {
            return w1(w2(x));
        };
}
