#include "core.H"

namespace Core
{

    wrapper wrapper_id = [](const Exp& x) {return x;};

    wrapper operator*(const wrapper& w1, const wrapper& w2)
    {
        return [=](const Exp& x)
        {
            return w1(w2(x));
        };
    }
}
