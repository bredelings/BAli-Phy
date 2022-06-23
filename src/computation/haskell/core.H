#ifndef CORE_H
#define CORE_H

#include <utility>  // for function
#include "computation/expression/expression_ref.H"

namespace Haskell {
    typedef expression_ref Core;

    typedef std::function<Core(const Core&)> wrapper;

};
#endif
