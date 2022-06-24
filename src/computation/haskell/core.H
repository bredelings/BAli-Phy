#ifndef CORE_H
#define CORE_H

#include <utility>  // for function
#include "computation/expression/expression_ref.H"

// make a Core namespace

namespace Haskell
{
    // This should be Core::Exp
    typedef expression_ref Core;

    typedef std::function<Core(const Core&)> wrapper;

    // wrapper operator*(const wrapper&, const wrapper&)
};
#endif
