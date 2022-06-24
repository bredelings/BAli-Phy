#ifndef CORE_H
#define CORE_H

#include <utility>  // for function
#include "computation/expression/expression_ref.H"

// make a Core namespace

namespace Core
{
    // This should be Core::Exp
    typedef expression_ref Exp;
    typedef expression_ref Decl;

    // Wrappers
    typedef std::function<Exp(const Exp&)> wrapper;

    extern Core::wrapper wrapper_id;

    Exp Tuple(const std::vector<Exp>& es);
};

Core::wrapper operator*(const Core::wrapper&, const Core::wrapper&);

#endif
