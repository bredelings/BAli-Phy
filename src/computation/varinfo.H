#ifndef INFO_H
#define INFO_H
#endif

#include "haskell/core.H"

struct VarInfo
{
    bool always_unfold = false;
    Core::Exp unfolding;

    // pragmas
};
