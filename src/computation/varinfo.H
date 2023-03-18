#ifndef VAR_INFO_H
#define VAR_INFO_H

#include "haskell/core.H"

struct VarInfo
{
    bool conlike = false;
    bool always_unfold = false;
    Core::Exp unfolding;

    // pragmas
    bool pragma_inline = false;
    bool pragma_inlineable = false;
};

#endif
