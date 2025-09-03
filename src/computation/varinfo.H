#ifndef VAR_INFO_H
#define VAR_INFO_H

#include <optional>
#include "computation/unfolding.H"

struct VarInfo
{
    bool conlike = false;
    Unfolding unfolding;

    // pragmas
    bool pragma_inline = false;
    bool pragma_inlineable = false;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(conlike, unfolding, pragma_inline, pragma_inlineable);
    }
};

#endif
