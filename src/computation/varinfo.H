#ifndef VAR_INFO_H
#define VAR_INFO_H

#include "haskell/core.H"
#include "expression/convert.H"

struct VarInfo
{
    bool conlike = false;
    bool always_unfold = false;
    std::optional<Occ::Exp> unfolding;

    // pragmas
    bool pragma_inline = false;
    bool pragma_inlineable = false;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(conlike, always_unfold, unfolding, pragma_inline, pragma_inlineable);
    }
};

#endif
