#ifndef HASKELL_GENERATED_H
#define HASKELL_GENERATED_H

#include <vector>

#include "computation/haskell/haskell.H"

namespace Haskell::Generated
{
    LExp Loc(const Exp&);

    Exp Apply(const Exp& head, const std::vector<Exp>& args);
    Exp List(const std::vector<Exp>& elements);
    Exp Tuple(const std::vector<Exp>& elements);
}

#endif
