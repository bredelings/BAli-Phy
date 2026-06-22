#include "computation/haskell/generated.H"

namespace Haskell::Generated
{
    LExp Loc(const Exp& E)
    {
        return {noloc, E};
    }

    Exp Apply(const Exp& head, const std::vector<Exp>& args)
    {
        Exp result = head;
        for(const auto& arg: args)
            result = ApplyExp(Loc(result), Loc(arg));
        return result;
    }

    Exp List(const std::vector<Exp>& elements)
    {
        std::vector<LExp> located_elements;
        for(const auto& element: elements)
            located_elements.push_back(Loc(element));
        return Haskell::List(located_elements);
    }

    Exp Tuple(const std::vector<Exp>& elements)
    {
        std::vector<LExp> located_elements;
        for(const auto& element: elements)
            located_elements.push_back(Loc(element));
        return Haskell::Tuple(located_elements);
    }
}
