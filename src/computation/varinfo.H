#ifndef VAR_INFO_H
#define VAR_INFO_H

#include <optional>
#include "computation/optimization/occurrence_info.H"


struct MethodUnfolding
{
    int index;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(index);
    }
};

struct DFunUnfolding
{
    std::vector<Occ::Var> binders;
    std::string head;
    std::vector<Occ::Exp> args;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(binders, head, args);
    }
};

struct CoreUnfolding
{
    Occ::Exp expr;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(expr);
    }
};

typedef std::variant<std::monostate, MethodUnfolding, DFunUnfolding, CoreUnfolding> Unfolding;

struct VarInfo
{
    bool conlike = false;
    bool always_unfold = false;
    Unfolding unfolding;

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
