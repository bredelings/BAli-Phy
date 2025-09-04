#ifndef UNFOLDING_H
#define UNFOLDING_H

#include <vector>
#include <string>
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

    /* GHC distinguishes: (i)   vanilla (e.g. rhs)
                          (ii)  stable (INLINE or INLINEABLE)
                          (iii) stable-system (wrapper or system-generated unfolding)
                          (iv)  compulsory (i.e. no RHS, so must be inlined)
    */
    bool always_unfold = false;

    // GHC stores if this is a top-level binding.
    // GHC stores size info

    /* GHC caches and stores
       (i)   exprIsHNF
       (ii)  exprIsConLike
       (iii) exprIsCheap
       (iv)  exprIsExpandable
    */

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(expr, always_unfold);
    }

    CoreUnfolding() = default; // for cereal
    CoreUnfolding(const Occ::Exp& e):expr(e) {}
    CoreUnfolding(const Occ::Exp& e,bool b):expr(e),always_unfold(b) {}
};

typedef std::variant<std::monostate, MethodUnfolding, DFunUnfolding, CoreUnfolding> Unfolding;



#endif
