#ifndef UNFOLDING_H
#define UNFOLDING_H

#include <vector>
#include <string>
#include "computation/optimization/occurrence_info.H"
#include "computation/optimization/inliner.H"

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
    std::set<Occ::Var> free_vars;
    
    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(binders, head, args);
    }
};

struct UnfoldNever
{
    template <class Archive>
    void serialize(Archive& /*ar*/)
    {
    }
};

struct UnfoldWhen
{
    bool unsaturated_ok = false;
    bool boring_ok = false;
    int arity = 0;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(unsaturated_ok, boring_ok, arity);
    }
};

struct UnfoldIfGoodArgs
{
    std::vector<int> arg_discounts;
    int size = 0;
    int scrut_discount = 0;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(arg_discounts, size, scrut_discount);
    }

};

typedef std::variant<UnfoldNever, UnfoldWhen, UnfoldIfGoodArgs> UnfoldingGuidance;

UnfoldingGuidance make_unfolding_guidance(const inliner_options& opts, const Occ::Exp& e);

struct CoreUnfolding
{
    Occ::Exp expr;

    /* GHC distinguishes: (i)   vanilla (e.g. rhs)
                          (ii)  stable (INLINE or INLINEABLE)
                          (iii) stable-system (wrapper or system-generated unfolding)
                          (iv)  compulsory (i.e. no RHS, so must be inlined)
    */

    UnfoldingGuidance unf_guidance;

    // GHC stores if this is a top-level binding.

    bool always_unfold = false;

    /* GHC caches and stores
       (i)   exprIsHNF
       (ii)  exprIsConLike
       (iii) exprIsCheap
       (iv)  exprIsExpandable
    */

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(expr, unf_guidance, always_unfold);
    }

    CoreUnfolding() = default; // for cereal
    CoreUnfolding(const Occ::Exp& e, const UnfoldingGuidance& g, bool b=false):expr(e),unf_guidance(g), always_unfold(b) {}
};


typedef std::variant<std::monostate, MethodUnfolding, DFunUnfolding, CoreUnfolding> Unfolding;

CoreUnfolding make_core_unfolding(const inliner_options& opts, const Occ::Exp& e);

#endif
