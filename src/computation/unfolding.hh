#ifndef UNFOLDING_H
#define UNFOLDING_H

#include <vector>
#include <string>
#include "computation/optimization/occurrence_info.hh"

// UnfoldingGuidance - for CoreUnfolding

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

// Unfoldings

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

struct OtherConUnfolding
{
    std::vector<std::string> constructors_not_taken;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(constructors_not_taken);
    }
};


struct CoreUnfolding
{
private:
    // Cached copies share a single decode. The loader outlives Program when a body is retained.
    struct deferred_body
    {
        std::string bytes;
        std::optional<Occ::Exp> expression;
        std::shared_ptr<module_loader> loader;
        std::string owner;
    };
    Occ::Exp expr_;
    std::shared_ptr<deferred_body> deferred_;
    std::string serialized_expression() const;
    friend struct symbol_info;

public:
    const Occ::Exp& expression() const;

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
       (iii) exprIsWorkFree
       (iv)  exprIsExpandable
    */

    // Each expression has its own pointer-ID scope; guidance remains available without decoding it.
    template <class Archive>
    void save(Archive& ar) const
    {
        ar(unf_guidance, always_unfold, serialized_expression());
    }

    // Retain owned bytes and builtin-resolution context without borrowing Program or its archive.
    template <class Archive>
    void load(Archive& ar)
    {
        ar(unf_guidance, always_unfold);
        deferred_ = std::make_shared<deferred_body>();
        ar(deferred_->bytes);
        if (dynamic_cast<cereal::UserDataAdapter<const Program, Archive>*>(&ar))
            deferred_->loader = cereal::get_user_data<const Program>(ar).get_module_loader();
        expr_ = Occ::Exp{};
    }

    CoreUnfolding() = default; // for cereal
    CoreUnfolding(const Occ::Exp& e, const UnfoldingGuidance& g, bool b=false):expr_(e),unf_guidance(g), always_unfold(b) {}
};


typedef std::variant<std::monostate, MethodUnfolding, OtherConUnfolding, DFunUnfolding, CoreUnfolding> Unfolding;

#endif
