#ifndef INLINER_H
#define INLINER_H

#include <vector>
#include <string>
#include <variant>
#include <memory>
#include "substitution.H"
#include "simplifier_env.H" // for Unfolding, Occ::Var, and rebind_var( )

struct ok_context;
struct apply_context;
struct case_context;

struct inliner_options
{
    int creation_threshold = 75;
    int inline_threshhold = 9; // use_threshold
    int fun_app_discount = 6;
    int dict_discount = 3;
};


struct discounts
{
    int max;
    int sum;

    inline discounts operator+(discounts d2) const
    {
        return discounts( std::max(max, d2.max), sum + d2.sum );
    }

    explicit discounts(int m, int s):max(m),sum(s) {}
};

typedef immer::map<Occ::Var, discounts> var_discounts;

struct ExprSize
{
    int size = 0;
    var_discounts arg_discounts;
    int inspect_discount = 0;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(size, arg_discounts, inspect_discount);
    }
};

enum CallCtxt
{
    BoringCtxt,
    RhsRecCtxt,
    RhcNonRecCtxt,
    DiscountArgCtxt,
    ApplyCtxt,
    CaseCtxt
};

enum DupStatus
{
    NoDup,           // unsimplified
    Simplified,      // simplified, but cannot dup
    OkToDup          // simplified, and ok to dup
};


class inline_context: public std::variant<std::shared_ptr<const ok_context>, std::shared_ptr<const apply_context>, std::shared_ptr<const case_context>>
{
public:
    std::shared_ptr<const case_context> is_case_context() const
    {
        if (index() == 2)
            return std::get<std::shared_ptr<const case_context>>(*this);
        else
            return {};
    }

    std::shared_ptr<const apply_context> is_apply_context() const
    {
        if (index() == 1)
            return std::get<std::shared_ptr<const apply_context>>(*this);
        else
            return {};
    }

    std::shared_ptr<const ok_context> is_ok_context() const
    {
        if (index() == 0)
            return std::get<std::shared_ptr<const ok_context>>(*this);
        else
            return {};
    }

    std::optional<inline_context> prev_context() const;

    using variant::variant;
};

struct apply_context
{
    DupStatus dup_status = DupStatus::NoDup;
    Occ::Exp arg;
    simplifier::substitution subst;
    in_scope_set bound_vars;
    inline_context next;
    apply_context(const Occ::Exp& a, const simplifier::substitution& s, const in_scope_set& bvs, const inline_context& i):arg(a), subst(s), bound_vars(bvs), next(i) {}
};

struct case_context
{
    DupStatus dup_status = DupStatus::NoDup;
    std::vector<Occ::Alt> alts;
    simplifier::substitution subst;
    inline_context next;
    case_context(const std::vector<Occ::Alt>& as, const simplifier::substitution& s, const inline_context& i):alts(as), subst(s), next(i) {}
};

// It is allowed to inline here.
struct ok_context
{
};

inline_context make_ok_context();
inline_context make_apply_context_one_arg(const inline_context& context, const simplifier::substitution& S, const in_scope_set& bound_vars, const Occ::Var& arg);
inline_context make_apply_context(const Occ::Apply& A, const simplifier::substitution& S, const in_scope_set& bound_vars, inline_context context);
inline_context make_case_context(const Occ::Case& C, const simplifier::substitution& S, const inline_context& context);

bool is_trivial(const Occ::Exp& E);

int simple_size(const Occ::Exp& E);

int simple_size(const Core2::Exp<>& E);

class Module;

ExprSize size_of_expr(const Module& m, const inliner_options& opts, int max_size, const std::vector<Occ::Var>& top_args, const Occ::Exp& E);

bool is_WHNF(const Occ::Exp& E);

UnfoldingGuidance make_unfolding_guidance(const Module& m, const inliner_options& opts, const Occ::Exp& e);

CoreUnfolding make_core_unfolding(const Module& m, const inliner_options& opts, const Occ::Exp& e);

#endif
