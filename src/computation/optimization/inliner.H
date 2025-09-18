#ifndef INLINER_H
#define INLINER_H

#include <vector>
#include <string>
#include <variant>
#include <memory>
#include "substitution.H"

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

    explicit discounts(int i):max(i),sum(i) {}
    explicit discounts(int m, int s):max(m),sum(s) {}
};

struct ExprSize
{
    int size = 0;
    immer::map<Occ::Var, discounts> arg_discounts;
    int inspect_discount = 0;
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
    Occ::Exp arg;
    const simplifier::substitution& subst;
    inline_context next;
    apply_context(const Occ::Exp& a, const simplifier::substitution& s, const inline_context& i):arg(a), subst(s), next(i) {}
};

struct case_context
{
    std::vector<Occ::Alt> alts;
    const simplifier::substitution& subst;
    inline_context next;
    case_context(const std::vector<Occ::Alt>& as, const simplifier::substitution& s, const inline_context& i):alts(as), subst(s), next(i) {}
};

// It is allowed to inline here.
struct ok_context
{
};

inline_context make_ok_context();
inline_context make_apply_context_one_arg(const inline_context& context, const simplifier::substitution& S, const Occ::Var& arg);
inline_context make_apply_context(const Occ::Apply& A, const simplifier::substitution& S, inline_context context);
inline_context make_case_context(const Occ::Case& C, const simplifier::substitution& S, const inline_context& context);

bool is_trivial(const Occ::Exp& E);

int simple_size(const Occ::Exp& E);

int simple_size(const Core2::Exp<>& E);

ExprSize size_of_expr(const inliner_options& opts, int max_size, const std::vector<Occ::Var>& top_args, const Occ::Exp& E);

bool is_WHNF(const Occ::Exp& E);
#endif
