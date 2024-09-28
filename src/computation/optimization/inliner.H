#ifndef INLINER_H
#define INLINER_H

#include <vector>
#include <string>
#include <variant>
#include <memory>
#include "computation/expression/expression_ref.H"
#include "substitution.H"

struct ok_context;
struct stop_context;
struct apply_context;
struct case_context;

class inline_context: public std::variant<std::shared_ptr<const ok_context>, std::shared_ptr<const stop_context>, std::shared_ptr<const apply_context>, std::shared_ptr<const case_context>>
{
public:
    std::shared_ptr<const case_context> is_case_context() const
    {
        if (index() == 3)
            return std::get<std::shared_ptr<const case_context>>(*this);
        else
            return {};
    }

    std::shared_ptr<const apply_context> is_apply_context() const
    {
        if (index() == 2)
            return std::get<std::shared_ptr<const apply_context>>(*this);
        else
            return {};
    }

    std::shared_ptr<const stop_context> is_stop_context() const
    {
        if (index() == 1)
            return std::get<std::shared_ptr<const stop_context>>(*this);
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
    Occ::Var arg;
    const simplifier::substitution& subst;
    inline_context next;
    apply_context(const Occ::Var& x, const simplifier::substitution& s, const inline_context& i):arg(x), subst(s), next(i) {}
};

struct case_context
{
    Occ::Alts alts;
    const simplifier::substitution& subst;
    inline_context next;
    case_context(const Occ::Alts& as, const simplifier::substitution& s, const inline_context& i):alts(as), subst(s), next(i) {}
};

// It is allowed to inline here.
struct ok_context
{
};

// It is not allowed to inline here.
struct stop_context
{
};


inline_context make_stop_context();
inline_context make_ok_context();
inline_context make_apply_context_one_arg(const inline_context& context, const simplifier::substitution& S, const Occ::Var& arg);
inline_context make_apply_context(const Occ::Apply& A, const simplifier::substitution& S, inline_context context);
inline_context make_case_context(const Occ::Case& C, const simplifier::substitution& S, const inline_context& context);

int simple_size(const expression_ref& E);

bool is_trivial(const Occ::Exp& E);
#endif
