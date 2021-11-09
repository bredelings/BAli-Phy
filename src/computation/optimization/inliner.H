#ifndef INLINER_H
#define INLINER_H

#include <vector>
#include <string>
#include <variant>
#include <memory>
#include "expression/AST_node.H"
#include "computation/expression/expression_ref.H"
#include "simplifier.H"

struct apply_context;
struct case_context;
struct stop_context;

class inline_context: public std::variant<std::shared_ptr<const stop_context>, std::shared_ptr<const apply_context>, std::shared_ptr<const case_context>>
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

    std::shared_ptr<const stop_context> is_stop_context() const
    {
        if (index() == 0)
            return std::get<std::shared_ptr<const stop_context>>(*this);
        else
            return {};
    }

    std::optional<inline_context> prev_context() const;

    using variant::variant;
};

struct apply_context
{
    expression_ref arg;
    const simplifier::substitution& subst;
    inline_context next;
    apply_context(const expression_ref& e, const simplifier::substitution& s, const inline_context& i):arg(e), subst(s), next(i) {}
};

struct case_context
{
    expression_ref alts;
    const simplifier::substitution& subst;
    const in_scope_set& bound_vars;
    inline_context next;
    case_context(const expression_ref& e, const simplifier::substitution& s, const in_scope_set& iss, const inline_context& i):alts(e), subst(s), bound_vars(iss), next(i) {}
};

struct stop_context
{
};


std::shared_ptr<const stop_context> make_stop_context();
std::shared_ptr<const apply_context> make_apply_context_one_arg(const inline_context& context, const simplifier::substitution& S, const expression_ref& arg);
std::shared_ptr<const apply_context> make_apply_context(const expression_ref E, const simplifier::substitution& S, inline_context context);
std::shared_ptr<const case_context> make_case_context(const expression_ref E, const simplifier::substitution& S,  const in_scope_set& iss, const inline_context& context);

bool do_inline(const simplifier_options& options, const expression_ref& rhs, const occurrence_info& occur, const inline_context& context);

int simple_size(const expression_ref& E);

#endif
