#ifndef EXPRESSION_LET_H
#define EXPRESSION_LET_H

#include <vector>
#include <utility>
#include <optional>
#include <tuple>
#include "computation/object.H"
#include "expression_ref.H"
#include "var.H"
#include "computation/operations.H"

typedef std::pair<var, expression_ref> CDecl;
typedef std::vector<CDecl> CDecls;

std::string print_cdecls(const CDecls&);

struct let_exp: public Object
{
    CDecls binds;
    expression_ref body;

    let_exp* clone() const {return new let_exp(*this);}

    bool operator==(const Object& o) const;

    type_constant type() const {return type_constant::let_type;}

    std::string print() const;

    let_exp(const CDecls& bs, const expression_ref& b)
        : binds(bs), body(b)
    {}
};

template <typename First>
First let(const First& first)
{
    return first;
}

template <typename First, typename Second, typename... Rest>
auto let(const First& first, const Second& second, const Rest&... rest)
{
    return let_expression(first,second,let(rest...));
}

expression_ref let_expression(const CDecls& decls, const expression_ref& T);

expression_ref let_expression(const std::vector<CDecls>& decl_groups, const expression_ref& T);

Let indexed_let_expression(const std::vector<expression_ref>& bodies, const expression_ref& T);

expression_ref unlet(const expression_ref& E);

bool is_let_expression(const expression_ref& E);

// Should probably be optional<Let>, but is Let destructurable?
std::optional<Let> parse_indexed_let_expression(const expression_ref& E);

expression_ref multi_let_body(expression_ref E);

std::vector<CDecls> strip_multi_let(expression_ref& E);

std::optional<var> find_first_duplicate_var(const CDecls& decls);
void check_duplicate_var(const CDecls& decls);
void check_duplicate_var(const expression_ref& decls);

#endif
