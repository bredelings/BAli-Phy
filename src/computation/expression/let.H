#ifndef EXPRESSION_LET_H
#define EXPRESSION_LET_H

#include <string>
#include <vector>
#include <utility>
#include "computation/object.H"
#include "expression_ref.H"
#include "var.H"

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

bool is_let_expression(const expression_ref& E);

#endif
