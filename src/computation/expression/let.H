#ifndef EXPRESSION_LET_H
#define EXPRESSION_LET_H

#include <vector>
#include <utility>
#include <boost/optional.hpp>
#include "object.H"
#include "expression_ref.H"
#include "var.H"

struct let_obj: public Object
{
    let_obj* clone() const {return new let_obj(*this);}

    bool operator==(const Object& o) const;

    type_constant type() const {return let_type;}

    std::string print() const;
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

typedef std::pair<var, expression_ref> CDecl;
typedef std::vector<CDecl> CDecls;

expression_ref let_expression(const CDecls& decls, const expression_ref& T);

expression_ref let_expression(const std::vector<CDecls>& decl_groups, const expression_ref& T);

expression_ref indexed_let_expression(const std::vector<expression_ref>& bodies, const expression_ref& T);

expression_ref unlet(const expression_ref& E);

bool is_let_expression(const expression_ref& E);

bool parse_let_expression(const expression_ref& E, CDecls& decls, expression_ref& body);

bool parse_indexed_let_expression(const expression_ref& E, std::vector<expression_ref>& bodies, expression_ref& T);

void get_decls(const expression_ref& E, CDecls& decls);

void let_decls(const expression_ref& E, CDecls& decls);

CDecls let_decls(const expression_ref& E);

CDecls parse_decls(const expression_ref& E);

expression_ref make_decls(const CDecls&);

expression_ref make_topdecls(const CDecls&);

expression_ref let_body(const expression_ref& E);

expression_ref multi_let_body(expression_ref E);

std::vector<CDecls> strip_multi_let(expression_ref& E);

boost::optional<var> find_first_duplicate_var(const CDecls& decls);
boost::optional<var> find_first_duplicate_var(const expression_ref& decls);
void check_duplicate_var(const CDecls& decls);
void check_duplicate_var(const expression_ref& decls);

#endif
