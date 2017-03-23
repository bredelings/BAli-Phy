#ifndef EXPRESSION_LET_H
#define EXPRESSION_LET_H

#include <vector>
#include <utility>
#include "object.H"
#include "expression_ref.H"
#include "dummy.H"

struct let_obj: public Object
{
    let_obj* clone() const {return new let_obj(*this);}

    tribool compare(const Object& o) const;

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

expression_ref let_expression(const std::vector<std::pair<dummy, expression_ref>>& decls, const expression_ref& T);

expression_ref indexed_let_expression(const std::vector<expression_ref>& bodies, const expression_ref& T);

expression_ref unlet(const expression_ref& E);

bool is_let_expression(const expression_ref& E);

bool parse_let_expression(const expression_ref& E, std::vector<std::pair<dummy, expression_ref>>& decls, expression_ref& body);

bool parse_indexed_let_expression(const expression_ref& E, std::vector<expression_ref>& bodies, expression_ref& T);

void get_decls_from_let(const expression_ref& E, std::vector<std::pair<dummy, expression_ref>>& decls);

std::vector<std::pair<dummy, expression_ref>> get_decls_from_let(const expression_ref& E);

void strip_let(expression_ref& E, std::vector<std::pair<dummy, expression_ref>>& decls);

std::vector<std::pair<dummy, expression_ref>> strip_let(expression_ref& E);

#endif
