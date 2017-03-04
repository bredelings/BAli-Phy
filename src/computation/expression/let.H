#ifndef EXPRESSION_LET_H
#define EXPRESSION_LET_H

#include <vector>
#include "object.H"
#include "expression_ref.H"

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

expression_ref let_expression(const std::vector<expression_ref>& vars, const std::vector<expression_ref>& bodies, const expression_ref& T);

expression_ref let_expression(const expression_ref& var, const expression_ref& body, const expression_ref& T);

expression_ref indexed_let_expression(const std::vector<expression_ref>& bodies, const expression_ref& T);

expression_ref unlet(const expression_ref& E);

bool parse_let_expression(const expression_ref& E, std::vector<expression_ref>& vars, std::vector<expression_ref>& bodies, expression_ref& T);

bool parse_indexed_let_expression(const expression_ref& E, std::vector<expression_ref>& bodies, expression_ref& T);


#endif
