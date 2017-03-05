#ifndef EXPRESSION_LAMBDA_H
#define EXPRESSION_LAMBDA_H

#include "object.H"
#include "expression_ref.H"

class Operator;

struct lambda: public Object
{
    lambda* clone() const {return new lambda(*this);}
    tribool compare(const Object& o) const;
    type_constant type() const {return lambda_type;}
    std::string print() const;
};

struct lambda2: public Object
{
    lambda2* clone() const {return new lambda2(*this);}
    tribool compare(const Object& o) const;

    type_constant type() const {return lambda2_type;}
    std::string print() const;
};

expression_ref lambda_expression(const Operator& O);
expression_ref lambda_quantify(int dummy_index, const expression_ref& E);
expression_ref lambda_quantify(const expression_ref& dummy, const expression_ref& E);

#endif
