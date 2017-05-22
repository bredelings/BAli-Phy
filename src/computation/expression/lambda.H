#ifndef EXPRESSION_LAMBDA_H
#define EXPRESSION_LAMBDA_H

#include "object.H"
#include "expression_ref.H"

struct Operator;

struct lambda: public Object
{
    lambda* clone() const {return new lambda(*this);}
    bool operator==(const Object& o) const;
    type_constant type() const {return lambda_type;}
    std::string print() const;
};

struct lambda2: public Object
{
    lambda2* clone() const {return new lambda2(*this);}
    bool operator==(const Object& o) const;

    type_constant type() const {return lambda2_type;}
    std::string print() const;
};

expression_ref lambda_expression(const Operator& O);
expression_ref lambda_quantify(int dummy_index, const expression_ref& E);
expression_ref lambda_quantify(const expression_ref& dummy, const expression_ref& E);

inline bool is_lambda(const expression_ref& E)
{
    return E.type() == lambda_type;
}

#endif
