#ifndef EXPRESSION_LAMBDA_H
#define EXPRESSION_LAMBDA_H

#include "computation/object.H"
#include "expression_ref.H"
#include "var.H"

struct lambda: public Object
{
    lambda* clone() const {return new lambda(*this);}
    bool operator==(const Object& o) const;
    type_constant type() const {return type_constant::lambda_type;}
    std::string print() const;
};

struct lambda2: public Object
{
    lambda2* clone() const {return new lambda2(*this);}
    bool operator==(const Object& o) const;

    type_constant type() const {return type_constant::lambda2_type;}
    std::string print() const;
};

expression_ref lambda_n(const Object& O, int n);
expression_ref lambda_quantify(int var_index, const expression_ref& E);
expression_ref lambda_quantify(const expression_ref& var, const expression_ref& E);
expression_ref lambda_quantify(const std::vector<expression_ref>& var, const expression_ref& E);
expression_ref lambda_quantify(const std::vector<var>& var, const expression_ref& E);

inline bool is_lambda(const expression_ref& E)
{
    return E.type() == type_constant::lambda_type;
}

inline bool is_lambda_exp(const expression_ref& E)
{
    if (is_lambda(E.head()))
    {
	assert(E.size() > 0);
	return true;
    }
    else
	return false;
}

#endif
