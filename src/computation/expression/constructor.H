#ifndef CONSTRUCTOR_H
#define CONSTRUCTOR_H

#include <string>
#include "computation/object.H"
#include "expression_ref.H"

struct constructor: public Object
{
    constructor* clone() const {return new constructor(*this);}

    bool operator==(const Object& o) const;
  
    std::string f_name;
    int n_args_;

    int n_args() const {return n_args_;}

    int prec;

    virtual int precedence() const {return prec;}

    std::string name() const {return f_name;}

    std::string print() const {return f_name;}

    type_constant type() const {return type_constant::constructor_type;}

    constructor(const std::string&, int n);
};

bool has_constructor(const expression_ref&, const std::string&);

inline bool is_constructor(const expression_ref& E)
{
    return E.type() == type_constant::constructor_type;
}

inline bool is_constructor_exp(const expression_ref& E)
{
    return is_constructor(E.head());
}

#endif
