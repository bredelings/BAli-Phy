#ifndef CONSTRUCTOR_H
#define CONSTRUCTOR_H

#include <string>
#include "object.H"
#include "computation/operator.H"
#include "expression_ref.H"

struct constructor: public Operator
{
    constructor* clone() const {return new constructor(*this);}

    tribool compare(const Object& o) const;
  
    std::string f_name;
    int n_args_;

    int n_args() const {return n_args_;}

    assoc_type assoc;

    int prec;

    assoc_type associativity() const {return assoc;}
    virtual int precedence() const {return prec;}

    std::string name() const {return f_name;}

    type_constant type() const {return constructor_type;}

    constructor(const std::string&, int n);
};

constructor left_assoc_constructor(const std::string& s,int prec=0);

constructor right_assoc_constructor(const std::string& s,int prec=0);

bool has_constructor(const expression_ref&, const std::string&);

inline bool is_constructor(const expression_ref& E)
{
    return E.type() == constructor_type;
}

#endif
