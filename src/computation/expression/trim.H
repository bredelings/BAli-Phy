#ifndef EXPRESSION_TRIM_H
#define EXPRESSION_TRIM_H

#include "computation/object.H"
#include "expression_ref.H"

struct Trim: public Object
{
    Trim* clone() const {return new Trim(*this);}
    bool operator==(const Object& o) const;

    type_constant type() const {return type_constant::trim_type;}

    std::string print() const;
};

expression_ref trim_normalize(const expression_ref&);

expression_ref trim(const expression_ref&);

expression_ref trim_unnormalize(const expression_ref&);

expression_ref untrim(const expression_ref&);

#endif
