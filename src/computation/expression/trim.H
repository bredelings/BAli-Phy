#ifndef EXPRESSION_TRIM_H
#define EXPRESSION_TRIM_H

#include "computation/object.H"

struct Trim: public Object
{
    Trim* clone() const {return new Trim(*this);}
    bool operator==(const Object& o) const;

    type_constant type() const {return type_constant::trim_type;}

    std::string print() const;
};

#endif
