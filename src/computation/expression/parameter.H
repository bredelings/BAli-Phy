#ifndef PARAMETER_H
#define PARAMETER_H

#include <string>
#include "computation/object.H"
#include "computation/type_constant.H"
#include "computation/expression/expression_ref.H"

struct parameter: public Object
{
    std::string parameter_name;

    parameter* clone() const {return new parameter(*this);}
    std::string print() const {return parameter_name;}
    bool operator==(const Object& o) const;
  
    type_constant type() const {return parameter_type;}

    parameter(const std::string& s);
};

bool is_parameter(const expression_ref&);

#endif
