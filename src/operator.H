#ifndef OPERATOR_H
#define OPERATOR_H

#include <vector>
#include <string>
#include "object.H"

struct Operator: virtual public Object
{
  virtual Operator* clone() const =0;
  virtual std::string name() const=0;
  virtual std::string print() const;
  virtual std::string print_expression(const std::vector<std::string>& args) const;
  tribool compare(const Object& o) const;
};

#endif
