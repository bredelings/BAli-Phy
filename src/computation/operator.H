#ifndef OPERATOR_H
#define OPERATOR_H

#include <vector>
#include <string>
#include "object.H"

enum assoc_type {assoc_none=0,assoc_left=1, assoc_right=2};

struct Operator: public Object
{
  // -1 is left associativity
  virtual assoc_type associativity() const {return assoc_none;}
  // -1 is not infix
  virtual int precedence() const {return -1;}
  virtual Operator* clone() const =0;
  virtual std::string name() const=0;
  virtual std::string print() const;
  virtual std::string print_expression(const std::vector<std::string>& args) const;
  virtual int n_args() const=0;
  tribool compare(const Object& o) const;
};

#endif
