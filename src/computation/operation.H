#ifndef OPERATION_H
#define OPERATION_H

#include <vector>
#include <string>

#include "object.H"

struct OperationArgs;

#include "operator.H"
#include "closure.H"

struct Operation: public Operator
{
  int n_args_;
  int n_args() const {return n_args_;}

  virtual Operation* clone() const=0;
  virtual closure operator()(OperationArgs& Args) const = 0;

  Operation(int n):n_args_(n) { }
};

std::string print_operator_expression(const std::vector<std::string>& arguments);

std::string print_infix_expression(const std::vector<std::string>& arguments);

#endif
