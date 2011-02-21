#ifndef OPERATION_H
#define OPERATION_H

#include <boost/shared_ptr.hpp>
#include <vector>
#include <string>

class OperationArgs;

struct Operation: public Object
{
  int n_args_;
  int n_args() const {return n_args_;}

  virtual Operation* clone() const=0;
  virtual boost::shared_ptr<const Object> operator()(OperationArgs& Args) const =0;
  virtual std::string expression(const std::vector<std::string>& inputs) const;

  Operation(int n):n_args_(n) { }
};

#endif
