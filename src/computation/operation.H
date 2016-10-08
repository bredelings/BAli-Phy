#ifndef OPERATION_H
#define OPERATION_H

#include <vector>
#include <string>

#include "object.H"

class OperationArgs;

#include "operator.H"
#include "closure.H"
#include "type_constant.H"

typedef closure (*operation_fn)(OperationArgs&);

struct Operation: public Operator
{
    int n_args_;

    std::string name_;

    operation_fn op = 0;

    virtual Operation* clone() const {return new Operation(*this);}

    int n_args() const {return n_args_;}

    std::string name() const {return name_;}

    tribool compare(const Object& o) const 
    {
	const Operation* O = dynamic_cast<const Operation*>(&o);
	if (not O) return false;

	if (op != O->op) return false;

	if (n_args() != O->n_args()) return false;

	if (name() != O->name()) return false;

	return true;
    }

    type_constant type() const {return operation_type;} // See expression.H to change this
    Operation(int n, closure (*f)(OperationArgs&), const std::string& s):n_args_(n),name_(s),op(f) { }
};

std::string print_operator_expression(const std::vector<std::string>& arguments);

std::string print_infix_expression(const std::vector<std::string>& arguments);

#endif
