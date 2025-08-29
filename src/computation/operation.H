#ifndef OPERATION_H
#define OPERATION_H

#include <vector>
#include <string>

#include "computation/object.H"

class OperationArgs;

#include "closure.H"
#include "type_constant.H"

typedef closure (*operation_fn)(OperationArgs&);

struct Operation: public Object
{
    std::string name_;

    operation_fn op = 0;

    virtual Operation* clone() const {return new Operation(*this);}

    std::string name() const {return name_;}

    std::string print() const {return name_;}

    bool operator==(const Object& o) const
    {
	const Operation* O = dynamic_cast<const Operation*>(&o);
	if (not O) return false;

	if (op != O->op) return false;

	if (name() != O->name()) return false;

	return true;
    }

    type_constant type() const {return type_constant::operation_type;}
    Operation(closure (*f)(OperationArgs&), const std::string& s):name_(s),op(f) { }
};

std::string print_operator_expression(const std::vector<std::string>& arguments);

std::string print_infix_expression(const std::vector<std::string>& arguments);

inline bool is_non_apply_op(const expression_ref& E)
{
    return E.type() == type_constant::operation_type;
}

inline bool is_non_apply_op_exp(const expression_ref& E)
{
    return is_non_apply_op(E.head());
}

#endif
