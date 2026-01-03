#ifndef OPERATION_H
#define OPERATION_H

#include <vector>
#include <string>
#include <variant>

#include "computation/object.H"

class OperationArgs;

#include "closure.H"
#include "type_constant.H"

typedef closure (*o_operation_fn)(OperationArgs&);

// An operation with unboxed arguments and unboxed result.
//   #
//   # -> #
//   # -> # -> #
//   # -> # -> # -> #
// etc.
//
// In theory we could handle
//   # -> * -> #
// if we knew not to evaluate the 2nd arg.
typedef expression_ref (*e_operation_fn)(std::vector<expression_ref>&);

closure evaluate_e_op_to_c(OperationArgs& Args);
expression_ref evaluate_e_op(OperationArgs& Args, const expression_ref& E);

struct Operation: public Object
{
    std::string name_;

    o_operation_fn op = nullptr;

    e_operation_fn e_op = nullptr;
    
    // If we have an e_operation_op, then are arguments used or forced?
    bool force_only = false;

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
    Operation(o_operation_fn f, const std::string& s):name_(s),op(f) { }
    Operation(e_operation_fn f, const std::string& s):name_(s),op(evaluate_e_op_to_c),e_op(f) { }
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


inline bool is_eop_exp(const expression_ref& E)
{
    auto O = E.head().to<Operation>();
    if (O and O->e_op)
        return true;
    else
        return false;
}
#endif
