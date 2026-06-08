#ifndef OPERATION_H
#define OPERATION_H

#include <vector>
#include <string>

#include "computation/object.H"
#include "type_constant.H"

class OperationArgs;
struct closure;

namespace Runtime
{
    struct Exp;
}

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
typedef Runtime::Exp (*e_operation_fn)(std::vector<Runtime::Exp>&);

closure evaluate_e_op_to_c(OperationArgs& Args);
Runtime::Exp evaluate_e_op(OperationArgs& Args, const Runtime::Exp& E);

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

bool is_eop_exp(const Runtime::Exp& E);
#endif
