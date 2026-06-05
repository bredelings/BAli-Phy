#ifndef OPERATIONS_H
#define OPERATIONS_H

#include "util/myexception.H"
#include "operation.H"

class OperationArgs;

// In general, these operations simply ask for the value of each input
// from the OperationArgs object.  They therefore need have no knowledge of
// how these values are arrived at.

closure apply_op(OperationArgs&);

struct Apply: public Operation
{
    virtual Apply* clone() const {return new Apply(*this);}

    int precedence() const {return 10;}

    type_constant type() const {return type_constant::apply_type;}

    Apply():Operation(apply_op,"@") {}
};

closure case_op(OperationArgs&);

struct Case: public Operation
{
    virtual Case* clone() const {return new Case(*this);}

    type_constant type() const {return type_constant::case_type;}

    Case():Operation(case_op, "case") {}
};

closure let_op(OperationArgs&);

#endif
