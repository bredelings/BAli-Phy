#ifndef OPERATIONS_H
#define OPERATIONS_H

#include <vector>
#include <string>
#include "myexception.H"
#include "operation.H"
#include "computation.H"
#include "expression.H"


// In general, these operations simply ask for the value of each input
// from the OperationArgs object.  They therefore need have no knowledge of
// how these values are arrived at.

closure apply_op(OperationArgs&);

struct Apply: public Operation
{
    virtual Apply* clone() const {return new Apply(*this);}

    int precedence() const {return 10;}

    assoc_type associativity() const {return assoc_left;}

    Apply():Operation(-1,apply_op,"@") {}
};

closure case_op(OperationArgs&);

struct Case: public Operation
{
    virtual Case* clone() const {return new Case(*this);}

    type_constant type() const {return case_type;}

    Case():Operation(2,case_op, "case") {}
};

closure let_op(OperationArgs&);

struct Let: public Operation
{
    virtual Let* clone() const {return new Let(*this);}

    type_constant type() const {return let2_type;}

    Let():Operation(2, let_op, "let") {}
};

closure modifiable_op(OperationArgs&);

struct modifiable: public Operation
{
    modifiable* clone() const {return new modifiable(*this);}

    type_constant type() const {return modifiable_type;}

    modifiable():Operation(2, modifiable_op,"modifiable") {}
};

#endif
