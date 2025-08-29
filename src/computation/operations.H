#ifndef OPERATIONS_H
#define OPERATIONS_H

#include <vector>
#include <string>
#include "util/myexception.H"
#include "operation.H"
#include "expression/expression_ref.H"

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

struct Let: public Operation
{
    virtual Let* clone() const {return new Let(*this);}

    type_constant type() const {return type_constant::let2_type;}

    std::string print() const;

    std::vector<expression_ref> binds;

    expression_ref body;

    Let(const std::vector<expression_ref>& bs, const expression_ref& b)
        :Operation(let_op, "let"),
         binds(bs),
         body(b)
        {}
};

#endif
