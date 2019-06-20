#ifndef RANDOM_VARIABLE_H
#define RANDOM_VARIABLE_H

#include "computation/closure.H"
#include "computation/type_constant.H"
#include "computation/operation.H"
#include "computation/expression/expression_ref.H"
class OperationArgs;

closure random_variable_op(OperationArgs&);

struct random_variable: public Operation
{
    random_variable* clone() const {return new random_variable(*this);}

    type_constant type() const {return random_variable_type;}

    random_variable();
};

bool is_random_variable(const expression_ref& E);

#endif
