#ifndef INTERCHANGEABLE_H
#define INTERCHANGEABLE_H

#include "computation/closure.H"
#include "computation/type_constant.H"
#include "computation/operation.H"
#include "computation/machine/args.H"

closure interchangeable_op(OperationArgs&);

struct interchangeable: public Operation
{
    interchangeable* clone() const {return new interchangeable(*this);}

    type_constant type() const {return type_constant::interchangeable_type;}

    interchangeable();
};

bool is_interchangeable(const expression_ref&);

#endif
