#ifndef INTERCHANGEABLE_H
#define INTERCHANGEABLE_H

#include "computation/closure.hh"
#include "computation/type_constant.hh"
#include "computation/operation.hh"
#include "computation/machine/args.hh"

closure interchangeable_op(OperationArgs&);

struct interchangeable: public Operation
{
    interchangeable* clone() const {return new interchangeable(*this);}

    type_constant type() const {return type_constant::interchangeable_type;}

    interchangeable();
};

bool is_interchangeable(const Runtime::Exp&);

#endif
