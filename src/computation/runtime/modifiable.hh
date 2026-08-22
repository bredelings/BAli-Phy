#ifndef MODIFIABLE_H
#define MODIFIABLE_H

#include "computation/closure.hh"
#include "computation/type_constant.hh"
#include "computation/operation.hh"
#include "computation/machine/args.hh"

closure modifiable_op(OperationArgs&);

struct modifiable: public Operation
{
    modifiable* clone() const {return new modifiable(*this);}

    type_constant type() const {return type_constant::modifiable_type;}

    modifiable();
};

bool is_modifiable(const Runtime::Exp&);

#endif
