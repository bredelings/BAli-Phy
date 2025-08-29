#ifndef MODIFIABLE_H
#define MODIFIABLE_H

#include "computation/closure.H"
#include "computation/type_constant.H"
#include "computation/operation.H"
#include "computation/machine/args.H"

closure modifiable_op(OperationArgs&);

struct modifiable: public Operation
{
    modifiable* clone() const {return new modifiable(*this);}

    type_constant type() const {return type_constant::modifiable_type;}

    modifiable();
};

bool is_modifiable(const expression_ref&);

#endif
