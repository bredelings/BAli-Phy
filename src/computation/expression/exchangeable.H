#ifndef EXCHANGEABLE_H
#define EXCHANGEABLE_H

#include "computation/closure.H"
#include "computation/type_constant.H"
#include "computation/operation.H"
#include "computation/machine/args.H"

closure exchangeable_op(OperationArgs&);

struct exchangeable: public Operation
{
    exchangeable* clone() const {return new exchangeable(*this);}

    type_constant type() const {return exchangeable_type;}

    exchangeable();
};

bool is_exchangeable(const expression_ref&);

#endif
