#ifndef OPERATIONS_H
#define OPERATIONS_H

#include "operation.H"

class OperationArgs;

// In general, these operations simply ask for the value of each input
// from the OperationArgs object.  They therefore need have no knowledge of
// how these values are arrived at.

closure apply_op(OperationArgs&);

closure case_op(OperationArgs&);

closure let_op(OperationArgs&);

#endif
