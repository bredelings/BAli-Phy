// Can we get to typed refs? e.g. term_ref<Double> expression_ref<Double>
// Can we get using operator* and operator+ and int and double constants?
//   This involves getting Multiple<Double,Double,Double> down to Multiply<Double,Double>
// Can we add more checks for duplicates when adding things, e.g. state nodes.

/*---------------------------------------------------------------------*/
#include <boost/shared_ptr.hpp>
#include <vector>
#include <string>
#include <iostream>
#include "cow-ptr.H"
#include "object.H"
#include "util.H"
#include "myexception.H"

#include "formula.H"

#include "context.H"

#include "computation.H"

#include "operation.H"

#include "operations.H"
/*-----------------------------------------------------------------------------*/


