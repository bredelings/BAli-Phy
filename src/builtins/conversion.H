#ifndef CONVERSION_H
#define CONVERSION_H

#include "computation/computation.H"

template<typename X, typename Y>
closure numeric_conversion_function(OperationArgs& Args)
{
  object_ptr<const POD_math<X> > x = Args.evaluate_as<POD_math<X> >(0);

  return object_ptr<const POD_math<Y>>(new POD_math<Y>(*x));
}

#endif
