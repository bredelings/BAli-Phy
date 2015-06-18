#ifndef IOTA_H
#define IOTA_H

#include "computation/computation.H"

template <typename T>
closure iota_function(OperationArgs& Args)
{
  unsigned n = Args.evaluate(0).as_int();
  
  object_ptr<Box<std::vector<T> > > V(new Box<std::vector<T> >() );
  
  V->resize(n);
  for(unsigned i=0;i<n;i++)
    (*V)[i] = i;
  
  return V;
}

#endif
