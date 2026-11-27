#ifndef IOTA_H
#define IOTA_H

#include "computation/machine/args.hh"

template <typename T>
closure iota_function(OperationArgs& Args)
{
  int n = Args.evaluate_slot_to_value(0).as_cpp_int();
  if (n < 0) throw myexception()<<"iota: negative length";
  
  object_ptr<Box<std::vector<T> > > V(new Box<std::vector<T> >() );
  
  V->resize(n);
  for(int i=0;i<n;i++)
    (*V)[i] = i;
  
  return V;
}

#endif
