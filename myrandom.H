#ifndef MYRANDOM_H
#define MYRANDOM_H
#include <cstdlib>

//Returns a random number r such that 0 <= r < (the argument to the function).
unsigned myrand_init(int=-1);

inline double myrandomf() {
  return drand48();
}

inline int myrandom() {
  return mrand48();
}

inline long myrandom(long max) {
  return (long)(myrandomf()*max);
}

inline long myrandom(long min,long max) {
  return myrandom(max-min+1)+min;
}

class chancein {
  long Nm; //N divided by m
  long N;  //One chance in N.  But N is modified slightly to get the best fit
  long i;  //If i==r, then "it" happens
  long r;  
public:
  bool happen();
  void set_chance(float);
  chancein();
  chancein(float);
};

inline bool chancein::happen() {
  bool success=false;
  if (i==r) {
    success=true;
    i=0;
    r=myrandom(N);
  }
  else if (++i>=Nm) {
    i=0;
    r=myrandom(N);
  }
  return success;
}

#endif
