#include <stdlib.h>
#include <assert.h>
#include <time.h>
#include "myrandom.H"

const int m=4;


void chancein::set_chance(float f) {
  if (f<1.0)  
    N=long( 1/f );
  else
    N=long(f);
  float temp = (2*N-1)*m;
  temp/=(2*m-1);
  N=(long)(temp+0.5);
  Nm=N/m;
  //assert(N>10 && N<100000);
  i=0;
  r=myrandom(N);
}

chancein::chancein() {
  Nm=0;
  N=0;
  i=0;
  r=0;
}

chancein::chancein(float f) {
  set_chance(f);
}

unsigned myrand_init(int seed) {
  time_t t = seed;
  if (seed == -1) {
    t = time(NULL);
  }
  srand48(t);
  return t;
}
