#include <cassert>
#include "choose.H"
#include "rng.H"
#include "logsum.H"
#include "myexception.H"

using std::vector;

int choose2(efloat_t x, efloat_t y) 
{
  efloat_t sum = x+y;

  efloat_t r = myrandomf();

  if (r*sum < x)
    return 0;
  else
    return 1;
}

