#include <cassert>
#include "choose.H"
#include "rng.H"
#include "logsum.H"
#include "myexception.H"

using std::vector;

int choose2(efloat_t x, efloat_t y) 
{
  std::vector<efloat_t> Pr(2);
  Pr[0] = x;
  Pr[1] = y;

  return choose_scratch(Pr);
}

