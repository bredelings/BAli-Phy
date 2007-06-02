#include "util-random.H"

using std::vector;

vector<int> random_permutation(int size)
{
  vector<int> v = iota(size);
  random_shuffle(v);
  return v;
}
