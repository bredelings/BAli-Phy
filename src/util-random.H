#include <cassert>
#include "rng.H"
#include "util.H"

template <typename T>
void random_shuffle(std::vector<T>& v) 
{
  for(int i=1;i<v.size();i++) 
  {
    int l = myrandom(i+1);
    if (l<i)
      std::swap(v[l],v[i]);
  }
}

template <typename T>
std::vector<T> randomize(const std::vector<T>& v) 
{
  std::vector<T> v2 = v;
  random_shuffle(v2);
  return v2;
}


std::vector<int> random_permutation(int size);
