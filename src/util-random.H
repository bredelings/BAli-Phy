#include "rng.H"
#include <cassert>

template <typename T>
std::vector<T> randomize(const std::vector<T>& v) {
  std::vector<T> work = v;

  std::vector<T> newv = v;
  for(int i=0;i<newv.size();i++) {
    int j = myrandom(work.size());
    newv[i] = work[j];
    work.erase(work.begin()+j);
  }
  assert(work.size()==0);
  assert(newv.size() == v.size());

  return newv;
}
