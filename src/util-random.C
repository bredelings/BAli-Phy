#include "util.H"
#include "rng.H"

using std::vector;

vector<int> randomize(const std::vector<int>& v) {
  vector<int> work = v;

  vector<int> newv = v;
  for(int i=0;i<newv.size();i++) {
    int j = myrandom(work.size());
    newv[i] = work[j];
    work.erase(work.begin()+j);
  }
  assert(work.size()==0);
  assert(newv.size() == v.size());

  return newv;
}
