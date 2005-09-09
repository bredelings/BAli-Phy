#include "bootstrap.H"

using std::vector;

vector<int> bootstrap_sample_indices(unsigned size,unsigned blocksize) 
{
  assert(blocksize <= size);

  vector<int> resample(size);

  int i=0;
  while(i<size) {
    int j = myrandom(size+1-blocksize);
    for(int k=0;k<blocksize and i < size;k++)
      resample[i++] = j++;
  }

  return resample;
}

