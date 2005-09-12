#include "bootstrap.H"

using std::vector;

void bootstrap_sample_indices(vector<int>& sample,unsigned blocksize) 
{
  unsigned size = sample.size();

  if (blocksize > size) 
    blocksize = size;

  int i=0;
  while(i<size) {
    int j = myrandom(size+1-blocksize);
    for(int k=0;k<blocksize and i < size;k++)
      sample[i++] = j++;
  }
}

vector<int> bootstrap_sample_indices(unsigned size,unsigned blocksize) 
{
  vector<int> sample(size);

  bootstrap_sample_indices(sample,blocksize);

  return sample;
}

