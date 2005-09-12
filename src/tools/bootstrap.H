#ifndef BOOTSTRAP_H
#define BOOTSTRAP_H 

#include <vector>
#include <valarray>
#include <cmath>
#include "rng.H"

std::vector<int> bootstrap_sample_indices(unsigned size,unsigned blocksize=1);

void bootstrap_sample_indices(std::vector<int>& sample,unsigned blocksize=1);


template <typename T>
std::valarray<T> bootstrap_sample(const std::valarray<T>& sample,int blocksize=1) {
  assert(blocksize > 0);
  assert(blocksize <= sample.size());

  std::valarray<T> resample(sample.size());

  int i=0;
  while(i<resample.size()) {
    int j = myrandom(sample.size()+1-blocksize);
    for(int k=0;k<blocksize and i< resample.size();k++,i++,j++)
      resample[i] = sample[j];
  }

  return resample;
}

//template <typename T,typename U>
//typedef U (*statistic_t)(const valarray<T>&);

template <typename T,typename U>
std::valarray<U> bootstrap_apply(const std::valarray<T>& sample,
				 U (*statistic)(const std::valarray<T>&),
				 int nresamples,int blocksize=1) 
{
  std::valarray<T> temp(sample.size());
  std::valarray<U> results(nresamples);
  for(int i=0;i<nresamples;i++) {
    temp = bootstrap_sample(sample,blocksize);
    results[i] = statistic(temp);
  }
  return results;
}

#endif
