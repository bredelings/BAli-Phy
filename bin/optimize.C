#include <cassert>
#include <iostream>
#include "optimize.H"

using std::vector;
namespace optimize {

vector<double> search_basis(const vector<double>& start,const function& f, int maxiterations) {
  const int dimension = start.size();

  vector<double> v = start;

  // vector of sizes for each direction
  vector<double> basis(2*dimension,1);
  for(int i=dimension;i<basis.size();i++)
    basis[i] = -1;

  // where we left off last time
  int k=0;
  bool done = false;

  double value = f(start);
  for(int iterations=0;iterations<maxiterations and not done;iterations++) {
    for(int i=0;i<basis.size();i++) {
      int ii = (i+k)%basis.size();
      vector<double> nextv = v;
      nextv[ii%dimension] += basis[ii];
      double next_value = f(nextv);
      std::cout<<"iteration = "<<iterations<<
	"   ii = "<<ii<<
	"   size = "<<basis[ii]<<
	"   old = "<<value<<
	"   new = "<<next_value<<std::endl;
      if (next_value > value) {
	v = nextv;
	value = next_value;
	k == ii;
	if (i==0) basis[ii] *= 2.0;
	break;
      }
      else {
	basis[ii] /= 2.0;
      }
    }
    for(int i=0;i<v.size();i++)
      std::cout<<v[i]<<"  ";
    std::cout<<std::endl;

    done = true;
    for(int i=0;i<basis.size();i++)
      if (std::abs(basis[i]) > 1.0e-9) done = false;
  }
  assert(done);

  return v;
}

}
