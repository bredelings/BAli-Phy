#include <cassert>
#include <iostream>
#include "optimize.H"

using std::vector;
namespace optimize {

vector<double> search_basis(const vector<double>& start,const function& f, int maxiterations) {
  const int dimension = start.size();

  vector<double> v = start;

  // vector of sizes for each direction
  vector<bool> moved(2*dimension,false);
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
      std::cerr<<"iteration = "<<iterations<<
	"   ii = "<<ii<<
	"   size = "<<basis[ii]<<
	"   old = "<<value<<
	"   new = "<<next_value<<std::endl;
      if (next_value > value) {
	v = nextv;
	value = next_value;
	k = ii+1;
	if (moved[ii]) {
	  basis[ii] *= 2.0;
	  moved[ii] = false;
	}
	else
	  moved[ii] = true;
	break;
      }
      else {
	moved[ii] = false;
	basis[ii] /= 2.0;
      }
    }
    for(int i=0;i<v.size();i++)
      std::cerr<<v[i]<<"  ";
    std::cerr<<std::endl;

    done = true;
    for(int i=0;i<basis.size();i++)
      if (std::abs(basis[i]) > 1.0e-9) done = false;
  }
  assert(done);

  return v;
}

}
