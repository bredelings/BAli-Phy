#include <iostream>
#include <cassert>
#include "optimize.H"

using namespace optimize;
using std::vector;

class parabola:public function {
  double x;
  double y;
public:
  double operator()(const vector<double>& v) const {
    assert(v.size() == 2);
    return -( (v[0]-x)*(v[0]-x)+(v[1]-y)*(v[1]-y) );
  }

  parabola(double x1,double y1):x(x1),y(y1) {}
};


int main() {
  parabola P(2.3456789,1.2345678);

  vector<double> start(2,0);

  vector<double> finish = search_basis(start,P);

  std::cout<<finish[0]<<"  "<<finish[1]<<std::endl;

  return 0;
}
