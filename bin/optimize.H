#ifndef OPTIMIZE_H
#define OPTIMIZE_H

#include <vector>

namespace optimize {

  class function {
  public:
    virtual double operator()(const std::vector<double>&) const =0;
    ~function() {}
  };

  std::vector<double> search_basis(const std::vector<double>& start,const function& f,int maxiterations=200);

}

#endif
