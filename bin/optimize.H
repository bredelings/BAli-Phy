#ifndef OPTIMIZE_H
#define OPTIMIZE_H

#include <vector>
#include <valarray>

namespace optimize {
  using std::valarray;

  typedef valarray<double> Vector;

  inline double dot(const Vector& v1,const Vector& v2) {
    return (v1*v2).sum();
  }

  class function {
  public:
    virtual double operator()(const Vector&) const =0;
    virtual ~function() {}
  };

  /// Derivative of f, using dx as the delta
  Vector gradient(const function& f,const Vector& x,const Vector& dx);

  /// Derivative of f in direction dx, using scale*dx as the delta
  double derivative(const function& f,const Vector& x,Vector dx,const double scale=0.01);

  /// 2nd Derivative of f in direction dx, using scale*dx as the delta
  double derivative2(const function& f,const Vector& x,Vector dx,const double scale=0.01);

  Vector search_basis(const Vector& start,const function& f,double delta = 1.0e-6,int maxiterations=200);

  Vector search_gradient(const Vector& start,const function& f,double delta = 1.0e-6,int maxiterations=200);

}

#endif
