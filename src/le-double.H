#ifndef LE_DOUBLE_H
#define LE_DOUBLE_H
#include <cassert>
#include <climits>

class le_double {
  static const int BITS=53;
  static bool initialized;
  static double* T;

  double m;
  int e;


public:
  static void initialize() {
    T = new double[BITS+1];
    for(int i=0;i<=BITS;i++)
      T[i] = pow(2,-i);
    initialized=true;
  }

  bool balanced() const {
    return ((0.5 <= m and m < 1) or e == INT_MIN);
  }

  double log() const {
    if (m==0) return -1.0/0;
    return ::log(m) + e/M_LN2;
  }

  // does adding __THROW make a different?
  inline le_double& operator+=(const le_double& y) throw() {
    int delta = y.e-e;
    if (delta < -BITS)
      return *this;
    else if (delta > BITS) {
      (*this)=y;
      return *this;
    }
    else if (delta < 0)
      m = m + y.m * T[-delta];
    else {
      e = y.e;
      m = T[delta]*m + y.m;
    }

    if (m >= 1) {
      m /= 2;
      e++;
    }

    assert(balanced());
    return *this;
  }

  le_double& operator-=(const le_double& y) throw() {
    double ym = y.m;
    int delta = y.e-e;
    if (delta < BITS)
      return *this;
    else if (delta > BITS) {
      (*this)=y;
      return *this;
    }
    else if (delta < 0)
      m = m + ym *T[-delta];
    else {
      e += delta;
      m = T[delta]*m + y.m;
    }

    if (m >= 1) {
      m /= 2;
      assert(e < INT_MAX);
      e++;
    }
    assert(balanced());
    return *this;
  }

  le_double& operator*= (const le_double& y) throw() {
    m *= y.m;
    e += y.e;
    if (m < 0.5) {
      if (m==0) {    // how much slowdown does this give?
	e = INT_MIN;
      }
      else {
	m *= 2;
	assert(e > INT_MIN);
	e--;
      }
    }
    assert(balanced());
    return *this;
  }

  le_double& operator/=(const le_double& y) throw() {
    m /= y.m;
    e -= y.e;
    assert(balanced());
    return *this;
  }

  le_double& operator=(double d) throw() {
    if (d==0) {
      m=0;
      e=INT_MIN;
    }
    else
      m = frexp(d,&e); 
    assert(balanced());
    return *this;
  }
  
  le_double() throw() {};

  le_double(double d) throw() { 
    operator=(d);
  }

  operator double() const throw() {
    return ldexp(m,e); 
  }
};

bool le_double::initialized=false;
double* le_double::T = NULL;

double log(const le_double& x) {return x.log();}

inline le_double operator+(const le_double& x,const le_double& y) throw() {
  le_double z=x;
  z += y;
  return z;
}

inline le_double operator-(const le_double& x,const le_double& y) throw() {
  le_double z=x;
  z-=y;
  return z;
}


inline le_double operator*(const le_double& x,const le_double& y) throw() {
  le_double z=x;
  z*=y;
  return z;
}

inline le_double operator/(const le_double& x,const le_double& y) throw() {
  le_double z=x;
  z/=y;
  return z;
}

#endif
