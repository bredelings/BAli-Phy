#ifndef LE_DOUBLE_H
#define LE_DOUBLE_H
#include <cassert>
#include <climits>

#include "logsum.H"

class le_double_t {
  static const int BITS=53+1;
  static bool initialized;
  static double* table;

  /// the mantissa, in [0.5 , 1.0 )
  double m;

  /// the exponent
  int e;


public:
  static inline int cmp(const le_double_t& x,const le_double_t& y) {
    if (x.e < y.e) return -1;
    if (x.e > y.e) return 1;
    if (x.m < y.m) return -1;
    if (x.m > y.m) return 1;
    return 0;
  }

  static void initialize() {
    table = new double[BITS+1];
    for(int i=0;i<=BITS;i++)
      table[i] = pow(2.0,-i);
    initialized=true;
  }

  bool balanced() const {
    return ((0.5 <= m and m < 1) or e == INT_MIN);
  }

  double log() const {
    if (m==0) return log_0;
    return ::log(m) + e*M_LN2;
  }

  friend le_double_t pow(const le_double_t& x,double p);

  // does adding __THROW make a different?
  inline le_double_t& operator+=(const le_double_t& y) throw() 
  {
    if (y.e < e) {
      unsigned delta = (unsigned)(e-y.e);
      if (delta > BITS)
	return *this;
      else
	m = m + y.m * table[delta];
    }
    else {
      unsigned delta = (unsigned)(y.e-e);
      if (delta > BITS) {
	(*this)=y;
	return *this;
      }
      else {
	m = table[delta]*m + y.m;
	e = y.e;
      }
    }

    if (m >= 1) {
      m /= 2;
      e++;
    }

    // We don't check for -x + x -> 0. But 0+0->0 does work.
    // FIXME - assert that everything has the same sign?
    assert(m != 0 or y.m==0);

    assert(balanced());
    return *this;
  }

  le_double_t& operator-=(const le_double_t& y) throw() 
  {
    assert(cmp(*this,y) >= 0);
    if (y.e < e) {
      unsigned delta = (unsigned)(e-y.e);
      if (delta > BITS)
	return *this;
      else
	m = m - y.m * table[delta];
    }      
    else {
      unsigned delta = (unsigned)(y.e-e);
      if (delta > BITS) {
	(*this)=y;
	return *this;
      }
      else {
	m = table[delta]*m - y.m;
	e = y.e;
      }
    }

    if (m < 0.5) {
      if (m==0)
	e = INT_MIN;
      else
	while (m<0.5) {
	  m *= 2;
	  e--;
	}
    }
    assert(balanced());
    return *this;
  }

  le_double_t& operator*= (const le_double_t& y) throw() {
    m *= y.m;
    e += y.e;
    if (m < 0.5) {
      if (m==0)     // we need this to correctly multiple by zero
	e = INT_MIN;
      else {
	m *= 2;
	assert(e > INT_MIN);
	e--;
      }
    }
    assert(balanced());
    return *this;
  }

  le_double_t& operator/=(const le_double_t& y) throw() {
    m /= y.m;
    e -= y.e;

    if (m >= 1) {
      m /= 2;
      e++;
    }
    else if (m == 0) // do we really need this/
      e = INT_MIN;

    assert(balanced());
    return *this;
  }

  le_double_t& operator=(double d) throw() {
    assert( d >= 0 );

    if (d==0) {
      m=0;
      e=INT_MIN;
    }
    else
      m = frexp(d,&e); 
    assert(balanced());
    return *this;
  }
  
  le_double_t() throw() {};

  le_double_t(double d) throw() { 
    operator=(d);
  }

  operator double() const throw() {
    return ldexp(m,e); 
  }
};

#define decl_to_convert_from_double(rtype,op)  \
inline rtype operator op (double x,const le_double_t& y) { \
  return le_double_t(x) op y; \
} \
\
inline rtype operator op (const le_double_t& x,double y) {\
  return x op le_double_t(y);\
}

inline le_double_t operator+(const le_double_t& x,const le_double_t& y) throw() {
  le_double_t z=x;
  z += y;
  return z;
}

inline le_double_t operator-(const le_double_t& x,const le_double_t& y) throw() {
  le_double_t z=x;
  z-=y;
  return z;
}


inline le_double_t operator*(const le_double_t& x,const le_double_t& y) throw() {
  le_double_t z=x;
  z*=y;
  return z;
}

inline le_double_t operator/(const le_double_t& x,const le_double_t& y) throw() {
  le_double_t z=x;
  z/=y;
  return z;
}

decl_to_convert_from_double(le_double_t,+)
decl_to_convert_from_double(le_double_t,-)
decl_to_convert_from_double(le_double_t,*)
decl_to_convert_from_double(le_double_t,/)

inline bool operator <  (le_double_t x,le_double_t y) {
  return le_double_t::cmp(x,y) < 0;
}

inline bool operator <= (le_double_t x,le_double_t y) {
  return le_double_t::cmp(x,y) <= 0;
}

inline bool operator >  (le_double_t x,le_double_t y) {
  return le_double_t::cmp(x,y) > 0;
}

inline bool operator >= (le_double_t x,le_double_t y) {
  return le_double_t::cmp(x,y) >= 0;
}

inline bool operator == (le_double_t x,le_double_t y) {
  return le_double_t::cmp(x,y) == 0;
}

inline bool operator != (le_double_t x,le_double_t y) {
  return le_double_t::cmp(x,y) != 0;
}

decl_to_convert_from_double(bool,< )
decl_to_convert_from_double(bool,<=)
decl_to_convert_from_double(bool,> )
decl_to_convert_from_double(bool,>=)
decl_to_convert_from_double(bool,==)
decl_to_convert_from_double(bool,!=)

inline double log(const le_double_t& x) {return x.log();}

// (m*2^e)^p  = (m^p)*(2^(ep)) = (m^p * 2^frac(ep)) * 2^(int)ep
inline le_double_t pow(const le_double_t& x,double p) 
{
  double round;
  double fraction = modf(x.e * p, &round);

  le_double_t y = pow(x.m,p) * pow(2,fraction);

  if (y.m != 0)
    y.e += (int)round;

  assert(y.balanced());

  return y;
}
#endif
