#include <cassert>
#include <ctime>
#include <cmath>

#include <iostream>

#include "rng.H"

void dng::init() { }

using namespace rng;

void rng::init() {
  // set up default generator and default seed from environment
  gsl_rng_env_setup();
}

void RNG::seed(unsigned long int s) {
  assert(generator != NULL);
  gsl_rng_set(generator,s);
}


RNG::RNG() {
  generator = gsl_rng_alloc(gsl_rng_default);
  time_t t = time(NULL);

  seed(t);
}

RNG::~RNG() {
  gsl_rng_free(generator);
}



unsigned Binomial::operator()(double p,unsigned long n1) {
  //  cerr<<"mean = "<<p*n<<endl;
  unsigned n = (unsigned)n1;
  assert((unsigned long)(n) == n1);
  
  assert(p>=0.0);
  assert(p<=1.0+1e-10);
  assert(n>=0);

  return gsl_ran_binomial(generator,p,n);
}


unsigned Poisson::operator()(double lambda) {
  //  cerr<<"lambda = "<<lambda<<endl;
  assert(lambda>=0.0);
  
  return gsl_ran_poisson(generator,lambda);
}


// this division and stuff is killing us
tuple Multinomial::operator()(const valarray<double>& p,unsigned long n1) {
  unsigned n = (unsigned)n1;
  assert((unsigned long)(n) == n1);

  assert(abs(double(p.sum())-1.0) < 1e-10);

  tuple m(0,p.size());
  double remaining = 1.0;
  for(int i=1;n && i<p.size();i++) {
    m[i] = Bin(p[i]/remaining,n);
    n -= m[i];
    remaining -= p[i];
  }
  m[0] = n;
  return m;
}



/*
  Hey!  I could plug this into the other Multinomial, so that whenever the "n" decreases to less than
  k*(p.size()-i) we switch algorithms...

  tuple m(0,p.size());
  if (n < p.size()) {
    double r = random();
    double sum=0;
    for(int i=0;i<p.size();i++) {
      sum += p[i];
      if (r<=sum) {
	m[i]++;
	break;
      }
    }
  }
*/
