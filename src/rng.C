#include <cassert>
#include <ctime>
#include <cmath>
#include <fstream>
#include <iostream>

#include "rng.H"

/************* Interfaces to rng::standard *********************/
namespace rng {
  RNG* standard;
};

unsigned long myrand_init() {
  assert(not rng::standard);
  rng::init();
  unsigned long s = rng::standard->seed();
  
  assert(rng::standard);
  return s;
}

unsigned long myrand_init(unsigned long s) {
  assert(not rng::standard);
  rng::init();
  s = rng::standard->seed(s);
  
  assert(rng::standard);
  return s;
}

double uniform() {
  return rng::standard->uniform();
}

double myrandomf() {
  return uniform();
}

double log_unif() {
  return rng::standard->log_unif();
}

double gaussian(double mu,double sigma) {
  return rng::standard->gaussian(mu,sigma);
}

double shift_laplace(double mu,double sigma) {
  return rng::standard->shift_laplace(mu,sigma);
}

double exponential(double mu) {
  return rng::standard->exponential(mu);
}

unsigned poisson(double mu) {
  return rng::standard->poisson(mu);
}

unsigned geometric(double mu) {
  return rng::standard->geometric(mu);
}

/*************** Functions for rng,dng and RNG **************/
void dng::init() { }

using namespace rng;

void rng::init() {
  // set up default generator and default seed from environment
  gsl_rng_env_setup();
  standard = new RNG;
}

unsigned long RNG::seed() {
  unsigned long s=0;

  std::ifstream random("/dev/urandom");
  for(int i=0;i<sizeof(s);i++) {
    unsigned char c;
    random >> c;
    s <<= 8;
    s |=  c;
  }
  random.close();
  return seed(s);
}

unsigned long RNG::seed(unsigned long int s) {
  assert(generator != NULL);
  gsl_rng_set(generator,s);
  return s;
}

RNG::RNG() {
  generator = gsl_rng_alloc(gsl_rng_default);

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

double Exponential::operator()() {
  return gsl_ran_exponential(generator,mu);
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

  assert(std::abs(double(p.sum())-1.0) < 1e-10);

  tuple m(0,p.size());
  double remaining = 1.0;
  for(int i=1;n and i<p.size();i++) {
    m[i] = Bin(p[i]/remaining,n);
    n -= m[i];
    remaining -= p[i];
  }
  m[0] = n;
  return m;
}


valarray<double> dirichlet_fiddle(const valarray<double>& p1,double sigma) {

  double total=0;
  valarray<double> p2=p1;
  for(int i=0;i<p2.size();i++) {
    p2[i] *= exp(gaussian(0,sigma));
    total += p2[i];
  }

  p2 /= total;

  return p2;
}
