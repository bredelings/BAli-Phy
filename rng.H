#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <valarray>

using std::valarray;

unsigned long myrand_init();
unsigned long myrand_init(unsigned long);

// returns a value in [0,max-1]
unsigned long myrandom(unsigned long max);

// returns a value in [min,max-1]
inline long myrandom(long min,long max) {
  unsigned long diff = max - min;
  return myrandom(diff)+min;
}

// returns a value in [0,1)
double myrandomf();

// return the log of a variable that is uniform on [0,1)
double log_unif();

// return the log of a variable that is uniform on [0,1)
double gaussian(double,double);


namespace rng {

  typedef int amount_t;
  typedef valarray<amount_t> tuple;

  class RNG {

  protected:
    gsl_rng* generator;
    

  public:
    unsigned long int seed(unsigned long int);
    unsigned long int seed();
    
    unsigned long min() const { return gsl_rng_min(generator); }
    unsigned long max() const { return gsl_rng_max(generator); }

    unsigned long get() { return gsl_rng_get(generator) ;}

    double uniform() { return gsl_rng_uniform(generator); }
    
    double uniform_int(unsigned long int n) { return gsl_rng_uniform_int(generator,n); }

    double log_unif() {return -gsl_ran_exponential(generator,1.0);}

    double gaussian(double mu,double sigma) {return gsl_ran_gaussian(generator,sigma)+mu;}

    RNG();
    ~RNG();
  };


  class Binomial:public RNG {
  public:
    unsigned operator()(double p,unsigned long n);
  };
  
  
  class Poisson:public RNG {
  public:
    unsigned operator()(double lambda);
  };
  

  class Multinomial {
    Binomial Bin;
  public:
    tuple operator()(const valarray<double>& p,unsigned long n);
  };

  class Exponential: public RNG {
    double mu;
  public:
    double operator()();

    Exponential(double d):mu(d) { assert(mu>0.0); }
  };

  void init();
};

namespace dng {
  
  typedef double amount_t;
  typedef valarray<amount_t> tuple;
  
  class Binomial {
  public:
    double operator()(double p,double n) { return p*n;}
  };
  
  
  class Poisson {
  public:
    double operator()(double lambda) {return lambda;}
  };
  
  
  class Multinomial {
  public:
    tuple operator()(const valarray<double>& p,double n) { return p*n;}
  };


  void init();
};



