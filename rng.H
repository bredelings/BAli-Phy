#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <valarray>

using std::valarray;

namespace rng {

  typedef int amount_t;
  typedef valarray<amount_t> tuple;

  class RNG {

  protected:
    gsl_rng* generator;


  public:
    void seed(unsigned long int);
    
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



