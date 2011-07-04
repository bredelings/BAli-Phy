#ifndef DISTRIBUTION_OPERATIONS_H
#define DISTRIBUTION_OPERATIONS_H

#include <vector>
#include <valarray>
#include <string>
#include "expression.H"
#include "operation.H"
#include "computation.H"
#include "probability.H"

struct exponential_density: public Operation
{
  exponential_density* clone() const {return new exponential_density;}
  
  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    boost::shared_ptr<const Double> x = Args.evaluate_as<Double>(0);
    boost::shared_ptr<const Double> mu = Args.evaluate_as<Double>(1);

    Log_Double result = log_double_t(0);
    if (*x >= 0.0)
      result = exp<log_double_t>(-*x/ *mu)/ *mu;
    return boost::shared_ptr<const Object>(result.clone());
  }

  std::string name() const {return "exponential_density";}

  exponential_density():Operation(2) { }
};

struct dirichlet_density: public Operation
{
  dirichlet_density* clone() const {return new dirichlet_density;}
    
  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    expression_ref X = Args.evaluate(0);
    expression_ref N = Args.evaluate(1);
      
    std::valarray<double> x = get_varray<double,Double>(X);
    std::valarray<double> n = get_varray<double,Double>(N);

    boost::shared_ptr<Log_Double> R (new Log_Double( ::dirichlet_pdf(x,n) ) );

    return R;
  }

  std::string name() const {return "dirichlet_density";}
    
  dirichlet_density():Operation(2) { }
};

struct laplace_density: public Operation
{
  laplace_density* clone() const {return new laplace_density;}
    
  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    double x = *Args.evaluate_as<Double>(0);
    expression_ref A =  Args.evaluate(1);

    std::vector<double> a = get_vector<double,Double>(A);

    return boost::shared_ptr<Log_Double> (new Log_Double( ::laplace_pdf(x,a[0],a[1]) ) );
  }

  std::string name() const {return "laplace_density";}
    
  laplace_density():Operation(2) { }
};

struct log_laplace_density: public Operation
{
  log_laplace_density* clone() const {return new log_laplace_density;}
    
  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    double x = *Args.evaluate_as<Double>(0);
    expression_ref A =  Args.evaluate(1);

    std::vector<double> a = get_vector<double,Double>(A);

    return boost::shared_ptr<Log_Double> (new Log_Double( ::laplace_pdf(log(x),a[0],a[1])/x ) );
  }

  std::string name() const {return "log_laplace_density";}
    
  log_laplace_density():Operation(2) { }
};

extern expression_ref log_laplace;

struct uniform_density: public Operation
{
  uniform_density* clone() const {return new uniform_density;}
    
  boost::shared_ptr<const Object> operator()(OperationArgs&) const
  {
    return boost::shared_ptr<Log_Double> (new Log_Double( 1.0 ) );
  }

  std::string name() const {return "uniform_density";}
    
  uniform_density():Operation(2) { }
};


#endif
