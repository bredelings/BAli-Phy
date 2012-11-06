#ifndef DISTRIBUTION_OPERATIONS_H
#define DISTRIBUTION_OPERATIONS_H

#include <vector>
#include <valarray>
#include <string>
#include "computation/expression.H"
#include "computation/operation.H"
#include "computation/computation.H"
#include "probability/probability.H"
#include "computation/program.H"

struct exponential_density: public Operation
{
  exponential_density* clone() const {return new exponential_density;}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "exponential_density";}

  exponential_density():Operation(2) { }
};

extern expression_ref exponential_dist;

struct log_exponential_density: public Operation
{
  log_exponential_density* clone() const {return new log_exponential_density;}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "log_exponential_density";}

  log_exponential_density():Operation(2) { }
};

extern expression_ref log_exponential_dist;

struct gamma_density: public Operation
{
  gamma_density* clone() const {return new gamma_density;}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "gamma_density";}

  gamma_density():Operation(3) { }
};

struct gamma_quantile_op: public Operation
{
  gamma_quantile_op* clone() const {return new gamma_quantile_op;}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "gamma_quantile";}

  gamma_quantile_op():Operation(3) { }
};

extern expression_ref gamma_dist;

struct log_gamma_density: public Operation
{
  log_gamma_density* clone() const {return new log_gamma_density;}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "log_gamma_density";}

  log_gamma_density():Operation(3) { }
};

extern expression_ref log_gamma_dist;

struct beta_density: public Operation
{
  beta_density* clone() const {return new beta_density;}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "beta_density";}

  beta_density():Operation(3) { }
};

extern expression_ref beta_dist;

struct beta_quantile_op: public Operation
{
  beta_quantile_op* clone() const {return new beta_quantile_op;}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "beta_quantile_op";}

  beta_quantile_op():Operation(3) { }
};

struct normal_density: public Operation
{
  normal_density* clone() const {return new normal_density;}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "normal_density";}

  normal_density():Operation(3) { }
};

extern expression_ref normal_dist;

struct log_normal_density: public Operation
{
  log_normal_density* clone() const {return new log_normal_density;}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;
 
  std::string name() const {return "log_normal_density";}

  log_normal_density():Operation(3) { }
};

struct log_normal_quantile_op: public Operation
{
  log_normal_quantile_op* clone() const {return new log_normal_quantile_op;}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;
 
  std::string name() const {return "log_normal_quantile";}

  log_normal_quantile_op():Operation(3) { }
};

extern expression_ref log_normal_dist;

struct cauchy_density: public Operation
{
  cauchy_density* clone() const {return new cauchy_density;}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "cauchy_density";}

  cauchy_density():Operation(3) { }
};

extern expression_ref cauchy_dist;

struct dirichlet_density: public Operation
{
  dirichlet_density* clone() const {return new dirichlet_density;}
    
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "dirichlet_density";}
    
  dirichlet_density():Operation(2) { }
};

extern expression_ref dirichlet_dist;

struct laplace_density: public Operation
{
  laplace_density* clone() const {return new laplace_density;}
    
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "laplace_density";}
    
  laplace_density():Operation(3) { }
};

extern expression_ref laplace_dist;

struct log_laplace_density: public Operation
{
  log_laplace_density* clone() const {return new log_laplace_density;}
    
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "log_laplace_density";}
    
  log_laplace_density():Operation(3) { }
};

extern expression_ref log_laplace_dist;

struct epsilon_density: public Operation
{
  epsilon_density* clone() const {return new epsilon_density;}
    
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "epsilon_density";}
    
  epsilon_density():Operation(2) { }
};

struct uniform_density: public Operation
{
  uniform_density* clone() const {return new uniform_density;}
    
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "uniform_density";}
    
  uniform_density():Operation(3) { }
};

extern expression_ref uniform_dist;

struct bernoulli_prob: public Operation
{
  bernoulli_prob* clone() const {return new bernoulli_prob;}
    
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "bernoulli_prob";}
    
  bernoulli_prob():Operation(2) { }
};

extern expression_ref bernoulli_dist;

// 2 Fields: string, density op
extern expression_ref prob_density;

Program Distribution_Functions();

#endif
