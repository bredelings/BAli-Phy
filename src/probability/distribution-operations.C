#include "probability/distribution-operations.H"

#include <vector>
#include <valarray>
#include <string>
#include "computation/operation.H"
#include "computation/computation.H"
#include "probability/probability.H"
#include "bounds.H"
#include "computation/prelude.H"

using std::vector;
using std::valarray;

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

closure exponential_density::operator()(OperationArgs& Args) const
{
  double mu = *Args.evaluate_as<Double>(0);
  double x = *Args.evaluate_as<Double>(1);
  
  Log_Double result = exponential_pdf(x,mu);
  return object_ptr<const Object>(result.clone());
}

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

closure log_exponential_density::operator()(OperationArgs& Args) const
{
  double mu = *Args.evaluate_as<Double>(0);
  double x = *Args.evaluate_as<Double>(1);
  
  Log_Double result = exponential_pdf(log(x),mu)/x;
  return object_ptr<const Object>(result.clone());
}

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

closure gamma_density::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x = *Args.evaluate_as<Double>(2);
  
  Log_Double result = gamma_pdf(x, a1, a2);
  return object_ptr<const Object>(result.clone());
}

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

closure gamma_quantile_op::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double p  = *Args.evaluate_as<Double>(2);

  Double result = gamma_quantile(p, a1, a2);
  return object_ptr<const Object>(result.clone());
}

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

closure log_gamma_density::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  Log_Double result = gamma_pdf(log(x), a1, a2)/x;
  return object_ptr<const Object>(result.clone());
}

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

closure beta_density::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  Log_Double result = beta_pdf(x, a1, a2);
  return object_ptr<const Object>(result.clone());
}

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

closure beta_quantile_op::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double p  = *Args.evaluate_as<Double>(2);
  
  Double result = beta_quantile(p, a1, a2);
  return object_ptr<const Object>(result.clone());
}

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

closure normal_density::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  Log_Double result = normal_pdf(x, a1, a2);
  return object_ptr<const Object>(result.clone());
}

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

closure log_normal_density::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);

  Log_Double result = log_normal_pdf(x, a1, a2);
  return object_ptr<const Object>(result.clone());
}

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

closure log_normal_quantile_op::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double p  = *Args.evaluate_as<Double>(2);

  Double result = log_normal_quantile(p, a1 ,a2);
  return object_ptr<const Object>(result.clone());
}

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

closure cauchy_density::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);

  Log_Double result = cauchy_pdf(x, a1, a2);
  return object_ptr<const Object>(result.clone());
}

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

// First convert N from tuple to list...
// Second convert this builtin routine to just take two Vector<double> arguments.
// Third convert all the expression_ref's here to "var" and use Distribution_Functions()
closure dirichlet_density::operator()(OperationArgs& Args) const
{
  const vector<double>& n = Args.evaluate_as<Vector<double>>(0)->t;
  const vector<double>& x = Args.evaluate_as<Vector<double>>(1)->t;
  
  object_ptr<Log_Double> R (new Log_Double( ::dirichlet_pdf(x,n) ) );
  
  return R;
}

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

closure laplace_density::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  return object_ptr<Log_Double> (new Log_Double( ::laplace_pdf(x, a1, a2) ) );
}

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

closure log_laplace_density::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);

  return object_ptr<Log_Double> (new Log_Double( ::laplace_pdf(log(x),a1,a2)/x ) );
}

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

closure uniform_density::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  return object_ptr<Log_Double> (new Log_Double( ::uniform_pdf(x,a1,a2) ) );
}

// Fields: n_random, n_parameters, string, density op
expression_ref prob_density = lambda_expression( constructor("Distributions.ProbDensity",5) );

struct GetBounds: public Operation
{
  GetBounds* clone() const {return new GetBounds;}
    
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;

    if (typeid(*this) != typeid(O)) return false;

    return true;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "_GetBounds";}
    
  GetBounds():Operation(2) { }
};

closure GetBounds::operator()(OperationArgs& Args) const
{
  auto L = Args.evaluate(0);
  auto U = Args.evaluate(1);
  auto has_lower = boost::dynamic_pointer_cast<const Double>(L);
  auto has_upper = boost::dynamic_pointer_cast<const Double>(U);
  double lower = 0;
  double upper = 0;
  if (has_lower)
    lower = *has_lower;
  if (has_upper)
    upper = *has_upper;
  
  return Bounds<double>(has_lower, lower, has_upper, upper);
}

Program Range_Functions()
{
  Program P("Range");
  P.import_module(get_Prelude(),"Prelude",false);
  P.def_constructor("OpenInterval",2);
  P.def_constructor("Real",1);
  P.def_constructor("Inf",0);
  P.def_constructor("NegInf",0);
  P.def_function("builtinGetBounds", 2, lambda_expression( GetBounds() ) );

  P += "{realLine = OpenInterval Nothing Nothing}";
  P += "{above l = OpenInterval (Just l) Nothing}";
  P += "{below u = OpenInterval Nothing (Just u)}";
  P += "{between l u = OpenInterval (Just l) (Just u)}";
  P += "{getBounds (OpenInterval Nothing Nothing)   = builtinGetBounds () ();\
         getBounds (OpenInterval Nothing (Just u))  = builtinGetBounds () u;\
         getBounds (OpenInterval (Just l) Nothing)  = builtinGetBounds l ();\
         getBounds (OpenInterval (Just l) (Just u)) = builtinGetBounds l u;\
         getBounds _                                = ()}";

  return P;
}

Program Distribution_Functions()
{
  Program P("Distributions");
  P.import_module(get_Prelude(),"Prelude",false);
  P.import_module(Range_Functions(),"Range",false);

  // Note: we separate the "builtin" versions (which don't do case analysis on their arguments)
  //       from the from the real versions (which do).

  P.def_constructor("ProbDensity",5);

  P.def_function("exponentialDensity", 2, lambda_expression( exponential_density() ) );
  P.def_function("logExponentialDensity", 2, lambda_expression( log_exponential_density() ) );

  P.def_function("builtinGammaDensity", 3, lambda_expression( gamma_density() ) );
  P.def_function("builtinGammaQuantile", 3, lambda_expression( gamma_quantile_op() ) );
  P += "{gammaDensity (a,b) x = builtinGammaDensity a b x}";
  P += "{gammaQuantile (a,b) p = builtinGammaQuantile a b p}";

  P.def_function("builtinLogGammaDensity", 3, lambda_expression( log_gamma_density() ) );
  P += "{logGammaDensity (a,b) x = builtinLogGammaDensity (a,b) x}";

  P.def_function("builtinBetaDensity", 3, lambda_expression( beta_density() ) );
  P.def_function("builtinBetaQuantile", 3, lambda_expression( beta_quantile_op() ) );
  P += "{betaDensity (a,b) x = builtinBetaDensity a b x}";
  P += "{betaQuantile (a,b) p = builtinBetaQuantile a b p}";

  P.def_function("builtinNormalDensity", 3, lambda_expression( normal_density() ) );
  P += "{normalDensity (mu,sigma) x =  builtinNormalDensity mu sigma x}";

  P.def_function("builtinLogNormalDensity", 3, lambda_expression( log_normal_density() ) );
  P.def_function("builtinLogNormalQuantile", 3, lambda_expression( log_normal_quantile_op() ) );
  P += "{logNormalDensity (mu,sigma) x = builtinLogNormalDensity mu sigma x}";
  P += "{logNormalQuantile (mu,sigma) x = builtinLogNormalQuantile mu sigma x}";

  P.def_function("builtinCauchyDensity", 3, lambda_expression( cauchy_density() ) );
  P += "{cauchyDensity (m,s) x = builtinCauchyDensity m s x}";

  P.def_function("builtinLaplaceDensity", 3, lambda_expression( laplace_density() ) );
  P += "{laplaceDensity (m,s) x = builtinLaplaceDensity m s x}";

  P.def_function("builtinDirichletDensity", 3, lambda_expression( dirichlet_density() ) );
  P += "{dirichletDensity ps xs = builtinDirichletDensity (listToVectorDouble ps) (listToVectorDouble xs)}";

  P.def_function("builtinLogLaplaceDensity", 3, lambda_expression( log_laplace_density() ) );
  P += "{logLaplaceDensity (m,s) x = builtinLogLaplaceDensity m s x}";

  P.def_function("builtinUniformDensity", 3, lambda_expression( uniform_density() ) );
  P += "{uniformDensity (min,max) x = builtinUniformDensity min max x}";

  P += "{exponentialQuantile mu p = gammaQuantile (1.0,mu) p}";

  P += "{mixtureDensity ((p1,(ProbDensity _ density1 _ _ _,args1)):l) x = (doubleToLogDouble p1)*(density1 args1 x)+(mixtureDensity l x);\
         mixtureDensity [] _ = (doubleToLogDouble 0.0)}";

  P += "{mixtureDefault ((p1,(ProbDensity _ _ _ d _,args1)):l) = (d args1)}";
  P += "{dirichletDefault l = let {n = length l} in (take n (repeat 1.0/(intToDouble n)))}";
  P += "{iidDefault l = let {n = length l} in (take n (repeat 1.0/(intToDouble n)))}";

  //------------ Define distribution objects --------------------//
  P += "{betaDist  =       (ProbDensity \"Beta\"        betaDensity        betaQuantile (\\args->0.5) 0)}";

  P += "{bernoulliDensity p b = if b then (doubleToLogDouble p) else (doubleToLogDouble (1.0-p))}";
  P += "{bernoulli args = (ProbDensity \"Bernoulli\" bernoulliDensity (error \"Bernoulli has no quantile\") (\\_->True) (), args)}";
  P += "{normal args = (ProbDensity \"Normal\" normalDensity () (\\_->0.0) (\\_->realLine), args)}";
  P += "{exponential args = (ProbDensity \"Exponential\" exponentialDensity exponentialQuantile (\\mu->mu) (\\_->above 0.0), args)}";
  P += "{gamma args = (ProbDensity \"Gamma\" gammaDensity gammaQuantile (\\(a,b)->a*b) (\\_->above 0.0), args)}";
  P += "{betaD args = (ProbDensity \"Beta\"        betaDensity        betaQuantile (\\(a,b)->a/(a+b)) (\\_->between 0.0 1.0), args)}";
  P += "{mixture args = (ProbDensity \"Mixture\" mixtureDensity () mixtureDefault (), args)}";
  P += "{dirichlet args = (ProbDensity \"Dirichlet\" dirichletDensity (error \"Dirichlet has no quantiles\") () (), args)}";
  P += "{laplace args = (ProbDensity \"Laplace\" laplaceDensity () (\\(m,s)->m) (\\_->above 0.0), args)}";
  P += "{logLaplace args = (ProbDensity \"LogLaplace\" logLaplaceDensity () (\\(m,s)->log m) (\\_->above 0.0), args)}";
  P += "{logExponential args = (ProbDensity \"LogExponential\" logExponentialDensity () (\\mu->log mu) (\\_->above 0.0), args)}";
  P += "{logNormal args = (ProbDensity \"LogNormal\" logNormalDensity logNormalQuantile () (\\_->above 0.0), args)}";
  P += "{logGamma args = (ProbDensity \"LogGamma\" logGammaDensity () () (\\_->above 0.0), args)}";
  P += "{uniform args = (ProbDensity \"Uniform\" uniformDensity () () (\\(l,u)->between l u), args)}";
  P += "{cauchy args = (ProbDensity \"Cauchy\" cauchyDensity () () realLine, args)}";
  P += "{distRange (ProbDensity _ _ _ _ r,args) = r args}";

  P += "{iidDensity (n,((ProbDensity _ density _ _ _),args)) xs = let {densities = (map (density args) xs) ; pr = foldl' (*) (doubleToLogDouble 1.0) densities} in if (length xs == n) then pr else (doubleToLogDouble 0.0)}";
  P += "{iid args = (ProbDensity \"i.i.d.\" iidDensity () () (), args )}";

  P += "{\
plateDensity (n,f) xs = let {xs' = zip [1..] xs;\
                             densities = map (\\(i,x) -> case (f i) of {(ProbDensity _ d _ _ _, a) -> d a x}) xs';\
                             pr = foldl' (*) (doubleToLogDouble 1.0) densities}\
                        in if (length xs == n) then pr else (doubleToLogDouble 0.0)}";
  P += "{plate args = (ProbDensity \"Plate\" plateDensity () () (), args )}";

  return P;
}
