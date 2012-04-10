#ifndef SMODEL_OPERATIONS_H
#define SMODEL_OPERATIONS_H

#include <vector>
#include <valarray>
#include "alphabet.H"
#include "smodel/objects.H"
#include "object.H"
#include "probability.H"
#include "distribution.H"
#include "model.H"
#include "computation/formula_expression.H"
#include "computation/expression.H"
#include "computation/operation.H"
#include "computation/computation.H"

namespace substitution {

  struct Plus_gwF_Op: public Operation
  {
    Plus_gwF_Op* clone() const {return new Plus_gwF_Op(*this);}

    tribool compare(const Object& O) const
    {
      if (this == &O) 
	return true;
      
      if (typeid(*this) != typeid(O)) return false;
      
      return true;
    }

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const;

    std::string name() const {return "+gwF";}

    Plus_gwF_Op():Operation(3) { }
  };

  extern const expression_ref Plus_gwF;

  struct HKY_Op: public Operation
  {
    HKY_Op* clone() const {return new HKY_Op;}
    
    tribool compare(const Object& O) const
    {
      if (this == &O) 
	return true;
      
      if (typeid(*this) != typeid(O)) return false;
      
      return true;
    }

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const;

    std::string name() const {return "HKY";}
    
    HKY_Op():Operation(2) { }
  };

  formula_expression_ref HKY_Model(const alphabet& a);
  
  struct TN_Op: public Operation
  {
    TN_Op* clone() const {return new TN_Op;}
    
    tribool compare(const Object& O) const
    {
      if (this == &O) 
	return true;
      
      if (typeid(*this) != typeid(O)) return false;
      
      return true;
    }

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const;

    std::string name() const {return "TN";}
    
    TN_Op():Operation(3) { }
  };

  formula_expression_ref TN_Model(const alphabet& a);
  
  struct GTR_Op: public Operation
  {
    GTR_Op* clone() const {return new GTR_Op;}
    
    tribool compare(const Object& O) const
    {
      if (this == &O) 
	return true;
      
      if (typeid(*this) != typeid(O)) return false;
      
      return true;
    }

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const;

    std::string name() const {return "GTR";}
    
    GTR_Op():Operation(7) { }
  };

  formula_expression_ref GTR_Model(const alphabet& a);
  
  boost::shared_ptr<AlphabetExchangeModelObject> M0_Function(const Codons& C, const ExchangeModelObject& S2,double omega);

  boost::shared_ptr<AlphabetExchangeModelObject> SingletToTripletExchangeFunction(const Triplets& T, const ExchangeModelObject& S2);

  formula_expression_ref Plus_F_Model(const alphabet& a, const std::valarray<double>& pi);

  formula_expression_ref Plus_F_Model(const alphabet& a);

  formula_expression_ref Plus_gwF_Model(const alphabet& a, const std::valarray<double>& pi);

  formula_expression_ref Plus_gwF_Model(const alphabet& a);

  struct Q_Op: public Operation
  {
    Q_Op* clone() const {return new Q_Op(*this);}

    tribool compare(const Object& O) const
    {
      if (this == &O) 
	return true;
      
      if (typeid(*this) != typeid(O)) return false;
      
      return true;
    }

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const;

    std::string name() const {return "Q";}

    Q_Op():Operation(2) { }
  };

  extern const expression_ref Q;

  struct M0_Op: public Operation
  {
    M0_Op* clone() const {return new M0_Op(*this);}

    tribool compare(const Object& O) const
    {
      if (this == &O) 
	return true;
      
      if (typeid(*this) != typeid(O)) return false;
      
      return true;
    }

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
    {
      boost::shared_ptr<const Codons> C = Args.evaluate_as<Codons>(0);
      boost::shared_ptr<const ExchangeModelObject> S = Args.evaluate_as<ExchangeModelObject>(1);
      boost::shared_ptr<const Double> omega = Args.evaluate_as<Double>(2);

      return M0_Function(*C, *S, *omega);
    }

    std::string name() const {return "M0";}

    M0_Op():Operation(3) { }
  };

  extern expression_ref M0E;

  formula_expression_ref Reversible_Markov_Model(const formula_expression_ref& FR, const formula_expression_ref& FS);

  formula_expression_ref Simple_gwF_Model(const formula_expression_ref& FR, const alphabet& a);

  formula_expression_ref Simple_gwF_Model(const formula_expression_ref& FR, const alphabet& a, const std::valarray<double>& pi);

  formula_expression_ref Unit_Model(const formula_expression_ref&);

  expression_ref DiscretizationFunction(const probability::Distribution& D, Int n);

  struct DiscretizationOp: public Operation
  {
    DiscretizationOp* clone() const {return new DiscretizationOp(*this);}

    tribool compare(const Object& O) const
    {
      if (this == &O) 
	return true;
      
      if (typeid(*this) != typeid(O)) return false;
      
      return true;
    }

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
    {
      boost::shared_ptr<const probability::Distribution> D = Args.evaluate_as<probability::Distribution>(0);
      boost::shared_ptr<const Int> n = Args.evaluate_as<Int>(1);
      
      return DiscretizationFunction(*D, *n);
    }

    std::string name() const {return "DiscretizedDistribution";}

    DiscretizationOp():Operation(2) { }
  };

  extern expression_ref Discretize;

  // We want Q(mi -> mj) = Q[m](i -> j)   for letter exchange
  //         Q(mi -> ni) = R(m->n)        for model exchange
  // and     Q(mi -> nj) = 0              for all other pairs

  // We assume that R(m->n) = S(m,n) * M->distribution()[n]

  // This should result in a Markov chain where the frequencies are
  //  frequencies()[mi] = pi[i] * f[m] 
  // with pi = M->frequencies() 
  // and   f = M->distribution()

  // PROBLEM: I don't have a good way of defining the switching rate.
  // Right now, I have S(m,n) = rho, S(m,m) = 0
  // But, the S(m,n) do not correspond to switching rates exactly.
  // Instead, the switching rate is now rho*f[n], which is going to
  // be something like rho*(n-1)/n if there are n categories.
  
  // ADDITIONALLY, depending on how fine-grained the categories are,
  // a switching rate has a different interpretation.

  // HOWEVER, I think the current approach works for now, because it
  // approximates the model that at rate 'rho' the rate is randomly
  // re-drawn from the underlying distribution.  A lot of the time it
  // will fall in the same bin, giving a lower observed switching rate
  // when the discrete approximation to the continuous distribution has
  // low resolution.

  extern expression_ref Modulated_Markov_E;

  extern expression_ref Mixture_E;

  formula_expression_ref Mixture_Model(const std::vector<formula_expression_ref>& models);

  boost::shared_ptr<const EigenValues> Get_Eigensystem_Function(const Matrix& Q, const std::vector<double>& pi);

  // Matrix exp(const EigenValues& eigensystem,const std::vector<double>& D,double t);x
  struct Get_Eigensystem_Op: public Operation
  {
    Get_Eigensystem_Op* clone() const {return new Get_Eigensystem_Op(*this);}

    tribool compare(const Object& O) const
    {
      if (this == &O) 
	return true;
      
      if (typeid(*this) != typeid(O)) return false;
      
      return true;
    }

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const;
 
    std::string name() const {return "Get_Eigensystem";}

    Get_Eigensystem_Op(): Operation(2) { }
  };

  extern expression_ref Get_Eigensystem;

  boost::shared_ptr<const Double> 
  Get_Equilibrium_Rate_Function(const alphabet&, const std::vector<unsigned>&, const Matrix&, const expression_ref&);

  // Matrix exp(const EigenValues& eigensystem,const std::vector<double>& D,double t);x
  struct Get_Equilibrium_Rate_Op: public Operation
  {
    Get_Equilibrium_Rate_Op* clone() const {return new Get_Equilibrium_Rate_Op(*this);}

    tribool compare(const Object& O) const
    {
      if (this == &O) 
	return true;
      
      if (typeid(*this) != typeid(O)) return false;
      
      return true;
    }

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const;
 
    std::string name() const {return "Get_Eigensystem";}

    Get_Equilibrium_Rate_Op(): Operation(4) { }
  };

  extern expression_ref Get_Equilibrium_Rate;

}

#endif
