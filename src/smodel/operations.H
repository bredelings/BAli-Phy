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

  boost::shared_ptr<ReversibleFrequencyModelObject> Plus_gwF_Function(const alphabet& a, double f, const std::vector<double>& pi);

  struct Plus_gwF_Op: public Operation
  {
    boost::shared_ptr<const alphabet> a;

    Plus_gwF_Op* clone() const {return new Plus_gwF_Op(*this);}

    tribool compare(const Object& O) const
    {
      if (this == &O) 
	return true;
      
      if (typeid(*this) != typeid(O)) return false;

      const Plus_gwF_Op* O2 = dynamic_cast<const Plus_gwF_Op*>(&O);

      if (a != O2->a) return false;
      
      return true;
    }

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const;

    std::string name() const {return "+gwF";}

    Plus_gwF_Op(const alphabet& A):Operation(2),a(A.clone()) { }
  };

  expression_ref Plus_gwF(const alphabet& a);

  boost::shared_ptr<ExchangeModelObject> SimpleExchangeFunction(double rho, int n);

  boost::shared_ptr<ExchangeModelObject> EQU_Exchange_Function(const alphabet& a);

  boost::shared_ptr<AlphabetExchangeModelObject> HKY_Function(const Nucleotides& a, double kappa);

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

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
    {
      boost::shared_ptr<const Nucleotides> N = Args.evaluate_as<Nucleotides>(0);
      double kappa = *Args.evaluate_as<Double>(1);

      return HKY_Function(*N,kappa);
    }

    std::string name() const {return "HKY";}
    
    HKY_Op():Operation(2) { }
  };

  formula_expression_ref HKY_Model(const alphabet& a);
  
  boost::shared_ptr<ExchangeModelObject> TN_Function(const Nucleotides& a, double kappa1, double kappa2);

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

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
    {
      boost::shared_ptr<const Nucleotides> N = Args.evaluate_as<Nucleotides>(0);
      double kappa1 = *Args.evaluate_as<Double>(1);
      double kappa2 = *Args.evaluate_as<Double>(2);

      return TN_Function(*N,kappa1,kappa2);
    }

    std::string name() const {return "TN";}
    
    TN_Op():Operation(3) { }
  };

  formula_expression_ref TN_Model(const alphabet& a);
  
  boost::shared_ptr<AlphabetExchangeModelObject> GTR_Function(const Nucleotides& a, 
							      double AG, double AT, double AC,
							      double GT, double GC, 
							      double TC);

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

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
    {
      boost::shared_ptr<const Nucleotides> N = Args.evaluate_as<Nucleotides>(0);
      double AG = *Args.evaluate_as<Double>(1);
      double AT = *Args.evaluate_as<Double>(2);
      double AC = *Args.evaluate_as<Double>(3);
      double GT = *Args.evaluate_as<Double>(4);
      double GC = *Args.evaluate_as<Double>(5);
      double TC = *Args.evaluate_as<Double>(6);

      return GTR_Function(*N,AG,AT,AC,GT,GC,TC);
    }

    std::string name() const {return "GTR";}
    
    GTR_Op():Operation(7) { }
  };

  formula_expression_ref GTR_Model(const alphabet& a);
  
  boost::shared_ptr<AlphabetExchangeModelObject> INV_Exchange_Function(const alphabet& a,int n);

  boost::shared_ptr<AlphabetExchangeModelObject> INV_for_Mixture_Function(const boost::shared_ptr<const MultiModelObject>& M);

  struct INV_for_Mixture_Op: public Operation
  {
    INV_for_Mixture_Op* clone() const {return new INV_for_Mixture_Op;}
    
    tribool compare(const Object& O) const
    {
      if (this == &O) 
	return true;
      
      if (typeid(*this) != typeid(O)) return false;
      
      return true;
    }

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
    {
      return INV_for_Mixture_Function( Args.evaluate_as<MultiModelObject>(0) );
    }

    std::string name() const {return "INV_for_Mixture";}
    
    INV_for_Mixture_Op():Operation(1) { }
  };

  extern expression_ref INV_for_Mixture;

  formula_expression_ref WithINV_Model(const formula_expression_ref&);

  boost::shared_ptr<AlphabetExchangeModelObject> M0_Function(const Codons& C, const ExchangeModelObject& S2,double omega);

  boost::shared_ptr<AlphabetExchangeModelObject> SingletToTripletExchangeFunction(const Triplets& T, const ExchangeModelObject& S2);

  formula_expression_ref Plus_F_Model(const alphabet& a, const std::valarray<double>& pi);

  formula_expression_ref Plus_F_Model(const alphabet& a);

  formula_expression_ref Plus_gwF_Model(const alphabet& a, const std::valarray<double>& pi);

  formula_expression_ref Plus_gwF_Model(const alphabet& a);

  boost::shared_ptr<ReversibleMarkovModelObject> Q_from_S_and_R_Function(const ExchangeModelObject& S, const ReversibleFrequencyModelObject& F);

  struct Q_from_S_and_R_Op: public Operation
  {
    Q_from_S_and_R_Op* clone() const {return new Q_from_S_and_R_Op(*this);}

    tribool compare(const Object& O) const
    {
      if (this == &O) 
	return true;
      
      if (typeid(*this) != typeid(O)) return false;
      
      return true;
    }

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
    {
      boost::shared_ptr<const ExchangeModelObject> S = Args.evaluate_as<ExchangeModelObject>(0);
      boost::shared_ptr<const ReversibleFrequencyModelObject> F = Args.evaluate_as<ReversibleFrequencyModelObject>(1);
      
      return Q_from_S_and_R_Function(*S, *F);
    }

    std::string name() const {return "Q_from_S_and_R";}

    Q_from_S_and_R_Op():Operation(2) { }
  };

  extern expression_ref Q_from_S_and_R;

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

  boost::shared_ptr<MultiModelObject>
  Unit_Mixture_Function(const boost::shared_ptr<const ReversibleAdditiveObject>& O);

  struct Unit_Mixture_Op: public Operation
  {
    Unit_Mixture_Op* clone() const {return new Unit_Mixture_Op(*this);}

    tribool compare(const Object& O) const
    {
      if (this == &O) 
	return true;
      
      if (typeid(*this) != typeid(O)) return false;
      
      return true;
    }

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
    {
      boost::shared_ptr<const ReversibleAdditiveObject> O = Args.evaluate_as<const ReversibleAdditiveObject>(0);
      
      return Unit_Mixture_Function(O);
    }

    std::string name() const {return "Unit_Mixture";}

    Unit_Mixture_Op():Operation(1) { }
  };

  extern expression_ref Unit_Mixture;

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

  boost::shared_ptr<const MultiModelObject> MixtureModelFunction(const expression_ref& D);

  struct MixtureModelOp: public Operation
  {
    MixtureModelOp* clone() const {return new MixtureModelOp(*this);}

    tribool compare(const Object& O) const
    {
      if (this == &O) 
	return true;
      
      if (typeid(*this) != typeid(O)) return false;
      
      return true;
    }

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const;

    std::string name() const {return "MixtureModel";}

    MixtureModelOp():Operation(1) { }
  };

  extern expression_ref MixtureModel;

  MultiModelObject MultiRateFunction(const MultiModelObject& M_, const expression_ref& D);

  struct MultiRateOp: public Operation
  {
    MultiRateOp* clone() const {return new MultiRateOp(*this);}

    tribool compare(const Object& O) const
    {
      if (this == &O) 
	return true;
      
      if (typeid(*this) != typeid(O)) return false;
      
      return true;
    }

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
    {
      // The input-model should really be a lambda function taking the single value (or first value) p_change
      boost::shared_ptr<const MultiModelObject> M = Args.evaluate_as<MultiModelObject>(0);
      expression_ref D = Args.evaluate(1);
      
      boost::shared_ptr< MultiModelObject > R ( MultiRateFunction(*M, *D).clone() );

      return R;
    }

    std::string name() const {return "MultiRate";}

    MultiRateOp():Operation(2) { }
  };

  extern expression_ref MultiRate;

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

  boost::shared_ptr<ReversibleMarkovModelObject> Modulated_Markov_Function(const ExchangeModelObject& S,MultiModelObject M);

  struct Modulated_Markov_Op: public Operation
  {
    Modulated_Markov_Op* clone() const {return new Modulated_Markov_Op(*this);}

    tribool compare(const Object& O) const
    {
      if (this == &O) 
	return true;
      
      if (typeid(*this) != typeid(O)) return false;
      
      return true;
    }

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
    {
      boost::shared_ptr<const MultiModelObject> M = Args.evaluate_as<MultiModelObject>(0);
      boost::shared_ptr<const ExchangeModelObject> S = Args.evaluate_as<ExchangeModelObject>(1);

      return Modulated_Markov_Function(*S, *M);
    }

    std::string name() const {return "Modulated_Markov";}

    Modulated_Markov_Op():Operation(2) { }
  };

  extern expression_ref Modulated_Markov_E;

  // Mixture_Function [Double] [MultiModelObject] -> MultiModelObject
  boost::shared_ptr<const MultiModelObject> Mixture_Function(const expression_ref&, const expression_ref&);

  struct Mixture_Op: public Operation
  {
    Mixture_Op* clone() const {return new Mixture_Op(*this);}

    tribool compare(const Object& O) const
    {
      if (this == &O) 
	return true;
      
      if (typeid(*this) != typeid(O)) return false;
      
      return true;
    }

    boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
    {
      expression_ref DL = Args.evaluate(0);
      expression_ref ML = Args.evaluate(1);

      return Mixture_Function(DL, ML);
    }

    std::string name() const {return "Mixture";}

    Mixture_Op(): Operation(2) { }
  };

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

}

#endif
