/*
   Copyright (C) 2004-2010 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

/* TODO
   0. Can we have a triplet model that isn't derived from M0*Neutral?

   1. The WithINV models currently have INV frequencies = variant frequencies.
    a) If we had *2* submodels, then INV and the other models could have
       DIFFERENT frequencies.

   2. If we want to do M0 + M7 + AAINV, then 
      a) how do we make the AAINV model share the same alphabet model?
      b) could we make it share the same TripletFrequencies but
         use a different CodonFrequencies, and so have different AA 
         frequencies?
      c) can we use '+' to represent mixtures, instead of stacking?
         M0~[w=0 + w~Beta( ) + w=1]
         WAG~[rate=0 + rate~Gamma( )]  vs WAG~[INV + gamma] + WAG~[gamma+INV]
      d) It looks like we need to have a special class to specify a PRIOR on the mixture
         probabilities, though.
      e) How about M0~[w=0] instead of Conserved?
 
   3. Compare the marginal probability of CodonFrequencyModels on bglobin
      (fixed A) at different settings:
      a) f = 0.5 / nuX = 1/n (g=1) / c=0 (b_X=1/n, h=0) -> HKY (should match)
      b) fX=1/n (f=1) / no constraint / c=0 (b_X=1/n, h=0) -> M0 (should match)
      c) fX=1/n (f=1) / nX = 1/n (g=1) / c=1 -> AA flat model
      d) fX=1/n (f=1) / no constraint /  c=1 -> AA flat model w/ codon bias
      e) no constraint / no constraint / c=1 -> AA flat model w/ codon bias + nuc. freqs.
      f) no constraint / no constraint / no constraint

   4. What if we want to do {M0 + M7 + AAINV + Neutral} ?
      We should perhaps consider mixtures of exchange models.
      Would this handle gamma?

   5. We could handle exchange models and frequencies separately:
      {WAG ~ [gamma+INV]}*{multifreq}

   6. Could we do multi-codonfrequency models? (e.g. decide to share the nuc. freq.
      model, but have different aa freqs)

   7. Is there a more natural separation of this file?
      NOTE: lots of this use {get,set}_varray( ), etc.
      Maybe just separate the headers.

   8. In the future, specifying priors and proposals could hopefully be separated
      from models [or, at least, model structure]

   9. In order to construct models -- like the branch-site model -- with different
      Q matrices on different branches, we need to be able to generate transition
      matrix collections of the form transition_P(m,b).  

*/

#ifndef SMODEL_H
#define SMODEL_H

#include "smodel/objects.H"
#include <valarray>
#include <vector>
#include <string>
#include <boost/shared_ptr.hpp>
#include <boost/numeric/ublas/symmetric.hpp>
#include "mytypes.H"
#include "alphabet.H"
#include "rates.H"
#include "model.H"
#include "eigenvalue.H"
#include "cached_value.H"

namespace substitution {

  using std::string;
  using std::valarray;

  //--------------------- class AlphabetModel ----------------------//

  /// A generic model of the process of substitution
  class Model: virtual public ::Model
  {
  public:
    /// Make a copy of this object
    virtual Model* clone() const =0;

    virtual ~Model() {}
  };


  //----------------------- Nested Models ------------------------///

  // This models a substitution process in which evolution is independent between
  // non-overlapping intervals.  We should have that transition_p(t/2)^2 = transition_p(t)
  // This model must also be reversible

  class ReversibleModel: virtual public Model 
  {
  public:
    /// Make a copy of this object
    virtual ReversibleModel* clone() const =0;

    ReversibleModel() {}
  };

  /// An AdditiveModel depends only on t2-t1

  /// In additive models P(X[t+tau]=b|X[t]=a) = P(X[tau]=b|X[0]=a)
  /// Also E f(X,(a,b)) = E f(X,(0,b-a))

  class AdditiveModel: virtual public Model 
  {
  public:
    virtual AdditiveModel* clone() const=0;

    AdditiveModel() {}
  };

  class ReversibleAdditiveModel: virtual public ReversibleModel, virtual public AdditiveModel 
  {
  public:
    virtual ReversibleAdditiveModel* clone() const=0;
  };

  // With an Independant Model, you can chain transition matrices.
  // Q_t(a,b) = sum_c Q[0,tau](a,c) x Q[tau,t](c,b)

  // In an independent additive
  class IndependantAdditiveModel: virtual public AdditiveModel 
  {
  public:
    virtual IndependantAdditiveModel* clone() const=0;

    IndependantAdditiveModel() {}
  };

  class Independant_Additive_Reversible_Model: public ReversibleAdditiveModel, 
					       virtual public IndependantAdditiveModel
  {
  public:
    virtual Independant_Additive_Reversible_Model* clone() const=0;
  };


  class ExchangeModel: virtual public ::Model
  {
  public:
    
    ExchangeModel* clone() const =0;
    
    virtual ~ExchangeModel() {};
  };

  class SimpleExchangeModel: public ExchangeModel
  {
  public:

    SimpleExchangeModel* clone() const {return new SimpleExchangeModel(*this);}

    /// Get the parameter 'rho' (model switching rate)
    double rho() const {return get_parameter_value_as<Double>(0);}
    /// Set the parameter 'rho' (model switching rate)
    void   rho(double r) {set_parameter_value(0,r);}

    virtual efloat_t prior() const;

    string name() const;

    boost::shared_ptr<const Object> result() const;

    SimpleExchangeModel(unsigned n);

    ~SimpleExchangeModel() {}
  };


  class AlphabetExchangeModel: public ExchangeModel
  {
  public:
    AlphabetExchangeModel* clone() const =0;
    
    boost::shared_ptr<const alphabet> get_alphabet() const 
    {
      return result_as<AlphabetExchangeModelObject>()->get_alphabet();
    }

    virtual ~AlphabetExchangeModel() {};
  };

  
  class INV_Model: public AlphabetExchangeModel
  {
  public:
    INV_Model* clone() const {return new INV_Model(*this);}

    boost::shared_ptr<const Object> result() const;

    string name() const;
      
    INV_Model(const alphabet& a);
  };

  //--------------------- EQU Substitution Model ------------------------/

  /// The EQU model for any alphabet - all letters are treated the same
  /// This model has no parameters
  class EQU: public AlphabetExchangeModel
  {
  public:

    virtual EQU* clone() const {return new EQU(*this);}

    boost::shared_ptr<const Object> result() const;

    string name() const;

    EQU(const alphabet& a);

    virtual ~EQU() {}
  };

  //------------------ Empirical Substitution Model ---------------------//

  /// A generic model for any alphabet - the exchangeability matrix is loaded from a file
  class Empirical: public AlphabetExchangeModel
  {
    /// Specificy this model based on the matrix that was loaded
    string name_;

  public:
    /// Load the exhangeability matrix from file 'filename'
    void load_file(std::istream&);

    /// Load the exhangeability matrix from file 'filename'
    void load_file(const string& filename);

    virtual Empirical* clone() const {return new Empirical(*this);}

    string name() const {return name_;}

    boost::shared_ptr<const Object> result() const;

    Empirical(const alphabet& a); 

    Empirical(const alphabet& a, const string& n); 

    ~Empirical() {}
  };

  class PAM: public Empirical
  {
  public:
    virtual PAM* clone() const {return new PAM(*this);}

    PAM();
  };

  class JTT: public Empirical
  {
  public:
    virtual JTT* clone() const {return new JTT(*this);}

    JTT();
  };

  class WAG: public Empirical
  {
  public:
    virtual WAG* clone() const {return new WAG(*this);}

    WAG();
  };

  class LG: public Empirical
  {
  public:
    virtual LG* clone() const {return new LG(*this);}

    LG();
  };

  class NucleotideExchangeModel: public AlphabetExchangeModel
  {
  public:
    NucleotideExchangeModel* clone() const=0;
  };

  /// The Hasegawa, Kishino, Yano (1985)
  class HKY: public NucleotideExchangeModel
  {
  public:

    virtual HKY* clone() const {return new HKY(*this);}

    /// Get the parameter 'kappa' (transition/transversion ratio)
    double kappa() const {return get_parameter_value_as<Double>(1);}
    /// Set the parameter 'kappa' (transition/transversion ratio)
    void kappa(double k) {set_parameter_value(1,k);}

    virtual efloat_t prior() const;

    string name() const;

    boost::shared_ptr<const Object> result() const;

    /// Construct an HKY model on alphabet 'a'
    HKY(const Nucleotides& N);
  };

  /// The Tamura-Nei (1993) model
  class TN: public NucleotideExchangeModel 
  {
  public:

    virtual TN* clone() const {return new TN(*this);}

    /// Get the parameter 'kappa' (transition/transversion ratio)
    double kappa1() const {return get_parameter_value_as<Double>(1);}
    /// Get the parameter 'kappa' (transition/transversion ratio)
    double kappa2() const {return get_parameter_value_as<Double>(2);}

    /// Set the parameter 'kappa' (transition/transversion ratio)
    void kappa1(double k) {set_parameter_value(1,k);}
    /// Set the parameter 'kappa' (transition/transversion ratio)
    void kappa2(double k) {set_parameter_value(2,k);}

    virtual efloat_t prior() const;

    string name() const;

    boost::shared_ptr<const Object> result() const;

    /// Construct an HKY model on alphabet 'a'
    TN(const Nucleotides& N);
  };

  /// The General Time Reversible model
  class GTR: public NucleotideExchangeModel 
  {
  public:
    
    virtual GTR* clone() const {return new GTR(*this);}

    virtual efloat_t prior() const;

    string name() const;

    boost::shared_ptr<const Object> result() const;

    /// Construct a GTR model on alphabet 'a'
    GTR(const Nucleotides& N);
  };
    

  class TripletExchangeModel: public AlphabetExchangeModel
  {
  public:
    TripletExchangeModel* clone() const=0;

    TripletExchangeModel(const Triplets& T);

    ~TripletExchangeModel() {}
  };

  class SingletToTripletExchangeModel: public TripletExchangeModel,
				       public SuperModelOver<NucleotideExchangeModel>
  {
  public:

    efloat_t prior() const { return SuperModelOver<NucleotideExchangeModel>::prior(); }

    SingletToTripletExchangeModel* clone() const {return new SingletToTripletExchangeModel(*this);}

    string name() const;

    boost::shared_ptr<const Object> result() const;

    SingletToTripletExchangeModel(const Triplets& T,const NucleotideExchangeModel& M);

    ~SingletToTripletExchangeModel() {}
  };



  class M0: public AlphabetExchangeModel, public OpModel
  {
    int omega_index;
  public:
    efloat_t super_prior() const;

    M0* clone() const {return new M0(*this);}

    /// Get the parameter 'omega' (non-synonymous/synonymous rate ratio)
    double omega() const;
    /// Set the parameter 'omega' (non-synonymous/synonymous rate ratio)
    void omega(double w);

    M0(const Codons& C,const NucleotideExchangeModel& M);

    ~M0();
  };

  //------------------------ Reversible frequency model --------------------//

  class ReversibleFrequencyModel: virtual public ::Model
  {
  public:
    ReversibleFrequencyModel* clone() const =0;

    ReversibleFrequencyModel(const alphabet& a);

    virtual ~ReversibleFrequencyModel() {}
  };



  class SimpleFrequencyModel: public ReversibleFrequencyModel
  {
  protected:
    void recalc(const std::vector<int>&) {std::abort();}
    void update() { }
  public:

    boost::shared_ptr<const Object> result() const;

    SimpleFrequencyModel* clone() const {return new SimpleFrequencyModel(*this);}

    efloat_t prior() const;

    string name() const;

    explicit SimpleFrequencyModel(const alphabet& a);

    explicit SimpleFrequencyModel(const alphabet& a,const valarray<double>&);
  };

  class UniformFrequencyModel: public ReversibleFrequencyModel
  {
  protected:
    void recalc(const std::vector<int>&) {std::abort();}

  public:
    
    boost::shared_ptr<const Object> result() const;

    UniformFrequencyModel* clone() const {return new UniformFrequencyModel(*this);}

    string name() const;

    explicit UniformFrequencyModel(const alphabet& a);

    explicit UniformFrequencyModel(const alphabet& a,const valarray<double>&);
  };

  class TripletFrequencyModel: public ReversibleFrequencyModel
  {
  public:
    TripletFrequencyModel* clone() const =0;

    explicit TripletFrequencyModel(const Triplets& T);
  };

  class IndependentNucleotideFrequencyModel: public TripletFrequencyModel,
					     public SuperModelOver<SimpleFrequencyModel>
  {
    void recalc(const std::vector<int>&) {std::abort();}

  public:
    boost::shared_ptr<const Object> result() const;

    IndependentNucleotideFrequencyModel* clone() const {return new IndependentNucleotideFrequencyModel(*this);}

    string name() const;

    IndependentNucleotideFrequencyModel(const Triplets& T);
  };


  class TripletsFrequencyModel: public TripletFrequencyModel,
				public SuperModelOver<SimpleFrequencyModel>
  {
  protected:
    void recalc(const std::vector<int>&) {std::abort();}

  public:
    boost::shared_ptr<const Object> result() const;

    TripletsFrequencyModel* clone() const {return new TripletsFrequencyModel(*this);}

    efloat_t super_prior() const;

    string name() const;

    explicit TripletsFrequencyModel(const Triplets& T);
  };

  class CodonFrequencyModel: public ReversibleFrequencyModel
  {
  public:
    CodonFrequencyModel* clone() const =0;

    explicit CodonFrequencyModel(const Codons& C);
  };

  class AACodonFrequencyModel: public CodonFrequencyModel,
			       public SuperModelOver<SimpleFrequencyModel>
  {
  protected:
    void recalc(const std::vector<int>&) {std::abort();}
  public:
    boost::shared_ptr<const Object> result() const;

    AACodonFrequencyModel* clone() const {return new AACodonFrequencyModel(*this);}

    string name() const;

    explicit AACodonFrequencyModel(const Codons& C);
  };

  class CodonsFrequencyModel: public CodonFrequencyModel,
			      public SuperModelOver<TripletsFrequencyModel>
  {
  protected:
    void recalc(const std::vector<int>&) {std::abort();}

  public:
    boost::shared_ptr<const Object> result() const;

    CodonsFrequencyModel* clone() const {return new CodonsFrequencyModel(*this);}

    efloat_t super_prior() const;

    string name() const;

    explicit CodonsFrequencyModel(const Codons& C);
  };

  
  class CodonsFrequencyModel2: public CodonFrequencyModel,
			      public SuperModelOver<TripletsFrequencyModel>
  {
  protected:
    void recalc(const std::vector<int>&) {std::abort();}

  public:
    boost::shared_ptr<const Object> result() const;

    CodonsFrequencyModel2* clone() const {return new CodonsFrequencyModel2(*this);}

    efloat_t super_prior() const;

    string name() const;

    explicit CodonsFrequencyModel2(const Codons& C);
  };

  

  //------------------------- Reversible Substitution Model -----------------------//

  // Store S2 = pi^0.5 * S * pi^0.5
  // Store EigenValues(S2) -> D,O
  // Is this worth it?  Try it later...

  // exp(Qt) ->
  //   1. get Dt = D*exp(t)
  //   2. get O*Dt*O
  //   3. return DN*(O*Dt*O)*DP

  /// A Markov substitution model which is reversible
  class ReversibleMarkovModel: public Independant_Additive_Reversible_Model 
  {
  public:

    /// Make a copy of this object
    virtual ReversibleMarkovModel* clone() const =0;

    boost::shared_ptr<const alphabet> get_alphabet() const 
    {
      return result_as<ReversibleMarkovModelObject>()->get_alphabet();
    }

    ~ReversibleMarkovModel() {}
  };

  /// A Felsenstein-81 model, in which Q_ij = a*pi[j]
  class F81_Model: public ReversibleMarkovModel
  {
    void recalc(const std::vector<int>&) {std::abort();}
  public:
    /// Make a copy of this object
    virtual F81_Model* clone() const {return new F81_Model(*this);}

    virtual efloat_t prior() const;

    string name() const;

    boost::shared_ptr<const Object> result() const;

    F81_Model(const alphabet& a);
    F81_Model(const alphabet& a,const valarray<double>&);
    ~F81_Model() {}
  };


  class ReversibleMarkovSuperModel: public ReversibleMarkovModel,
				    public OpModel
  {
  public:
    /// Make a copy of this object
    virtual ReversibleMarkovSuperModel* clone() const {return new ReversibleMarkovSuperModel(*this);}

    /// Construct a reversible Markov model on alphabet 'a'
    ReversibleMarkovSuperModel(const ::Model& S1, const ::Model& R1);
    
    ~ReversibleMarkovSuperModel() {}
  };


  class SimpleReversibleMarkovModel: public ReversibleMarkovModel,
				     public OpModel
  {
  public:
    /// Make a copy of this object
    virtual SimpleReversibleMarkovModel* clone() const {return new SimpleReversibleMarkovModel(*this);}

    explicit SimpleReversibleMarkovModel(const AlphabetExchangeModel& E);
    explicit SimpleReversibleMarkovModel(const AlphabetExchangeModel& E,const valarray<double>&);
  };

  //--------------------------- Implement the underlying alphabet -------------------------//


  template <class T>
  class ReversibleWrapperOver: virtual public ReversibleModel,
			       public SuperModelOver< ::Model>
  {
  public:
    T& SubModel() {return SubModelAs<T>(0); }

    const T& SubModel() const { return SubModelAs<T>(0); }

    virtual ReversibleWrapperOver* clone() const=0;

    ReversibleWrapperOver(const T& M) {
      insert_submodel("BASE",M);
    }
  };

  /// Class for a collection of models that could be used on different branches of the tree.
  class ReversibleAdditiveCollection: virtual public Model, 
				      virtual public AdditiveModel
  {
  public:
    boost::shared_ptr<const alphabet> get_alphabet() const 
    {
      return result_as<ReversibleAdditiveCollectionObject>()->get_alphabet();
    }

    virtual ReversibleAdditiveCollection* clone() const=0;

    virtual ~ReversibleAdditiveCollection() {}
  };

  class BranchSiteCollection: public ReversibleAdditiveCollection, public SuperModel
  {
    std::vector< owned_ptr<AlphabetExchangeModel> > S;

    owned_ptr<ReversibleFrequencyModel> R;

    std::vector<owned_ptr<ReversibleMarkovSuperModel> > P;

    boost::shared_ptr<const Object> result() const;

    int n_submodels() const;

    const ::Model& SubModels(int) const ;

          ::Model& SubModels(int);

  public:
    BranchSiteCollection* clone() const {return new BranchSiteCollection(*this);}

    std::string name() const;

    BranchSiteCollection(const std::vector<AlphabetExchangeModel>& S1, const ReversibleFrequencyModel& R1);
  };

  /// A multi-branch model with only 1 part.
  class SimpleReversibleAdditiveCollection: public ReversibleAdditiveCollection,
					    public SuperModelOver<ReversibleAdditiveModel>
  {
  public:
    virtual SimpleReversibleAdditiveCollection* clone() const {return new SimpleReversibleAdditiveCollection(*this);}

          ReversibleAdditiveModel& SubModel()       {return SubModels(0); }

    const ReversibleAdditiveModel& SubModel() const {return SubModels(0); }

    boost::shared_ptr<const Object> result() const;

    void update() { }

    std::string name() const;
    
    SimpleReversibleAdditiveCollection(const ReversibleAdditiveModel& t);

    virtual ~SimpleReversibleAdditiveCollection() {}
  };

  //----------------------- MultiModel ---------------------------*/

  class MultiModel: public ReversibleAdditiveModel
  {

  public:
    virtual MultiModel* clone() const=0;

    ~MultiModel() {}
  };

  /// A MultiRate Model with a single rate
  class UnitModel: public MultiModel,
		   public SuperModelOver< ReversibleAdditiveCollection >
  {
    void recalc(const std::vector<int>&) {std::abort();}
  public:
    UnitModel* clone() const {return new UnitModel(*this);}

          ReversibleAdditiveCollection& SubModel() {return SuperModelOver<ReversibleAdditiveCollection>::SubModels(0); }

    const ReversibleAdditiveCollection& SubModel() const {return SuperModelOver<ReversibleAdditiveCollection>::SubModels(0); }

    string name() const;

    void update() { }

    boost::shared_ptr<const Object> result() const;

    UnitModel(const ReversibleMarkovModel& M);

    UnitModel(const ReversibleAdditiveCollection& M);

    ~UnitModel() {};
  };


  /// A substitution model based on running identical copies of an input model with different frequencies.
  class MultiFrequencyModel: public MultiModel, 
			     public ReversibleWrapperOver<SimpleReversibleMarkovModel> 
  {
  protected:

    void recalc(const std::vector<int>&) {std::abort();}


  public:
    efloat_t super_prior() const;

    MultiFrequencyModel* clone() const {return new MultiFrequencyModel(*this);}

    boost::shared_ptr<const Object> result() const;

    string name() const;

    MultiFrequencyModel(const AlphabetExchangeModel& M,int n_bins);

    ~MultiFrequencyModel() {}
  };


  /// A substitution model based on running identical copies of an input model with different frequencies.
  class CAT_FixedFrequencyModel: public MultiModel
  {
  protected:
    /// Fraction of the sites with each parameter value, according to model estimate
    std::vector<double> prior_fraction;

    string name_;

    void recalc(const std::vector<int>&) {std::abort();}

    void update() { }
  public:
    const alphabet& Alphabet() const;

    /// Load the site frequencies from \a file
    void load_file(std::istream& file);

    /// Load the site frequencies from \a filename
    void load_file(const string& filename);

    // no parameters: super_prior() == 1

    CAT_FixedFrequencyModel* clone() const {return new CAT_FixedFrequencyModel(*this);}

    efloat_t prior() const;

    string name() const {return name_;}

    boost::shared_ptr<const Object> result() const;

    /// Create a CAT_FixedFrequencyModel with alphabet \a a.
    CAT_FixedFrequencyModel(const alphabet& a);

    /// Create a CAT_FixedFrequencyModel with alphabet \a a and name \a n.
    CAT_FixedFrequencyModel(const alphabet& a, const string& n);

    ~CAT_FixedFrequencyModel() {}
  };


  class C10_CAT_FixedFrequencyModel: public CAT_FixedFrequencyModel
  {
  public:
    C10_CAT_FixedFrequencyModel* clone() const {return new C10_CAT_FixedFrequencyModel(*this);}

    C10_CAT_FixedFrequencyModel();
  };

  class C20_CAT_FixedFrequencyModel: public CAT_FixedFrequencyModel
  {
  public:
    C20_CAT_FixedFrequencyModel* clone() const {return new C20_CAT_FixedFrequencyModel(*this);}

    C20_CAT_FixedFrequencyModel();
  };

  /// A mixture of variants of model M with parameter p set to n_bin different values
  class MultiParameterModel: public MultiModel, 
			     public ReversibleWrapperOver<MultiModel> 
  {
  protected:
    /// The parameter which is varying;
    int p_change;

    /// The values it takes on
    mutable DiscreteDistribution D;

  public:
    boost::shared_ptr<const Object> result() const;

    MultiParameterModel* clone() const=0;

    /// Get the equilibrium frequencies
    valarray<double> frequencies() const;

    /// Construct a mixture of variants of model M with parameter p set to n_bin different values
    MultiParameterModel(const MultiModel& M,int p,int n_bins);

    ~MultiParameterModel() {}
  };


  /// A class of uncoupled rates/values
  class DirichletParameterModel: public MultiModel,
				 public ReversibleWrapperOver<MultiModel> 
  {
  protected:
    /// The parameter which is varying;
    int p_change;

    /// The number of bins
    int n_bins;

    efloat_t super_prior() const;

    void recalc(const std::vector<int>&) {std::abort();}

    void update() { }

  public:
    boost::shared_ptr<const Object> result() const;

    DirichletParameterModel* clone() const {return new DirichletParameterModel(*this);}

    string name() const;

    DirichletParameterModel(const MultiModel& M,int p,int n);
  };


  class DistributionParameterModel: public MultiModel, public OpModel
  {
  public:
    DistributionParameterModel* clone() const {return new DistributionParameterModel(*this);}

    DistributionParameterModel(const MultiModel& M, const Distribution& RD,int p,int n);
  };

  /// A discrete-gamma Model
  //  Only has one parameter: the shape parameter for the gamma
  class GammaParameterModel: public MultiModel, public OpModel
  {
  public:
    GammaParameterModel* clone() const {return new GammaParameterModel(*this);}

    /// Construct a discrete gamma model on top of 'M', with 'n' bins
    GammaParameterModel(const MultiModel& M, int n);
  };

  /// A discrete-lognormal Model
  //  Only has one parameter: the shape parameter for the gamma
  class LogNormalParameterModel: public MultiModel, public OpModel
  {
  public:
    LogNormalParameterModel* clone() const {return new LogNormalParameterModel(*this);}

    /// Construct a discrete gamma model on top of 'M', with 'n' bins
    LogNormalParameterModel(const MultiModel& M, int n);
  };



  /// The Invariant-sites model: a certain fraction of the sites don't change

  /// The Invariant-sites model: a certain fraction of the sites don't change
  /// The only parameter is 'p', the fraction of sites which don't change
  /// To keep branch lengths the same, inflate rates by 1/(1-p).
  class WithINV: public MultiModel, public ReversibleWrapperOver<MultiModel> 
  {
    static const double inv_frac_mean;
    static const double max_inv_rate;

    int p_index;

  public:
    virtual WithINV* clone() const {return new WithINV(*this);}

    void update() { }

    void recalc(const std::vector<int>&) {std::abort();}

    boost::shared_ptr<const Object> result() const;

    efloat_t super_prior() const;

    string name() const;

    WithINV(const MultiModel& M);
  };

  class M2: public MultiParameterModel
  {
    void recalc(const std::vector<int>&) {std::abort();}
    void update() { }

  public:
    boost::shared_ptr<const Object> result() const;

    efloat_t super_prior() const;

    M2* clone() const {return new M2(*this);}

    string name() const;

    M2(const M0& M1,const ReversibleFrequencyModel& R);
  };

  /// M2a model has three omega parameters.
  /// In M2 model, omega_1 = 0. In M2a model,
  /// omega_1 < 1.
  class M2a: public MultiModel, 
	     public ReversibleWrapperOver<MultiModel> 
  {
    /// The parameter which is varying;
    int p_change;

    /// The values it takes on
    mutable DiscreteDistribution D;

    void recalc(const std::vector<int>&) {std::abort();}
    void update() { }

  public:
    boost::shared_ptr<const Object> result() const;

    efloat_t super_prior() const;

    M2a* clone() const {return new M2a(*this);}

    string name() const;

    M2a(const M0& M1,const ReversibleFrequencyModel& R);
  };

  class M3: public MultiParameterModel 
  {
    void recalc(const std::vector<int>&) {std::abort();}
    void update() { }
  public:
    boost::shared_ptr<const Object> result() const;
    
    efloat_t super_prior() const;

    M3* clone() const {return new M3(*this);}

    /// Get the parameter 'omega[i]' (non-synonymous/synonymous rate ratio)
    double omega(int i) const;
    /// Set the parameter 'omega[i]' (non-synonymous/synonymous rate ratio)
    void omega(int i,double w);


    string name() const;

    M3(const M0& M1,const ReversibleFrequencyModel& R, int n);
  };


  class M7: public DistributionParameterModel {
  public:
    
    M7* clone() const {return new M7(*this);}

    //    string name() const;

    M7(const M0& M1,const ReversibleFrequencyModel& R, int n);
  };


  class MixtureModel: public MultiModel, public SuperModelOver<MultiModel> 
  {
    efloat_t super_prior() const;
    void recalc(const std::vector<int>&) {std::abort();}
    void update() { }
  public:
    virtual MixtureModel* clone() const {return new MixtureModel(*this);}

    boost::shared_ptr<const Object> result() const;

    string name() const;

    MixtureModel(const std::vector<owned_ptr<MultiModel> >& models);
  };

  
  class ModulatedMarkovModel: public ReversibleMarkovModel,
			      public OpModel
  {
  protected:
    void recalc(const std::vector<int>&) {std::abort();}

  public:
    virtual ModulatedMarkovModel* clone() const {return new ModulatedMarkovModel(*this);}

    ModulatedMarkovModel(const MultiModel& M, const ExchangeModel& S);
  };

  /// In M8b model, omega1 ~ Beta with proportion p1.
  /// omega2 = 1 with p2.
  /// omega3 > 1 with p3.
  ///class M8b: public MultiParameterModel {
  ///public:
  ///
  ///  M8b(const M0& M1,const ReversibleFrequencyModel& R, int n);
  ///};
  ///public ReversibleMarkovModel,
  /// MultiParameterModel::p_change - 
  /// 3? Two from Beta, omega3. omega2 is fixed.
  /// 2? omega1, and omega3. omega2 is fixed.
  /// We need bin numbers for Beta.
  /// MultiParameterModel (MultiModel object, p = 2, n = 3)
  /// 
  /// NOTE that we need to instantiate M8b somewhere else
  /// when the program starts.
  class M8b: public MultiParameterModel
  {
  protected:
    owned_ptr<Beta> S;

    int nbin;
    std::vector<double> pi;

    void recalc(const std::vector<int>&) {std::abort();}
    void update() { }

    /* These methods are already defined for us in SuperModelOver<::Model>
    const ::Model& SubModels(int i) const;
          ::Model& SubModels(int i) */

  public:
    boost::shared_ptr<const Object> result() const;

    virtual M8b* clone() const {return new M8b(*this);}

    efloat_t super_prior() const;

    // This is already defined as SubmodelAs<MultiModel>(0).frequencies() in ReversibleWrapperOver<MultiModel>
    // const valarray<double>& frequencies() const {return pi;}

    string name() const;

    // This is already defined as SubmodelAs<MultiModel>(0).frequencies() in ReversibleWrapperOver<MultiModel>
    // const alphabet& Alphabet() const {return M->Alphabet();}

    M8b(const M0& M, const ReversibleFrequencyModel& R, const int n_binBeta);
  };


}

#endif


