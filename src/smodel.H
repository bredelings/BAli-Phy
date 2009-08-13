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
*/

#ifndef SMODEL_H
#define SMODEL_H

#include <valarray>
#include <vector>
#include <string>
#include <boost/shared_ptr.hpp>
#include "mytypes.H"
#include "alphabet.H"
#include "rates.H"
#include "model.H"
#include "eigenvalue.H"

namespace substitution {

  using std::string;
  using std::valarray;

  //--------------------- class AlphabetModel ----------------------//
  /// A model that is associated with an alphabet
  class AlphabetModel: virtual public ::Model
  {
  public:
    virtual const alphabet& Alphabet() const =0;

    unsigned n_letters() const {return Alphabet().size();}
  };

  /// This class stores an alphabet AND associates a model with it
  template <class alphabet_t>
  class ModelWithAlphabet: virtual public AlphabetModel
  {
    boost::shared_ptr<const alphabet_t> A;
  public:
    const alphabet_t& Alphabet() const {return *A;}

    ModelWithAlphabet(const alphabet_t& a)
      :A(a.clone())
    {}
  };
  


  /// A generic model of the process of substitution
  class Model: virtual public AlphabetModel
  {
  public:
    
    virtual const vector<unsigned>& state_letters() const=0;

    unsigned n_states() const { return state_letters().size();}

    /// Make a copy of this object
    virtual Model* clone() const =0;

    /// Construct an empty model
    Model() {};

    virtual ~Model() {}
  };


  //----------------------- Nested Models ------------------------///

  // This models a substitution process in which evolution is independant between
  // non-overlapping intervals.  We should have that transition_p(t/2)^2 = transition_p(t)
  // This model must also be reversible

  class ReversibleModel: virtual public Model 
  {
  public:
    /// Make a copy of this object
    virtual ReversibleModel* clone() const =0;

    /// The transition probability matrix over time t
    virtual Matrix transition_p(double t) const =0;

    /// Get the equilibrium frequencies
    virtual const valarray<double>& frequencies() const=0;

    ReversibleModel() {}
  };

  /// An AdditiveModel depends only on t2-t1

  /// In additive models P(X[t+tau]=b|X[t]=a) = P(X[tau]=b|X[0]=a)
  /// Also E f(X,(a,b)) = E f(X,(0,b-a))

  class AdditiveModel: virtual public Model 
  {
  public:
    virtual AdditiveModel* clone() const=0;

    virtual double rate() const =0;
    virtual void set_rate(double) =0;

    AdditiveModel() {}
  };

  class ReversibleAdditiveModel: virtual public ReversibleModel, virtual public AdditiveModel 
  {
  public:
    virtual ReversibleAdditiveModel* clone() const=0;
  };

  // With an Independant Model, you can chain transition matrices.
  // Q_t(a,b) = sum_c Q[0,tau](a,c) x Q[tau,t](c,b)

  // In an independant additive
  class IndependantAdditiveModel: virtual public AdditiveModel 
  {
  public:
    virtual IndependantAdditiveModel* clone() const=0;

    /// The transition probability matrix
    virtual Matrix transition_p(double t) const =0;

    IndependantAdditiveModel() {}
  };

  class Independant_Additive_Reversible_Model: public ReversibleAdditiveModel, 
					       virtual public IndependantAdditiveModel
  {
  public:
    virtual Independant_Additive_Reversible_Model* clone() const=0;

    /// The transition probability matrix
    virtual Matrix transition_p(double) const=0;
  };


  //------------------------- Markov Substitution Model ----------------------------//

  /// A substitution model which obeys the Markov property
  class MarkovModel: virtual public IndependantAdditiveModel 
  {
  protected:

    /// Rate matrix
    Matrix Q;

    vector<unsigned> state_letters_;

  public:

    virtual MarkovModel* clone() const =0;

    /// The transition rate matrix
    virtual const Matrix& transition_rates() const {return Q;};

    /// The transition probability matrix
    virtual Matrix transition_p(double t) const =0;

    virtual const vector<unsigned>& state_letters() const {return state_letters_;}

    /// Construct a Makov model on alphabet 'a_'
    MarkovModel(const alphabet& a);
  };


  //------------------------ Exchangeability Model ----------------------//



  class ExchangeModel: virtual public ::Model
  {
  protected:
    SMatrix S;

  public:
    
    double operator()(int i,int j) const {return S(i,j);}

    unsigned n_states() const {return S.size1();}

    ExchangeModel* clone() const =0;
    
    ExchangeModel(unsigned n);

    virtual ~ExchangeModel() {};
  };

  class SimpleExchangeModel: public ExchangeModel
  {
    void recalc(const vector<int>&);

  public:

    SimpleExchangeModel* clone() const {return new SimpleExchangeModel(*this);}

    /// Get the parameter 'rho' (model switching rate)
    double rho() const {return parameter(0);}
    /// Set the parameter 'rho' (model switching rate)
    void   rho(double r) {parameter(0,r);}

    virtual efloat_t prior() const;

    string name() const;

    SimpleExchangeModel(unsigned n);

    ~SimpleExchangeModel() {}
  };


  class AlphabetExchangeModel: public ExchangeModel,
			       virtual public AlphabetModel 
  {
  public:
    
    AlphabetExchangeModel* clone() const =0;
    
    AlphabetExchangeModel(const alphabet& a);

    virtual ~AlphabetExchangeModel() {};
  };

  
  class INV_Model: public AlphabetExchangeModel,public ModelWithAlphabet<alphabet> 
  {
    Matrix P;
  public:
    INV_Model* clone() const {return new INV_Model(*this);}

    string name() const;
      
    INV_Model(const alphabet& a);
  };

  //--------------------- EQU Substitution Model ------------------------/

  /// The EQU model for any alphabet - all letters are treated the same
  /// This model has no parameters
  class EQU: public AlphabetExchangeModel,public ModelWithAlphabet<alphabet> 
  {
  public:

    virtual EQU* clone() const {return new EQU(*this);}

    string name() const;

    EQU(const alphabet& a);

    virtual ~EQU() {}
  };

  //------------------ Empirical Substitution Model ---------------------//

  /// A generic model for any alphabet - the exchangeability matrix is loaded from a file
  class Empirical: public AlphabetExchangeModel,public ModelWithAlphabet<alphabet> 
  {
    /// Specificy this model based on the matrix that was loaded
    string modelname;

    /// Load the exhangeability matrix from file 'filename'
    void load_file(const string& filename);

  public:

    virtual Empirical* clone() const {return new Empirical(*this);}

    string name() const;

    Empirical(const alphabet& a,const string& filename); 

    ~Empirical() {}
  };

  class NucleotideExchangeModel: public AlphabetExchangeModel,
				 public ModelWithAlphabet<Nucleotides>
  {
  public:
    NucleotideExchangeModel* clone() const=0;

    NucleotideExchangeModel(const Nucleotides& N)
      :AlphabetExchangeModel(N),
       ModelWithAlphabet<Nucleotides>(N)
    {}
  };

  /// The Hasegawa, Kishino, Yano (1985)
  class HKY: public NucleotideExchangeModel
  {
    virtual void recalc(const vector<int>&);
  public:

    virtual HKY* clone() const {return new HKY(*this);}

    /// Get the parameter 'kappa' (transition/transversion ratio)
    double kappa() const {return parameter(0);}
    /// Set the parameter 'kappa' (transition/transversion ratio)
    void kappa(double k) {parameter(0,k);}

    virtual efloat_t prior() const;

    string name() const;

    /// Construct an HKY model on alphabet 'a'
    HKY(const Nucleotides& N);
  };

  /// The Tamura-Nei (1993) model
  class TN: public NucleotideExchangeModel 
  {
    virtual void recalc(const vector<int>&);
  public:

    virtual TN* clone() const {return new TN(*this);}

    /// Get the parameter 'kappa' (transition/transversion ratio)
    double kappa1() const {return parameter(0);}
    /// Get the parameter 'kappa' (transition/transversion ratio)
    double kappa2() const {return parameter(1);}

    /// Set the parameter 'kappa' (transition/transversion ratio)
    void kappa1(double k) {parameter(0,k);}
    /// Set the parameter 'kappa' (transition/transversion ratio)
    void kappa2(double k) {parameter(1,k);}

    virtual efloat_t prior() const;

    string name() const;

    /// Construct an HKY model on alphabet 'a'
    TN(const Nucleotides& N);
  };

  /// The General Time Reversible model
  class GTR: public NucleotideExchangeModel 
  {
    virtual void recalc(const vector<int>&);
  public:
    
    virtual GTR* clone() const {return new GTR(*this);}

    virtual efloat_t prior() const;

    string name() const;

    /// Construct a GTR model on alphabet 'a'
    GTR(const Nucleotides& N);
  };
    

  class TripletExchangeModel: public AlphabetExchangeModel, 
			      public ModelWithAlphabet<Triplets> 
  {
  public:
    TripletExchangeModel* clone() const=0;

    TripletExchangeModel(const Triplets& T);

    ~TripletExchangeModel() {}
  };

  class SingletToTripletExchangeModel: public TripletExchangeModel,
				       public SuperModelOver<NucleotideExchangeModel>
  {
    void recalc(const vector<int>&);
  public:

    efloat_t prior() const { return SuperModelOver<NucleotideExchangeModel>::prior(); }

    SingletToTripletExchangeModel* clone() const {return new SingletToTripletExchangeModel(*this);}

    string name() const;

    SingletToTripletExchangeModel(const Triplets& T,const NucleotideExchangeModel& M);

    ~SingletToTripletExchangeModel() {}
  };



  class CodonAlphabetExchangeModel: public AlphabetExchangeModel, 
			    public ModelWithAlphabet<Codons> 
  {
  public:
    CodonAlphabetExchangeModel* clone() const=0;

    int AminoAcid(int l) const {return Alphabet().translate(l);}

    CodonAlphabetExchangeModel(const Codons& C);

    ~CodonAlphabetExchangeModel() {}
  };


  class M0: public CodonAlphabetExchangeModel,
		public SuperModelOver<NucleotideExchangeModel>
  {
    void recalc(const vector<int>&);
  public:
    efloat_t super_prior() const;

    efloat_t prior() const;

    M0* clone() const {return new M0(*this);}

    /// Get the parameter 'omega' (non-synonymous/synonymous rate ratio)
    double omega() const;
    /// Set the parameter 'omega' (non-synonymous/synonymous rate ratio)
    void omega(double w);

    string name() const;

    M0(const Codons& C,const NucleotideExchangeModel& M);

    ~M0();
  };

  //------------------------ Reversible frequency model --------------------//
  class ReversibleFrequencyModel: virtual public AlphabetModel
  {
  protected:
    Matrix R;

    valarray<double> pi;
  public:
    ReversibleFrequencyModel* clone() const =0;

    /// Get the equilibrium frequencies
    const valarray<double>& frequencies() const {return pi;}

    double operator()(int i,int j) const {return R(i,j);}

    ReversibleFrequencyModel(const alphabet& a);

    virtual ~ReversibleFrequencyModel() {}
  };



  class SimpleFrequencyModel: public ReversibleFrequencyModel,
			      public ModelWithAlphabet<alphabet>
  {
  protected:
    void recalc(const vector<int>&);

  public:
    
    const valarray<double>& frequencies() const {return pi;}

    void frequencies(const valarray<double>&);
    
    double f() const {return parameters_[0];}

    SimpleFrequencyModel* clone() const {return new SimpleFrequencyModel(*this);}

    efloat_t prior() const;

    string name() const;

    explicit SimpleFrequencyModel(const alphabet& a);

    explicit SimpleFrequencyModel(const alphabet& a,const valarray<double>&);
  };

  class UniformFrequencyModel: public ReversibleFrequencyModel,
			      public ModelWithAlphabet<alphabet>
  {
  protected:
    void recalc(const vector<int>&);

  public:
    
    const valarray<double>& frequencies() const {return pi;}

    void frequencies(const valarray<double>&);
    
    UniformFrequencyModel* clone() const {return new UniformFrequencyModel(*this);}

    string name() const;

    explicit UniformFrequencyModel(const alphabet& a);

    explicit UniformFrequencyModel(const alphabet& a,const valarray<double>&);
  };

  class TripletFrequencyModel: public ReversibleFrequencyModel,
			       public ModelWithAlphabet<Triplets>
  {
  public:
    TripletFrequencyModel* clone() const =0;

    explicit TripletFrequencyModel(const Triplets& T);
  };

  class IndependentNucleotideFrequencyModel: public TripletFrequencyModel,
					 public SuperModelOver<SimpleFrequencyModel>
  {
    OwnedPointer<SimpleFrequencyModel> triplets;

    void recalc(const vector<int>&);

  public:
    IndependentNucleotideFrequencyModel* clone() const {return new IndependentNucleotideFrequencyModel(*this);}

    string name() const;

    IndependentNucleotideFrequencyModel(const Triplets& T);
  };


  class TripletsFrequencyModel: public TripletFrequencyModel,
				public SuperModelOver<SimpleFrequencyModel>
  {
  protected:
    void recalc(const vector<int>&);

  public:
    TripletsFrequencyModel* clone() const {return new TripletsFrequencyModel(*this);}

    efloat_t super_prior() const;

    string name() const;

    explicit TripletsFrequencyModel(const Triplets& T);
  };

  class CodonFrequencyModel: public ReversibleFrequencyModel,
			       public ModelWithAlphabet<Codons>
  {
  public:
    CodonFrequencyModel* clone() const =0;

    explicit CodonFrequencyModel(const Codons& C);
  };

  class AACodonFrequencyModel: public CodonFrequencyModel,
			       public SuperModelOver<SimpleFrequencyModel>
  {
  protected:
    OwnedPointer<SimpleFrequencyModel> codons;

    void recalc(const vector<int>&);

    int aa_size() const {return Alphabet().getAminoAcids().size();}
  public:
    AACodonFrequencyModel* clone() const {return new AACodonFrequencyModel(*this);}

    string name() const;

    explicit AACodonFrequencyModel(const Codons& C);
  };

  class CodonsFrequencyModel: public CodonFrequencyModel,
			      public SuperModelOver<TripletsFrequencyModel>
  {
  protected:
    void recalc(const vector<int>&);

    int aa_size() const {return Alphabet().getAminoAcids().size();}

  public:
    CodonsFrequencyModel* clone() const {return new CodonsFrequencyModel(*this);}

    efloat_t super_prior() const;

    string name() const;

    explicit CodonsFrequencyModel(const Codons& C);
  };

  
  class CodonsFrequencyModel2: public CodonFrequencyModel,
			      public SuperModelOver<TripletsFrequencyModel>
  {
  protected:
    void recalc(const vector<int>&);

    int aa_size() const {return Alphabet().getAminoAcids().size();}

  public:
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
  class ReversibleMarkovModel: public MarkovModel,
			       public Independant_Additive_Reversible_Model 
  {
    EigenValues eigensystem;

  protected:
    void recalc_eigensystem();

  public:
    double rate() const;
    void set_rate(double);
    
    /// Make a copy of this object
    virtual ReversibleMarkovModel* clone() const =0;

    /// The transition probability matrix - which we can now compute
    Matrix transition_p(double t) const;

    ReversibleMarkovModel(const alphabet& a);
    
    ~ReversibleMarkovModel() {}
  };
  
  class ReversibleMarkovSuperModel: public ReversibleMarkovModel,
				    public SuperModel,
				    public ModelWithAlphabet<alphabet>
  {
  protected:
    /// The exchange model
    OwnedPointer<AlphabetExchangeModel> S;

    /// The frequency model
    OwnedPointer<ReversibleFrequencyModel> R;

    void recalc(const vector<int>&);

    const ::Model& SubModels(int i) const {
      if (i==0)
	return *S;
      else if (i==1)
	return *R;
      else
	std::abort();
    }

    ::Model& SubModels(int i) {
      if (i==0)
	return *S;
      else if (i==1)
	return *R;
      else
	std::abort();
    }

  public:

    /// Make a copy of this object
    virtual ReversibleMarkovSuperModel* clone() const {return new ReversibleMarkovSuperModel(*this);}

    /// Get the equilibrium
    const valarray<double>& frequencies() const {return R->frequencies();}

    efloat_t prior() const {return S->prior() * R->prior();}

    string name() const;

    /// Construct a reversible Markov model on alphabet 'a'
    ReversibleMarkovSuperModel(const AlphabetExchangeModel& S1, const ReversibleFrequencyModel& R1);
    
    ~ReversibleMarkovSuperModel() {}
  };


  class SimpleReversibleMarkovModel: public ReversibleMarkovSuperModel
  {
  public:
    /// Make a copy of this object
    virtual SimpleReversibleMarkovModel* clone() const {return new SimpleReversibleMarkovModel(*this);}

    const valarray<double>& frequencies() const {return R->frequencies();}
    
    void frequencies(const valarray<double>& pi);

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

    /// Get the equilibrium frequencies
    const valarray<double>& frequencies() const {
      return SubModel().frequencies();
    }

    const alphabet& Alphabet() const { 
      return SubModel().Alphabet(); 
    }

    ReversibleWrapperOver(const T& M) {
      insert_submodel("BASE",M);
    }
  };


  //----------------------- MultiModel ---------------------------*/

  //Hmmm [FIXME] ... The peeling algorithm assumes independance across branches
  //                 and a reversible markov chain across each branch.
  //
  //                 The reversible Markov chain on a branch can be generated from
  //                 a CONTINUOUS TIME Markov Chain - OR something else.
  //
  //                 Right now I am requiring an Independant, Additive, Reversible
  //                 PROCESS, but of course the gamma_branch model doesn't fit
  //                 that requirement.  So we are faking it...

  //NOTE: - a linear combination of RA models is RA, but not RAI
  //Do I really need to special-case this so that multi-models of RA models are
  //known to be RA?  Would prefer not to use templates unless necessary/helpful.

  /// A mixture of reversible additive models

  /// Umm... is this reversible?
  /// Also, what if we have two base classes: ReversibleAdditive, and F81?
  class MultiModel: public ReversibleAdditiveModel 
  {
  public:

    typedef ReversibleAdditiveModel Base_Model_t;
    
    MultiModel* clone() const =0;

    /// The number of models
    virtual int n_base_models() const=0;

    /// Access the base models
    virtual const Base_Model_t& base_model(int m) const=0;
    virtual       Base_Model_t& base_model(int m)      =0;

    double rate() const;
    void set_rate(double);

    virtual const vector<unsigned>& state_letters() const {return base_model(0).state_letters();}

    /// Get the probability of each base models
    virtual std::vector<double> distribution() const=0;

    /// Get a transition probability matrix for time 't', averaging over models
    Matrix transition_p(double t) const;

    /// Get a transition probability matrix for time 't' and model 'm'
    Matrix transition_p(double t,int m) const {return base_model(m).transition_p(t);}
  };

  Matrix frequency_matrix(const MultiModel&);
 
  /// A MultiRate Model with a single rate
  class UnitModel: public MultiModel,
		   public ReversibleWrapperOver<MultiModel::Base_Model_t> 
  {
  public:
    UnitModel* clone() const {return new UnitModel(*this);}

    /// The number of models
    int n_base_models() const {return 1;}

    /// Access the base model
    const Base_Model_t& base_model(int) const {return SubModel();}
          Base_Model_t& base_model(int)       {return SubModel();}

    std::vector<double> distribution() const {return vector<double>(1,1);}

    string name() const;

    UnitModel(const Base_Model_t& M);

    ~UnitModel() {};
  };


  /// A substitution model based on running identical copies of an input model with different frequencies.
  class MultiFrequencyModel: public MultiModel, 
			     public ReversibleWrapperOver<SimpleReversibleMarkovModel> 
  {
  protected:
    /// Components of the mixture - Reversible Markov models express as S+R
    vector<OwnedPointer<SimpleReversibleMarkovModel> > sub_parameter_models;

    /// Fraction of the sites with each parameter value (weights on the mixture components)
    vector<double> fraction;

    // a(m,l) is the probability that a letter is in component "m" given that it is of type "l"
    inline double& a(int m,int l)       {return parameters_[m+l*fraction.size()];}
    inline double  a(int m,int l) const {return parameters_[m+l*fraction.size()];}

    void recalc(const vector<int>&);


  public:
    efloat_t super_prior() const;

    MultiFrequencyModel* clone() const {return new MultiFrequencyModel(*this);}

    /// The number of models
    int n_base_models() const {return fraction.size();}

    /// Access the base models
    const Base_Model_t& base_model(int m) const;
          Base_Model_t& base_model(int m)      ;

    /// Get the probability of each base models
    std::vector<double> distribution() const;

    /// Get the equilibrium frequencies
    const valarray<double>& frequencies() const;

    string name() const;

    MultiFrequencyModel(const AlphabetExchangeModel& M,int n_bins);

    ~MultiFrequencyModel() {}
  };


  /// A substitution model based on running identical copies of an input model with different frequencies.
  class CAT_FixedFrequencyModel: public MultiModel, public ModelWithAlphabet<alphabet>
  {
  protected:
    /// Components of the mixture - Reversible Markov models express as S+R
    vector<OwnedPointer<SimpleReversibleMarkovModel> > sub_parameter_models;

    /// Fraction of the sites with each parameter value (weights on the mixture components)
    vector<double> fraction;

    string model_name;

    void load_file(const string& filename);

  public:

    // no parameters: super_prior() == 1

    CAT_FixedFrequencyModel* clone() const {return new CAT_FixedFrequencyModel(*this);}

    /// The number of models
    int n_base_models() const {return fraction.size();}

    /// Access the base models
    const Base_Model_t& base_model(int m) const;
          Base_Model_t& base_model(int m)      ;

    /// Get the probability of each base models
    std::vector<double> distribution() const;

    /// Get the equilibrium frequencies - Gotta define this, cuz MultiModel is reversible...
    const valarray<double>& frequencies() const;

    string name() const;

    CAT_FixedFrequencyModel(const alphabet& a, const string& filename);

    ~CAT_FixedFrequencyModel() {}
  };


  /// A substitution model based on running Reversible Models with the same equilibrium at different rates
  class MultiParameterModel: public MultiModel, 
			     public ReversibleWrapperOver<MultiModel> 
  {
  protected:
    /// MultiModels w/ different values of the parameter set
    vector<OwnedPointer<MultiModel> > sub_parameter_models;
    /// Fraction of the sites with each parameter value
    vector<double> fraction;

    /// The parameter which is varying;
    int p_change;
    /// The values it takes on
    vector<double> p_values;

    void recalc_submodel_instances();
  public:
    MultiParameterModel* clone() const=0;

    /// The number of models
    int n_base_models() const {return p_values.size() * SubModel().n_base_models();}

    /// Access the base models
    const Base_Model_t& base_model(int m) const;
          Base_Model_t& base_model(int m)      ;

    /// Get the probability of each base models
    std::vector<double> distribution() const;

    /// Get the equilibrium frequencies
    const valarray<double>& frequencies() const;

    MultiParameterModel(const MultiModel& M,int p,int n_bins);

    ~MultiParameterModel() {}
  };


  /// A class of uncoupled rates/values
  class DirichletParameterModel: public MultiParameterModel
  {
  protected:
    efloat_t super_prior() const;

    void recalc(const vector<int>&);

  public:

    DirichletParameterModel* clone() const {return new DirichletParameterModel(*this);}

    string name() const;

    DirichletParameterModel(const MultiModel& M,int p,int n);
  };


  class DistributionParameterModel: public MultiParameterModel
  {
    bool good_enough;

  protected:
          Distribution& D();
    const Distribution& D() const;

    efloat_t super_prior() const;

    void recalc(const vector<int>&);

  public:

    DistributionParameterModel* clone() const {return new DistributionParameterModel(*this);}

    string name() const;

    DistributionParameterModel(const MultiModel& M, const Distribution& RD,int p,int n);
  };

  /// A discrete-gamma Model
  //  Only has one parameter: the shape parameter for the gamma
  class GammaParameterModel: public DistributionParameterModel {

  public:
    virtual GammaParameterModel* clone() const {return new GammaParameterModel(*this);}

    /// Construct a discrete gamma model on top of 'M', with 'n' bins
    GammaParameterModel(const MultiModel& M, int n);
  };

  /// A discrete-lognormal Model
  //  Only has one parameter: the shape parameter for the gamma
  class LogNormalParameterModel: public DistributionParameterModel {
  public:
    virtual LogNormalParameterModel* clone() const {return new LogNormalParameterModel(*this);}

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

    OwnedPointer<SimpleReversibleMarkovModel> INV;

  public:
    virtual WithINV* clone() const {return new WithINV(*this);}

    void recalc(const vector<int>&);

    efloat_t super_prior() const;

    /// The number of models
    int n_base_models() const {return 1 +  SubModel().n_base_models();}

    /// Access the base models
    const Base_Model_t& base_model(int m) const;
          Base_Model_t& base_model(int m)      ;

    std::vector<double> distribution() const;

    /// Get the equilibrium frequencies
    const valarray<double>& frequencies() const;

    string name() const;

    WithINV(const MultiModel& M);
  };

  /// The Invariant-sites model: a certain fraction of the sites don't change

  /// The Invariant-sites model: a certain fraction of the sites don't change
  /// The only parameter is 'p', the fraction of sites which don't change
  /// To keep branch lengths the same, inflate rates by 1/(1-p).
  //  class WithINV2: public MultiModel, public ReversibleWrapperOver<MultiModel> 

  class WithINV2: public MultiModel,
		  public SuperModelOver< ::Model>
  {
    valarray<double> freq;

    static const double inv_frac_mean;
    static const double max_inv_rate;

    const MultiModel& VAR() const;
          MultiModel& VAR();

    const SimpleReversibleMarkovModel& INV() const ;
          SimpleReversibleMarkovModel& INV();

  public:
    virtual WithINV2* clone() const {return new WithINV2(*this);}

    const alphabet& Alphabet() const {return VAR().Alphabet();}

    void recalc(const vector<int>&);

    efloat_t super_prior() const;

    /// The number of models
    int n_base_models() const {return 1 +  VAR().n_base_models();}

    /// Access the base models
    const Base_Model_t& base_model(int m) const;
          Base_Model_t& base_model(int m)      ;

    std::vector<double> distribution() const;

    /// Get the equilibrium frequencies
    const valarray<double>& frequencies() const;

    string name() const;

    WithINV2(const MultiModel& M);
  };

  class M2: public MultiParameterModel
  {
    void recalc(const vector<int>&);

  public:
    
    efloat_t super_prior() const;

    M2* clone() const {return new M2(*this);}

    string name() const;

    M2(const M0& M1,const ReversibleFrequencyModel& R);
  };


  class M3: public MultiParameterModel 
  {
    void recalc(const vector<int>&);
  public:
    
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
    void recalc(const vector<int>&);

    valarray<double> pi;

  public:
    virtual MixtureModel* clone() const {return new MixtureModel(*this);}

    /// The number of models
    int n_base_models() const;

    const Base_Model_t& base_model(int m) const;
          Base_Model_t& base_model(int m);

    /// Get the equilibrium frequencies
    const valarray<double>& frequencies() const {return pi;}

    std::vector<double> distribution() const;

    string name() const;

    const alphabet& Alphabet() const {
      assert(SubModels(0).Alphabet() == SubModels(1).Alphabet());
      return SubModels(0).Alphabet();
    }

    MixtureModel(const std::vector<OwnedPointer<MultiModel> >& models);
  };

  
  class ModulatedMarkovModel: public ReversibleMarkovModel,
			      public SuperModel
  {
  protected:
    OwnedPointer<MultiModel> M;
    OwnedPointer<ExchangeModel> S;

    valarray<double> pi;

    void recalc(const vector<int>&);

    const ::Model& SubModels(int i) const {
      if (i==0)
	return *M;
      else if (i==1)
	return *S;
      else
	std::abort();
    }

    ::Model& SubModels(int i) {
      if (i==0)
	return *M;
      else if (i==1)
	return *S;
      else
	std::abort();
    }

  public:

    virtual ModulatedMarkovModel* clone() const {return new ModulatedMarkovModel(*this);}

    const valarray<double>& frequencies() const {return pi;}

    string name() const;

    const alphabet& Alphabet() const {return M->Alphabet();}

    ModulatedMarkovModel(const MultiModel& M, const ExchangeModel& S);
  };
}
#endif


