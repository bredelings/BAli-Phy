#ifndef SMODEL_H
#define SMODEL_H

#include <valarray>
#include <vector>
#include <string>
#include "mytypes.H"
#include "alphabet.H"
#include "rates.H"
#include "model.H"
#include "eigenvalue.H"

namespace substitution {
  using ::SuperDerivedModel;
  using ::SuperModelOver;
  using ::SuperDerivedModelOver;

  using std::string;

  class FullModel {
  public:
    bool full_tree;
  };


  //--------------------- Abstract base Model ----------------------///

  /// A generic substitution model
  class Model: virtual public ::Model {
  protected:

    /// Construct an empty model
    Model(int s):full_tree(true) {
      set_n_parameters(s);
    }

  public:
    
    bool full_tree;

    /// The alphabet we are modelling substitution on
    virtual const alphabet& Alphabet() const =0;

    /// Make a copy of this object
    virtual Model* clone() const =0;

    /// Construct an empty model
    Model():full_tree(true) {};

    virtual ~Model() {}
  };


  //----------------------- Nested Models ------------------------///
  // FIXME - I need to expand this to store multiple submodels


  // Still need to deal with full_tree
  // full_tree = SubModels(0).full_tree;

  /// Abstract class for substitution models built on a single submodel
  class NestedModel: virtual public SuperDerivedModel<Model> {

  protected:
    /// Access the submodel
    virtual const Model& SubModel() const=0;
    virtual Model& SubModel()=0;

  public:
    /// Make a copy of ourselves
    NestedModel* clone() const =0;
  };


  /// This is a substitution model built over a submodel of a particular type
  template <class SubModel_t>
  class NestedModelOver: virtual public NestedModel, public SuperDerivedModelOver<SubModel_t,Model> {

  protected:
    /// Access the submodel
    const SubModel_t& SubModel() const {return SubModels(0);}
    SubModel_t& SubModel() {return SubModels(0);}

  public:
    /// Make a copy of ourselves
    NestedModelOver<SubModel_t>* clone() const =0;

    /// Construct a Nested model based on model 'M', with 'dp' extra parameters
    NestedModelOver(const SubModel_t& M,int dp)
      : SuperDerivedModelOver<SubModel_t,Model>(vector< OwnedPointer<SubModel_t> >(1,M),dp)
    { }
  };


  // This models a substitution process in which evolution is independant between
  // non-overlapping intervals.  We should have that transition_p(t/2)^2 = transition_p(t)
  // This model must also be reversible

  class ReversibleModel: virtual public Model {

  public:
    /// Make a copy of this object
    virtual ReversibleModel* clone() const =0;

    /// The transition probability matrix over time t
    virtual Matrix transition_p(double t) const =0;

    /// Get the equilibrium frequencies
    virtual const std::valarray<double>& frequencies() const=0;

    /// Set the equilibrium frequencies
    virtual void frequencies(const std::valarray<double>&) =0;

    ReversibleModel() {}
  };

  /// An AdditiveModel depends only on t2-t1

  /// In additive models P(X[t+tau]=b|X[t]=a) = P(X[tau]=b|X[0]=a)
  /// Also E f(X,(a,b)) = E f(X,(0,b-a))

  class AdditiveModel: virtual public Model {
  public:
    virtual AdditiveModel* clone() const=0;

    virtual double rate() const =0;
    virtual void set_rate(double) =0;

    AdditiveModel() {}
  };

  class ReversibleAdditiveModel: virtual public ReversibleModel, virtual public AdditiveModel {
  public:
    virtual ReversibleAdditiveModel* clone() const=0;
  };

  // With an Independant Model, you can chain transition matrices.
  // Q_t(a,b) = sum_c Q[0,tau](a,c) x Q[tau,t](c,b)

  // In an independant additive
  class IndependantAdditiveModel: virtual public AdditiveModel {

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
  class MarkovModel: virtual public IndependantAdditiveModel {

  protected:

    /// The computed rate matrix
    Matrix Q;

  public:

    virtual MarkovModel* clone() const =0;

    /// The transition rate matrix
    virtual const Matrix& transition_rates() const {return Q;}

    /// The transition probability matrix
    virtual Matrix transition_p(double t) const =0;

    /// Construct a Makov model on alphabet 'a_'
    MarkovModel(const alphabet& a)
      :Q(a.size(),a.size())
    {}
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
  protected:
    /// The equilibrium frequencies
    std::valarray<double> pi;

    /// The exchangeability matrix
    Matrix S;

    EigenValues eigensystem;

    virtual void recalc();

    /// Construct a reversible Markov model on alphabet 'a_'
    ReversibleMarkovModel(const alphabet& a,int dp):
      MarkovModel(a), pi(1.0/a.size(),a.size()), 
      S(a.size(),a.size()),
      eigensystem(a.size())
    { set_n_parameters(dp+1);parameters_[0] = 1;}
    

  public:
    double rate() const;
    void set_rate(double);
    
    Matrix getD() const;
    
    const Matrix& getS() const {return S;}
    
    /// Make a copy of this object
    virtual ReversibleMarkovModel* clone() const =0;

    /// The transition probability matrix - which we can now compute
    Matrix transition_p(double t) const;

    /// Get the equilibrium frequencies
    const std::valarray<double>& frequencies() const {return pi;}

    /// Set the equilibrium frequencies
    void frequencies(const std::valarray<double>& pi_) {pi=pi_; recalc();}
  
    efloat_t prior() const;

    /// Construct a reversible Markov model on alphabet 'a_'
    ReversibleMarkovModel(const alphabet& a):
      MarkovModel(a), pi(1.0/a.size(),a.size()), 
      S(a.size(),a.size()),
      eigensystem(a.size())
    { set_n_parameters(1) ; parameters_[0] = 1;}
    
    ~ReversibleMarkovModel() {}
  };

    //--------------------------- Implement the underlying alphabet -------------------------//


    template <class alphabet_t>
    class ModelWithAlphabet: virtual public Model {
      //    protected:
      OwnedPointer<alphabet_t> A;
    public:
      const alphabet_t& Alphabet() const {return *A;}
      
      ModelWithAlphabet(const alphabet_t& a)
	:A(a)
      {}
    };


  class INV_Model: public ReversibleMarkovModel,public ModelWithAlphabet<alphabet> 
  {
    Matrix P;
  public:
    INV_Model* clone() const {return new INV_Model(*this);}

    void set_rate(double);

    void recalc() {}

    double fiddle(int) {return 1;}

    Matrix transition_p(double t) const;

    string name() const;
    string parameter_name(int i) const;
      
    INV_Model(const alphabet& a);
  };


  template <class T>
  class ReversibleWrapperOver: virtual public ReversibleModel,
			       public NestedModelOver<T>
  {
  public:
    virtual ReversibleWrapperOver* clone() const=0;

    /// Get the equilibrium frequencies
    const std::valarray<double>& frequencies() const {
      return NestedModelOver<T>::SubModel().frequencies();
    }
    /// Set the equilibrium frequencies
    void frequencies(const std::valarray<double>& f) {
      return NestedModelOver<T>::SubModel().frequencies(f);
    }

    const alphabet& Alphabet() const { 
      return NestedModelOver<T>::SubModel().Alphabet(); 
    }

    ReversibleWrapperOver(const T& t,int dp):NestedModelOver<T>(t,dp) {}
  };


  // This model is not really additive: E N ~ t but Var N ~ t^2 so that sigma/mu is constant
  class Gamma_Branch_Model:public Independant_Additive_Reversible_Model,
			   public ReversibleWrapperOver<ReversibleMarkovModel> 
  {
    double super_fiddle(int);
    efloat_t super_prior() const;

  public:
    virtual Gamma_Branch_Model* clone() const {return new Gamma_Branch_Model(*this);}
    
    double rate() const {return SubModel().rate();}
    void set_rate(double r) {SubModel().set_rate(r);}
    
    string name() const;
    string super_parameter_name(int i) const;
      
    Matrix transition_p(double t) const;

    Gamma_Branch_Model(const ReversibleMarkovModel& M)
      :ReversibleWrapperOver<ReversibleMarkovModel>(M,1) {
      parameters_[parameters_.size()-1]=0.1;
    }
  };



  // This model is truly additive: E N ~ t and Var N ~ t, where N is number of changes.
  class Gamma_Stretched_Branch_Model:public Independant_Additive_Reversible_Model,
				     public ReversibleWrapperOver<ReversibleMarkovModel> 
  {
    double super_fiddle(int);
    efloat_t super_prior() const;
    string super_parameter_name(int i) const;
  public:
    virtual Gamma_Stretched_Branch_Model* clone() const {return new Gamma_Stretched_Branch_Model(*this);}
    
    double rate() const {return SubModel().rate();}
    void set_rate(double r) {SubModel().set_rate(r);}
    
    string name() const;
    
    Matrix transition_p(double t) const;

    Gamma_Stretched_Branch_Model(const ReversibleMarkovModel& M)
      :ReversibleWrapperOver<ReversibleMarkovModel>(M,1) {
      parameters_[parameters_.size()-1]=0.1;
    }
  };



  //--------------------- EQU Substitution Model ------------------------/

  /// The EQU model for any alphabet - all letters are treated the same

  /// The EQU model for any alphabet - all letters are treated the same
  /// This model has no parameters
  class EQU: public ReversibleMarkovModel,public ModelWithAlphabet<alphabet> {
    virtual void recalc();
  public:

    double fiddle(int) {return 1;}

    virtual EQU* clone() const {return new EQU(*this);}

    string name() const;
    string parameter_name(int i) const;

    /// Construct an EQU model on alphabet 'a'
    EQU(const alphabet& a)
      :ReversibleMarkovModel(a),ModelWithAlphabet<alphabet>(a)
    { recalc(); }

    virtual ~EQU() {}
  };

  //------------------ Empirical Substitution Model ---------------------//

  /// A generic model for any alphabet - the exchangeability matrix is loaded from a file
  class Empirical: public ReversibleMarkovModel,public ModelWithAlphabet<alphabet> {
    /// Specificy this model based on the matrix that was loaded
    string modelname;

    /// Load the exhangeability matrix from file 'filename'
    void load_file(const string& filename);

    virtual void recalc();
  public:
    double fiddle(int);

    virtual Empirical* clone() const {return new Empirical(*this);}

    string name() const;
    string parameter_name(int i) const;

    /// Construct an Empirical model on alphabet 'a' with matrix from 'filename'
    Empirical(const alphabet& a,const string& filename) 
      : ReversibleMarkovModel(a),ModelWithAlphabet<alphabet>(a)
    { 
      load_file(filename); 
      recalc();
    }

    virtual ~Empirical() {}
  };

  //------------------------------ HKY Substitution Model ----------------------------//

  class ReversibleMarkovNucleotideModel
    :public ReversibleMarkovModel,public ModelWithAlphabet<Nucleotides> {
  public:
    ReversibleMarkovNucleotideModel* clone() const=0;
    
    ReversibleMarkovNucleotideModel(const Nucleotides& N,int dp=0)
      :ReversibleMarkovModel(N,dp),
       ModelWithAlphabet<Nucleotides>(N)
    { }
  };

  /// The HKY substitution model for DNA or RNA

  /// The HKY substitution model for DNA or RNA.
  /// The only parameter in this model is 'kappa' - the transition/transversion ratio.
  class HKY: public ReversibleMarkovNucleotideModel
  {

    virtual void recalc();
  public:

    virtual HKY* clone() const {return new HKY(*this);}

    double fiddle(int); 

    /// Get the parameter 'kappa' (transition/transversion ratio)
    double kappa() const {return parameters_[1];}
    /// Set the parameter 'kappa' (transition/transversion ratio)
    void kappa(double k) {parameters_[1]=k;recalc();}

    virtual efloat_t prior() const;

    string name() const;
    string parameter_name(int i) const;

    /// Construct an HKY model on alphabet 'a'
    HKY(const Nucleotides& N)
      : ReversibleMarkovNucleotideModel(N,1)
    { 
      kappa(2);
    }
  };

  class TN: public ReversibleMarkovNucleotideModel {
    virtual void recalc();
  public:

    virtual TN* clone() const {return new TN(*this);}

    double fiddle(int); 

    /// Get the parameter 'kappa' (transition/transversion ratio)
    double kappa1() const {return parameters_[1];}
    /// Get the parameter 'kappa' (transition/transversion ratio)
    double kappa2() const {return parameters_[2];}

    /// Set the parameter 'kappa' (transition/transversion ratio)
    void kappa1(double k) {parameters_[1]=k;recalc();}
    /// Set the parameter 'kappa' (transition/transversion ratio)
    void kappa2(double k) {parameters_[2]=k;recalc();}

    virtual efloat_t prior() const;

    string name() const;
    string parameter_name(int i) const;

    /// Construct an HKY model on alphabet 'a'
    TN(const Nucleotides& N)
      : ReversibleMarkovNucleotideModel(N,2)
    { 
      kappa1(2);
      kappa2(2);
    }

  };

  //------------------------ Yang Codon Model -------------------------//
  class CodonModel: public ReversibleMarkovModel, public ModelWithAlphabet<Codons> 
  {
  public:
    CodonModel* clone() const=0;

    int AminoAcid(int l) const {return Alphabet().translate(l);}

    CodonModel(const Codons& C);

    ~CodonModel();
  };

  class YangM0: public CodonModel,
			public NestedModelOver<ReversibleMarkovNucleotideModel>
  {
    double super_fiddle(int);
    void recalc();
  public:
    efloat_t super_prior() const;

    efloat_t prior() const;

    YangM0* clone() const {return new YangM0(*this);}

    /// Get the parameter 'omega' (non-synonymous/synonymous rate ratio)
    double omega() const;
    /// Set the parameter 'omega' (non-synonymous/synonymous rate ratio)
    void omega(double w);

    string name() const;
    std::string super_parameter_name(int i) const;

    YangM0(const Codons& C,const ReversibleMarkovNucleotideModel& M);

    ~YangM0();
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

  /// A linear combination of reversible additive models w/ identical equilibria
  class MultiModel: public ReversibleAdditiveModel {
    //  protected:
    //    void recalc();
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

    efloat_t super_prior() const {return 1;}

    /// The number of models
    int n_base_models() const {return 1;}

    /// Access the base model
    const Base_Model_t& base_model(int m) const {return SubModel();}
          Base_Model_t& base_model(int m)       {return SubModel();}

    std::vector<double> distribution() const {return vector<double>(1,1);}

    string name() const;
    string super_parameter_name(int i) const;

    UnitModel(const Base_Model_t& M):
      ReversibleWrapperOver<Base_Model_t>(M,0)
    { }

    ~UnitModel() {};
  };

  /// A substitution model based on running Reversible Models with the same equilibrium at different rates
  class MultiFrequencyModel: public MultiModel, 
			     public ReversibleWrapperOver<MultiModel> 
  {
  protected:
    /// MultiModels w/ different values of the parameter set
    vector<OwnedPointer<MultiModel> > sub_parameter_models;

    /// Fraction of the sites with each parameter value
    vector<double> fraction;

    std::valarray<double> get_a(int m) const;
    void set_a(int m,const std::valarray<double>& f);
    
    void recalc();
    double super_fiddle(int);

    inline double& a(int m,int l)       {return super_parameters_[m+l*fraction.size()];}
    inline double  a(int m,int l) const {return super_parameters_[m+l*fraction.size()];}

  public:
    efloat_t super_prior() const;

    MultiFrequencyModel* clone() const {return new MultiFrequencyModel(*this);}

    /// The number of models
    int n_base_models() const {return fraction.size() * SubModel().n_base_models();}

    /// Access the base models
    const Base_Model_t& base_model(int m) const;
          Base_Model_t& base_model(int m)      ;

    /// Get the probability of each base models
    std::vector<double> distribution() const;

    /// Get the equilibrium frequencies
    const std::valarray<double>& frequencies() const;

    /// Set the equilibrium frequencies
    void frequencies(const std::valarray<double>&);
  
    string name() const;
    string super_parameter_name(int i) const;

    MultiFrequencyModel(const MultiModel& M,int n_bins);

    ~MultiFrequencyModel() {}
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

    void recalc();
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
    const std::valarray<double>& frequencies() const;

    /// Set the equilibrium frequencies
    void frequencies(const std::valarray<double>&);
  
    MultiParameterModel(const MultiModel& M,int dp,int p,int n_bins);

    ~MultiParameterModel() {}
  };


  class DistributionParameterModel: public MultiParameterModel
  {
  protected:
    OwnedPointer<RateDistribution> D;

    double super_fiddle(int);
    void recalc();

  public:

    efloat_t super_prior() const;

    DistributionParameterModel* clone() const {return new DistributionParameterModel(*this);}

    string name() const;
    string super_parameter_name(int i) const;

    DistributionParameterModel(const MultiModel& M, const RateDistribution& RD,int p,int n);
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
  class WithINV: public MultiModel, public ReversibleWrapperOver<MultiModel> {
    static double const inv_frac_mean = 0.1;
    static double const max_inv_rate = 0.01;

    OwnedPointer<INV_Model> INV;

  protected:

    double super_fiddle(int);

  public:
    virtual WithINV* clone() const {return new WithINV(*this);}

    void recalc();

    efloat_t super_prior() const;

    /// The number of models
    int n_base_models() const {return 1 +  SubModel().n_base_models();}

    /// Access the base models
    const Base_Model_t& base_model(int m) const;
          Base_Model_t& base_model(int m)      ;

    std::vector<double> distribution() const;

    /// Get the equilibrium frequencies
    const std::valarray<double>& frequencies() const;

    /// Set the equilibrium frequencies
    void frequencies(const std::valarray<double>&);
  
    string name() const;
    string super_parameter_name(int i) const;

    WithINV(const MultiModel& M);
  };

  class YangM2: public MultiParameterModel {
    double super_fiddle(int);
    void recalc();
  public:
    
    efloat_t super_prior() const;

    YangM2* clone() const {return new YangM2(*this);}

    string name() const;
    string super_parameter_name(int i) const;

    /// Get the probability of each base models
    std::vector<double> distribution() const;

    YangM2(const YangM0& M1);
  };


  class YangM3: public MultiParameterModel {
    double super_fiddle(int);
    void recalc();
  public:
    
    efloat_t super_prior() const;

    YangM3* clone() const {return new YangM3(*this);}

    string name() const;
    string super_parameter_name(int i) const;

    YangM3(const YangM0& M1);
  };


  class MixtureModel: public SuperDerivedModelOver<MultiModel,MultiModel> 
  {
    efloat_t super_prior() const;
    double super_fiddle(int);
    void recalc();

    std::valarray<double> pi;

  public:
    virtual MixtureModel* clone() const {return new MixtureModel(*this);}

    /// The number of models
    int n_base_models() const;

    const Base_Model_t& base_model(int m) const;
          Base_Model_t& base_model(int m);

    /// Get the equilibrium frequencies
    const std::valarray<double>& frequencies() const {return pi;}

    /// Set the equilibrium frequencies
    void frequencies(const std::valarray<double>& f);

    std::vector<double> distribution() const;

    string name() const;
    string super_parameter_name(int i) const;

    const alphabet& Alphabet() const {
      assert(SubModels(0).Alphabet() == SubModels(1).Alphabet());
      return SubModels(0).Alphabet();
    }

    MixtureModel(const std::vector<OwnedPointer<MultiModel> >& models);
  };

}
#endif


