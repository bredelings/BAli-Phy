#ifndef SMODEL_H
#define SMODEL_H

#include <valarray>
#include <vector>
#include <string>
#include "mytypes.H"
#include "alphabet.H"
#include "rates.H"
#include "clone.H"

namespace substitution {

  using std::string;

  //--------------------- Abstract base Model ----------------------///

  /// A generic substitution model
  class Model: public Cloneable {
  protected:
    /// Model parameters
    vector<double> parameters_;

  public:

    bool full_tree;
  protected:
    /// Recalculate internal data based on current parameters
    virtual void recalc() =0;

    /// Construct an empty model
    Model(int s);
  public:

    /// The alphabet we are modelling substitution on
    virtual const alphabet& Alphabet() const =0;

    /// Make a copy of this object
    virtual Model* clone() const =0;

    /// Get the model parameters
    virtual const vector<double>& parameters() const {return parameters_;}

    /// Set the model parameters
    virtual void parameters(const vector<double>& p) {
      assert(parameters_.size() == p.size()) ; 
      parameters_=p; 
      recalc();
    }

    virtual std::string parameter_name(int i) const;

    /// The prior on the current parameters
    virtual double prior() const {return 0;}

    /// Symmetrically propose another vector of parameters (calls recalc)
    virtual void fiddle(const std::valarray<bool>&) = 0; 

    /// The model's name
    virtual string name() const =0;

    /// Construct an empty model
    Model();

    virtual ~Model() {}
  };


  //----------------------- Nested Models ------------------------///

  /// Abstract class for substitution models built on a submodel
  class NestedModel: virtual public Model {

  protected:
    /// The prior for the parameters of the upper model
    virtual double super_prior() const =0;

    /// Fiddle the parameters of the upper model (may need to call recalc)
    virtual void super_fiddle(const std::valarray<bool>&) {recalc();}

    virtual std::string super_parameter_name(int i) const {return Model::parameter_name(i);}

  public:
    /// Make a copy of ourselves
    NestedModel* clone() const =0;

    /// Access the submodel
    virtual const Model& SubModel() const=0;
    virtual Model& SubModel()=0;

    std::string parameter_name(int i) const;

    double prior() const {return super_prior() + SubModel().prior();}

    /// Symmetrically propose parameters (calls recalc through super_fiddle if necessary)
    void fiddle(const std::valarray<bool>& fixed) {
      SubModel().fiddle(fixed);
      for(int i=0;i<SubModel().parameters().size();i++)
	parameters_[i] = SubModel().parameters()[i];
      super_fiddle(fixed);
    }
    
    /// We have the same alphabet as our submodel
    const alphabet& Alphabet() const {return SubModel().Alphabet();}

    void recalc();
  };


  /// This is a substitution model built over a submodel of a particular type
  template <class SubModel_t>
  class NestedModelOver: virtual public NestedModel {

  protected:
    /// The sub-model this model is build upon
    OwnedPointer<SubModel_t> sub_model;

  public:
    /// Make a copy of ourselves
    NestedModelOver<SubModel_t>* clone() const =0;

    /// Reference the submodel
    virtual const SubModel_t& SubModel() const {return *sub_model;};
    virtual SubModel_t& SubModel() {return *sub_model;}

    /// Construct a Nested model based on model 'M', with 'dp' extra parameters
    NestedModelOver(const SubModel_t& M,int dp)
      :sub_model(M.clone()) 
    {
      parameters_.resize(M.parameters().size()+dp);

      for(int i=0;i<M.parameters().size();i++)
	parameters_[i] = M.parameters()[i];

      full_tree = M.full_tree;
    }
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


  class ReversibleAdditiveModel: public ReversibleModel, virtual public AdditiveModel {
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

    string name() const;

    /// Construct a Makov model on alphabet 'a_'
    MarkovModel(const alphabet& a)
      :Q(a.size(),a.size())
    {}
  };



  //------------------------- Reversible Substitution Model -----------------------//

  // FIXME - cache the eigensystem!
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

    virtual void recalc();
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
  
    string name() const;

    double prior() const;

    /// Construct a reversible Markov model on alphabet 'a_'
    ReversibleMarkovModel(const alphabet& a):
      MarkovModel(a), pi(1.0/a.size(),a.size()), 
      S(a.size(),a.size())
    { }

    ~ReversibleMarkovModel() {}
  };

  // Perhaps this is just a REVERSIBLE Model?
  class Gamma_Branch_Model:public Independant_Additive_Reversible_Model,public NestedModelOver<ReversibleMarkovModel> 
  {
    void super_fiddle(const std::valarray<bool>&);
    double super_prior() const;
  public:
    virtual Gamma_Branch_Model* clone() const {return new Gamma_Branch_Model(*this);}
    
    double rate() const {return SubModel().rate();}
    void set_rate(double r) {SubModel().set_rate(r);}
    
    string name() const;
      
    /// Get the equilibrium frequencies
    const std::valarray<double>& frequencies() const {return SubModel().frequencies();}
    /// Set the equilibrium frequencies
    void frequencies(const std::valarray<double>& f) {SubModel().frequencies(f);}

    Matrix transition_p(double t) const;
    Gamma_Branch_Model(const ReversibleMarkovModel& M)
      :NestedModelOver<ReversibleMarkovModel>(M,1) {
      parameters_[parameters_.size()-1]=0.1;
    }
  };



  class Gamma_Stretched_Branch_Model:public Independant_Additive_Reversible_Model,public NestedModelOver<ReversibleMarkovModel> 
  {
    void super_fiddle(const std::valarray<bool>&);
    double super_prior() const;
  public:
    virtual Gamma_Stretched_Branch_Model* clone() const {return new Gamma_Stretched_Branch_Model(*this);}
    
    double rate() const {return SubModel().rate();}
    void set_rate(double r) {SubModel().set_rate(r);}
    
    string name() const;
    
    /// Get the equilibrium frequencies
    const std::valarray<double>& frequencies() const {return SubModel().frequencies();}
    
    /// Set the equilibrium frequencies
    void frequencies(const std::valarray<double>& f) {SubModel().frequencies(f);}

    Matrix transition_p(double t) const;
    Gamma_Stretched_Branch_Model(const ReversibleMarkovModel& M)
      :NestedModelOver<ReversibleMarkovModel>(M,1) {
      parameters_[parameters_.size()-1]=0.1;
    }
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


  //------------------------------ HKY Substitution Model ---------------------------**/

  /// The HKY substitution model for DNA or RNA

  /// The HKY substitution model for DNA or RNA.
  /// The only parameter in this model is 'kappa' - the transition/transversion ratio.
  class HKY: public ReversibleMarkovModel,public ModelWithAlphabet<Nucleotides> {

    virtual void recalc();
  public:

    virtual HKY* clone() const {return new HKY(*this);}

    virtual void fiddle(const std::valarray<bool>&); 

    /// Get the parameter 'kappa' (transition/transversion ratio)
    double kappa() const {return parameters_[0];}
    /// Set the parameter 'kappa' (transition/transversion ratio)
    void kappa(double k) {parameters_[0]=k;recalc();}

    string parameter_name(int i) const;

    virtual double prior() const;

    string name() const;

    /// Construct an HKY model on alphabet 'a'
    HKY(const Nucleotides& N)
      : Model(1),ReversibleMarkovModel(N),ModelWithAlphabet<Nucleotides>(N)
    { 
      kappa(2);
    }

    /// Construct an HKY model on alphabet 'a', w/ kappa=k
    HKY(const Nucleotides& N,double k)
      : Model(1),ReversibleMarkovModel(N),ModelWithAlphabet<Nucleotides>(N)
    {
      kappa(k);
    }
  };

  //--------------------- EQU Substitution Model ------------------------/

  /// The EQU model for any alphabet - all letters are treated the same

  /// The EQU model for any alphabet - all letters are treated the same
  /// This model has no parameters
  class EQU: public ReversibleMarkovModel,public ModelWithAlphabet<alphabet> {
    virtual void recalc();
  public:
    virtual void fiddle(const std::valarray<bool>&) {}

    virtual EQU* clone() const {return new EQU(*this);}

    string name() const;

    /// Construct an HKY model on alphabet 'a'
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
    virtual void fiddle(const std::valarray<bool>&) {}

    virtual Empirical* clone() const {return new Empirical(*this);}

    string name() const;

    /// Construct an HKY model on alphabet 'a' with matrix from 'filename'
    Empirical(const alphabet& a,const string& filename) 
      : ReversibleMarkovModel(a),ModelWithAlphabet<alphabet>(a)
    { 
      load_file(filename); 
      recalc();
    }

    virtual ~Empirical() {}
  };


  //------------------------ Yang Codon Model -------------------------//
  class YangCodonModel: public ReversibleMarkovModel,public ModelWithAlphabet<Codons> {
    void recalc();

    Translation_Table T;

  public:
    void fiddle(const std::valarray<bool>&);

    YangCodonModel* clone() const {return new YangCodonModel(*this);}

    string name() const;

    int AminoAcid(int i) const {
      return T[i];
    }

    double prior() const;

    /// Get the parameter 'kappa' (transition/transversion rate ratio)
    double kappa() const {return parameters_[0];}
    /// Set the parameter 'kappa' (transition/transversion rate ratio)
    void kappa(double k) {parameters_[0]=k;recalc();}

    /// Get the parameter 'omega' (non-synonymous/synonymous rate ratio)
    double omega() const {return parameters_[1];}
    /// Set the parameter 'omega' (non-synonymous/synonymous rate ratio)
    void omega(double w) {parameters_[1]=w;recalc();}

    std::string parameter_name(int i) const;

    /// Set the equilibrium frequencies
    void frequencies(const std::valarray<double>& pi_);
    
    /// Get the equilibrium frequencies
    const std::valarray<double>& frequencies() const;

    YangCodonModel(const Translation_Table& T1);

    ~YangCodonModel();
  };


  //FIXME - we are generalizing this to 
  // include a linear combination of reversible models with the same
  // alphabet and equilibrium frequencies

  // For now, we assume that all models can be generated by one base model...

  //----------------------- MultiModel ---------------------------*/

  /// A linear combination of reversible additive models identical equilibria

  class MultiModel: public ReversibleAdditiveModel {
  protected:
    /// The probability of each model
    vector<double> distribution_;

    /// The probability of each model
    vector<double> rates_;
  public:
    MultiModel* clone() const =0;

    void recalc();

    /// The number of models
    int nmodels() const {return distribution_.size();}

    virtual const ReversibleAdditiveModel& get_model(int i) const=0;

    /// Get the rates at which we run the individual models
    const vector<double>& rates() const {return rates_;}

    double rate() const;
    void set_rate(double);

    /// Get the probability of the models
    const vector<double>& distribution() const {return distribution_;}

    /// Get a transition probability matrix for time 't', averaging over models
    virtual Matrix transition_p(double t) const;

    /// Get a transition probability matrix for time 't' and model 'm'
    virtual Matrix transition_p(double t,int m) const {return get_model(m).transition_p(t*rates_[m]); }

    MultiModel(int n_models):distribution_(n_models),rates_(n_models) {}

    ~MultiModel() {}
  };


  /// A substitution model based on running Reversible Models with the same equilibrium at different rates
  class MultiRateModel: public MultiModel, virtual public NestedModel {
  public:
    MultiRateModel* clone() const =0;

    void recalc();

    /// The Reversible model which is run at different rates
    virtual const ReversibleAdditiveModel& BaseModel() const =0;
    /// The Reversible model which is run at different rates
    virtual ReversibleAdditiveModel& BaseModel() =0;
  
    double rate() const;
    void set_rate(double);
    const ReversibleAdditiveModel& get_model(int i) const;

    /// The number of rates
    int nrates() const {return nmodels();}

    /// Get the equilibrium frequencies
    const std::valarray<double>& frequencies() const {return BaseModel().frequencies();}
    /// Set the equilibrium frequencies
    void frequencies(const std::valarray<double>& f) {BaseModel().frequencies(f);recalc();}

    /// Get a transition probability matrix for time 't', averaging over models
    Matrix transition_p(double t) const { return MultiModel::transition_p(t);}

    /// Get a transition probability matrix for time 't' and rate 'r'
    Matrix transition_p(double t,int r) const { return BaseModel().transition_p(t*rates_[r]); }

    MultiRateModel(int nr):MultiModel(nr) {}

    ~MultiRateModel() {}
  };


  /// A substitution model based on running a Reversible model at different rates
  template<class SubModel_t>
  class MultiRateModelOver: public MultiRateModel, public NestedModelOver<SubModel_t> {

  public:
    virtual MultiRateModelOver<SubModel_t>* clone() const=0;

    const ReversibleAdditiveModel& BaseModel() const {return SubModel().BaseModel();}
    ReversibleAdditiveModel& BaseModel() {return SubModel().BaseModel();}

    MultiRateModelOver(const SubModel_t& M, int dp, int nr)
      :MultiRateModel(nr),
       NestedModelOver<SubModel_t>(M,dp)
    { recalc(); }
  };


  /// A MultiRate Model which is directly over a ReversibleModel
  template<>
  class MultiRateModelOver<ReversibleAdditiveModel>: public MultiRateModel, public NestedModelOver<ReversibleAdditiveModel> {

  public:
    virtual MultiRateModelOver<ReversibleAdditiveModel>* clone() const =0;

    const ReversibleAdditiveModel& SubModel() const {return *sub_model;}
    ReversibleAdditiveModel& SubModel() {return *sub_model;}

    const ReversibleAdditiveModel& BaseModel() const {return SubModel();}
    ReversibleAdditiveModel& BaseModel() {return SubModel();}

    MultiRateModelOver<ReversibleAdditiveModel>(const ReversibleAdditiveModel& M, int dp, int nr)
      :MultiRateModel(nr),
       NestedModelOver<ReversibleAdditiveModel>(M,dp)
    {}

  };


  /// A MultiRate Model with a single rate
  class SingleRateModel: public MultiRateModelOver<ReversibleAdditiveModel> {

  protected:
    double super_prior() const {return 0;}
  public:
    virtual SingleRateModel* clone() const {return new SingleRateModel(*this);}

    string name() const;

    SingleRateModel(const ReversibleAdditiveModel& M):MultiRateModelOver<ReversibleAdditiveModel>(M,0,1) {
      rates_[0] = 1.0;
      distribution_[0] = 1.0;
    }

    virtual ~SingleRateModel() {};
  };

  /// A discrete-gamma Model

  class DistributionRateModel:  public MultiRateModelOver<ReversibleAdditiveModel> {
  protected:
    double super_prior() const;
    void super_fiddle(const std::valarray<bool>&);
    void recalc();

    OwnedPointer<RateDistribution> D;

  public:

    DistributionRateModel* clone() const {return new DistributionRateModel(*this);}

    const RateDistribution& distribution() const {return *D;}

    string name() const;

    DistributionRateModel(const ReversibleAdditiveModel& M, const RateDistribution& RD,int n);
  };

  /// A discrete-gamma Model
  //  Only has one parameter: the shape parameter for the gamma
  class GammaRateModel: public DistributionRateModel {
    string super_parameter_name(int i) const;

  public:
    virtual GammaRateModel* clone() const {return new GammaRateModel(*this);}

    string name() const;

    /// Construct a discrete gamma model on top of 'M', with 'n' bins
    GammaRateModel(const ReversibleAdditiveModel& M,int n);
  };

  /// A discrete-lognormal Model
  //  Only has one parameter: the shape parameter for the gamma
  class LogNormalRateModel: public DistributionRateModel {
  public:
    virtual LogNormalRateModel* clone() const {return new LogNormalRateModel(*this);}

    string name() const;

    /// Construct a discrete gamma model on top of 'M', with 'n' bins
    LogNormalRateModel(const ReversibleAdditiveModel& M,int n);
  };



  /// The Invariant-sites model: a certain fraction of the sites don't change

  /// The Invariant-sites model: a certain fraction of the sites don't change
  /// The only parameter is 'p', the fraction of sites which don't change
  /// To keep branch lenghts the same, inflate rates by 1/(1-p).
  class INV_Model: public MultiRateModelOver<MultiRateModel> {
    static double const inv_frac_mean = 0.1;
    static double const max_inv_rate = 0.01;

  protected:
    double super_prior() const;

    void super_fiddle(const std::valarray<bool>&);

    string super_parameter_name(int i) const;

    void recalc();
  public:
    virtual INV_Model* clone() const {return new INV_Model(*this);}

    string name() const;

    INV_Model(const MultiRateModel& M);
  };

}
#endif


