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

    vector<bool> fixed;

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

    /// The prior on the current parameters
    virtual double prior() const {return 0.0;}

    /// Symmetrically propose another vector of parameters (calls recalc)
    virtual void fiddle() = 0; 

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

    /// Fiddle the parameters of the upper model
    virtual void super_fiddle() =0;

  public:
    /// Make a copy of ourselves
    NestedModel* clone() const =0;

    /// Access the submodel
    virtual const Model& SubModel() const=0;
    virtual Model& SubModel()=0;


    double prior() const {return super_prior() + SubModel().prior();}

    void fiddle() {
      SubModel().fiddle();
      for(int i=0;i<SubModel().parameters().size();i++)
	parameters_[i] = SubModel().parameters()[i];
      super_fiddle();
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

    /// The prior for the parameters of the upper model
    virtual double super_prior() const =0;

    /// Fiddle the parameters of the upper model
    virtual void super_fiddle() =0;

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
      fixed.resize(parameters_.size());

      for(int i=0;i<M.parameters().size();i++) {
	parameters_[i] = M.parameters()[i];
	fixed[i] = M.fixed[i];
      }

      full_tree = M.full_tree;
    }
  };
  // move 'fixed' to an argument of fiddle(), store 'fixed' in Parameters


  // This models a substitution process in which evolution is independant between
  // non-overlapping intervals.  We should have that transition_p(t/2)^2 = transition_p(t)
  // This model must also be reversible

  class ReversibleModel: virtual public Model {

  public:
    /// Make a copy of this object
    virtual ReversibleModel* clone() const =0;

    /// The transition probability matrix over time t
    virtual Matrix transition_p(double t) const =0;

    /// The equilibrium frequencies
    virtual const std::valarray<double>& frequencies() const=0;

    ReversibleModel() {}
  };


  class IndependantAdditiveModel: virtual public Model {

  public:
    virtual IndependantAdditiveModel* clone() const=0;

    /// The transition probability matrix
    virtual Matrix transition_p(double t) const =0;

    IndependantAdditiveModel() {}
  };

  class Independant_Additive_Reversible_Model: public ReversibleModel, 
					    public IndependantAdditiveModel
  {
  public:
    virtual Independant_Additive_Reversible_Model* clone() const=0;

    virtual Matrix transition_p(double) const=0;
  };


  //------------------------- Markov Substitution Model ----------------------------//

  /// A substitution model which obeys the Markov property
  class MarkovModel: virtual public Model {

  protected:

    /// The computed rate matrix
    Matrix Q;

  public:

    virtual MarkovModel* clone() const =0;

    /// The transition rate matrix
    virtual const Matrix& rates() const {return Q;}

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

  // exp(Qt) ->
  //   1. get Dt = D*exp(t)
  //   2. get O*Dt*O
  //   3. return DN*(O*Dt*O)*DP
  // Is this worth it?  Try it later...


  /// A Markov substitution model which is reversible
  class ReversibleMarkovModel: public MarkovModel,public ReversibleModel {

  protected:
    /// The equilibrium frequencies
    std::valarray<double> pi;

    /// The exchangeability matrix
    Matrix S;

    virtual void recalc();
  public:
    Matrix getD() const;

    const Matrix& getS() const {return S;}

    /// Make a copy of this object
    virtual ReversibleMarkovModel* clone() const =0;

    /// The transition probability matrix - which we can now compute
    Matrix transition_p(double t) const;

    /// Get the equilibrium frequencies
    const std::valarray<double>& frequencies() const {return pi;}

    /// Set the equilibrium frequencies
    virtual void frequencies(const std::valarray<double>& pi_) {pi=pi_; recalc();}
  
    string name() const;

    /// Construct a reversible Markov model on alphabet 'a_'
    ReversibleMarkovModel(const alphabet& a):
      MarkovModel(a), pi(1.0/a.size(),a.size()), 
      S(a.size(),a.size())
    { }

    ~ReversibleMarkovModel() {}
  };

    class Gamma_Branch_Model:public Independant_Additive_Reversible_Model,public NestedModelOver<ReversibleMarkovModel> 
    {
    public:
      virtual Gamma_Branch_Model* clone() const {return new Gamma_Branch_Model(*this);}
      
      void super_fiddle();
      double super_prior() const;
      string name() const;
      
      /// Get the equilibrium frequencies
      const std::valarray<double>& frequencies() const {return SubModel().frequencies();}
      
      Matrix transition_p(double t) const;
      Gamma_Branch_Model(const ReversibleMarkovModel& M)
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

    virtual void fiddle(); 

    /// Get the parameter 'kappa' (transition/transversion ratio)
    double kappa() const {return parameters_[0];}
    /// Set the parameter 'kappa' (transition/transversion ratio)
    void kappa(double k) {parameters_[0]=k;recalc();}

    virtual double prior() const;

    string name() const;

    /// Construct an HKY model on alphabet 'a'
    HKY(const Nucleotides& N)
      : Model(1),ReversibleMarkovModel(N),ModelWithAlphabet<Nucleotides>(N)
    { 
      kappa(1);
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
    virtual void fiddle() {}

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
    virtual void fiddle() {}

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
    void fiddle();

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

    /// Set the equilibrium frequencies
    void frequencies(const std::valarray<double>& pi_);
    
    /// Get the equilibrium frequencies
    const std::valarray<double>& frequencies() const;

    YangCodonModel(const Translation_Table& T1);

    ~YangCodonModel();
  };



  //----------------------- MultiRateModel ---------------------------*/

  /// A substitution model based on running a ReversibleModel at different rates
  class MultiRateModel: virtual public NestedModel {
  protected:
    /// The probability of each rate
    vector<double> distribution_;
    /// The different rates
    vector<double> rates_;

  public:
    MultiRateModel* clone() const =0;

    void recalc();

    /// The Reversible model which is run at different rates
    virtual const ReversibleModel& BaseModel() const =0;
    /// The Reversible model which is run at different rates
    virtual ReversibleModel& BaseModel() =0;
  
    /// The number of rates
    int nrates() const {return rates_.size();}

    /// Get the rates
    const vector<double>& rates() const {return rates_;}
    /// Get the probability of the rates
    const vector<double>& distribution() const {return distribution_;}

    /// Get a transition probability matrix for time 't' and rate 'r'
    Matrix transition_p(double t,int r) const {return BaseModel().transition_p(t*rates_[r]);}

    MultiRateModel(int nr):distribution_(nr),rates_(nr) {}

    ~MultiRateModel() {}
  };


  /// A substitution model based on running a Reversible model at different rates
  template<class SubModel_t>
  class MultiRateModelOver: public MultiRateModel, public NestedModelOver<SubModel_t> {

  public:
    virtual MultiRateModelOver<SubModel_t>* clone() const=0;

    const ReversibleModel& BaseModel() const {return SubModel().BaseModel();}
    ReversibleModel& BaseModel() {return SubModel().BaseModel();}

    MultiRateModelOver(const SubModel_t& M, int dp, int nr)
      :MultiRateModel(nr),
       NestedModelOver<SubModel_t>(M,dp)
    {}
  };


  /// A MultiRate Model which is directly over a ReversibleModel
  template<>
  class MultiRateModelOver<ReversibleModel>: public MultiRateModel, public NestedModelOver<ReversibleModel> {

  public:
    virtual MultiRateModelOver<ReversibleModel>* clone() const =0;

    const ReversibleModel& SubModel() const {return *sub_model;}
    ReversibleModel& SubModel() {return *sub_model;}

    const ReversibleModel& BaseModel() const {return SubModel();}
    ReversibleModel& BaseModel() {return SubModel();}

    MultiRateModelOver<ReversibleModel>(const ReversibleModel& M, int dp, int nr)
      :MultiRateModel(nr),
       NestedModelOver<ReversibleModel>(M,dp)
    {}

  };


  /// A MultiRate Model with a single rate
  class SingleRateModel: public MultiRateModelOver<ReversibleModel> {

  protected:
    double super_prior() const {return 0;}
    void super_fiddle() {}; 
  public:
    virtual SingleRateModel* clone() const {return new SingleRateModel(*this);}

    string name() const;

    SingleRateModel(const ReversibleModel& M):MultiRateModelOver<ReversibleModel>(M,0,1) {
      rates_[0] = 1.0;
      distribution_[0] = 1.0;
    }

    virtual ~SingleRateModel() {};
  };

  /// A discrete-gamma Model

  class DistributionRateModel:  public MultiRateModelOver<ReversibleModel> {
  protected:
    double super_prior() const;
    void super_fiddle();
    void recalc();

    RateDistribution* D;

  public:

    DistributionRateModel* clone() const {return new DistributionRateModel(*this);}

    const RateDistribution& distribution() const {return *D;}

    string name() const;
    
    DistributionRateModel& operator=(const DistributionRateModel&);

    DistributionRateModel(const DistributionRateModel&);

    DistributionRateModel(const ReversibleModel& M, const RateDistribution& RD,int n);
    ~DistributionRateModel();
  };

  /// A discrete-gamma Model
  //  Only has one parameter: the shape parameter for the gamma
  class GammaRateModel: public DistributionRateModel {
  public:
    virtual GammaRateModel* clone() const {return new GammaRateModel(*this);}

    string name() const;

    /// Construct a discrete gamma model on top of 'M', with 'n' bins
    GammaRateModel(const ReversibleModel& M,int n);
    virtual ~GammaRateModel() {}
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

    void super_fiddle();
    void recalc();
  public:
    virtual INV_Model* clone() const {return new INV_Model(*this);}

    // submodel is a multirate model
    //    const MultiRateModel& SubModel() const {return *(MultiRateModel*)sub_model;};
    //    MultiRateModel& SubModel() {return *(MultiRateModel*)sub_model;}

    //    const ReversibleModel& BaseModel() const {return SubModel().BaseModel();}
    //    ReversibleModel& BaseModel() {return SubModel().BaseModel();}

    string name() const;

    INV_Model(const MultiRateModel& M);
  };

}
#endif


