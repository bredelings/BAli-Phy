#ifndef SMODEL_H
#define SMODEL_H

#include <valarray>
#include <vector>
#include "mytypes.H"
#include "alphabet.H"

/************ Abstract class Substitution Model **************/
namespace substitution {

/// A generic substitution model
class Model {
protected:
  /// Model parameters
  vector<double> parameters_;

public:

  bool full_tree;
protected:
  /// Construct a substitution model with 's' parameters
  Model(int s): parameters_(s),full_tree(true)
  { }

  /// Recalculate internal data based on current parameters
  virtual void recalc() =0;
public:

  /// The alphabet we are modelling substitution on
  virtual const alphabet& Alphabet() const =0;

  /// Make a copy of this object
  virtual Model* clone() const =0;

  /// Get the model parameters
  virtual const vector<double>& parameters() const {return parameters_;}

  /// Set the model parameters
  virtual void parameters(const vector<double>& p) {parameters_=p; recalc();}

  /// The prior on the current parameters
  virtual double prior() const {return 0.0;}

  /// Symmetrically propose another vector of parameters
  virtual void fiddle() = 0; 

  /// The model's name
  virtual string name() const =0;

  /// Construct an empty model
  Model();

  virtual ~Model() {}
};

// FIXME - cache the eigensystem!
// Store S2 = pi^0.5 * S * pi^0.5
// Store EigenValues(S2) -> D,O
// exp(Qt) ->
//   1. get Dt = D*exp(t)
//   2. get O*Dt*O
//   3. return DN*(O*Dt*O)*DP
// Is this worth it?  Try it later...

/*------------------------- Markov Substitution Model ----------------------------*/

/// A substitution model which obeys the Markov property
class MarkovModel: public Model {

protected:
  /// The alphabet for this model
  alphabet a;

  /// The computed rate matrix
  Matrix Q;

  /// Construct a Markov model on alphabet 'a_', and 's' parameters
  MarkovModel(const alphabet& a_,int s):
    Model(s),
    a(a_),
    Q(a.size(),a.size())
  {}
public:
  const alphabet& Alphabet() const {return a;}

  virtual MarkovModel* clone() const =0;

  /// The transition rate matrix
  virtual const Matrix& rates() const {return Q;}

  /// The transition probability matrix
  virtual Matrix transition_p(double t) const =0;

  string name() const;

  /// Construct a Makov model on alphabet 'a_'
  MarkovModel(const alphabet& a_):
    a(a_),
    Q(a.size(),a.size())
  {}
};




/************************** Reversible Substitution Model ************************/

/// A Markov substitution model which is reversible
class ReversibleModel: public MarkovModel {

protected:
  /// The equilibrium frequencies
  std::valarray<double> pi;

  /// The exchangeability matrix
  Matrix S;

  /// Construct a reversible Markov model on alphabet 'a_', and 's' parameters
  ReversibleModel(const alphabet& a_,int s):
    MarkovModel(a_,s),
    pi(1.0/a_.size(),a_.size()), 
    S(a_.size(),a_.size())
  { }

  virtual void recalc();
public:

  /// The transition probability matrix - which we can now compute
  Matrix transition_p(double t) const;

  /// Get the equilibrium frequencies
  const std::valarray<double>& frequencies() const {return pi;}
  /// Set the equilibrium frequencies
  virtual void frequencies(const std::valarray<double>& pi_) {pi=pi_; recalc();}
  
  string name() const;

  /// Construct a reversible Markov model on alphabet 'a_'
  ReversibleModel(const alphabet& a_):
    MarkovModel(a_), pi(1.0/a_.size(),a_.size()), 
    S(a_.size(),a_.size())
  { }

  virtual ~ReversibleModel() {}
};




/******************************* HKY Substitution Model *****************************/

/// The HKY substitution model for DNA or RNA

/// The HKY substitution model for DNA or RNA.
/// The only parameter in this model is 'kappa' - the transition/transversion ratio.
class HKY: public ReversibleModel {
  virtual void recalc();

  int A;
  int T;
  int G;
  int C;

  /// Figure out the indices for A,G,C and T/U
  void setup_alphabet();
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
  HKY(const alphabet& a): ReversibleModel(a,1)
  { 
    setup_alphabet();
    kappa(1);
  }

  /// Construct an HKY model on alphabet 'a', w/ kappa=k
  HKY(const alphabet& a,double k): ReversibleModel(a,1)
  {
    setup_alphabet();
    kappa(k);
  }

  /// Construct an HKY model on alphabet 'a', w/ kappa=k, and frequencies 'f'
  HKY(const alphabet& a,double k,const std::valarray<double>& f): ReversibleModel(a,1)
  {
    setup_alphabet();
    kappa(k);
    frequencies(f);
  }

  virtual ~HKY() {}
};

/********************** EQU Substitution Model ************************/

/// The EQU model for any alphabet - all letters are treated the same

/// The EQU model for any alphabet - all letters are treated the same
/// This model has no parameters
class EQU: public ReversibleModel {
  virtual void recalc();
public:
  virtual void fiddle() {}

  virtual EQU* clone() const {return new EQU(*this);}

  string name() const;

  /// Construct an HKY model on alphabet 'a'
  EQU(const alphabet& a): ReversibleModel(a) 
  { recalc(); }

  /// Construct an HKY model on alphabet 'a', w/ frequencies 'f'
  EQU(const alphabet& a,const std::valarray<double>& f)
    : ReversibleModel(a)
  {
    frequencies(f);
  }

  virtual ~EQU() {}
};

/******************* Empirical Substitution Model *********************/

/// A generic model for any alphabet - the exchangeability matrix is loaded from a file
class Empirical: public ReversibleModel {
  /// Specificy this model based on the matrix that was loaded
  string modelname;

  /// Load the exhangeability matrix from file 'filename'
  void load_file(const char* filename);

  virtual void recalc();
public:
  virtual void fiddle() {}

  virtual Empirical* clone() const {return new Empirical(*this);}

  string name() const;

  /// Construct an HKY model on alphabet 'a' with matrix from 'filename'
  Empirical(const alphabet& a,const char* filename) 
    : ReversibleModel(a)
  { load_file(filename); recalc();}

  /// Construct an HKY model on alphabet 'a' with matrix from 'filename', and frequencies 'f'
  Empirical(const alphabet& a,const char* filename,const std::valarray<double>& f)
    : ReversibleModel(a)
  {
    load_file(filename);
    frequencies(f);
  }

  virtual ~Empirical() {}
};

/*----------------------- MultiRateModel ---------------------------*/

/// This is a generic substitution model which is built on a submodel
class NestedModel: public Model {

protected:
  /// The sub-model this model is build upon
  Model* sub_model;

  /// The prior for the parameters of the upper model
  virtual double super_prior() const =0;

  /// Fiddle the parameters of the upper model
  virtual void super_fiddle() =0;

public:
  NestedModel* clone() const =0;

  double prior() const {return super_prior() + sub_model->prior();}

  void fiddle() {
    sub_model->fiddle();
    for(int i=0;i<sub_model->parameters().size();i++)
      parameters_[i] = sub_model->parameters()[i];
    super_fiddle();
  }

  /// Reference the submodel
  virtual const Model& SubModel() const {return *sub_model;};
  /// Reference the submodel
  virtual Model& SubModel() {return *sub_model;}

  // we have the same alphabet as submodel
  const alphabet& Alphabet() const {return sub_model->Alphabet();}

  // when copying, get our own copy of the submodel
  NestedModel& operator=(const NestedModel& M) {
    Model::operator=(M);

    assert(sub_model);
    delete sub_model;
    sub_model = M.sub_model->clone();

    return (*this);
  }

  // set parameters
  virtual const vector<double>& parameters() const {return parameters_;}
  virtual void parameters(const vector<double>& p);

  // when copying, get our own copy of the submodel
  NestedModel(const NestedModel& M):Model(M),sub_model(M.sub_model->clone()) 
  {}


  /*------------ Construction and Destruction ---------------*/
  /// Construct a Nested model based on model 'M', with 'dp' extra parameters
  NestedModel(const Model& M,int dp)
    :Model(M.parameters().size()+dp),sub_model(M.clone()) 
  {
    for(int i=0;i<M.parameters().size();i++)
      parameters_[i] = M.parameters()[i];

    full_tree = M.full_tree;
  }

  virtual ~NestedModel() {delete sub_model;}
};

/// A substitution model based on running a Reversible model at different rates
class MultiRateModel: public NestedModel {

protected:
  /// The probability of each rate
  vector<double> distribution_;
  /// The different rates
  vector<double> rates_;

public:
  MultiRateModel* clone() const =0;

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
  Matrix transition_p(double t,int r) {return BaseModel().transition_p(t*rates_[r]);}

  /// Construct a MultiRate model based on 'M', with 'dp' extra parameters, and 'nr' rates
  MultiRateModel(const Model& M, int dp, int nr): 
    NestedModel(M,dp),distribution_(nr),rates_(nr) 
  {}

  virtual ~MultiRateModel() {}

};


/// A MultiRate Model which is directly over a ReversibleModel
class MultiRateOnReversible: public MultiRateModel {

protected:
  /// Construct a MultiRate model based on 'M', with 'dp' extra parameters, and 'nr' rates
  MultiRateOnReversible(const ReversibleModel& M,int dp,int nr)
    :MultiRateModel(M,dp,nr)
  {}

public:
  const ReversibleModel& SubModel() const {return *(ReversibleModel*)sub_model;}
  ReversibleModel& SubModel() {return *(ReversibleModel*)sub_model;}

  const ReversibleModel& BaseModel() const {return SubModel();}
  ReversibleModel& BaseModel() {return SubModel();}
};


/// A MultiRate Model with a single rate
class SingleRateModel: public MultiRateOnReversible {

protected:
  double super_prior() const {return 0;}
  void super_fiddle() {}; 
  void recalc() {};
public:
  virtual SingleRateModel* clone() const {return new SingleRateModel(*this);}

  string name() const;

  SingleRateModel(const ReversibleModel& M):MultiRateOnReversible(M,0,1) {
    rates_[0] = 1.0;
    distribution_[0] = 1.0;
  }

  virtual ~SingleRateModel() {};
};

/// A discrete-gamma Model

/// A discrete-gamma Model
//  Only has one parameter: the shape parameter for the gamma
class GammaRateModel: public MultiRateOnReversible {
protected:
  double super_prior() const;
  void super_fiddle();
  void recalc();
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
/// The expected number of changes per site is now (1-p), instead of 1.0
class INV_Model: public MultiRateModel {
protected:
  double super_prior() const {
    double p = parameters()[parameters().size()-1];
    return p/-0.1;
  }

  void super_fiddle();
  void recalc();
public:
  virtual INV_Model* clone() const {return new INV_Model(*this);}

  // submodel is a multirate model
  const MultiRateModel& SubModel() const {return *(MultiRateModel*)sub_model;};
  MultiRateModel& SubModel() {return *(MultiRateModel*)sub_model;}

  const ReversibleModel& BaseModel() const {return SubModel().BaseModel();}
  ReversibleModel& BaseModel() {return SubModel().BaseModel();}

  string name() const;

  INV_Model(const MultiRateModel& M);
};

}
#endif


