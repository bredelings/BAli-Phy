#ifndef SMODEL_H
#define SMODEL_H

#include <valarray>
#include <vector>
#include "mytypes.H"
#include "alphabet.H"

/******************************* Abstract class Substitution Model ***********************/
namespace substitution {

class Model {
protected:
  // model parameters
  vector<double> parameters_;

  Model(int s): parameters_(s) 
  { }

  virtual void recalc() =0;
public:
  virtual const alphabet& Alphabet() const =0;

  virtual Model* clone() const =0;

  // get/set parameters
  virtual const vector<double>& parameters() const {return parameters_;}
  virtual void parameters(const vector<double>& p) {parameters_=p; recalc();}

  // prior on the current parameters
  virtual double prior() const {return 0.0;}

  // how to sample from the distribution of parameters?
  virtual void fiddle() = 0; 

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

class MarkovModel: public Model {

protected:
  alphabet a;

  // the computed rate matrix
  Matrix Q;

  MarkovModel(const alphabet& a_,int s):
    Model(s),
    a(a_),
    Q(a.size(),a.size())
  {}
public:
  const alphabet& Alphabet() const {return a;}

  virtual MarkovModel* clone() const =0;

  // transition rates
  virtual const Matrix& rates() const {return Q;}

  // transition probabilities
  virtual Matrix transition_p(double t) const =0;

  MarkovModel(const alphabet& a_):
    a(a_),
    Q(a.size(),a.size())
  {}
};




/************************** Reversible Substitution Model ************************/

class ReversibleModel: public MarkovModel {

protected:
  // the current frequencies
  std::valarray<double> pi;

  // the exchangeability matrix
  Matrix S;

  ReversibleModel(const alphabet& a_,int s):
    MarkovModel(a_,s),
    pi(1.0/a_.size(),a_.size()), 
    S(a_.size(),a_.size())
  { }

  virtual void recalc();
public:

  // Now we know how to do transition probabilities
  Matrix transition_p(double t) const;

  // get/set frequencies
  const std::valarray<double>& frequencies() const {return pi;}
  virtual void frequencies(const std::valarray<double>& pi_) {pi=pi_; recalc();}
  
  ReversibleModel(const alphabet& a_):
    MarkovModel(a_), pi(1.0/a_.size(),a_.size()), 
    S(a_.size(),a_.size())
  { }

  virtual ~ReversibleModel() {}
};




/******************************* HKY Substitution Model *****************************/

class HKY: public ReversibleModel {
  virtual void recalc();

  int A;
  int T;
  int G;
  int C;

  void setup_alphabet();
public:

  virtual HKY* clone() const {return new HKY(*this);}

  // change kappa
  virtual void fiddle(); 

  double kappa() const {return parameters_[0];}
  void kappa(double k) {parameters_[0]=k;recalc();}

  virtual double prior() const;

  HKY(const alphabet& a): ReversibleModel(a,1)
  { 
    setup_alphabet();
    kappa(1);
  }

  HKY(const alphabet& a,double k): ReversibleModel(a,1)
  {
    setup_alphabet();
    kappa(k);
  }

  HKY(const alphabet& a,double k,const std::valarray<double>& f): ReversibleModel(a,1)
  {
    setup_alphabet();
    kappa(k);
    frequencies(f);
  }

  virtual ~HKY() {}
};

/******************************* EQU Substitution Model *****************************/

class EQU: public ReversibleModel {
  virtual void recalc();
public:
  virtual void fiddle() {}

  virtual EQU* clone() const {return new EQU(*this);}

  EQU(const alphabet& a): ReversibleModel(a) 
  { recalc(); }

  EQU(const alphabet& a,const std::valarray<double>& f)
    : ReversibleModel(a)
  {
    frequencies(f);
  }

  virtual ~EQU() {}
};

/************************ Empirical Substitution Model **************************/

class Empirical: public ReversibleModel {
  void load_file(const char* filename);
  virtual void recalc();
public:
  virtual void fiddle() {}

  virtual Empirical* clone() const {return new Empirical(*this);}

  Empirical(const alphabet& a,const char* filename) 
    : ReversibleModel(a)
  { load_file(filename); recalc();}

  Empirical(const alphabet& a,const char* filename,const std::valarray<double>& f)
    : ReversibleModel(a)
  {
    load_file(filename);
    frequencies(f);
  }

  virtual ~Empirical() {}
};

/*----------------------- MultiRateModel ---------------------------*/
class NestedModel: public Model {

protected:
  Model* sub_model;

  virtual double super_prior() const =0;
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

  // access the submodel
  virtual const Model& SubModel() const {return *sub_model;};
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
  NestedModel(const Model& M,int dp)
    :Model(M.parameters().size()+dp),sub_model(M.clone()) 
  {
    for(int i=0;i<M.parameters().size();i++)
      parameters_[i] = M.parameters()[i];
    
  }

  virtual ~NestedModel() {delete sub_model;}
};

class MultiRateModel: public NestedModel {

protected:
  vector<double> distribution_;
  vector<double> rates_;

public:
  MultiRateModel* clone() const =0;

  virtual const ReversibleModel& BaseModel() const =0;
  virtual ReversibleModel& BaseModel() =0;
  
  int nrates() const {return rates_.size();}
  const vector<double>& rates() const {return rates_;}
  const vector<double>& distribution() const {return distribution_;}

  Matrix transition_p(double t,int r) {return BaseModel().transition_p(t*rates_[r]);}

  MultiRateModel(const Model& M, int dp, int nr): 
    NestedModel(M,dp),distribution_(nr),rates_(nr) 
  {}

  virtual ~MultiRateModel() {}

};


class MultiRateOnReversible: public MultiRateModel {

protected:
  MultiRateOnReversible(const ReversibleModel& M,int dp,int nr)
    :MultiRateModel(M,dp,nr)
  {}

public:
  const ReversibleModel& SubModel() const {return *(ReversibleModel*)sub_model;}
  ReversibleModel& SubModel() {return *(ReversibleModel*)sub_model;}

  const ReversibleModel& BaseModel() const {return SubModel();}
  ReversibleModel& BaseModel() {return SubModel();}
};

class SingleRateModel: public MultiRateOnReversible {

protected:
  double super_prior() const {return 0;}
  void super_fiddle() {}; 
  void recalc() {};
public:
  virtual SingleRateModel* clone() const {return new SingleRateModel(*this);}

  SingleRateModel(const ReversibleModel& M):MultiRateOnReversible(M,0,1) {
    rates_[0] = 1.0;
    distribution_[0] = 1.0;
  }

  virtual ~SingleRateModel() {};
};

class GammaRateModel: public MultiRateOnReversible {
protected:
  double super_prior() const;
  void super_fiddle();
  void recalc();
public:
  virtual GammaRateModel* clone() const {return new GammaRateModel(*this);}

  GammaRateModel(const ReversibleModel& M,int n);
  virtual ~GammaRateModel() {}
};

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

  INV_Model(const MultiRateModel& M);
};

}
#endif


