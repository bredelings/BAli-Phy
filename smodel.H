#ifndef SMODEL_H
#define SMODEL_H

#include <valarray>
#include <vector>
#include "mytypes.H"
#include "alphabet.H"

/******************************* Abstract class Substitution Model ***********************/

class SubstitutionModel {
protected:
  alphabet a;

  // the current frequencies
  std::valarray<double> pi;

  // model parameters
  vector<double> parameters_;

  SubstitutionModel(const alphabet& a_,int s):a(a_), pi(1.0/a.size(),a.size()), parameters_(s) 
  { }

  virtual void recalc() =0;
public:
  const alphabet& Alphabet() const { return a;}

  virtual SubstitutionModel* clone() const =0;

  // get/set parameters
  const vector<double>& parameters() const {return parameters_;}
  virtual void parameters(const vector<double>& p) {parameters_=p; recalc();}

  // get/set frequencies
  const std::valarray<double>& frequencies() const {return pi;}
  virtual void frequencies(const std::valarray<double>& pi_) {pi=pi_; recalc();}
  
  virtual double prior() const {return 0.0;}

  // transition rates
  virtual const Matrix& rates() const =0;

  // transition probabilities
  virtual Matrix transition_p(double t) const =0;

  // how to sample from the distribution of parameters?
  virtual void fiddle() {}; 

  SubstitutionModel(const alphabet& a_): a(a_), pi(1.0/a.size(),a.size()) 
  { }

  virtual ~SubstitutionModel() {}
};

// FIXME - cache the eigensystem!

/************************** Equilibrium Substitution Model ************************/

class EquilibriumModel: public SubstitutionModel{
protected:
  // the exchangeability matrix
  Matrix S;

  // the computed rate matrix
  Matrix Q;

  EquilibriumModel(const alphabet& a_,int s):SubstitutionModel(a_,s),S(a.size(),a.size()), Q(a.size(),a.size())
  { }

  virtual void recalc();
public:

  // transition rates
  virtual const Matrix& rates() const {return Q;}

  // transition probabilities
  virtual Matrix transition_p(double t) const;

  EquilibriumModel(const alphabet& a_):SubstitutionModel(a_),S(a.size(),a.size()), Q(a.size(),a.size())
  { }

  virtual ~EquilibriumModel() {}
};




/******************************* HKY Substitution Model *****************************/

class HKY: public EquilibriumModel {
  virtual void recalc();

  int A;
  int T;
  int G;
  int C;

  void setup_alphabet();
public:

  virtual SubstitutionModel* clone() const {return new HKY(*this);}

  // change kappa
  virtual void fiddle(); 

  double kappa() const {return parameters_[0];}
  void kappa(double k) {parameters_[0]=k;recalc();}

  virtual double prior() const;

  HKY(const alphabet& a): EquilibriumModel(a,1)
  { 
    setup_alphabet();
    kappa(1);
  }

  HKY(const alphabet& a,double k): EquilibriumModel(a,1)
  {
    setup_alphabet();
    kappa(k);
  }

  HKY(const alphabet& a,double k,const std::valarray<double>& f): EquilibriumModel(a,1)
  {
    setup_alphabet();
    kappa(k);
    frequencies(f);
  }

  virtual ~HKY() {}
};

/******************************* EQU Substitution Model *****************************/

class EQU: public EquilibriumModel {
  virtual void recalc();
public:

  virtual SubstitutionModel* clone() const {return new EQU(*this);}

  EQU(const alphabet& a): EquilibriumModel(a) 
  { recalc(); }

  EQU(const alphabet& a,const std::valarray<double>& f)
    : EquilibriumModel(a)
  {
    frequencies(f);
  }

  virtual ~EQU() {}
};

/************************ Empirical Substitution Model **************************/

class Empirical: public EquilibriumModel {
  void load_file(const char* filename);
  virtual void recalc();
public:

  virtual SubstitutionModel* clone() const {return new Empirical(*this);}

  Empirical(const alphabet& a,const char* filename) 
    : EquilibriumModel(a)
  { load_file(filename); recalc();}

  Empirical(const alphabet& a,const char* filename,const std::valarray<double>& f)
    : EquilibriumModel(a)
  {
    load_file(filename);
    frequencies(f);
  }

  virtual ~Empirical() {}
};

#endif

