#ifndef PARAMETERS_H
#define PARAMETERS_H

#include "sequence.H"
#include "mytypes.H"
#include "tree.H"


/******************************* Abstract class Substitution Model ***********************/

class SubstitutionModel {
protected:
  const alphabet* a;

  vector<double> parameters_;
  vector<double> frequencies_;


  SubstitutionModel(const alphabet& a_,int s):
    a(&a_), parameters_(s),frequencies_(a->size(),1.0/a->size()) { }
public:
  const alphabet& Alphabet() const { return *a;}

  virtual void fiddle()=0;

  const vector<double>& parameters() const {return parameters_;}
  virtual void parameters(const vector<double>&) =0;

  const vector<double>& frequencies() const {return frequencies_;}
  virtual void frequencies(const vector<double>&) =0;
  
  virtual Matrix rates() const =0;
  virtual Matrix transition_p(double t) const =0;


  SubstitutionModel(const alphabet& a_):
    a(&a_), frequencies_(a->size(),1.0/a->size()) { }

  virtual ~SubstitutionModel() {}
};

/******************************* HKY Substitution Model *****************************/

class HKY: public SubstitutionModel {
  
  Matrix rates_;

  void recalc();
public:
  double alpha() const {return parameters_[0];}
  double alpha(double a) const {parameters_[0]=a;recalc()}
  double gamma() const {return parameters_[1];}
  double gamma(double g) const {parameters_[1]=g;recalc()}

  void fiddle();

  void parameters(const vector<double>& p) {parameters_=p; recalc();}
  void parameters(double a,double g) {parameters_[0]=a;parameters_[1]=g; recalc();}
  void frequencies(const vector<double>& f) {frequencies_=f; recalc();}

  Matrix rates() const {return rates_;}
  Matrix transition_p(double t) const;

  HKY(const alphabet& a) 
    : SubstitutionModel(a,2),rates_(4,4)
  { }

  HKY(const alphabet& a,double alpha,double gamma) 
    : SubstitutionModel(a,2),rates_(4,4)
  {
    parameters(alpha,gamma);
  }

  ~HKY() {}
};


/******************************* paramater-containing class **************************/

class Parameters {
  const alphabet* a;

  Matrix rates;

  vector<Matrix> substitution_;

  void recalc();
public:
  //FIXME - this should be private - need an interface
  // for changing branch lengths

  void setlength(int,double);

  const alphabet& get_alphabet() const {return *a;}
  void load_rates(const char*);

  const Matrix& substitution(int i) const {if (i==T.num_nodes()-2) i--;return substitution_[i];}
  
  SequenceTree T;

  double lambda_O;
  double lambda_E;

  // parameter for exponential prior on branch length
  double branch_mean; 

  std::valarray<double> frequency;

  Parameters(const alphabet& a_,const SequenceTree& t);
};

#endif
