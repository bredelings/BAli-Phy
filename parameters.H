#ifndef PARAMETERS_H
#define PARAMETERS_H

#include "sequence.H"
#include "mytypes.H"
#include "tree.H"
#include <valarray>


/******************************* Abstract class Substitution Model ***********************/

class SubstitutionModel {
protected:
  const alphabet * const a;

  vector<double> parameters_;
  std::valarray<double> frequencies_;


  SubstitutionModel(const alphabet& a_,int s):
    a(&a_), parameters_(s),frequencies_(1.0/a->size(),a->size()) { }
public:
  const alphabet& Alphabet() const { return *a;}

  virtual void fiddle()=0;

  const vector<double>& parameters() const {return parameters_;}
  virtual void parameters(const vector<double>&) =0;

  const std::valarray<double>& frequencies() const {return frequencies_;}
  virtual void frequencies(const std::valarray<double>&) =0;
  
  virtual Matrix rates() const =0;
  virtual Matrix transition_p(double t) const =0;

  SubstitutionModel(const alphabet& a_):
    a(&a_), frequencies_(1.0/a->size(),a->size()) { }

  virtual ~SubstitutionModel() {}
};

// FIXME - cache the eigensystem!

/******************************* HKY Substitution Model *****************************/

class HKY: public SubstitutionModel {
  
  Matrix rates_;

  void recalc();
public:
  double kappa() const {return parameters_[0];}
  void kappa(double k) {parameters_[0]=k;recalc();}

  void fiddle();

  void parameters(const vector<double>& p) {parameters_=p; recalc();}
  void frequencies(const std::valarray<double>& f) {frequencies_=f; recalc();}

  Matrix rates() const {return rates_;}
  Matrix transition_p(double t) const;

  HKY(const alphabet& a) 
    : SubstitutionModel(a,1),rates_(4,4)
  { 
    kappa(1);
  }

  HKY(const alphabet& a,double k)
    : SubstitutionModel(a,1),rates_(4,4)
  {
    kappa(k);
  }

  HKY(const alphabet& a,double k,const std::valarray<double>& f)
    : SubstitutionModel(a,1),rates_(4,4)
  {
    kappa(k);
    frequencies(f);
  }

  ~HKY() {}
};


class IndelModel {
  std::vector<double> p_length;
  
  void construct_lengthp(int);
public:
  double lambda_O;
  double lambda_E;

  vector<double> pi;
  ublas::matrix<double> P;
  ublas::matrix<double> Q;
  ublas::matrix<double> R;

  double tau;     //probability of going -> E
  double delta;   //probability of starting a gap: M -> G1,G2
  double epsilon; //probability of extending a gap: G1->G1,  G2->G2

  double lengthp(int i) const {return p_length[i];}

  IndelModel(double LO,double LE);
};



/******************************* paramater-containing class **************************/

class Parameters {
  vector<Matrix> substitution_;

  void recalc();
public:
  //FIXME - this should be private - need an interface for changing branch lengths (?)

  /* Stuff for Substitution Model */
  SubstitutionModel const* SModel;
  const alphabet& get_alphabet() const {return SModel->Alphabet();}
  const std::valarray<double>& frequencies() const {return SModel->frequencies();}
  const Matrix& substitution(int i) const {if (i==T.num_nodes()-2) i--;return substitution_[i];}
  void setlength(int,double);
  
  /* Indel Model */
  IndelModel IModel;

  SequenceTree T;


  // parameter for exponential prior on branch length
  double branch_mean; 

  Parameters(SubstitutionModel&,double,double,const SequenceTree&);
};

#endif
