#ifndef PARAMETERS_H
#define PARAMETERS_H

#include "alphabet.H"
#include "mytypes.H"
#include "tree.H"
#include <valarray>
#include "smodel.H"

/******************************* InDel Models ***********************************/

class IndelModel {
protected:
  std::vector<double> p_length;
  
  void construct_lengthp(int);
  ublas::matrix<double> P;
  ublas::matrix<double> R;

public:
  double lambda_O;
  double lambda_E;

  vector<double> pi;
  ublas::matrix<double> Q;

  double lengthp(int i) const {return p_length[i];}

  IndelModel();
};

class IndelModel1 : public IndelModel {
public:
  double tau;     //probability of going -> E
  double delta;   //probability of starting a gap: M -> G1,G2
  double epsilon; //probability of extending a gap: G1->G1,  G2->G2

  explicit IndelModel1(int maxlength,double LO,double LE);
};

class IndelModel2 : public IndelModel {
public:
  double tau;     //probability of going -> E
  double delta;   //probability of starting a gap: M -> G1,G2
  double epsilon; //probability of extending a gap: G1->G1,  G2->G2
  double beta;    //unweighting factor for adjacent G1/G2 gaps

  explicit IndelModel2(int maxlength,double LO,double LE,double b=0);
};

/******************************* parameter-containing class **************************/

class Parameters {
  vector<Matrix> substitution_;

  //FIXME - this should be private - need an interface for changing branch lengths (?)

  /*--------We own this----------*/
  SubstitutionModel* SModel_;

public:
  /*---------Indel Model---------*/
  IndelModel IModel;

  /*------------tree-------------*/
  SequenceTree T;

  const SubstitutionModel& SModel() const {return *SModel_;}

  const alphabet& get_alphabet() const {return SModel_->Alphabet();}

  const std::valarray<double>& frequencies() const {return SModel_->frequencies();}

  void fiddle() {SModel_->fiddle();recalc();}

  //FIXME - why this hack?
  const Matrix& substitution(int i) const {if (i==T.num_nodes()-2) i--;return substitution_[i];}

  void setlength(int,double);
  
  void recalc();

  // parameter for exponential prior on branch length
  double branch_mean; 

  Parameters& operator=(const Parameters&);

  Parameters(const Parameters&);
  Parameters(const SubstitutionModel&,const IndelModel&,const SequenceTree&);
  ~Parameters() {delete SModel_;}
};

#endif
