#ifndef PARAMETERS_H
#define PARAMETERS_H

#include "alphabet.H"
#include "mytypes.H"
#include "tree.H"
#include <valarray>
#include "SModel.H"

/******************************* InDel Models ***********************************/

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

  explicit IndelModel(double LO,double LE);
};



/******************************* parameter-containing class **************************/

class Parameters {
  vector<Matrix> substitution_;

public:
  //FIXME - this should be private - need an interface for changing branch lengths (?)

  /*-------We own this----------*/
  SubstitutionModel* SModel;

  const alphabet& get_alphabet() const {return SModel->Alphabet();}

  const std::valarray<double>& frequencies() const {return SModel->frequencies();}

  const Matrix& substitution(int i) const {if (i==T.num_nodes()-2) i--;return substitution_[i];}

  void setlength(int,double);
  
  /* Indel Model */
  IndelModel IModel;

  SequenceTree T;

  void recalc();

  // parameter for exponential prior on branch length
  double branch_mean; 

  Parameters& operator=(const Parameters&);

  Parameters(const Parameters&);
  Parameters(SubstitutionModel&,double,double,const SequenceTree&);
  ~Parameters() {delete SModel;}
};

#endif
