#ifndef IMODEL_H
#define IMODEL_H

#include "mytypes.H"

/******************************* InDel Models ***********************************/

class IndelModel {
protected:
  std::vector<double> p_length;
  std::vector<double> p_length_plus;
  
  void construct_lengthp(int);
  void construct_length_plus_p(int);
  ublas::matrix<double> P;

public:
  /// The probability that we start in each state (a way of not dealing with start states)
  vector<double> pi;

  /// The transition matrix
  ublas::matrix<double> Q;

  /// Probability that an alignment has a sequence with length 'i'
  double lengthp(int i) const {return p_length[i];}

  /// Probability that an alignment has a sequence with length 'i' in paths without G1
  double length_plus_p(int i) const {return p_length_plus[i];}

  IndelModel();
};

class IndelModel1 : public IndelModel {
public:
  ////probability of going -> E
  double tau;     

  ///probability of starting a gap: M -> G1,G2
  double delta;   

  ///probability of extending a gap: G1->G1,  G2->G2
  double epsilon; 

  explicit IndelModel1(int maxlength,double LO,double LE);
};

class IndelModel2 : public IndelModel {
public:
  ///probability of going -> E
  double tau;     

  ///probability of starting a gap: M -> G1,G2
  double delta;   

  ///probability of extending a gap: G1->G1,  G2->G2
  double epsilon; 

  ///unweighting factor for adjacent G1/G2 gaps
  double beta;    

  explicit IndelModel2(int maxlength,double LO,double LE,double b=0);
};

class SingleIndelModel : public IndelModel {

public:
  ///probability of going -> E
  double tau;

  ///probability of starting a gap: M -> G1,G2
  double delta;

  explicit SingleIndelModel(int maxlength,double LO);
};

#endif
