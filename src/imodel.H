#ifndef IMODEL_H
#define IMODEL_H

#include "mytypes.H"

/******************************* InDel Models ***********************************/

//separate pairwise and complete indel models? (e.g. where does full_tree go?)


/// A generic insertion/deletion model
class IndelModel {
protected:
  /// Transition matrix w/o the end state
  ublas::matrix<double> P;

  /// Model parameters
  vector<double> parameters_;

  IndelModel(int s);
public:
  /// Are we using a binary tree (instead of a star tree) for the gap prior?
  bool full_tree;

  /// Change the model parameters
  virtual void fiddle(const std::valarray<bool>&)=0;

  /// Update structures from the model parameters
  virtual void recalc() {}

  /// Get the model parameters
  virtual const vector<double>& parameters() const {return parameters_;}

  /// Set the model parameters
  virtual void parameters(const vector<double>& p) {parameters_=p; recalc();}

  /// The prior on the current parameters
  virtual double prior(double) const {return 0.0;}

  virtual IndelModel* clone() const =0;

  /// The probability that we start in each state (a way of not dealing with start states)
  vector<double> pi;

  /// The transition matrix
  ublas::matrix<double> Q;

  /// Probability that an alignment has a sequence with length 'i'
  double lengthp(int i,int G=1) const;

  /// Probability that an alignment has a sequence with length 'i' in paths without G1
  double length_plus_p(int i,int G=1) const;

  IndelModel();

  virtual ~IndelModel();
};

class IndelModel1 : public IndelModel {
public:

  void fiddle(const std::valarray<bool>&);

  void recalc();

  double prior(double) const;

  IndelModel1* clone() const {return new IndelModel1(*this);}

  ////probability of going -> E
  double tau;     

  ///probability of starting a gap: M -> G1,G2
  double delta;   

  ///probability of extending a gap: G1->G1,  G2->G2
  double epsilon; 

  explicit IndelModel1(double LO,double LE);

  ~IndelModel1() {};
};

class UpweightedIndelModel : public IndelModel {
public:
  void fiddle(const std::valarray<bool>&);

  void recalc();

  double prior(double) const;

  UpweightedIndelModel* clone() const {return new UpweightedIndelModel(*this);}

  ///probability of going -> E
  double tau;     

  ///probability of starting a gap: M -> G1,G2
  double delta;   

  ///probability of extending a gap: G1->G1,  G2->G2
  double epsilon; 

  explicit UpweightedIndelModel(double LO,double LE);
};



/// An IndelModel which assumes indels consist of only one letter
class SingleIndelModel : public IndelModel {

public:
  void fiddle(const std::valarray<bool>&);

  void recalc();

  double prior(double) const;

  SingleIndelModel* clone() const {return new SingleIndelModel(*this);}

  ///probability of going -> E
  double tau;

  ///probability of starting a gap: M -> G1,G2
  double delta;

  explicit SingleIndelModel(double LO);
};



#endif
