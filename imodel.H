#ifndef IMODEL_H
#define IMODEL_H

#include "mytypes.H"

/******************************* InDel Models ***********************************/

/// A generic insertion/deletion model
class IndelModel {
protected:
  /// (log) probability that a sequences has length i
  std::vector<double> p_length;
  /// (log) probability that a sequences has length i in the clustal model
  std::vector<double> p_length_plus;
  
  /// Calculate the sequence length distribution
  void construct_lengthp();
  /// Calculate the sequence length distribution in the clustal model
  void construct_length_plus_p();

  /// Transition matrix w/o the end state
  ublas::matrix<double> P;

  /// Model parameters
  vector<double> parameters_;

  IndelModel(int s);
public:
  /// are the parameters fixed?
  vector<bool> fixed;

  /// Are we using a binary tree (instead of a star tree) for the gap prior?
  bool full_tree;

  /// Change the model parameters
  virtual void fiddle()=0;

  /// Update structures from the model parameters
  virtual void recalc() {
    construct_lengthp();
    construct_length_plus_p();
  }

  /// Get the model parameters
  virtual const vector<double>& parameters() const {return parameters_;}

  /// Set the model parameters
  virtual void parameters(const vector<double>& p) {parameters_=p; recalc();}

  /// The prior on the current parameters
  virtual double prior() const {return 0.0;}

  virtual IndelModel* clone() const =0;

  /// The probability that we start in each state (a way of not dealing with start states)
  vector<double> pi;

  /// The transition matrix
  ublas::matrix<double> Q;

  /// Probability that an alignment has a sequence with length 'i'
  double lengthp(int i) const {
    if (i<p_length.size()) 
      return p_length[i];
    else {
      int j1 = p_length.size()-1;
      return p_length[j1] + (i-j1)*(p_length[j1]-p_length[j1-1]);
    }
  }

  /// Probability that an alignment has a sequence with length 'i' in paths without G1
  double length_plus_p(int i) const {
    if (i<p_length_plus.size())
      return p_length_plus[i];
    else {
      int j1 = p_length_plus.size()-1;
      return p_length_plus[j1] + (i-j1)*(p_length_plus[j1]-p_length_plus[j1-1]);
    }
  }

  IndelModel();

  virtual ~IndelModel() {}
};

class IndelModel1 : public IndelModel {
public:
  void fiddle();

  void recalc();

  double prior() const;

  IndelModel1* clone() const {return new IndelModel1(*this);}

  ////probability of going -> E
  double tau;     

  ///probability of starting a gap: M -> G1,G2
  double delta;   

  ///probability of extending a gap: G1->G1,  G2->G2
  double epsilon; 

  explicit IndelModel1(double LO,double LE);
};

class IndelModel2 : public IndelModel {
public:
  void fiddle();

  void recalc();

  double prior() const;

  IndelModel2* clone() const {return new IndelModel2(*this);}

  ///probability of going -> E
  double tau;     

  ///probability of starting a gap: M -> G1,G2
  double delta;   

  ///probability of extending a gap: G1->G1,  G2->G2
  double epsilon; 

  ///unweighting factor for adjacent G1/G2 gaps
  double beta;    

  explicit IndelModel2(double LO,double LE,double b=0);
};

class SingleIndelModel : public IndelModel {

public:
  void fiddle();

  void recalc();

  double prior() const;

  SingleIndelModel* clone() const {return new SingleIndelModel(*this);}

  ///probability of going -> E
  double tau;

  ///probability of starting a gap: M -> G1,G2
  double delta;

  explicit SingleIndelModel(double LO);
};

#endif
