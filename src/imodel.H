#ifndef IMODEL_H
#define IMODEL_H

#include "mytypes.H"
#include "model.H"

/******************************* InDel Models ***********************************/

//separate pairwise and complete indel models? (e.g. where does full_tree go?)

//FIXME - add a 'name' method, and names for parameters.

namespace indel {
  class PairHMM: public eMatrix {
  public:
    int n_states() const {return size1()-1;}
    efloat_t start(int i) const;
    efloat_t start_pi(int i) const {return (*this)(n_states(),i);}
    std::vector<efloat_t> start_pi() const;
   PairHMM();
  };
}

/// A generic insertion/deletion model
class IndelModel:public Model {
protected:
  IndelModel(int s);

public:
  /// Are we using a binary tree (instead of a star tree) for the gap prior?
  bool full_tree;

  virtual IndelModel* clone() const =0;

  /// Probability that an alignment has a sequence with length 'i'
  virtual double lengthp(int i) const=0;

  /// Alignment distribution for a branch of time t
  virtual indel::PairHMM get_branch_HMM(double t) const=0;

  IndelModel();

  virtual ~IndelModel();
};

class SimpleIndelModel : public IndelModel {
protected:
  void recalc();

  /// The transition matrix with G2 state removed
  indel::PairHMM Q1;

  /// The transition matrix with G1 state removed
  Matrix QE;

public:
  void fiddle();
  double prior() const;

  SimpleIndelModel* clone() const {return new SimpleIndelModel(*this);}

  double lengthp(int i) const;

  indel::PairHMM get_branch_HMM(double t) const;

  std::string name() const;

  std::string parameter_name(int) const;

  explicit SimpleIndelModel();
};


class NewIndelModel: public IndelModel {
protected:
  void recalc();
public:
  void fiddle();
  double prior() const;

  NewIndelModel* clone() const {return new NewIndelModel(*this);}

  double lengthp(int i) const;

  indel::PairHMM get_branch_HMM(double t) const;

  std::string name() const;

  std::string parameter_name(int) const;

  explicit NewIndelModel();
};


#endif
