#ifndef IMODEL_H
#define IMODEL_H

#include "mytypes.H"
#include "model.H"

/******************************* InDel Models ***********************************/

//separate pairwise and complete indel models? (e.g. where does full_tree go?)

//FIXME - add a 'name' method, and names for parameters.

namespace indel {
  class PairHMM: public Matrix {
  public:
    int n_states() const {return size1()-1;}
    double start(int i) const;
    double start_pi(int i) const {return (*this)(n_states(),i);}
    std::vector<double> start_pi() const;
   PairHMM();
  };
}

/// A generic insertion/deletion model
class IndelModel:public Model {
protected:
  IndelModel(int s);

public:

  virtual IndelModel* clone() const =0;

  /// Probability that an alignment has a sequence with length 'i'
  virtual efloat_t lengthp(int i) const=0;

  /// Alignment distribution for a branch of time t
  virtual indel::PairHMM get_branch_HMM(double t) const=0;

  IndelModel();

  virtual ~IndelModel();
};

class SimpleIndelModel : public IndelModel {
protected:
  void recalc(const vector<int>&);

  /// The transition matrix with G2 state removed
  indel::PairHMM Q1;

  /// The transition matrix with G1 state removed
  Matrix QE;

public:
  efloat_t prior() const;

  SimpleIndelModel* clone() const {return new SimpleIndelModel(*this);}

  efloat_t lengthp(int i) const;

  indel::PairHMM get_branch_HMM(double t) const;

  std::string name() const;

  std::string parameter_name(int) const;

  SimpleIndelModel();
};


class NewIndelModel: public IndelModel 
{
  bool time_dependant;
public:
  efloat_t prior() const;

  NewIndelModel* clone() const {return new NewIndelModel(*this);}

  efloat_t lengthp(int i) const;

  indel::PairHMM get_branch_HMM(double t) const;

  std::string name() const;

  std::string parameter_name(int) const;

  explicit NewIndelModel(bool);
};


class TKF1: public IndelModel 
{
  bool time_dependant;
public:

  efloat_t prior() const;
  
  TKF1* clone() const {return new TKF1(*this);}

  efloat_t lengthp(int i) const;
  indel::PairHMM get_branch_HMM(double t) const;

  std::string name() const;

  std::string parameter_name(int) const;

  explicit TKF1(bool);
};

#endif
