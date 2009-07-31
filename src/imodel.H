#ifndef IMODEL_H
#define IMODEL_H

#include "mytypes.H"
#include "model.H"

/******************************* InDel Models ***********************************/

//separate pairwise and complete indel models? (e.g. where does full_tree go?)

//FIXME - add a 'name' method, and names for parameters.

namespace indel {
  class PairHMM: public Matrix {
    std::vector<double> start_pi_;
  public:
    int n_states() const {return size1()-1;}
    double start(int i) const;
    double& start_pi(int i) {return start_pi_[i];}
    double start_pi(int i) const {return start_pi_[i];}
    const std::vector<double>& start_pi() const {return start_pi_;}
    PairHMM();
  };

  class PairTransducer: public Matrix
  {
    /// letter emitted from sequence 1
    vector<int> e1;

    /// letter emitted from sequence 2
    vector<int> e2;

    int n_letters_;

    int start_;

    int end_;

  public:

    int n_states() const {return size1();}

    /// how many letters
    int n_letters() const {return n_letters_;}

    int emits_1(int i) const {return e1[i];}
    int emits_2(int i) const {return e2[i];}

    bool is_match(int) const;
    bool is_insert(int) const;
    bool is_delete(int) const;
    bool is_silent(int) const;
    bool is_start(int) const;
    bool is_end(int)  const;

    int start_state() const {return start_;}
    int end_state() const {return end_;}

    void remove_silent();

    void check_states();

    PairTransducer(int,int);
    PairTransducer(int,int,const vector<int>&,const vector<int>&);
  };

}

// Construct matrices M(i,j) for the M state emitting letter i, where letter j is going to be the next letter emitted.
// The indices i and j range from L1 (0), L2 (1), ... , E (n_letters())
struct transducer_state_info
{
  ublas::matrix<int> M;
  ublas::matrix<int> D;
  ublas::matrix<int> I;
  transducer_state_info(const indel::PairTransducer& P);
};

/// A generic insertion/deletion model
class IndelModel: public Model 
{
public: 

  virtual IndelModel* clone() const =0;

  /// Probability that an alignment has a sequence with length 'i'
  virtual efloat_t lengthp(int i) const=0;

  /// Alignment distribution for a branch of time t
  virtual indel::PairHMM get_branch_HMM(double t) const=0;

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

  explicit TKF1(bool);
};

class TKF2: public IndelModel 
{
  bool time_dependant;
public:

  efloat_t prior() const;
  
  TKF2* clone() const {return new TKF2(*this);}

  efloat_t lengthp(int i) const;
  indel::PairHMM get_branch_HMM(double t) const;

  std::string name() const;

  explicit TKF2(bool);
};

/// A generic insertion/deletion model
class TransducerIndelModel: public Model 
{
public: 

  virtual Matrix root_chain() const =0;

  virtual TransducerIndelModel* clone() const =0;

  /// Alignment distribution for a branch of time t
  virtual indel::PairTransducer get_branch_Transducer(double t) const=0;

  virtual int n_letters() const =0;

  virtual ~TransducerIndelModel();
};

class TKF1_Transducer: public TransducerIndelModel
{
  bool time_dependent;
public:
  Matrix root_chain() const;

  efloat_t prior() const;
  TKF1_Transducer* clone() const {return new TKF1_Transducer(*this);}
  indel::PairTransducer get_branch_Transducer(double t) const;
  std::string name() const;
  int n_letters() const {return 1;}
  TKF1_Transducer(bool);
};

class FS_Transducer: public TransducerIndelModel
{
  bool time_dependent;
public:
  Matrix root_chain() const;

  efloat_t prior() const;
  FS_Transducer* clone() const {return new FS_Transducer(*this);}
  indel::PairTransducer get_branch_Transducer(double t) const;
  std::string name() const;
  int n_letters() const {return 2;}
  FS_Transducer(bool);
};

#endif
