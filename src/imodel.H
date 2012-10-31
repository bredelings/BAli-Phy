/*
   Copyright (C) 2004-2007,2010 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

///
/// \file imodel.H
///
/// \brief This file defines classes and functions related to insertions and deletions.
///

#ifndef IMODEL_H
#define IMODEL_H

#include "mytypes.H"
#include "model.H"
#include "object.H"
#include "computation/operation.H"

/******************************* InDel Models ***********************************/

//separate pairwise and complete indel models? (e.g. where does full_tree go?)

//FIXME - add a 'name' method, and names for parameters.

namespace indel {
  class PairHMM: public Matrix, public Object {
    std::vector<double> start_pi_;
  public:
    PairHMM* clone() const {return new PairHMM(*this);}

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
    std::vector<int> e1;

    /// letter emitted from sequence 2
    std::vector<int> e2;

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
    PairTransducer(int,int,const std::vector<int>&,const std::vector<int>&);
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
  bool in_training;
  double heat;
public: 

  virtual IndelModel* clone() const =0;

  /// Probability that an alignment has a sequence with length 'i'
  virtual efloat_t lengthp(int i) const=0;

  /// Alignment distribution for a branch of time t
  virtual indel::PairHMM get_branch_HMM(double t) const=0;

  virtual void set_training(bool);

  virtual bool is_training() const;

  virtual void set_heat(double);

  virtual double get_heat() const;

  IndelModel();

  virtual ~IndelModel();
};

class SimpleIndelModel : public IndelModel {
protected:
  void recalc(const std::vector<int>&);

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


struct get_transition_counts: public Operation
{
  virtual get_transition_counts* clone() const {return new get_transition_counts(*this);}

  tribool compare(const Object& o) const 
  {
    const get_transition_counts* E = dynamic_cast<const get_transition_counts*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "getTransitionCounts";}
  
  get_transition_counts():Operation(1) {}
};


struct pairwise_alignment_probability_from_counts: public Operation
{
  virtual pairwise_alignment_probability_from_counts* clone() const {return new pairwise_alignment_probability_from_counts(*this);}

  tribool compare(const Object& o) const 
  {
    const pairwise_alignment_probability_from_counts* E = dynamic_cast<const pairwise_alignment_probability_from_counts*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "pairwiseAlignmentProbabilityFromCounts";}
  
  pairwise_alignment_probability_from_counts():Operation(2) {}
};


struct RS07_branch_HMM: public Operation
{
  virtual RS07_branch_HMM* clone() const {return new RS07_branch_HMM(*this);}

  tribool compare(const Object& o) const 
  {
    const RS07_branch_HMM* E = dynamic_cast<const RS07_branch_HMM*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "RS07_branch_HMM";}
  
  RS07_branch_HMM():Operation(4) {}
};


struct RS07_lengthp: public Operation
{
  virtual RS07_lengthp* clone() const {return new RS07_lengthp(*this);}

  tribool compare(const Object& o) const 
  {
    const RS07_lengthp* E = dynamic_cast<const RS07_lengthp*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "RS07_lengthp";}
  
  RS07_lengthp():Operation(2) {}
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
