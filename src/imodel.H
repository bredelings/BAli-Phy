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
}

/// A generic insertion/deletion model
class IndelModel: public Model 
{
public: 

  virtual IndelModel* clone() const =0;

  /// Probability that an alignment has a sequence with length 'i'
  virtual efloat_t lengthp(int i) const=0;

  /// Alignment distribution for a branch of time t
  virtual indel::PairHMM get_branch_HMM(double t) const=0;

  virtual void set_training(bool) {};

  virtual bool is_training() const {return false;}

  virtual ~IndelModel();
};

class SimpleIndelModel : public IndelModel {
protected:
  void recalc(const std::vector<int>&);

  /// The transition matrix with G2 state removed
  indel::PairHMM Q1;

  /// The transition matrix with G1 state removed
  Matrix QE;

  bool in_training;
public:
  efloat_t prior() const;

  SimpleIndelModel* clone() const {return new SimpleIndelModel(*this);}

  efloat_t lengthp(int i) const;

  indel::PairHMM get_branch_HMM(double t) const;

  std::string name() const;

  void set_training(bool);

  bool is_training() const {return in_training;}

  SimpleIndelModel();
};


class NewIndelModel: public IndelModel 
{
  bool time_dependant;

  bool in_training;
public:
  efloat_t prior() const;

  NewIndelModel* clone() const {return new NewIndelModel(*this);}

  efloat_t lengthp(int i) const;

  indel::PairHMM get_branch_HMM(double t) const;

  std::string name() const;

  void set_training(bool);

  bool is_training() const {return in_training;}

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

#endif
