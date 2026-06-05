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

#include "computation/object.H"

/******************************* InDel Models ***********************************/

//separate pairwise and complete indel models? (e.g. where does full_tree go?)

//FIXME - add a 'name' method, and names for parameters.

namespace indel
{
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
    matrix<int> M;
    matrix<int> D;
    matrix<int> I;
    transducer_state_info(const indel::PairTransducer& P);
};


void remove_one_state(Matrix& Q,int S);
void fragmentize(Matrix& Q,double e,int S);
void fragmentize(Matrix& Q,double e);
void exitize(Matrix& Q,double t,int S1,int S2);

#endif
