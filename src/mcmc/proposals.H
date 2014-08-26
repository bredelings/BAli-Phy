/*
   Copyright (C) 2006-2010 Benjamin Redelings

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

#ifndef PROPOSALS_H
#define PROPOSALS_H

#include <vector>
#include <boost/shared_ptr.hpp>
#include "object.H"

double scale_gaussian(double& x,double sigma);

#include "models/parameters.H"

class Proposal: public Object {
public:
  virtual std::set<int> get_affected_parameters(const owned_ptr<Model>&) const = 0;
  Proposal* clone() const =0;
  virtual double operator()(Model& P) const=0;
};

typedef double (*generic_proposal)(Model& P);

class Generic_Proposal: public Proposal
{
  generic_proposal proposal;
  std::set<int> affected_parameters;
  
public:
  Generic_Proposal* clone() const {return new Generic_Proposal(*this);}
  std::set<int> get_affected_parameters(const owned_ptr<Model>&) const {return affected_parameters;}
  double operator()(Model& P) const {return proposal(P);}
  Generic_Proposal(generic_proposal gp):proposal(gp) {}
  Generic_Proposal(generic_proposal gp,const std::set<int>& a):proposal(gp),affected_parameters(a) {}
};

// This type of proposal can't check if arguments are fixed, and therefore must assume that they are
// all variable.  Therefore, it can't have any fixed arguments.
typedef double (*proposal_fn)(std::vector< object_ref >& x,const std::vector<double>& p);

// This wrapper class therefore cannot have any fixed arguments.
class Proposal_Fn
{
  proposal_fn proposal;
public:
  virtual Proposal_Fn* clone() const {return new Proposal_Fn(*this);}

  virtual double operator()(std::vector< object_ref >& x,const std::vector<double>& p) const 
  {
    return proposal(x,p);
  }

  Proposal_Fn():proposal(NULL) {}
  Proposal_Fn(const proposal_fn& p):proposal(p) {}

  virtual ~Proposal_Fn() {}
};

class less_than: public Proposal_Fn
{
  double max;
  owned_ptr<Proposal_Fn> proposal;
public:
  less_than* clone() const {return new less_than(*this);}
  double operator()(std::vector< object_ref >& x,const std::vector<double>& p) const;
  less_than(double m,const Proposal_Fn& P);
};

class more_than: public Proposal_Fn
{
  double min;
  owned_ptr<Proposal_Fn> proposal;
public:
  more_than* clone() const {return new more_than(*this);}
  double operator()(std::vector< object_ref >& x,const std::vector<double>& p) const;
  more_than(double m,const Proposal_Fn& P);
};

class Between: public Proposal_Fn
{
  double min;
  double max;
  owned_ptr<Proposal_Fn> proposal;
public:
  Between* clone() const {return new Between(*this);}
  double operator()(std::vector< object_ref >& x,const std::vector<double>& p) const;
  Between(double m1,double m2,const Proposal_Fn& P);
};

class Reflect: public Proposal_Fn
{
  Bounds<double> bounds;
  owned_ptr<Proposal_Fn> proposal;
public:
  Reflect* clone() const {return new Reflect(*this);}
  double operator()(std::vector< object_ref >& x,const std::vector<double>& p) const;
  Reflect(const Bounds<double>& b,const Proposal_Fn& P);
};

class log_scaled: public Proposal_Fn
{
  owned_ptr<Proposal_Fn> proposal;
public:
  log_scaled* clone() const {return new log_scaled(*this);}
  double operator()(std::vector< object_ref >& x,const std::vector<double>& p) const;
  log_scaled(const Proposal_Fn& P);
};

class LOD_scaled: public Proposal_Fn
{
  owned_ptr<Proposal_Fn> proposal;
public:
  LOD_scaled* clone() const {return new LOD_scaled(*this);}
  double operator()(std::vector< object_ref >& x,const std::vector<double>& p) const;
  LOD_scaled(const Proposal_Fn& P);
};

class sorted: public Proposal_Fn
{
  owned_ptr<Proposal_Fn> proposal;
public:
  sorted* clone() const {return new sorted(*this);}
  double operator()(std::vector< object_ref >& x,const std::vector<double>& p) const;
  sorted(const Proposal_Fn& P);
};

class Proposal2: public Proposal
{
  owned_ptr<Proposal_Fn> proposal;
  std::vector<int> indices;
  std::vector<std::string> pnames;

public:
  const std::vector<int>& get_indices() const {return indices;}
  std::set<int> get_affected_parameters(const owned_ptr<Model>&) const;
  Proposal2* clone() const {return new Proposal2(*this);}
  double operator()(Model& P) const;
  Proposal2(const Proposal_Fn& p,const std::string& s, const std::vector<std::string>& v,
	    const Model& P);
  Proposal2(const Proposal_Fn& p,const std::vector<std::string>& s, const std::vector<std::string>& v,
	    const Model& P);
};


class Proposal2M: public Proposal
{
  owned_ptr<Proposal_Fn> proposal;
  std::vector<int> indices;
  std::vector<double> parameters;

public:
  const std::vector<int>& get_indices() const {return indices;}
  std::set<int> get_affected_parameters(const owned_ptr<Model>&) const;
  Proposal2M* clone() const {return new Proposal2M(*this);}
  double operator()(Model& P) const;
  Proposal2M(const Proposal_Fn& p,int, const std::vector<double>& v);
  Proposal2M(const Proposal_Fn& p,const std::vector<int>&, const std::vector<double>& v);
};


double dirichlet_proposal(std::vector< object_ref >& x,const std::vector<double>& p);

double scale_gaussian2(std::vector< object_ref >& x,const std::vector<double>& p);

double shift_gaussian(std::vector< object_ref >& x,const std::vector<double>& p);

double shift_laplace(std::vector< object_ref >& x,const std::vector<double>& p);

double shift_cauchy(std::vector< object_ref >& x,const std::vector<double>& p);

double shift_delta(std::vector< object_ref >& x, const std::vector<double>& p);

double shift_epsilon(std::vector< object_ref >& x, const std::vector<double>& p);

double bit_flip(std::vector< object_ref >& x, const std::vector<double>& p);

double discrete_uniform(std::vector< object_ref >& x, const std::vector<double>& p);

double move_scale_branch(Model& P);

double move_subst_type_branch(Model& P);

#endif
