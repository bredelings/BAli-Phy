/*
   Copyright (C) 2004-2010 Benjamin Redelings

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
/// \file   mcmc.C
/// \brief  Provides classes for constructing MCMC samplers.
///
/// This file provides classes for constructing MCMC samplers.  The
/// class Sampler is used to run the main loop of the sampler for
/// bali-phy.
///
/// \author Benjamin Redelings
/// 

#include <boost/numeric/ublas/io.hpp>
#include <iostream>
#include <algorithm>

#include "mcmc.H"
#include "sample.H"
#include "myexception.H"
#include "rng.H"
#include "util.H"
#include "util-random.H"

#include "../setup.H"           // for letter_counts( )

#include "monitor.H"
#include "proposals.H"
#include "alignment/alignment-util.H"

#include "slice-sampling.H"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_MPI
#include <mpi.h>
#include <boost/mpi.hpp>
namespace mpi = boost::mpi;
#endif

using std::endl;

namespace MCMC {
  using std::vector;
  using std::valarray;
  using std::cerr;
  using std::clog;
  using std::string;
  using std::ostream;

  void Result::inc(const Result& R) {
    if (not counts.size()) {
      counts.resize(R.size(),0);
      totals.resize(R.size(),0);
    }

    counts += R.counts;
    totals += R.totals;
  }

  Result::Result(int l,int n)
    :counts(n,l),totals(0.0,l) 
  { }

  Result::Result(bool b)
    :counts(1,1),totals(0.0,1) 
  { 
    if (b) 
      totals[0] = 1;
  }


  void MoveStats::inc(const string& name,const Result& R) {
    (*this)[name].inc(R);
  }

  Move::Move(const string& n)
    :enabled_(true),name(n),iterations(0)
  { }

  Move::Move(const string& n,const string& v)
    :enabled_(true),name(n),attributes(split(v,':')),iterations(0)
  { }

  void Move::enable(const string& s) {
    if (s == "all" or s == name)
      enable();
  }
  
  void Move::disable(const string& s) {
    if (s == "all" or s == name)
      disable();
    else 
      for(int j=0;j<attributes.size();j++)
	if (attributes[j] == s) {
	  disable();
	  break;
	}
  }

  int Move::reset(double lambda)
  {
    int l = (int)lambda;
    lambda -= l;
    return l + poisson(lambda);
  }

  void Move::show_enabled(ostream& o,int depth) const {
    for(int i=0;i<depth;i++)
      o<<"  ";
    o<<"move "<<name<<": ";
    if (enabled_)
      o<<"enabled.\n";
    else 
      o<<"DISABLED.\n";
  }
  
  /// Add a sub-move \a m with weight \a l
  void MoveGroupBase::add(double l,const Move& m,bool enabled) 
  {
    moves.push_back(m);
    if (not enabled)
      moves.back()->disable();
    lambda.push_back(l);
  }

  std::set<int> MoveGroup::get_affected_parameters(const owned_ptr<Probability_Model>& P) const
  {
    std::set<int> affected;
    for(const auto& m: moves)
      ::add(affected, m->get_affected_parameters(P) );

    return affected;
  }

  /// Calculate the sum of the weights of enabled moves in this group
  double MoveGroup::sum() const 
  {
    double total=0;
    for(int i=0;i<lambda.size();i++)
      if (moves[i]->enabled())
	total += lambda[i];
    return total;
  }

  void MoveGroup::enable(const string& s) {
    // Operate on this move
    Move::enable(s);

    // Operate on children
    for(int i=0;i<moves.size();i++)
      moves[i]->enable(s);
  }

  void MoveGroup::disable(const string& s) {
    // Operate on this move
    Move::disable(s);

    // Operate on children
    for(int i=0;i<moves.size();i++)
      moves[i]->disable(s);
  }

  void MoveGroup::iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats) {
    reset(1.0);
    for(int i=0;i<order.size();i++)
      iterate(P,Stats,i);
  }


  void MoveGroup::iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats,int i) 
  {
    assert(i < order.size());

#ifndef NDEBUG
    clog<<" move = "<<name<<endl;
    clog<<"   submove = "<<moves[order[i]]->name<<endl;
#endif

    try {
      moves[order[i]]->iterate(P,Stats,suborder[i]);
    }
    catch (myexception& e)
    {
      std::ostringstream o;
      o<<" move = "<<name<<"\n";
      o<<"   submove = "<<moves[order[i]]->name<<"\n";
      e.prepend(o.str());
      throw e;
    }
  }

  void MoveGroup::start_learning(int i) 
  {
    // Operate on children
    for(int j=0;j<moves.size();j++)
      moves[j]->start_learning(i);
  }

  void MoveGroup::stop_learning(int i) {
    // Operate on children
    for(int j=0;j<moves.size();j++)
      moves[j]->stop_learning(i);
  }

  int MoveGroup::reset(double l) {
    iterations += l;
    getorder(l);
    order = randomize(order);

    // calculate suborder
    vector<int> total(nmoves(),0);
    suborder.resize(order.size(),0);
    for(int i=0;i<suborder.size();i++) {
      suborder[i] = total[order[i]];
      total[order[i]]++;
    }

    return order.size();
  }

  void MoveGroup::show_enabled(ostream& o,int depth) const {
    Move::show_enabled(o,depth);
  
    for(int i=0;i<nmoves();i++)
      moves[i]->show_enabled(o,depth+1);
  }

  void MoveAll::getorder(double l) {
    order.clear();
    for(int i=0;i<nmoves();i++) {
      if (not moves[i]->enabled()) continue;

      int n = moves[i]->reset(l*lambda[i]);
      for(int j=0;j<n;j++)
	order.insert(order.end(),i);
    }
  }


  int MoveOne::choose() const 
  {
    double r = uniform()*sum();

    double sum = 0;
    int i = 0;
    int enabled_submoves=0;
    for(;i < moves.size();i++) {

      if (not moves[i]->enabled())
	continue;
      else
	enabled_submoves++;

      sum += lambda[i];
      if (r<sum) break;
    }

    if (not enabled_submoves)
      throw myexception()<<"move "<<name<<" has no enabled submoves";

    return i;
  }

  void MoveOne::getorder(double l) 
  {
    // get total count
    int total = (int)l;
    double frac = l-total;
    total += poisson(frac);

    // get count per type
    vector<int> count(nmoves(),0);
    for(int i=0;i<total;i++) {
      int m = choose();
      count[m]++;
    }

    order.clear();
    for(int i=0;i<nmoves();i++) {
      int n = moves[i]->reset(count[i]);
      if (not moves[i]->enabled())
	assert(n==0);
      for(int j=0;j<n;j++)
	order.insert(order.end(),i);
    }
  }

  int SingleMove::reset(double lambda) {
    int l = (int)lambda;
    lambda -= l;
    return l + poisson(lambda);
  }

  void SingleMove::iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats,int) 
  {
#ifndef NDEBUG
    clog<<" [single] move = "<<name<<endl;
#endif

    iterations++;
    try {
      (*m)(P,Stats);
    }
    catch (myexception& e)
    {
      std::ostringstream o;
      o<<" [single] move = "<<name<<"\n";
      e.prepend(o.str());
      throw e;
    }
  }

  int MH_Move::reset(double lambda) {
    int l = (int)lambda;
    lambda -= l;
    return l + poisson(lambda);
  }

  void MH_Move::iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats,int) 
  {
#ifndef NDEBUG
    clog<<" [MH] move = "<<name<<endl;
#endif

    iterations++;

    owned_ptr<Probability_Model> P2 = P;

    double ratio = 1;
    try {
      ratio = (*proposal)(*P2);
    }
    catch (myexception& e)
    {
      std::ostringstream o;
      o<<" [MH] move = "<<name<<"  (during proposal)\n";
      e.prepend(o.str());
      throw e;
    }
    

    int n = 1;
    Proposal2* p2 = dynamic_cast<Proposal2*>(&(*proposal));
    int n_indices = -1;
    if (p2) {
      n_indices = p2->get_indices().size();
      n = 2;
    }
    Result result(n);

#ifndef NDEBUG
    show_parameters(std::cerr,*P);
    std::cerr<<P->probability()<<" = "<<P->likelihood()<<" + "<<P->prior()<<endl;
    std::cerr<<endl;

    show_parameters(std::cerr,*P2);
    std::cerr<<P2->probability()<<" = "<<P2->likelihood()<<" + "<<P2->prior();
    std::cerr<<endl<<endl;
#endif

#ifndef NDEBUG
    // Check that we have not strayed outside the bounds.
    for(int i=0;i<P->n_parameters();i++)
    {
      if (not P->parameter_is_modifiable(i)) continue;

      if (not P->parameter_has_type<Double>(i)) continue;

      if (not P->has_bounds(i)) continue;
	
      Bounds<double> range = P->get_bounds(i);
      if (not range.in_range(P->get_parameter_value_as<Double>(i)))
	throw myexception()<<"Parameter "<<P->parameter_name(i)<<" = "<<P->get_parameter_value_as<Double>(i)<<" is NOT in range "<<range;
      if (not range.in_range(P2->get_parameter_value_as<Double>(i)))
	throw myexception()<<"Parameter "<<P->parameter_name(i)<<" = "<<P->get_parameter_value_as<Double>(i)<<" is NOT in range "<<range;
    }
#endif

    // Accept or Reject
    if (accept_MH(*P,*P2,ratio)) {
      result.totals[0] = 1;
      if (n == 2) {
	int first_index = p2->get_indices()[0];
	if (n_indices == 1 and P->parameter_has_type<Double>(first_index)) {
	  double v1 = P->get_parameter_value_as<Double>(first_index);
	  double v2 = P2->get_parameter_value_as<Double>(first_index);
	  //      cerr<<"v1 = "<<v1<<"   v2 = "<<v2<<"\n";
	  result.totals[1] = std::abs(v2-v1);
	}
	else if (n_indices > 1 and P->parameter_has_type<Double>(first_index)) //currently this can only be a dirichlet proposal
	{
	  double total = 0;
	  for(int i=0;i<n_indices;i++) 
	  {
	    int j = p2->get_indices()[i];
	    double v1 = P->get_parameter_value_as<Double>(j);
	    double v2 = P2->get_parameter_value_as<Double>(j);
	    total += std::abs(log(v1/v2));
	  }
	  result.totals[1] = total;
	}
      }
      P = P2;
    }

    Stats.inc(name,result);
  }

  double Slice_Move::sample(Probability_Model& P, slice_function& slice_levels, double v1)
  {
#ifndef NDEBUG
    clog<<" [Slice] move = "<<name<<endl;
#endif

    iterations++;

    //------------- Find new value --------------//
#ifndef NDEBUG
    show_parameters(std::cerr,P);
    std::cerr<<P.probability()<<" = "<<P.likelihood()<<" + "<<P.prior();
    std::cerr<<endl<<endl;
#endif
    double transformed_v1 = transform(v1);
    double transformed_v2 = slice_sample(transformed_v1,slice_levels,W,100);
    double v2 = inverse(transformed_v2);

    if (n_learning_iterations-- > 0) 
    {
      n_tries++;
      total_movement += std::abs(transformed_v2 - transformed_v1);
      
      double W_predicted = 4.0*total_movement/n_tries;
      if (n_tries > 3)
	W = 0.95*W + 0.05*W_predicted;
    }

#ifndef NDEBUG
    show_parameters(std::cerr,P);
    std::cerr<<P.probability()<<" = "<<P.likelihood()<<" + "<<P.prior();
    std::cerr<<endl<<endl;
#endif
    return v2;
  }

  /// FIXME - how to learn differently for different temperatures?
  void Slice_Move::start_learning(int n)
  {
    n_learning_iterations = n;
    n_tries = 0;
    total_movement = 0;
  }

  void Slice_Move::stop_learning(int)
  {
    n_learning_iterations = 0;
  }

  Slice_Move::Slice_Move(const string& s)
    :Move(s),
     W(1),
     transform(slice_sampling::identity),
     inverse(slice_sampling::identity),
     n_learning_iterations(0),
     n_tries(0),
     total_movement(0)
  {}

  Slice_Move::Slice_Move(const string& s, const string& v)
    :Move(s,v),
     W(1),
     transform(slice_sampling::identity),
     inverse(slice_sampling::identity),
     n_learning_iterations(0),
     n_tries(0),
     total_movement(0)
  {}

  Slice_Move::Slice_Move(const string& s, const string& v, double W_)
    :Move(s,v),
     W(W_),
     transform(slice_sampling::identity),
     inverse(slice_sampling::identity),
     n_learning_iterations(0),
     n_tries(0),
     total_movement(0)
  {}

  Slice_Move::Slice_Move(const string& s, double W_)
    :Move(s),
     W(W_),
     transform(slice_sampling::identity),
     inverse(slice_sampling::identity),
     n_learning_iterations(0),
     n_tries(0),
     total_movement(0)
  {}

  Slice_Move::Slice_Move(const string& s, const string& v, double W_,
			 double(*f1)(double),
			 double(*f2)(double))
    :Move(s,v),
     W(W_),
     transform(f1),
     inverse(f2),
     n_learning_iterations(0),
     n_tries(0),
     total_movement(0)
  {}

  void Parameter_Slice_Move::iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats,int)
  {
    double v1 = P->get_parameter_value_as<Double>(index);

    parameter_slice_function logp(*P,index,transform,inverse);

    double v2 = sample(*P,logp,v1);

    //---------- Record Statistics - -------------//
    Result result(2);
    result.totals[0] = std::abs(v2-v1);
    result.totals[1] = logp.count;
    
    Stats.inc(name,result);
  }

  Parameter_Slice_Move::Parameter_Slice_Move(const string& s,int i,
					     double W_)
    :Slice_Move(s,W_),index(i)
  {}

  Parameter_Slice_Move::Parameter_Slice_Move(const string& s, const string& v,int i,
					     double W_)
    :Slice_Move(s,v,W_),index(i)
  {}

  Parameter_Slice_Move::Parameter_Slice_Move(const string& s,int i,
					     double W_,
					     double(*f1)(double),
					     double(*f2)(double))
    :Slice_Move(s,"",W_,f1,f2),index(i)
  {}

  Parameter_Slice_Move::Parameter_Slice_Move(const string& s, const string& v,int i,
					     double W_,
					     double(*f1)(double),
					     double(*f2)(double))
    :Slice_Move(s,v,W_,f1,f2),index(i)
  {}

  void Modifiable_Slice_Move::iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats,int)
  {
#ifndef NDEBUG
    clog<<" [modifiable slice] move = "<<m_index<<endl;
#endif

    double v1 = P->get_modifiable_value_as<Double>(m_index);

    modifiable_slice_function logp(*P, m_index, bounds, transform, inverse);

    double v2 = sample(*P,logp,v1);

    //---------- Record Statistics --------------//
    Result result(2);
    result.totals[0] = std::abs(v2-v1);
    result.totals[1] = logp.count;
    
    Stats.inc(name,result);
  }

  Modifiable_Slice_Move::Modifiable_Slice_Move(const string& s,int m,
					       const Bounds<double>& b, double W_)
    :Slice_Move(s,W_), m_index(m), bounds(b)
  {}

  Modifiable_Slice_Move::Modifiable_Slice_Move(const string& s, const string& v,int m,
					       const Bounds<double>& b, double W_)
    :Slice_Move(s,v,W_), m_index(m), bounds(b)
  {}

  Modifiable_Slice_Move::Modifiable_Slice_Move(const string& s,int m,
					       const Bounds<double>& b, double W_,
					       double(*f1)(double),
					       double(*f2)(double))
    :Slice_Move(s,"",W_,f1,f2), m_index(m), bounds(b)
  {}

  Modifiable_Slice_Move::Modifiable_Slice_Move(const string& s, const string& v,int m,
					       const Bounds<double>& b, double W_,
					       double(*f1)(double),
					       double(*f2)(double))
    :Slice_Move(s,v,W_,f1,f2), m_index(m), bounds(b)
  {}

  void Integer_Modifiable_Slice_Move::iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats,int)
  {
#ifndef NDEBUG
    clog<<" [integer modifiable slice] move = "<<m_index<<endl;
#endif

    int v1 = P->get_modifiable_value_as<Int>(m_index);
    double x1 = double(v1)+uniform();

    integer_modifiable_slice_function logp(*P, m_index, bounds, transform, inverse);

    double x2 = sample(*P,logp,x1);

    //---------- Record Statistics --------------//
    Result result(2);
    result.totals[0] = std::abs(x2-x1);
    result.totals[1] = logp.count;
    
    Stats.inc(name,result);
  }

  Integer_Modifiable_Slice_Move::Integer_Modifiable_Slice_Move(const string& s,int m,
					       const Bounds<int>& b, double W_)
    :Slice_Move(s,W_), m_index(m), bounds(b)
  {}

  Integer_Modifiable_Slice_Move::Integer_Modifiable_Slice_Move(const string& s, const string& v,int m,
					       const Bounds<int>& b, double W_)
    :Slice_Move(s,v,W_), m_index(m), bounds(b)
  {}

  Integer_Modifiable_Slice_Move::Integer_Modifiable_Slice_Move(const string& s,int m,
					       const Bounds<int>& b, double W_,
					       double(*f1)(double),
					       double(*f2)(double))
    :Slice_Move(s,"",W_,f1,f2), m_index(m), bounds(b)
  {}

  Integer_Modifiable_Slice_Move::Integer_Modifiable_Slice_Move(const string& s, const string& v,int m,
					       const Bounds<int>& b, double W_,
					       double(*f1)(double),
					       double(*f2)(double))
    :Slice_Move(s,v,W_,f1,f2), m_index(m), bounds(b)
  {}

  void Dirichlet_Slice_Move::iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats,int)
  {
#ifndef NDEBUG
    clog<<" [dirichlet slice] move"<<endl;
#endif
    double v1 = P->get_parameter_value_as<Double>(indices[n]);
    constant_sum_slice_function slice_levels_function(*P,indices,n);

    double v2 = sample(*P,slice_levels_function,v1);

    //---------- Record Statistics - -------------//
    Result result(2);
    vector<Double> x = P->get_parameter_values_as<Double>(indices);
    double total = sum(x);
    double factor = (total - v2)/(total-v1);
    result.totals[0] = std::abs(log(v2/v1)) + (indices.size()-1)*(std::abs(log(factor)));
    result.totals[1] = slice_levels_function.count;

    Stats.inc(name,result);
  }

  std::set<int> Dirichlet_Slice_Move::get_affected_parameters(const owned_ptr<Probability_Model>&) const
  {
    std::set<int> affected;
    affected.insert(indices.begin(), indices.end());
    return affected;
  }

  Dirichlet_Slice_Move::Dirichlet_Slice_Move(const string& s, const vector<int>& indices_, int n_)
    :Slice_Move(s,0.2/indices_.size()),indices(indices_),n(n_)
  { }

  void Dirichlet_Modifiable_Slice_Move::iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats,int)
  {
#ifndef NDEBUG
    clog<<" [dirichlet modifiable slice] move"<<endl;
#endif
    double v1 = P->get_modifiable_value_as<Double>(indices[n]);
    constant_sum_modifiable_slice_function slice_levels_function(*P,indices,n);

    double v2 = sample(*P,slice_levels_function,v1);

    //---------- Record Statistics - -------------//
    Result result(2);
    vector<Double> x = P->get_modifiable_values_as<Double>(indices);
    double total = sum(x);
    double factor = (total - v2)/(total-v1);
    result.totals[0] = std::abs(log(v2/v1)) + (indices.size()-1)*(std::abs(log(factor)));
    result.totals[1] = slice_levels_function.count;

    Stats.inc(name,result);
  }

  std::set<int> Dirichlet_Modifiable_Slice_Move::get_affected_parameters(const owned_ptr<Probability_Model>&) const
  {
    return std::set<int>{};
  }

  Dirichlet_Modifiable_Slice_Move::Dirichlet_Modifiable_Slice_Move(const string& s, const vector<int>& indices_, int n_)
    :Slice_Move(s,0.2/indices_.size()),indices(indices_),n(n_)
  { }

  std::set<int> Scale_Means_Only_Slice_Move::get_affected_parameters(const owned_ptr<Probability_Model>& P) const
  {
    const Parameters& PP = *P.as<const Parameters>();
    std::set<int> affected;
    for(int i=0;i<PP.n_branch_means();i++)
      affected.insert(PP.branch_mean_index(i));
    return affected;
  }

  void Scale_Means_Only_Slice_Move::iterate(owned_ptr<Probability_Model>& P, MoveStats& Stats,int)
  {
#ifndef NDEBUG
    clog<<" [scale means only slice] move"<<endl;
#endif
    Parameters& PP = *P.as<Parameters>();
    // If any of the branch means are fixed, this won't work.

    double v1 = 0;
    try
    {
      scale_means_only_slice_function slice_levels_function(PP);

      double v2 = sample(PP,slice_levels_function, v1);

      //---------- Record Statistics --------------//
      Result result(2);
      result.totals[0] = std::abs(v2);
      result.totals[1] = slice_levels_function.count;

      Stats.inc(name,result);
    }
    catch (...) {}
  }

  Scale_Means_Only_Slice_Move::Scale_Means_Only_Slice_Move(const string& s, double W_)
		:Slice_Move(s,W_)
  { }


int MoveArg::reset(double l) 
{
  vector<int> numbers(args.size());
  for(int i=0;i<numbers.size();i++)
    numbers[i] = i;

  order.clear();
  while(l>0) {
    vector<int> v = randomize(numbers);

    if (l < 1) {
      double l2 = l*numbers.size();
      int n = (int)l2;
      if (uniform() < (l2-n))
	n++;
      assert(n < v.size());
      v.erase(v.begin()+n,v.end());
    }

    order.insert(order.end(),v.begin(),v.end());

    l--;
  }
  return order.size();
}

void MoveArg::iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats) {
  for(int i=0;i<order.size();i++)
    iterate(P,Stats,i);
}

void MoveArg::iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats,int i) 
{
  (*this)(P,Stats,order[i]);
}


  void IOMove::iterate(owned_ptr<Probability_Model>& P, MoveStats& M)
  {
    iterate(P,M,0);
  }

  void IOMove::iterate(owned_ptr<Probability_Model>& P, MoveStats& M,int)
  {
#ifndef NDEBUG
    clog<<" [IO Move] move = "<<head<<endl;
#endif
    P->perform(head);
  }

void MoveEach::add(double l,const MoveArg& m,bool enabled) {
  MoveGroupBase::add(l,m,enabled);

  subarg.push_back(vector<int>(args.size(),-1));
  
  for(int i=0;i<m.args.size();i++) {
    int found = -1;
    for(int j=0;j<args.size();j++) {
      if (args[j] == m.args[i])
	found=j;
    }
    if (found == -1) {
      args.push_back(m.args[i]);
      for(int k=0;k<subarg.size();k++) 
	subarg[k].push_back(-1);
      found = args.size()-1;
    }

    subarg[subarg.size()-1][found] = i;
  }
}

void MoveEach::enable(const string& s) {
  // Operate on this move
  Move::enable(s);

  // Operate on children
  for(int i=0;i<moves.size();i++)
    moves[i]->enable(s);
}

void MoveEach::disable(const string& s) {
  // Operate on this move
  Move::disable(s);

  // Operate on children
  for(int i=0;i<moves.size();i++)
    moves[i]->disable(s);
}

double MoveEach::sum(int arg) const {
  double total=0;
  for(int i=0;i<lambda.size();i++)
    if (submove_has_arg(i,arg) and moves[i]->enabled())
      total += lambda[i];

  return total;
}

int MoveEach::choose(int arg) const {
  assert(moves.size() > 0);

  double r = uniform()*sum(arg);

  double sum = 0;
  int i = 0;
  int enabled_submoves=0;
  for(;i < moves.size();i++) {

    if (moves[i]->enabled())
      enabled_submoves++;

    if (not submove_has_arg(i,arg) or not moves[i]->enabled())
      continue;

    sum += lambda[i];
    if (r<sum) break;
  }

  if (not enabled_submoves)
    throw myexception()<<"move "<<name<<" has no enabled submoves";

  // is sum(arg) > 0 ?
  if (i >= moves.size())
    assert(i < moves.size());

  return i;
}

void MoveEach::operator()(owned_ptr<Probability_Model>& P,MoveStats& Stats,int arg) {
  iterations += 1.0/args.size();
  int m = choose(arg);
  MoveArg* temp = dynamic_cast<MoveArg*>(&*moves[m]);
  if (not temp)
    std::abort();
  else
    (*temp)(P,Stats,subarg[m][arg]);
}


void MoveEach::show_enabled(ostream& o,int depth) const {
  Move::show_enabled(o,depth);
  
  for(int i=0;i<nmoves();i++)
    moves[i]->show_enabled(o,depth+1);
}

void MoveArgSingle::operator()(owned_ptr<Probability_Model>& P,MoveStats& Stats,int arg) 
{
#ifndef NDEBUG
  clog<<" [single] move = "<<name<<endl;
#endif

  iterations++;
  try {
    (*m)(P,Stats,args[arg]);
  }
  catch (myexception& e)
  {
    std::ostringstream o;
    o<<" [single] move = "<<name<<"\n";
    e.prepend(o.str());
    throw e;
  }
}
    


std::ostream& operator<<(std::ostream& o,const Matrix& M) {
  ublas::operator<<(o,M);
  return o;
}

#ifdef HAVE_MPI
  void exchange_random_pairs(int iterations, Parameters& P, MCMC::MoveStats& /*Stats*/)
{
  mpi::communicator world;
  world.barrier();

  int proc_id = world.rank();
  int n_procs = world.size();
  
  if (n_procs < 2) return;
  
  vector<int> p(n_procs);
  if (proc_id == 0) 
  {
    p = random_permutation(n_procs);

    for(int dest=1;dest<n_procs;dest++)
      world.send(dest,0,p);
  }
  else
    world.recv(0, 0, p);

  int partner = -1;

  for(int i=0;i<p.size();i++) 
  {
    if (p[i] != proc_id) continue;
    
    if (i%2)
      partner = i-1;
    else
      partner = i+1;
    
    break;
  }

  if (partner < p.size())
    partner = p[partner];
  else
    partner = -1;
  
  cerr<<"iteration="<<iterations<<"   proc="<<proc_id<<": choosing partner "<<partner<<endl;

  if (partner >=0 and partner < n_procs)
  {
    double L1 = log(P.likelihood());

    double b1 = P.get_beta();
    
    if (proc_id > partner) {
      cerr<<"Proc "<<proc_id<<": sending beta = "<<b1<<endl;

      world.send(partner, 0, b1);

      cerr<<"Proc "<<proc_id<<": sending likelihood = "<<L1<<endl;

      world.send(partner, 0, L1);

      int exchange = -1;
      
      world.recv(partner, mpi::any_tag, exchange);

      cerr<<"Proc "<<proc_id<<": result = "<<exchange<<endl;
      if (exchange == 1) {
	world.recv(partner, mpi::any_tag, b1);
	cerr<<"Proc "<<proc_id<<": new beta = "<<b1<<endl;
	P.set_beta(b1);
      }
    }
    else {
      double L2;
      double b2;
      
      world.recv(partner, 0, b2);
      world.recv(partner, 0, L2);
      
      cerr<<"Proc "<<proc_id<<": b1 = "<<b1<<endl;
      cerr<<"Proc "<<proc_id<<": b2 = "<<b2<<endl;
      cerr<<"Proc "<<proc_id<<": L1 = "<<L1<<endl;
      cerr<<"Proc "<<proc_id<<": L2 = "<<L2<<endl;
      
      // db * dL = -db*dE   because E = -L = -log(likelihood)
      double ratio = exp( (b2-b1)*(L1-L2) );
      int exchange = 0;
      if (ratio >= 1 or uniform() < ratio)
	exchange = 1;
      
      cerr<<"Proc "<<proc_id<<": ratio = "<<ratio<<endl;
      cerr<<"Proc "<<proc_id<<": result = "<<exchange<<endl;
      
      world.send(partner, 0, exchange); // MPI::COMM_WORLD.Send   (&exchange, 1, MPI::INT,  partner, 0);
      if (exchange == 1) 
      {
	world.send(partner, 0, b1); // MPI::COMM_WORLD.Send   (&b1, 1, MPI::DOUBLE,  partner, 0);
	
	P.set_beta(b2);
      }
    }
  }
}

void exchange_adjacent_pairs(int /*iterations*/, Parameters& P, MCMC::MoveStats& Stats)
{
  mpi::communicator world;
  world.barrier();

  int proc_id = world.rank();
  int n_procs = world.size();

  if (n_procs < 2) return;
  if (not P.all_betas.size()) return;

  // Determine the probability of this chain at each temperature
  efloat_t Pr1 = P.heated_probability();
  vector<double> Pr;
  for(int i=0;i<P.all_betas.size();i++)
  {
    P.set_beta(P.all_betas[i]);
    Pr.push_back(log(P.heated_probability()));
  }
  P.set_beta(P.all_betas[P.beta_index]);
  efloat_t Pr2 = P.heated_probability();

  assert(std::abs(log(Pr1)-log(Pr2)) < 1.0e-9);


  //  double oldbeta = beta;
  vector< vector<double> > Pr_all;

  // maps from chain -> position
  vector< int > chain_to_beta;

  // Has each chain recently been at the high beta (1) or the low beta (0)
  vector<int> updowns;

  // Collect the Betas and probabilities in chain 0 (master)
  gather(world, Pr, Pr_all, 0);
  gather(world, P.beta_index, chain_to_beta, 0);
  gather(world, P.updown, updowns, 0);

  // maps from beta index to chain index
  vector<int> beta_to_chain = invert(chain_to_beta);

  if (proc_id == 0)
  {
    //----- Compute an order of chains in decreasing order of beta -----//
    MCMC::Result exchange(n_procs-1,0);

    for(int i=0;i<3;i++)
    {
      //----- Propose pairs of adjacent-temperature chains  ----//
      for(int j=0;j<n_procs-1;j++)
      {
	int chain1 = beta_to_chain[j];
	int chain2 = beta_to_chain[j+1];

	// Compute the log probabilities for the two terms in the current order
	double log_Pr1 = Pr_all[chain1][j] + Pr_all[chain2][j+1];
	// Compute the log probabilities for the two terms in the proposed order
	double log_Pr2 = Pr_all[chain2][j] + Pr_all[chain1][j+1];

	// Swap the chain in beta positions j and j+1 if we accept the proposal
	exchange.counts[j]++;
	if (uniform() < exp(log_Pr2 - log_Pr1) )
	{
	  std::swap(beta_to_chain[j],beta_to_chain[j+1]);
	  exchange.totals[j]++;
	}
      }
    }

    // estimate average regeneration times for beta high->low->high
    MCMC::Result regeneration(n_procs,0);

    if (updowns[beta_to_chain[0]] == 0)
      regeneration.counts[beta_to_chain[0]]++;

    for(int i=0;i<n_procs;i++)
      regeneration.totals[i]++;
    

    // fraction of visitors that most recently visited highest Beta
    MCMC::Result f_recent_high(n_procs, 0); 

    // the lowest chain has hit the lower bound more recently than the higher bound
    updowns[beta_to_chain[0]] = 1;
    // the highest chain has hit the upper bound more recently than the higher bound
    updowns[beta_to_chain.back()] = 0;

    for(int j=0;j<n_procs;j++)
      if (updowns[beta_to_chain[j]] == 1) {
	f_recent_high.counts[j] = 1;
	f_recent_high.totals[j] = 1;
      }
      else if (updowns[beta_to_chain[j]] == 0)
	f_recent_high.counts[j] = 1;

	

    Stats.inc("MC3.exchange",exchange);
    Stats.inc("MC3.fracRecentHigh",f_recent_high);
    Stats.inc("MC3.betaRegenerationTimes",regeneration);
  }

  // recompute the chain_to_beta mapping
  vector<int> chain_to_beta2 = invert(beta_to_chain);

  // Broadcast the new betas for each chain
  int old_index = P.beta_index;
  scatter(world, chain_to_beta2, P.beta_index, 0);
  scatter(world, updowns, P.updown, 0);

  if (log_verbose)
    cerr<<"Proc["<<proc_id<<"] changing from "<<old_index<<" -> "<<P.beta_index<<endl;

  P.set_beta(P.all_betas[P.beta_index]);
}
#endif


void mcmc_init(Parameters& P, ostream& s_out)
{
  const SequenceTree& T = P.T();

  // Check that the Alignments and Tree are properly linked
  for(int i=0;i<P.n_data_partitions();i++) 
  {
    const alignment& A = *P[i].A;
    if (P[i].has_IModel())
      assert(A.n_sequences() == T.n_nodes() and P[i].variable_alignment()); 
    else
      assert(A.n_sequences() == T.n_leaves() and not P[i].variable_alignment());

    for(int j=0;j<A.n_sequences();j++)
      assert(T.get_label(j) == A.seq(j).name);    
  }

  s_out<<"\n\n\n";

  /// Output extra initial frequences if we have a triplets alphabet
  for(int i=0;i<P.n_data_partitions();i++)
  {
    const alignment& A = *P[i].A;
    if (const Triplets* T = dynamic_cast<const Triplets*>(&A.get_alphabet()) ) 
      {
	s_out<<"observed nucleotide frequencies = "<<endl;
	valarray<double> counts = letter_counts(A);
	valarray<double> N_counts = get_nucleotide_counts_from_codon_counts(*T,counts);
	
	show_frequencies(s_out,T->getNucleotides(),N_counts/N_counts.sum());
	s_out<<endl<<endl;
	
	//      FIXME!
	//
	//	s_out<<"current nucleotide frequencies = "<<endl;
	//	valarray<double> fT = ... convert to valarray .. P[i].frequencies();
	//	valarray<double> fN = get_nucleotide_counts_from_codon_counts(*T,fT);
	//	fN /= fN.sum();
	
	//	show_frequencies(s_out,T->getNucleotides(),fN);
	//	s_out<<endl<<endl;
      }
  }
}

void mcmc_log(long iterations, long max_iter, int subsample, Parameters& P, ostream& s_out, 
	      const MoveStats& S, const vector<owned_ptr<Logger> >& loggers)
{
  s_out<<"iterations = "<<iterations<<"\n";
  clog<<"iterations = "<<iterations<<"\n";

  // Don't print alignments into console log file:
  //  - Its hard to separate alignments from different partitions.
  if (iterations%subsample == 0)
    // FIXME - There are now only 2 calls to print_stats left: the other is in bali-phy.C
    ;
    //    print_stats(s_out, P, iterations%(10*subsample) == 0);

  for(int i=0;i<loggers.size();i++)
    (*loggers[i])(P,iterations);

  /*
  if (iterations%20 == 0 or iterations < 20 or iterations >= max_iter) {
      std::cout<<"Success statistics (and other averages) for MCMC transition kernels:\n\n";
      std::cout<<S<<endl;
      std::cout<<endl;
      std::cout<<"CPU Profiles for various (nested and/or overlapping) tasks:\n\n";
    }
  */
}

template <typename T>
void add_at_end(vector<T>& v1, const vector<T>& v2)
{
  v1.insert(v1.end(), v2.begin(), v2.end());
}

void Sampler::add_logger(const owned_ptr<Logger>& L)
{
  loggers.push_back(L);
}

void Sampler::check_moves(const owned_ptr<Probability_Model>& P) const
{
  std::set<int> affected = get_affected_parameters(P);

  std::cerr<<"\n";
  for(int p=0;p<P->n_parameters();p++)
  {
    // FIXME - also warn if a TK affects a variable, but not that variable's co-distributed variables.
    // Problem: that is a property of individual TKs, not aggregated TKs.

    if (P->is_random_variable(p) and not affected.count(p))
      std::cerr<<"Parameter '"<<P->parameter_name(p)<<"' is random, but is not affected by MCMC.\n";
    if (not P->is_random_variable(p) and affected.count(p))
      std::cerr<<"Parameter '"<<P->parameter_name(p)<<"' is affected by MCMC, but is not random.\n";
  }
  std::cerr<<"\n";
}

void Sampler::go(owned_ptr<Probability_Model>& P,int subsample,const int max_iter, ostream& s_out)
{
#ifdef NDEBUG
  P->compile();
#endif

  int alignment_burnin_iterations = (int)P->load_value("alignment-burnin",10.0);

  {
    Parameters& PP = *P.as<Parameters>();

    const SequenceTree& T = PP.T();

    mcmc_init(PP,s_out);

    //--------- Determine some values for this chain -----------//
    if (subsample <= 0) subsample = 2*int(log(T.n_leaves()))+1;

    if (alignment_burnin_iterations > 0)
    {
      //      PP.branch_length_max = 2.0;
      //
      //      for(int i=0; i<PP.T->n_branches(); i++)
      //	if (PP.T->branch(i).length() > PP.branch_length_max)
      //	  PP.setlength(i, PP.branch_length_max);

      PP.set_parameter_value(PP.find_parameter("IModels.training"), new constructor("Prelude.True",0));
    }
  }

  //---------------- Run the MCMC chain -------------------//
  for(int iterations=0; iterations < max_iter; iterations++) 
  {
    Parameters& PP = *P.as<Parameters>();

    // Free temporarily fixed parameters at iteration 5
    if (iterations == alignment_burnin_iterations)
    {
      PP.set_parameter_value(PP.find_parameter("IModels.training"), new constructor("Prelude.False",0));

      PP.branch_length_max = -1;
    }

    // Change the temperature according to the pattern suggested
    if (iterations < PP.beta_series.size())
      PP.set_beta( PP.beta_series[iterations] );

    // Start learning step sizes at iteration 5
    if (iterations == 5)
      start_learning(100);

    // Stop learning step sizes at iteration 500
    if (iterations == 500)
      stop_learning(0);

    //------------------ record statistics ---------------------//
    mcmc_log(iterations, max_iter, subsample, *P.as<Parameters>(), s_out, *this, loggers);

    //------------------- move to new position -----------------//
    iterate(P,*this);


#ifdef HAVE_MPI
    //------------------ Exchange Temperatures -----------------//

    // FIXME: How am I going to allow larger proposals for different temperatures?

    // This move doesn't respect up/down at the moment
    //exchange_random_pairs(iterations,P,*this);

    exchange_adjacent_pairs(iterations,*P.as<Parameters>(),*this);
#endif
  }

  mcmc_log(max_iter, max_iter, subsample, *P.as<Parameters>(), s_out, *this, loggers);

  s_out<<"total samples = "<<max_iter<<endl;
}
}

std::ostream& operator<<(std::ostream& o, const MCMC::MoveStats& Stats) 
{
  int prec = o.precision(4);
  
  for(std::map<std::string,MCMC::Result>::const_iterator entry = Stats.begin(); entry != Stats.end();entry++)
  {
    const MCMC::Result& R = entry->second;

    // print move name
    o<<entry->first<<":  ";

    // print move stats
    for(int i=0;i<R.size();i++) {
      o<<"  X"<<i<<" = ";
      if (R.counts[i])
	o<<R.totals[i]/R.counts[i];
      else
	o<<"?";
      o<<" ["<<R.counts[i]<<"]";
    }
    o<<endl;
  }
  if (Stats.empty())
    o<<"   Transition kernel average-based statistics: no data.\n";
  o.precision(prec);
  return o;
}
