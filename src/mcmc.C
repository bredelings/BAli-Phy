#undef NDEBUG
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

//for the different models used in print_stats()
#include "likelihood.H"
#include "substitution.H"
#include "setup.H"           // for standardize

#include "monitor.H"
#include "proposals.H"
#include "n_indels.H"
#include "tools/parsimony.H"
#include "alignment-util.H"

#include "slice-sampling.H"
#include "timer_stack.H"

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


  void MoveGroup::iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats,int i) {
    assert(i < order.size());

    default_timer_stack.push_timer(name);

#ifndef NDEBUG
    clog<<" move = "<<name<<endl;
    clog<<"   submove = "<<moves[order[i]]->name<<endl;
#endif

    moves[order[i]]->iterate(P,Stats,suborder[i]);
    default_timer_stack.pop_timer();
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
    double r = myrandomf()*sum();

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
    default_timer_stack.push_timer(name);

#ifndef NDEBUG
    clog<<" [single] move = "<<name<<endl;
#endif

    iterations++;
    (*m)(P,Stats);
    default_timer_stack.pop_timer();
  }

  int MH_Move::reset(double lambda) {
    int l = (int)lambda;
    lambda -= l;
    return l + poisson(lambda);
  }

  void MH_Move::iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats,int) 
  {
    default_timer_stack.push_timer(name);

#ifndef NDEBUG
    clog<<" [MH] move = "<<name<<endl;
#endif

    iterations++;

    owned_ptr<Probability_Model> P2 = P;

    double ratio = (*proposal)(*P2);

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

    // Check that we have not strayed outside the bounds.
    bool in_range = true;
    for(int i=0;i<P->n_parameters();i++)
    {
      Bounds<double> range = P->get_bounds(i);
      if (not range.in_range(P->get_parameter_value(i)))
	throw myexception()<<"Parameter "<<P->parameter_name(i)<<" = "<<P->get_parameter_value(i)<<" is NOT in range "<<range;
      if (not range.in_range(P2->get_parameter_value(i)))
	in_range = false;
    }

    // Accept or Reject
    if (in_range and accept_MH(*P,*P2,ratio)) {
      result.totals[0] = 1;
      if (n == 2) {
	if (n_indices == 1) {
	  int i = p2->get_indices()[0];
	  double v1 = P->get_parameter_value(i);
	  double v2 = P2->get_parameter_value(i);
	  //      cerr<<"v1 = "<<v1<<"   v2 = "<<v2<<"\n";
	  result.totals[1] = std::abs(v2-v1);
	}
	else //currently this can only be a dirichlet proposal
	{
	  double total = 0;
	  for(int i=0;i<n_indices;i++) 
	  {
	    int j = p2->get_indices()[i];
	    double v1 = P->get_parameter_value(j);
	    double v2 = P2->get_parameter_value(j);
	    total += std::abs(log(v1/v2));
	  }
	  result.totals[1] = total;
	}
      }
      P = P2;
    }

    Stats.inc(name,result);
    default_timer_stack.pop_timer();
  }

  double Slice_Move::sample(Probability_Model& P, slice_function& slice_levels, double v1)
  {
    default_timer_stack.push_timer(name);

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
    default_timer_stack.pop_timer();
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
    if (P->is_fixed(index)) return;

    double v1 = P->get_parameter_value(index);

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

  void Dirichlet_Slice_Move::iterate(owned_ptr<Probability_Model>& P,MoveStats& Stats,int)
  {
    for(int i=0;i<indices.size();i++)
      if (P->is_fixed(indices[i])) return;

    double v1 = P->get_parameter_value(indices[n]);
    constant_sum_slice_function slice_levels_function(*P,indices,n);

    double v2 = sample(*P,slice_levels_function,v1);

    //---------- Record Statistics - -------------//
    Result result(2);
    vector<double> x = P->get_parameter_values(indices);
    double total = sum(x);
    double factor = (total - v2)/(total-v1);
    result.totals[0] = std::abs(log(v2/v1)) + (indices.size()-1)*(std::abs(log(factor)));
    result.totals[1] = slice_levels_function.count;

    Stats.inc(name,result);
  }

  Dirichlet_Slice_Move::Dirichlet_Slice_Move(const string& s, const vector<int>& indices_, int n_)
    :Slice_Move(s,0.2/indices_.size()),indices(indices_),n(n_)
  { }

  void Scale_Means_Only_Slice_Move::iterate(owned_ptr<Probability_Model>& P, MoveStats& Stats,int)
  {
    Parameters& PP = *P.as<Parameters>();
    // If any of the branch means are fixed, then bail
    for(int i=0;i<PP.n_branch_means();i++)
      if (PP.is_fixed(i)) return;

    double v1 = 0;
    scale_means_only_slice_function slice_levels_function(PP);

    double v2 = sample(PP,slice_levels_function, v1);

    //---------- Record Statistics --------------//
    Result result(2);
    result.totals[0] = std::abs(v2);
    result.totals[1] = slice_levels_function.count;

    Stats.inc(name,result);
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
      if (myrandomf() < (l2-n))
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
  default_timer_stack.push_timer(name);
  (*this)(P,Stats,order[i]);
  default_timer_stack.pop_timer();
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

  double r = myrandomf()*sum(arg);

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
  default_timer_stack.push_timer(name);
#ifndef NDEBUG
  clog<<" [single] move = "<<name<<endl;
#endif

  iterations++;
  (*m)(P,Stats,args[arg]);
  default_timer_stack.pop_timer();
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

    double b1 = P.beta[0];
    
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
	P.beta[0] = b1;
	for(int i=0;i<P.n_data_partitions();i++)
	  P[i].beta[0] = b1;
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
	
	P.beta[0] = b2;
	for(int i=0;i<P.n_data_partitions();i++)
	  P[i].beta[0] = b2;
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
  
  double beta = P.beta[0];
  double l    = log(P.likelihood());
  //  double oldbeta = beta;
  vector<double> betas;
  vector<double> L;
  vector<int> updowns;

  // Collect the Betas and log-Likelihoods in chain 0 (master)
  gather(world, beta, betas, 0);
  gather(world, l   , L    , 0);
  gather(world, P.updown, updowns, 0);


  if (proc_id == 0)
  {
    //----- Compute an order of chains in decreasing order of beta -----//
    vector<int> order = iota<int>(n_procs);
    
    sort(order.begin(), order.end(), sequence_order<double>(betas));
    std::reverse(order.begin(), order.end());
    
    MCMC::Result exchange(n_procs-1,0);
    for(int i=0;i<3;i++)
    {
      //----- Propose pairs of adjacent-temperature chains  ----//
      for(int j=0;j<n_procs-1;j++)
      {
	double b1 = betas[order[j]];
	double b2 = betas[order[j+1]];
	assert(b2 <= b1);

	double L1 = L[order[j]];
	double L2 = L[order[j+1]];

	//---- We swap both betas and order to preserve the decreasing betas ----//

	exchange.counts[j]++;
	if (uniform() < exp( (b2-b1)*(L1-L2) ) )
	{
	  std::swap(betas[order[j]],betas[order[j+1]]);
	  std::swap(order[j],order[j+1]);
	  exchange.totals[j]++;
	}
	
	
      }
    }

    // estimate average regeneration times for beta high->low->high
    MCMC::Result regeneration(n_procs,0);

    if (updowns[order[0]] == 0)
      regeneration.counts[order[0]]++;

    for(int i=0;i<n_procs;i++)
      regeneration.totals[i]++;
    

    // fraction of visitors that most recently visited highest Beta
    MCMC::Result f_recent_high(n_procs, 0); 

    updowns[order[0]] = 1;
    updowns[order.back()] = 0;

    for(int j=0;j<n_procs;j++)
      if (updowns[order[j]] == 1) {
	f_recent_high.counts[j] = 1;
	f_recent_high.totals[j] = 1;
      }
      else if (updowns[order[j]] == 0)
	f_recent_high.counts[j] = 1;

	

    Stats.inc("MC^3::Exchange",exchange);
    Stats.inc("MC^3::Frac_recent_high",f_recent_high);
    Stats.inc("MC^3::Beta_regeneration_times",regeneration);
  }

  // Broadcast the new betas for each chain
  scatter(world, betas, beta, 0);
  scatter(world, updowns, P.updown, 0);

  // cerr<<"Proc["<<proc_id<<"] changing from "<<oldbeta<<" -> "<<beta<<endl;

  P.beta[0] = beta;
  for(int i=0;i<P.n_data_partitions();i++)
    P[i].beta[0] = beta;
}
#endif


/// \brief Force identifiability by sorting certain parameters according to the order of indices[0]
///
/// \param v The values of all parameters.
/// \param indices The indices of parameter values to reorder.
///
/// Parameter values indexed by indices[i] are sorted so that the parameter values indexed
/// by indices[0] are in increasing order.
///
vector<double> make_identifiable(const vector<double>& v,const vector< vector<int> >& indices)
{
  assert(indices.size());
  int N = indices[0].size();

  vector<double> v_sub = select(v,indices[0]);

  vector<int> O = iota(N);
  std::sort(O.begin(),O.end(), sequence_order<double>(v_sub));

  vector<int> O_all = iota<int>(v.size());
  for(int i=0;i<indices.size();i++) 
  {
    assert(indices[i].size() == N);
    for(int j=0;j<N;j++) {
      // indices[i][j] -> indices[i][O[j]]
      O_all[indices[i][j]] = indices[i][O[j]];
    }
  }
  vector<double> v2 = apply_mapping(v,invert(O_all));

  return v2;
}

/// Determine the parameters of model \a M that must be sorted in order to enforce identifiability.
vector< vector< vector<int> > > get_un_identifiable_indices(const Model& M)
{
  vector< vector< vector<int> > > indices;

  int n_smodels = dynamic_cast<const Parameters&>(M).n_smodels();

  for(int i=0;i<n_smodels+1;i++) 
  {
    string prefix = "^";
    if (i>0)
      prefix = string("S")+convertToString(i) + "::";

    vector< vector<int> > DP;
    if (parameters_with_extension(M, prefix + "DP::rate*").size()  )
    {
      DP.push_back( parameters_with_extension(M, prefix + "DP::rate*") );
      DP.push_back( parameters_with_extension(M, prefix + "DP::f*") );
      indices.push_back( DP );
    }

    vector< vector<int> > M3;
    if (parameters_with_extension(M, prefix + "M3::omega*").size() )
    {
      M3.push_back( parameters_with_extension(M, prefix + "M3::omega*") );
      M3.push_back( parameters_with_extension(M, prefix + "M3::f*") );
      indices.push_back( M3 );
    }
  }

  return indices;
}

void mcmc_init(Parameters& P, ostream& s_out, ostream& s_parameters)
{
  const SequenceTree& T = *P.T;

  // Check that the Alignments and Tree are properly linked
  for(int i=0;i<P.n_data_partitions();i++) 
  {
    if (P[i].has_IModel())
      assert(P[i].A->n_sequences() == T.n_nodes() and P[i].variable_alignment()); 
    else
      assert(P[i].A->n_sequences() == T.n_leaves() and not P[i].variable_alignment());

    for(int j=0;j<T.n_leaves();j++)
      assert(T.seq(j) == P[i].A->seq(j).name);    
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
	
	s_out<<"current nucleotide frequencies = "<<endl;
	valarray<double> fT = P[i].SModel().frequencies();
	valarray<double> fN = get_nucleotide_counts_from_codon_counts(*T,fT);
	fN /= fN.sum();
	
	show_frequencies(s_out,T->getNucleotides(),fN);
	s_out<<endl<<endl;
      }
  }

  /// Output headers to log file
  s_parameters<<"iter\t";
  s_parameters<<"prior\t";
  for(int i=0;i<P.n_data_partitions();i++)
    if (P[i].variable_alignment()) s_parameters<<"prior_A"<<i+1<<"\t";
  s_parameters<<"likelihood\tlogp\tbeta\t";
  s_parameters<<P.header();
  for(int i=0;i<P.n_data_partitions();i++) {
    if (P[i].variable_alignment()) {
      s_parameters<<"\t|A"<<i+1<<"|";
      s_parameters<<"\t#indels"<<i+1;
      s_parameters<<"\t|indels"<<i+1<<"|";
    }
    s_parameters<<"\t#substs"<<i+1;
    if (dynamic_cast<const Triplets*>(&P[i].get_alphabet()))
      s_parameters<<"\t#substs(nuc)"<<i+1;
    if (dynamic_cast<const Codons*>(&P[i].get_alphabet()))
      s_parameters<<"\t#substs(aa)"<<i+1;
  }
  if (P.n_data_partitions() > 1) {
    if (P.variable_alignment())
      s_parameters<<"\t|A|\t#indels\t|indels|";
    s_parameters<<"\t#substs";
  }
  s_parameters<<"\t|T|"<<endl;

}

void mcmc_log(int iterations, int subsample, Parameters& P, 
	      ostream& s_out, ostream& s_parameters, ostream& s_trees, ostream& s_map,vector<ostream*>& files,
	      efloat_t& MAP_score,
	      const vector< vector< vector<int> > >& un_identifiable_indices,
	      const valarray<double>& weights)
{
  efloat_t prior = P.prior();
  efloat_t likelihood = P.likelihood();
  efloat_t Pr = prior * likelihood;

  // Log the alignments every 10th sample - they take a lot of space!
  bool show_alignment = (iterations%(10*subsample) == 0);

  // Don't print alignments into console log file:
  //  - Its hard to separate alignments from different partitions.
  print_stats(s_out,s_trees,P,false);

  // Print the alignments here instead
  if (show_alignment) {
    for(int i=0;i<P.n_data_partitions();i++)
    {
      (*files[5+i])<<"iterations = "<<iterations<<"\n\n";
      if (not iterations or P[i].variable_alignment())
	(*files[5+i])<<standardize(*P[i].A, *P.T)<<"\n";
    }
  }

  // Write parameter values to parameter log file
  s_parameters<<iterations<<"\t";
  s_parameters<<prior<<"\t";
  for(int i=0;i<P.n_data_partitions();i++)
    if (P[i].variable_alignment()) s_parameters<<P[i].prior_alignment()<<"\t";
  s_parameters<<likelihood<<"\t"<<Pr<<"\t"<<P.beta[0]<<"\t";

  // Sort parameter values to resolve identifiability and then output them.
  vector<double> values = P.get_parameter_values();
  for(int i=0;i<un_identifiable_indices.size();i++) 
    values = make_identifiable(values,un_identifiable_indices[i]);
  s_parameters<<join(values,'\t');

  unsigned total_length=0;
  unsigned total_indels=0;
  unsigned total_indel_lengths=0;
  unsigned total_substs=0;
  for(int i=0;i<P.n_data_partitions();i++)
  {
    unsigned x1 = P[i].A->length();
    total_length += x1;

    if (P[i].variable_alignment()) 
    {
      unsigned x2 = n_indels(*P[i].A, *P[i].T);
      total_indels += x2;

      unsigned x3 = total_length_indels(*P[i].A, *P[i].T);
      total_indel_lengths += x3;
      s_parameters<<"\t"<<x1;
      s_parameters<<"\t"<<n_indels(*P[i].A, *P[i].T);
      s_parameters<<"\t"<<x3;
    }
    unsigned x4 = n_mutations(*P[i].A, *P[i].T);
    total_substs += x4;

    s_parameters<<"\t"<<x4;
    if (const Triplets* Tr = dynamic_cast<const Triplets*>(&P[i].get_alphabet()))
      s_parameters<<"\t"<<n_mutations(*P[i].A, *P[i].T ,nucleotide_cost_matrix(*Tr));
    if (const Codons* C = dynamic_cast<const Codons*>(&P[i].get_alphabet()))
      s_parameters<<"\t"<<n_mutations(*P[i].A, *P[i].T, amino_acid_cost_matrix(*C));
  }
  if (P.n_data_partitions() > 1) {
    if (P.variable_alignment()) {
      s_parameters<<"\t"<<total_length;
      s_parameters<<"\t"<<total_indels;
      s_parameters<<"\t"<<total_indel_lengths;
    }
    s_parameters<<"\t"<<total_substs;
  }
  double mu_scale=0;
  for(int i=0;i<P.n_data_partitions();i++)
    mu_scale += P[i].branch_mean()*weights[i];
  s_parameters<<"\t"<<mu_scale*length(*P.T)<<endl;

    //---------------------- estimate MAP ----------------------//
    if (Pr > MAP_score) {
      MAP_score = Pr;
      s_map<<"iterations = "<<iterations<<"       MAP = "<<MAP_score<<"\n";
      print_stats(s_map,s_map,P);
    }

}

std::pair<int, Bounds<double> > change_bound(owned_ptr<Probability_Model>& P, 
					     const string& name, const Bounds<double>& new_bounds)
{
  int index = find_parameter(*P, name);
  if (index == -1)
    return std::pair<int, Bounds<double> >(-1, new_bounds);

  Bounds<double> orig_bounds = P->get_bounds(index);
  P->set_bounds(index, orig_bounds and new_bounds);
  Bounds<double> total_bounds = P->get_bounds(index);
  P->set_parameter_value(index, wrap(P->get_parameter_value(index), total_bounds));
#ifndef NDEBUG
  clog<<"bounds: "<<name<<" = "<<P->get_parameter_value(index)<<"  in  "<<P->get_bounds(index)<<endl;
#endif
  return std::pair<int,Bounds<double> >(index,orig_bounds);
}

void Sampler::go(owned_ptr<Probability_Model>& P,int subsample,const int max_iter,
		 ostream& s_out,ostream& s_trees, ostream& s_parameters,ostream& s_map,
		 vector<ostream*>& files)
{
  P->recalc_all();
  efloat_t MAP_score = 0;
  valarray<double> weights(P.as<Parameters>()->n_data_partitions());

  int alignment_burnin_iterations = (int)loadvalue(P->keys,"alignment-burnin",10.0);

  {
    Parameters& PP = *P.as<Parameters>();

    const SequenceTree& T = *PP.T;

    mcmc_init(PP,s_out,s_parameters);

    //--------- Determine some values for this chain -----------//
    if (subsample <= 0) subsample = 2*int(log(T.n_leaves()))+1;

    // Compute the relative number of letters in each partition.
    for(int i=0;i<weights.size();i++)
      weights[i] = max(sequence_lengths(*PP[i].A, PP.T->n_leaves()));
    weights /= weights.sum();

    if (alignment_burnin_iterations > 0)
    {
      //      PP.branch_length_max = 2.0;
      //
      //      for(int i=0; i<PP.T->n_branches(); i++)
      //	if (PP.T->branch(i).length() > PP.branch_length_max)
      //	  PP.setlength(i, PP.branch_length_max);

      for(int i=0;i<PP.n_imodels();i++)
	PP.IModel(i).set_training(true);
      PP.recalc_imodels();
    }  
  }

  /// Find parameters to fix for the first 5 iterations
  vector<std::pair<int, Bounds<double> > > restore_bounds;

  if (alignment_burnin_iterations > 0)
  {
    restore_bounds.push_back( change_bound(P, "lambda",  ::upper_bound(-4.0)  ) );
    restore_bounds.push_back( change_bound(P, "delta",   ::upper_bound(-5.0)  ) );
    restore_bounds.push_back( change_bound(P, "epsilon", ::upper_bound(-0.25) ) );
    restore_bounds.push_back( change_bound(P, "mu1",     ::upper_bound(0.5)   ) );
  }

  vector< vector< vector<int> > > un_identifiable_indices = get_un_identifiable_indices(*P);

  //---------------- Run the MCMC chain -------------------//
  for(int iterations=0; iterations < max_iter; iterations++) 
  {
    Parameters& PP = *P.as<Parameters>();

    // Free temporarily fixed parameters at iteration 5
    if (iterations == alignment_burnin_iterations)
    {
      for(int i=0;i<restore_bounds.size();i++)
	if (restore_bounds[i].first != -1)
	  P->set_bounds(restore_bounds[i].first, restore_bounds[i].second);
      restore_bounds.clear();
      
      for(int i=0;i<PP.n_imodels();i++)
	PP.IModel(i).set_training(false);
      PP.recalc_imodels();

      PP.branch_length_max = -1;
    }

    // Change the temperature according to the pattern suggested
    if (iterations < PP.beta_series.size())
      for(int i=0;i < PP.n_data_partitions();i++)
	PP.beta[0] = PP[i].beta[0] = PP.beta_series[iterations];

    // Start learning step sizes at iteration 5
    if (iterations == 5)
      start_learning(100);

    // Stop learning set sizes at iteration 500
    if (iterations == 500)
      stop_learning(0);

    //------------------ record statistics ---------------------//
    s_out<<"iterations = "<<iterations<<"\n";
    clog<<"iterations = "<<iterations<<"\n";

    if (iterations%subsample == 0)
      mcmc_log(iterations,subsample,*P.as<Parameters>(),s_out,s_parameters,s_trees,s_map,files,MAP_score,un_identifiable_indices,weights);

    if (iterations%20 == 0 or iterations < 20) {
      std::cout<<"Success statistics (and other averages) for MCMC transition kernels:\n\n";
      const MoveStats& S = *this;
      std::cout<<S<<endl;
      std::cout<<endl;
      std::cout<<"CPU Profiles for various (nested and/or overlapping) tasks:\n\n";
      std::cout<<default_timer_stack.report()<<endl;
    }

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

  /// Write a summary after the chain has finished.
  std::cout<<"Success statistics (and other averages) for MCMC transition kernels:\n\n";
  std::cout<<*(MoveStats*)this<<endl;
  std::cout<<endl;
  std::cout<<"CPU Profiles for various (nested and/or overlapping) tasks:\n\n";
  std::cout<<default_timer_stack.report()<<endl;

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

