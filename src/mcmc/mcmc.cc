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

#include "mcmc.H"
#include "util/assert.hh"                           // for assert
#include <math.h>                                   // for log
#include <stdlib.h>                                 // for abs, abort
#include <algorithm>                                // for copy, max, fill
#include <boost/smart_ptr/intrusive_ptr.hpp>        // for intrusive_ptr
#include <iostream>                                 // for operator<<, ostream
#include <iterator>                                 // for valarray
#include <memory>                                   // for allocator_traits<...
#include <utility>                                  // for pair
#include "computation/expression/expression_ref.H"  // for expression_ref
#include "computation/object.H"                     // for intrusive_ptr_rel...
#include "models/TreeInterface.H"                   // for TreeInterface
#include "models/model.H"                           // for Model, show_param...
#include "models/parameters.H"                      // for Parameters, accep...
#include "proposals.H"                              // for Proposal2, Proposal
#include "slice-sampling.H"                         // for integer_random_va...
#include "util/math/log-double.H"                   // for operator<<, log_d...
#include "util/myexception.H"                       // for myexception
#include "util/permute.H"                           // for randomize
#include "util/range.H"                             // for sum
#include "util/rng.H"                               // for poisson, uniform
#include "util/string/split.H"                      // for split

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_MPI
#include <mpi.h>
#include <boost/mpi.hpp>
namespace mpi = boost::mpi;
#endif

extern int log_verbose;

using std::endl;

namespace MCMC {
    using std::vector;
    using std::valarray;
    using std::cerr;
    using std::clog;
    using std::string;
    using std::shared_ptr;
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

    void MoveGroup::iterate(owned_ptr<Model>& P,MoveStats& Stats) {
	reset(1.0);
	for(int i=0;i<order.size();i++)
	    iterate(P,Stats,i);
    }


    void MoveGroup::iterate(owned_ptr<Model>& P,MoveStats& Stats,int i) 
    {
	assert(i < order.size());

	if (log_verbose >= 3)
	{
	    clog<<" move = "<<name<<endl;
	    clog<<"   submove = "<<moves[order[i]]->name<<endl;
	}

	try {
	    moves[order[i]]->iterate(P,Stats,suborder[i]);
	}
	catch (myexception& e)
	{
	    std::ostringstream o;
	    o<<" move = "<<name<<"\n";
	    o<<"   submove = "<<moves[order[i]]->name<<"\n";
	    e.prepend(o.str());
	    throw;
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

    void SingleMove::iterate(owned_ptr<Model>& P,MoveStats& Stats,int) 
    {
	if (log_verbose >= 3) clog<<" [single] move = "<<name<<endl;

	iterations++;
	try {
	    (*m)(P,Stats);
	}
	catch (myexception& e)
	{
	    std::ostringstream o;
	    o<<" [single] move = "<<name<<"\n";
	    e.prepend(o.str());
	    throw;
	}
    }

    int MH_Move::reset(double lambda) {
	int l = (int)lambda;
	lambda -= l;
	return l + poisson(lambda);
    }

    void MH_Move::iterate(owned_ptr<Model>& P,MoveStats& Stats,int) 
    {
	if (log_verbose >= 3) clog<<" [MH] move = "<<name<<endl;

	iterations++;

        P->evaluate_program();
	owned_ptr<Model> P2 = P;

	log_double_t ratio = 1;
	try {
	    ratio = proposal(*P2);
	}
	catch (myexception& e)
	{
	    std::ostringstream o;
	    o<<" [MH] move = "<<name<<"  (during proposal)\n";
	    e.prepend(o.str());
	    throw;
	}

	Result result(1);

	// Accept or Reject
	if (perform_MH(*P, *P2, ratio))
	    result.totals[0] = 1;

	Stats.inc(name,result);
    }

    double Slice_Move::sample(Model& P, slice_function& slice_levels, double v1)
    {
	if (log_verbose >= 3) clog<<" [Slice] move = "<<name<<endl;

	iterations++;

	//------------- Find new value --------------//
	if (log_verbose >= 4)
	{
	    show_parameters(std::cerr,P);
	    std::cerr<<P.probability()<<" = "<<P.likelihood()<<" + "<<P.prior();
	    std::cerr<<endl<<endl;
	}

        // NOTE: Although this function returns a value, we are expecting the
        //       slice_sample routine to set the variable to the final value.
	double v2 = slice_sample(v1,slice_levels,W, 50);

	if (log_verbose >= 4)
	{
	    show_parameters(std::cerr,P);
	    std::cerr<<P.probability()<<" = "<<P.likelihood()<<" + "<<P.prior()<<std::endl;
	    std::cerr<<endl<<endl;
	    std::cerr<<" [Slice] window size = "<<W<<std::endl;
	}

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
	 n_learning_iterations(0),
	 n_tries(0),
	 total_movement(0)
    {}

    Slice_Move::Slice_Move(const string& s, const string& v)
	:Move(s,v),
	 W(1),
	 n_learning_iterations(0),
	 n_tries(0),
	 total_movement(0)
    {}

    Slice_Move::Slice_Move(const string& s, const string& v, double W_)
	:Move(s,v),
	 W(W_),
	 n_learning_iterations(0),
	 n_tries(0),
	 total_movement(0)
    {}

    Slice_Move::Slice_Move(const string& s, double W_)
	:Move(s),
	 W(W_),
	 n_learning_iterations(0),
	 n_tries(0),
	 total_movement(0)
    {}

    void Random_Variable_Slice_Move::iterate(owned_ptr<Model>& P,MoveStats& Stats,int)
    {
	if (log_verbose >= 3) clog<<" [modifiable slice] move = "<<m_index<<endl;

	double v1 = P->get_modifiable_value(m_index).as_double();

	random_variable_slice_function logp(*P, bounds_, m_index);

	double v2 = sample(*P,logp,v1);

        //---------- Record Statistics --------------//
	Result result(2);
	result.totals[0] = std::abs(v2-v1);
	result.totals[1] = logp.count;
    
	if (log_verbose >= 3) std::cerr<<" [modifiable slice] posterior evaluated "<<logp.count<<" times."<<std::endl;
	Stats.inc(name,result);
    }

    Random_Variable_Slice_Move::Random_Variable_Slice_Move(const string& s,int m,
						 const bounds<double>& b, double W_)
	:Slice_Move(s,W_), m_index(m), bounds_(b)
    {}

    Random_Variable_Slice_Move::Random_Variable_Slice_Move(const string& s, const string& v,int m,
						 const bounds<double>& b, double W_)
	:Slice_Move(s,v,W_), m_index(m), bounds_(b)
    {}

    void Integer_Random_Variable_Slice_Move::iterate(owned_ptr<Model>& P,MoveStats& Stats,int)
    {
	if (log_verbose >= 3) clog<<" [integer modifiable slice] move = "<<m_index<<endl;

	int v1 = P->get_modifiable_value(m_index).as_int();
	double x1 = double(v1)+uniform();

	integer_random_variable_slice_function logp(*P, bounds_, m_index);

	double x2 = sample(*P,logp,x1);

	//---------- Record Statistics --------------//
	Result result(2);
	result.totals[0] = std::abs(x2-x1);
	result.totals[1] = logp.count;
    
	if (log_verbose >= 3) std::cerr<<"   - Posterior evaluated "<<logp.count<<" times."<<std::endl;

	Stats.inc(name,result);
    }

    Integer_Random_Variable_Slice_Move::Integer_Random_Variable_Slice_Move(const string& s,int m,
								 const bounds<int>& b, double W_)
	:Slice_Move(s,W_), m_index(m), bounds_(b)
    {}

    Integer_Random_Variable_Slice_Move::Integer_Random_Variable_Slice_Move(const string& s, const string& v,int m,
								 const bounds<int>& b, double W_)
	:Slice_Move(s,v,W_), m_index(m), bounds_(b)
    {}

    void Dirichlet_Modifiable_Slice_Move::iterate(owned_ptr<Model>& P,MoveStats& Stats,int)
    {
	if (log_verbose >= 3) clog<<" [dirichlet modifiable slice] move"<<endl;

	double v1 = P->get_modifiable_value(indices[n]).as_double();
	constant_sum_modifiable_slice_function slice_levels_function(*P,indices,n);

	double v2 = sample(*P,slice_levels_function,v1);

	//---------- Record Statistics - -------------//
	Result result(2);
	auto x = (vector<double>) P->get_modifiable_values(indices);
	double total = sum(x);
	double factor = (total - v2)/(total-v1);
	result.totals[0] = std::abs(log(v2/v1)) + (indices.size()-1)*(std::abs(log(factor)));
	result.totals[1] = slice_levels_function.count;

	Stats.inc(name,result);
    }

    Dirichlet_Modifiable_Slice_Move::Dirichlet_Modifiable_Slice_Move(const string& s, const vector<int>& indices_, int n_)
	:Slice_Move(s,0.2/indices_.size()),indices(indices_),n(n_)
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

    void MoveArg::iterate(owned_ptr<Model>& P,MoveStats& Stats) {
	for(int i=0;i<order.size();i++)
	    iterate(P,Stats,i);
    }

    void MoveArg::iterate(owned_ptr<Model>& P,MoveStats& Stats,int i) 
    {
	(*this)(P,Stats,order[i]);
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

    void MoveEach::operator()(owned_ptr<Model>& P,MoveStats& Stats,int arg) {
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

    void MoveArgSingle::operator()(owned_ptr<Model>& P,MoveStats& Stats,int arg) 
    {
	if (log_verbose >= 3) clog<<" [single] move = "<<name<<endl;

	iterations++;
	try {
	    (*m)(P,Stats,args[arg]);
	}
	catch (myexception& e)
	{
	    std::ostringstream o;
	    o<<" [single] move = "<<name<<"\n";
	    e.prepend(o.str());
	    throw;
	}
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
		log_double_t ratio = exp_to<log_double_t>( (b2-b1)*(L1-L2) );
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
	log_double_t Pr1 = P.heated_probability();
	vector<double> Pr;
	for(int i=0;i<P.all_betas.size();i++)
	{
	    P.set_beta(P.all_betas[i]);
	    Pr.push_back(log(P.heated_probability()));
	}
	P.set_beta(P.all_betas[P.beta_index]);
	log_double_t Pr2 = P.heated_probability();

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


    void mcmc_log(long iterations, long /* max_iter*/, int /*subsample*/, Model& P, ostream& s_out, 
		  const MoveStats& /* S */, vector<Logger>& loggers)
    {
	s_out<<"iterations = "<<iterations<<"\n";
	clog<<"iterations = "<<iterations<<"\n";

	for(auto& logger: loggers)
	    logger(P,iterations);

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

    void Sampler::add_logger(const Logger& L)
    {
	loggers.push_back(L);
    }

    void Sampler::run_loggers(const Model& M, long t) const
    {
	for(auto& logger: loggers)
	    logger(M, t);
    }

    void Sampler::go(owned_ptr<Model>& P,int subsample,const int max_iter, ostream& s_out)
    {
#ifdef NDEBUG
	P->compile();
#endif

	if (owned_ptr<Parameters> PP = P.as<Parameters>())
	{
	    auto t = PP->t();

	    //--------- Determine some values for this chain -----------//
	    if (subsample <= 0) subsample = 2*int(log(t.n_leaves()))+1;
	}

	//---------------- Run the MCMC chain -------------------//
	for(int iterations=0; iterations < max_iter; iterations++) 
	{
	    if (owned_ptr<Parameters> PP = P.as<Parameters>())
	    {
		// Change the temperature according to the pattern suggested
		if (iterations < PP->PC->beta_series.size())
		    PP->set_beta( PP->PC->beta_series[iterations] );

		// Start learning step sizes at iteration 5
		if (iterations == 5)
		    start_learning(100);
	    }
	    else
	    {
		if (iterations == 0)
		    start_learning(100);
	    }

	    // Stop learning step sizes at iteration 500
	    if (iterations == 500)
		stop_learning(0);

	    //------------------ record statistics ---------------------//
	    mcmc_log(iterations, max_iter, subsample, *P, s_out, *this, loggers);

	    //------------------- move to new position -----------------//
	    iterate(P,*this);

            P->run_transition_kernels();

#ifdef HAVE_MPI
	    //------------------ Exchange Temperatures -----------------//

	    // FIXME: How am I going to allow larger proposals for different temperatures?

	    // This move doesn't respect up/down at the moment
	    //exchange_random_pairs(iterations,P,*this);

	    if (P.as<Parameters>())
		exchange_adjacent_pairs(iterations,*P.as<Parameters>(),*this);
#endif
            if (log_verbose and iterations%100 == 0)
                s_out<<(const MoveStats&)(*this);
	}

	s_out<<(const MoveStats&)(*this);

	mcmc_log(max_iter, max_iter, subsample, *P, s_out, *this, loggers);

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
