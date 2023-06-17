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
