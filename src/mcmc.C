/*
   Copyright (C) 2004-2009 Benjamin Redelings

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

namespace MCMC {
  using std::valarray;
  using std::cerr;
  using std::clog;
  using std::string;
  using std::ostream;


  valarray<double> result(int l,int n) {
    valarray<double> v(0.0,2*l);
    for(int i=0;i<l;i++)
      v[2*i] = n;
    return v;
  }


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

void Move::show_enabled(ostream& o,int depth) const {
  for(int i=0;i<depth;i++)
    o<<"  ";
  o<<"move "<<name<<": ";
  if (enabled_)
    o<<"enabled.\n";
  else 
    o<<"DISABLED.\n";
}


void MoveGroupBase::add(double l,const Move& m,bool enabled) {
  moves.push_back(m);
  if (not enabled)
    moves.back()->disable();
  lambda.push_back(l);
}

double MoveGroup::sum() const {
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

void MoveGroup::iterate(Parameters& P,MoveStats& Stats) {
  reset(1.0);
  for(int i=0;i<order.size();i++)
    iterate(P,Stats,i);
}


void MoveGroup::iterate(Parameters& P,MoveStats& Stats,int i) {
  assert(i < order.size());

  default_timer_stack.push_timer(name);

#ifndef NDEBUG
  clog<<" move = "<<name<<endl;
  clog<<"   submove = "<<moves[order[i]]->name<<endl;
#endif

  moves[order[i]]->iterate(P,Stats,suborder[i]);
  default_timer_stack.pop_timer();
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

void SingleMove::iterate(Parameters& P,MoveStats& Stats,int) 
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

void MH_Move::iterate(Parameters& P,MoveStats& Stats,int) 
{
  default_timer_stack.push_timer(name);

#ifndef NDEBUG
  clog<<" [MH] move = "<<name<<endl;
#endif

  iterations++;

  Parameters P2 = P;

  double ratio = (*proposal)(P2);

  int n = 1;
  Proposal2* p2 = dynamic_cast<Proposal2*>(&(*proposal));
  if (p2 and p2->get_indices().size() == 1)
    n = 2;
  Result result(n);

#ifndef NDEBUG
  show_parameters(std::cerr,P);
  std::cerr<<P.probability()<<" = "<<P.likelihood()<<" + "<<P.prior();
  std::cerr<<endl<<endl;

  show_parameters(std::cerr,P2);
  std::cerr<<P2.probability()<<" = "<<P2.likelihood()<<" + "<<P2.prior();
  std::cerr<<endl<<endl;
#endif

  if (accept_MH(P,P2,ratio)) {
    result.totals[0] = 1;
    if (n == 2) {
      int i = p2->get_indices()[0];
      double v1 = P.parameter(i);
      double v2 = P2.parameter(i);
      //      cerr<<"v1 = "<<v1<<"   v2 = "<<v2<<"\n";
      result.totals[1] = std::abs(v2-v1);
    }
    P = P2;
  }

  Stats.inc(name,result);
  default_timer_stack.pop_timer();
}

int Slice_Move::reset(double lambda) {
  int l = (int)lambda;
  lambda -= l;
  return l + poisson(lambda);
}

void Slice_Move::iterate(Parameters& P,MoveStats& Stats,int)
{
  if (P.fixed(index)) return;

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

  double v1 = P.parameter(index);
  parameter_slice_function logp(P,index,transform,inverse);
  if (lower_bound) logp.set_lower_bound(lower);
  if (upper_bound) logp.set_upper_bound(upper);

  double w = W;
  double tv2 = slice_sample(transform(v1),logp,w,100);
  double v2 = inverse(tv2);

#ifndef NDEBUG
  show_parameters(std::cerr,P);
  std::cerr<<P.probability()<<" = "<<P.likelihood()<<" + "<<P.prior();
  std::cerr<<endl<<endl;
#endif

  //---------- Record Statistics - -------------//
  Result result(2);
  result.totals[0] = std::abs(v2-v1);
  result.totals[1] = logp.count;

  Stats.inc(name,result);
  default_timer_stack.pop_timer();
}

Slice_Move::Slice_Move(const string& s,int i,
		       bool lb,double l,bool ub,double u,double W_)
  :Move(s),index(i),
   lower_bound(lb),lower(l),upper_bound(ub),upper(u),W(W_),window(0),
   transform(slice_sampling::identity),
   inverse(slice_sampling::identity)
{}

Slice_Move::Slice_Move(const string& s, const string& v,int i,
		       bool lb,double l,bool ub,double u,double W_)
  :Move(s,v),index(i),
   lower_bound(lb),lower(l),upper_bound(ub),upper(u),W(W_),window(0),
   transform(slice_sampling::identity),
   inverse(slice_sampling::identity)
{}

Slice_Move::Slice_Move(const string& s,int i,
		       bool lb,double l,bool ub,double u,double (*W_)(const Parameters&))
  :Move(s),index(i),
   lower_bound(lb),lower(l),upper_bound(ub),upper(u),W(0),window(W_),
   transform(slice_sampling::identity),
   inverse(slice_sampling::identity)
{}

Slice_Move::Slice_Move(const string& s, const string& v,int i,
		       bool lb,double l,bool ub,double u,double (*W_)(const Parameters&))
  :Move(s,v),index(i),
   lower_bound(lb),lower(l),upper_bound(ub),upper(u),W(0),window(W_),
   transform(slice_sampling::identity),
   inverse(slice_sampling::identity)
{}

Slice_Move::Slice_Move(const string& s,int i,
		       bool lb,double l,bool ub,double u,double W_,
		       double(*f1)(double),
		       double(*f2)(double))
  :Move(s),index(i),
   lower_bound(lb),lower(l),upper_bound(ub),upper(u),W(W_),
   transform(f1),
   inverse(f2)
{}

Slice_Move::Slice_Move(const string& s, const string& v,int i,
		       bool lb,double l,bool ub,double u,double W_,
		       double(*f1)(double),
		       double(*f2)(double))
  :Move(s,v),index(i),
   lower_bound(lb),lower(l),upper_bound(ub),upper(u),W(W_),
   transform(f1),
   inverse(f2)
{}

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

void MoveArg::iterate(Parameters& P,MoveStats& Stats) {
  for(int i=0;i<order.size();i++)
    iterate(P,Stats,i);
}

void MoveArg::iterate(Parameters& P,MoveStats& Stats,int i) 
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

void MoveEach::operator()(Parameters& P,MoveStats& Stats,int arg) {
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

void MoveArgSingle::operator()(Parameters& P,MoveStats& Stats,int arg) 
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
  void exchange_random_pairs(int iterations, Parameters& P, MCMC::MoveStats& Stats)
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

void exchange_adjacent_pairs(int iterations, Parameters& P, MCMC::MoveStats& Stats)
{
  mpi::communicator world;
  world.barrier();

  int proc_id = world.rank();
  int n_procs = world.size();

  if (n_procs < 2) return;
  
  double beta = P.beta[0];
  double l    = log(P.likelihood());
  double oldbeta = beta;
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
    MCMC:Result regeneration(n_procs,0);

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

void Sampler::go(Parameters& P,int subsample,const int max_iter,
		 ostream& s_out,ostream& s_trees, ostream& s_parameters,ostream& s_map,
		 vector<ostream*>& files)
{
  P.recalc_all();

  const SequenceTree& T = *P.T;

  // make sure that the Alignment and Tree are linked
  for(int i=0;i<P.n_data_partitions();i++) {
    if (P[i].has_IModel())
      assert(P[i].A->n_sequences() == T.n_nodes() and P[i].variable_alignment()); 
    else
      assert(P[i].A->n_sequences() == T.n_leaves() and P[i].variable_alignment());

    for(int j=0;j<T.n_leaves();j++)
      assert(T.seq(j) == P[i].A->seq(j).name);    
  }

  
  //--------- Determine some values for this chain -----------//
  if (subsample <= 0) subsample = 2*int(log(T.n_leaves()))+1;

  efloat_t MAP_score = 0;

  string tag = string("sample (")+convertToString(subsample)+")";

  s_out<<"\n\n\n";

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

  s_parameters<<"iter\t";
  s_parameters<<"prior\t";
  for(int i=0;i<P.n_data_partitions();i++)
    s_parameters<<"prior_A"<<i+1<<"\t";
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

  vector<string> restore_names;
  restore_names.push_back("lambda");
  restore_names.push_back("delta");
  restore_names.push_back("epsilon");
  vector<int> restore;
  for(int i=0;i<restore_names.size();i++) 
  {
    int index = find_parameter(P,restore_names[i]);
    if (index != -1) {
      restore.push_back(index);
      P.fixed(index,true);
    }
  }
  valarray<double> weights(P.n_data_partitions());
  for(int i=0;i<weights.size();i++)
    weights[i] = max(sequence_lengths(*P[i].A, P.T->n_leaves()));
  weights /= weights.sum();

      
  //---------------- Run the MCMC chain -------------------//
  for(int iterations=0; iterations < max_iter; iterations++) 
  {
    if (iterations == 5)
      for(int i=0;i<restore.size();i++)
	P.fixed(restore[i],false);

    if (iterations < P.beta_series.size())
      for(int i=0;i < P.n_data_partitions();i++)
	P.beta[0] = P[i].beta[0] = P.beta_series[iterations];

    //------------------ record statistics ---------------------//
    s_out<<"iterations = "<<iterations<<"\n";
    clog<<"iterations = "<<iterations<<"\n";

    efloat_t prior = P.prior();
    efloat_t likelihood = P.likelihood();
    efloat_t Pr = prior * likelihood;

    if (iterations%subsample == 0) 
    {
      bool show_alignment = (iterations%(10*subsample) == 0);

      // Don't print alignments here - hard to separate alignments
      //                               from different partitions.
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

      s_parameters<<iterations<<"\t";
      s_parameters<<prior<<"\t";
      for(int i=0;i<P.n_data_partitions();i++)
	s_parameters<<P[i].prior_alignment()<<"\t";
      s_parameters<<likelihood<<"\t"<<Pr<<"\t"<<P.beta[0]<<"\t";
      s_parameters<<P.state();

      unsigned total_length=0;
      unsigned total_indels=0;
      unsigned total_indel_lengths=0;
      unsigned total_substs=0;
      for(int i=0;i<P.n_data_partitions();i++)
      {
	if (P[i].variable_alignment()) {
	  unsigned x1 = P[i].A->length();
	  total_length += x1;

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
    }

    if (iterations%20 == 0 or iterations < 20) {
      std::cout<<"Success statistics (and other averages) for MCMC transition kernels:\n\n";
      const MoveStats& S = *this;
      std::cout<<S<<endl;
      std::cout<<endl;
      std::cout<<"CPU Profiles for various (nested and/or overlapping) tasks:\n\n";
      std::cout<<default_timer_stack.report()<<endl;
    }

    //---------------------- estimate MAP ----------------------//
    if (Pr > MAP_score) {
      MAP_score = Pr;
      s_map<<"iterations = "<<iterations<<"       MAP = "<<MAP_score<<"\n";
      print_stats(s_map,s_map,P);
    }

    //------------------- move to new position -----------------//
    iterate(P,*this);


#ifdef HAVE_MPI
    //------------------ Exchange Temperatures -----------------//

    // FIXME
    // 1. How do I log the results in a meaningful way?
    // 3. What statistics should we print?
    //   (a) # of transitions between low/high temp?
    //   (b)
    // 4. How am I going to allow larger proposals for different temperatures?
    // 5. How am I going to merge results for the same temperature, while
    //     keeping track of the threading of each change through the temperatures?
    // 6. Switch to BOOST MPI

    // FIXME - let the temperatures go equilibrium, given likelihoods?

    // This move doesn't respect up/down at the moment
    //exchange_random_pairs(iterations,P,*this);

    exchange_adjacent_pairs(iterations,P,*this);
#endif
  }

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

