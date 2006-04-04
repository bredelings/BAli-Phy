#include <boost/numeric/ublas/io.hpp>
#include <iostream>

#include "mcmc.H"
#include "sample.H"
#include "myexception.H"
#include "rng.H"
#include "util.H"

//for the different models used in print_stats()
#include "likelihood.H"
#include "substitution.H"
#include "setup.H"           // for standardize

#include "monitor.H"

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

  std::ostream& operator<<(std::ostream& o, const MoveStats& Stats) 
  {
    int prec = o.precision(4);

    foreach(entry,Stats) 
    {
      const Result& R = entry->second;

      // print move name
      o<<entry->first<<":  ";

      // print move stats
      for(int i=0;i<R.size();i++) {
	o<<"  X"<<i<<" = "<<R.totals[i]/R.counts[i];
	o<<" ["<<R.counts[i]<<"]";
      }
      o<<endl;
    }
    o.precision(prec);
    return o;
  }

  Move::Move(const string& v)
    :enabled_(true),attributes(split(v,':')),iterations(0)
  { }

  void Move::enable(const string& s) {
    if (s == "all")
      enable();
    else 
      for(int j=0;j<attributes.size();j++)
	if (attributes[j] == s) {
	  enable();
	  break;
	}
  }
  
  void Move::disable(const string& s) {
    if (s == "all")
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
  o<<"move "<<attributes[0]<<": ";
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

void MoveGroup::iterate(alignment& A,Parameters& P,MoveStats& Stats) {
  reset(1.0);
  for(int i=0;i<order.size();i++)
    iterate(A,P,Stats,i);
}


void MoveGroup::iterate(alignment& A,Parameters& P,MoveStats& Stats,int i) {
  assert(i < order.size());

#ifndef NDEBUG
  clog<<" move = "<<attributes[0]<<endl;
  clog<<"   submove = "<<moves[order[i]]->attributes[0]<<endl;
#endif

  moves[order[i]]->iterate(A,P,Stats,suborder[i]);
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
    throw myexception()<<"move "<<attributes[0]<<" has no enabled submoves";

  return i;
}

void MoveOne::getorder(double l) {
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

void SingleMove::iterate(alignment& A,Parameters& P,MoveStats& Stats,int) 
{
#ifndef NDEBUG
  clog<<" [single]move = "<<attributes[0]<<endl;
#endif

  iterations++;
  (*m)(A,P,Stats);
}

int MoveArg::reset(double l) {
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

void MoveArg::iterate(alignment& A,Parameters& P,MoveStats& Stats) {
  for(int i=0;i<order.size();i++)
    iterate(A,P,Stats,i);
}

void MoveArg::iterate(alignment& A,Parameters& P,MoveStats& Stats,int i) {
  (*this)(A,P,Stats,order[i]);
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
    throw myexception()<<"move "<<attributes[0]<<" has no enabled submoves";

  // is sum(arg) > 0 ?
  if (i >= moves.size())
    assert(i < moves.size());

  return i;
}

void MoveEach::operator()(alignment& A,Parameters& P,MoveStats& Stats,int arg) {
  iterations += 1.0/args.size();
  int m = choose(arg);
  MoveArg* temp = dynamic_cast<MoveArg*>(&*moves[m]);
  if (not temp)
    std::abort();
  else
    (*temp)(A,P,Stats,subarg[m][arg]);
}


void MoveEach::show_enabled(ostream& o,int depth) const {
  Move::show_enabled(o,depth);
  
  for(int i=0;i<nmoves();i++)
    moves[i]->show_enabled(o,depth+1);
}

void MoveArgSingle::operator()(alignment& A,Parameters& P,MoveStats& Stats,int arg) 
{
#ifndef NDEBUG
  clog<<" [single]move = "<<attributes[0]<<endl;
#endif

  iterations++;
  (*m)(A,P,Stats,args[arg]);
}
    


std::ostream& operator<<(std::ostream& o,const Matrix& M) {
  ublas::operator<<(o,M);
  return o;
}

void Sampler::go(alignment& A,Parameters& P,int subsample,const int max,
		 ostream& s_out,ostream& s_trees, ostream& s_parameters,ostream& s_map)
{
  P.recalc();
  const SequenceTree& T = P.T;

  // make sure that the Alignment and Tree are linked
  assert(A.n_sequences() == T.n_nodes());
  for(int i=0;i<T.n_leaves();i++)
    assert(T.seq(i) == A.seq(i).name);
  
  //--------- Determine some values for this chain -----------//
  if (subsample <= 0) subsample = 2*int(log(T.n_leaves()))+1;

  efloat_t MAP_score = 0;

  string tag = string("sample (")+convertToString(subsample)+")";

  s_out<<"\n\n\n";

  if (const Triplets* T = dynamic_cast<const Triplets*>(&P.get_alphabet()) ) 
  {
    s_out<<"observed nucleotide frequencies = "<<endl;
    valarray<double> counts = letter_counts(A);
    valarray<double> N_counts = get_nucleotide_counts_from_codon_counts(*T,counts);

    show_frequencies(s_out,T->getNucleotides(),N_counts/N_counts.sum());
    s_out<<endl<<endl;

    s_out<<"current nucleotide frequencies = "<<endl;
    valarray<double> fT = P.SModel().frequencies();
    valarray<double> fN = get_nucleotide_counts_from_codon_counts(*T,fT);
    fN /= fN.sum();

    show_frequencies(s_out,T->getNucleotides(),fN);
    s_out<<endl<<endl;
  }
  
  s_parameters<<"iter\t";
  s_parameters<<"prior\tlikelihood\tlogp\tbeta\t";
  s_parameters<<"mu\t"<<P.SModel().header()<<"\t";
  s_parameters<<P.IModel().header()<<endl;

  //---------------- Run the MCMC chain -------------------//
  for(int iterations=0; iterations < max; iterations++) {

    //------------------ record statistics ---------------------//
    s_out<<"iterations = "<<iterations<<"\n";
    clog<<"iterations = "<<iterations<<"\n";

    efloat_t prior = P.basic_prior(A,P);
    efloat_t likelihood = P.basic_likelihood(A,P);
    efloat_t Pr = prior * likelihood;

    if (iterations%subsample == 0) {
      bool show_alignment = (iterations%(10*subsample) == 0);
      if (not (P.IModel().full_tree)) show_alignment = false;
      print_stats(s_out,s_trees,A,P,show_alignment);

      s_parameters<<iterations<<"\t";
      s_parameters<<prior<<"\t"<<likelihood<<"\t"<<Pr<<"\t"<<P.beta[0]<<"\t";
      s_parameters<<P.branch_mean<<"\t"<<P.SModel().state()<<"\t";
      s_parameters<<P.IModel().state()<<endl;
    }

    if (iterations%20 == 0) {
      std::cerr<<endl;
      std::cerr<<*(MoveStats*)this<<endl;
    }

    //---------------------- estimate MAP ----------------------//
    if (Pr > MAP_score) {
      MAP_score = Pr;
      s_map<<"iterations = "<<iterations<<"       MAP = "<<MAP_score<<"\n";
      print_stats(s_map,s_map,A,P);
    }

    //------------------- move to new position -----------------//
    iterate(A,P,*this);

  }

  s_out<<"total samples = "<<max<<endl;
}



}
