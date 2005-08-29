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
  using std::cout;
  using std::clog;
  using std::string;

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

void Move::show_enabled(int depth) const {
  for(int i=0;i<depth;i++)
    cout<<"  ";
  cout<<"move "<<attributes[0]<<": ";
  if (enabled_)
    cout<<"enabled.\n";
  else 
    cout<<"DISABLED.\n";
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

void MoveGroup::show_enabled(int depth) const {
  Move::show_enabled(depth);
  
  for(int i=0;i<nmoves();i++)
    moves[i]->show_enabled(depth+1);
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


void MoveEach::show_enabled(int depth) const {
  Move::show_enabled(depth);
  
  for(int i=0;i<nmoves();i++)
    moves[i]->show_enabled(depth+1);
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

void Sampler::go(alignment& A,Parameters& P,int subsample,const int max) {
  P.recalc();
  const SequenceTree& T = P.T;

  // make sure that the Alignment and Tree are linked
  assert(A.n_sequences() == T.n_nodes());
  for(int i=0;i<T.n_leaves();i++)
    assert(T.seq(i) == A.seq(i).name);
  
  /*--------- Determine some values for this chain -----------*/
  if (subsample <= 0) subsample = 2*int(log(T.n_leaves()))+1;

  efloat_t MAP_score = 0;

  string tag = string("sample (")+convertToString(subsample)+")";

  cout<<"\n\n\n";

  if (const Triplets* T = dynamic_cast<const Triplets*>(&P.get_alphabet()) ) {
    
    cout<<"observed nucleotide frequencies = "<<endl;
    valarray<double> counts = letter_counts(A);
    valarray<double> N_counts = get_nucleotide_counts_from_codon_counts(*T,counts);

    show_frequencies(cout,T->getNucleotides(),N_counts/N_counts.sum());
    cout<<endl<<endl;

    cout<<"current nucleotide frequencies = "<<endl;
    valarray<double> fT = P.SModel().frequencies();
    valarray<double> fN = get_nucleotide_counts_from_codon_counts(*T,fT);
    fN /= fN.sum();

    show_frequencies(cout,T->getNucleotides(),fN);
    cout<<endl<<endl;
  }
  
  ofstream tree_stream("trees"), pS_stream("pS"), pI_stream("pI"), map_stream("MAP"), Pr_stream("Pr");

  //---------------- Run the MCMC chain -------------------//
  for(int iterations=0; iterations < max; iterations++) {

    //------------------ record statistics ---------------------//
    cout<<"iterations = "<<iterations<<"\n";
    clog<<"iterations = "<<iterations<<"\n";

    if (iterations%subsample == 0) {
      bool show_alignment = (iterations%(10*subsample) == 0);
      if (not (P.IModel().full_tree)) show_alignment = false;
      print_stats(cout,tree_stream,pS_stream,pI_stream,A,P,tag,show_alignment);
    }

    efloat_t Pr = P.basic_prior(A,P) * P.basic_likelihood(A,P);
    Pr_stream<<"iterations = "<<iterations
	     <<"    prior = "<<P.basic_prior(A,P)
	     <<"    likelihood = "<<P.basic_likelihood(A,P)
	     <<"    logp = "<<Pr
	     <<"    weight = "<<pow(Pr,1.0 - 1.0/P.Temp)<<std::endl;

    if (iterations%20 == 0) {
      std::cerr<<endl;
      std::cerr<<*(MoveStats*)this<<endl;
    }

    //---------------------- estimate MAP ----------------------//
    if (Pr > MAP_score) {
      MAP_score = Pr;
      map_stream<<"iterations = "<<iterations<<"       MAP = "<<MAP_score<<"\n";
      print_stats(map_stream,map_stream,map_stream,map_stream,A,P,"MAP");
    }

    //------------------- move to new position -----------------//
    iterate(A,P,*this);

  }

  // Close all the streams, and write a notification that we finished all the iterations.
  tree_stream.close();  map_stream.close();   pS_stream.close();   pI_stream.close();   Pr_stream.close();
  cout<<"total samples = "<<max<<endl;
}



};
