#include <boost/numeric/ublas/config.hpp>
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


void Move::print_move_stats(int depth) const {
  for(int i=0;i<depth;i++)
    cerr<<"  ";
  cerr<<"move "<<attributes[0]<<": ";
  cerr<<"     cycles = "<<iterations;
  cerr<<"     ";

  assert(total_results.size() %2 == 0);

  for(int i=0;i<total_results.size()/2;i++) {
    cerr<<" E X"<<i<<" = "<<total_results[2*i+1]/total_results[2*i]
	<<" [n"<<i<<"="<<total_results[2*i]<<"]";
  }
  cerr<<endl;
}

void MoveGroupBase::add(double l,const Move& m,bool enabled) {
  Move* m2 = m.clone();
  if (not enabled)
    m2->disable();
  moves.push_back(m2);
  lambda.push_back(l);
}

MoveGroupBase& MoveGroupBase::operator=(const MoveGroupBase& m) {
  for(int i=0;i<moves.size();i++) {
    assert(moves[i]);
    delete moves[i];
  }

  moves = m.moves;
  lambda = m.lambda;

  for(int i=0;i<moves.size();i++)
    moves[i] = moves[i]->clone();

  return *this;
}

MoveGroupBase::MoveGroupBase(const MoveGroupBase& m) {
  moves = m.moves;
  lambda = m.lambda;

  for(int i=0;i<moves.size();i++)
    moves[i] = moves[i]->clone();
}


MoveGroupBase::~MoveGroupBase() {
  for(int i=0;i<moves.size();i++) {
    assert(moves[i]);
    delete moves[i];
  }
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

void MoveGroup::print_move_stats(int depth) const {
  Move::print_move_stats(depth);

  // Operate on children
  for(int i=0;i<moves.size();i++)
    moves[i]->print_move_stats(depth+1);
}

void MoveGroup::iterate(alignment& A,Parameters& P) {
  reset(1.0);
  for(int i=0;i<order.size();i++)
    iterate(A,P,i);
}


result_t MoveGroup::iterate(alignment& A,Parameters& P,int i) {
  assert(i < order.size());

#ifndef NDEBUG
  cerr<<" move = "<<attributes[0]<<endl;
  cerr<<"   submove = "<<moves[order[i]]->attributes[0]<<endl;
#endif

  result_t r = moves[order[i]]->iterate(A,P,suborder[i]);
  return r;
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


int MoveOne::choose() const {
  double r = myrandomf()*sum();

  double sum = 0;
  int i = 0;
  for(;i < moves.size();i++) {

    if (not moves[i]->enabled())
      continue;

    sum += lambda[i];
    if (r<sum) break;
  }
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

result_t SingleMove::iterate(alignment& A,Parameters& P,int) 
{
  cerr<<" [single]move = "<<attributes[0]<<endl;

  iterations++;
  result_t r = (*m)(A,P);

  // get the right size for results
  if (not total_results.size())
    total_results.resize(r.size(),0.0);
  assert(r.size() == total_results.size());

  total_results += r;
  return r;
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

void MoveArg::iterate(alignment& A,Parameters& P) {
  for(int i=0;i<order.size();i++)
    iterate(A,P,i);
}

result_t MoveArg::iterate(alignment& A,Parameters& P,int i) {
  return (*this)(A,P,order[i]);
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

result_t MoveEach::operator()(alignment& A,Parameters& P,int arg) {
  iterations += 1.0/args.size();
  int m = choose(arg);
  return (*(MoveArg*)moves[m])(A,P,subarg[m][arg]);
}


void MoveEach::show_enabled(int depth) const {
  Move::show_enabled(depth);
  
  for(int i=0;i<nmoves();i++)
    moves[i]->show_enabled(depth+1);
}

void MoveEach::print_move_stats(int depth) const {
  Move::print_move_stats(depth);

  // Operate on children
  for(int i=0;i<moves.size();i++)
    moves[i]->print_move_stats(depth+1);
}

result_t MoveArgSingle::operator()(alignment& A,Parameters& P,int arg) {

  cerr<<" [single]move = "<<attributes[0]<<endl;

  iterations++;
  result_t r = (*m)(A,P,args[arg]);

  // get the right size for results
  if (not total_results.size())
    total_results.resize(r.size(),0.0);
  assert(r.size() == total_results.size());

  total_results += r;

  return r;
}
    


std::ostream& operator<<(std::ostream& o,const Matrix& M) {
  ublas::operator<<(o,M);
  return o;
}

void Sampler::go(alignment& A,Parameters& P,int subsample,const int max) {
  P.recalc();
  const SequenceTree& T = P.T;
  Parameters MAP_P = P;
  alignment MAP_alignment = A;
  bool MAP_printed = true;

  // make sure that the Alignment and Tree are linked
  assert(A.num_sequences() == T.n_nodes());
  for(int i=0;i<T.n_leaves();i++)
    assert(T.seq(i) == A.seq(i).name);
  
  /*--------- Determine some values for this chain -----------*/
  if (subsample <= 0)
    subsample = 2*int(log(T.n_leaves()))+1;

  int total_samples = 0;

  double Pr_prior = P.prior(A,P);
  double Pr_likelihood = P.likelihood(A,P);
  double Pr = Pr_prior + Pr_likelihood;

  double MAP_score = Pr;

  string tag = string("sample (")+convertToString(subsample)+")";

  /*--------- Print out info about this chain -----------*/
  //  cout<<"rate matrix = \n";
  //  for(int i=0;i<P.get_alphabet().size();i++) {
  //    for(int j=0;j<P.get_alphabet().size();j++) 
  //      cout<<P.SModel().BaseModel().rates()(i,j)<<" ";
  //    cout<<endl;
  //  }
  cout<<endl;
  cout<<endl<<endl;

  if (const Codons* C = dynamic_cast<const Codons*>(&P.get_alphabet()) ) {
    cout<<"nucleotide frequencies = "<<endl;
    valarray<double> fC = P.SModel().frequencies();
    valarray<double> fN = get_nucleotide_counts_from_codon_counts(*C,fC);
    fN /= fN.sum();

    //    show_frequencies(cout,C->getNucleotides(),fN);
    //    cout<<endl<<endl;
  }
  
  //  Matrix T1 = P.SModel().BaseModel().transition_p(0.1);
  //  Matrix T1a = prod(T1,T1);
  //  Matrix T2 = P.SModel().BaseModel().transition_p(0.2);
  //  cout<<T1<<endl<<endl;
  //  cout<<T1a<<endl<<endl;
  //  cout<<T2<<endl<<endl;


  cout<<"Initial Alignment = \n";
  print_stats(cout,cout,cout,cout,A,P,"Initial");
    
  cout<<"Initial Tree = \n";
  cout<<T<<endl<<endl;

  ofstream tree_stream("trees");
  ofstream pS_stream("pS");
  ofstream pI_stream("pI");
  ofstream map_stream("MAP");
  ofstream Pr_stream("Pr");

  //---------------- Run the MCMC chain -------------------//

  for(int iterations=0; iterations < max; iterations++) {
    cerr<<"iterations = "<<iterations<<endl;;
    Pr_stream<<"iterations = "<<iterations
	     <<"    prior = "<<Pr_prior
	     <<"    likelihood = "<<Pr_likelihood
	     <<"    logp = "<<Pr
	     <<"    weight = "<<Pr*(1.0 - 1.0/P.Temp)
	     <<endl;

    //------------------ record statistics ---------------------//
    cout<<"iterations = "<<iterations<<endl;
    if (iterations%subsample == 0) {
      bool show_alignment = (iterations%(10*subsample) == 0);
      print_stats(cout,tree_stream,pS_stream,pI_stream,A,P,tag,show_alignment);
      cout<<endl<<endl;
    }

    //------------------- move to new position -----------------//
    iterate(A,P);

    Pr_prior = P.basic_prior(A,P);
    Pr_likelihood = P.basic_likelihood(A,P);
    Pr = Pr_prior + Pr_likelihood;

    //---------------------- estimate MAP ----------------------//
    if (Pr > MAP_score) {
      // arguably I could optimize these for a few iterations
      MAP_score = Pr;
      MAP_P = P;
      MAP_alignment = A;

      MAP_printed = false;
    }

    if (not MAP_printed and iterations % 50 == 0) {
      map_stream<<"iterations = "<<iterations<<"       MAP = "<<MAP_score<<endl;
      print_stats(map_stream,map_stream,map_stream,map_stream,MAP_alignment,MAP_P,"MAP");
      MAP_printed = true;
    }

    //----------------- print diagnostic output -----------------//

    if (iterations%50 == 0)
      print_move_stats();
  }
  tree_stream.close();
  map_stream.close();
  pS_stream.close();
  pI_stream.close();
  Pr_stream.close();
  cerr<<"total samples = "<<total_samples<<endl;
}



};
