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

void print_stats(std::ostream& o,std::ostream& trees,std::ostream& pS,std::ostream& pI,
		 const alignment& A,const Parameters& P,const string& tag) {
  
  o<<endl;
  o<<" no A  ["<<substitution::Pr_unaligned(A,P)<<endl;
  o<<" sgsl  ["<<Pr_sgaps_sletters(A,P)<<": "<<prior_HMM_notree(A,P)<<" + "<<substitution::Pr_star_estimate(A,P)<<"]"<<endl;
  o<<" sg    ["<<Pr_sgaps_tletters(A,P)<<": "<<prior_HMM_notree(A,P)<<" + "<<substitution::Pr(A,P)<<"]"<<endl;
  o<<" sl    ["<<Pr_tgaps_sletters(A,P)<<": "<<prior_HMM(A,P)<<" + "<<substitution::Pr_star_estimate(A,P)<<"]"<<endl;
  o<<" Full  ["<<Pr_tgaps_tletters(A,P)<<": "<<prior_HMM(A,P)<<" + "<<substitution::Pr(A,P)<<"]"<<endl;
  
  double Pr_prior = P.prior(A,P);
  double Pr_likelihood = P.likelihood(A,P);
  double Pr = Pr_prior + Pr_likelihood;

  o<<"    prior = "<<Pr_prior
   <<"    likelihood = "<<Pr_likelihood
   <<"    logp = "<<Pr<<endl;

  o<<"align["<<tag<<"] = "<<endl;
  o<<standardize(A,P.T)<<endl<<endl;
  
  trees<<P.T<<endl;
  
  pS<<  "    mu = "<<P.branch_mean<<"   ";
  for(int i=0;i<P.SModel().parameters().size();i++)
    pS<<"    pS"<<i<<" = "<<P.SModel().parameters()[i];
  pS<<endl;
  
  for(int i=0;i<P.IModel().parameters().size();i++)
    pI<<"    pI"<<i<<" = "<<P.IModel().parameters()[i];
  pI<<endl;
  
  for(int i=0;i<P.SModel().nrates();i++)
    o<<"    rate"<<i<<" = "<<P.SModel().rates()[i];
  o<<endl<<endl;


  // The leaf sequences should NOT change during alignment
#ifndef NDEBUG
  for(int i=0;i<P.T.leaves();i++) {
    vector<int> s;
    for(int column=0;column<A.length();column++)
      if (not A.gap(column,i))
	s.push_back(A(column,i));

    assert(s == A.seq(i));
  }
#endif
}

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
    order.insert(order.end(),n,i);
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
    order.insert(order.end(),n,i);
  }
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
      int n = poisson(l*numbers.size());
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
  double r = myrandomf()*sum(arg);

  double sum = 0;
  int i = 0;
  for(;i < moves.size();i++) {

    if (not submove_has_arg(i,arg) or not moves[i]->enabled())
      continue;

    sum += lambda[i];
    if (r<sum) break;
  }
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
  const SequenceTree& T = P.T;
  Parameters MAP_P = P;
  alignment MAP_alignment = A;
  bool MAP_printed = true;

  // make sure that the Alignment and Tree are linked
  assert(A.num_sequences() == T.num_nodes()-1);
  for(int i=0;i<T.leaves();i++)
    assert(T.seq(i) == A.seq(i).name);
  
  /*--------- Determine some values for this chain -----------*/
  if (subsample <= 0)
    subsample = 2*int(log(T.leaves()))+1;
  const int start_after = 0;// 600*correlation_time;
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
  cout<<"frequencies = "<<endl;
  for(int i=0;i<P.get_alphabet().size();i++) {
    cout<<"f"<<P.get_alphabet().lookup(i)<<" = "<<P.SModel().BaseModel().frequencies()[i]<<endl;
  }
  cout<<endl;
  cout<<endl;
  
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

  print_stats(cout,tree_stream,pS_stream,pI_stream,A,P,tag);

  /*---------------- Run the MCMC chain -------------------*/

  for(int iterations=0; iterations < max; iterations++) {
    cerr<<"iterations = "<<iterations<<endl;;
    Pr_stream<<"iterations = "<<iterations<<
      "    prior = "<<Pr_prior<<
      "    likelihood = "<<Pr_likelihood<<
      "    logp = "<<Pr<<endl;

    /*------------------ record statistics ---------------------*/
    if (iterations > start_after) {
      cout<<"iterations = "<<iterations<<endl;
      bool full_sample = (iterations%subsample == 0);
      if (full_sample)
	print_stats(cout,tree_stream,pS_stream,pI_stream,A,P,tag);
      cout<<endl<<endl;
    }

    /*--------------------- get new position -------------------*/
    alignment A2 = A;
    Parameters P2 = P;

    iterate(A2,P2);

    double new_prior = P.prior(A2,P2);
    double new_likelihood = P.likelihood(A2,P2);
    double new_Pr = new_prior + new_likelihood;

    /*---------------------- estimate MAP ----------------------*/
    if (new_Pr > MAP_score) {
      // arguably I could optimize these for a few iterations
      MAP_score = new_Pr;
      MAP_P = P2;
      MAP_alignment = A2;

      MAP_printed = false;
    }

    if (not MAP_printed and iterations % 50 == 0) {
      map_stream<<"iterations = "<<iterations<<"       MAP = "<<MAP_score<<endl;
      print_stats(map_stream,map_stream,map_stream,map_stream,MAP_alignment,MAP_P,"MAP");
      MAP_printed = true;
    }

    /*----------------- print diagnostic output -----------------*/

    if (iterations%50 == 0 or std::abs(Pr - new_Pr)>12) {
      print_move_stats();
#ifndef NDEBUG
      print_stats(cerr,cerr,cerr,cerr,A,P,"check (A1)");
      print_stats(cerr,cerr,cerr,cerr,A2,P2,"check (A2)");

      A2.print_fasta(cerr);
#endif

    }

    /*------------------ move to new position -------------------*/
    A = A2;
    P = P2;

    Pr_prior = new_prior;
    Pr_likelihood = new_likelihood;
    Pr = new_Pr;
  }
  tree_stream.close();
  map_stream.close();
  pS_stream.close();
  pI_stream.close();
  Pr_stream.close();
  cerr<<"total samples = "<<total_samples<<endl;
}



};
