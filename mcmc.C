#include "mcmc.H"
#include "sample.H"
#include "myexception.H"
#include "rng.H"
#include "util.H"

//for the different models used in print_stats()
#include "likelihood.H"
#include "substitution.H"

namespace MCMC {
using std::valarray;


Move::Move(const string& v)
  :enabled_(true),attributes(split(v,':')),iterations(0),failures(0),successes(0)
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
    std::cout<<"  ";
  std::cout<<"move "<<attributes[0]<<": ";
  if (enabled_)
    std::cout<<"enabled.\n";
  else 
    std::cout<<"DISABLED.\n";
}


void Move::print_move_stats(int depth) const {
  for(int i=0;i<depth;i++)
    std::cerr<<"  ";
  std::cerr<<"move "<<attributes[0]<<": ";
  std::cerr<<"     cycles = "<<iterations;
  int total = successes + failures;
  if (total > 0) {
    std::cerr<<"         success = "<<double(successes)/total;
    std::cerr<<" ("<<successes<<"/"<<total<<")";
  }
  std::cerr<<endl;
}

void MoveGroupBase::add(double l,const Move& m) {
  moves.push_back(m.clone());
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
  std::cerr<<" move = "<<attributes[0]<<endl;
  std::cerr<<"   submove = "<<moves[order[i]]->attributes[0]<<endl;
#endif

  result_t r = moves[order[i]]->iterate(A,P,suborder[i]);
  if (r == success)
    successes++;
  if (r == failure)
    failures++;
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
  std::cerr<<" [single]move = "<<attributes[0]<<endl;

  iterations++;
  result_t r = (*m)(A,P);
  if (r == success)
    successes++;
  if (r == failure)
    failures++;
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


void MoveEach::add(double l,const MoveArg& m) {
  MoveGroupBase::add(l,m);

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

  std::cerr<<" [single]move = "<<attributes[0]<<endl;

  iterations++;
  result_t r = (*m)(A,P,args[arg]);
  if (r == success)
    successes++;
  if (r == failure)
    failures++;
  return r;
}
    


alignment standardize(const alignment& A, const SequenceTree& T) {
  alignment A2 = A;
  SequenceTree T2 = T;
  vector<int> mapping = T2.standardize();
  vector<int> imapping = invert(mapping);

  for(int i=0;i<A.num_sequences();i++) {
    if (imapping[i] == i) continue;

    A2.seq(i) = A.seq(imapping[i]);
    for(int column=0;column<A2.length();column++)
      A2(column,i) = A(column,imapping[i]);
  }
  return A2;
}


void print_stats(std::ostream& o,const alignment& A,const Parameters& P) {
  o<<endl;
  o<<" no A  ["<<substitution::Pr_unaligned(A,P)<<endl;
  o<<" sgsl  ["<<Pr_sgaps_sletters(A,P)<<": "<<prior_HMM_notree(A,P)<<" + "<<substitution::Pr_star_estimate(A,P)<<"]"<<endl;
  o<<" sg    ["<<Pr_sgaps_tletters(A,P)<<": "<<prior_HMM_notree(A,P)<<" + "<<substitution::Pr(A,P)<<"]"<<endl;
  o<<" sl    ["<<Pr_tgaps_sletters(A,P)<<": "<<prior_HMM(A,P)<<" + "<<substitution::Pr_star_estimate(A,P)<<"]"<<endl;
  o<<" Full  ["<<Pr_tgaps_tletters(A,P)<<": "<<prior_HMM(A,P)<<" + "<<substitution::Pr(A,P)<<"]"<<endl;

  o<<standardize(A,P.T)<<endl<<endl;

  o<<"tree = "<<P.T<<endl<<endl;

  o<<"mu = "<<P.branch_mean<<endl;

  for(int i=0;i<P.SModel().parameters().size();i++)
    o<<"    p"<<i<<" = "<<P.SModel().parameters()[i];
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

void Sampler::go(alignment& A,Parameters& P,const int max) {
  const SequenceTree& T = P.T;
  Parameters MAP_P = P;
  alignment MAP_alignment = A;
  bool MAP_printed = true;

  // make sure that the Alignment and Tree are linked
  assert(A.num_sequences() == T.num_nodes()-1);
  for(int i=0;i<T.leaves();i++)
    assert(T.seq(i) == A.seq(i).name);
  
  std::cout<<"rate matrix = \n";
  for(int i=0;i<P.get_alphabet().size();i++) {
    for(int j=0;j<P.get_alphabet().size();j++) 
      std::cout<<P.SModel().BaseModel().rates()(i,j)<<" ";
    std::cout<<endl;
  }
  std::cout<<endl;
  std::cout<<"frequencies = ";
  for(int i=0;i<P.get_alphabet().size();i++) {
    std::cout<<P.SModel().BaseModel().frequencies()[i]<<" ";
  }
  std::cout<<endl;
  std::cout<<endl;
  
  std::cout<<"Initial Alignment = \n";
  print_stats(std::cout,A,P);
    
  std::cout<<"Initial Tree = \n";
  std::cout<<T<<endl<<endl;

  const int correlation_time = 2*int(log(T.leaves()))+1;
  const int start_after = 100;// 600*correlation_time;
  int total_samples = 0;

  double Pr_prior = P.prior(A,P);
  double Pr_likelihood = P.likelihood(A,P);
  double Pr = Pr_prior + Pr_likelihood;

  double MAP_score = Pr;

  for(int iterations=0; iterations < max; iterations++) {
    std::cerr<<"iterations = "<<iterations<<
      "    prior = "<<Pr_prior<<
      "    likelihood = "<<Pr_likelihood<<
      "    logp = "<<Pr<<endl;

    /*------------------ record statistics ---------------------*/
    if (iterations > start_after) {
      if (iterations%correlation_time == 0) {
	std::cout<<"iterations = "<<iterations<<endl;
	print_stats(std::cout,A,P);
	std::cout<<endl<<endl;
      }
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
      std::cout<<"iterations = "<<iterations<<"       ML = "<<MAP_score<<endl;
      print_stats(std::cout,MAP_alignment,MAP_P);
      MAP_printed = true;
    }

    /*----------------- print diagnostic output -----------------*/

    if (iterations%50 == 0 or std::abs(Pr - new_Pr)>12) {
      print_move_stats();
#ifndef NDEBUG
      print_stats(std::cerr,A,P);
      print_stats(std::cerr,A2,P2);

      A2.print_fasta(std::cerr);
#endif

    }

    /*------------------ move to new position -------------------*/
    A = A2;
    P = P2;

    Pr_prior = new_prior;
    Pr_likelihood = new_likelihood;
    Pr = new_Pr;
  }
  std::cerr<<"total samples = "<<total_samples<<endl;
}



};
