#include "mcmc.H"
#include "sample.H"
#include "likelihood.H"
#include "myexception.H"
#include "rng.H"
#include "util.H"

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

void Move::show_enabled() const {
  std::cout<<"move "<<attributes[0]<<": ";
  if (enabled_)
    std::cout<<"enabled.\n";
  else 
    std::cout<<"DISABLED.\n";
}


void Move::print_move_stats() const {
  std::cerr<<"move "<<attributes[0]<<": ";
  std::cerr<<"     cycles = "<<iterations;
  int total = successes + failures;
  if (total > 0) {
    std::cerr<<"         success = "<<double(successes)/total;
    std::cerr<<" ("<<successes<<"/"<<total<<")";
  }
  std::cerr<<endl;
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

void MoveGroup::add(double l,const Move& m) {
  moves.push_back(m.clone());
  lambda.push_back(l);
}

void MoveGroup::print_move_stats() const {
  Move::print_move_stats();

  // Operate on children
  for(int i=0;i<moves.size();i++)
    moves[i]->print_move_stats();
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
  iterations++;
  if (r == success)
    successes++;
  if (r == failure)
    failures++;
  return r;
}

int MoveGroup::reset(double l) {
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

void MoveGroup::show_enabled() const {
  Move::show_enabled();
  
  for(int i=0;i<nmoves();i++)
    moves[i]->show_enabled();
}

MoveGroup& MoveGroup::operator=(const MoveGroup& m) {
  for(int i=0;i<moves.size();i++) {
    assert(moves[i]);
    delete moves[i];
  }

  Move::operator=(m);
  order = m.order;
  suborder = m.suborder;
  moves = m.moves;
  lambda = m.lambda;

  for(int i=0;i<moves.size();i++)
    moves[i] = moves[i]->clone();

  return *this;
}

MoveGroup::MoveGroup(const MoveGroup& m):Move(m) {
  Move::operator=(m);
  order = m.order;
  suborder = m.suborder;
  moves = m.moves;
  lambda = m.lambda;

  for(int i=0;i<moves.size();i++)
    moves[i] = moves[i]->clone();
}


MoveGroup::~MoveGroup() {
  for(int i=0;i<moves.size();i++) {
    assert(moves[i]);
    delete moves[i];
  }
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


void print_stats(std::ostream& o,const alignment& A,const Parameters& P,
		 double probability(const alignment&,const Parameters&)) {
  o<<endl;
  o<<" sgsl  ["<<Pr_sgaps_sletters(A,P)<<": "<<prior_HMM_notree(A,P)<<" + "<<substitution::Pr_star_estimate(A,P)<<"]"<<endl;
  o<<" sg    ["<<Pr_sgaps_tletters(A,P)<<": "<<prior_HMM_notree(A,P)<<" + "<<substitution::Pr(A,P)<<"]"<<endl;
  o<<" sl    ["<<Pr_tgaps_sletters(A,P)<<": "<<prior_HMM(A,P)<<" + "<<substitution::Pr_star_estimate(A,P)<<"]"<<endl;
  o<<" Full  ["<<Pr_tgaps_tletters(A,P)<<": "<<prior_HMM(A,P)<<" + "<<substitution::Pr(A,P)<<"]"<<endl;

  o<<standardize(A,P.T)<<endl<<endl;

  o<<"tree = "<<P.T<<endl<<endl;

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
  Parameters ML_P = P;
  alignment ML_alignment = A;
  bool ML_printed = true;

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
  print_stats(std::cout,A,P,probability);
    
  std::cout<<"Initial Tree = \n";
  std::cout<<T<<endl<<endl;

  const int correlation_time = 2*int(log(T.leaves()))+1;
  const int start_after = 0;// 600*correlation_time;
  int total_samples = 0;

  double p=probability(A,P);
  double ML_score = p;
  double new_p=0;

  valarray<double> v(p,5000);

  for(int iterations=0; iterations < max; iterations++) {
    std::cerr<<"iterations = "<<iterations<<"    logp = "<<p<<endl;

    /*------------------ record statistics ---------------------*/
    if (iterations > start_after) {
      if (iterations%correlation_time == 0) {
	std::cout<<"iterations = "<<iterations<<endl;
	print_stats(std::cout,A,P,probability);
	std::cout<<endl<<endl;
      }
    }

    /*--------------------- get new position -------------------*/
    alignment A2 = A;
    Parameters P2 = P;

    iterate(A2,P2);

    new_p = probability(A2,P2);

    /*---------------------- estimate MAP ----------------------*/
    if (new_p > ML_score) {
      // arguably I could optimize these for a few iterations
      ML_score = new_p;
      ML_P = P2;
      ML_alignment = A2;

      ML_printed = false;
    }

    if (not ML_printed and iterations % 100 == 0) {
      std::cout<<"iterations = "<<iterations<<"       ML = "<<ML_score<<endl;
      print_stats(std::cout,ML_alignment,ML_P,probability);
      ML_printed = true;
    }

    /*----------------- print diagnostic output -----------------*/

    if (iterations %200 == 0 or std::abs(p - new_p)>12) {
      print_stats(std::cerr,A,P,probability);
      print_stats(std::cerr,A2,P2,probability);

      A2.print_fasta(std::cerr);

      print_move_stats();
    }

    /*------------------ move to new position -------------------*/
    A = A2;
    P = P2;
    p = new_p;
  }
  std::cerr<<"total samples = "<<total_samples<<endl;
}



};
