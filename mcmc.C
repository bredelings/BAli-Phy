#include "mcmc.H"
#include "sample.H"
#include "likelihood.H"
#include "myexception.H"
#include "rng.H"
#include "util.H"

using std::valarray;
static int total_samples = 0;


void record_move(int index,bool success) {
  move_stats[index].times++;
  if (success)
    move_stats[index].successes++;
    
}

void record_move(const string& s,bool success) {
  int index=-1;
  for(int i=0;i<move_stats.size();i++)
    if (move_stats[i].name == s) {
      index=i;
      break;
    }
  assert(index != -1);
  record_move(index,success);
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
  o<<endl<<" old  ["<<probability2(A,P)<<": "<<prior_internal(A,P)<<" + "<<substitution(A,P)<<"]"<<endl
   <<" HMM  ["<<probability3(A,P)<<": "<<prior_HMM(A,P)<<" + "<<substitution(A,P)<<"]"<<endl<<endl;
  
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

valarray<double> autocorrelation(valarray<double> v) {
  double mean = v.sum()/v.size();
  v -= mean;
  valarray<double> w(v.size()/2);
  for(int i=0;i<w.size();i++) {
    double sum1=0;
    double sum2=0;
    double sum3=0;
    for(int j=0;j<v.size()-i;j++) {
      sum1 += v[j]*v[j+i];
      sum2 += v[j]*v[j];
      sum3 += v[j+i]*v[j+i];
    }
    w[i] = sum1/sqrt(sum2*sum3);
  }
  return w;
}

void MCMC::recalc() {
  sum_enabled_weights = 0;
  for(int i=0;i<moves.size();i++)
    if (moves[i].enabled)
      sum_enabled_weights += moves[i].weight;
};

void MCMC::enable(const string& s) {
  for(int i=0;i<moves.size();i++) {
    for(int j=0;j<moves[i].attributes.size();j++)
      if (moves[i].attributes[j] == s or s == "all") {
	moves[i].enabled = true;
	break;
      }
  }

  recalc();
}

void MCMC::disable(const string& s) {
  for(int i=0;i<moves.size();i++) {
    for(int j=0;j<moves[i].attributes.size();j++)
      if (moves[i].attributes[j] == s or s == "all") {
	moves[i].enabled = false;
	break;
      }
  }

  recalc();
}

void MCMC::sample(alignment& A,Parameters& P) const {
  double r = myrandomf()*sum_enabled_weights;

  double sum = 0;
  int i = 0;
  for(;i < moves.size();i++) {

    if (not moves[i].enabled) continue;
    sum += moves[i].weight;
    if (r<sum) break;
  }
  assert(i < moves.size());
  (moves[i].m)(A,P);
}

void MCMC::add(move m,double weight,const string& keys) {
  move_info mi(m,weight,true);
  mi.attributes = split(keys,':');
  sum_enabled_weights += weight;
  moves.push_back(mi);
}

void MCMC::iterate(alignment& A,Parameters& P,const int max) {
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
      std::cout<<P.SModel().rates()(i,j)<<" ";
    std::cout<<endl;
  }
  std::cout<<endl;
  std::cout<<"frequencies = ";
  for(int i=0;i<P.get_alphabet().size();i++) {
    std::cout<<P.SModel().frequencies()[i]<<" ";
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

    sample(A2,P2);

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
      std::cout<<"ML = "<<ML_score<<endl;
      print_stats(std::cout,ML_alignment,ML_P,probability);
      ML_printed = true;
    }

    /*----------------- print diagnostic output -----------------*/

    if (iterations %200 == 0 or std::abs(p - new_p)>12) {
      print_stats(std::cerr,A,P,probability);
      print_stats(std::cerr,A2,P2,probability);

      A2.print_fasta(std::cerr);

      for(int i=0;i<move_stats.size();i++) {
	int times = move_stats[i].times;
	int successes = move_stats[i].successes;

	std::cerr<<move_stats[i].name<<" = "<<double(successes)/times<<"    ("<<successes<<"/"<<times<<")\n";
      }
    }

    /*------------------ move to new position -------------------*/
    A = A2;
    P = P2;
    p = new_p;
  }
  std::cerr<<"total samples = "<<total_samples<<endl;
}


MCMC::MCMC() : sum_enabled_weights(0), probability(probability3)
{
}
