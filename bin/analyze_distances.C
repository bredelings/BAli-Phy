#include <iostream>
#include <fstream>
#include "tree.H"
#include "alignment.H"
#include "arguments.H"
#include "imodel.H"
#include "smodel.H"
#include "substitution.H"
#include "parameters.H"
#include "rng.H"
#include "logsum.H"
#include "util.H"
#include "optimize.H"
#include "setup.H"

#include <boost/numeric/ublas/io.hpp>

using std::cout;
using std::cerr;
using std::endl;

using namespace optimize;

vector<double> get_post_rate_probs(const alignment& A,const substitution::MultiRateModel& SM,const SequenceTree& T) {

  SingleIndelModel temp(-6);
  Parameters P(SM,temp,T);

  const int nbins = SM.rates().size();
  valarray<double> f(nbins);

  for(int column =0;column<A.length();column++) {
    // get the residues
    vector<int> residues(A.size2());
    for(int i=0;i<residues.size();i++)
      residues[i] = A(column,i);
    
    // get the rate likelihoods
    valarray<double> L(nbins);
    for(int r=0;r<nbins;r++)
      L[r] = SM.distribution()[r] * substitution::Pr(residues,
				       P.T,
				       SM.BaseModel(),
				       P.transition_P(r)
				       );
    // normalize rate likelihoods
    L /= L.sum();
    
    f += L;
  }

  f /= A.length();

  vector<double> f2(f.size());
  for(int i=0;i<f.size();i++)
    f2[i] = f[i];

  return f2;
}

class substitution_score: public function {
  alignment A;
  SequenceTree T;
  substitution::MultiRateModel *smodel;
  IndelModel* imodel;

public:
  double operator()(const optimize::Vector&) const;

  substitution_score(const alignment& A1,
		     const substitution::MultiRateModel& SM,
		     const SequenceTree& T1)
    : A(A1),T(T1),smodel(SM.clone()),imodel(new SingleIndelModel(-6))
  { }

  ~substitution_score() {delete smodel;delete imodel;}
};

double substitution_score::operator()(const optimize::Vector& v) const {
  if (v.size() == T.branches())
    ;
  else if (v.size() == T.branches() + smodel->parameters().size()) {
    vector<double> p(smodel->parameters().size());
    for(int i=0;i<p.size();i++)
      p[i] = v[T.branches()+i];
    smodel->parameters(p);
  }
  else 
    std::abort();


  SequenceTree T2 = T;
  for(int i=0;i<T.branches();i++) {
    if (v[i] <= 0)
      return log_0;
    T2.branch(i).length() = v[i];
  }

  Parameters P(*smodel,*imodel,T2);

  return substitution::Pr(A,P)+smodel->prior();
}

double getSimilarity(double t,substitution::MultiRateModel& SM) {
  double S = 0;

  for(int r=0;r<SM.rates().size();r++) {
    Matrix Q = SM.transition_p(t,r);
    double Sr = 0;
    for(int i=0;i<Q.size1();i++)
      Sr += SM.BaseModel().frequencies()[i]*Q(i,i);
    S += Sr * SM.distribution()[r];
  }
  
  return S;
}

Matrix getSimilarity(const SequenceTree& T,substitution::MultiRateModel& SM) {
  int n = T.leaves();
  Matrix S(n,n);

  for(int i=0;i<n;i++)
    for(int j=0;j<i;j++)
      S(i,j) = S(j,i) = getSimilarity(T.distance(i,j),SM);

  return S;
}


double getSimilarity(const alignment& A,int s1,int s2) {
  int match=0;
  int total=0;
  for(int column=0;column<A.length();column++) {
    if (A.gap(column,s1) or A.gap(column,s2)) continue;
    total++;

    if (A(column,s1) == A(column,s2))
      match++;
  }
  return double(match)/total;
}

Matrix getSimilarity(const alignment& A) {
  const int n = A.size2()/2+1;
  Matrix S(n,n);

  for(int i=0;i<n;i++)
    for(int j=0;j<i;j++)
      S(i,j) = S(j,i) = getSimilarity(A,i,j);

  return S;
}

Matrix DistanceMatrix(const SequenceTree& T) {
  const int n = T.leaves();
  Matrix D(n,n);

  for(int i=0;i<n;i++)
    for(int j=0;j<i;j++)
      D(i,j) = T.distance(i,j);

  return D;
}

Matrix C(const Matrix& M) {
  Matrix I = M;
  for(int i=0;i<I.size1();i++)
    for(int j=0;j<I.size2();j++)
      I(i,j) = 1.0 - I(i,j);

  return I;
}


std::ostream& print_lower(std::ostream& o,const vector<string>& labels, const Matrix& M) {
  assert(M.size1() == M.size2());
  for(int i=0;i<M.size1();i++) {
    std::cout<<labels[i]<<"  ";
    for(int j=0;j<i;j++)
      std::cout<<M(i,j)<<"  ";
    std::cout<<"\n";
  }
  return o;
}

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  try {
    unsigned long seed = 0;
    if (args.set("seed")) {
      seed = convertTo<unsigned long>(args["seed"]);
      myrand_init(seed);
    }
    else
      seed = myrand_init();
    cerr<<"random seed = "<<seed<<endl<<endl;
    
    cerr.precision(10);
    cout.precision(10);
    
    SequenceTree T;
    if (not args.set("tree"))
      throw myexception("Tree file not specified! (tree=<filename>)");
    T.read(args["tree"]);

    /* ----- Try to load alignment ------ */

    if (not args.set("align")) 
      throw myexception("Alignment file not specified! (align=<filename>)");

    /* ----- Alphabets to try ------ */
    alphabet dna("DNA nucleotides","AGTC","N");
    alphabet rna("RNA nucleotides","AGUC","N");
    alphabet amino_acids("Amino Acids","ARNDCQEGHILKMFPSTWYV","X");
    vector<alphabet> alphabets;
    alphabets.push_back(dna);
    alphabets.push_back(rna);
    alphabets.push_back(amino_acids);


    std::ifstream afile(args["align"].c_str());    
    alignment A;
    A.load_phylip(alphabets,afile);
    afile.close();

    link(A,T);

    std::cout<<"Using alphabet: "<<A.get_alphabet().name<<endl<<endl;

    /*------------- Show Similarity/Distances between sequences ---------*/
    Matrix S = getSimilarity(A);

    std::cout<<"similarity = \n";
    print_lower(std::cout,T.get_sequences(),S)<<"\n";

    Matrix D = C(S);
    std::cout<<"distance = \n";
    print_lower(std::cout,T.get_sequences(),D)<<"\n";

    substitution::MultiRateModel* full_smodel = get_smodel(args,A);
    std::cout<<"Using substitution model: "<<full_smodel->name()<<endl;

    /* ----- Prior & Posterior Rate Distributions (rate-bin probabilities) -------- */
    vector<double> prior_bin_f = full_smodel->distribution();
    vector<double> post_bin_f = get_post_rate_probs(A,*full_smodel,T);
    
    double prior_rate=0;
    double post_rate=0;
    for(int i=0;i<prior_bin_f.size();i++) {
      prior_rate += prior_bin_f[i]*full_smodel->rates()[i];
      post_rate += post_bin_f[i]*full_smodel->rates()[i];
    }

    for(int i=0;i<full_smodel->nrates();i++)
      cout<<"    rate"<<i<<" = "<<full_smodel->rates()[i];
    cout<<endl<<endl;

    cout<<" Prior rate = "<<prior_rate<<endl;;
    for(int i=0;i<full_smodel->nrates();i++)
      cout<<"    prior_bin_f"<<i<<" = "<<prior_bin_f[i];
    cout<<endl<<endl;

    cout<<" Posterior rate = "<<post_rate<<endl;;
    for(int i=0;i<full_smodel->nrates();i++)
      cout<<"    prior_bin_f"<<i<<" = "<<post_bin_f[i];
    cout<<endl<<endl;

    for(int i=0;i<full_smodel->nrates();i++)
      cout<<"    odds_ratio"<<i<<" = "<<post_bin_f[i]/prior_bin_f[i];
    cout<<endl<<endl;


    /* ------- Estimate branch lengths -------------*/
    SequenceTree T2 = T;
    substitution_score score(A,*full_smodel,T2);
    int delta=0;
    if (args["search"] == "model_parameters")
      delta = full_smodel->parameters().size();
    optimize::Vector start(0.1,T2.branches()+delta);
    //    for(int b=0;b<T.branches();b++)
    //      start[b] = T.branch(b).length();
    //    for(int b=T.branches();b<start.size();b++)
    //      start[b] = full_smodel->parameters()[b-T.branches()];
    optimize::Vector end = search_gradient(start,score);
    //    optimize::Vector end = search_basis(start,score);
    for(int b=0;b<T.branches();b++)
      T2.branch(b).length() = end[b];

    std::cout<<"E T = "<<T2<<std::endl;
    for(int b=T.branches();b<end.size();b++)
      std::cout<<"   p"<<b-T.branches()<<" = "<<end[b];
    std::cout<<std::endl;

    /* ------- Set up function to maximize -------- */

    Matrix S1 = getSimilarity(T,*full_smodel);
    print_lower(std::cout,T.get_sequences(),C(S1))<<"\n";

    std::cout<<"tree distances (input) = \n";
    print_lower(std::cout,T.get_sequences(),DistanceMatrix(T))<<"\n";
    std::cout<<"%difference (from input tree) = \n";
    print_lower(std::cout,T.get_sequences(),C(S1))<<"\n\n";

    Matrix S2 = getSimilarity(T2,*full_smodel);

    std::cout<<"tree distances (estimated) = \n";
    print_lower(std::cout,T.get_sequences(),DistanceMatrix(T2))<<"\n\n";
    std::cout<<"%difference (from estimated tree) = \n";
    print_lower(std::cout,T.get_sequences(),C(S2))<<"\n\n";


    delete full_smodel;
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;

}
