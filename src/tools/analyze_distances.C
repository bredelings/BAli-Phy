#include <iostream>
#include <fstream>
#include "tree.H"
#include "alignment.H"
#include "arguments.H"
#include "smodel.H"
#include "substitution.H"
#include "matcache.H"
#include "rng.H"
#include "logsum.H"
#include "util.H"
#include "optimize.H"
#include "setup.H"
#include "likelihood.H"
#include <boost/numeric/ublas/io.hpp>
#include "distance-methods.H"
using std::cout;
using std::cerr;
using std::endl;

using namespace optimize;

vector<double> get_post_rate_probs(const alignment& A,const substitution::MultiModel& SM,const SequenceTree& T) {

  MatCache MC(T,SM);

  const int nbins = SM.n_base_models();
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
						     T,
						     SM.base_model(r),
						     MC.transition_P(r)
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

class likelihood: public function {
protected:
  alignment A;
  SequenceTree T;
  substitution::MultiModel *smodel;
public:
  likelihood(const alignment& A1,
		     const substitution::MultiModel& SM,
		     const SequenceTree& T1)
    : A(A1),T(T1),smodel(SM.clone())
  { }
  ~likelihood() {delete smodel;}
};

class branch_likelihood: public likelihood {

public:
  double operator()(const optimize::Vector&) const;

  branch_likelihood(const alignment& A1,
		     const substitution::MultiModel& SM,
		     const SequenceTree& T1)
    : likelihood(A1,SM,T1)
  { }

};

double branch_likelihood::operator()(const optimize::Vector& v) const {
  if (v.size() == T.n_branches())
    ;
  else if (v.size() == T.n_branches() + smodel->parameters().size()) {
    vector<double> p(smodel->parameters().size());
    for(int i=0;i<p.size();i++)
      p[i] = v[T.n_branches()+i];
    smodel->parameters(p);
  }
  else 
    std::abort();

  SequenceTree T2 = T;
  for(int i=0;i<T.n_branches();i++) {
    if (v[i] <= 0)
      return log_0;
    T2.branch(i).set_length(v[i]);
  }

  MatCache MC(T2,*smodel);

  return substitution::Pr(A,T2,*smodel,MC) + smodel->prior() + prior(T2,0.2);
}

double getSimilarity(double t,substitution::MultiModel& SM) {
  double S = 0;

  for(int r=0;r<SM.n_base_models();r++) {
    Matrix Q = SM.transition_p(t,r);
    double Sr = 0;
    for(int i=0;i<Q.size1();i++)
      Sr += SM.base_model(r).frequencies()[i]*Q(i,i);
    S += Sr * SM.distribution()[r];
  }
  
  return S;
}

Matrix getSimilarity(const SequenceTree& T,substitution::MultiModel& SM) {
  int n = T.n_leaves();
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


/// elements in s1 that are conserved in s2
double getConserved(const alignment& A,int s1,int s2) {
  int match=0;
  int total=0;
  for(int column=0;column<A.length();column++) {
    if (A.gap(column,s1)) continue;

    total++;

    if (A(column,s1) == A(column,s2))
      match++;
  }
  return double(match)/total;
}

Matrix getConserved(const alignment& A) {
  const int n = A.size2()/2+1;
  Matrix S(n,n);

  for(int i=0;i<n;i++)
    for(int j=0;j<n;j++)
      S(i,j) = getConserved(A,i,j);

  return S;
}

vector<string> standardize_lengths(const vector<string>& labels,char padding=' ') {
  int max=0;
  for(int i=0;i<labels.size();i++)
    if (max < labels[i].size())
      max = labels[i].size();

  vector<string> labels2 = labels;
  for(int i=0;i<labels2.size();i++) {
    labels2[i] += string(max-labels2[i].size(),padding);
  }

  return labels2;
}


std::ostream& print_lower(std::ostream& o,vector<string> labels, const Matrix& M) {
  labels = standardize_lengths(labels);

  assert(M.size1() == M.size2());
  for(int i=0;i<M.size1();i++) {
    std::cout<<labels[i]<<"  ";
    for(int j=0;j<i;j++)
      std::cout<<M(i,j)<<"  ";
    std::cout<<"\n";
  }
  return o;
}

std::ostream& print_entire(std::ostream& o,vector<string> labels, const Matrix& M) {
  labels = standardize_lengths(labels);

  assert(M.size1() == M.size2());
  for(int i=0;i<M.size1();i++) {
    std::cout<<labels[i]<<"  ";
    for(int j=0;j<M.size2();j++)
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

    alignment A;
    SequenceTree T;
    load_A_and_T(args,A,T);

    std::cout<<"Using alphabet: "<<A.get_alphabet().name<<endl<<endl;

    /*------------- Show Similarity/Distances between sequences ---------*/
    std::cout.precision(3);

    std::cout<<"conserved = \n";
    print_entire(std::cout,T.get_sequences(),getConserved(A))<<"\n";

    Matrix S = getSimilarity(A);

    std::cout<<"similarity = \n";
    print_lower(std::cout,T.get_sequences(),S)<<"\n";

    Matrix D = C(S);
    std::cout<<"distance = \n";
    print_lower(std::cout,T.get_sequences(),D)<<"\n";

    OwnedPointer<substitution::MultiModel> full_smodel = get_smodel(args,A);
    std::cout<<"Using substitution model: "<<full_smodel->name()<<endl;

    /* ----- Prior & Posterior Rate Distributions (rate-bin probabilities) -------- */
    vector<double> prior_bin_f = full_smodel->distribution();
    vector<double> post_bin_f = get_post_rate_probs(A,*full_smodel,T);
    
    double prior_rate=0;
    double post_rate=0;
    for(int i=0;i<prior_bin_f.size();i++) {
      prior_rate += prior_bin_f[i]*full_smodel->base_model(i).rate();
      post_rate += post_bin_f[i]*full_smodel->base_model(i).rate();
    }

    for(int i=0;i<full_smodel->n_base_models();i++)
      cout<<"    rate"<<i<<" = "<<full_smodel->base_model(i).rate();
    cout<<endl<<endl;

    cout<<" Prior rate = "<<prior_rate<<endl;;
    for(int i=0;i<full_smodel->n_base_models();i++)
      cout<<"    prior_bin_f"<<i<<" = "<<prior_bin_f[i];
    cout<<endl<<endl;

    cout<<" Posterior rate = "<<post_rate<<endl;;
    for(int i=0;i<full_smodel->n_base_models();i++)
      cout<<"    post_bin_f"<<i<<" = "<<post_bin_f[i];
    cout<<endl<<endl;

    for(int i=0;i<full_smodel->n_base_models();i++)
      cout<<"    odds_ratio"<<i<<" = "<<post_bin_f[i]/prior_bin_f[i];
    cout<<endl<<endl;


    /* ------- Estimate branch lengths -------------*/
    SequenceTree T2 = T;
    branch_likelihood score(A,*full_smodel,T2);
    int delta=0;
    if (args["search"] == "model_parameters")
      delta = full_smodel->parameters().size();
    optimize::Vector start(0.1,T2.n_branches()+delta);
    for(int b=0;b<T.n_branches();b++)
      start[b] = T.branch(b).length();
    for(int b=T.n_branches();b<start.size();b++)
      start[b] = full_smodel->parameters()[b-T.n_branches()];
    optimize::Vector end = search_gradient(start,score);
    //    optimize::Vector end = search_basis(start,score);
    for(int b=0;b<T.n_branches();b++)
      T2.branch(b).set_length(end[b]);

    std::cout<<"E T = "<<T2<<std::endl;
    for(int b=T.n_branches();b<end.size();b++)
      std::cout<<"   p"<<b-T.n_branches()<<" = "<<end[b];
    std::cout<<std::endl;

    /* ------- Set up function to maximize -------- */

    Matrix S1 = getSimilarity(T,*full_smodel);

    std::cout<<"tree distances (input) = \n";
    print_lower(std::cout,T.get_sequences(),DistanceMatrix(T))<<"\n";
    std::cout<<"%difference (from input tree) = \n";
    print_lower(std::cout,T.get_sequences(),C(S1))<<"\n\n";

    Matrix S2 = getSimilarity(T2,*full_smodel);

    std::cout<<"tree distances (estimated) = \n";
    print_lower(std::cout,T.get_sequences(),DistanceMatrix(T2))<<"\n\n";
    std::cout<<"%difference (from estimated tree) = \n";
    print_lower(std::cout,T.get_sequences(),C(S2))<<"\n\n";
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
