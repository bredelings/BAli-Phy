#include <iostream>
#include <fstream>
#include "tree.H"
#include "alignment.H"
#include "smodel.H"
#include "substitution.H"
#include "substitution-cache.H"
#include "substitution-index.H"
#include "matcache.H"
#include "rng.H"
#include "logsum.H"
#include "util.H"
#include "optimize.H"
#include "setup.H"
#include "likelihood.H"
#include <boost/numeric/ublas/io.hpp>
#include "distance-methods.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;

using namespace optimize;

vector<double> get_post_rate_probs(const Matrix& P) 
{
  vector<double> f(P.size2());
  
  // compute the total probability for each sub-model
  for(int m=0;m<P.size2();m++)
    for(int c=0; c<P.size1(); c++) 
      f[m] += P(c,m);
  
  // compute the total probability
  double total=0;
  for(int m=0;m<P.size2();m++)
    total += f[m];

  // normalize sub-model probabilities
  for(int m=0;m<P.size2();m++)
    f[m] /= total;

  return f;
}

class likelihood: public function 
{
protected:
  alignment A;
  SequenceTree T;
  OwnedPointer<substitution::MultiModel> smodel;
  mutable Likelihood_Cache LC;
public:
  likelihood(const alignment& A1,
		     const substitution::MultiModel& SM,
		     const SequenceTree& T1)
    : A(A1),T(T1),smodel(SM),LC(T,*smodel,A.length())
  { }
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

double branch_likelihood::operator()(const optimize::Vector& v) const 
{
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

  smodel->set_rate(1);

  LC.invalidate_all();

  SequenceTree T2 = T;
  for(int i=0;i<T.n_branches();i++) {
    if (v[i] <= 0)
      return log_0;
    T2.branch(i).set_length(v[i]);
  }

  MatCache MC(T2,*smodel);

  return log(substitution::Pr(A,MC,T2,LC,*smodel) * smodel->prior() * prior(T2,0.2));
}


class log_branch_likelihood: public likelihood {

public:
  double operator()(const optimize::Vector&) const;

  log_branch_likelihood(const alignment& A1,
		     const substitution::MultiModel& SM,
		     const SequenceTree& T1)
    : likelihood(A1,SM,T1)
  { }

};

double log_branch_likelihood::operator()(const optimize::Vector& v) const {
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

  smodel->set_rate(1);

  LC.invalidate_all();

  SequenceTree T2 = T;
  for(int i=0;i<T.n_branches();i++)
    T2.branch(i).set_length(exp(v[i]));

  MatCache MC(T2,*smodel);

  return log(substitution::Pr(A,MC,T2,LC,*smodel) * smodel->prior() * prior(T2,0.2));
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
  const int n = A.n_sequences()/2+1;
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
  const int n = A.n_sequences()/2+1;
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
    o<<labels[i]<<"  ";
    for(int j=0;j<i;j++)
      o<<M(i,j)<<"  ";
    o<<"\n";
  }
  return o;
}

std::ostream& print_entire(std::ostream& o,vector<string> labels, const Matrix& M) {
  labels = standardize_lengths(labels);

  assert(M.size1() == M.size2());
  for(int i=0;i<M.size1();i++) {
    o<<labels[i]<<"  ";
    for(int j=0;j<M.size2();j++)
      o<<M(i,j)<<"  ";
    o<<"\n";
  }
  return o;
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("align", value<string>(),"file with sequences and initial alignment")
    ("tree",value<string>(),"file with initial tree")
    ("letters",value<string>()->default_value("full_tree"),"if set to 'star', then use a star tree for substitution")
    ("smodel",value<string>(),"substitution model")
    ("set",value<vector<string> >()->composing(),"set parameter=<value>")
    ("fix",value<vector<string> >()->composing(),"fix parameter[=<value>]")
    ("unfix",value<vector<string> >()->composing(),"un-fix parameter")
    ("frequencies",value<string>(),"comma-separated vector of frequencies to use as initial condition") 
    ("data-dir", value<string>()->default_value("Data"),"data directory")
    ("alphabet",value<string>(),"set to 'Codons' to prefer codon alphabets")
    ("search",value<string>(),"search model_parameters?")
    ;

  // positional options
  positional_options_description p;
  p.add("align", 1);
  p.add("tree", 2);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: analyze-distances <alignment-file> <tree-file> [OPTIONS]\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}

using namespace std;

int main(int argc,char* argv[]) 
{ 
  try {

    cerr.precision(10);
    cout.precision(10);

    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //---------- Initialize random seed -----------//
    unsigned long seed = 0;
    if (args.count("seed")) {
      seed = args["seed"].as<unsigned long>();
      myrand_init(seed);
    }
    else
      seed = myrand_init();
    cout<<"random seed = "<<seed<<endl<<endl;
    
    alignment A;
    SequenceTree T;
    load_A_and_T(args,A,T);

    cout<<"Using alphabet: "<<A.get_alphabet().name<<endl<<endl;

    /*------------- Show Similarity/Distances between sequences ---------*/
    cout.precision(3);

    cout<<"conserved = \n";
    print_entire(cout,T.get_sequences(),getConserved(A))<<"\n";

    Matrix S = getSimilarity(A);

    cout<<"%similarity = \n";
    print_lower(cout,T.get_sequences(),S)<<"\n";

    Matrix D = C(S);
    cout<<"%difference = \n";
    print_lower(cout,T.get_sequences(),D)<<"\n";

    OwnedPointer<substitution::MultiModel> full_smodel = get_smodel(args,A);
    cout<<"Using substitution model: "<<full_smodel->name()<<endl;
    full_smodel->set_rate(1);

    // ----- Prior & Posterior Rate Distributions (rate-bin probabilities) -------- //
    MatCache MC(T,*full_smodel);

    Likelihood_Cache LC(T,*full_smodel,A.length());

    add_leaf_seq_note(A,T.n_leaves());
    add_subA_index_note(A,T.n_branches());

    Matrix rate_probs = get_rate_probabilities(A,MC,T,LC,*full_smodel);

    vector<double> prior_bin_f = full_smodel->distribution();
    vector<double> post_bin_f = get_post_rate_probs(rate_probs);
    
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


    // ------- Estimate branch lengths -------------//
    SequenceTree T2 = T;
    branch_likelihood score(A,*full_smodel,T2);
    log_branch_likelihood score2(A,*full_smodel,T2);
    int delta=0;
    if (args.count("search") and args["search"].as<string>() == "model_parameters")
      delta = full_smodel->parameters().size();

    // Initialize starting point
    optimize::Vector start(0.1, T2.n_branches()+delta);
    for(int b=0;b<T.n_branches();b++)
      start[b] = log(T.branch(b).length());
    for(int b=T.n_branches();b<start.size();b++)
      start[b] = full_smodel->parameters()[b-T.n_branches()];


    optimize::Vector end = search_gradient(start,score2,1e-3);
    //    optimize::Vector end = search_gradient(start,score);
    //    optimize::Vector end = search_basis(start,score);
    for(int b=0;b<T.n_branches();b++)
      T2.branch(b).set_length(exp(end[b]));

    cout<<"E T = "<<T2<<endl;
    for(int b=T.n_branches();b<end.size();b++)
      cout<<"   "<<full_smodel->parameter_name(b-T.n_branches())<<" = "<<end[b];
    cout<<endl<<endl;

    /* ------- Set up function to maximize -------- */

    Matrix S1 = getSimilarity(T,*full_smodel);

    cout<<"tree distances (input) = \n";
    print_lower(cout,T.get_sequences(),DistanceMatrix(T))<<"\n";
    cout<<"%difference (from input tree) = \n";
    print_lower(cout,T.get_sequences(),C(S1))<<"\n\n";

    Matrix S2 = getSimilarity(T2,*full_smodel);

    cout<<"tree distances (estimated) = \n";
    print_lower(cout,T.get_sequences(),DistanceMatrix(T2))<<"\n\n";
    cout<<"%difference (from estimated tree) = \n";
    print_lower(cout,T.get_sequences(),C(S2))<<"\n\n";
  }
  catch (std::exception& e) {
    cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
