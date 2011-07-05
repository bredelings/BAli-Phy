/*
   Copyright (C) 2004-2006,2008-2009 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

#include <iostream>
#include <fstream>
#include "tree.H"
#include "alignment.H"
#include "smodel/smodel.H"
#include "substitution.H"
#include "substitution-cache.H"
#include "substitution-index.H"
#include "rng.H"
#include "logsum.H"
#include "util.H"
#include "optimize.H"
#include "setup.H"
#include "likelihood.H"
#include <boost/numeric/ublas/io.hpp>
#include "distance-methods.H"
#include "monitor.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::vector;
using std::string;

using std::cout;
using std::cerr;
using std::endl;

using namespace optimize;

// This is a bit odd, but probably works.
vector<double> get_post_rate_probs(const vector< vector<double> >& P) 
{
  const int n_models = P[0].size();
  vector<double> f(n_models);
  
  // compute the total probability for each sub-model
  for(int c=0; c<P.size(); c++) 
    for(int m=0;m<P[c].size();m++)
      f[m] += P[c][m];
  
  // compute the total probability
  double total=0;
  for(int m=0;m<n_models;m++)
    total += f[m];

  // normalize sub-model probabilities
  for(int m=0;m<n_models;m++)
    f[m] /= total;

  return f;
}

double E_rate(const vector<vector<double> >& P, int c,const substitution::MultiModelObject& smodel)
{
  double R=0;
  for(int m=0;m<smodel.n_base_models();m++)
    R += P[c][m]*smodel.base_model(m).rate();
  return R;
}

void show_rate_probs(std::ostream& o, const vector< vector<double> >& P,
		     const substitution::MultiModelObject& smodel)
{
  for(int c=0; c<P.size(); c++)
    o<<c<<" "<<E_rate(P,c,smodel)<<endl;
}

class likelihood: public function 
{
protected:
  alignment A;
  SequenceTree T;
  owned_ptr<Model> smodel;
  vector<int> parameters;
public:
  likelihood(const alignment& A1,
	     const Model& SM,
	     const SequenceTree& T1,
	     const vector<int>& v)
    : A(A1),
      T(T1),
      smodel(SM),
      parameters(v)
  { }
};

class branch_likelihood: public likelihood {

public:
  double operator()(const optimize::Vector&) const;

  branch_likelihood(const alignment& A1,
		    const Model& SM,
		    const SequenceTree& T1,
		    const vector<int>& v)
    : likelihood(A1,SM,T1,v)
  { }

};

double branch_likelihood::operator()(const optimize::Vector& v) const 
{
  assert(v.size() == T.n_branches() + parameters.size());

  //--------------- Set branch lengths ----------------//
  SequenceTree T2 = T;
  for(int i=0;i<T.n_branches();i++) {
    if (v[i] <= 0) return log_0;
    T2.branch(i).set_length(v[i]);
  }

  //---------------- Set parameters -------------------//
  for(int i=0;i<parameters.size();i++)
    smodel->set_parameter_value(parameters[i], v[T.n_branches()+i]);

  //----- Setup cached CL's + Transition matrices -----//
  substitution::MultiModelObject M = *smodel->result_as<substitution::MultiModelObject>();
  M.set_rate(1);
  data_partition DP("DP",A,T2,M);

  return log(DP.likelihood() * smodel->prior() * prior_exponential(T2,0.2));
}


class log_branch_likelihood: public likelihood {

public:
  double operator()(const optimize::Vector&) const;

  log_branch_likelihood(const alignment& A1,
			const Model& SM,
			const SequenceTree& T1,
			const vector<int>& v)
    : likelihood(A1,SM,T1,v)
  { }
};

double log_branch_likelihood::operator()(const optimize::Vector& v) const 
{
  assert(v.size() == T.n_branches() + parameters.size());

  //--------------- Set branch lengths ----------------//
  SequenceTree T2 = T;
  for(int i=0;i<T.n_branches();i++)
    T2.branch(i).set_length(exp(v[i]));

  //---------------- Set parameters -------------------//
  for(int i=0;i<parameters.size();i++) 
    smodel->set_parameter_value(parameters[i], v[T.n_branches()+i]);

  //----- Setup cached CL's + Transition matrices -----//
  substitution::MultiModelObject M = *smodel->result_as<substitution::MultiModelObject>();
  M.set_rate(1);
  data_partition DP("DP",A,T2,M);

  return log(DP.likelihood() * smodel->prior() * prior_exponential(T2,0.2));
}


double getSimilarity(double t,const substitution::MultiModelObject& SM) 
{
  double S = 0;

  for(int m=0;m<SM.n_base_models();m++) {
    // We can't use models with more than one part
    Matrix Q = SM.transition_p(t,0,m);
    double Sm = 0;
    for(int i=0;i<Q.size1();i++)
      Sm += SM.base_model(m).frequencies()[i]*Q(i,i);
    S += Sm * SM.distribution()[m];
  }
  
  return S;
}

Matrix getSimilarity(const SequenceTree& T,const substitution::MultiModelObject& SM) {
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

void analyze_rates(const alignment& A,const SequenceTree& T,
		   substitution::MultiModelObject smodel)
{
  if (smodel.n_base_models() == 1) return;

  smodel.set_rate(1);

  data_partition DP("DP",A,T,smodel);

  vector< vector<double> > rate_probs = substitution::get_model_probabilities_by_alignment_column(DP);

  vector<double> prior_bin_f = smodel.distribution();
  
  vector<double> post_bin_f = get_post_rate_probs(rate_probs);

  show_rate_probs(cout,rate_probs,smodel);
    
  double prior_rate=0;
  double post_rate=0;
  for(int i=0;i<prior_bin_f.size();i++) {
    prior_rate += prior_bin_f[i]*smodel.base_model(i).rate();
    post_rate += post_bin_f[i]*smodel.base_model(i).rate();
  }

  for(int i=0;i<smodel.n_base_models();i++)
    cout<<"    rate"<<i<<" = "<<smodel.base_model(i).rate();
  cout<<endl<<endl;
  
  cout<<" Prior rate = "<<prior_rate<<endl;;
  for(int i=0;i<smodel.n_base_models();i++)
    cout<<"    prior_bin_f"<<i<<" = "<<prior_bin_f[i];
  cout<<endl<<endl;
  
  cout<<" Posterior rate = "<<post_rate<<endl;;
  for(int i=0;i<smodel.n_base_models();i++)
    cout<<"    post_bin_f"<<i<<" = "<<post_bin_f[i];
  cout<<endl<<endl;
  
  for(int i=0;i<smodel.n_base_models();i++)
    cout<<"    odds_ratio"<<i<<" = "<<post_bin_f[i]/prior_bin_f[i];
  cout<<endl<<endl;
}


void set_parameters(Model& M, const variables_map& args) 
{
  //-------------- Specify fixed parameters ----------------//
  vector<string> doset;
  if (args.count("set"))
    doset = args["set"].as<vector<string> >();

  // set parameters
  for(int i=0;i<doset.size();i++) {
    //parse
    vector<string> parse = split(doset[i],'=');
    if (parse.size() != 2)
      throw myexception()<<"Ill-formed initial condition '"<<doset[i]<<"'.";

    string name = parse[0];
    double value = convertTo<double>(parse[1]);

    int p=-1;
    if (p=find_parameter(M,name),p!=-1)
      M.set_parameter_value(p,value);
  }
}


void estimate_tree(const alignment& A,
		   SequenceTree& T,
		   Model& smodel,
		   const vector<int>& parameters)
{
  //------- Estimate branch lengths -------------//
  log_branch_likelihood score2(A,smodel,T,parameters);
  
  // Initialize starting point
  optimize::Vector start(0.1, T.n_branches() + parameters.size());
  for(int b=0;b<T.n_branches();b++)
    start[b] = log(T.branch(b).length());
  for(int i=0;i<parameters.size();i++)
    start[i+T.n_branches()] = smodel.get_parameter_value_as<Double>( parameters[i] );

  //    optimize::Vector end = search_gradient(start,score);
  //    optimize::Vector end = search_basis(start,score);
  optimize::Vector end = search_gradient(start,score2,1e-3);

  for(int b=0;b<T.n_branches();b++)
    T.branch(b).set_length(exp(end[b]));

  for(int i=0;i<parameters.size();i++)
    smodel.set_parameter_value(parameters[i], end[i+T.n_branches()]);
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

    //------------- Show Similarity/Distances between sequences ---------//
    cout.precision(3);

    cout<<"conserved = \n";
    print_entire(cout,T.get_leaf_labels(),getConserved(A))<<"\n";

    Matrix S = getSimilarity(A);

    cout<<"%similarity = \n";
    print_lower(cout,T.get_leaf_labels(),S)<<"\n";

    owned_ptr<Model> smodel_in = FormulaModel( get_smodel(args,A) );
    set_parameters(*smodel_in,args);
    cout<<"Using substitution model: "<<smodel_in->name()<<endl;
    // smodel_in->set_rate(1);
    show_parameters(cout,*smodel_in);
    cout<<endl;

    cout<<"input T = "<<T<<endl;
    cout<<endl;


    //----- Prior & Posterior Rate Distributions (rate-bin probabilities) -------- //
    analyze_rates(A,T,*smodel_in->result_as<substitution::MultiModelObject>());

    //------- Estimate branch lengths -------------//
    owned_ptr<Model> smodel_est = smodel_in;
    SequenceTree T2 = T;

    if (args.count("search")) {

      vector<int> parameters;
      if (args["search"].as<string>() == "smodel")
	for(int i=0;i<smodel_est->n_parameters();i++)
	  if (not smodel_est->is_fixed(i))
	    parameters.push_back(i);
      
      estimate_tree(A,T2,*smodel_est,parameters);
    

      cout<<"E T = "<<T2<<endl;
      show_parameters(cout,*smodel_est);
      cout<<endl<<endl;

      analyze_rates(A,T2,*smodel_est->result_as<substitution::MultiModelObject>());
    }

    //------- Set up function to maximize --------//
    Matrix S1 = getSimilarity(T,*smodel_in->result_as<substitution::MultiModelObject>());
    Matrix S2 = getSimilarity(T2,*smodel_est->result_as<substitution::MultiModelObject>());

    Matrix D = C(S);
    cout<<"%difference (actual) = \n";
    print_lower(cout,T.get_leaf_labels(),D)<<"\n";
    cout<<"%difference (input) = \n";
    print_lower(cout,T.get_leaf_labels(),C(S1))<<"\n";
    if (args.count("search")){
      cout<<"%difference (estimated) = \n";
      print_lower(cout,T.get_leaf_labels(),C(S2))<<"\n\n";
    }

    cout<<"tree distances (input) = \n";
    print_lower(cout,T.get_leaf_labels(),DistanceMatrix(T))<<"\n";
    if (args.count("search")){
      cout<<"tree distances (estimated) = \n";
      print_lower(cout,T.get_leaf_labels(),DistanceMatrix(T2))<<"\n\n";
    }

  }
  catch (std::exception& e) {
    cerr<<"analyze_distances: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
