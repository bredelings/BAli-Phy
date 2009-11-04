/*
   Copyright (C) 2004-2005,2007-2009 Benjamin Redelings

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
#include <algorithm>
#include <string>
#include <vector>
#include <list>
#include <map>
#include <cmath>
#include <fstream>
#include <boost/numeric/ublas/matrix.hpp>
#include "statistics.H"

#include "sequencetree.H"
#include "util.H"
#include "tree-util.H"
#include "tree-dist.H"

#include <boost/program_options.hpp>

namespace ublas = boost::numeric::ublas;
namespace po = boost::program_options;
using po::variables_map;

using namespace std;
using namespace statistics;

ublas::matrix<double> remove_duplicates(const ublas::matrix<double>& D)
{
  assert(D.size1() == D.size2());
  int N = D.size1();

  // find duplicates
  vector<int> remove;
  for(int i=0;i<N;i++) {
    bool found=false;
    for(int j=0;j<i and not found;j++)
      if (D(i,j) == 0.0)
	found=true;
    if (found)
      remove.push_back(i);
  }

  if (not remove.size()) return D;

  cerr<<"removing "<<remove.size()<<" duplicates."<<endl;

  // compute mapping of old to new indices
  vector<int> indices(N-remove.size());
  int k=0;
  int p=0;
  for(int i=0;i<indices.size();i++)
  {
    while (p<remove.size() and k==remove[p]) {
      k++;
      p++;
    }
    indices[i] = k++;
  }

  // construct the new matrix
  ublas::matrix<double> D2(indices.size(),indices.size());

  for(int i=0;i<D2.size1();i++)
    for(int j=0;j<D2.size2();j++)
      D2(i,j) = D(indices[i],indices[j]);

  return D2;
}

SequenceTree unit_branch_tree(const SequenceTree& T1)
{
  SequenceTree T2 = T1;
  for(int i=0;i<T2.n_branches();i++)
    T2.branch(i).set_length(1.0);
  return T2;
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description invisible("Invisible options");
  invisible.add_options()
    ("files",value<vector<string> >()->composing(),"tree samples to examine")
    ;

  options_description input("Input options");
  input.add_options()
    ("help", "produce help message")
    ("skip",value<unsigned>()->default_value(0),"number of tree samples to skip")
    ("max",value<int>(),"maximum number of tree samples to read")
    ("sub-sample",value<int>()->default_value(1),"factor by which to sub-sample")
    ("verbose,v","Output more log messages on stderr.")
    ;

  options_description analysis("Analysis options");
  analysis.add_options()
    ("analysis", value<string>()->default_value("matrix"), "Analysis: matrix, autocorrelation, diameter, compare, convergence, converged,")
    ("metric", value<string>()->default_value("topology"),"Tree distance: topology, branch, internal-branch")
    ("remove-duplicates","[matrix]: disallow zero distances  between points.")
    ("max-lag",value<int>(),"[autocorrelation]: max lag to consider.")
    ("CI",value<double>()->default_value(0.95),"Confidence interval size.")
    ("converged",value<double>()->default_value(0.05),"Comma-separated quantiles of distance required for converged? (smaller is more strict).")
    ("mean", "Show mean and standard deviation")
    ("median", "Show median and confidence interval")
    ("minmax", "Show minumum and maximum distances")
    ;

  options_description visible("All options");
  visible.add(input).add(analysis);

  options_description all("All options");
  all.add(invisible).add(input).add(analysis);

  // positional options
  positional_options_description p;
  p.add("analysis", 1);
  p.add("files", -1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: trees-distances <analysis> trees-file1 [trees-file2 ...]\n";
    cout<<"Compute autocorrelations or other functions of tree distances.\n\n";
    cout<<visible<<"\n";
    exit(0);
  }

  if (args.count("verbose")) log_verbose = 1;

  return args;
}

typedef double (*tree_metric_fn)(const tree_record&,const tree_record&);

ublas::matrix<double> distances(const vector<tree_record>& trees, 
				tree_metric_fn metric_fn
				)
{
    ublas::matrix<double> D(trees.size(),trees.size());

    // calculate the pairwise distances
    for(int i=0;i<trees.size();i++) {
      for(int j=0;j<i;j++)
	D(i,j) = D(j,i) = metric_fn(trees[i],trees[j]);
      D(i,i) = 0;
    }
    return D;
}

double distance(const tree_record& T, 
		const vector<tree_record>& trees,
		tree_metric_fn metric_fn
		)
{
  double D=0;
  for(int i=0;i<trees.size();i++)
    D += metric_fn(T,trees[i]);
  D /= trees.size();
  return D;
}

void check_supplied_filenames(int n,vector<string>& files)
{
  if (files.size() == n-1)
    files.insert(files.begin(),"-");
  if (files.size() == n)
    return;

  if (files.size() < n)
    throw myexception()<<"Wanted "<<n<<" filenames, but got only "<<files.size()<<".";
  if (files.size() > n)
    cerr<<"Warning: ignoring "<<files.size()-n<<" extra filenames."<<endl;
}

double max(const valarray<double>& v)
{
  double m=v[0];
  for(int i=1;i<v.size();i++)
    m = std::max(m,v[i]);
  return m;
}

double min(const valarray<double>& v)
{
  double m=v[0];
  for(int i=1;i<v.size();i++)
    m = std::min(m,v[i]);
  return m;
}

void report_distances(const valarray<double>& distances,
		      const string& name,
		      variables_map& args
		      )
{
  if (not distances.size()) return;

  bool show_mean = args.count("mean");
  bool show_median = args.count("median");
  bool show_minmax = args.count("minmax");

  if (not show_mean and not show_median and not show_minmax)
    show_median = true;

  if (show_minmax)
    cout<<"    "<<name<<" in ["<<min(distances)<<", "<<max(distances)<<"]"<<endl;
  if (show_mean){
      cout<<"  E "<<name<<" = "<<distances.sum()/distances.size();
      cout<<"   [+- "<<sqrt(Var(distances))<<"]"<<endl;
  }
  if (show_median) {
    double P = args["CI"].as<double>();
    pair<double,double> interval = confidence_interval(distances,P);
    cout<<"    "<<name<<" ~ "<<median(distances);
    cout<<"   ("<<interval.first<<", "<<interval.second<<")"<<endl;
  }
}

// We consider 4 random distributions:
//  1. D(t[i],t[j])
//  2. E_t[i] D(t[i],t[j])
//  3. E_t[j] D(t[i],t[j])
//  4. E_t[i],t[j] D(t[i],t[j])
//
//  If t[i] and t[j] have the same distribution then #2 == #3.
//  Also, #4 is constant so it is not really a distribution.

void diameter(const ublas::matrix<double>& D,const string& name,variables_map& args)
{
  const unsigned N = D.size1();

  int k=0;
  valarray<double> d1(0.0, N);
  valarray<double> d11(0.0, N*(N-1)/2);

  for(int i=0;i<N;i++)
    for(int j=0;j<i;j++) {
      d1[i] += D(i,j);
      d1[j] += D(i,j);
      d11[k++] = D(i,j);
    }
  d1 /= (N-1);


  string name1 = string("D")+name+name + "  ";
  string name2 = string("D")+name+"("+name+")";
  report_distances(d11,name1, args);cout<<endl;
  report_distances(d1 ,name2, args);
}

double fair_probability_x_less_than_y(const valarray<double>& x,const valarray<double>& y)
{
  return 0.5*(probability_x_less_than_y(x,y) + 1.0 - probability_x_less_than_y(y,x));
}


int topology_distance2(const tree_record& t1, const tree_record& t2)
{
  assert(t1.n_leaves() == t2.n_leaves());

  unsigned n1 = t1.n_internal_branches();
  unsigned n2 = t2.n_internal_branches();

  // Accumulate distances for T1 partitions
  unsigned shared=0;

  int i=0,j=0;
  while (1) {
    if (i >= n1) break;
    if (j >= n2) break;

    if (t1.partitions[i] == t2.partitions[j]) {
      i++;
      j++;
      shared++;
    }
    else if (t1.partitions[i] < t2.partitions[j])
      i++;
    else
      j++;
  }

  return (n1-shared) + (n2-shared);
}

double robinson_foulds_distance2(const tree_record& t1, const tree_record& t2)
{
  return topology_distance2(t1,t2) * 0.5;
}

double branch_distance2(const tree_record& t1, const tree_record& t2)
{
  return topology_distance2(t1,t2) * 0.5;
}

double internal_branch_distance2(const tree_record& t1, const tree_record& t2)
{
  return topology_distance2(t1,t2) * 0.5;
}

int main(int argc,char* argv[]) 
{ 
  try 
  {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    string analysis = args["analysis"].as<string>();

    unsigned skip = args["skip"].as<unsigned>();

    int subsample=args["sub-sample"].as<int>();

    int max = -1;
    if (args.count("max"))
      max = args["max"].as<int>();

    tree_metric_fn metric_fn = NULL;
    string metric = args["metric"].as<string>();
    if (metric == "topology" or metric == "RF")
      metric_fn = &robinson_foulds_distance2;
    else if (metric == "branch" or metric == "branches")
      metric_fn = &branch_distance2;
    else if (metric == "internal-branch")
      metric_fn = &internal_branch_distance2;
    else
      throw myexception()<<"Metric '"<<metric<<"' not implemented.";
      
    //----------- read in trees ------------//
    vector<string> files;
    if (args.count("files"))
      files = args["files"].as<vector<string> >();


    //----------- task "matrix" ------------//
    if (analysis == "matrix") 
    {
      check_supplied_filenames(1,files);
      tree_sample trees;
      if (files[0] == "-")
	trees = tree_sample(std::cin,skip,subsample,max);
      else
	trees = tree_sample(files[0],skip,subsample,max);
      //      tree_sample trees(files[0],skip,subsample,max);

      ublas::matrix<double> D = distances(trees,metric_fn);

      if (args.count("remove-duplicates"))
	D = remove_duplicates(D);

      for(int i=0;i<D.size1();i++) {
	vector<double> v(D.size2());
	for(int j=0;j<v.size();j++)
	  v[j] = D(i,j);
	cout<<join(v,'\t')<<endl;
      }
    }

    else if (analysis == "autocorrelation") 
    {
      check_supplied_filenames(1,files);
      tree_sample trees(files[0],skip,subsample,max);

      ublas::matrix<double> D = distances(trees,metric_fn);
      
      // set the window size
      int max_lag = int( double(trees.size()/20.0 + 1.0 ) );
      if (args.count("max-lag"))
	max_lag = min(max_lag,args["max-lag"].as<int>());
      
      // bound the max_lag
      if (max_lag >= trees.size()/2)
	max_lag = trees.size()/2;

      // write out the average distances
      valarray<double> distances(0.0,max_lag);
      for(int d=0;d<distances.size();d++) {
	double dd = 0;
	for(int i=0;i+d<trees.size();i++)
	  dd += D(i,i+d);
	distances[d] = dd/(trees.size() - d);
      }
      
      // write out the average distances
      for(int i=0;i<distances.size();i++)
	cout<<i<<"   "<<distances[i]<<endl;
    }

    else if (analysis == "diameter") 
    {
      check_supplied_filenames(1,files);
      tree_sample trees(files[0],skip,subsample,max);
      if (trees.size() < 2)
	throw myexception()<<"diameter: only 1 point in set.";

      ublas::matrix<double> D = distances(trees,metric_fn);
      
      diameter(D,"1",args);
    }

    else if (analysis == "compare") 
    {
      check_supplied_filenames(2,files);

      tree_sample trees1(files[0],skip,subsample,max);
      tree_sample trees2(files[1],skip,subsample,max);
      const unsigned N1 = trees1.size();
      const unsigned N2 = trees2.size();

      tree_sample both = trees1;
      for(int i=0;i<trees2.size();i++)
	both.add_tree(trees2.trees[i]);

      ublas::matrix<double> D1 = distances(trees1,metric_fn);
      ublas::matrix<double> D2 = distances(trees2,metric_fn);
      ublas::matrix<double> D  = distances(both,metric_fn);
      
      valarray<double> d1(0.0, N1);
      valarray<double> d11(0.0, N1*(N1-1)/2);
      
      {
	int k=0;
	for(int i=0;i<N1;i++)
	  for(int j=0;j<i;j++) {
	    d1[i] += D1(i,j);
	    d1[j] += D1(i,j);
	    d11[k++] = D1(i,j);
	  }
	d1 /= (N1-1);
      }

      valarray<double> d2(0.0, N2);
      valarray<double> d22(0.0, N2*(N2-1)/2);
      
      {
	int k=0;
	for(int i=0;i<N2;i++)
	  for(int j=0;j<i;j++) {
	    d2[i] += D2(i,j);
	    d2[j] += D2(i,j);
	    d22[k++] = D2(i,j);
	  }
	d2 /= (N2-1);
      }

      cout<<endl;
      diameter(D1,"1",args);cout<<endl;
      cout<<endl;
      diameter(D2,"2",args);cout<<endl;
      cout<<endl;

      valarray<double> d12(0.0, N1*N2);
      valarray<double> d12_1(0.0, N1);
      valarray<double> d12_2(0.0, N2);
      for(int i=0;i<N1;i++)
	for(int j=0;j<N2;j++) {
	  double DIJ = D(i,N1+j);
	  d12[i*N2+j] = DIJ;
	  d12_1[i] += DIJ;
	  d12_2[j] += DIJ;
	}

      d12_1 /= N2;
      d12_2 /= N1;
      
      report_distances(d12,"D12  ",args);cout<<endl;
      report_distances(d12_1 ,"D1(2)",args);cout<<endl;
      report_distances(d12_2 ,"D2(1)",args);cout<<endl;
      cout<<endl;

      //NOTE: D12 != D11 when 1==2 because D12 includes the zero's on the diagonal.

      cout<<"    P(D12 > D11) = "<<fair_probability_x_less_than_y(d11,d12)<<endl;
      cout<<"    P(D12 > D22) = "<<fair_probability_x_less_than_y(d22,d12)<<endl;
      cout<<endl;
      cout<<"    P(D2(1) > D1(1)) = "<<fair_probability_x_less_than_y(d1,d12_2)<<endl;
      cout<<"    P(D1(2) > D2(2)) = "<<fair_probability_x_less_than_y(d2,d12_1)<<endl;
      cout<<endl;
      cout<<"    P(D1(2) > D1(1)) = "<<fair_probability_x_less_than_y(d1,d12_1)<<endl;
      cout<<"    P(D2(1) > D2(2)) = "<<fair_probability_x_less_than_y(d2,d12_2)<<endl;
    }

    else if (analysis == "convergence") 
    {
      check_supplied_filenames(2,files);

      tree_sample trees1(files[0],skip,subsample,max);
      tree_sample trees2(files[1],skip,subsample,max);

      for(int i=0;i<trees1.size();i++)
	cout<<distance(trees1[i],trees2,metric_fn)<<"\n";
    }
    else if (analysis == "converged") 
    {
      double alpha = args["converged"].as<double>();
      if (alpha <= 0 or alpha >= 1)
        throw myexception()<<"Converged quartile "<<alpha<<" is not between 0 and 1";

      if (alpha > 0.5) alpha = 1.0-alpha;

      check_supplied_filenames(2,files);
      
      tree_sample trees1(files[0],skip,subsample,max);
      tree_sample trees2(files[1],skip,subsample,max);
      
      ublas::matrix<double> D2 = distances(trees2,metric_fn);
      valarray<double> distances(0.0, trees2.size());
      for(int i=0;i<D2.size1();i++)
        for(int j=0;j<i;j++) {
          distances[i] += D2(i,j);
          distances[j] += D2(i,j);
	}
      distances /= (trees2.size()-1);

      double x1 = quantile(distances,alpha);
      double x2 = quantile(distances,0.5);
      double x3 = quantile(distances,1.0-alpha);

      cout<<"Equilibrium: median = "<<x2<<"     target distances["<<alpha<<"] = ("<<x1<<", "<<x3<<")\n";

      double closest = distance(trees1[0],trees2,metric_fn);
      int direction = 0;
      int required_hits = 4;
      int t=1;
      for(;t<trees1.size() and required_hits;t++) 
      {
        double d = distance(trees1[t],trees2,metric_fn);
        closest = min(closest,d);

        if (direction == 0 and d < x1) {
          if (required_hits ==4)
            cout<<"First hit at iteration "<<t<<endl;
          required_hits--;
          direction = !direction;
        }
        if (direction == 1 and d > x3) {
          required_hits--;
          direction = !direction;
        }
      }

      if (not required_hits)
        cout<<"Stabilized ["<<alpha<<"] at iteration "<<t<<endl;
      else
        cout<<"Did not converge! (Hit "<<(required_hits-4)<<" boundaries.)";

      cout<<"The closest we got to the equilibrium was "<<closest<<endl;
    }
    else
      throw myexception()<<"Analysis '"<<analysis<<"' not recognized.";
  }
  catch (std::exception& e) {
    std::cerr<<"trees-distances: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
