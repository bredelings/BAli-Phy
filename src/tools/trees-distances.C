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
#include "matrix.H"
#include "statistics.H"

#include "tree/sequencetree.H"
#include "util.H"
#include "tree/tree-util.H"
#include "tree-dist.H"

#include <boost/program_options.hpp>
#include "distance-report.H"

namespace po = boost::program_options;
using po::variables_map;

using namespace std;
using namespace statistics;

template <typename T>
void print_matrix(const matrix<T>& M, char space=' ', char eol='\n')
{
  for(int i=0;i<M.size1();i++) {
    vector<double> v(M.size2());
    for(int j=0;j<v.size();j++)
      v[j] = M(i,j);
    cout<<join(v,space)<<eol;
  }
}

matrix<double> remove_duplicates(const matrix<double>& D)
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
  matrix<double> D2(indices.size(),indices.size());

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
    ("help,h", "produce help message")
    ("skip,s",value<unsigned>()->default_value(0),"number of tree samples to skip")
    ("max,m",value<int>(),"maximum number of tree samples to read")
    ("sub-sample,x",value<int>()->default_value(1),"factor by which to sub-sample")
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
    ("leaves-only", "Show minumum and maximum distances")
    ("topology-only", "Show minumum and maximum distances")
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

matrix<double> leaf_distances(const Tree& T)
{
  const int n = T.n_leaves();
  matrix<double> D(n, n);

  // calculate the pairwise distances
  for(int i=0;i<n;i++)
  {
    D(i,i) = 0;
    for(int j=0;j<i;j++)
      D(i,j) = D(j,i) = T.distance(i,j);
  }

  return D;
}

matrix<double> distances(const vector<tree_record>& trees, 
				tree_metric_fn metric_fn
				)
{
    matrix<double> D(trees.size(),trees.size());

    // calculate the pairwise distances
    for(int i=0;i<trees.size();i++) {
      for(int j=0;j<i;j++)
      {
	D(i,j) = D(j,i) = metric_fn(trees[i],trees[j]);
	assert(D(i,j) == metric_fn(trees[j],trees[i]));
      }
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

// G= is the (undirected) graph of edges in G with slack(x,y)=0.
// 

struct hungarian_data
{
  int N;

  matrix<int> cost;

  vector<int> lx;
  vector<int> ly;

  // M is the matching on (some) of the vertices.
  vector<int> x_to_y;
  vector<int> y_to_x;

  // U is the set of vertices reachable from unmatched vertices in X.  It is divided into UX and UY.
  vector<int> UX;
  vector<int> in_UX;

  vector<int> UY;
  vector<int> in_UY;

  // Since an alternating tree means that x->y->x2 requires (x2,y) to be an edge, we just need to
  // store the x->x2 edge.  We can reconstruct the y from x2.
  vector<int> prev_x_of_x;

  void init();

  void clear();

  bool is_in_UX(int x) const {return in_UX[x];}
  int add_to_UX(int);

  void check_matching() const;

  bool is_in_UY(int y) const {return in_UY[y];}
  void add_to_UY(int);

  std::pair<int,int> consider_new_edges();

  void update_labels();

  vector<int> perfect_matching();

  int slack(int x, int y) const
  {
    return cost(x,y) - lx[x] - ly[y];
  }

  hungarian_data(const matrix<int>& M)
    :N(M.size1()),
     cost(M),
     lx(N),
     ly(N),
     x_to_y(N),
     y_to_x(N),
     in_UX(N),
     in_UY(N),
     prev_x_of_x(N)
  { 
    assert(cost.size1() == cost.size2());
  }
};

void hungarian_data::init()
{
  for(int i=0;i<N;i++)
  {
    x_to_y[i] = -1;
    y_to_x[i] = -1;
  }
}

void hungarian_data::clear()
{
  UX.clear();
  UY.clear();

  for(int x=0;x<N;x++)
  {
    int y = x;

    prev_x_of_x[x] = -1;

    in_UY[y] = 0;
    in_UX[x] = 0;
  }

  for(int x=0;x<N;x++)
    if (x_to_y[x] == -1)
      add_to_UX(x);
}

void hungarian_data::update_labels()
{
  // We update the labelling by computing
  //   delta = min(x \in UX,y \not \in UY) slack(x,y)
  // Here slack(x,y) = l[x] + l[y] - cost(x,y) >=0 and slack(x,y) == 0 implies
  //   that (x,y) is in the equality subgraph.

  // compute the first_y not in UY.
  int first_y = 0;
  while(is_in_UY(first_y))
    first_y++;

  int delta = slack(UX[0],first_y);

  for(int s=0;s<UX.size();s++)
  {
    int x = UX[s];
    for(int y=first_y;y<N;y++)
    {
      if (is_in_UY(y)) continue;

      delta = std::min(delta, slack(x,y));
    }
  }
  
  check_matching();

  // if x is in UX, then += delta.
  for(int i=0;i<UX.size();i++)
    lx[UX[i]] += delta;

  // if y is in UY, then -= delta.
  for(int i=0;i<UY.size();i++)
    ly[UY[i]] -= delta;

  check_matching();
}

void hungarian_data::check_matching() const
{
  // all of the existing 
  for(int x=0;x<N;x++)
    if (x_to_y[x] != -1)
      assert(slack(x,x_to_y[x]) == 0);

  for(int x1=0;x1<N;x1++)
    for(int x2=0;x2<x1;x2++)
      if (x_to_y[x1] != -1)
	assert(x_to_y[x1] != x_to_y[x2]);

  for(int y1=0;y1<N;y1++)
    for(int y2=0;y2<y1;y2++)
      if (y_to_x[y1] != -1)
	assert(y_to_x[y1] != y_to_x[y2]);
}

int hungarian_data::add_to_UX(int x)
{
  assert(0 <= x and x < N);
  assert(not is_in_UX(x));
  in_UX[x] = true;
  UX.push_back(x);

  return UX.size();
}

void hungarian_data::add_to_UY(int y)
{
  assert(0 <= y and y < N);
  in_UY[y] = true;
  UY.push_back(y);
}

std::pair<int,int> hungarian_data::consider_new_edges()
{
  for(int s=0;s<UX.size();s++)
  {
    int x = UX[s];
    
    for(int y=0;y<N;y++)
    {
      // We've already added this one
      if (is_in_UY(y)) continue;
      
      // For each edge from x in the equality graph...
      if (slack(x,y) == 0)
      {
	// We found an unused one!
	if (y_to_x[y] == -1)
	  return std::pair<int,int>(x,y);
	else
	{
	  // add y to the set UY
	  add_to_UY(y);
	  int x2 = y_to_x[y];

	  prev_x_of_x[x2] = x;
	  add_to_UX(x2);
	}
      }
    }
  }
  return std::pair<int,int>(-1,-1);
}

vector<int> hungarian_data::perfect_matching()
{
  assert(cost.size1() == cost.size2());
  assert(N == cost.size1());

  init();

  // The edges with this orientation are now part of the matching M
  while(true)
  {
    std::pair<int,int> found(-2,-2);
    do
    {
      // 1. Initialize variables
      clear();
      
      if (UX.size() == 0) return x_to_y;

      // We search for a path from a vertex x \in UX to a vertex y in \UY.
      // (1) We can make this a tree, by refusing to revisit any previously-visited node.
      // (2) We can keep track of this tree by remembering the parent node for every visited node.
      // (3) We can, in fact track only the sequences of x's, and the last y.
      //     Thus, x1, x2, x3 y4
      //      This is because we actually have x1->(y2,x2)->(y3,x3)->y4
      //      So, from x3,y4 we can recovert (x2 as prevx[x3], and y3 as x_to_y[x2])

      // Consider each (UX,y) vertex to add to the tree
      found = consider_new_edges();

      if (found.first < 0) update_labels();
      
    } while(found.first < 0);

    vector<int> x_to_y_old = x_to_y;

    // we should have found a new edge, here.
    int x = found.first;
    int y = found.second;
    while(x >= 0)
    {
      int next_y = x_to_y[x];
      int next_x = prev_x_of_x[x];

      x_to_y[x] = y;
      y_to_x[y] = x;

      x = next_x;
      y = next_y;
    }

    check_matching();
  }
}

int min_perfect_matching_cost(const matrix<int>& C)
{
  vector<int> x_to_y = hungarian_data(C).perfect_matching();

  // make sure all vertices are matched
  for(int x=0;x<x_to_y.size();x++)
    assert(x_to_y[x] != -1);

  // compute the cost
  int cost = 0;
  for(int x=0;x<x_to_y.size();x++)
    cost += C(x, x_to_y[x]);

  return cost;
}

using boost::dynamic_bitset;

int split_distance_to_dummy(const dynamic_bitset<>& S1)
{
  int count1 = S1.count();
  int count2 = S1.size() - count1;

  return std::min(count1, count2);
}

int split_distance(const dynamic_bitset<>& S1, const dynamic_bitset<>& S2)
{
  return std::min((S1^S2).count(), (S1&S2).count());
}

double matching_distance(const tree_record& t1, const tree_record& t2)
{
  assert(t1.n_leaves() == t2.n_leaves());

  const int N = std::max(t1.n_internal_branches(), t2.n_internal_branches());

  matrix<int> C(N,N);

  for(int i=0;i<t1.n_internal_branches();i++)
    for(int j=0;j<t2.n_internal_branches();j++)
      C(i,j) = split_distance(t1.partitions[i], t2.partitions[j]);

  for(int i=t1.n_internal_branches();i<N;i++)
    for(int j=0;j<t2.n_internal_branches();j++)
      C(i,j) = split_distance_to_dummy(t1.partitions[i]);

  for(int j=t2.n_internal_branches();j<N;j++)
    for(int i=0;i<t1.n_internal_branches();i++)
      C(i,j) = split_distance_to_dummy(t2.partitions[j]);

  return min_perfect_matching_cost(C);
}

double robinson_foulds_distance2(const tree_record& t1, const tree_record& t2)
{
  return topology_distance2(t1,t2);
}

double branch_distance2(const tree_record& t1, const tree_record& t2)
{
  return topology_distance2(t1,t2);
}

double internal_branch_distance2(const tree_record& t1, const tree_record& t2)
{
  return topology_distance2(t1,t2);
}

void write_distance_cvars_header(const SequenceTree& T,bool leaves) {

  vector<string> names;

  if (leaves) {
    for(int n1=0;n1<T.n_leaves();n1++)
      names.push_back(string("L")+convertToString(n1));
  }
  else {
    for(int n1=0;n1<T.n_nodes()-1;n1++)
      for(int n2=0;n2<n1;n2++) {
	string field = string("D")+convertToString(n1)+"-"+convertToString(n2);
	names.push_back(field);
      }
  }

  
  std::cout<<join(names,',')<<endl;
}

void write_distance_cvars_out(const SequenceTree& T,bool leaves) {

  vector<string> lengths;
  if (leaves) {
    for(int n1=0;n1<T.n_leaves();n1++)
      lengths.push_back(convertToString(T.branch(n1).length()));
  }
  else {
    for(int n1=0;n1<T.n_nodes()-1;n1++)
      for(int n2=0;n2<n1;n2++)
	lengths.push_back(convertToString(T.distance(n1,n2)));
  }
  std::cout<<join(lengths,',')<<endl;
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
    else if (metric == "matching")
      metric_fn = &matching_distance;
    else
      throw myexception()<<"Metric '"<<metric<<"' not implemented.";
      
    //----------- read in trees ------------//
    vector<string> files;
    if (args.count("files"))
      files = args["files"].as<vector<string> >();

    //----------- task "matrix" ------------//
    if (analysis == "matrix") 
    {
      check_supplied_filenames(1,files,false);

      tree_sample all_trees;
      for(int i=0;i<files.size();i++) 
      {
	int count = 0;
	if (files[i] == "-")
	  count = all_trees.load_file(std::cin,skip,subsample,max);
	else
	  count = all_trees.load_file(files[i],skip,subsample,max);
	if (log_verbose)
	  std::cerr<<"Read "<<count<<" trees from '"<<files[i]<<"'"<<std::endl;
      }

      matrix<double> D = distances(all_trees,metric_fn);

      if (args.count("remove-duplicates"))
	D = remove_duplicates(D);

      print_matrix(D,'\t','\n');
    }

    else if (analysis == "autocorrelation") 
    {
      check_supplied_filenames(1,files);
      tree_sample trees(files[0],skip,subsample,max);

      matrix<double> D = distances(trees,metric_fn);
      
      // set the window size
      int max_lag = int( double(trees.size()/10.0 + 1.0 ) );
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

      matrix<double> D = distances(trees,metric_fn);
      
      diameter(D,"1",args);
    }

    else if (analysis == "node-dist-cvars") 
    {
      bool leaves_only = args.count("leaves-only");
      bool topology_only = args.count("topology-only");

      check_supplied_filenames(1,files);
      tree_sample trees(files[0],skip,subsample,max);

      write_distance_cvars_header(trees.T(0),leaves_only);

      for(int i=0;i<trees.size();i++)
      {
	SequenceTree T = trees.T(i);
	if (topology_only)
	  for(int b=0;b<T.n_branches();b++)
	    T.branch(b).set_length(1.0);
	write_distance_cvars_out(T,leaves_only);
      }
    }
    else if (analysis == "leaf-dist-matrix") 
    {
      bool topology_only = args.count("topology-only");

      check_supplied_filenames(1,files);
      tree_sample trees(files[0],skip,subsample,max);

      for(int i=0;i<trees.size();i++)
      {
	SequenceTree T = trees.T(i);
	if (topology_only)
	  for(int b=0;b<T.n_branches();b++)
	    T.branch(b).set_length(1.0);

	matrix<double> D = leaf_distances(T);
	
	cout<<join(trees.names(), '\t')<<"\n";
	vector<double> v(D.size2());
	for(int i=0;i<D.size1();i++) 
	{
	  for(int j=0;j<v.size();j++)
	    v[j] = D(i,j);
	  cout<<join(v,'\t')<<endl;
	}
      }
    }
    else if (analysis == "compare") 
    {
      check_supplied_filenames(2,files);

      tree_sample trees1(files[0],skip,subsample,max);
      tree_sample trees2(files[1],skip,subsample,max);

      tree_sample both = trees1;
      both.append_trees(trees2);

      matrix<double> D  = distances(both,metric_fn);
      report_compare(args, D, trees1.size(), trees2.size());
    }

    else if (analysis == "convergence") 
    {
      check_supplied_filenames(2,files);

      tree_sample trees1(files[0],skip,subsample,max);
      tree_sample trees2(files[1],0,0,-1);

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
      tree_sample trees2(files[1],0,0,-1);
      
      matrix<double> D2 = distances(trees2,metric_fn);
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
