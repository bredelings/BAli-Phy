/*
   Copyright (C) 2004-2009 Benjamin Redelings

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

#include <fstream>
#include <string>
#include <cmath>
#include <vector>
#include <list>
#include "myexception.H"
#include "alignment/alignment.H"
#include "math/logsum.H"
#include "optimize.H"
#include "findroot.H"
#include "util.H"
#include "setup.H"
#include "alignment/alignment-util.H"
#include "distance-methods.H"

#include <boost/program_options.hpp>
#include <boost/dynamic_bitset.hpp>

namespace po = boost::program_options;
using std::vector;
using std::string;
using std::endl;
using po::variables_map;
using boost::dynamic_bitset;

// FIXME - also show which COLUMNS are more that 99% conserved?

// With characters, you take i~j~k and split the column, then if i and j
// are in different columns, then cannot both be aligned to k.  But, if
// you have i~j~-, then i and j can be in different columns and both aligned
// to - in the third sequence.  Likewise with '?'.

// If we compute the certainty of +~- only based on the + nodes, then + will
// always be 100% aligned, which doesn't seem fair.  Also, if we suddenly switch
// to '?', then the remaining characters will become "better aligned".

// If we could represent sub-groups, then we wouldn't need a root.

using std::cin;
using std::cout;
using std::cerr;
using std::istream;
using std::ifstream;
using std::list;
using namespace optimize;

inline double log_fact(int n) {
  if (n<2)
    return 0;

  double total=0;
  for(int i=2;i<=n;i++)
    total += log(i);

  return total;
}

Matrix probability_to_distance(const Matrix &Q) {
  Matrix D(Q.size1(),Q.size2());
  for(int i=0;i<Q.size1();i++)
    for(int j=0;j<Q.size2();j++) {
      assert(0 <= Q(i,j) and Q(i,j) <= 1.0);
      D(i,j) = -log(Q(i,j));
    }
  return D;
}

Matrix probability_to_distance_variance(const Matrix &Q) 
{
  unsigned N = 1;

  Matrix S2(Q.size1(),Q.size2());

  for(int i=0;i<Q.size1();i++)
    for(int j=0;j<Q.size2();j++) {

      assert(0 <= Q(i,j) and Q(i,j) <= 1.0);

      S2(i,j) = (1.0-Q(i,j))/(Q(i,j)*N);
    }
  return S2;
}

Matrix probability_to_distance_weights(const Matrix &Q) 
{
  unsigned N = 1;

  Matrix W(Q.size1(),Q.size2());

  for(int i=0;i<Q.size1();i++)
    for(int j=0;j<Q.size2();j++) {

      assert(0 <= Q(i,j) and Q(i,j) <= 1.0);

      W(i,j) = Q(i,j)*N/(1.0-Q(i,j));
    }
  return W;
}

// character i of species s1 is not homologous to any character of species s2
unsigned count_homology(int s1, int i1, int s2, int i2, 
			const vector< vector< vector<int> > >& column_indices) 
{
  unsigned count = 0;
  for(int j=0;j<column_indices.size();j++)
  {
    int c1 = column_indices[j][s1][i1];
    int c2 = column_indices[j][s2][i2];
    if (c1 == c2)
      count++;
  }
  return count;
}

// character i of species s1 is not homologous to any character of species s2
unsigned count_non_homology(int s1, int i1, int s2, const vector<matrix<int> >& Ms,
			    const vector< vector< vector<int> > >& column_indices) 
{
  unsigned count = 0;
  for(int j=0;j<column_indices.size();j++)
  {
    int c = column_indices[j][s1][i1];
    if (Ms[j](c,s2) == alphabet::gap)
      count++;
  }
  return count;
}

// Compute the probability that residues (i,j) are aligned
//   - v[i][j] represents the column of the feature j in alignment i.
//   - so if v[i][j] == v[i][k] then j and k are paired in alignment i.
Matrix counts_to_probability(const Tree& T,const vector<int>& column, 
			     const vector<matrix<int> >& Ms,
			     const vector< vector< vector<int> > >& column_indices) 
{
  assert(T.n_leaves() == column.size());
  assert(Ms.size() == column_indices.size());

  const int N = column.size();

  // initialize the pseudocount matrix
  const double edge_prior = 0.5/(T.n_branches()/2);
  const double prior = 0.5;

  Matrix pseudocount = EdgesDistanceMatrix(T);
  for(int i=0;i<pseudocount.size1();i++)
    for(int j=0;j<pseudocount.size2();j++)
      pseudocount(i,j) = prior + pseudocount(i,j)*edge_prior;

  for(int i=0;i<pseudocount.size1();i++)
    for(int j=0;j<pseudocount.size2();j++)
      assert(pseudocount(i,j) > 0);

  // initialize the matrix - add a pseudocount to avoid P=0 or P=1
  Matrix Pr_align_pair = 0.1*0.5*pseudocount;

  // For each label, count all present pairs
  for(int i=0;i<N;i++) 
    for(int j=0;j<i;j++) 
      if (column[i] == alphabet::unknown or column[j] == alphabet::unknown)
	Pr_align_pair(i,j) = Pr_align_pair(j,i) = 1.0;
      else if (column[i] == alphabet::gap and column[j] == alphabet::gap)
	Pr_align_pair(i,j) = Pr_align_pair(j,i) = 1.0;
      else {
	if (column[i] == alphabet::gap)
	  Pr_align_pair(i,j) += count_non_homology(j,column[j],i,Ms,column_indices);
	else if (column[j] == alphabet::gap)
	  Pr_align_pair(i,j) += count_non_homology(i,column[i],j,Ms,column_indices);
	else
	  Pr_align_pair(i,j) += count_homology(i,column[i],j,column[j],column_indices);
	
	// Divide by count to yield an average
	Pr_align_pair(i,j) /= (Ms.size() + 0.1*pseudocount(i,j));
	Pr_align_pair(j,i) = Pr_align_pair(i,j);
      }

  // we didn't handle the diagonal entries at all...
  for(int i=0;i<N;i++)
    Pr_align_pair(i,i) = 1.0;

  // Check symmetry
  for(int i=0;i<N;i++)
    for(int j=0;j<N;j++)
      assert(std::abs(Pr_align_pair(i,j) - Pr_align_pair(j,i)) < 1.0e-9);

  for(int i=0;i<pseudocount.size1();i++)
    for(int j=0;j<pseudocount.size2();j++)
      if (i==j)
	assert(Pr_align_pair(i,j) == 1.0);
      else
	assert(0.0 < Pr_align_pair(i,j) and Pr_align_pair(i,j) <= 1.0);

  return Pr_align_pair;
}

void add_internal_labels(SequenceTree& T);

void do_setup(const variables_map& args,list<alignment>& alignments,alignment& A,RootedSequenceTree& T) 
{
  //--------------- Load and link template A and T -----------------//
  load_A_and_T(args,A,T,false);

  //------------ Try to load alignments -----------//
  int maxalignments = args["max-alignments"].as<int>();
  unsigned skip = args["skip"].as<unsigned>();

  if (log_verbose) std::cerr<<"alignment-gild: Loading alignments...";

  // Instead of reordering the tree, just specify the names, here.

  try
  {
    alignments = load_alignments(std::cin, T.get_leaf_labels(), A.get_alphabet(), skip, maxalignments);
  }
  catch (std::exception& e)
  {
    if (alignments.size() == 0)
    {
      add_internal_labels(T);
      alignments = load_alignments(std::cin, T.get_labels(), A.get_alphabet(), skip, maxalignments);
    }
  }

  if (log_verbose) std::cerr<<"done. ("<<alignments.size()<<" alignments)"<<std::endl;
  if (alignments.empty()) 
    throw myexception()<<"Alignment sample is empty.";

  /*
  //---------- Re-link the tree to the loaded alignments -----------//
  alignment A2 = chop_internal(alignments.front());
  
  link(A2,T,false);

  //---------- Check compatability of estimate & samples -----------//
  assert(A.n_sequences() == T.n_leaves());
  
  //  if (alignments.front().n_sequences() != T.n_nodes())
  //    throw myexception()<<"Number of sequences in alignment estimate is NOT equal to number of tree nodes!";
  
  vector<int> pi = compute_mapping(T.get_leaf_labels(), sequence_names(A));

  vector<string> names = sequence_names(A);
  
  for(int i=0;i<T.n_leaves();i++) {

    if (A.seq(pi[i]).name != A2.seq(i).name)
      throw myexception()<<"Alignment estimate has different sequences or sequence order than alignment samples";
    
    if (A.seqlength(pi[i]) != A2.seqlength(i))
      throw myexception()<<"Sequence '"<<T.get_label(i)<<"' has different length in alignment estimate and alignment samples!";
  }
  */
}


vector<int> get_column(const matrix<int>& MA,int c,int nleaves) {
  vector<int> column(nleaves);
  for(int i=0;i<nleaves;i++)
    column[i] = MA(c,i);
  return column;
}


double get_column_probability(const vector<int>& column, 
			      const list<alignment>& alignments,
			      const vector< vector< vector<int> > >& column_indexes) 
{
  unsigned int count=0;
  int i=0;
  foreach(A,alignments) {
    bool found=true;

    // Can we find a common column for all features?
    int c=-1;
    for(int j=0;j<column.size() and found;j++) {

      // if there is a gap in this row, ignore it
      if (column[j] == alphabet::gap) continue;

      // if there is a gap in this row, ignore it
      if (column[j] == alphabet::unknown) continue;

      // find the column that for the column[j]-th feature of species j
      int cj = column_indexes[i][j][column[j]];

      if (c == -1)
	c = cj;
      else if (c != cj)
	found = false;
    }
    
    assert(c != -1);

    if (c != -1) {
      // Does this column have gaps in the right place?
      for(int j=0;j<column.size() and found;j++) {
	
	// if there is a NOT gap in this column, ignore it
	if (column[j] != alphabet::gap) continue;
	
	// if the template doesn't have a gap, then this doesn't match
	if (A->character(c,j))
	  found = false;
      }
    }

    if (found) count++;
    i++;
  }

  return double(0.5+count)/(1.0+alignments.size());
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
    ("find-root","estimate the root position from branch lengths")
    ("alphabet",value<string>(),"set to 'Codons' to prefer codon alphabets")
    ("skip",value<unsigned>()->default_value(0),"number of alignment samples to skip")
    ("max-alignments",value<int>()->default_value(1000),"maximum number of alignments to analyze")
    ("verbose,v","Output more log messages on stderr.")
    ;

  // positional options
  positional_options_description p;
  p.add("align", 1);
  p.add("tree", 1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-gild alignment-file tree-file ... [OPTIONS] < alignments-file\n";
    cout<<"Annotate each residue in the alignment according to the probability.\n";
    cout<<" that it should align to the hypothetical root character in its column.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  if (args.count("verbose")) log_verbose = 1;

  return args;
}

int which_directed_branch_equals(const Tree& T, const dynamic_bitset<>& p)
{
  assert(p.size() == T.n_leaves());

  for(int b=0;b<2*T.n_branches();b++)
    if (p == branch_partition(T,b))
      return b;

  return -1;
}

struct root_position
{
  int b;
  double x;
  root_position() {}
  root_position(int i,double d):b(i),x(d) {}
};


root_position find_root_branch_and_position(const SequenceTree& T,const RootedSequenceTree& RT)
{
  root_position rootp;

  vector<const_branchview> branches;
  append(RT.root().branches_out(),branches);

  if (branches.size() < 2)
    throw myexception()<<"Can't use a leaf node as root, sorry.";

  rootp.b = which_directed_branch_equals(T, branch_partition(RT,branches[0]));
  rootp.x = 0;

  assert(rootp.b != -1);

  if (branches.size() == 2)
    rootp.x = branches[1].length()/(branches[0].length() + branches[1].length());
  return rootp;
}

vector<double> letter_weights_project(const vector<int>& column, const Matrix& Q, const SequenceTree& T,
				      vector<vector<int> >& leaf_sets)
{
  // get ordered list of features to keep (also a mapping)
  vector<int> f;
  for(int i=0;i<column.size();i++)
    if (column[i] != alphabet::gap and column[i] != alphabet::unknown)
      f.push_back(i);

  vector<double> w(T.n_leaves(),-1);
  
  if (f.size() == 0)
    throw myexception()<<"Column has no non-gap features!";
  else if (f.size() == 1)
    w[f[0]] = 1.0;
  else if (f.size() == 2) {
    double P = Q(f[0],f[1]);
    w[f[0]] = sqrt(P);
    w[f[1]] = sqrt(P);
  }
  else {
    vector<int> remove;
    for(int i=0;i<column.size();i++)
      if (column[i] == alphabet::gap or column[i] == alphabet::unknown)
	remove.push_back(i);

    // Map the leaf indices of ST to the leaf indices of T
    SequenceTree ST = T;
    vector<int> mapping = ST.prune_leaves(remove);
    mapping.resize(ST.n_leaves());
    assert(f == mapping);

    // Project Q down
    Matrix Q2 (f.size(),f.size());
    for(int i=0;i<f.size();i++) {
      Q2(i,i) = 1.0;
      for(int j=0;j<i;j++)
	Q2(i,j)=Q2(j,i)=Q(f[i],f[j]);
    }

    // Compute the LS branch lengths
    Matrix D = probability_to_distance(Q2);
    Matrix W = probability_to_distance_weights(Q2);

    vector<double> LS_branch_lengths = FastLeastSquares(ST,D,partition_sets(ST));
    
    for(int b=0;b<ST.n_branches();b++)
      ST.branch(b).set_length(std::max(0.0, LS_branch_lengths[b]));

    // Find root on the tree
    int r = ST.n_leaves();
    double max = 0;
    for(int i=ST.n_leaves();i<ST.n_nodes();i++)
    {
      double P=0;
      for(int j=0;j<ST.n_leaves();j++)
	P += exp(-ST.distance(i,j));
      if (P > max) {
	max = P;
	r = i;
      }
    }

    // Compute the uncertainty values for the letters
    for(int i=0;i<ST.n_leaves();i++) {
      double length = ST.distance(r,i);
      w[f[i]] =  exp(-length);  // P(no events between leaf and root)
    }
  }

  return w;
}


vector<double> letter_weights(const vector<int>& column, const Matrix& Q, const SequenceTree& T,
			      vector<vector<int> >& leaf_sets)
{
  // Store ordered list of features to keep
  vector<int> f;
  for(int i=0;i<column.size();i++)
    if (column[i] != alphabet::gap and column[i] != alphabet::unknown)
      f.push_back(i);

  // Compute weights for '+'
  vector<double> w = letter_weights_project(column,Q,T,leaf_sets);

  // Compute weights for '-' and '?'
  for(int i=0;i<column.size();i++)
    if (column[i] == alphabet::unknown)
      w[i] = 1.0;
    else if (column[i] == alphabet::gap) {
      vector<double> P(f.size());
      for(int j=0;j<f.size();j++)
	P[j] = w[f[j]] * Q(i,f[j]);
      w[i] = max(P);
    }

  return w;
}

int main(int argc,char* argv[]) { 
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);
    
    //----------- Load alignment and tree ---------//
    alignment A;
    RootedSequenceTree RT;
    list<alignment> alignments;
    vector<matrix<int> > Ms;
    do_setup(args,alignments,A,RT);
    foreach(i,alignments)
      Ms.push_back(M(*i));

    SequenceTree T = RT;
    remove_sub_branches(T);
    if (not is_Cayley(T))
      throw myexception()<<"Multifurcating trees are not handled yet.";

    vector<int> pi = compute_mapping(T.get_leaf_labels(), sequence_names(A));

    //----------- Find root branch ---------//
    if (args.count("find-root"))
      RT = find_rooted_tree(T);

    root_position rootp = find_root_branch_and_position(T,RT);

    //----------- Construct alignment indexes ----------//
    vector< vector< vector<int> > >  column_indexes;
    foreach(i,alignments)
      column_indexes.push_back( column_lookup(*i,T.n_leaves()) );

    //------- Convert template to index form-------//
    matrix<int> MA = M(A);

    //--------- Compute full entire column probabilities -------- */
    vector<double> column_probabilities(A.length());
    for(int c=0;c<A.length();c++)
      column_probabilities[c] = get_column_probability(compose(pi,get_column(MA,c,T.n_leaves())),
						       alignments,
						       column_indexes
						       );

    //------- Print column names -------//
    for(int i=0;i<T.n_leaves();i++) {
      cout<<T.get_label(i);
      if (i != T.n_leaves()-1)
	cout<<" ";
      else
	cout<<endl;
    }

    //------- Analyze the columns -------//
    Matrix P_total(T.n_leaves(),T.n_leaves());
    for(int i=0;i<P_total.size1();i++)
      for(int j=0;j<P_total.size2();j++)
	P_total(i,j) = 0;

    vector<vector<int> > leaf_sets = partition_sets(T);

    for(int c=0;c<A.length();c++) 
    {
      vector<int> column = get_column(MA,c,T.n_leaves());

      column = compose(pi,column);

      // Get the pairwise alignment probabilities
      Matrix Q = counts_to_probability(T,column, Ms, column_indexes);

      // Convert the pairwise probabilities to weights
      vector<double> w = letter_weights(column,Q,T,leaf_sets);

      // Print out the weights
      for(int i=0;i<w.size();i++)
	cout<<w[i]<<" ";

      cout<<column_probabilities[c]<<endl;
    }
  }
  catch (std::exception& e) {
    std::cerr<<"alignment-gild: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
