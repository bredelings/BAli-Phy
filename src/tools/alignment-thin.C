/*
   Copyright (C) 2005-2009 Benjamin Redelings

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
#include <string>
#include "tree/tree.H"
#include "alignment/alignment.H"
#include "alignment/alignment-util.H"
#include "tree/tree-util.H"
#include "util.H"
#include "setup.H"
#include "distance-methods.H"
#include <utility>
#include "alignment/index-matrix.H"

using std::pair;
using std::vector;
using std::string;
using std::endl;

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;

using std::string;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("align", value<string>(),"file with sequences and initial alignment")
    ("find-dups", value<string>(),"for each other sequence, find the closest sequence")
    ("cutoff",value<unsigned>(),"only leave taxa with more mismatches than this value")
    ("longer-than",value<unsigned>(),"only leave taxa w/ sequences longer than this")
    ("shorter-than",value<unsigned>(),"only leave taxa w/ sequences shorter than this")
    ("down-to",value<int>(),"number of taxa to keep")
    ("keep",value<string>(),"comma-separated list of taxon names to keep - remove others")
    ("min-letters",value<int>(),"Remove columns with fewer letters.")
    ("verbose,v","Output more log messages on stderr.")
    ("show-lengths","just print out sequence lengths")
    ("sort","Sort partially ordered columns to minimize the number of visible indels.")
    ("remove-unique",value<int>(),"Remove insertions in a single sequence if longer than this many letters")
    ("remove-crazy",value<int>(),"Remove sequence that have deleted conserved sites")
    ("remove",value<string>(),"comma-separated list of taxon names to remove")
    ("conserved-fraction",value<double>()->default_value(0.75),"Fraction of sequences that must contain a letter for it to be considered conserved.")
    ;

  // positional options
  positional_options_description p;
  p.add("align", 1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-thin <alignment-file>\n";
    cout<<"Remove the most closely related sequences from an alignment.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  if (args.count("verbose")) log_verbose = 1;

  return args;
}

std::pair<int,int> argmin(matrix<int>& M)
{
  double mvalue = M(0,0);
  int m1 = 0;
  int m2 = 0;

  for(int i=0;i<M.size1();i++)
    for(int j=0;j<M.size2();j++)
      if (M(i,j) < mvalue) {
	mvalue = M(i,j);
	m1 = i;
	m2 = j;
      }
  return std::pair<int,int>(m1,m2);
}


/// Find a pair m1,m2 such that D(m1,m2) is smallest among the sequences not yet removed (keep[]==0) and such that 
/// m1 is not protected (keep[m1]==
std::pair<int,int> argmin(matrix<int>& M, const vector<int>& keep)
{
  double mvalue = -1;
  int m1 = -1;
  int m2 = -1;

  for(int i=0;i<M.size1();i++)
    if (keep[i] == 1) // present, but not protected
      for(int j=0;j<M.size2();j++)
	if (i != j and keep[j])
	  if (M(i,j) < mvalue or m1 == -1) {
	    mvalue = M(i,j);
	    m1 = i;
	    m2 = j;
	  }
  return std::pair<int,int>(m1,m2);
}

int argmin_row(matrix<int>& M, int i, const vector<int>& keep)
{
  double mvalue = -1;
  int m2 = -1;

  for(int j=0;j<M.size2();j++)
    if (keep[j])
      if (M(i,j) < mvalue or m2 == -1) {
	mvalue = M(i,j);
	m2 = j;
      }

  return m2;
}

unsigned asymmetric_distance(const alignment& A,int i,int j)
{
  const int L = A.length();
  unsigned D=0;

  if (i==j) return 0;

  for(int c=0;c<L;c++)
    if (A(c,i) >= 0 and A(c,i) != A(c,j))
      D++;

  return D;
}

unsigned symmetric_distance(const alignment& A,int i,int j)
{
  const int L = A.length();
  unsigned D=0;

  if (i==j) return 0;

  for(int c=0;c<L;c++)
    if (A(c,i) >= 0 and A(c,j)>=0 and A(c,i) != A(c,j))
      D++;

  return D;
}

double symmetric_overlap(const alignment& A,int i,int j)
{
  const int L = A.length();
  unsigned D=0;

  for(int c=0;c<L;c++)
    if (A(c,i) >= 0 and A(c,j)>=0)
      D++;

  return D;
}

int n_positive(const vector<int>& v) 
{
  int count=0;
  for(int i=0;i<v.size();i++)
    if (v[i]>0)
      count++;
  return count;
}

matrix<int> asymmetric_distance_matrix(const alignment& A)
{
  const int N = A.n_sequences();

  matrix<int> D(N,N);

  for(int i=0;i<N;i++)
    for(int j=0;j<N;j++)
      D(i,j) = asymmetric_distance(A,i,j);

  return D;
}

matrix<int> symmetric_distance_matrix(const alignment& A)
{
  const int N = A.n_sequences();

  matrix<int> D(N,N);

  for(int i=0;i<N;i++)
    for(int j=0;j<=i;j++)
      D(i,j) = D(j,i) = symmetric_distance(A,i,j);

  return D;
}

matrix<int> symmetric_overlap_matrix(const alignment& A)
{
  const int N = A.n_sequences();

  matrix<int> D(N,N);

  for(int i=0;i<N;i++)
    for(int j=0;j<=i;j++)
      D(i,j) = D(j,i) = symmetric_overlap(A,i,j);

  return D;
}

int remove_almost_empty_columns(alignment& A,int n)
{
  int length = 0;

  for(int column=0;column<A.length();column++)
    if (n_characters(A,column) >= n) 
    {
      if (column != length)
	for(int i=0;i<A.n_sequences();i++)
	  A.set_value(length,i, A(column,i) );
      length++;
    }

  int n_empty = A.length() - length;

  A.changelength(length);

  return n_empty;
}

int unique_letter(const alignment& A,int c)
{
  if (n_characters(A,c) != 1) return -1;

  for(int i=0;i<A.n_sequences();i++)
    if (A.character(c,i))
      return i;

  return -1;
}

vector<int> get_taxon_indices(const vector<string>& names,const vector<string>& lookup)
{
  vector<int> indices(lookup.size());
  for(int i=0;i<indices.size();i++) 
  {
    int l = find_index(names,lookup[i]);
    if (l == -1)
      throw myexception()<<"keep: can't find sequence '"<<lookup[i]<<"' to keep.";
    indices[i] = l;
  }
  return indices;
}

vector<int> get_taxon_indices(const vector<string>& names,const string& lookup)
{
  vector<string> lookup_v = split(lookup,',');

  return get_taxon_indices(names,lookup_v);
}


int main(int argc,char* argv[])
{
  try {
    cerr.precision(10);
    cout.precision(10);
    
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //----------- Load alignment and tree ---------//
    alignment A = load_A(args,false);
    const int N = A.n_sequences();
    const int L = A.length();

    if (log_verbose) 
      cerr<<"Read "<<N<<" sequences at length "<<L<<endl;

    vector<string> names = sequence_names(A);

    vector<int> AL(N);
    for(int i=0;i<N;i++)
      AL[i] = A.seqlength(i);

    if (args.count("show-lengths")) {
      for(int i=0;i<A.n_sequences();i++) {
	cout<<AL[i]<<endl;
      }
      exit(0);
    }

    //--------------------- keep -------------------------//

    // By default every sequence has status 1 which means, removeable, but not removed.
    vector<int> keep(A.n_sequences(),1);

    vector<string> protect;
    if (args.count("keep"))
      protect = split(args["keep"].as<string>(),',');

    for(int i=0;i<protect.size();i++) {
      int p = find_index(names,protect[i]);
      if (p == -1)
	throw myexception()<<"keep: can't find sequence '"<<protect[i]<<"' to keep.";
      keep[p] = 2;
    }

    //----------------- remove by length ------------------//

    if (args.count("longer-than"))
    {
      unsigned cutoff = args["longer-than"].as<unsigned>();
      
      for(int i=0;i<A.n_sequences();i++)
	if (AL[i] <= cutoff and keep[i] < 2)
	  keep[i] = 0;
    }

    if (args.count("shorter-than"))
    {
      unsigned cutoff = args["shorter-than"].as<unsigned>();
      
      for(int i=0;i<A.n_sequences();i++)
	if (AL[i] >= cutoff and keep[i] < 2)
	  keep[i] = 0;
    }

    //-------------------- remove ------------------------//

    vector<string> remove;
    if (args.count("remove"))
      remove = split(args["remove"].as<string>(),',');

    for(int i=0;i<remove.size();i++) {
      int r = find_index(names,remove[i]);
      if (r == -1)
	throw myexception()<<"remove: can't find sequence '"<<remove[i]<<"' to remove.";
      if (keep[r] == 2)
      //	throw myexception()<<"Can't both keep AND remove '"<<remove[i]<<"'.";
	;
      else
	keep[r] = 0;

    }
      

    if (log_verbose and keep.size() != n_positive(keep)) cerr<<"Removed "<<keep.size()-sum(keep)<<" sequences because of length constraints."<<endl;

    //-------------------- remove ------------------------//

    if (args.count("remove-crazy"))
    {
      int n_remove = args["remove-crazy"].as<int>();
      double conserved_fraction = args["conserved-fraction"].as<double>();

      vector<int> conserved(A.n_sequences());
      int n_conserved_columns=0;
      for(int i=0;i<A.length();i++)
      {
	double fraction = double(n_characters(A,i))/A.n_sequences();
	if (fraction < conserved_fraction) continue;

	n_conserved_columns++;
	
	for(int j=0;j<A.n_sequences();j++)
	  if (A.character(i,j))
	    conserved[j]++;
      }

      vector<int> order = iota<int>(conserved.size());
      sort(order.begin(), order.end(), sequence_order<int>(conserved));

      if (log_verbose) {
	cerr<<"total # conserved columns = "<<n_conserved_columns<<endl;
	cerr<<"  conserved: ";
	cerr<<"  min = "<<conserved[order[0]];
	cerr<<"  median = "<<conserved[order[order.size()/2]];
	cerr<<"  max = "<<conserved[order.back()]<<endl;
      }

      for(int i=0;i<n_remove;i++) 
	if (keep[order[i]] == 1 and keep[order[i]] != 2) {
	  cerr<<"Remove crazy: "<<names[order[i]]<<"    "<<conserved[order[i]]<<endl;
	  keep[order[i]] = 0;
	}
	else if (n_remove < order.size())
	  n_remove++;
    }

    //------- Find the most redundant --------//
    matrix<int> D;

    // report distances to specified taxa
    if (args.count("find-dups"))
    {
      D = asymmetric_distance_matrix(A);
      matrix<int> DS = symmetric_distance_matrix(A);
      matrix<int> DO = symmetric_overlap_matrix(A);

      matrix<double> DEV(D.size1(),D.size2());
      for(int i=0;i<DEV.size1();i++) 
	for(int j=0;j<DEV.size2();j++)
	  if (not DO(i,j))
	    DEV(i,j) = 1.0;
	  else {
	    DEV(i,j) = double(DS(i,j)+5.0)/double(DO(i,j)+5.0);
	  }
	
      
      // get the indices for the taxa to compare to
      vector<int> compare_to = get_taxon_indices(names, args["find-dups"].as<string>());

      // convert the indices to a mask
      vector<int> target(names.size(),0);
      for(int i=0;i<compare_to.size();i++)
	target[compare_to[i]] = 1;

      // compute get the indices of the other taxa
      vector<int> not_compare_to;
      for(int i=0;i<target.size();i++)
	if (not target[i])
	  not_compare_to.push_back(i);

      // find the closest neighbors
      vector<int> closest(not_compare_to.size());
      vector<double> distance(not_compare_to.size());
      for(int i=0;i<not_compare_to.size();i++) {
	closest[i] = argmin_row(DS,not_compare_to[i],target);
	distance[i] = DEV(not_compare_to[i],closest[i]);
      }
	
      vector<int> order = iota<int>(not_compare_to.size());
      sort(order.begin(), order.end(), sequence_order<double>(distance));

      for(int i=0;i<not_compare_to.size();i++) 
      {
	  int p1 = not_compare_to[order[i]];

	  int p2 = closest[order[i]];

	  int percent = DEV(p1,p2)*100;
	  cerr<<"  #"<<i<<": "<<names[p1]<<" -> "<<names[p2];
	  cerr<<"     D=[ "<<D(p1,p2)<<" / "<<D(p2,p1)<<" ]";
	  cerr<<"     D = "<<percent<<"% [ "<<DS(p1,p2)<<"/"<<DO(p1,p2)<<" ] "<<endl;
      }
    }

    if (args.count("cutoff") or args.count("down-to"))
    {
      int cutoff = -1;
      if (args.count("cutoff"))
	cutoff = args["cutoff"].as<unsigned>();

      int down_to = A.n_sequences();
      if (args.count("down-to"))
	down_to = args["down-to"].as<int>();

      D = asymmetric_distance_matrix(A);
      matrix<int> DS = symmetric_distance_matrix(A);
      
      vector<int> removed;

      while(true)
      {
	// find the smallest pair (p1,p2) where neither has been removed.
	std::pair<int,int> p = argmin(D,keep);
	int p1 = p.first;
	int p2 = p.second;
	int MD = D(p1,p2);
	
	// exit if this distance is larger than the cutoff.
	if (MD >= cutoff and (A.n_sequences() - removed.size() <= down_to)) break;
	
	// remove the sequence with the shorter comment, if they are the same
	if (D(p1,p2) == D(p2,p1) and keep[p2]< 2 and A.seq(p1).comment.size() > A.seq(p2).comment.size())
	  std::swap(p1,p2);
	
	// mark as removed
	keep[p1] = 0;
	removed.push_back(p1);
      }

      // compute distances to those that remain
      vector<int> closest(removed.size());
      vector<int> distance(removed.size());

      // Count how many we *actually* remove, after we decide to put some back!
      int n_removed=0;

      for(int i=closest.size()-1; i>=0; i--)
      {
	closest[i]  = argmin_row(D,removed[i],keep);
	distance[i] = D(removed[i],closest[i]);

	// put item back if too far from remaining items
	if (cutoff != -1 and distance[i] >= cutoff and (A.n_sequences() - i - 1 - n_removed) < down_to)
	  keep[removed[i]] = 1;
	else
	  n_removed++;
      }

      if (log_verbose)
      {
	vector<int> order = iota<int>(removed.size());
	sort(order.begin(), order.end(), sequence_order<int>(distance));

	cerr<<"\nRemoved "<<n_removed<<" similar sequences:"<<endl;
	n_removed=0;

	for(int i=0;i<removed.size();i++) 
	{
	  int p1 = removed[order[i]];

	  int p2 = closest[order[i]];

	  if (keep[p1] > 0) continue;

	  cerr<<"  #"<<++n_removed<<": "<<names[p1]<<" -> "<<names[p2]<<"  D=[ "<<D(p1,p2)<<" / "<<D(p2,p1)<<" / "<<DS(p2,p1)<<" ]"<<endl;
	}
      }
    }

    //------- Select the sequences -------//
    alignment A2 = select_rows(A,keep);

    //------- Remove consecutive unique sites -----//

    // Question: should I remove ALL of the sites, or just the sites beyond L aa/nucs?
    //    And, how would I remove the MIDDLE sites?
    // Question: should I remove only sites that have no ?  How should I do so?
    if (args.count("remove-unique"))
    {
      int L = args["remove-unique"].as<int>();

      vector<int> keep_sites(A2.length(),1);

      vector<int> unique(A2.length(),-1);
      for(int i=0;i<A2.length();i++)
	unique[i] = unique_letter(A,i);

      vector<vector<int> > columns = column_lookup(A);

      for(int i=0;i<A.n_sequences();i++)
      {
	int count =0;

	for(int j=0;j<columns[i].size()+1;j++)
	{
	  int u = -1;
	  if (j< columns[i].size())
	    u = unique[columns[i][j]];

	  if (u == i)
	    count++;
	  else {
	    // close out the current insertion if there is one
	    if (count > L) 
	    {
	      // remove count-L sites from the middle of the insertion
	      int start = j - count;
	      int end   = j - 1;

	      if (start == 0)
		end -= L;
	      else if (end == columns[i].size()-1)
		start += L;
	      else {
		start += L/2;
		end   -= (L-L/2);
	      }

	      for(int k=start;k<=end;k++)
		keep_sites[columns[i][k]]=0;
	    }
	    count = 0;
	  }
	}
      }

      // actually remove the unwanted sites
      vector<int> sites;
      for(int i=0;i<keep_sites.size();i++)
	if (keep_sites[i])
	  sites.push_back(i);
      A2 = select_columns(A2,sites);
    }


    //------- Remove columns with too few letters ------//
    if (args.count("min-letters")) {
      int m = args["min-letters"].as<int>();
      remove_almost_empty_columns(A2,m);
    }

    if (args.count("sort"))
      A2 = get_ordered_alignment(A2);

    //------- Print out the alignment -------//
    std::cout<<A2;

    if (log_verbose) cerr<<"Went from "<<A.n_sequences()<<" -> "<<A2.n_sequences()<<" sequences."<<endl;
    if (log_verbose) cerr<<"Went from "<<A.length()<<" -> "<<A2.length()<<" columns."<<endl;

  }
  catch (std::exception& e) {
    cerr<<"alignment-thin: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
