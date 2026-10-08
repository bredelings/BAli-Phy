/*
   Copyright (C) 2006-2008 Benjamin Redelings

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
#include <numeric>
#include <set>
#include "util/myexception.hh"
#include "alignment/alignment.hh"
#include "optimize.hh"
#include "findroot.hh"
#include "alignment/load.hh"
#include "alignment/alignment-util.hh"
#include "alignment/index-matrix.hh" // for M( )
#include "distance-methods.hh"

#include <CLI/CLI.hpp>
#include "util/mapping.hh"
#include "util/string/join.hh"
#include "util/rng.hh"
#include "util/io.hh"
#include "util/range.hh"
#include "util/assert.hh"

extern int log_verbose;


using namespace std;

void load_alignments(vector<alignment>& alignments,
		     const string& filename, 
		     const string& alph_name,
		     int maxalignments,
		     string what)
{
  if (what.size() > 0)
    what = string(" ")+what;

  if (log_verbose)
    std::cerr<<"alignment-compare: Loading alignment sample"<<what<<"...";

  checked_ifstream file(filename, "alignment sample file");

  list<alignment> As = load_alignments(file, alph_name, 0, maxalignments);

  alignments.clear();
  alignments.insert(alignments.begin(),As.begin(),As.end());

  if (log_verbose)
    std::cerr<<"done. ("<<alignments.size()<<" alignments)"<<std::endl;
  if (not alignments.size())
    throw myexception()<<"Alignment sample"<<what<<" is empty.";  

  for(auto& alignment: alignments)
    alignment = chop_internal(alignment);


}
		     


matrix<double> get_counts(int s1,int s2,int L1,int L2,
			      const vector<matrix<int> >& Ms)
{
  const double unit = 1.0/Ms.size();

  // Initialize the count matrix
  matrix<double> count(L1+1,L2+1);
  for(int i=0;i<count.size1();i++)
    for(int j=0;j<count.size2();j++)
      count(i,j) = 0;

  // get counts of each against each
  for(auto& M: Ms)
  {
    for(int c=0;c<M.size1();c++) {
      int index1 = M(c,s1);
      int index2 = M(c,s2);
      count(index1 + 1, index2 + 1) += unit;
    }
  }
  count(0,0) = 0;

  return count;
}

double compute_tv_12(int x,const matrix<double>& count1,const matrix<double>& count2)
{
  x++;

  double D=0;

  assert(count1.size1() == count2.size1());
  assert(count1.size2() == count2.size2());

  for(int y=0;y<count1.size2();y++)
    D += std::abs(count1(x,y)-count2(x,y));

  D /= 2.0;

  assert(D <= 1.0);

  return D;
}


double compute_tv_21(int y,const matrix<double>& count1,const matrix<double>& count2)
{
  y++;

  double D=0;

  assert(count1.size1() == count2.size1());
  assert(count1.size2() == count2.size2());

  for(int x=0;x<count1.size1();x++)
    D += std::abs(count1(x,y)-count2(x,y));

  D /= 2.0;

  assert(D <= 1.0);

  return D;
}


void compute_tv(int i,int j,int L1,int L2,
		vector<vector<vector<double> > >&  TV,
		const vector<matrix<int> >& M1,
		const vector<matrix<int> >& M2)
{
  matrix<double> count1 = get_counts(i,j,L1,L2,M1);
  matrix<double> count2 = get_counts(i,j,L1,L2,M2);

  for(int x=0;x<L1;x++)
    TV[i][x][j] = compute_tv_12(x,count1,count2);

  for(int y=0;y<L2;y++)
    TV[j][y][i] = compute_tv_21(y,count1,count2);
}

alignment get_alignment(const matrix<int>& M, alignment& A1) 
{
  alignment A2 = A1;
  A2.changelength(M.size1());

  // get letters information
  vector<vector<int> > sequences;
  for(int i=0;i<A1.n_sequences();i++) {
    vector<int> sequence;
    for(int c=0;c<A1.length();c++) {
      if (not A1.gap(c,i))
	sequence.push_back(A1(c,i));
    }
    sequences.push_back(sequence);
  }

  for(int i=0;i<A2.n_sequences();i++) {
    for(int c=0;c<A2.length();c++) {
      int index = M(c,i);

      if (index >= 0)
	index = sequences[i][index];

      A2.set_value(c,i, index);
    }
  }

  return A2;
}



int main(int argc,char* argv[]) 
{ 
  try {
    //---------- Parse command line  -------//
    CLI::App app{"Compare two alignment distributions and annotate a target alignment.", "alignment-compare"};
    app.usage("Usage: alignment-compare [OPTIONS] SAMPLE1 SAMPLE2 TARGET");
    app.get_formatter()->long_option_alignment_ratio(0.2f);
    string sample1_file, sample2_file, target_file, alphabet_name;
    unsigned long seed = 0;
    int max_alignments = 1000;
    bool verbose = false;
    app.add_option("SAMPLE1", sample1_file, "First alignment sample file")->required()->type_name("");
    app.add_option("SAMPLE2", sample2_file, "Second alignment sample file")->required()->type_name("");
    app.add_option("TARGET", target_file, "Target alignment to annotate ('-' reads stdin)")
        ->required()->type_name("");
    app.add_option("--alphabet", alphabet_name,
                   "Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop")
        ->type_name("ALPHABET");
    app.add_option("--seed", seed, "Random seed")->type_name("SEED");
    app.add_option("--max-alignments", max_alignments, "Maximum retained alignments per sample (-1: unlimited)")
        ->type_name("N")->capture_default_str();
    app.add_flag("--verbose", verbose, "Output more log messages on stderr");
    try
    {
      app.parse(argc, argv);
    }
    catch (const CLI::ParseError& error)
    {
      // Let CLI11 print help or diagnostics, retaining the tool's 0/1 exit statuses.
      app.exit(error);
      return error.get_exit_code() == 0 ? 0 : 1;
    }
    if (verbose) log_verbose = 1;

    // The target is required; load it before reading samples or computing scores.
    alignment A = chop_internal(load_alignment(target_file, alphabet_name));

    //---------- Initialize random seed -----------//
    if (app.count("--seed")) {
      myrand_init(seed);
    }
    else
      seed = myrand_init();

    if (log_verbose)
      cerr<<"alignment-compare: random seed = "<<seed<<endl<<endl;
    
    //------------ Load alignment and tree ----------//
    vector<alignment> alignments1;
    vector<alignment> alignments2;
    vector<matrix<int> > M1;
    vector<matrix<int> > M2;

    load_alignments(alignments1, sample1_file, alphabet_name, max_alignments, "#1");
    load_alignments(alignments2, sample2_file, alphabet_name, max_alignments, "#2");

    
    int N = alignments1[0].n_sequences();
    const auto names = sequence_names(alignments1[0]);
    vector<int> L(N);
    for(int i=0;i<L.size();i++)
      L[i] = alignments1[0].seqlength(i);

    // A residue is identified by sequence name and position, not its row in a sample file.
    // Normalize rows and validate every retained alignment before using those positions as indices.
    for(int sample=0;sample<2;sample++)
    {
      auto& alignments = sample == 0 ? alignments1 : alignments2;
      const auto& filename = sample == 0 ? sample1_file : sample2_file;
      for(int k=0;k<alignments.size();k++)
      {
        auto& current = alignments[k];
        try
        {
          // Name-based reordering must not discard extra rows or reuse a duplicate name.
          check_names_unique(current);
          if (current.n_sequences() != N)
            throw myexception()<<"Expected "<<N<<" sequences, but found "<<current.n_sequences()<<".";
          if (sequence_names(current) != names)
            current = reorder_sequences(current, names);
          check_same_sequence_lengths(L, current);

          // Counts distinguish a known residue index from a gap. Unknown presence cannot be
          // indexed or treated as a gap without changing the homology distribution being compared.
          for(int i=0;i<current.n_sequences();i++)
            for(int c=0;c<current.length();c++)
              if (current(c,i) == alphabet::unknown)
                throw myexception()<<"Unknown gap/residue status in sequence '"<<current.seq(i).name
                                   <<"', processed column "<<c+1<<": samples cannot contain '?' or '='";
        }
        catch (myexception& e)
        {
          e.prepend("Alignment sample '"+filename+"', retained alignment "+std::to_string(k+1)+": ");
          throw;
        }
      }
    }

    // Preserve the target's row order while checking its residue indices against the sample lengths.
    vector<int> pi;
    try
    {
      check_names_unique(A);
      pi = compute_mapping(sequence_names(A), names);
      for(int i=0;i<A.n_sequences();i++)
        if (A.seqlength(i) != L[pi[i]])
          throw myexception()<<"Sequence '"<<A.seq(i).name<<"': length "<<A.seqlength(i)
                             <<" differs from expected length "<<L[pi[i]];
    }
    catch (std::exception& e)
    {
      throw myexception()<<"Target alignment '"<<target_file<<"': "<<e.what();
    }
    
    //--------- Construct alignment indexes ---------//
    for(auto& alignment: alignments1)
      M1.push_back(M(alignment));

    for(auto& alignment: alignments2)
      M2.push_back(M(alignment));

    //-------- Compute tv distances for single homology statements --------//
    vector< vector< vector<double> > > TV1(N);
    for(int i=0;i<N;i++) 
      TV1[i] = vector< vector<double> >(L[i],vector<double>(N));

    for(int i=0;i<N;i++)
      for(int j=0;j<i;j++)
	compute_tv(i,j,L[i],L[j],TV1,M1,M2);

    //-------- compute final distances -------//
    vector< vector<double> > TV2(N);
    for(int i=0;i<N;i++) {
      TV2[i].resize(L[i]);
      for(int x=0;x<TV2[i].size();x++)
	TV2[i][x] = max(TV1[i][x]);
    }

    //------------- output info --------------//
    matrix<double> m(A.length(),A.n_sequences());
    for(int i=0;i<A.n_sequences();i++) {
      int x=0;
      for(int c=0;c<A.length();c++) {
	if (A.character(c,i)) {
	  m(c,i) = 1.0 - TV2[pi[i]][x];
	  x++;
	}
	else
	  m(c,i) = 1.0;
      }
    }
    
    cout<<join(sequence_names(A),' ')<<endl;
    for(int c=0;c<A.length();c++) {
      for(int i=0;i<A.n_sequences();i++)
	cout<<m(c,i)<<" ";
      cout<<1.0<<endl;
    }
  }
  catch (std::exception& e) {
    std::cerr<<"alignment-compare: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
