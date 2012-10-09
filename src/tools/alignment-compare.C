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
#include "myexception.H"
#include "alignment/alignment.H"
#include "mytypes.H"
#include "logsum.H"
#include "optimize.H"
#include "findroot.H"
#include "util.H"
#include "alignment/alignment-util.H"
#include "distance-methods.H"
#include "rng.H"

#ifdef NDEBUG
#undef NDEBUG
#endif

#include <boost/program_options.hpp>
#include <boost/shared_ptr.hpp>
#include "io.H"

namespace po = boost::program_options;
using po::variables_map;

using namespace std;

void load_alignments(vector<alignment>& alignments,
		     const string& filename, 
		     const vector<object_ptr<const alphabet> >& alphabets,
		     int maxalignments,
		     string what)
{
  if (what.size() > 0)
    what = string(" ")+what;

  if (log_verbose)
    std::cerr<<"alignment-compare: Loading alignment sample"<<what<<"...";

  checked_ifstream file(filename, "alignment sample file");

  list<alignment> As = load_alignments(file, alphabets, 0, maxalignments);

  alignments.clear();
  alignments.insert(alignments.begin(),As.begin(),As.end());

  if (log_verbose)
    std::cerr<<"done. ("<<alignments.size()<<" alignments)"<<std::endl;
  if (not alignments.size())
    throw myexception()<<"Alignment sample"<<what<<" is empty.";  

  for(int i=0;i<alignments.size();i++)
    alignments[i] = chop_internal(alignments[i]);


  if (alignments.size() > 1) {
    int N = alignments[0].n_sequences();
    assert(alignments[1].n_sequences() == N);
    assert(alignments[1].seqlength(N-1) == alignments[0].seqlength(N-1));
  }
}
		     


void do_setup(const variables_map& args,
	      vector<alignment>& alignments1, 
	      vector<alignment>& alignments2) 
{
  //------------ Try to load alignments -----------//
  int maxalignments = args["max-alignments"].as<int>();

  // --------------------- try ---------------------- //
  {
    string filename = args["file1"].as<string>();
    load_alignments(alignments1,filename,load_alphabets(args),maxalignments,"#1");
  }

  {
    string filename = args["file2"].as<string>();
    load_alignments(alignments2,filename,load_alphabets(args),maxalignments,"#2");
  }
}


variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // Named options
  options_description invisible("Invisible options");
  invisible.add_options()
    ("file1", value<string>(),"first alignment file")
    ("file2", value<string>(),"second alignment file");

  // 
  options_description visible("Allowed options");
  visible.add_options()
    ("help", "produce help message")
    ("alphabet",value<string>(),"Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.")
    ("seed", value<unsigned long>(),"random seed")
    ("align", value<string>(),"alignment to output values for.")
    ("max-alignments",value<int>()->default_value(1000),"maximum number of alignments to analyze")
    ("verbose","Output more log messages on stderr.")
    ;

  options_description all("All options");
  all.add(invisible).add(visible);

  // positional options
  positional_options_description p;
  p.add("file1", 1);
  p.add("file2", 2);

  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-compare <alignment-file1> <alignment-file2> [OPTIONS]\n";
    cout<<"Compare two alignment distributions.\n";
    cout<<" o label each residue by its maximum pairwise homology deviance.\n";
    cout<<" o output AU-style annotations for the alignment given by --align.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  if (args.count("verbose")) log_verbose = 1;

  return args;
}

ublas::matrix<double> get_counts(int s1,int s2,int L1,int L2,
			      const vector<ublas::matrix<int> >& Ms)
{
  const double unit = 1.0/Ms.size();

  // Initialize the count matrix
  ublas::matrix<double> count(L1+1,L2+1);
  for(int i=0;i<count.size1();i++)
    for(int j=0;j<count.size2();j++)
      count(i,j) = 0;

  // get counts of each against each
  for(int i=0;i<Ms.size();i++) 
  {
    const ublas::matrix<int>& M = Ms[i];

    for(int c=0;c<M.size1();c++) {
      int index1 = M(c,s1);
      int index2 = M(c,s2);
      count(index1 + 1, index2 + 1) += unit;
    }
  }
  count(0,0) = 0;

  return count;
}

double compute_tv_12(int x,const ublas::matrix<double>& count1,const ublas::matrix<double>& count2)
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


double compute_tv_21(int y,const ublas::matrix<double>& count1,const ublas::matrix<double>& count2)
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
		const vector<ublas::matrix<int> >& M1,
		const vector<ublas::matrix<int> >& M2)
{
  ublas::matrix<double> count1 = get_counts(i,j,L1,L2,M1);
  ublas::matrix<double> count2 = get_counts(i,j,L1,L2,M2);

  for(int x=0;x<L1;x++)
    TV[i][x][j] = compute_tv_12(x,count1,count2);

  for(int y=0;y<L2;y++)
    TV[j][y][i] = compute_tv_21(y,count1,count2);
}

alignment get_alignment(const ublas::matrix<int>& M, alignment& A1) 
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
    variables_map args = parse_cmd_line(argc,argv);

    //---------- Initialize random seed -----------//
    unsigned long seed = 0;
    if (args.count("seed")) {
      seed = args["seed"].as<unsigned long>();
      myrand_init(seed);
    }
    else
      seed = myrand_init();

    if (log_verbose)
      cerr<<"alignment-compare: random seed = "<<seed<<endl<<endl;
    
    //------------ Load alignment and tree ----------//
    vector<alignment> alignments1;
    vector<alignment> alignments2;
    vector<ublas::matrix<int> > M1;
    vector<ublas::matrix<int> > M2;

    do_setup(args,alignments1,alignments2);

    
    int N = alignments1[0].n_sequences();

    {
      int N2 = alignments2[0].n_sequences();
      if (N != N2)
	throw myexception()<<"Sample #1 has "<<N<<" sequences, but sample #2 has "<<N2<<".";
    }

    vector<int> L(N);
    for(int i=0;i<L.size();i++) {
      L[i] = alignments1[0].seqlength(i);
      int L2 = alignments2[0].seqlength(i);
      if (L[i] != L2)
	throw myexception()<<"Sequence "<<i+1<<": sample #1 has length "<<L[i]<<
	  " but sample #2 has length "<<L2<<".";
    }
    
    //--------- Construct alignment indexes ---------//
    for(int i=0;i<alignments1.size();i++)
      M1.push_back(M(alignments1[i]));

    for(int i=0;i<alignments2.size();i++)
      M2.push_back(M(alignments2[i]));

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
    alignment A = load_A(args,false);

    vector<int> pi = compute_mapping(sequence_names(A),sequence_names(alignments1[0]));

    ublas::matrix<double> m(A.length(),A.n_sequences());
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
