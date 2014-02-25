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

#include "index-matrix.H"
#include <fstream>
#include <string>
#include <cmath>
#include <list>
#include <numeric>
#include "myexception.H"
#include "math/logsum.H"
#include "optimize.H"
#include "findroot.H"
#include "util.H"
#include "alignment/alignment-util.H"
#include "distance-methods.H"
#include "rng.H"
#include "alignment/alignment-util.H"
#include "statistics.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using namespace std;

void do_setup(const variables_map& args,vector<alignment>& alignments) 
{
  //------------ Try to load alignments -----------//
  int maxalignments = args["max-alignments"].as<int>();
  unsigned skip = args["skip"].as<unsigned>();

  // --------------------- try ---------------------- //
  if (log_verbose) std::cerr<<"alignment-identity: Loading alignments...";
  list<alignment> As = load_alignments(std::cin,load_alphabets(args),skip,maxalignments);
  alignments.insert(alignments.begin(),As.begin(),As.end());
  if (log_verbose) std::cerr<<"done. ("<<alignments.size()<<" alignments)"<<std::endl;
  if (not alignments.size())
    throw myexception()<<"Alignment sample is empty.";
}

#undef NDEBUG

unsigned n_with_identity(const alignment& A,int s1,int s2,double I)
{
  // Get matches
  vector<int> F(A.length()+1);

  unsigned L=0;
  unsigned T = 0;
  F[0]=0;
  for(int i=0;i<A.length();i++) 
  {
    if (not A.character(i,s1) and not A.character(i,s2)) continue;

    L++;
    
    if (A(i,s1) == A(i,s2))
      T++;

    F[L] = T;
  }
  F.resize(L+1);

  // Get positions
  vector<int> FI(T+1);
  FI[0]=0;
  for(int i=0;i<L;i++)
    if (F[i+1] > F[i])
      FI[F[i+1]] = i+1;

  // tag positions that 
  vector<int> tagged(L,0);

  const unsigned w = 4;
  for(int i=1;i<=T;i++) {
    for(int j=20;j>=w;j--) {
      int i2 = i+j;
      if (i2 > T) continue;
      assert(FI[i]  > 0 and FI[i]  <=L);
      assert(FI[i2] > 0 and FI[i2] <=L);
      assert(FI[i2] > FI[i]);

      if (double(i2-i+1)/(FI[i2]-FI[i]+1) > I) {
	for(int k=FI[i];k<=FI[i2];k++)
	  tagged[k-1]=1;
	break;
      }
    }
  }

  return sum(tagged);
}


variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("alphabet",value<string>(),"Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.")
    ("with-indels", "Calculate percent-identity w/ indels")
    ("seed", value<unsigned long>(),"random seed")
    ("skip",value<unsigned>()->default_value(0),"number of tree samples to skip")
    ("max-alignments",value<int>()->default_value(1000),"maximum number of alignments to analyze")
    ("cutoff",value<string>()->default_value("0.75"),"ignore events below this probability")
    ("identity",value<double>()->default_value(0.4),"Find fraction of sequences that have this level of identity.")
    ("analysis",value<string>(),"What analysis to do: default, matrix, nmatrix")
    ("strict","require all implied pairs pass the cutoff")
    ;

  variables_map args;     
  store(parse_command_line(argc, argv, all), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-identity [OPTIONS] < sequence-file\n";
    cout<<"Report statistics on pairwise identity.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}


Matrix aligned_fraction(const variables_map& args,unsigned N,const vector<int>& L, 
			    const alignment& A, const Edges& E, double cutoff)
{
  Matrix fraction_aligned(N,N);
  matrix<int> Matches(N,N);
  matrix<int> Gaps(N,N);
  
  
  for(int s1=0;s1<N;s1++) {
    for(int s2=0;s2<N;s2++) {
      fraction_aligned(s1,s2)=0;
      Matches(s1,s2) = 0;
      Gaps(s1,s2)=0;
    }
    fraction_aligned(s1,s1) = 1.0;
    Matches(s1,s1) = L[s1];
  }
  
  // count supported matches and gaps at each level
  foreach(e,E) 
  {
    // supposedly the list is sorted, and decreasing in e->p
    if (e->p < cutoff) break;
    
    //	cout<<"s1 = "<<e->s1<<" s2 = "<<e->s2<<"  x1 = "<<e->x2<<" x2 = "<<e->x2<<" p = "<<e->p<<endl;
    
    if (e->x1 >=0 and e->x2 >=0) {
      Matches(e->s1, e->s2)++;
      Matches(e->s2, e->s1)++;
    }
    else {
      Gaps(e->s1, e->s2)++;
      Gaps(e->s2, e->s1)++;
    }
  }
  
  if (args["analysis"].as<string>() == "matrix")
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++)
	fraction_aligned(s1,s2) = (2.0*Matches(s1,s2)+1.0*Gaps(s1,s2))/(L[s1]+L[s2]);
  else
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++)
	fraction_aligned(s1,s2) = Matches(s1,s2);
  
  return fraction_aligned;
}

double ave_aligned_fraction(const vector< matrix<int> >& Ms, 
			    int s1,int s2,int L1, int L2) 
{
  matrix<int> count(L1+1,L2+1);
  for(int i=0;i<count.size1();i++)
    for(int j=0;j<count.size2();j++)
      count(i,j) = 0;

  // get counts of each against each
  for(int i=0;i<Ms.size();i++) {
    const matrix<int>& M = Ms[i];

    for(int c=0;c<M.size1();c++) {
      int index1 = M(c,s1);
      int index2 = M(c,s2);
      if (index1 != -3 and index2 != -3)
	count(index1 + 1, index2 + 1)++;
    }
  }
  count(0,0) = 0;

  valarray<int> max1(0.0, L1);
  valarray<int> max2(0.0, L2);

  for(int i=0;i<count.size1();i++)
    for(int j=0;j<count.size2();j++) {
      if (i>0)
	max1[i-1] = std::max(max1[i-1],count(i,j));
      if (j>0)
	max2[j-1] = std::max(max2[j-1],count(i,j));
    }

  int total = max1.sum()+max2.sum();
  if (log_verbose) cerr<<"alignment-identity: "<<total<<"   "<<double(total)/(max1.size()+max2.size())/Ms.size()<<endl;

  return double(total)/(max1.size()+max2.size())/Ms.size();
}

/*
 * Goal: Rewrite to output statistics in a tracer-readable form.
 * Problem: Sometimes this would lead to N*N output variables... we just output the minimum.
 * Current Solution: output the minimum average
 * Alternative: output the average minimum
 */ 

int main(int argc,char* argv[]) 
{ 
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);
    bool strict = args.count("strict")>0;

    //---------- Initialize random seed -----------//
    unsigned long seed = 0;
    if (args.count("seed")) {
      seed = args["seed"].as<unsigned long>();
      myrand_init(seed);
    }
    else
      seed = myrand_init();
    if (log_verbose) cerr<<"alignment-identity: random seed = "<<seed<<endl<<endl;
    
    //------------ Load alignments ---- ----------//
    vector<alignment> alignments;
    vector<matrix<int> > Ms;

    do_setup(args,alignments);
    for(int i=0;i<alignments.size();i++)
      alignments[i] = chop_internal(alignments[i]);

    if (not alignments.size()) 
      throw myexception()<<"Didn't read any alignments!";      

    int N = alignments[0].n_sequences();
    if (alignments.size() > 1) {
      assert(alignments[1].n_sequences() == N);
      assert(alignments[1].seqlength(N-1) == alignments[0].seqlength(N-1));
    }
    const alignment& A = alignments[0];

    vector<int> L(N);
    for(int i=0;i<L.size();i++)
      L[i] = A.seqlength(i);
    
    //--------- Construct alignment indexes ---------//
    for(int i=0;i<alignments.size();i++)
      Ms.push_back(M(alignments[i]));

    //--------- Get list of supported pairs ---------//
    Edges E(L);

    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<s1;s2++)
	add_edges(E,Ms,s1,s2,L[s1],L[s2],0.5);

    E.build_index();

    //--------- Build alignment from list ---------//
    vector<double> cutoffs = split<double>(args["cutoff"].as<string>(),',');
    for(int i=0;i<cutoffs.size();i++)
      if (cutoffs[i] < 0.5) cutoffs[i] = 0.5;
    double cutoff=cutoffs[0];
    
    //--------- matrix of alignabilities ----------//
    // get fraction of residues aligned at some level
    if (args.count("analysis") and args["analysis"].as<string>() == "statistics")
    {
      vector<string> h;
      h.push_back("L");
	
      h.push_back("letter_var");
      h.push_back("letter_invar");
      h.push_back("letter_inform");
      h.push_back("letter_uninform");

      h.push_back("gap_var");
      h.push_back("gap_invar");
      h.push_back("gap_inform");
      h.push_back("gap_uninform");
      
      h.push_back("both_var");
      h.push_back("both_invar");
      h.push_back("both_inform");
      h.push_back("both_uninform");

      cout<<join(h,'\t')<<endl;

      for(int i=0; i<alignments.size(); i++)
      {
	const alignment& AA = alignments[i];
	int L = AA.length();

	int letter_var = n_letter_variable_sites(AA);
	int letter_invar = L - letter_var;
	int letter_inform = n_letter_informative_sites(AA);
	int letter_uninform = L - letter_inform;

	int gap_var = n_gap_variable_sites(AA);
	int gap_invar = L - gap_var;
	int gap_inform = n_gap_informative_sites(AA);
	int gap_uninform = L - gap_inform;

	int both_var = (letter_variable_sites(AA) | gap_variable_sites(AA)).count();
	int both_invar = L - both_var;
	int both_inform = (letter_informative_sites(AA) | gap_informative_sites(AA)).count();
	int both_uninform = L - both_inform;

	vector<int> v;

	v.push_back(L);
	
	v.push_back(letter_var);
	v.push_back(letter_invar);
	v.push_back(letter_inform);
	v.push_back(letter_uninform);

	v.push_back(gap_var);
	v.push_back(gap_invar);
	v.push_back(gap_inform);
	v.push_back(gap_uninform);

	v.push_back(both_var);
	v.push_back(both_invar);
	v.push_back(both_inform);
	v.push_back(both_uninform);
	cout<<join(v,'\t')<<endl;
      }
      exit(0);
    }
    if (args.count("analysis") and 
	(args["analysis"].as<string>() == "matrix" or args["analysis"].as<string>() == "nmatrix")) 
    {
      vector<string> s_out;
      vector<double> v_out;
      s_out.push_back("level");
      for(int s1=0;s1<N;s1++)
	for(int s2=0;s2<s1;s2++) {
	  string name1 = A.seq(s1).name;
	  string name2 = A.seq(s2).name;
	  if (std::less<string>()(name2,name1)) std::swap(name1,name2);
	  s_out.push_back(name1 +"-"+name2);
	}
      cout<<join(s_out,'\t')<<endl;
      
      for(int i=0;i<cutoffs.size();i++) {
	Matrix AF = aligned_fraction(args,N,L,A,E,cutoffs[i]);
	
	v_out.clear();
	v_out.push_back(cutoffs[i]);
	for(int s1=0;s1<N;s1++)
	  for(int s2=0;s2<s1;s2++)
	    v_out.push_back(AF(s1,s2));
	cout<<join(v_out,'\t')<<endl;
      }
      
      exit(0);
    }
    // get some kind of distance matrix to find out which pairs are badly aligned
    else if (args.count("analysis") and args["analysis"].as<string>() == "d-matrix")
    {
      Matrix D(N,N);
      for(int s1=0;s1<N;s1++)
	for(int s2=0;s2<=s1;s2++)
	  if (s1 == s2)
	    D(s1,s2) = 0;
	  else
	    D(s2,s1) = D(s1,s2) = 1.0-ave_aligned_fraction(Ms,s1,s2,L[s1],L[s2]);

      for(int i=0;i<D.size1();i++) {
	vector<double> v(D.size2());
	for(int j=0;j<v.size();j++)
	  v[j] = D(i,j);
	cout<<join(v,'\t')<<endl;
      }

      exit(0);
      
    }


    //-------- Build a beginning alignment --------//
    index_matrix M = unaligned_matrix(L);

    M.merge(E,cutoff,strict);

    matrix<int> M2 = get_ordered_matrix(M);

    alignment consensus = get_alignment(M2,alignments[0]);

    //---------- Get %identity ------------//
    bool gaps_count = args.count("with-indels");
    vector<vector<valarray<double> > > identity(N,vector<valarray<double> >(N));
      for(int s1=0;s1<N;s1++)
	for(int s2=0;s2<N;s2++) 
	  identity[s1][s2].resize(alignments.size());

    for(int i=0;i<alignments.size();i++)
    {
      for(int s1=0;s1<N;s1++)
	for(int s2=0;s2<N;s2++)
	  identity[s1][s2][i] = fraction_identical(alignments[i],s1,s2,gaps_count);
    }

    Matrix identity_median(N,N);
    Matrix identity_Q1(N,N);
    Matrix identity_Q2(N,N);
    const double p = 0.05;
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++) {
	identity_median(s1,s2) = statistics::median(identity[s1][s2]);
	identity_Q1(s1,s2) = statistics::quantile(identity[s1][s2],p/2);
	identity_Q2(s1,s2) = statistics::quantile(identity[s1][s2],1.0-p/2);
      }

    int s1_min=0; int s2_min=0;
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++)
	if (identity_median(s1,s2) < identity_median(s1_min,s2_min)) {
	  s1_min = s1;
	  s2_min = s2;
	}

    cout<<"Min identity = "<<identity_median(s1_min,s2_min)<<" ("<<identity_Q1(s1_min,s2_min)<<","<<identity_Q2(s1_min,s2_min)<<")  ["<<A.seq(s1_min).name<<","<<A.seq(s2_min).name<<"]"<<endl;
    
    //---------- Get % WITH identity I ------------//
    const double I = args["identity"].as<double>();
    vector<vector<valarray<double> > > ifraction(N,vector<valarray<double> >(N));
      for(int s1=0;s1<N;s1++)
	for(int s2=0;s2<N;s2++) 
	  ifraction[s1][s2].resize(alignments.size());

    for(int i=0;i<alignments.size();i++)
    {
      for(int s1=0;s1<N;s1++)
	for(int s2=0;s2<N;s2++)
	  ifraction[s1][s2][i] = n_with_identity(alignments[i],s1,s2,I);
    }

    Matrix ifraction_median(N,N);
    Matrix ifraction_Q1(N,N);
    Matrix ifraction_Q2(N,N);
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++) {
	ifraction_median(s1,s2) = statistics::median(ifraction[s1][s2]);
	ifraction_Q1(s1,s2) = statistics::quantile(ifraction[s1][s2],p/2);
	ifraction_Q2(s1,s2) = statistics::quantile(ifraction[s1][s2],1.0-p/2);
      }

    s1_min=0; s2_min=0;
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++)
	if (ifraction_median(s1,s2) < ifraction_median(s1_min,s2_min)) {
	  s1_min = s1;
	  s2_min = s2;
	}

    cout<<"Min ifraction = "<<ifraction_median(s1_min,s2_min)<<" ("<<ifraction_Q1(s1_min,s2_min)<<","<<ifraction_Q2(s1_min,s2_min)<<")  ["<<A.seq(s1_min).name<<","<<A.seq(s2_min).name<<"]"<<endl;
    
    //---------- Get %-homology > cutoff --------------//
    Matrix percent_aligned(N,N);
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++)
	percent_aligned(s1,s2) = fraction_homologous(consensus,s1,s2);

    s1_min=0;s2_min=0;
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++)
	if (percent_aligned(s1,s2) < percent_aligned(s1_min,s2_min)) {
	  s1_min = s1;
	  s2_min = s2;
	}

    cout<<"Min %aligned = "<<percent_aligned(s1_min,s2_min)<<"  ["<<A.seq(s1_min).name<<","<<A.seq(s2_min).name<<"]"<<endl;
    
    //---------- Get #-homology > cutoff --------------//
    Matrix n_aligned(N,N);
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++)
	n_aligned(s1,s2) = n_homologous(consensus,s1,s2);

    s1_min=0;s2_min=0;
    for(int s1=0;s1<N;s1++)
      for(int s2=0;s2<N;s2++)
	if (n_aligned(s1,s2) < n_aligned(s1_min,s2_min)) {
	  s1_min = s1;
	  s2_min = s2;
	}

    cout<<"Min #aligned = "<<n_aligned(s1_min,s2_min)<<"  ["<<A.seq(s1_min).name<<","<<A.seq(s2_min).name<<"]"<<endl;
    
  }
  catch (std::exception& e) {
    std::cerr<<"alignment-identity: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
