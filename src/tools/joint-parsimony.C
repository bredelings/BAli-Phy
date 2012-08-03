/*
   Copyright (C) 2005-2006,2008 Benjamin Redelings

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
#include <boost/program_options.hpp>

#include "myexception.H"
#include "alignment/alignment.H"

#include "mytypes.H"
#include "logsum.H"
#include "optimize.H"
#include "findroot.H"
#include "util.H"
#include "setup.H"

#include "alignment/alignment-util.H"
#include "tree-util.H"
#include "parsimony.H"
#include "joint-A-T.H"
#include "n_indels.H"

#include "dp/2way.H"
using namespace A2;

using std::cin;
using std::cout;
using std::cerr;
using std::istream;
using std::ifstream;
using std::vector;
using std::string;
using std::endl;

namespace po = boost::program_options;
using po::variables_map;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description invisible("Invisible options");
  invisible.add_options()
    ("alignments", value<string>(),"file of alignment samples")
    ("trees", value<string>(), "file of corresponding tree samples")
    ;

  options_description visible("All options");
  visible.add_options()
    ("help", "produces help message")
    ("subsample",value<unsigned>()->default_value(10),"factor by which to sub-sample trees")
    ("alphabet",value<string>(),"set to 'Codons' to prefer codon alphabets")
    ;

  options_description all("All options");
  all.add(visible).add(invisible);

  // positional options
  positional_options_description p;
  p.add("alignments", 1);
  p.add("trees", 2);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: joint-parsimony <alignments file> <trees file> [OPTIONS]\n";
    cout<<visible<<"\n";
    exit(0);
  }

  return args;
}

// If we could read back in the indel-model state we could
// estimate the number of indels in an indel block.
vector<unsigned> n_indels_with_phase(const alignment& A,const Tree& T, int b)
{
  vector<unsigned> indels(4,0);

  vector<int> pairwiseA = get_path(A, T.branch(b).target(), T.branch(b).source());

  int last_state = states::M;

  for(unsigned i=0,i1=0,i2=0; i<pairwiseA.size(); i++) 
  {
    int current_state = pairwiseA[i];
    
    if (last_state != current_state)
      if ((current_state == states::G1) or (current_state == states::G2)) 
      {
	int phase1 = i1%3;
	int phase2 = i2%3;
	if (phase1 == phase2)
	  indels[phase1]++;
	else
	  indels[3]++;
      }

    last_state = current_state;

    if (current_state == states::M or current_state == states::G2)
      i1++;

    if (current_state == states::M or current_state == states::G1)
      i2++;
  }

  return indels;
}

template <typename T,typename U>
void add(vector<T>& v1,const vector<U>& v2)
{
  for(unsigned i=0;i<v1.size();i++)
    v1[i] += v2[i];
}


vector<unsigned> n_indels_with_phase(const alignment& A,const Tree& T)
{
  vector<unsigned> indels(4,0);
  for(unsigned b=0;b<T.n_branches();b++)
    add(indels,n_indels_with_phase(A,T,b));

  return indels;
}

unsigned total_length_indels2(const alignment& A,const Tree& T)
{
  unsigned total=0;
  for(int c=0;c<A.length();c++)
  {
    for(int b=0;b<T.n_branches();b++) {
      int t = T.branch(b).target();
      int s = T.branch(b).source();
      if (A.gap(c,t) and not A.gap(c,s))
	total++;
      if (A.gap(c,s) and not A.gap(c,t))
	total++;
    }
  }
  return total;
}


int main(int argc,char* argv[]) { 
  try {
    variables_map args = parse_cmd_line(argc,argv);

    joint_A_T J = get_joint_A_T(args,true);

    for(int i=0;i<J.size();i++) {
      const alignment& A = J.A[i];
      const SequenceTree& T = J.T[i];

      cout<<" length = "<<n_mutations(A,T)<<"   ";
      
      cout<<" #indels = "<<n_indels(A,T)<<"   ";

      cout<<" |indels| = "<<total_length_indels2(A,T)<<"   ";

      vector<unsigned> indels = n_indels_with_phase(A,T);

      for(int i=0;i<3;i++)
	cout<<" phase"<<i<<" = "<<indels[i];
      cout<<" no-phase = "<<indels[3];
      cout<<endl;
    }
  }
  catch (std::exception& e) {
    std::cerr<<"joint-parsimony: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
