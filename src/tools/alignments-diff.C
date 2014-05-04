/*
   Copyright (C) 2008-2010 Benjamin Redelings

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
#include "optimize.H"
#include "findroot.H"
#include "util.H"
#include "setup.H"
#include "alignment/alignment-util.H"
#include "distance-methods.H"

#include <boost/program_options.hpp>
#include <boost/shared_ptr.hpp>

namespace po = boost::program_options;
using po::variables_map;
using boost::shared_ptr;

using std::string;
using std::vector;
using std::endl;

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

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("alignment1", value<string>(),"First alignment")
    ("alignment2", value<string>(),"Second alignment")
    ("alphabet",value<string>(),"set to 'Codons' to prefer codon alphabets")
    ("merge","Stack the two alignments into one alignment with duplicate names")
    ("fill",value<string>()->default_value("gap"),"blank columns filled with: gap or unknown")
    ("differences-file,d",value<string>(),"Filename to store differences in AU format")
    ;

  // positional options
  positional_options_description p;
  p.add("alignment1", 1);
  p.add("alignment2", 2);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-align alignment-file1 alignment-file2 ... [OPTIONS] < alignments-file\n";
    cout<<"Align two alignments for comparison.\n";
    cout<<all<<"\n";
    exit(0);
  }

  if (not args.count("alignment1"))
    throw myexception()<<"No alignment file names given!";

  if (not args.count("alignment2"))
    throw myexception()<<"Only one alignment file names given!";

  return args;
}

int score(const matrix<int>& M1, const matrix<int>& M2, int c1, int c2)
{
  assert(M1.size2() == M2.size2());
  const int N = M1.size2();

  int total = 0;

  for(int i=0;i<N;i++) 
  {
    if (M1(c1,i) == alphabet::unknown or M2(c2,i) == alphabet::unknown)
      continue;

    if (M1(c1,i) == alphabet::unknown or M2(c2,i) == alphabet::gap)
      continue;

    if (M1(c1,i) == M2(c2,i))
      total++;
  }

  return total;
}

alignment stretch(const alignment& A1, const vector<int>& columns,int fill)
{
  alignment A2 = A1;
  A2.changelength(columns.size());
  for(int c=0;c<A2.length();c++) {
    if (columns[c] == -1)
      for(int i=0;i<A2.n_sequences();i++)
	A2.set_value(c,i, fill);
    else
      for(int i=0;i<A2.n_sequences();i++)
	A2.set_value(c,i, A1(columns[c],i) );
  }
  return A2;
}

void align_alignments(const matrix<int>& M1, const matrix<int>& M2, vector<int>& columns1, vector<int>& columns2)
{
  //--------- Construct Forward Matrix ---------//

  matrix<int> F(M1.size1()+1, M2.size1()+1);

  for(int i=0;i<F.size1();i++)
    for(int j=0;j<F.size2();j++) {
      F(i,j) = 0;
      if (i>0)         F(i,j) = std::max(F(i,j),F(i-1,j  ));
      if (j>0)         F(i,j) = std::max(F(i,j),F(i  ,j-1));
      if (i>0 and j>0) F(i,j) = std::max(F(i,j),F(i-1,j-1)+score(M1,M2,i-1,j-1));
    }

  //-------------- Find best path --------------//

  int i=F.size1()-1;
  int j=F.size2()-1;

  vector<int> S(3);
  while (i>0 or j>0) 
  {
    if (i>0) 
      S[0] = F(i-1,j);
    else
      S[0] = -1;

    if (j>0)
      S[1] = F(i,j-1);
    else
      S[1] = -1;

    if (i>0 and j>0)
      S[2] = F(i-1,j-1)+score(M1,M2,i-1,j-1);
    else
      S[2] = -1;

    int C = argmax(S);

    // cerr<<i<<"  "<<j<<"   C="<<C<<"    "<<S[0]<<" "<<S[1]<<" "<<S[2]<<endl;

    if (C == 0) {
      i--;
      columns1.push_back(i);
      columns2.push_back(-1);
    }
    else if (C == 1)
    {
      j--;
      columns1.push_back(-1);
      columns2.push_back(j);
    }
    else {
      assert(C==2);
      i--;
      j--;
      columns1.push_back(i);
      columns2.push_back(j);
    }
  }

  std::reverse(columns1.begin(),columns1.end());
  std::reverse(columns2.begin(),columns2.end());
}


int main(int argc,char* argv[]) 
{ 
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);
    
    //----------- Load alignment and tree ---------//
    string filename1 = args["alignment1"].as<string>();
    string filename2 = args["alignment2"].as<string>();

    alignment A1 = load_alignment(filename1,load_alphabets(args));
    check_names_unique(A1);

    alignment A2 = load_alignment(filename2,load_alphabets(args));
    A2 = reorder_sequences(A2, sequence_names(A1));

    matrix<int> M1 = M(A1);
    matrix<int> M2 = M(A2);

    int fill = alphabet::gap;
    if (args.count("fill") and args["fill"].as<string>() == "unknown")
      fill = alphabet::unknown;

    vector<int> columns1;
    vector<int> columns2;
    align_alignments(M1, M2, columns1, columns2);

    //----------- Write out Results ----------//
    alignment A1b = stretch(A1,columns1,fill);
    alignment A2b = stretch(A2,columns2,fill);

    matrix<int> M1b = M(A1b);
    matrix<int> M2b = M(A2b);

    if (args.count("merge")) 
    {
      alignment O = A1b;
      // add a blank row

      O.add_sequence(sequence("",""));
      for(int c=0;c<O.length();c++)
	O.set_value(c,O.n_sequences()-1, fill);

      // append the second alignment as extra rows
      vector<sequence> sequences = A2b.convert_to_sequences();
      for(int i=0;i<A2b.n_sequences();i++) {
	sequence s = sequences[i];
	s.name += '2';
	O.add_sequence(s);
      }

      cout<<O<<endl;

      if (args.count("differences-file")) 
      {
	const int N = A1b.n_sequences();

	//	cerr<<"N = "<<N<<"  O is "<<O.n_sequences()<<" x "<<O.length()<<endl;
	// construct difference matrix C in AU format
	matrix<int> D(O.length(), O.n_sequences()+1);
	for(int c=0;c<D.size1();c++) {
	  D(c,N)=0;
	  D(c,2*N+1)=0;
	  for(int j=0;j<N;j++) {
	    if (M1b(c,j) == M2b(c,j) and M1b(c,j) >= 0)
	      D(c,j) = D(c,j+1+N)=1;
	    else
	      D(c,j) = D(c,j+1+N)=0;
	  }
	}

	// write out header: sequence names
	string filename = args["differences-file"].as<string>();
	std::ofstream d(filename.c_str());
	for(int i=0;i<O.n_sequences();i++) {
	  d<<O.seq(i).name;
	  if (i !=  O.n_sequences()-1)
	    d<<" ";
	  else
	    d<<endl;
	}

	// write out D matrix
	for(int c=0;c<D.size1();c++) 
	{
	  for(int j=0;j<D.size2();j++) 
	  {
	    d<<D(c,j);
	    if (j != D.size2()-1)
	      d<<" ";
	    else
	      d<<endl;
	  }
	}
      }
    }
    else {
      cout<<A1b<<endl<<endl;
      cout<<A2b<<endl<<endl;
    }
  }
  catch (std::exception& e) {
    std::cerr<<"alignments-diff: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
