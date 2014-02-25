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
#include "alignment/alignment.H"
#include "alignment/alignment-util.H"
#include "util.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::string;
using std::vector;
using std::endl;

using std::cout;
using std::cerr;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("align", value<string>(),"file with sequences and initial alignment")
    ("alphabet",value<string>(),"set to 'Codons' to prefer codon alphabets")
    ("columns,c", value<string>(),"Ranges of columns to keep, like: 1-10,30-")
    ("invariant",value<int>(),"print only sites where this site and <arg> neighbors are invariant.")
    ("differences",value<int>(),"how many sequences may differ from the majority?")
    ("avoid-gaps",value<int>()->default_value(3),"How far from a gap must a column be to be invariant?")
    ("min-constraints",value<int>(),"Minimum # of constraints per column for a constraint to be emitted")
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
    cout<<"Usage: alignment-indices <alignment-file> [OPTIONS]\n";
    cout<<"Show the alignment in terms of the index of each character in its sequence.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}


vector<int> column_count(const alignment& A, int c)
{
  const alphabet& a = A.get_alphabet();
  vector<int> count(a.size()+1,0);

  for(int i=0;i<A.n_sequences();i++) {
    int l = A(c,i);
    if (A.get_alphabet().is_letter(l))
      count[l]++;

    if (l == alphabet::gap or l == alphabet::unknown)
      count.back()++;
  }
  return count;
}

std::pair<vector<int>,vector<int> > find_major_character(const alignment& A,int allowed_differences)
{
  const alphabet& a = A.get_alphabet();

  vector<int> majority(A.length(), alphabet::unknown);

  vector<int> safe(A.length(), 0);

  for(int c=0;c<majority.size();c++) 
  {
    vector<int> count = column_count(A,c);
    
    int max_letter = argmax(count);
    majority[c] = max_letter;
    
    if (A.n_sequences() - count[max_letter] <= allowed_differences)
      safe[c] = 1;

    if (max_letter == a.size()) {
      majority[c] = alphabet::gap;
      // Columns with majority gaps are not safe - we should probably get the second-highest letter
      safe[c] = 0;
    }
    
    //    if (safe[c]) {
    //      std::cerr<<"Column "<<c+1<<" is safe: "<<a.lookup(majority[c])<<"\n";
    //    }

  }
  
  return std::pair<vector<int>,vector<int> >(majority,safe);
}

int n_characters_in_column(const matrix<int>& M,int c)
{
  int count = 0;
  for(int s=0;s<M.size2();s++)
    if (M(c,s) >= 0)
      count++;
  return count;
}

bool nearby_gap(const alignment& A, int c, int s, int w)
{
  if (w <=0) return false;

  for(int i=c-w;i<=c+w;i++) {
    if (i < 0) continue;
    if (i >= A.length()) continue;
    if (not A.character(i,s)) return true;
  }
  return false;
}

bool nearby_unsafe(const vector<int>& v, int c, int w)
{
  for(int i=c-w;i<=c+w;i++) {
    if (i < 0) continue;
    if (i >= v.size()) continue;
    if (not v[i]) return true;
  }
  return false;
}

int main(int argc,char* argv[]) 
{ 

  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //------- Try to load alignment --------//
    alignment A = load_A(args);

    vector<unsigned> L = sequence_lengths(A);

    matrix<int> MA = M(A);
    matrix<int> MA2 = MA;

    //------- Write the sequence names ------//
    for(int i=0;i<A.n_sequences();i++) {
      cout<<A.seq(i).name;
      if (i==A.n_sequences()-1)
	cout<<std::endl;
      else
	cout<<" ";
    }
      
    const alphabet& a = A.get_alphabet();


    //------- Determine invariant sites -----//
    int allowed_differences = (A.n_sequences()+3)/10;
    if (args.count("differences"))
      allowed_differences = args["differences"].as<int>();
    int avoid_gaps = args["avoid-gaps"].as<int>();

    std::pair<vector<int>,vector<int> > result = find_major_character(A,allowed_differences);
    vector<int> majority = result.first;
    vector<int> safe     = result.second;

    int invariant = -1;
    vector<int> safe2 = safe;

    bool columns = false;

    if (args.count("columns"))
    {
      columns = true;
      int L = A.length();
      vector<int> columns = parse_multi_range(args["columns"].as<string>(), L);

      safe2 = vector<int>(L,0);
      for(int i=0;i<columns.size();i++) {
	safe2[columns[i]] = 1;
      }
    }
    else if (args.count("invariant"))
    {
      invariant = args["invariant"].as<int>();

      for(int c=0;c<safe2.size();c++)
      {
	if (not safe2[c]) continue;

	// since gap columns are unsafe, insertions make their neighboring columns unsafe, which is probably unfair.
	if (nearby_unsafe(safe,c,invariant)) safe2[c] = 0;

	// Solution to insertions: 
	// Determine a majority next/prev column -- allow 

	//count them as an unsafe character... 

	// Don't issue constraints for letters that are non-majority, or near a gap
	for(int s=0;s<A.n_sequences();s++) {
	  if (A(c,s) != majority[c] or nearby_gap(A,c,s,avoid_gaps))
	    MA2(c,s) = alphabet::gap;
	}

	// We should perhaps check for the previous or next columns having the same value...
      }

    }

    // Constraining less than 2 characters is not meaningful
    int min_constraints = 2;
    if (args.count("min-constraints"))
      min_constraints = std::max(2,args["min-constraints"].as<int>());

    vector<int> last_constrained(A.n_sequences(),-1);
    vector<int> largest_block(A.n_sequences(),0);

    //------- Write the columns ------//
    for(int c=0;c<MA.size1();c++) 
    {
      if (invariant != -1 or columns)
	if (not safe2[c]) continue;

      if (invariant != -1 and n_characters_in_column(MA2,c) < min_constraints) continue;

      // write the indices
      for(int i=0;i<MA2.size2();i++) {
	if (MA2(c,i) == alphabet::gap or MA2(c,i) == alphabet::unknown)
	  cout<<"-";
	else {
	  largest_block[i] = std::max(largest_block[i], MA2(c,i)-last_constrained[i]);
	  last_constrained[i] = MA2(c,i);
	  cout<<MA2(c,i);
	}
	cout<<" ";
      } 

      // start a comment
      cout<<"    #  ";

      // write the column number
      cout<<c+1<<"   ";

      // write the letters
      for(int i=0;i<MA.size2();i++) {
	if (MA(c,i) == MA2(c,i))
	  cout<<a.lookup(A(c,i))<<" ";
	else
	  cout<<"["<<a.lookup(A(c,i))<<"] ";
      }
      cout<<std::endl;
    }
    for(int i=0;i<MA2.size2();i++)
      largest_block[i] = std::max<int>(largest_block[i], L[i]-last_constrained[i]-1);

    if (invariant != -1)
    {
      int L_orig = 0;
      int L_new = 0;
      for(int i=0;i<A.n_sequences();i++) {
	int L1 = A.seqlength(i)+1;
	int L2 = largest_block[i]+1;
	cerr<<i<<":   "<<L1<<" -> "<<L2<<endl;
	L_orig = std::max(L_orig,L1);
	L_new = std::max(L_new,L2);
      }
      cerr<<"max:   "<<L_orig<<" -> "<<L_new<<endl;
    }
  }
  catch (std::exception& e) {
    cerr<<"alignment-indices: Error! "<<e.what()<<endl;
  }
  return 0;
}
