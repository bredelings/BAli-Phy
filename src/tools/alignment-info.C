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
#include <map>
#include <string>
#include "tree.H"
#include "alignment/alignment.H"
#include "alignment/alignment-util.H"
#include "util.H"
#include "setup.H"
#include "findroot.H"
#include "parsimony.H"
#include "statistics.H"
#include <boost/program_options.hpp>
#include <boost/dynamic_bitset.hpp>

using std::vector;
using std::valarray;
using std::map;
using std::string;
using std::endl;

namespace po = boost::program_options;
using po::variables_map;

using boost::dynamic_bitset;

using std::cout;
using std::cerr;
using std::endl;

using std::string;

template<typename T>
void add(vector<T>& v1,const vector<T>& v2) 
{
  for(int i=0;i<v1.size();i++)
    v1[i] += v2[i];
}


vector<int> find_triplet(const sequence& s,const string& triplet) 
{
  sequence s2 = s;
  s2.strip_gaps();

  vector<int> found(3,0);

  int pos=-1;
  while(pos=s.find(triplet,pos+1),pos != -1)
    found[pos%3]++;

  return found;
}

vector<int> find_triplet(const vector<sequence>& sequences,const string& triplet) 
{
  vector<int> found(3,0);
  for(int i=0;i<sequences.size();i++)
    add(found, find_triplet(sequences[i],triplet) );
  return found;
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
    ("alphabet",value<string>(),"specify the alphabet: DNA, RNA, Amino-Acids, Triplets, or Codons")
    ;

  // positional options
  positional_options_description p;
  p.add("align", 1);
  p.add("tree", 2);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-info <alignment-file> [<tree-file>] [OPTIONS]\n";
    cout<<"Show useful statistics about the alignment.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  cout<<endl;
  return args;
}


int n_letters(const valarray<int>& count,int level) {
  int n = 0;
  for(int l=0;l<count.size();l++)
    if (count[l] > level) n++;
  return n;

}

bool is_informative(const valarray<int>& count,int level) 
{
  int n = n_letters(count,level);
  if (level == 0) assert(n > 0);
  return n>1;
}


double min_identity(const alignment& A,bool gaps_count)
{
  double identity = 1.0;
  for(int i=0;i<A.n_sequences();i++)
    for(int j=0;j<i;j++)
      identity = std::min(identity,fraction_identical(A,i,j,gaps_count));

  return identity;
}

unsigned letter_classes(const alignment& A) 
{
  const alphabet& a = A.get_alphabet();

  // Count the occurrence of the different letters
  unsigned count=0;
  for(int i=0;i<A.length();i++) {
    for(int j=0;j<A.n_sequences();j++) {
      if (alphabet::is_letter_class(A(i,j)) and not a.is_letter(A(i,j)))
	count++;
    }
  }

  return count;
}
struct gap {
  int start;
  int length;
  mutable int type;
  gap(int s,int l,int t):start(s),length(l),type(t) {}
};

bool operator==(const gap& i1,const gap& i2)
{
  return (i1.start== i2.start) and (i1.length == i2.length);
}

bool operator<(const gap& i1,const gap& i2)
{
  if (i1.start < i2.start)
    return true;
  if (i1.start > i2.start)
    return false;
  return (i1.length < i2.length);
}

vector<gap> find_gaps(const alignment& A,int t)
{
  vector<gap> gaps;
  bool prev_gap=false;
  for(int i=0;i<A.length();i++)
  {
    if (A.gap(i,t)) {
      if (prev_gap)
	gaps.back().length++;
      else
	gaps.push_back(gap(i,1,1));
      prev_gap = true;
    }
    else
      prev_gap=false;
  }
  return gaps;
}

unsigned n_insertions(const alignment& A,int start,int length)
{
  unsigned count=0;
  for(int j=0;j<A.n_sequences();j++) {
    bool found = false;
    for(int k=0;k<length and not found;k++)
      if (A.gap(start+k,j))
	found=true;
    if (not found)
      count++;
  }
  return count;
}

map<gap,unsigned> guess_indels(const alignment& A)
{
  map<gap,unsigned> gaps;
  for(int i=0;i<A.n_sequences();i++)
  {
    vector<gap> row = find_gaps(A,i);
    for(int j=0;j<row.size();j++)
      gaps[row[j]]++;
  }

  // flip deletions in many taxa to insertions in few taxa
  for(map<gap,unsigned>::iterator i=gaps.begin();i!=gaps.end();) 
  {
    const gap& g = i->first;
    unsigned ins_count = n_insertions(A,g.start,g.length);
    if (ins_count < i->second) {
      g.type = 2;
      i->second = ins_count;
    }

    // increment counter and remove current element if count==0.
    {
      map<gap,unsigned>::iterator j = i;
      i++;
      if (not j->second)
	gaps.erase(j);
    }
  }
  return gaps;
}

int main(int argc,char* argv[]) 
{ 
  try {
    cerr.precision(10);
    cout.precision(10);
    
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //----------- Load alignment and tree ---------//
    alignment A;
    SequenceTree T;
    if (args.count("tree"))
      load_A_and_T(args,A,T,false);
    else
      A = load_A(args,false);
    const alphabet& a = A.get_alphabet();
    
    //----- Count informative/non-constant sites ----//
    dynamic_bitset<> informative(A.length());
    dynamic_bitset<> informative2(A.length());
    dynamic_bitset<> different(A.length());
    dynamic_bitset<> different2(A.length());
    dynamic_bitset<> contains_a_gap(A.length());

    valarray<int> count(a.size());
    valarray<int> count2(2);
    for(int c=0;c<A.length();c++) {
      count = 0;
      count2 = 0;
      for(int i=0;i<A.n_sequences();i++) {
	int l = A(c,i);
	if (a.is_letter(l))
	  count[l]++;

	if (a.is_feature(l))
	  count2[0]++;
	else if (l == alphabet::gap)
	  count2[1]++;
      }

      different[c]  =   is_informative(count ,0);
      informative[c]  = is_informative(count ,1);

      contains_a_gap[c] = (count2[1]>0);
      different2[c] =   different[c] or contains_a_gap[c];
      informative2[c] = informative[c] or is_informative(count2,1);
    }
    
    int n_different  = different.count();
    int n_same = A.length() - n_different;
    int n_informative  = informative.count();

    int n_different2 = different2.count();
    int n_same2 = A.length() - n_different2;
    int n_informative2 = informative2.count();
    int n_with_gaps = contains_a_gap.count();


    cout.precision(3);

    valarray<double> lengths(A.n_sequences());
    for(int i=0;i<A.n_sequences();i++)
      lengths[i] = A.seqlength(i);

    cout<<"Alignment: "<<A.length()<<" columns of "<<A.n_sequences()<<" sequences         ";
    cout<<"Alphabet: "<<a.name<<"\n";
    cout<<"  sequence lengths: "<<lengths.min()<<"-"<<lengths.max();
    cout<<"      mean = "<<statistics::average(lengths);
    cout<<"      median = "<<statistics::median(lengths)<<"\n";
    cout<<"\n";
    cout<<" ====== w/o indels ======\n";
    cout<<"  const.: "<<n_same<<" ("<<double(n_same)/A.length()*100<<"%)    ";
    cout<<"  non-const.: "<<n_different<<" ("<<double(n_different)/A.length()*100<<"%)    ";
    cout<<"  inform.: "<<n_informative<<" ("<<double(n_informative)/A.length()*100<<"%)\n";
    cout<<"  "<<min_identity(A,false)*100<<"% minimum sequence identity.\n";
    cout<<"\n";
    cout<<" ====== w/  indels ======\n";
    cout<<"  const.: "<<n_same2<<" ("<<double(n_same2)/A.length()*100<<"%)    ";
    cout<<"  non-const.: "<<n_different2<<" ("<<double(n_different2)/A.length()*100<<"%)    ";
    cout<<"  inform.: "<<n_informative2<<" ("<<double(n_informative2)/A.length()*100<<"%)\n";
    cout<<"  "<<min_identity(A,true)*100<<"% minimum sequence identity.\n";
    cout<<"\n";

    //----------- guess # of indels -----------//
    cout<<" ========   gaps ========\n";
    cout<<"  "<<n_with_gaps<<" ("<<double(n_with_gaps)/A.length()*100<<"%) sites contain a gap.\n";
    map<gap,unsigned> gaps = guess_indels(A);
    vector<int> gap_lengths;
    int total_gaps =0;
    int inf_gaps=0;
    int unique = 0;
    int n_ins=0;
    int n_del=0;
    foreach(i,gaps) {
      total_gaps += (*i).second;
      gap_lengths.push_back(i->first.length);
      if (i->first.type == 1)
	n_del++;
      if (i->first.type == 2)
	n_ins++;
      if ((*i).second == 1)
	unique++;
      if (i->second > 1 and (A.n_sequences() - i->second > 1))
	  inf_gaps++;
    }
    valarray<double> gap_lengths2(gap_lengths.size());
    for(int i=0;i<gap_lengths2.size();i++)
      gap_lengths2[i] = gap_lengths[i];
    
    cout<<"  "<<gaps.size()<<" indel groups seem to exist. ("<<total_gaps<<" separate)\n";
    if (total_gaps) {
      cout<<"       unique/inform. = "<<unique<<"/"<<inf_gaps;
      cout<<"       ins./del. = "<<n_ins<<"/"<<n_del<<endl;

      cout<<"  gap lengths: "<<gap_lengths2.min()<<"-"<<gap_lengths2.max();
      cout<<"      mean = "<<statistics::average(gap_lengths2);
      cout<<"      median = "<<statistics::median(gap_lengths2)<<"\n";
    }
    cout<<endl;


    //------------ Get Tree Lengths ------------//
    if (args.count("tree")) {
      cout<<"  tree length = "<<n_mutations(A,T)<<"\n";
      if (const Triplets* Tr = dynamic_cast<const Triplets*>(&a))
	cout<<"  tree length (nuc) = "<<n_mutations(A,T,nucleotide_cost_matrix(*Tr))<<"\n";
      if (const Codons* C = dynamic_cast<const Codons*>(&a))
	cout<<"  tree length (aa)  = "<<n_mutations(A,T,amino_acid_cost_matrix(*C))<<"\n";
    }

    if (dynamic_cast<const Nucleotides*>(&a)) 
    {
      vector<sequence> sequences = A.convert_to_sequences();

      vector<int> found(3,0);
      add(found, find_triplet( sequences , "TAA" ) );
      add(found, find_triplet( sequences , "TGA" ) );
      add(found, find_triplet( sequences , "TAG" ) );

      cout<<"Stop Codons:  "<<join(found,'/')<<"\n";
    }

    valarray<double> counts = letter_counts(A);
    valarray<double> frequencies = A.get_alphabet().get_frequencies_from_counts(counts,A.n_sequences()/2);

    cout<<"\nFreqencies:   ";
    for(int i=0;i<a.size();i++)
      cout<<a.lookup(i)<<"="<<frequencies[i]*100<<"%  ";
    cout<<"\n";

    int classes = letter_classes(A);
    int wildcards = letter_count(A,alphabet::not_gap);
    int total = classes + wildcards + (int)counts.sum();
    cout<<"  Classes:  "<<classes<<" ["<<double(classes)/total*100<<"%]      ";
    cout<<"  Wildcards: "<<wildcards<<" ["<<double(wildcards)/total*100<<"%]\n";
  }
  catch (std::exception& e) {
    cerr<<"alignment-info: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}

