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
#include "alignment.H"
#include "alignment-util.H"
#include "util.H"
#include "sequence/sequence-format.H"
#include <boost/program_options.hpp>
#include "io.H"
#include "findroot.H"

using namespace sequence_format;
namespace po = boost::program_options;
using po::variables_map;

using std::ifstream;
using std::istream;
using std::vector;
using std::string;

using std::cin;
using std::cout;
using std::cerr;
using std::endl;

bool all_same_length(const vector<sequence>& s)
{
  for(int i=1;i<s.size();i++)
    if (s[i].size() != s[0].size())
      return false;
  return true;
}


//FIXME - make this handle un-aligned gaps...
// diagnose sequences which are not a multiple of 3
// look for reading frames?  start codons?
// translate just the sequences before translating
// the ALIGNMENT of the sequences to print out

vector<int> get_mapping(const vector<sequence>& S1, const vector<sequence>& S2)
{

  vector<string> names1(S1.size());
  for(int i=0;i<S1.size();i++)
    names1[i] = S1[i].name;

  vector<string> names2(S2.size());
  for(int i=0;i<S2.size();i++)
    names2[i] = S2[i].name;

  vector<int> mapping;

  try {
    mapping = compute_mapping(names1,names2);
  }
  catch (const bad_mapping<string>& b) {
    bad_mapping<string> b2 = b;
    b2.clear();
    if (b.from == 0)
      b2<<"Couldn't find sequence '"<<b2.missing<<"'.";
    else
      b2<<"Extra sequence '"<<b2.missing<<"' not contained in earlier alignments.";
    throw b2;    
  }
  return mapping;
}

sequence strip_gaps(const sequence& s1, const vector<char>& missing)
{
  sequence s2 = s1;
  int L=0;
  for(int c=0;c<s2.size();c++)
    if (not includes(missing,s2[c]))
      s2[L++] = s2[c];
  s2.resize(L);
  return s2;
}

vector<sequence> strip_gaps(const vector<sequence>& S1, const vector<char>& missing)
{
  vector<sequence> S2 = S1;
  for(int i=0;i<S2.size();i++)
    S2[i] = strip_gaps(S2[i], missing);
  return S2;
}

vector<sequence> concatenate(const vector<sequence>& S1, const vector<sequence>& S2)
{
  if (not S1.size())
    return S2;

  assert(S1.size() == S2.size());

  vector<sequence> S = S1;
  for(int i=0;i<S1.size();i++) {
    
    vector<int> mapping = get_mapping(S1,S2);

    (string&)S[i] = S1[i] + S2[mapping[i]];
  }

  return S;
}


vector<sequence> select(const vector<sequence>& s,const vector<int>& columns)
{
  assert(all_same_length(s));

  //------- Start with empty sequences --------//
  vector<sequence> S = s;
  for(int i=0;i<s.size();i++)
    S[i].string::operator=("");

  //------- Append columns to sequences -------//
  for(int i=0;i<columns.size();i++)
    for(int j=0;j<s.size();j++)
      S[j] += s[j][columns[i]];

  return S;
}

vector<sequence> select(const vector<sequence>& s,const string& range)
{
  assert(all_same_length(s));

  int L = s[0].size();

  vector<int> columns = parse_multi_range(range, L);

  return select(s,columns);
}

void check_all_same_length(const vector<sequence>& s, const string& reason)
{
  for(int i=1;i<s.size();i++)
    if (s[i].size() != s[0].size())
    {
      myexception e;
      e<<"All sequences in an alignment must have the same length "<<reason<<"\n";
      e<<"Alignment file: sequence #"<<i+1<<" '"<<s[i].name<<"' has length "<<s[i].size()<<" != "<<s[0].size()<<"\n";
      e<<"Consider option -p to pad them to the same length.";
      throw e;
    }
}

vector<sequence> remove_empty_columns(const vector<sequence>& s,const vector<char>& missing)
{
  check_all_same_length(s, "in order to remove empty columns.");

  // cache length of longest sequences
  int L = s[0].size();

  // find non-empty columns
  vector<int> columns;
  for(int c=0;c<L;c++)
  {
    bool empty = true;
    for(int j=0;j<s.size() and empty;j++)
      if ((c < s[j].size()) and (not includes(missing,s[j][c])))
	empty=false;

    if (not empty)
      columns.push_back(c);
  }

  // select the non-empty columns
  return select(s,columns);
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description invisible("Invisible options");
  invisible.add_options()
    ("file", value<vector<string> >(),"Alignment files")
    ;

  options_description visible("All options");
  visible.add_options()
    ("help,h", "Produce help message")
    ("output", value<string>()->default_value("fasta"),"Which output format: fasta or phylip?")
    ("columns,c", value<string>(),"Ranges of columns to keep, like: 1-10,30-")
    ("taxa,t", value<string>(),"Taxa to keep, comma-separated")
    ("pad,p", "Add gaps to make sequence lengths identical")
    ("reverse,r", "Reverse the sequences")
    ("erase-empty-columns,e","Remove columns with no characters (all gaps).")
    ("missing",value<string>()->default_value("-?"),"What letters are not characters (e.g. gaps)?")
    ("strip-gaps","Remove all non-character letters from sequences.")
    ("reorder-by-tree",value<string>(),"Reorder the sequences given a tree")
    ("use-root","use the root specified in the tree file to reorder")
    ("reorder-by-alignment",value<string>(),"Reorder the sequences following an alignment")
    ("align-by-amino",value<string>(),"Arrange nucleotides into codon alignment")
    ;

  options_description all("All options");
  all.add(visible).add(invisible);

  // positional options
  positional_options_description p;
  p.add("file", -1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);    

  bool error = false;

  if (args.count("help") or error) {
    cout<<"Usage: alignment-cat [file1] {[file2] ...} \n";
    cout<<"Concatenate several alignments (with the same sequence names) end-to-end.\n\n";
    cout<<visible<<"\n";
    cout<<" Examples:\n\n";
    cout<<"  To select columns from an alignment:\n";
    cout<<"    % alignment-cat -c1-10,50-100,600- filename.fasta > result.fasta\n";
    cout<<"    % alignment-cat -c5-250/3 filename.fasta > first_codon_position.fasta\n";
    cout<<"    % alignment-cat -c6-250/3 filename.fasta > second_codon_position.fasta\n\n";

    cout<<"  To concatenate two or more alignments:\n";
    cout<<"    % alignment-cat filename1.fasta filename2.fasta > all.fasta\n";
    
    exit(0);
  }

  return args;
}

void pad_to_same_length(vector<sequence>& s)
{
  // find total alignment length
  vector<unsigned> L;
  for(int i=0;i<s.size();i++)
    L.push_back(s[i].size());
  unsigned AL = max(L);

  // pad sequences if they are less than this length
  for(int i=0;i<s.size();i++)
    if (L[i] < AL)
      (string&)s[i] = (string&)s[i] + string(AL-L[i],'-');
}

vector<sequence> load_file(istream& file,bool pad)
{
  vector<sequence> s = sequence_format::read_guess(file);
  if (s.size() == 0)
    throw myexception()<<"Alignment file didn't contain any sequences!";

  if (pad)
    pad_to_same_length(s);

  return s;
}

vector<sequence> load_file(const string& filename,bool pad)
{
  checked_ifstream file(filename,"alignment file");

  vector<sequence> s = sequence_format::read_guess(file);
  if (s.size() == 0)
    throw myexception()<<"Alignment file '"<<filename<<"' didn't contain any sequences!";

  if (pad)
    pad_to_same_length(s);

  return s;
}

vector<sequence> select_taxa(const vector<sequence>& S,const vector<string>& names)
{
  vector<sequence> S2;

  vector<int> mapping(names.size(),-1);
  for(int i=0;i<names.size();i++) {
    for(int j=0;j<S.size() and mapping[i] == -1;j++)
      if (names[i] == S[j].name)
	mapping[i] = j;
  }

  bool ok=true;
  myexception error;
  for(int i=0;i<mapping.size();i++)
    if (mapping[i] == -1) {
      if (not ok)
	error<<"\n";
      error<<"Alignment contains no sequence named '"<<names[i]<<"'";
      ok = false;
    }

  if (not ok) throw error;

  for(int i=0;i<mapping.size();i++)
    S2.push_back(S[mapping[i]]);

  return S2;
}


struct branch_order {
  const Tree& T;

  bool operator()(int b1,int b2) const {
    if (subtree_height(T,b1) < subtree_height(T,b2))
      return true;
    if (subtree_height(T,b1) > subtree_height(T,b2))
      return false;
    return T.partition(b1).find_first() < T.partition(b2).find_first();
  }

  branch_order(const Tree& T_): T(T_) {}
};


/// get an ordered list of leaves under T[n]
vector<int> get_leaf_order(const Tree& T,int b) 
{
  vector<int> mapping;

  if (T.directed_branch(b).target().is_leaf_node()) {
    mapping.push_back( T.directed_branch(b).target() );
    return mapping;
  }

  // get sorted list of branches
  vector<const_branchview> branches;
  append(T.directed_branch(b).branches_after(),branches);
  std::sort(branches.begin(),branches.end(),branch_order(T));

  // accumulate results
  for(int i=0;i<branches.size();i++) {
    vector<int> sub_mapping = get_leaf_order(T,branches[i]);
    mapping.insert(mapping.end(),sub_mapping.begin(),sub_mapping.end());
  }

  return mapping;
}

/// get an ordered list of the leaves of T
vector<int> get_leaf_order(const RootedTree& RT) 
{
  vector<int> mapping;

  // get sorted list of branches
  vector<const_branchview> branches;
  append(RT.root().branches_out(),branches);
  std::sort(branches.begin(),branches.end(),branch_order(RT));

  // accumulate results
  for(int i=0;i<branches.size();i++) {
    vector<int> sub_mapping = get_leaf_order(RT,branches[i]);
    mapping.insert(mapping.end(),sub_mapping.begin(),sub_mapping.end());
  }

  assert(mapping.size() == RT.n_leaves());
  return mapping;
}

vector<string> get_names_from_tree(RootedSequenceTree T, bool use_root)
{
  //------- Re-root the tree appropriately  --------//
  if (not use_root)
  {
    int rootb=-1;
    double rootd = -1;
    find_root(T,rootb,rootd);
    if (log_verbose) {
      std::cerr<<"alignment-reorder: root branch = "<<rootb<<std::endl;
      std::cerr<<"alignment-reorder: x = "<<rootd<<std::endl;
      for(int i=0;i<T.n_leaves();i++)
	std::cerr<<"alignment-reorder: "<<T.get_label(i)<<"  "<<rootdistance(T,i,rootb,rootd)<<std::endl;
    }
    
    T = add_root((SequenceTree)T,rootb);  // we don't care about the lengths anymore
  }
  
  //----- Standardize initial leaf order by alphabetical order of names ----//
  vector<string> names = T.get_leaf_labels();
  
  std::sort(names.begin(),names.end());
  
  vector<int> mapping1 = compute_mapping(T.get_leaf_labels(),names);
  
  T.standardize(mapping1);
  
  //-------- Compute the mapping  -------//
  vector<int> order = get_leaf_order(T);

  //-------- Compute the ordered list of names ------//
  names.clear();
  for(int l: order)
    names.push_back(T.get_label(l));

  return names;
}

vector<string> get_names(const vector<sequence>& S)
{
  vector<string> names;
  for(const auto& s: S)
    names.push_back(s.name);
  return names;
}

vector<sequence> align_by_amino_acids(const vector<sequence>& S1, const string& filename, const vector<char>& missing)
{
  // 1. Check that codon sequences are a multiple of 3 nucleotides.
  for(const auto& S: S1)
    if (S.size() % 3)
      throw myexception()<<"Sequence '"<<S.name<<"' has length "<<S.size()<<" which is not a multiple of 3!";

  // 2. Load the amino acid sequence alignment, and pad it.
  vector<sequence> aminos = load_file(filename,true);

  // 3. Check that there are the same number of amino and nucleotide sequence.
  if (S1.size() != aminos.size())
    throw myexception()<<"Amino acid alignment has "<<aminos.size()<<" sequences, but there are "<<S1.size()<<" nucleotide sequences.";

  // 4. Rearrange nucleotide sequences in the same order as the amino acid sequences.
  vector<sequence> S2 = select_taxa(S1, get_names(aminos));

  for(int i=0;i<S2.size();i++)
  {
    sequence nuc = strip_gaps(S2[i],missing);
    const sequence& aa = aminos[i];
    assert(nuc.name == aa.name);
    int aa_length = strip_gaps(aa,missing).size();
    if (nuc.size()/3 != aa_length)
      throw myexception()<<"Sequence '"<<nuc.name<<"' has "<<nuc.size()<<" nucleotides - cannot match 3*"<<aa_length<<"="<<3*aa_length<<" amino acids.";

    S2[i].resize(3*aa.size());
    for(int j=0,k=0,l=0;j<aa.size();j++)
    {
      if (includes(missing,aa[j]))
      {
	S2[i][k++] = aa[j];
	S2[i][k++] = aa[j];
	S2[i][k++] = aa[j];
      }
      else
      {
	S2[i][k++] = nuc[l++];
	S2[i][k++] = nuc[l++];
	S2[i][k++] = nuc[l++];
      }
    }
  }

  return S2;
}


int main(int argc,char* argv[]) 
{ 

  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    bool pad = (args.count("pad")>0);

    vector<string> names;
    if (args.count("taxa"))
      names = split(args["taxa"].as<string>(),',');
    else if (args.count("reorder-by-tree"))
    {
      RootedSequenceTree RT;
      RT.read(args["reorder-by-tree"].as<string>());
      bool use_root = args.count("use-root");
      names = get_names_from_tree(RT, use_root);
    }
    else if (args.count("reorder-by-alignment"))
    {
      vector<sequence> sequences = load_file(args["reorder-by-alignment"].as<string>(), false);
      for(const auto& s: sequences)
	names.push_back(s.name);
    }

    //------- Determine filenames --------//
    vector<string> filenames;
    if (args.count("file"))
      filenames = args["file"].as<vector<string> >();
    else
      filenames = {"-"};

    //------- Try to load sequences --------//
    vector<sequence> S;
    vector<sequence> s_in;
    bool cin_read = false;
    for(int i=0;i<filenames.size();i++) 
    {
      // Read the sequences
      vector<sequence> s;
      if (filenames[i] == "-")
      {
	if (not cin_read) {
	  s_in = load_file(cin,pad);
	  cin_read = true;
	}
	s = s_in;
      }
      else
	s = load_file(filenames[i],pad);

      // Add the sequences to what we have so far
      try {
	if (filenames.size() > 1)
	  check_all_same_length(s, "in order to concatenate two or more alignments.");
	
	if (names.size())
	  s = select_taxa(s,names);
	S = concatenate(S,s);
      }
      catch (std::exception& e) {
	throw myexception()<<"File '"<<filenames[i]<<"': "<<e.what();
      }
    }

    // determine which chars are not characters
    vector<char> missing;
    {
      string missing2 = args["missing"].as<string>();
      for(int i=0;i<missing2.size();i++)
	missing.push_back(missing2[i]);
    }

    if (args.count("align-by-amino"))
    {
      const string& filename = args["align-by-amino"].as<string>();
      S = align_by_amino_acids(S,filename,missing);
    }
      
    if (args.count("columns"))
      S = select(S,args["columns"].as<string>());
    
    if (args.count("erase-empty-columns")) 
      S = remove_empty_columns(S,missing);

    if (args.count("strip-gaps"))
      S = strip_gaps(S, missing);

    // Reverse each sequence, if asked.
    if (args.count("reverse"))
      for(sequence& s: S)
	std::reverse(s.begin(), s.end());

    if (args["output"].as<string>() == "phylip")
      write_phylip(cout,S);
    else if (args["output"].as<string>() == "fasta")
      write_fasta(cout,S);
    else
      throw myexception()<<"I don't recognize requested format '"<<args["output"].as<string>()<<"'";
  }
  catch (std::exception& e) {
    std::cerr<<"alignment-cat: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
