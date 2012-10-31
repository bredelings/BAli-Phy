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

#include <string>
#include <valarray>
#include "myexception.H"
#include "alignment/alignment.H"
#include "sequence/sequence-format.H"
#include "tree/sequencetree.H"
#include "setup.H"
#include "alignment/alignment-util.H"
#include "util.H"
#include "parsimony.H"
#include "tree-dist.H"
#include "io.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;

using boost::dynamic_bitset;

using po::variables_map;
using std::string;
using std::cout;
using std::cerr;
using std::endl;
using std::vector;
using std::valarray;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("align", value<string>()->default_value("-"),"file with alignment to convert (default STDIN)")
    ("tree",value<string>(),"file with initial tree")
    ("alphabet",value<string>(),"specify the alphabet: DNA, RNA, Amino-Acids, Triplets, or Codons")
    ("groups", value<string>(),"file with taxon groups")
    ("split",value<vector<string> >()->composing(),"a split to consider")
    ("ignore-rate-change","ignore rate changes")
    ("require-conservation","Highlight conserved regions")
    ("require-change","Highlight regions if conservation changes")
    ("require-all-different","Taxa differences are interesting of there is no letter overlap")
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
    std::cout<<"Usage: alignment-find conserved <alignment-file> [OPTIONS]\n";
    std::cout<<"Convert the alignment to FASTA or PHYLIP.\n\n";
    std::cout<<all<<"\n";
    exit(0);
  }

  return args;
}

struct sequence_group
{
  string name;
  vector<int> taxa;
};


vector<sequence_group> load_groups(const alignment& A,const string& filename)
{
  vector<string> seq_names = sequence_names(A);
  vector<sequence_group> groups;

  checked_ifstream file(filename);
  string line;
  for(int g=1;getline(file,line);g++)
  {
    if (not groups.size() or groups.back().taxa.size())
      groups.push_back(sequence_group());

    vector<string> names = split(line,' ');

    sequence_group& current_group = groups.back();

    string n = names[0];
    if (n[n.size()-1] != ':')
      throw myexception()<<"Group "<<g<<": Group doesn't start with a name (ending in a ':')";
    n = n.substr(0,n.size()-1);
    names.erase(names.begin());

    current_group.name = n;

    for(int i=0;i<names.size();i++)
      if (names[i].size())
      {
	int j = find_index(seq_names, names[i]);
	if (j == -1)
	  throw myexception()<<"Group "<<g<<": I cant find taxon '"<<names[i]<<"' in the alignment!";
	current_group.taxa.push_back(j);
      }
  }

  return groups;
}


bool all_same_or(const vector<int>& v,int c)
{
  int value = c;
  for(int i=0;i<v.size() and value == c;i++)
    value = v[i];

  bool same = true;
  for(int i=0;i<v.size();i++)
    if (v[i] != value and v[i] != c)
      same = false;
  return same;
}

bool all_same(const vector<int>& v)
{
  bool same = true;
  for(int i=1;i<v.size();i++)
    if (v[i] != v[0])
      same = false;
  return same;
}

int most_common(const vector<int>& v)
{
  int half = v.size()/2;

  vector<int> counted(v.size(),0);

  vector<int> values;
  vector<int> counts;

  for(int i=0;i<v.size();i++) 
  {
    if (not counted[i]) 
    {
      int count = 0;
      for(int j=i;j<v.size();j++) {
	if (v[j] == v[i]) {
	  counted[j] = 1;
	  count++;
	}
      }

      if (count > half)
	return v[i];

      values.push_back(v[i]);
      counts.push_back(count);
    }
  }

  int m = argmax(counts);
  return values[m];
}

int number_of(const vector<int>& v,int n)
{
  int count = 0;
  for(int i=0;i<v.size();i++)
    if (v[i] == n)
      count++;
  return count;
}

bool all_different(const alignment& A,int c, const vector<sequence_group>& groups, int g1, int g2)
{
  for(int i=0;i<groups[g1].taxa.size();i++) 
  {
    int t1 = groups[g1].taxa[i];
    int l1 = A(c,t1);
    for(int j=0;j<groups[g2].taxa.size();j++) {
      int t2 = groups[g2].taxa[j];
      int l2 = A(c,t2);
      if (l1 == l2) return false;
    }
  }
  return true;
}


int main(int argc,char* argv[])
{ 
  try {
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
    
    //------- Load groups and find branches -------//
    vector<sequence_group> groups;
    if (args.count("groups")) 
      groups = load_groups(A,args["groups"].as<string>());

    for(int i=0;i<groups.size();i++) {
      cerr<<groups[i].name<<": ";
      for(int j=0;j<groups[i].taxa.size();j++)
	cerr<<A.seq(groups[i].taxa[j]).name<<" ";
      cerr<<endl;
    }

    vector<int> group_branches;
    if (args.count("tree"))
    {
      for(int i=0;i<groups.size();i++)
      {
	dynamic_bitset<> p(T.n_leaves());
	for(int j=0;j<groups[i].taxa.size();j++)
	  p[groups[i].taxa[j]] = true;

	int found = -1;
	for(int b=0;b<2*T.n_branches() and found == -1;b++)
	  if (p == branch_partition(T,b))
	    found = b;
	if (found == -1)
	  throw myexception()<<"I can't find group "<<i+1<<" on the tree!";
	
	group_branches.push_back(found);
      }
    }

    vector<string> group_names;
    for(int i=0;i<groups.size();i++)
      group_names.push_back(groups[i].name);

    vector<Partition> splits;
    if (args.count("split")) 
    {
      vector<string> split = args["split"].as<vector<string> >();
      for(int i=0;i<split.size();i++) 
	splits.push_back(Partition(group_names,split[i]));
    }

    //-------------------------------------------//

    Matrix C(A.length(),A.n_sequences()+1);
    for(int i=0;i<C.size1();i++)
      for(int j=0;j<C.size2();j++)
	C(i,j) = 0;

    // yes but, how much more conservation THAN EXPECTED do we see?

    for(int c=0;c<C.size1();c++) 
    {
      vector<bool> interesting(groups.size(), true);

      //-------------------------------------------------------//
      vector<int> leaf_letters( T.n_leaves() );
      for(int j=0;j<leaf_letters.size();j++)
	leaf_letters[j] = A(c,j);
      vector<vector<int> > node_letters = get_all_parsimony_letters(a,leaf_letters,T,unit_cost_matrix(a));

      vector<vector<int> > initial_value(groups.size());
      for(int g=0;g<groups.size();g++) 
      {
	int n = T.directed_branch(group_branches[g]).target();
	initial_value[g] = node_letters[n];
      }

      //------------ find 'group conserved at' values ----------//
      vector<int> value(groups.size(),alphabet::gap);

      for(int g=0;g<groups.size();g++) 
      {
	vector<int> temp;
	for(int i=0;i<groups[g].taxa.size();i++)
	  temp.push_back(A(c,groups[g].taxa[i]));

	int best = most_common(temp);
	int count = number_of(temp,best);
	
	if (count >= groups[g].taxa.size()-1 and count >=3 and count> groups[g].taxa.size()/2)
	  value[g] = best;
      }

      //-------- Determine whether column is interesting --------//
      if (args.count("require-all-different"))
      {
	if (args.count("split"))
	{
	  vector<bool> in_changed_split(groups.size(),false);
	  for(int i=0;i<splits.size();i++) 
	  {
	    bool some_different = false;
	    for(int g1=0;g1<groups.size();g1++)
	      for(int g2=0;g2<groups.size();g2++)
		if (splits[i].group1[g1] and splits[i].group2[g2]) {
		  if (all_different(A,c,groups,g1,g2))
		    some_different = true;
		}
	  
	    bool no_same = true;
	    for(int g1=0;g1<groups.size();g1++)
	      for(int g2=0;g2<groups.size();g2++)
		if (splits[i].group1[g1] and splits[i].group2[g2])
		  if (not all_different(A,c,groups,g1,g2))
		    no_same = false;

	    if (some_different and no_same) 
	      for(int g=0;g<groups.size();g++)
		if (splits[i].group1[g] or splits[i].group2[g])
		  in_changed_split[g] = true;
	  }

	  for(int g=0;g<groups.size();g++)
	    interesting[g] = interesting[g] and in_changed_split[g];
	}

	else {
	  bool different = false;
	  for(int g1=0;g1<groups.size();g1++)
	    for(int g2=0;g2<groups.size();g2++)
	      if (all_different(A,c,groups,g1,g2))
		different = true;

	  if (not different)
	    for(int g=0;g<groups.size();g++) 
	      interesting[g] = false;
	}
      }
    
      if (args.count("require-change"))
      {
	if (args.count("split"))
	{
	  vector<bool> in_changed_split(groups.size(),false);
	  for(int i=0;i<splits.size();i++) 
	  {
	    bool some_different = false;
	    for(int g1=0;g1<groups.size();g1++)
	      for(int g2=0;g2<groups.size();g2++)
		if (splits[i].group1[g1] and splits[i].group2[g2]) {
		  if (value[g1] != value[g2])
		    if (not args.count("ignore-rate-change") or 
			(value[g1] != alphabet::gap and value[g2] != alphabet::gap))
		      some_different = true;
		}
	  
	    bool no_same = true;
	    for(int g1=0;g1<groups.size();g1++)
	      for(int g2=0;g2<groups.size();g2++)
		if (splits[i].group1[g1] and splits[i].group2[g2])
		  if (value[g1] == value[g2])
		    no_same = false;

	    // This is Option #1 
	    //  - some conserved differences but no conserved similarities
	    // Also consider Option #2
	    //  - a change in both LETTER and CONSERVATION on at least one of the
	    //    two branches leading from the duplication.
	    if (some_different and no_same) 
	      for(int g=0;g<groups.size();g++)
		if (splits[i].group1[g] or splits[i].group2[g])
		  in_changed_split[g] = true;
	  }

	  for(int g=0;g<groups.size();g++)
	    interesting[g] = interesting[g] and in_changed_split[g];


	}

	else {
	  if (args.count("ignore-rate-change")) 
	  {
	    for(int g=0;g<groups.size();g++) 
	      interesting[g] = interesting[g] and not all_same_or(value,alphabet::gap);
	  }
	  else 
	  {
	    for(int g=0;g<groups.size();g++)
		interesting[g] = interesting[g] and not all_same(value);
	  }
	}
      }
    
      // A group is only interesting if its conserved
      if (args.count("require-conservation"))
	for(int g=0;g<groups.size();g++)
	  interesting[g] = interesting[g] and (value[g] != alphabet::gap);

      // A group is only interesting if it is in one of the splits
      if (args.count("split"))
	for(int g=0;g<groups.size();g++) {
	  bool found = false;
	  for(int i=0;i<splits.size() and not found;i++) 
	    if (splits[i].group1[g] or splits[i].group2[g])
	      found = true;
	  interesting[g] = interesting[g] and found;
	}

      //------------ print 'group conserved at' values ----------//
      cerr<<c+1<<"   ";
      for(int i=0;i<value.size();i++) 
	cerr<<a.lookup(value[i])<<" ";
      cerr<<"     ";

      for(int g=0;g<groups.size();g++) 
	if (interesting[g])
	  cerr<<"1 ";
	else
	  cerr<<"0 ";
      cerr<<endl;

      //------------- print parsimony initial values --------------//
      cerr<<"     ";
      for(int i=0;i<initial_value.size();i++) {
	for(int j=0;j<initial_value[i].size();j++)
	  cerr<<a.lookup(initial_value[i][j]);
	cerr<<" "; 
      }     
      cerr<<"    "<<n_mutations(a,leaf_letters,T,unit_cost_matrix(a))<<endl;
      cerr<<endl;



      //--------------------- Set highlighting ---------------------//
      // Interesting groups -> 1.0
      for(int g=0;g<groups.size();g++)
	if (interesting[g])
	  for(int i=0;i<groups[g].taxa.size();i++)
	    C(c,groups[g].taxa[i]) = 1.0;

      // Set conserved groups -> 0.5
      for(int g=0;g<groups.size();g++)
	if (value[g] != alphabet::gap)
	  for(int i=0;i<groups[g].taxa.size();i++)
	    C(c,groups[g].taxa[i]) = std::max(0.5, C(c,groups[g].taxa[i]));
    }


    cout<<join(sequence_names(A),' ')<<endl;
    for(int i=0;i<C.size1();i++) {
      vector<double> temp;
      for(int j=0;j<C.size2();j++)
	temp.push_back(C(i,j));
      cout<<join(temp,' ')<<endl;
    }
    
  }
  catch (std::exception& e) {
    std::cerr<<"alignment-find-conserved: Error! "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}
