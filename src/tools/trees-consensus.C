/*
   Copyright (C) 2004-2010 Benjamin Redelings

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
#include <algorithm>
#include <string>
#include <vector>
#include <list>
#include <set>
#include <map>
#include <cmath>
#include <fstream>
#include <sstream>
#include <map>
#include <set>
#include <list>

#include "tree/sequencetree.H"
#include "util.H"
#include "statistics.H"
#include "bootstrap.H"
#include "tree-dist.H"
#include "consensus-tree.H"
#include "mctree.H"
#include "rng.H"

#include <boost/program_options.hpp>

/// FIXME - construct a full tree by extending the moveable
///  a) randomly
///  b) according to posterior probabilities (?greedy?)

// TODO: 
// 1. Construct a set of @n trees that contain splits down to a frequency of 0.1
// 2. Consider seeding the branches-to-unplug with splits of lower frequency than 0.5?
//  2a. But, perhaps we should try the 0.5 branches FIRST.
//  2b. Or, perhaps we should only try branches consistent with the high-prob branches.
//      In short, construct a greedy tree, and use that...
// 3. Consider trying to construct disjoint sets of trees with the most resolved branches total.
/// 3a. This is better than trying to consider the highest MAP trees, since those trees may all
///     be very similar to each other: imagine of one branch has 1/3 on each NNI possibility...
/// 3b. This approach would capture correlations and anti-correlations, as well...


namespace po = boost::program_options;
using po::variables_map;

using boost::dynamic_bitset;

using namespace std;

using std::cout;
using std::cerr;
using std::endl;

using statistics::odds;

// What if everything in 'split' is true?
// What if everything in 'split' is true, but 1 taxa?
//  These are true by definition...

double getsum(const valarray<double>& v) {
  return v.sum();
}


unsigned changes(const valarray<bool>& sample,bool value) 
{
  unsigned count=0;
  for(int i=0;i<sample.size()-1;i++) {
    if (sample[i] == value and sample[i+1] != value)
      count++;
  }
  return count;
}


struct consensus_level_order
{
  bool operator()(const pair<double,string>& p1, const pair<double,string>& p2) const
  {
    return p1.first < p2.first;
  }
};

// FIXME - if n is small, first level may be ~ 0.57 -- good or bad?
vector<pair<double, string> > get_consensus_levels(const string& s) 
{
  // split into comma-separate components
  vector<string> levels;
  if (s.size())
    levels = split(s,',');

  vector<pair<double, string> > parsed_levels;

  // Split each component level>file into level (a double) and file (a string)
  for(int i=0;i<levels.size();i++) 
  {
    if (not levels[i].size()) continue;
    vector<string> parts = split(levels[i],':');
    if (parts.size() > 2) throw myexception()<<"Consensus level '"<<levels[i]<<"' has too many files.";
    if (parts.size() == 1) parts.push_back("-");

    assert(parts.size() == 2);
    double L = convertTo<double>(parts[0]);
    string filename = parts[1];

    if (L<0 or L > 1)
      throw myexception()<<"Ignoring bad consensus level '"<<L<<"'";

    parsed_levels.push_back( pair<double,string>(L,filename) );
  }

  std::sort(parsed_levels.begin(), parsed_levels.end(), consensus_level_order());

  return parsed_levels;
}

/// Add partitions in 'delta' if none of them are IDENTICAL to any partition in 'partitions'
void add_unique_partitions(vector<Partition>& partitions,const vector<Partition>& delta) {
  for(int j=0;j<delta.size();j++)
    if (not includes(partitions,delta[j]))
      partitions.push_back(delta[j]);
}

double odds_ratio(const vector<pair<Partition,unsigned> >& partitions, const pair<Partition,unsigned>& delta,
		  unsigned N, unsigned pseudocount=0)
{
  unsigned n1 = 0;
  for(int i=0;i<partitions.size();i++)
    if (implies(partitions[i].first, delta.first))
      n1 = std::max(n1, partitions[i].second);

  unsigned n2 = delta.second;

  return statistics::odds_ratio(n1,n2,N,pseudocount);
}

double odds_ratio(vector<pair<Partition,unsigned> > partitions, int i,
		  unsigned N, unsigned pseudocount=0)
{
  pair<Partition,unsigned> delta = partitions[i];
  partitions.erase(partitions.begin()+i);
  return odds_ratio(partitions,delta,N,pseudocount);
}



bool covers(const pair<Partition,unsigned>& p1,const pair<Partition,unsigned>& p2, 
	    double ratio,unsigned N,unsigned pseudocount)
{
  if (not implies(p1.first, p2.first))
    return false;

  int n1 = p1.second;
  int n2 = p2.second;

  return (statistics::odds_ratio(n1,n2,N,pseudocount) < ratio);
}

bool covers(const vector<pair<Partition,unsigned> >& partitions, int exclude,
	    const pair<Partition,unsigned>& delta,double ratio, unsigned N, unsigned pseudocount=0)
{
  for(int i=0;i<partitions.size();i++)
    if (i != exclude and covers(partitions[i],delta,ratio,N,pseudocount))
      return true;
  return false;
}

bool merge(vector<pair<Partition,unsigned> >& partitions,
	   const pair<Partition,unsigned>& delta,
	   double ratio,
	   unsigned N,
	   unsigned pseudocount=0)
{
  for(int i=partitions.size()-1;i>=0;i--) 
    if (covers(delta,partitions[i],ratio,N,pseudocount))
      partitions.erase(partitions.begin()+i);
    else if (covers(partitions[i],delta,ratio,N,pseudocount))
      return false;

  partitions.push_back(delta);

  return true;
}

vector<pair<Partition,unsigned> > 
thin(const vector<pair<Partition,unsigned> >& partitions, unsigned N, double ratio)
{
  vector<pair<Partition,unsigned> > thinned;
  for(int i=0;i<partitions.size();i++)
    merge(thinned,partitions[i],ratio,N,5);

  return thinned;
}

vector<Partition> strip(const vector<pair<Partition,unsigned> >& partitions) 
{
  vector<Partition> stripped;
  for(int i=0;i<partitions.size();i++)
    stripped.push_back(partitions[i].first);

  return stripped;
}


/// Merge the lists in 'partition_sets' while removing duplicates
vector<Partition> merge(const vector<vector<Partition> >& partition_sets) 
{
  vector<Partition> partitions;
  for(int i=0;i<partition_sets.size();i++) 
    add_unique_partitions(partitions,partition_sets[i]);
  return partitions;
}

vector< vector<Partition> >
get_sub_partitions(const tree_sample& tree_dist,const vector<double>& levels,int depth=1) 
{
  vector< vector<Partition> > partition_sets(levels.size());
  for(int i=0;i<partition_sets.size();i++)
    partition_sets[i] = get_Ml_sub_partitions(tree_dist,levels[i],depth);

  return partition_sets;
}

vector< vector<Partition> >
get_full_partitions(const tree_sample& tree_dist,const vector<double>& levels) 
{
  vector< vector<Partition> > partition_sets(levels.size());
  for(int i=0;i<partition_sets.size();i++)
    partition_sets[i] = get_Ml_partitions(tree_dist,levels[i]);

  return partition_sets;
}

vector<Partition> 
get_Ml_partitions(const vector<pair<Partition,unsigned> >& sp, unsigned min_count)
{
  vector<Partition> partitions;
  for(int i=0;i<sp.size();i++) {
    if (sp[i].second >= min_count)
      merge_partition(partitions,sp[i].first);
  }

  return partitions;
}


vector<Partition> 
get_Ml_partitions(const vector<pair<Partition,unsigned> >& sp, double l, unsigned total)
{
  unsigned min_count = std::min(1+(unsigned)(total*l),total);

  return get_Ml_partitions(sp,min_count);
}

using std::set;
vector<unsigned> get_Ml_levels(const vector<pair<Partition,unsigned> >& sp,unsigned N,double min_support)
{
  set<unsigned> levels;

  levels.insert((unsigned)(N*min_support+1));
  levels.insert(N);

  for(int i=0;i<sp.size();i++) {
    levels.insert(sp[i].second);
    levels.insert(std::min(sp[i].second+1,N));
  }

  vector<unsigned> levels2(levels.size());
  copy(levels.begin(),levels.end(),levels2.begin());
  return levels2;
}

unsigned
get_partition_count(const vector<pair<Partition,unsigned> >& sp, const Partition& p)
{
  for(int i=0;i<sp.size();i++) {
    if (sp[i].first == p)
      return sp[i].second;
  }

  throw myexception()<<"Can't find partition "<<p;
}



// FIXME - we use all full parition in 'sub'
//   - only do exhaustive search on partial partitions?
//   - divide into connected components?

// This is a greedy search.
// It would be nice to also pick which branches are "least informative".

vector<Partition> Ml_min_Hull(const vector<Partition>& full,const vector<Partition>& sub)
{
  // compute full partitions to keep
  dynamic_bitset<> keep(full.size());
  dynamic_bitset<> covered(sub.size());

  while (covered.count() < covered.size()) 
  {
    // how many UNCOVERED subs does each UNKEPT full branch imply?
    vector<int> covers(full.size(),0);

    for(int i=0;i<sub.size();i++) {
      if (covered[i]) continue;
      for(int j=0;j<full.size();j++)
	if (implies(full[j],sub[i])) {
	  assert(not keep[j]);
	  covers[j]++;
	}
    }

    // only do the argmax if there are args to max over!
    if (not covers.size())
      break;

    // choose full branch to keep
    int best = argmax(covers);    // all KEPT branches cover 0 uncovered branches.

    if (covers[best] == 0) 
      break;

    keep[best] = true;

    // mark its covered sub-branches as covered
    for(int i=0;i<sub.size();i++)
      if (not covered[i] and implies(full[best],sub[i]))
	covered[i] = true;
  }

  // collect the full partitions that have been chosen
  vector<Partition> hull;
  for(int i=0;i<full.size();i++)
    if (keep[i])
      hull.push_back(full[i]);

  return hull;
}

// Is there a way to choose branches that would imply sub-partitions that are
// not in sub because other full branches imply them?  

// That might be best...

vector<Partition> Ml_max_Hull(const vector<Partition>& full,const vector<Partition>& sub)
{
  vector<Partition> hull;

  for(int i=0;i<full.size();i++) 
  {
    bool ok = false;
    for(int j=0;j<sub.size() and not ok;j++)
      if (implies(full[i],sub[j])) 
	ok = true;
   
    if (ok) hull.push_back(full[i]);
  }

  return hull;
}


variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description invisible("Invisible options");
  invisible.add_options()
    ("files",value<vector<string> >()->composing(),"tree samples to examine")
    ("seed", value<unsigned long>(),"Random seed")
    ;

  options_description input("Input options");
  input.add_options()
    ("help,h", "Produce help message.")
    ("skip,s",value<string>()->default_value("10%"),"Number of trees to skip.")
    ("max,m",value<int>(),"Maximum number of trees to read.")
    ("sub-sample,x",value<int>()->default_value(1),"Factor by which to sub-sample.")
    ("ignore", value<string>(),"Comma-separated list of taxa to ignore.")
    ;
  
  options_description reporting("Reporting options");
  reporting.add_options()
    ("map-trees",value<int>()->default_value(1),"Only report the top <arg> trees per file.")
    ("map-tree",value<string>(),"Write out the map tree to file <arg>.")
    ("min-support",value<double>()->default_value(0.25),"Minimum threshold PP for splits.")
    ("report",value<string>(),"Write supported partitions to file <arg>.")
    ("consensus-PP",value<string>(),"Write out consensus trees+PP.")
    ("consensus",value<string>(),"Write out consensus trees.")
    ("greedy-consensus",value<string>(),"Write out greedy consensus trees.")
    ("extended-consensus-L",value<string>(),"Write out extended consensus trees + lengths.")
    ("extended-consensus",value<string>(),"Write out extended consensus trees.")
    ("support-levels",value<string>(),"Write #branches versus LOD to file <arg>.")
    ("extended-support-levels",value<string>(),"Write #sub-branches versus LOD to file <arg>.")
    ("odds-ratio",value<double>()->default_value(1.5),"Report partial-splits only if removing taxa improves the odds by at least this ratio.")
    ("verbose,v","Output more log messages on stderr.")
    ;
    
  options_description search("Search options");
  search.add_options()
    ("sub-partitions","Search for partial splits.")
    ("depth",value<int>()->default_value(1),"Depth at which to look for partial splits.")
    ("rooting",value<double>()->default_value(0.9,"0.9"),"Threshhold in search for partial splits.")
    ;

  options_description visible("All options");
  visible.add(input).add(reporting).add(search);

  options_description all("All options");
  all.add(visible).add(invisible);

  // positional options
  positional_options_description p;
  p.add("files", -1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: trees-consensus <file> [OPTIONS]\n";
    cout<<"Find consensus trees and supported splits.\n\n";
    cout<<input<<"\n";
    cout<<reporting<<"\n";
    cout<<search<<"\n";
    cout<<"Arguments for consensus trees are level1:filename1[,level2:filename2,...]\n\
  o each level is a minimum PP and should be in the range [0.5, 1.0].\n\
  o drop \"filename\" or specify \":-\" to write to the terminal.\n";
    exit(0);
  }

  if (args.count("verbose")) log_verbose = 1;

  return args;
}

/// A simple ordering operator to sort pair<Partition,unsigned> by the order of the Partition
struct count_more {
  bool operator()(const pair<Partition,unsigned>& p1,const pair<Partition,unsigned>& p2) const {
    return p1.second > p2.second;
  }
};

/// \brief Create a file containing the LOD, #of supported partitions, and PP in 3 tab-separated columsn
///
/// \param args Command-line arguments
/// \param all_partitions The count for each partition
/// \param N The total number of sampled trees
void write_support_level_graph(variables_map& args, const vector<pair<Partition,unsigned> >& all_partitions, unsigned N)
{
  if (!args.count("support-levels")) return;
  const string filename = args["support-levels"].as<string>();

  vector<unsigned> levels = get_Ml_levels(all_partitions, N, 0.5);

  ofstream file(filename.c_str());
  if (!file)
    throw myexception()<<"Couldn't open file '"<<filename<<"' for writing.";
  
  for(int j=0;j<levels.size();j++) 
  {
    double fraction = double(levels[j])/N;
    double LOD = log10(odds(levels[j],N,1));
    
    const vector<Partition> sub = get_Ml_partitions(all_partitions,levels[j]);
    const vector<Partition> full = select(sub,&Partition::full);

    file<<LOD<<"\t"<<count(full,informative)<<"\t"<<fraction<<"\n";
  }
  file.close();
}

/// \brief Create a file containing the LOD, #of supported partial splits, and PP in 3 tab-separated columsn
///
/// Here, the # of partial splits report is reduced to the largest
/// number of compatible partial splits.
///
/// \param args Command-line arguments
/// \param all_partitions The count for each partition
/// \param N The total number of sampled trees
void write_extended_support_level_graph(variables_map& args, const vector<pair<Partition,unsigned> >& all_partitions, 
					tree_sample& tree_dist, unsigned N)
{
  if (!args.count("extended-support-levels")) return;
  const string filename = args["extended-support-levels"].as<string>();

  vector<unsigned> levels = get_Ml_levels(all_partitions, N, 0.5);

  ofstream file(filename.c_str());
  if (!file)
    throw myexception()<<"Couldn't open file '"<<filename<<"' for writing.";
  
  for(int j=0;j<levels.size();j++) 
  {
    double fraction = double(levels[j])/N;
    double LOD = log10(odds(levels[j],N,1));
    
    vector<Partition> sub = get_Ml_partitions(all_partitions,levels[j]);
    add_leaf_partitions(tree_dist.names(), sub);

    // const vector<Partition> full = select(sub,&Partition::full);

    //  vector<Partition> full_hull = Ml_min_Hull(full_skeleton,sub);
    //  vector<Partition> sub_hull = Ml_min_Hull(skeleton,sub);

    file<<LOD<<"\t"<<count(get_moveable_tree(sub),informative)<<"\t"<<fraction<<"\n";
  }
  file.close();
}

/// \brief Create a new map<dynamic_bitset<>,count_and_length> that only contains records with a high enough count
///
/// \param all The full list of splits
/// \param count The required count
///
map<dynamic_bitset<>, count_and_length> select_splits(const map<dynamic_bitset<>,count_and_length>& all, int count)
{
  typedef map<dynamic_bitset<>,count_and_length> container_t;

  container_t some;

  for(container_t::const_iterator i = all.begin(); i != all.end(); i++)
  {
    if (i->second.count >= count)
      some.insert(*i);
  }

  return some;
}

void get_branch_lengths_and_PP(SequenceTree& T,vector<double>& PP,
			       const map<dynamic_bitset<>,count_and_length>& partitions, unsigned N)
{
  typedef const map<dynamic_bitset<>,count_and_length> container_t;

  // set branch lengths and PP on the consensus tree
  PP.resize(T.n_branches());

  for(int b=0;b<T.n_branches();b++) 
  {
    // Look up the record for this branch
    dynamic_bitset<> partition = branch_partition(T,b);
    if (not partition[0]) partition.flip();
    container_t::const_iterator record = partitions.find(partition);
    assert(record != partitions.end());

    // Record the length
    double L = record->second.length;
    T.branch(b).set_length(L);

    // Record the PP
    unsigned count = record->second.count;
    if (informative(partition))
      PP[b] = double(count)/N;
    else
      PP[b] = -1;
  }
}

void get_branch_lengths(SequenceTree& T,
			const map<dynamic_bitset<>,count_and_length>& partitions, unsigned N)
{
  vector<double> v;
  get_branch_lengths_and_PP(T, v, partitions, N);
}

/// \brief Write out standard majority consensus trees of various support levels
///
/// \param tree_dist The sampled trees
/// \param full_partitions The count and average lengths of each split
/// \param consensus_levels The support levels and the output file name for each one
/// \param with_PP Should the output trees contains Posterior Probabilities in addition to branch lengths?
///
void write_consensus_trees(const tree_sample& tree_dist, const map<dynamic_bitset<>,count_and_length>& full_partitions, 
			   const vector<pair<double, string> >& consensus_levels, bool with_PP)
{
  unsigned N = tree_dist.size();

  typedef const map<dynamic_bitset<>,count_and_length> container_t;
  
  container_t c50_partitions = select_splits(full_partitions, 1+N/2);

  for(int k=0;k < consensus_levels.size();k++) 
  {
    const string& filename = consensus_levels[k].second;

    ostream_or_ofstream output(cout,"-",filename,"consensus tree file");

    container_t partitions = select_splits(c50_partitions, N*consensus_levels[k].first);

    // construct the consensus topology
    SequenceTree consensus = star_tree(tree_dist.names());
    for(container_t::const_iterator i = partitions.begin(); i != partitions.end(); i++)
    {
      if (informative(i->first)) {
	int b = consensus.induce_partition(i->first);
	consensus.branch(b).set_length(i->second.length);
      }
    }

    // set branch lengths and PP on the consensus tree
    vector<double> PP;
    get_branch_lengths_and_PP(consensus, PP, partitions, N);

    bool show_branch_lengths = false;
    for(int b=0;b<consensus.n_branches();b++)
      if (consensus.branch(b).length() >= 0)
	show_branch_lengths = true;

    // write out the consensus tree
    output.unsetf(ios::fixed | ios::showpoint);
    if (with_PP)
      output<<consensus.write_with_bootstrap_fraction(PP,show_branch_lengths)<<std::endl;
    else
      output<<consensus.write(show_branch_lengths)<<std::endl;
  }
}

/// \brief Write out extended majority consensus trees of various support levels (no branch lengths)
///
/// \param tree_dist The sampled trees
/// \param full_partitions The count and average lengths of each split
/// \param consensus_levels The support levels and the output file name for each one
/// \param with_PP Should the output trees contains Posterior Probabilities in addition to branch lengths?
///
void write_extended_consensus_trees(const tree_sample& tree_dist, const vector<pair<Partition,unsigned> >& all_partitions, 
				    const vector<pair<double, string> >& consensus_levels)
{
  unsigned N = tree_dist.size();

  for(int k=0;k < consensus_levels.size();k++) 
  {
    const string& filename = consensus_levels[k].second;

    ostream_or_ofstream output(cout,"-",filename,"extended consensus tree file");

    vector<Partition> all  = get_Ml_partitions(all_partitions,consensus_levels[k].first ,N);
    vector<Partition> sub;
    vector<Partition> full;
    for(int i=0;i<all.size();i++)
      if (all[i].full())
	full.push_back(all[i]);
      else
	sub.push_back(all[i]);

    // the raw tree
    SequenceTree consensus = get_mf_tree(tree_dist.names(),full);

    output.unsetf(ios::fixed | ios::showpoint);
	
    // Write the tree/mf tree
    output<<consensus.write(false)<<std::endl;
    for(int i=0;i<sub.size();i++)
      output<<sub[i]<<endl;
  }
}

vector<double> get_mctree_mean_lengths(MC_tree& Q, 
				       const tree_sample& tree_dist,
				       const map<dynamic_bitset<>,count_and_length>& full_partitions)
{
  unsigned N = tree_dist.size();

  vector<Partition> partitions2;

  // incorporate lengths of branches that map to Q
  for(int i=0;i<Q.branch_order.size();i++) 
  {
    int b = Q.branch_order[i];
    //    int n1 = Q.mapping[b];
    //    int n2 = Q.mapping[Q.reverse(b)];

    Partition P = Q.partitions[b];
    dynamic_bitset<> mask = P.mask();

    // find non-wandering branches directly left of me
    for(int j=0;j<2*Q.n_branches();j++)
    {
      if (Q.directly_left_of(j,b)) 
      {
	if (Q.directly_wanders[j])
	  mask &= ~Q.partitions[j].group1;
	else
	  mask &= Q.partitions[j].mask();
      }
    }

    // find non-wandering branches directly right of me
    for(int j=0;j<2*Q.n_branches();j++)
    {
      if (Q.directly_left_of(b,j)) 
      {
	if (Q.directly_wanders[Q.reverse(j)])
	  mask &= ~Q.partitions[j].group2;
	else
	  mask &= Q.partitions[j].mask();
      }
    }

    // partition masks should be computable from the masks
    //    of both endpoint nodes.
    // but how about degree=0 nodes? this shouldn't work then
    //    assert(equal(mask,node_masks[n1] and node_masks[n2]));

    P.group1 &= mask;
    P.group2 &= mask;

    //    cerr<<"Branch: "<<P<<endl;
    //    cerr<<"   mask = ";show_name_set(cerr,P.names,P.mask())<<endl;

    partitions2.push_back(P);
  }


  // SPEEDUP - If partitions2[i].full(), then just look up the length - don't scan for
  //           branches that imply it.

  // Find the weighted sum of lengths that contribute to each partitions2[i]
  vector<double> branch_lengths(partitions2.size(), 0);
  typedef map<dynamic_bitset<>,count_and_length> container_t;
  for(container_t::const_iterator i = full_partitions.begin(); i != full_partitions.end(); i++)
  {
    Partition P(Q.names(),i->first);
    // Find mc tree branches implied by branch b
    vector<int> branches;
    for(int j=0;j<Q.branch_order.size();j++) 
    {
      if (implies(P,partitions2[j]))
	branches.push_back(j);
    }

    // Divide the branch length evenly between the branches it implies.
    double L = i->second.length;
    double Pr = double(i->second.count)/N;

    for(int j=0;j<branches.size();j++)
      branch_lengths[branches[j]] += L*Pr/branches.size();
  }

  // Compute the fraction of trees that imply each partitions2[i]
  vector<double> total_pr(partitions2.size(), 1);
  for(int j=0;j<Q.branch_order.size();j++) 
  {
    const Partition& P = partitions2[j];
    // leaf branches are implied by every tree - the default of 1 is correct
    if (not informative(P))
      total_pr[j] = 1;

    // If the partition is a full split, then look up its count
    else if (P.full()) {
      dynamic_bitset<> p = P.group2;
      if (not p[0]) p.flip();
      container_t::const_iterator record = full_partitions.find(p);
      if (record == full_partitions.end()) throw myexception()<<"This should not happen!";
      total_pr[j] = double(record->second.count)/N;
    }
    // This is the slow one.
    else 
      total_pr[j] = tree_dist.PP(P);
  }

  // Compute the expected length of partitions2[i] given that it exists.
  for(int i=0;i<branch_lengths.size();i++)
    branch_lengths[i] /= total_pr[i];

  return branch_lengths;
}

void output_mctree_with_lengths(ostream& output, MC_tree Q, 
				const vector<double>& branch_lengths, 
				const vector<double>& node_lengths)
{
  
  //------- Merge lengths and topology -------//
  for(int i=0;i<Q.branch_order.size();i++)
  {
    int p = Q.branch_order[i];
    const Partition& P = Q.partitions[p];
    if (not P.full()) continue;
    int b = which_branch(Q.T,P);
    //    cerr<<" i = "<<i<<"   p = "<<p<<"   b = "<<b<<endl;
    Q.T.branch(b).set_length(branch_lengths[i]);
  }

  output<<Q.T<<endl;

  for(int i=0;i<Q.branch_order.size();i++)
  {
    int b = Q.branch_order[i];
    if (not Q.partitions[b].full()) {
      output<<"branch "<<branch_lengths[i]<<endl;
      output<<Q.partitions[b]<<endl;
    }
  }

  for(int n=0;n<Q.n_nodes();n++) 
  {
    if (node_lengths[n] > 0) {
      output<<"node "<<node_lengths[n]<<endl;
      int b = Q.branch_to_node(n);
      output<<Q.partitions[b]<<endl;
    }
  }
}


/// \brief Write out extended majority consensus trees of various support levels (with branch lengths)
///
/// \param tree_dist The sampled trees
/// \param full_partitions The count and average lengths of each split
/// \param consensus_levels The support levels and the output file name for each one
/// \param with_PP Should the output trees contains Posterior Probabilities in addition to branch lengths?
///
void write_extended_consensus_trees_with_lengths(const tree_sample& tree_dist, 
						 const vector<pair<Partition,unsigned> >& all_partitions,
						 const map<dynamic_bitset<>,count_and_length>& full_partitions,
						 const vector<pair<double, string> >& consensus_levels)
{
  unsigned N = tree_dist.size();

  for(int k=0;k < consensus_levels.size();k++) 
  {
    const string& filename = consensus_levels[k].second;

    ostream_or_ofstream output(cout, "-", filename, "extended consensus tree file with lengths");

    vector<Partition> all  = get_Ml_partitions(all_partitions,consensus_levels[k].first ,N);
    add_leaf_partitions(tree_dist.names(), all);

    vector<Partition> sub;
    vector<Partition> full;
    for(int i=0;i<all.size();i++)
      if (all[i].full())
	full.push_back(all[i]);
      else
	sub.push_back(all[i]);

    // the raw tree
    MC_tree Q(get_moveable_tree(all));
    vector<double> branch_lengths = get_mctree_mean_lengths(Q,tree_dist,full_partitions);
    vector<double> node_lengths(Q.n_nodes(), 0);
    
    output.unsetf(ios::fixed | ios::showpoint);

    // Write the tree/mf tree
    output_mctree_with_lengths(output, Q, branch_lengths, node_lengths);
  }
}

/// A simple ordering operator to sort pair<Partition,unsigned> by the order of the Partition
struct count_more2 {
  bool operator()(const pair<dynamic_bitset<>,count_and_length>& p1,const pair<dynamic_bitset<>,count_and_length>& p2) const {
    return p1.second.count > p2.second.count;
  }
};

bool compatible(const dynamic_bitset<>& p1, const dynamic_bitset<>& p2)
{
  assert(p1.size() == p2.size());
  if (not p1.intersects(p2)) return true;
  if (not (~p1).intersects(p2)) return true;

  if (not p1.intersects(~p2)) return true;
  if (not (~p1).intersects(~p2)) return true;

  return false;
}

bool compatible(const std::vector<dynamic_bitset<> >&P1, const dynamic_bitset<>& p2)
{
  for(int i=0;i<P1.size();i++)
    if (not compatible(P1[i],p2)) return false;

  return true;
}

void write_greedy_consensus(const tree_sample& tree_dist,
			    const map<dynamic_bitset<>,count_and_length>& full_partitions,
			    const string& filename, bool with_PP)
{
  unsigned N = tree_dist.size();

  unsigned L = tree_dist.names().size();
  
  multiset< pair<dynamic_bitset<>,count_and_length>, count_more2 > sorted_splits;
  for(const auto& i: full_partitions)
    sorted_splits.insert(i);

  vector<dynamic_bitset<> > S;
  for(const auto& i: sorted_splits)
  {
    if (compatible(S, i.first))
      S.push_back(i.first);

    if (S.size() >= 2*L-3) break;
  }
  
  // construct the consensus topology
  SequenceTree consensus = star_tree(tree_dist.names());
  for(int i=0;i<S.size();i++)
    if (informative(S[i]))
      consensus.induce_partition(S[i]);

  // set branch lengths and PP on the consensus tree
  vector<double> PP;
  get_branch_lengths_and_PP(consensus, PP, full_partitions, N);
  
  bool show_branch_lengths = false;
  for(int b=0;b<consensus.n_branches();b++)
    if (consensus.branch(b).length() >= 0)
      show_branch_lengths = true;
  
  ostream_or_ofstream output(cout,"-",filename,"greedy consensus tree file");

  // write out the consensus tree
  output.unsetf(ios::fixed | ios::showpoint);
  if (with_PP)
    output<<consensus.write_with_bootstrap_fraction(PP,show_branch_lengths)<<std::endl;
  else
    output<<consensus.write(show_branch_lengths)<<std::endl;
}

int main(int argc,char* argv[]) 
{ 
  try {

    std::cout.precision(3);
    std::cout.setf(ios::fixed);

    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //--------------------- Initialize ---------------------//
    if (args.count("seed")) {
      unsigned long seed = args["seed"].as<unsigned long>();
      myrand_init(seed);
    }
    else
      myrand_init();

    int skip = 0;
    double skip_fraction=0;
    {
      string s = args["skip"].as<string>();
      if (not can_be_converted_to<int>(s,skip)) {
	skip = 0;
	if (not s.size() or s[s.size()-1] != '%')
	  throw myexception()<<"Argument to --skip="<<s<<" is neither an integer nor a percent";
	else
	  skip_fraction = convertTo<double>(s.substr(0,s.size()-1))/100;
      }
    }

    int subsample=args["sub-sample"].as<int>();

    int max = -1;
    if (args.count("max"))
      max = args["max"].as<int>();

    double min_support = args["min-support"].as<double>();

    double report_ratio = args["odds-ratio"].as<double>();

    bool show_sub = args.count("sub-partitions") or args.count("extended-consensus") or args.count("extended-consensus-L");

    // leaf taxa to ignore
    vector<string> ignore;
    if (args.count("ignore") and args["ignore"].as<string>().size() > 0)
      ignore = split(args["ignore"].as<string>(),',');

    // consensus levels 
    string c_levels = args.count("consensus") ? args["consensus"].as<string>() : "";
    vector<pair<double,string> > consensus_levels = get_consensus_levels(c_levels);
    string c_levels_pp = args.count("consensus-PP") ? args["consensus-PP"].as<string>() : "";
    vector<pair<double,string> > consensus_levels_pp = get_consensus_levels(c_levels_pp);
    string ec_levels = args.count("extended-consensus") ? args["extended-consensus"].as<string>() : "";
    vector<pair<double,string> > extended_consensus_levels = get_consensus_levels(ec_levels);
    string ecl_levels = args.count("extended-consensus-L") ? args["extended-consensus-L"].as<string>() : "";
    vector<pair<double,string> > extended_consensus_L_levels = get_consensus_levels(ecl_levels);
    string greedy_filename = args.count("greedy-consensus") ? args["greedy-consensus"].as<string>() : "";

    if (not args.count("consensus") 
	and not args.count("consensus-PP") 
	and not args.count("extended-consensus")
	and not args.count("extended-consensus-L")
	and not args.count("greedy-consensus"))
      consensus_levels_pp.push_back(pair<double,string>(0.5,"-"));

    ostream_or_ofstream report_file;

    if (args.count("report"))
    {
      string filename = args["report"].as<string>();
      report_file.open(cout,"-",filename,"report file");
    }

    //-------------- Read in tree distributions --------------//
    vector<string> files;
    if (args.count("files"))
      files = args["files"].as<vector<string> >();
    if (not files.size())
      throw myexception()<<"No filenames for trees specified.\n\nTry `"<<argv[0]<<" --help' for more information.";

    tree_sample tree_dist;

    vector<tree_sample> trees(files.size());
    int min_trees = -1;
    for(int i=0;i<files.size();i++) 
    {
      int count = 0;
      if (files[i] == "-")
	count = trees[i].load_file(std::cin,skip,subsample,max,ignore);
      else
	count = trees[i].load_file(files[i],skip,subsample,max,ignore);      

      if (log_verbose)
	std::cerr<<"Read "<<count<<" trees from '"<<files[i]<<"'"<<std::endl;

      if (min_trees == -1)
	min_trees = count;
      else
	min_trees = std::min(min_trees, count);
    }

    int min_skip = 0;
    if (skip == 0)
      min_skip = (int)(skip_fraction * min_trees);

    if (log_verbose and min_skip > 0)
      cerr<<"Skipping "<<skip_fraction*100<<"% of "<<min_trees<<" = "<<min_skip<<endl;
    for(int i=0;i<trees.size();i++) {
      if (skip == 0 and skip_fraction > 0) {
	int my_skip = std::min<int>(min_skip, trees[i].trees.size());
	trees[i].trees.erase(trees[i].trees.begin(), trees[i].trees.begin() + my_skip);
      }
      tree_dist.append_trees(trees[i]);
    }
    

    const unsigned N = tree_dist.size();

    dynamic_bitset<> ignore_mask = group_from_names(tree_dist.names(),vector<string>());

    //------ Compute Ml partitions or sub-partitions --------//
    std::map<dynamic_bitset<>,count_and_length> full_partitions = get_partition_counts_and_lengths(tree_dist);
    vector< pair<Partition,unsigned> > all_partitions;

    if (show_sub)
    {
      int depth = args["depth"].as<int>();

      double min_rooting = args["rooting"].as<double>();

      all_partitions = get_Ml_sub_partitions_and_counts(tree_dist,min_support, ~ignore_mask,min_rooting,depth);
      //      std::cerr<<"n_sub_partitions = "<<all_partitions.size()<<"\n";
    }
    else
      all_partitions = get_Ml_partitions_and_counts(tree_dist,min_support, ~ignore_mask);

    vector<int> which_topology;
    vector<int> topology_counts;
    std::map<tree_record,int> topologies_index;

    for(int i=0;i<tree_dist.size();i++)
    {
      std::map<tree_record,int>::iterator record = topologies_index.find(tree_dist[i]);
      if (record == topologies_index.end())
      {
	which_topology.push_back(i);
	topology_counts.push_back(0);

	topologies_index[tree_dist[i]] = which_topology.size()-1;
	record = topologies_index.find(tree_dist[i]);
      }
      topology_counts[record->second]++;
    }
    vector<int> order = iota<int>(topology_counts.size());

    std::sort(order.begin(),order.end(),sequence_order<int>(topology_counts));
    std::reverse(order.begin(), order.end());

    //------  Topologies to analyze -----//
    vector<string> topologies;

    const int L  = tree_dist.names().size();

    // # of trees read in
    report_file<<"# n_trees = "<<tree_dist.size();
    // # of distinct topologies
    report_file<<"   n_topologies = "<<topology_counts.size();
    // # of leaves
    report_file<<"   n_splits/(Leaves-3) = "<<double(full_partitions.size())/(L-3)<<endl;
    // # of distinct splits
    report_file<<"# n_splits = "<<full_partitions.size();
    // # of leaves
    report_file<<"   Leaves = "<<L<<endl;

    report_file<<"\nTopology support: \n\n";
    for(int i=0;i < args["map-trees"].as<int>() ;i++) 
    {
      if (i >= order.size()) continue;

      string t = tree_dist.T(which_topology[order[i]]).write(false);

      unsigned n = topology_counts[order[i]];
      double PP = double(n)/N;
      double o = odds(n,N,1);

      report_file<<"MAP-"<<i<<" = "<<t<<endl;
      report_file<<"   PP = "<<PP<<"       LOD = "<<log10(o)<<"     (count = "<<n<<")\n";
      report_file<<"\n";
    }

    // write out the map tree
    if (args.count("map-tree")) 
    {
      string filename = args["map-tree"].as<string>();

      ostream_or_ofstream output(cout,"-",filename,"MAP tree file");

      SequenceTree map_tree = tree_dist.T(which_topology[order[0]]);
      get_branch_lengths(map_tree, full_partitions, N);

      output<<map_tree<<endl;
    }

    
    for(int i=0,n=0;i<topology_counts.size();i++) 
    {
      n += topology_counts[i];
      double PP = double(n)/N;

      if (PP >= 0.95) {
	report_file<<"95% credible set contains "<<i+1<<" topologies."<<endl;
	break;
      }
    }
    report_file<<"\n\n";


    //------- Print out support for each partition --------//
    report_file<<"Partition support: \n\n";

    vector<pair<Partition,unsigned> > good_partitions = thin(all_partitions, N, report_ratio);

    sort(good_partitions.begin(),good_partitions.end(), count_more());

    for(int i=0;i<good_partitions.size();i++) 
    {
      if (not informative(good_partitions[i].first))
	continue;

      unsigned n = good_partitions[i].second;

      double PP = double(n)/N;
      double o = odds(n,N,1);

      report_file<<"   PP = "<<PP<<"       LOD = "<<log10(o);

      if (not good_partitions[i].first.full()) {
	double ratio = odds_ratio(good_partitions,i,N,1);
	report_file<<"       ratio = "<<log10(ratio);
      }
      report_file<<"       pi = "<<good_partitions[i].first<<endl;

      report_file<<endl<<endl;
    }


    //----------- display M[l] consensus levels ----------//
    write_support_level_graph(args, all_partitions, N);
    write_extended_support_level_graph(args, all_partitions, tree_dist, N);

    //----------- display M[l] consensus trees ----------//
    std::cout.precision(4);

    write_consensus_trees(tree_dist, full_partitions, consensus_levels,false);
    write_consensus_trees(tree_dist, full_partitions, consensus_levels_pp,true);
    if (args.count("greedy-consensus"))
      write_greedy_consensus(tree_dist, full_partitions, greedy_filename, true);
    write_extended_consensus_trees(tree_dist, all_partitions, extended_consensus_levels);
    write_extended_consensus_trees_with_lengths(tree_dist, all_partitions, full_partitions, extended_consensus_L_levels);
  }
  catch (std::exception& e) {
    std::cerr<<"trees-consensus: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
