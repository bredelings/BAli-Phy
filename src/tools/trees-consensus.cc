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

#include "tree/sequencetree.hh"
#include "util/string/split.hh"
#include "util/cmdline.hh"
#include "util/string/convert.hh"
#include "util/set.hh"
#include "util/range.hh"
#include "statistics.hh"
#include "bootstrap.hh"
#include "tree-dist.hh"
#include "consensus-tree.hh"
#include "mctree.hh"
#include "util/rng.hh"

#include <CLI/CLI.hpp>
#include <optional>

extern int log_verbose;

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



using boost::dynamic_bitset;

using std::cout;
using std::cerr;
using std::endl;
using std::vector;
using std::string;
using std::pair;
using std::valarray;
using std::ostream;
using std::map;
using std::set;
using std::multiset;
using std::ios;
using std::ofstream;

using statistics::odds;
using std::optional;

// What if everything in 'split' is true?
// What if everything in 'split' is true, but 1 taxa?
//  These are true by definition...

struct consensus_level_order
{
    bool operator()(const pair<double,string>& p1, const pair<double,string>& p2) const
	{
	    return p1.first < p2.first;
	}
};

// FIXME - if n is small, first level may be ~ 0.57 -- good or bad?
vector<pair<double, string> > get_consensus_levels(const vector<string>& levels)
{
    vector<pair<double, string> > parsed_levels;

    // Split each component level>file into level (a double) and file (a string)
    for(auto& level: levels)
    {
	if (not level.size()) continue;
	vector<string> parts = split(level,':');
	if (parts.size() > 2) throw myexception()<<"Consensus level '"<<level<<"' has too many files.";
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

double odds_ratio(const vector<pair<partition,unsigned> >& partitions, const pair<partition,unsigned>& delta,
		  unsigned N, unsigned pseudocount=0)
{
    unsigned n1 = 0;
    for(int i=0;i<partitions.size();i++)
	if (implies(partitions[i].first, delta.first))
	    n1 = std::max(n1, partitions[i].second);

    unsigned n2 = delta.second;

    return statistics::odds_ratio(n1,n2,N,pseudocount);
}

double odds_ratio(vector<pair<partition,unsigned> > partitions, int i,
		  unsigned N, unsigned pseudocount=0)
{
    pair<partition,unsigned> delta = partitions[i];
    partitions.erase(partitions.begin()+i);
    return odds_ratio(partitions,delta,N,pseudocount);
}



bool covers(const pair<partition,unsigned>& p1,const pair<partition,unsigned>& p2, 
	    double ratio,unsigned N,unsigned pseudocount)
{
    if (not implies(p1.first, p2.first))
	return false;

    int n1 = p1.second;
    int n2 = p2.second;

    return (statistics::odds_ratio(n1,n2,N,pseudocount) < ratio);
}

bool covers(const vector<pair<partition,unsigned> >& partitions, int exclude,
	    const pair<partition,unsigned>& delta,double ratio, unsigned N, unsigned pseudocount=0)
{
    for(int i=0;i<partitions.size();i++)
	if (i != exclude and covers(partitions[i],delta,ratio,N,pseudocount))
	    return true;
    return false;
}

bool merge(vector<pair<partition,unsigned> >& partitions,
	   const pair<partition,unsigned>& delta,
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

vector<pair<partition,unsigned> > 
thin(const vector<pair<partition,unsigned> >& partitions, unsigned N, double ratio)
{
    vector<pair<partition,unsigned> > thinned;
    for(int i=0;i<partitions.size();i++)
	merge(thinned,partitions[i],ratio,N,5);

    return thinned;
}

vector<partition> 
get_Ml_partitions(const vector<pair<partition,unsigned> >& sp, unsigned min_count)
{
    vector<partition> partitions;
    for(int i=0;i<sp.size();i++) {
	if (sp[i].second >= min_count)
	    merge_partition(partitions,sp[i].first);
    }

    return partitions;
}


vector<partition> 
get_Ml_partitions(const vector<pair<partition,unsigned> >& sp, double l, unsigned total)
{
    unsigned min_count = std::min(1+(unsigned)(total*l),total);

    return get_Ml_partitions(sp,min_count);
}

vector<unsigned> get_Ml_levels(const vector<pair<partition,unsigned> >& sp,unsigned N,double min_support)
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



/// A simple ordering operator to sort pair<partition,unsigned> by the order of the partition
struct count_more {
    bool operator()(const pair<partition,unsigned>& p1,const pair<partition,unsigned>& p2) const {
	return p1.second > p2.second;
    }
};

/// \brief Create a file containing the LOD, #of supported partitions, and PP in 3 tab-separated columsn
///
/// \param filename Output filename
/// \param all_partitions The count for each partition
/// \param N The total number of sampled trees
void write_support_level_graph(const string& filename, const vector<pair<partition,unsigned> >& all_partitions, unsigned N)
{
    vector<unsigned> levels = get_Ml_levels(all_partitions, N, 0.5);

    ofstream file(filename);
    if (!file)
	throw myexception()<<"Couldn't open file '"<<filename<<"' for writing.";
  
    for(int j=0;j<levels.size();j++) 
    {
	double fraction = double(levels[j])/N;
	double LOD = log10(odds(levels[j],N,1));
    
	const vector<partition> sub = get_Ml_partitions(all_partitions,levels[j]);
	const vector<partition> full = select(sub,&partition::full);

	file<<LOD<<"\t"<<count(full,informative)<<"\t"<<fraction<<"\n";
    }
    file.close();
}

/// \brief Create a file containing the LOD, #of supported partial splits, and PP in 3 tab-separated columsn
///
/// Here, the # of partial splits report is reduced to the largest
/// number of compatible partial splits.
///
/// \param filename Output filename
/// \param all_partitions The count for each partition
/// \param N The total number of sampled trees
void write_extended_support_level_graph(const string& filename, const vector<pair<partition,unsigned> >& all_partitions, 
					tree_sample& tree_dist, unsigned N)
{
    vector<unsigned> levels = get_Ml_levels(all_partitions, N, 0.5);

    ofstream file(filename);
    if (!file)
	throw myexception()<<"Couldn't open file '"<<filename<<"' for writing.";
  
    for(int j=0;j<levels.size();j++) 
    {
	double fraction = double(levels[j])/N;
	double LOD = log10(odds(levels[j],N,1));
    
	vector<partition> sub = get_Ml_partitions(all_partitions,levels[j]);
	add_leaf_partitions(tree_dist.names(), sub);

	// const vector<partition> full = select(sub,&partition::full);


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

void write_tree_with_PP(SequenceTree T,
			const tree_sample& tree_dist,
			const map<dynamic_bitset<>,count_and_length>& full_partitions,
			const string& filename,
			const string& description)
{
    int N = (int)tree_dist.size();

    // set branch lengths and PP on the tree
    vector<double> PP;
    get_branch_lengths_and_PP(T, PP, full_partitions, N);
  
    bool show_branch_lengths = false;
    for(int b=0;b<T.n_branches();b++)
	if (T.branch(b).length() >= 0)
	    show_branch_lengths = true;
  
    ostream_or_ofstream output(cout, "-", filename, description);

    // write out the tree
    output.unsetf(ios::fixed | ios::showpoint);
    output<<T.write_with_bootstrap_fraction(PP,show_branch_lengths)<<std::endl;
}

/// \brief Write out standard majority consensus trees of various support levels
///
/// \param tree_dist The sampled trees
/// \param full_partitions The count and average lengths of each split
/// \param consensus_levels The support levels and the output file name for each one
///
void write_consensus_trees(const tree_sample& tree_dist, const map<dynamic_bitset<>,count_and_length>& full_partitions, 
			   const vector<pair<double, string> >& consensus_levels)
{
    unsigned N = tree_dist.size();

    typedef const map<dynamic_bitset<>,count_and_length> container_t;
  
    container_t c50_partitions = select_splits(full_partitions, 1+N/2);

    for(int k=0;k < consensus_levels.size();k++) 
    {
	const string& filename = consensus_levels[k].second;

	const double level = consensus_levels[k].first;

	// construct the consensus topology
	SequenceTree consensus = star_tree(tree_dist.names());
	for(auto i = c50_partitions.begin(); i != c50_partitions.end(); ++i)
	{
	    // Compare probabilities directly: truncating N*level admits splits below the cutoff.
	    // Strict majorities ensure compatibility; level 1 denotes unanimous support.
	    const unsigned count = i->second.count;
	    if (level == 1 ? count != N : double(count)/N <= level)
		continue;
	    if (informative(i->first)) {
		int b = consensus.induce_partition(i->first);
		consensus.branch(b).set_length(i->second.length);
	    }
	}

	write_tree_with_PP(consensus, tree_dist, full_partitions, filename, "consensus tree file");
    }
}

/// \brief Write out extended majority consensus trees of various support levels (no branch lengths)
///
/// \param tree_dist The sampled trees
/// \param full_partitions The count and average lengths of each split
/// \param consensus_levels The support levels and the output file name for each one
/// \param with_PP Should the output trees contains Posterior Probabilities in addition to branch lengths?
///
void write_extended_consensus_trees(const tree_sample& tree_dist, const vector<pair<partition,unsigned> >& all_partitions, 
				    const vector<pair<double, string> >& consensus_levels)
{
    unsigned N = tree_dist.size();

    for(int k=0;k < consensus_levels.size();k++) 
    {
	const string& filename = consensus_levels[k].second;

	ostream_or_ofstream output(cout,"-",filename,"extended consensus tree file");

	vector<partition> all  = get_Ml_partitions(all_partitions,consensus_levels[k].first ,N);
	vector<partition> sub;
	vector<partition> full;
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
	    output<<Partition(tree_dist.names(),sub[i])<<endl;
    }
}

vector<double> get_mctree_mean_lengths(MC_tree& Q, 
				       const tree_sample& tree_dist,
				       const map<dynamic_bitset<>,count_and_length>& full_partitions)
{
    unsigned N = tree_dist.size();

    vector<partition> partitions2;

    // incorporate lengths of branches that map to Q
    for(int i=0;i<Q.branch_order.size();i++) 
    {
	int b = Q.branch_order[i];
	//    int n1 = Q.mapping[b];
	//    int n2 = Q.mapping[Q.reverse(b)];

	partition P = Q.partitions[b];
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
	const partition& P = partitions2[j];
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
	const partition& P = Q.partitions[p];
	if (not P.full()) continue;
	int b = which_branch(Q.T,P);
	//    cerr<<" i = "<<i<<"   p = "<<p<<"   b = "<<b<<endl;
	Q.T.branch(b).set_length(branch_lengths[i]);
    }

    output<<Q.T<<endl;

    auto& names = Q.names();
    for(int i=0;i<Q.branch_order.size();i++)
    {
	int b = Q.branch_order[i];
	if (not Q.partitions[b].full()) {
	    output<<"branch "<<branch_lengths[i]<<endl;
	    output<<Partition(names,Q.partitions[b])<<endl;
	}
    }

    for(int n=0;n<Q.n_nodes();n++) 
    {
	if (node_lengths[n] > 0) {
	    output<<"node "<<node_lengths[n]<<endl;
	    int b = Q.branch_to_node(n);
	    output<<Partition(names,Q.partitions[b])<<endl;
	}
    }
}


/// \brief Write out extended majority consensus trees of various support levels (with branch lengths)
///
/// \param tree_dist The sampled trees
/// \param full_partitions The count and average lengths of each split
/// \param consensus_levels The support levels and the output file name for each one
///
void write_extended_consensus_trees_with_lengths(const tree_sample& tree_dist, 
						 const vector<pair<partition,unsigned> >& all_partitions,
						 const map<dynamic_bitset<>,count_and_length>& full_partitions,
						 const vector<pair<double, string> >& consensus_levels)
{
    unsigned N = tree_dist.size();

    for(int k=0;k < consensus_levels.size();k++) 
    {
	const string& filename = consensus_levels[k].second;

	ostream_or_ofstream output(cout, "-", filename, "extended consensus tree file with lengths");

	vector<partition> all  = get_Ml_partitions(all_partitions,consensus_levels[k].first ,N);
	add_leaf_partitions(tree_dist.names(), all);

	vector<partition> sub;
	vector<partition> full;
	for(int i=0;i<all.size();i++)
	    if (all[i].full())
		full.push_back(all[i]);
	    else
		sub.push_back(all[i]);

	// the raw tree
	MC_tree Q({tree_dist.names(), get_moveable_tree(all)});
	vector<double> branch_lengths = get_mctree_mean_lengths(Q,tree_dist,full_partitions);
	vector<double> node_lengths(Q.n_nodes(), 0);
    
	output.unsetf(ios::fixed | ios::showpoint);

	// Write the tree/mf tree
	output_mctree_with_lengths(output, Q, branch_lengths, node_lengths);
    }
}

/// A simple ordering operator to sort pair<partition,unsigned> by the order of the partition
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
			    const string& filename)
{
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
    write_tree_with_PP(consensus, tree_dist, full_partitions, filename, "greedy consensus tree file");
}

int main(int argc,char* argv[]) 
{ 
    try {

	std::cout.precision(3);
	std::cout.setf(ios::fixed);

	//---------- Parse command line  -------//
        CLI::App app{"Find consensus trees and supported splits.", "trees-consensus"};
        app.usage("Usage: trees-consensus [OPTIONS] <sampled-trees> [<sampled-trees> ... <sampled-trees>]");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        vector<string> files;
        string skip = "10%", ignore;
        std::optional<int> last, max;
        int subsample = 1, map_trees = 1;
        std::optional<unsigned long> seed;
        bool verbose = false, sub_partitions = false;
        double min_support = 0.25, min_rooting = 0.9, report_ratio = 1.5;
        std::optional<string> report, map_tree, consensus, greedy_filename;
        std::optional<string> extended_consensus, extended_consensus_L, support_levels, extended_support_levels;
        app.add_option("--files,FILE", files, "tree samples to examine")->required();
        app.add_option("--seed", seed, "Random seed")->group("Input options");
        app.add_flag("-V,--verbose", verbose, "Output more log messages on stderr.")->group("Input options");
        app.add_option("-s,--skip", skip,
                       "Burn-in count for every run, or percent of the shortest run before thinning.")
            ->capture_default_str()->group("Input options");
        app.add_option("-u,--until", last, "Read until this number of trees.")->group("Input options");
        app.add_option("-m,--max", max, "Thin tree samples down to this number of trees.")->group("Input options");
        app.add_option("-x,--subsample", subsample, "Factor by which to subsample.")
            ->capture_default_str()->group("Input options");
        app.add_option("-i,--ignore", ignore, "Comma-separated list of taxa to ignore.")->group("Input options");
        app.add_option("--map-trees", map_trees, "Report at most this many topologies in the pooled report.")
            ->capture_default_str()->group("Reporting options");
        app.add_option("--min-support", min_support, "Minimum threshold PP for splits.")
            ->capture_default_str()->group("Reporting options");
        app.add_option("--report", report, "Write supported partitions to file <arg>.")->group("Reporting options");
        app.add_option("--map-tree", map_tree, "Write out the map tree to file <arg>.")->group("Reporting options");
        app.add_option("--consensus", consensus, "Write out consensus trees.")->group("Reporting options");
        app.add_option("--greedy-consensus", greedy_filename, "Write out greedy consensus trees.")
            ->group("Reporting options");
        app.add_option("--extended-consensus-L", extended_consensus_L, "Write out extended consensus trees + lengths.")
            ->group("Reporting options");
        app.add_option("--extended-consensus", extended_consensus, "Write out extended consensus trees.")
            ->group("Reporting options");
        app.add_option("--support-levels", support_levels, "Write #branches versus LOD to file <arg>.")
            ->group("Reporting options");
        app.add_option("--extended-support-levels", extended_support_levels,
                       "Write #sub-branches versus LOD to file <arg>.")
            ->group("Reporting options");
        app.add_flag("--sub-partitions", sub_partitions, "Search for partial splits.")->group("Search options");
        app.add_option("--rooting", min_rooting, "Threshold in search for partial splits.")
            ->capture_default_str()->group("Search options");
        app.add_option("--odds-ratio", report_ratio,
                       "Report partial-splits only if removing taxa improves the odds by at least this ratio.")
            ->capture_default_str()->group("Search options");
        app.footer("Examples:\n\n"
                   " Compute the majority consensus tree, skipping the first 10% of trees:\n"
                   "   % trees-consensus newick.trees > c50.tree\n"
                   "   % trees-consensus newick.trees --skip=10% > c50.tree\n\n"
                   " Skip the first 100 trees:\n"
                   "   % trees-consensus newick.trees --skip=100 > c50.tree\n\n"
                   " Skip the first 20% of trees and take every 10th tree thereafter:\n"
                   "   % trees-consensus newick.trees --skip=20% -x10 > c50.tree\n\n"
                   " Compute the 50% (majority) and 80% consensus trees:\n"
                   "   % trees-consensus newick.trees --consensus=0.5:c50.tree,0.8:c80.tree\n"
                   "   % trees-consensus newick.trees --consensus=0.5,0.8:c80.tree > c50.tree\n\n"
                   " Compute the MAP tree and write a summary of supported partitions:\n"
                   "   % trees-consensus newick.trees --map-tree=MAP.tree --report=partitions.txt\n\n"
                   " Compute the MAP tree and write it to the standard output:\n"
                   "   % trees-consensus newick.trees --map-tree=- --report=partitions.txt\n\n");
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }
        if (verbose) log_verbose = 1;

	//--------------------- Initialize ---------------------//
	if (seed) {
	    myrand_init(*seed);
	}
	else
	    myrand_init();

	bool show_sub = sub_partitions or extended_consensus or extended_consensus_L;

	// consensus levels 
	auto consensus_levels = get_consensus_levels(
            consensus ? get_string_list(*consensus) : vector<string>{});
	auto extended_consensus_levels = get_consensus_levels(
            extended_consensus ? get_string_list(*extended_consensus) : vector<string>{});
	auto extended_consensus_L_levels = get_consensus_levels(
            extended_consensus_L ? get_string_list(*extended_consensus_L) : vector<string>{});

	if (not consensus 
	    and not extended_consensus
	    and not map_tree
	    and not extended_consensus_L
	    and not greedy_filename)
	    consensus_levels.push_back(pair<double,string>(0.5,"-"));

	ostream_or_ofstream report_file;

	if (report)
	{
	    string filename = *report;
	    report_file.open(cout,"-",filename,"report file");
	}

	//-------------- Read in tree distributions --------------//
	auto tree_dist = read_trees(files, skip, last, subsample, max, get_string_list(ignore));

	const unsigned N = tree_dist.size();

	dynamic_bitset<> ignore_mask = group_from_names(tree_dist.names(),vector<string>());

	//------ Compute Ml partitions or sub-partitions --------//
	std::map<dynamic_bitset<>,count_and_length> full_partitions = get_partition_counts_and_lengths(tree_dist);
	vector< pair<partition,unsigned> > all_partitions;

	if (show_sub)
	{
	    all_partitions = get_Ml_sub_partitions_and_counts(tree_dist,min_support, ~ignore_mask,min_rooting);
	    //      std::cerr<<"n_sub_partitions = "<<all_partitions.size()<<"\n";
	}
	else
	    all_partitions = get_Ml_partitions_and_counts(tree_dist,min_support, ~ignore_mask);

	vector<int> which_topology;
	vector<int> topology_counts;
	std::map<tree_record,int> topologies_index;

	for(int i=0;i<tree_dist.size();i++)
	{
	    auto record = topologies_index.find(tree_dist[i]);
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
	for(int i=0;i < map_trees ;i++) 
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

	for(int i=0,n=0;i<topology_counts.size();i++) 
	{
	    n += topology_counts[order[i]];
	    double PP = double(n)/N;

	    if (PP >= 0.95) {
		report_file<<"95% credible set contains "<<i+1<<" topologies."<<endl;
		break;
	    }
	}
	report_file<<"\n\n";


	//------- Print out support for each partition --------//
	report_file<<"partition support: \n\n";

	vector<pair<partition,unsigned> > good_partitions = thin(all_partitions, N, report_ratio);

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
	    report_file<<"       pi = "<<Partition(tree_dist.names(), good_partitions[i].first)<<endl;

	    report_file<<endl<<endl;
	}

	//----------- display M[l] consensus levels ----------//
	if (support_levels)
	    write_support_level_graph(*support_levels, all_partitions, N);
	if (extended_support_levels)
	    write_extended_support_level_graph(*extended_support_levels, all_partitions, tree_dist, N);

	//----------- display M[l] consensus trees ----------//
	std::cout.precision(4);

	// write out the map tree
	if (map_tree) 
	{
	    string filename = *map_tree;

	    SequenceTree map_tree = tree_dist.T(which_topology[order[0]]);

	    write_tree_with_PP(map_tree, tree_dist, full_partitions, filename, "MAP tree file");
	}

    
	write_consensus_trees(tree_dist, full_partitions, consensus_levels);
	if (greedy_filename)
	    write_greedy_consensus(tree_dist, full_partitions, *greedy_filename);
	write_extended_consensus_trees(tree_dist, all_partitions, extended_consensus_levels);
	write_extended_consensus_trees_with_lengths(tree_dist, all_partitions, full_partitions, extended_consensus_L_levels);
    }
    catch (std::exception& e) {
	std::cerr<<"trees-consensus: Error! "<<e.what()<<endl;
	exit(1);
    }
    return 0;
}
