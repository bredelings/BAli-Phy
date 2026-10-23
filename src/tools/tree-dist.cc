/*
  Copyright (C) 2004-2009 Benjamin Redelings

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
#include "tree-dist.hh"
#include "util/io.hh"
#include "read-trees.hh"
#include "util/mapping.hh"
#include "util/string/convert.hh"
#include "util/cmdline.hh"
#include "util/range.hh"

extern int log_verbose;

using std::vector;
using std::valarray;

using std::string;
using std::endl;
using std::cerr;
using std::cout;
using std::istream;

using boost::dynamic_bitset;
using std::shared_ptr;
using std::optional;
using boost::program_options::variables_map;

int cmp(const tree_record& t1, const tree_record& t2)
{
    int x = (int)t1.n_leaves() - (int)t2.n_leaves();

    if (x != 0) return x;

    x = (int)t1.n_internal_branches() - (int)t2.n_internal_branches();

    if (x != 0) return x;

    for(int i=0;i<t1.n_internal_branches();i++)
    {
	if (t1.partitions[i] == t2.partitions[i])
	    continue;
	else if (t1.partitions[i] < t2.partitions[i])
	    return -1;
	else
	    return 1;
    }
    return 0;
}

bool operator<(const tree_record& t1, const tree_record& t2)
{
    return cmp(t1,t2) < 0;
}

bool operator>(const tree_record& t1, const tree_record& t2)
{
    return cmp(t1,t2) > 0;
}

SequenceTree tree_sample::T(int i) const 
{
    return get_mf_tree(leaf_names,trees[i].partitions, trees[i].branch_lengths);
}

valarray<bool> tree_sample::support(const partition& p) const 
{
    valarray<bool> result(size());

    for(int i=0;i<result.size();i++) 
    {
	// Get a tree with the same topology
	const vector<dynamic_bitset<> > & T = trees[i].partitions;
    
	result[i] = implies(T,p);
    }
    return result;
}

valarray<bool> tree_sample::support(const vector<partition>& partitions) const 
{
    valarray<bool> result(size());

    vector<partition> informative_partitions = select(partitions,informative);

    for(int i=0;i<result.size();i++) 
    {
	// Get a tree with the same topology
	const vector<dynamic_bitset<> >& T = trees[i].partitions;
    
	result[i] = implies(T,informative_partitions);
    }
    return result;
}

unsigned tree_sample::count(const partition& P) const 
{
    unsigned count=0;
    for(int t=0;t<trees.size();t++) 
	if (implies(trees[t].partitions,P))
	    count ++;
   
    return count;
}

unsigned tree_sample::count(const vector<partition>& partitions) const 
{
    unsigned count=0;
    for(int t=0;t<trees.size();t++) {
	if (implies(trees[t].partitions,partitions))
	    count ++;
    }
   
    return count;
}

double tree_sample::PP(const partition& P) const 
{
    return double(count(P))/size();
}

double tree_sample::PP(const vector<partition>& partitions) const 
{
    return double(count(partitions))/size();
}

tree_record::tree_record(const Tree& T)
    :n_leaves_(T.n_leaves()),
     partitions(T.n_branches()-T.n_leafbranches()),
     branch_lengths(T.n_branches())
{ 
    vector<dynamic_bitset<> > temp(partitions.size());

    const int L = T.n_leafbranches();
    for(int i=L;i<T.n_branches();i++) {
	temp[i-L] = branch_partition(T,i);
	if (not temp[i-L][0])
	    temp[i-L].flip();
    }

    vector<int> order = iota<int>(partitions.size());
    std::sort(order.begin(),order.end(),sequence_order<dynamic_bitset<> >(temp));

    for(int i=0;i<L;i++)
    {
	if (T.branch(i).has_length())
	    branch_lengths[i] = T.branch(i).length();
	else
	    branch_lengths[i] = 1.0;
    }

    for(int i=L;i<T.n_branches();i++)
    {
	int o = order[i-L];
	partitions[i-L] = temp[o];
	if (T.branch(o+L).has_length())
	    branch_lengths[i] = T.branch(o+L).length();
	else
	    branch_lengths[i] = 1.0;
    }
}


void tree_sample::add_tree(const tree_record& T)
{
    trees.push_back(T);
}

void tree_sample::add_tree(Tree& T)
{
    //------------ check tree ---------------//
    if (has_sub_branches(T))
	throw myexception()<<"Tree has node of degree 2";

    // Compute the standardized representation
    T.standardize();
    add_tree(tree_record(T));
}

void tree_sample::add_tree(RootedTree& T)
{
    if (T.root().degree() == 2)
	T.remove_node_from_branch(T.root());

    add_tree(static_cast<Tree&>(T));
}

// What we actually want is a standardized STRING representation.
//  - We need to have ways of *reading* a Newick or NEXUS file and then
//  - ... converting to some intermediate record structure..
//  - ... which in this case in the standard string.

// We should be able to avoid sorting the leaves with every tree... in both cases!

// Reading an individual tree requires (a) one line, and (b) some state: ordered leaf names.

// Because we have to prune everything anyway, let's make the standard intermediate format
// .. be a (unrooted) Tree -- the names should be determined and given beforehand.



// Thin only the newly loaded suffix, preserving earlier files. Equally spaced
// positions include both endpoints; rounding half upward selects the nearest tree.
// A one-tree cap retains the first tree without dividing by zero.
static void thin_sample(vector<tree_record>& trees, int first, int max)
{
    if (max < 1) throw myexception()<<"The maximum number of trees must be positive.";
    const int n = trees.size() - first;
    if (n <= max) return;

    vector<tree_record> selected;
    selected.reserve(max);
    for (int i=0; i<max; ++i)
    {
        int index = first + (max == 1 ? 0 : int(floor(0.5 + double(i)*(n-1)/(max-1))));
        selected.push_back(std::move(trees[index]));
    }
    if (first == 0)
        trees = std::move(selected);
    else
    {
        trees.erase(trees.begin()+first, trees.end());
        trees.insert(trees.end(), make_move_iterator(selected.begin()), make_move_iterator(selected.end()));
    }
}

// All samples have already been limited by --until, but not thinned. Use the
// shortest run to remove the same burn-in count everywhere; fractions below one
// retain part of every nonempty run. Subsampling starts at the first survivor,
// then the cap selects evenly spaced survivors, just as for integer burn-in.
void select_after_burnin(vector<tree_sample>& samples, double fraction, int subsample, optional<int> max)
{
    if (samples.empty()) return;
    unsigned shortest = samples.front().size();
    for (const auto& sample: samples) shortest = std::min(shortest, sample.size());
    const unsigned skip = floor(fraction * shortest);
    if (log_verbose) cerr<<"Skipping "<<fraction*100<<"% of "<<shortest<<" = "<<skip<<endl;

    for (auto& sample: samples)
    {
        vector<tree_record> selected;
        const int stride = std::max(1, subsample);
        selected.reserve((sample.size()-skip + stride-1)/stride);
        for (size_t i=skip; i<sample.size(); i+=stride)
            selected.push_back(std::move(sample.trees[i]));
        sample.trees = std::move(selected);
        if (max) thin_sample(sample.trees, 0, *max);
        if (sample.trees.empty()) throw myexception()<<"No trees remain after burn-in.";
    }
}

int tree_sample::load_file(istream& file, int skip, optional<int> last, int subsample, optional<int> max, const vector<string>& prune)
{
    using namespace trees_format;

    //----------- Construct File Reader / Filter -----------//
    shared_ptr<reader_t> trees_in(new Newick_or_NEXUS(file));

    if (last)
	trees_in = shared_ptr<reader_t>(new Last(*last,*trees_in));

    if (skip > 0)
	trees_in = shared_ptr<reader_t>(new Skip(skip,*trees_in));

    if (subsample > 1)
	trees_in = shared_ptr<reader_t>(new Subsample(subsample,*trees_in));

    trees_in = shared_ptr<reader_t>(new Fixroot(*trees_in));

    if (prune.size())
	trees_in = shared_ptr<reader_t>(new Prune(prune,*trees_in));

    if (not leaf_names.size())
	leaf_names = trees_in->names();
    else 
    {
	vector<string> leaf_names2 = trees_in->names();
	if (leaf_names2.size() != leaf_names.size())
	    throw myexception()<<"New trees with "<<leaf_names2.size()<<" leaves conflict with current trees with "<<leaf_names.size()<<" leaves.";

	try {
	    compute_mapping(leaf_names, leaf_names2);
	}
	catch (bad_mapping<string>& b) {
	    throw myexception()<<"New trees are missing leaf '"<<b.missing<<"'";
	}

    }

    //------------------- Process Trees --------------------//
    RootedTree T;
    int t=0;
    int old_size = trees.size();
    while (trees_in->next_tree(T)) {
	add_tree(T);
	t++;
    }

    if (max) thin_sample(trees, old_size, *max);

    if (size() == 0)
	throw myexception()<<"No trees were read in!";

    return t;
}

int tree_sample::load_file(const string& filename, int skip, optional<int> last, int subsample, optional<int> max, const vector<string>& prune)
{
    istream_or_ifstream file(std::cin, "-", filename, "tree samples file");
  
    int count = load_file(file,skip,last,subsample,max,prune);

    // There is no message if we just open a stream.
    if (log_verbose) std::cerr<<"Read "<<count<<" trees from '"<<filename<<"'"<<std::endl;

    return count;
}

int tree_sample::append_trees(const tree_sample& trees)
{
    if (not leaf_names.size())
	leaf_names = trees.names();
    else 
    {
	if (trees.names().size() != leaf_names.size())
	    throw myexception()<<"New trees with "<<trees.names().size()<<" leaves conflict with current trees with "<<leaf_names.size()<<" leaves.";

	try {
	    compute_mapping(leaf_names, trees.names());
	}
	catch (bad_mapping<string>& b) {
	    throw myexception()<<"New trees are missing leaf '"<<b.missing<<"'";
	}

    }

    for(int i=0;i<trees.size();i++) {
	add_tree(trees[i]);
    }

    return trees.size();
}

tree_sample::tree_sample(const vector<string>& L)
    :leaf_names(L)
{
}

tree_sample::tree_sample(istream& file, int skip, optional<int> last,int subsample, optional<int> max, const vector<string>& prune)
{
    load_file(file,skip,last,subsample,max,prune);
}

tree_sample::tree_sample(const string& filename, int skip, optional<int> last,int subsample, optional<int> max, const vector<string>& prune)
{
    load_file(filename,skip,last,subsample,max,prune);
}

void scan_trees(istream& file, int skip, optional<int> last, int subsample, const vector<string>& prune,
		const vector<string>& leaf_order, accumulator<SequenceTree>& op)
{
    using namespace trees_format;

    //----------- Construct File Reader / Filter -----------//
    shared_ptr<reader_t> trees(new Newick_or_NEXUS(file));

    if (skip > 0)
	trees = shared_ptr<reader_t>(new Skip(skip,*trees));

    if (subsample > 1)
	trees = shared_ptr<reader_t>(new Subsample(subsample,*trees));

    if (last)
	trees = shared_ptr<reader_t>(new Last(*last,*trees));

    trees = shared_ptr<reader_t>(new Fixroot(*trees));

    if (prune.size())
	trees = shared_ptr<reader_t>(new Prune(prune,*trees));

    if (leaf_order.size())
	trees = shared_ptr<reader_t>(new ReorderLeaves(leaf_order,*trees));

    //------------------- Process Trees --------------------//
    RootedSequenceTree T;
    while (trees->next_tree(T))
	op(T);
}

void scan_trees(istream& file,int skip,optional<int> last,int subsample, const vector<string>& prune,
		accumulator<SequenceTree>& op)
{
    scan_trees(file,skip,last,subsample,prune,vector<string>(),op);
}

void scan_trees(istream& file,int skip,optional<int> last,int subsample,
		accumulator<SequenceTree>& op)
{
    scan_trees(file,skip,last,subsample,vector<string>(),op);
}

tree_sample read_trees(variables_map& args, const vector<string>& leaf_labels)
{
    int read_skip = 0;
    optional<int> skip;
    optional<double> skip_fraction;
    if (args.count("skip"))
    {
	string s = args["skip"].as<string>();
	if (not s.empty() and s[s.size()-1] == '%')
	{
	    if (auto d = can_be_converted_to<double>(s.substr(0,s.size()-1)))
	    {
		skip_fraction = d.value()/100;
		if (*skip_fraction < 0 or *skip_fraction > 1)
		    throw myexception()<<"Argument to '--skip="<<s<<"' is an invalid percent";

		// We have to read the whole file to know how much to skip.
		read_skip = 0;
	    }
	}
	else if (auto skip_int = can_be_converted_to<int>(s))
	{
	    skip = skip_int.value();
	    // We can discard as we read the file.
	    read_skip = *skip;
	}

	if (not skip and not skip_fraction)
	    throw myexception()<<"Argument to '--skip="<<s<<"' is neither an integer nor a percent";
    }

    int subsample=args["subsample"].as<int>();

    optional<int> last;
    if (args.count("until"))
	last = args["until"].as<int>();

    optional<int> max;
    if (args.count("max"))
	max = args["max"].as<int>();

    // leaf taxa to ignore
    vector<string> ignore = get_string_list(args, "ignore");

    vector<string> filenames;
    if (args.count("files"))
	filenames = args["files"].as<vector<string> >();

    tree_sample tree_dist;
    if (not leaf_labels.empty())
	tree_dist = tree_sample(leaf_labels);

    // Percentage burn-in needs the unthinned run lengths before any selection.
    vector<tree_sample> tree_samples(filenames.size());
    for(int i=0;i<filenames.size();i++)
        tree_samples[i].load_file(filenames[i], read_skip, last,
                                 skip_fraction ? 1 : subsample, skip_fraction ? optional<int>{} : max, ignore);

    if (skip_fraction)
        select_after_burnin(tree_samples, *skip_fraction, subsample, max);

    // Concatenate the tree samples
    for(auto& sample: tree_samples)
	tree_dist.append_trees(sample);
    
    return tree_dist;
}


tree_sample read_trees(variables_map& args)
{
    return read_trees(args,{});
}
