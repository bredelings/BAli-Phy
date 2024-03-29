/*
  Copyright (C) 2009-2010 Benjamin Redelings

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

#include "consensus-tree.H"

#include <map>
#include <list>

#include "util/myexception.H"
#include "util/log-level.H"
#include "statistics.H"

using std::vector;
using std::string;
using std::map;
using std::list;
using std::pair;

using std::endl;
using std::cerr;

using boost::dynamic_bitset;


void add_partitions_and_counts(const vector<tree_sample>& samples, int index, map<dynamic_bitset<>,p_counts>& counts)
{
    const tree_sample& sample = samples[index];

    vector<string> names = sample.names();

    typedef map<dynamic_bitset<>,p_counts> container_t;

    for(int i=0;i<sample.trees.size();i++) 
    {
	const vector<dynamic_bitset<> >& T = sample.trees[i].partitions;

	// for each partition in the next tree
	dynamic_bitset<> partition(names.size());
	for(int b=0;b<T.size();b++) 
	{
	    partition = T[b];

	    if (not partition[0])
		partition.flip();

	    // Look up record for this partition
	    container_t::iterator record = counts.find(partition);
	    if (record == counts.end()) {
		counts.insert(container_t::value_type(partition,p_counts(samples.size())));
		record = counts.find(partition);
		assert(record != counts.end());
	    }

	    //      cerr<<" dist = "<<index<<" topology = "<<i<<" branch = "<<b<<"    split="<<partition<<endl;
    
	    // Record this tree as having the partition if we haven't already done so. 
	    p_counts& pc = record->second;
	    //      cerr<<"    "<<join(pc.counts,' ')<<endl;
	    pc.counts[index] ++;
	    //      cerr<<"    "<<join(pc.counts,' ')<<endl;
	}
    }
}

map<dynamic_bitset<>,p_counts> get_multi_partitions_and_counts(const vector<tree_sample>& samples)
{
    map<dynamic_bitset<>,p_counts> partitions;

    for(int i=0;i<samples.size();i++)
	add_partitions_and_counts(samples, i, partitions);

    return partitions;
}

struct p_count {
    int count = 0;
    int last_tree = -1;
    p_count() {}
};

/// \brief Get the count and average length for each split
///
/// \param sample The tree sample
///
map<dynamic_bitset<>,count_and_length>
get_partition_counts_and_lengths(const tree_sample& sample)
{
    // use an rbtree of <partition,count_and_length>, sorted by partition.
    typedef map<dynamic_bitset<>,count_and_length> container_t;
    container_t counts;

    vector<string> names = sample.names();
    const int L = names.size();
    const int N = sample.trees.size();

    // Setup: add leaf branch records and store references to them
    vector<container_t::iterator> leaf_branch_records;
    for(int i=0;i<L;i++) 
    {
	// construct leaf branch split
	dynamic_bitset<> partition(names.size());
	partition[i] = 1;
	if (not partition[0]) partition.flip();
  
	// insert it a get a reference
	counts.insert(container_t::value_type(partition,count_and_length(N,0)));
	container_t::iterator record = counts.find(partition);

	assert(record != counts.end());
	leaf_branch_records.push_back(record);
    }

    // Main loop: iterate over all trees
    for(int i=0;i<sample.trees.size();i++) 
    {
	const tree_record& T = sample.trees[i];

	// for each INTERNAL partition in the next tree
	for(int b=0;b<L;b++) {
	    count_and_length& cl = leaf_branch_records[b]->second;
	    cl.length += T.branch_lengths[b];
	}

	// for each INTERNAL partition in the next tree
	dynamic_bitset<> partition(names.size());
	for(int b=0;b<T.partitions.size();b++) 
	{
	    partition = T.partitions[b];
	    if (not partition[0]) partition.flip();

	    // Look up record for this partition
	    container_t::iterator record = counts.find(partition);
	    if (record == counts.end()) {
		counts.insert(container_t::value_type(partition,count_and_length()));
		record = counts.find(partition);          // FIXME - we are doing the lookup twice
		assert(record != counts.end());
	    }

	    // Increment the count and add in the new length
	    count_and_length& cl = record->second;
	    cl.count++;
	    cl.length += T.branch_lengths[L+b];
	}
    }

    for(container_t::iterator r = counts.begin();r != counts.end();r++) 
    {
	count_and_length& cl = r->second;
	cl.length /= cl.count;
    }

    return counts;
}

vector<pair<partition,unsigned> >
get_Ml_partitions_and_counts(const tree_sample& sample,double l,const dynamic_bitset<>&  mask) 
{
    // find the first bit
    int first = mask.find_first();
    assert(first >= 0);

    if (l <= 0.0)
	throw myexception()<<"Consensus level must be > 0.0";
    if (l > 1.0)
	throw myexception()<<"Consensus level must be <= 1.0";

    // use a sorted list of <partition,count>, sorted by partition.
    typedef map<dynamic_bitset<>,p_count> container_t;
    container_t counts;

    // use a linked list of pointers to <partition,count> records.
    list<container_t::iterator> majority;

    vector<string> names = sample.names();

    unsigned count = 0;

    for(int i=0;i<sample.trees.size();i++) 
    {
	const vector<dynamic_bitset<> >& T = sample.trees[i].partitions;

	unsigned min_old = std::min(1+(unsigned)(l*count),count);

	count ++;
	unsigned min_new = std::min(1+(unsigned)(l*count),count);

	// for each partition in the next tree
	dynamic_bitset<> partition(names.size());
	for(int b=0;b<T.size();b++) 
	{
	    partition = T[b];

	    if (not partition[first])
		partition.flip();

	    partition &= mask;

	    // Look up record for this partition
	    container_t::iterator record = counts.find(partition);
	    if (record == counts.end()) {
		counts.insert(container_t::value_type(partition,p_count()));
		record = counts.find(partition);
		assert(record != counts.end());
	    }

	    // FIXME - we are doing the lookup twice
	    p_count& pc = record->second;
	    int& C2 = pc.count;
	    int C1 = C2;
	    if (pc.last_tree != i) {
		pc.last_tree=i;
		C2 ++;
	    }
      
	    // add the partition if it wasn't good before, but is now
	    if ((C1==0 or C1<min_old) and C2 >= min_new)
		majority.push_back(record);
	}


	// for partition in the majority tree
	typedef list<container_t::iterator>::iterator iterator_t;
	for(iterator_t p = majority.begin();p != majority.end();) {
	    if ((*p)->second.count < min_new) {
		iterator_t old = p;
		p++;
		majority.erase(old);
	    }
	    else
		p++;
	}
    }

    vector<pair<partition,unsigned> > partitions;
    partitions.reserve( 2*names.size() );
    for(auto p : majority)
    {
	partition pi(p->first, mask);
	unsigned p_count = p->second.count;

	if (valid(pi))
	    partitions.push_back(pair<partition,unsigned>(pi,p_count));
    }

    return partitions;
}


vector<pair<partition,unsigned> > 
get_Ml_partitions_and_counts(const tree_sample& sample,double l) 
{
    dynamic_bitset<> mask(sample.names().size());
    mask.flip();
    return get_Ml_partitions_and_counts(sample,l,mask);
}

vector<partition> 
remove_counts(const vector<pair<partition,unsigned> >& partitions_and_counts) 
{
    vector<partition> partitions;
    for(int i=0;i<partitions_and_counts.size();i++)
	partitions.push_back(partitions_and_counts[i].first);

    return partitions;
}


vector<partition>
get_Ml_partitions(const tree_sample& sample,double l) 
{
    return remove_counts(get_Ml_partitions_and_counts(sample,l));
}

void add_unique(list<dynamic_bitset<> >& masks,const list<dynamic_bitset<> >& old_masks,
		const dynamic_bitset<>& mask) 
{
    // don't add the mask unless contains internal partitions (it could be all 0)
    if (mask.count() < 4) return;

    // don't add the mask if we already have that mask
    for(const auto& m: old_masks)
	if (m == mask) return;

    for(const auto& m: masks)
	if (m == mask) return;

    // otherwise, add the mask
    masks.push_front(mask);
}

void add_unique(list<dynamic_bitset<> >& masks,const list<dynamic_bitset<> >& old_masks,
		const list<dynamic_bitset<> >& new_masks)
{
    for(const auto& mask: new_masks)
	add_unique(masks, old_masks, mask);
}


void add_unique(list<dynamic_bitset<> >& masks,const dynamic_bitset<>& mask) 
{
    return add_unique(masks,list<dynamic_bitset<> >(),mask);
}


/// find out which splits imply the partial-splits, prefering the most likely.
vector<int> match(const vector<pair<partition,unsigned> >& full_splits,
		  const vector<pair<partition,unsigned> >& partial_splits)
{
    vector<int> m(partial_splits.size(),-1);

    for(int i=0;i<partial_splits.size();i++)
	for(int j=0;j<full_splits.size();j++) 
	{
	    // things that imply us cannot have a higher probability
	    if (full_splits[j].second > partial_splits[i].second)
		continue;

	    // skip this possible parent if it isn't as good as one we've found so far.
	    if (m[i] == -1 or full_splits[j].second > full_splits[m[i]].second)
	    {
		if (implies(full_splits[j].first,partial_splits[i].first))
		    m[i] = j;
	    }
	}

    return m;
}


// also construct list "who is implied by whom"

// consider only pulling out combinations of branches pointing to the same node

vector<pair<partition,unsigned> > 
get_Ml_sub_partitions_and_counts(const tree_sample& sample,double l,const dynamic_bitset<>& mask,
				 double min_rooting,int depth) 
{
    // get list of branches to consider cutting
    //   FIXME - consider 4n-12 most probable partitions, here?
    //         - Perhaps NOT, though.
    vector<partition> partitions_c50 = get_Ml_partitions(sample, 0.5);
    SequenceTree c50 = get_mf_tree(sample.names(),partitions_c50);
    vector<const_branchview> branches = branches_from_leaves(c50);  

    // construct unit masks
    // - unit masks are masks that come directly from a supported branch (full, or partial)
    list< dynamic_bitset<> > unit_masks;
    for(int b=0;b<branches.size();b++)
	add_unique(unit_masks, mask & branch_partition(c50,branches[b]) );

    // construct beginning masks
    list<dynamic_bitset<> > new_masks = unit_masks;
    list<dynamic_bitset<> > masks;

    // start collecting splits at M[l]
    vector<pair<partition,unsigned> > splits = get_Ml_partitions_and_counts(sample,l,mask);

    // any good mask should be combined w/ other good masks
    list<dynamic_bitset<> > good_masks;
    for(int iterations=0;not new_masks.empty();iterations++)
    {
	vector<pair<partition,unsigned> > full_splits = splits;

	if (log_verbose) cerr<<"iteration: "<<iterations<<"   depth: "<<depth<<"   new_masks: "<<new_masks.size()<<endl;
	list<dynamic_bitset<> > new_good_masks;
	list<dynamic_bitset<> > new_unit_masks;

	// get sub-splits for each mask
	for(const auto& mask: new_masks)
	{
	    // get sub-splits of mask
	    vector<pair<partition,unsigned> > partial_splits = get_Ml_partitions_and_counts(sample,l,mask);
    
	    // match up sub-splits and full splits
	    // FIXME - aren't we RE-doing a lot of work, here?
	    vector<int> parents = match(full_splits,partial_splits);

	    // check for splits with increased support when mask is unplugged
	    double rooting=1.0;
	    for(int i=0;i<partial_splits.size();i++) 
	    {
		if (not informative(partial_splits[i].first))
		    continue;
	    
		double r = 1;
		if (parents[i] == -1) {
		    r = (l*sample.size())/double(partial_splits[i].second);
		}
		else {
		    r = full_splits[parents[i]].second/double(partial_splits[i].second);
		    assert(r <= 1.0);
		}

		double OD = statistics::odds(partial_splits[i].second-5,sample.size(),10);

		// actually, considering bad rooting of low-probability edges may be a better (or alternate)
		// strategy to unplugging edges that are only slightly bad.

		// Determination of rooting probabilities seems to have the largest effect on computation time
		//  - thus, in the long run, new_good_masks has a larger effect than new_unit_masks.
		//  - actually, this makes kind of makes sense...
		//    + new_unit_masks can add splits they reveal under fairly weak conditions.
		//    + however, unless a new unit mask ends up being a good_mask, it won't trigger the quadratic behavior.

		// What happens when we consider unplugging ratios for branches (now) supported at level l<0.5?
		if (r < min_rooting and OD > 0.5) {
		    add_unique(new_unit_masks,unit_masks,partial_splits[i].first.group1);
		    add_unique(new_unit_masks,unit_masks,partial_splits[i].first.group2);
		    rooting = std::min(rooting,r);
		}

		// Store the new sub-splits we found
		if (r < 0.999 or 
		    (parents[i] != -1 and statistics::odds_ratio(partial_splits[i].second,
								 full_splits[parents[i]].second,
								 sample.size(),
								 10) > 1.1)
		    )
		    splits.push_back(partial_splits[i]);
	    }

	    // check if any of our branches make this branch badly rooted
	    if (rooting < min_rooting)
		new_good_masks.push_front(mask);
	}

	if (log_verbose) cerr<<"new unit_masks = "<<new_unit_masks.size()<<endl;

	// 1. masks += new_masks
	add_unique(masks, {}, new_masks);

	// 2. good_masks += new_good_masks
	add_unique(good_masks, {}, new_good_masks);

	// 3. unit_masks += new_unit_masks
	add_unique(unit_masks, {}, new_unit_masks);

	if (depth == 0) break;

	// 4. good_masks += (good_masks + new_good_masks) * new_good_masks
	// Rationale: pull out every combination of masks known to be "good"
	new_masks = new_unit_masks;
	for(const auto& i: new_good_masks)
	    for(const auto& j: good_masks)
		if (i != j)
		    add_unique(new_masks,masks,i & j);

	//cerr<<"   new good masks = "<<new_good_masks.size()<<"    new unit masks = "<<new_unit_masks.size()<<endl;
	//cerr<<"       good masks = "<<good_masks.size()    <<"       total masks = "<<masks.size()<<"       found = "<<splits.size()<<endl;

    }

    return splits;
}

vector<pair<partition,unsigned> > 
get_Ml_sub_partitions_and_counts(const tree_sample& sample,double l,double min_rooting,int depth) 
{
    dynamic_bitset<> mask(sample.names().size());
    mask.flip();
    return get_Ml_sub_partitions_and_counts(sample,l,mask,min_rooting,depth);
  
}

vector<partition> 
get_Ml_sub_partitions(const tree_sample& sample,double l,double min_rooting,int depth) 
{
    return remove_counts(get_Ml_sub_partitions_and_counts(sample,l,min_rooting,depth));
}


