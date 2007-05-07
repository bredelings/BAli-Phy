#include <map>
#include <fstream>
#include "tree-dist.H"
#include "tree-util.H"
#include "rng.H"
#include "statistics.H"
#include "mytypes.H"

using std::vector;
using std::list;
using std::valarray;

using std::map;
using std::pair;

using std::string;
using std::endl;
using std::cerr;
using std::cout;

valarray<bool> group_from_names(const vector<string>& names,const vector<string>& subset)
{
  assert(subset.size() <= names.size());

  valarray<bool> group(false,names.size());

  for(int i=0; i<subset.size(); i++) {
    if (includes(names,subset[i]))
      group[find_index(names,subset[i])] = true;
    else
      throw myexception()<<"Can't find taxon '"<<subset[i]<<"' in taxa set.";
  }

  return group;
}

Partition partition_from_branch(const SequenceTree& T,int b) {
  valarray<bool> group(T.n_leaves());
  group = T.partition(b);

  return Partition(T.get_sequences(),group);
}


Partition full_partition_from_names(const vector<string>& names, const vector<string>& names1) 
{
  valarray<bool> group1 = group_from_names(names,names1);

  return Partition(names,group1);
}


Partition partition_from_names(const vector<string>& names, const vector<string>& names1,
			       const vector<string>& names2)
{
  valarray<bool> group1 = group_from_names(names,names1);
  valarray<bool> group2 = group_from_names(names,names2);

  return Partition(names,group1,group1 or group2);
}

vector<Partition> partitions_from_tree(const SequenceTree& T) 
{
  vector<Partition> partitions;

  for(int b=T.n_leafbranches();b<T.n_branches();b++)
    partitions.push_back(partition_from_branch(T,b));

  return partitions;
}


bool Partition::full() const {
  for(int i=0;i<names.size();i++)
    if (not group1[i] and not group2[i])
      return false;
  return true;
}

Partition& Partition::flip() {
  std::swap(group1,group2);
  return *this;
}

Partition Partition::reverse() const {
  Partition p2 = *this;
  p2.flip();
  return p2;
}

Partition::Partition(const Partition& p,const valarray<bool>& mask)
  :names(p.names),
   group1(p.group1 and mask),
   group2(p.group2 and mask)
{
  assert(mask.size() == p.group1.size());
  assert(empty(group1 and group2));
}

Partition::Partition(const valarray<bool>& g) 
  :group1(g),group2(not g)
{ 
  assert(empty(group1 and group2));
}

Partition::Partition(const valarray<bool>& g,const valarray<bool>& mask) 
  :group1(g and mask),group2((not g) and mask)
{
  assert(g.size() == mask.size());
  assert(empty(group1 and group2));
}

Partition::Partition(const vector<string>& n,const valarray<bool>& g) 
  :names(n),group1(g),group2(not g)
{
  assert(n.size() == g.size());
  assert(empty(group1 and group2));
}

Partition::Partition(const vector<string>& n,const valarray<bool>& g,const valarray<bool>& mask) 
  :names(n),group1(g and mask),group2((not g) and mask)
{
  assert(n.size() == g.size());
  assert(g.size() == mask.size());
  assert(empty(group1 and not group1));
  assert(empty(group1 and group2));
}

vector< vector<string> > parse_partition(const string& line)
{
  vector<string> all_names = split(line,' ');

  vector< vector<string> > names(3);

  int group = 0;
  for(int i=0; i< all_names.size(); i++) 
  {
    if (all_names[i] == "|") {
      assert(group == 0);
      group++; 
    }
    else if (all_names[i] == "[") {
      assert(group == 1);
      group++;
    }
    else if (all_names[i] == "]") {
      assert(group == 2);
      group++;
    }
    else if (all_names[i].size())
      names[group].push_back(all_names[i]);
  }

  return names;
}


Partition::Partition(const string& line) 
{
  vector<string> all_names = split(line,' ');

  names.clear();
  vector< vector<string> > name_groups = parse_partition(line);
  for(int i=0;i<name_groups.size();i++)
    names.insert(names.end(), name_groups[i].begin(), name_groups[i].end());

  std::sort(names.begin(),names.end());
  group1.resize(names.size());
  group2.resize(names.size());
  
  group1 = group_from_names(names,name_groups[0]);
  group2 = group_from_names(names,name_groups[1]);
  assert(empty(group1 and group2));
}

Partition::Partition(const vector<string>& n,const string& line) 
  :names(n),group1(false,n.size()),group2(false,n.size())
{
  vector< vector<string> > name_groups = parse_partition(line);

  group1 = group_from_names(names,name_groups[0]);
  group2 = group_from_names(names,name_groups[1]);

  assert(empty(group1 and group2));
}

bool informative(const Partition& p) {
  return n_elements(p.group1) > 1 and n_elements(p.group2) > 1;
}

bool valid(const Partition& p) {
  return n_elements(p.group1) > 0 and n_elements(p.group2) > 0;
}

RootedSequenceTree standardized(const string& t) 
{
  RootedSequenceTree T;
  T.parse(t);

  if (T.root().degree() == 2)
    T.remove_node_from_branch(T.root());

  if (has_sub_branches(T))
    throw myexception()<<"Tree has node of degree 2";

  standardize(T);
  return T;
}

template <class T>
struct array_order
{
  const vector<T>& array;
  bool operator()(int i,int j) const {return std::less<T>()(array[i],array[j]);}

  array_order(const vector<T>& n):array(n) {}
};

vector<int> compute_sorted_mapping(const vector<string>& names)
{
  vector<int> mapping(names.size());
  for(int i=0;i<names.size();i++)
    mapping[i] = i;

  std::sort(mapping.begin(),mapping.end(),array_order<string>(names));

  return invert(mapping);
}



  
//FIXME - return mapping of leaf nodes?  Of all nodes?
void standardize(SequenceTree& T) 
{
  vector<int> mapping = compute_sorted_mapping(T.get_sequences());

  T.standardize(mapping);
}

void standardize(RootedSequenceTree& T) {

  vector<int> mapping = compute_sorted_mapping(T.get_sequences());

  T.standardize(mapping);

  T.reroot(T.directed_branch(0).target());
}

std::ostream& operator<<(std::ostream& o, const Partition& P) 
{
  assert(empty(P.group1 and P.group2));

  if (n_elements(P.group1)<n_elements(P.group2)) {
    for(int i=0;i<P.size();i++)
      if (P.group1[i]) o<<P.names[i]<<" ";
    
    o<<"| ";
    
    for(int i=0;i<P.size();i++)
      if (P.group2[i]) o<<P.names[i]<<" ";
  }
  else {
    for(int i=0;i<P.size();i++)
      if (P.group2[i]) o<<P.names[i]<<" ";
    
    o<<"| ";
    
    for(int i=0;i<P.size();i++)
      if (P.group1[i]) o<<P.names[i]<<" ";
  }
  std::valarray<bool> rmask = not(P.group1 or P.group2);
  if (statistics::count(rmask)) {
    o<<" [ ";
    for(int i=0;i<P.size();i++) {
      if (rmask[i]) o<<P.names[i]<<" ";
    }
    o<<"]";
  }
  return o;
}

SequenceTree get_mf_tree(const std::vector<std::string>& names,
			 const std::vector<Partition>& partitions) 
{
  SequenceTree T = star_tree(names);

  int i=0;
  try {
    for(;i<partitions.size();i++)
      T.induce_partition(partitions[i].group1);
  }
  catch(...) {
    throw myexception()<<"Partition ("<<partitions[i]<<") conflicts with tree "<<T;
  }

  for(int i=0;i<T.n_branches();i++)
    T.branch(i).set_length(1.0);

  return T;
}

SequenceTree get_mf_tree(const std::vector<std::string>& names,
			 const std::vector<std::valarray<bool> >& partitions) 
{
  SequenceTree T = star_tree(names);

  for(int i=0;i<partitions.size();i++)
    T.induce_partition(partitions[i]);

  for(int i=0;i<T.n_branches();i++)
    T.branch(i).set_length(1.0);

  return T;
}

bool operator==(const Partition& p1, const Partition& p2) {
  return 
    (p1.names == p2.names) and 
    (
     (equal(p1.group1,p2.group1) and equal(p1.group2,p2.group2)) or
     (equal(p1.group1,p2.group2) and equal(p1.group2,p2.group1))
    );
}

bool consistent(const Partition& p1, const Partition& p2) {
  if (not intersect(p1.group1,p2.group1)) return true;
  if (not intersect(p1.group1,p2.group2)) return true;

  if (not intersect(p1.group2,p2.group1)) return true;
  if (not intersect(p1.group2,p2.group2)) return true;

  return false;
}


/// Does the grouping of all nodes bm, imply *this?
bool implies(const Partition& p1, const Partition& p2) 
{
  if (is_subset(p2.group1,p1.group1) and is_subset(p2.group2,p1.group2)) return true;

  if (is_subset(p2.group2,p1.group1) and is_subset(p2.group1,p1.group2)) return true;

  return false;
}

/// Does any branch in T imply the partition p?
bool implies(const SequenceTree& T,const Partition& p) {
  bool result = false;
  for(int b=0;b<T.n_branches() and not result;b++) {
    valarray<bool> bp = branch_partition(T,b);

    if (implies(bp,p)) return true;
  }
  return false;
}

// Remove partitions we imply.  Add only if not implied. Return true if we expanded the coverage.
vector<Partition> unimplied_partitions(const vector<Partition>& partitions,const vector<Partition>& delta) 
{
  vector<Partition> unimplied;

  for(int i=0;i<delta.size();i++)
    if (not implies(partitions,delta[i]))
      unimplied.push_back(delta[i]);

  return unimplied;
}

// Remove partitions we imply.  Add only if not implied. Return true if we expanded the coverage.
bool merge_partition(vector<Partition>& partitions,const Partition& delta) 
{
  if (implies(partitions,delta)) 
    return false;

  for(int i=partitions.size()-1;i>=0;i--)
    if (implies(delta,partitions[i]))
      partitions.erase(partitions.begin()+i);

  partitions.push_back(delta);

  return true;
}

// Remove partitions we imply.  Add only if not implied. Return true if we expanded the coverage.
bool merge_partitions(vector<Partition>& partitions,const vector<Partition>& delta) 
{
  bool changed=false;

  for(int i=0;i<delta.size();i++) 
    if (merge_partition(partitions,delta[i]))
      changed = true;

  return changed;
}

int which_partition(const SequenceTree& T, const Partition& p) {
  for(int b=0; b<T.n_branches(); b++) {
    valarray<bool> bp = branch_partition(T,b);
    if( implies(bp,p) )
      return b;
  }
  throw myexception(string("Partition not found in tree!"));
}

SequenceTree tree_sample::T(int i) const {
  return get_mf_tree(leaf_names,topologies[i].partitions);
}

valarray<bool> tree_sample::support(const string& t) const 
{
  typeof(index.begin()) here = index.find(t);

  if (here == index.end())
    return valarray<bool>(false,size());

  int t_i = index[t];

  valarray<bool> result(size());

  for(int i=0;i<result.size();i++)
    result[i] = (t_i == which_topology[i]);

  return result;
}

valarray<bool> tree_sample::support(const Partition& p) const 
{
  valarray<bool> result(size());

  for(int i=0;i<result.size();i++) 
  {
    // Get a tree with the same topology
    const vector<valarray<bool> > & T = topologies[ which_topology[i] ].partitions;
    
    result[i] = implies(T,p);
  }
  return result;
}

valarray<bool> tree_sample::support(const vector<Partition>& partitions) const 
{
  valarray<bool> result(size());

  for(int i=0;i<result.size();i++) 
  {
    // Get a tree with the same topology
    const vector<valarray<bool> >& T = topologies[ which_topology[i] ].partitions;
    
    result[i] = implies(T,partitions);
  }
  return result;
}

unsigned tree_sample::count(const Partition& P) const 
{
  unsigned count=0;
  for(int t=0;t<topologies.size();t++) 
    if (implies(topologies[t].partitions,P))
	count += topologies[t].count;
   
  return count;
}

unsigned tree_sample::count(const vector<Partition>& partitions) const 
{
  unsigned count=0;
  for(int t=0;t<topologies.size();t++) {
    if (implies(topologies[t].partitions,partitions))
      count += topologies[t].count;
  }
   
  return count;
}

double tree_sample::PP(const Partition& P) const 
{
  return double(count(P))/size();
}

double tree_sample::PP(const vector<Partition>& partitions) const 
{
  return double(count(partitions))/size();
}

struct ordering {
  const vector<tree_sample::topology_record>& v;

  // decreasing order of count
  bool operator()(int i,int j) {return v[i].count > v[j].count;}
  
  ordering(const vector<tree_sample::topology_record>& v_):v(v_) {}
};


tree_sample::topology_record::topology_record(const SequenceTree& T,
					      const string& s)
  :topology(s),
   partitions(T.n_branches(),valarray<bool>(T.n_leaves())),
   count(0)
{ 
  for(int i=0;i<T.n_branches();i++)
    partitions[i] = T.partition(i);
}


tree_sample::tree_sample(std::istream& file,int skip,int max,int subsample) 
{
  int lines=0;
  string line;
  while(getline(file,line)) 
  {
    // don't start if we haven't skipped enough trees
    if (lines++ < skip) continue;

    // skip trees unless they are a multiple of 'subsample'
    if ((lines-skip) % subsample != 0) continue;

    // quit if we've read in 'max' trees
    if (max >= 0 and size() == max) break;

    //--------- Count how many of each topology -----------//
    RootedSequenceTree T;
    try {
      // This should make all the branch & node numbers the same if the topology is the same
      T = standardized(line);
    }
    catch (std::exception& e) {
      cerr<<"Exception: "<<e.what()<<endl;
      cerr<<" Quitting read of tree file"<<endl;
      break;
    }

    if (not leaf_names.size()) leaf_names = T.get_sequences();

    // This should be a standard string representation
    string t = T.write(false);
      
    // If it hasn't been seen before, insert it
    if (index.find(t) == index.end()) {
      topologies.push_back(topology_record(T,t));

      index[t] = topologies.size()-1;              // add to map of  (topology->index)
    }
      
    //----------- Add tree to distribution -------------//
    int i = index[t];
    which_topology.push_back(i);
    topologies[i].count++;
  }

  if (size() == 0)
    throw myexception()<<"No trees were read in!";
  
  cout<<"# n_trees = "<<size()<<"   n_topologies = "<<topologies.size()<<endl;
    
  //---------------  Sort topologies by count  ---------------//
  order.resize(topologies.size());
  for(int i=0;i<order.size();i++)
    order[i] = i;
  
  sort(order.begin(),order.end(),ordering(topologies));
}


struct p_count {
  int count;
  int last_tree;
  p_count(): count(0),last_tree(-1) {}
};

vector<pair<Partition,unsigned> > 
get_Ml_partitions_and_counts(const tree_sample& sample,double l,const valarray<bool>&  mask) 
{
  // find the first bit
  int first=0;
  while(first<mask.size() and not mask[first])
    first++;
  assert(first < mask.size());

  if (l <= 0.0)
    throw myexception()<<"Consensus level must be > 0.0";
  if (l > 1.0)
    throw myexception()<<"Consensus level must be <= 1.0";

  // use a sorted list of <partition,count>, sorted by partition.
  typedef map<valarray<bool>,p_count,compare_complete_partitions > container_t;
  container_t counts;

  // use a linked list of pointers to <partition,count> records.
  list<container_t::iterator> majority;

  vector<string> names = sample.names();

  unsigned count = 0;

  for(int i=0;i<sample.topologies.size();i++) 
  {
    const vector<valarray<bool> >& T = sample.topologies[i].partitions;

    unsigned delta = sample.topologies[i].count;

    unsigned min_old = std::min(1+(unsigned)(l*count),count);

    count += delta;
    unsigned min_new = std::min(1+(unsigned)(l*count),count);

    // for each partition in the next tree
    std::valarray<bool> partition(names.size());
    for(int b=0;b<T.size();b++) 
    {
      partition = T[b];
      if (not partition[first])
	partition = (not partition) and mask;
      else
	partition = partition and mask;

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
	C2 += delta;
      }
      
      // add the partition if it wasn't good before, but is now
      if ((C1==0 or C1<min_old) and C2 >= min_new)
	majority.push_back(record);
    }


    // for partition in the majority tree
    for(typeof(majority.begin()) p = majority.begin();p != majority.end();) {
      if ((*p)->second.count < min_new) {
	typeof(p) old = p;
	p++;
	majority.erase(old);
      }
      else
	p++;
    }
  }

  vector<pair<Partition,unsigned> > partitions;
  partitions.reserve( 2*names.size() );
  for(typeof(majority.begin()) p = majority.begin();p != majority.end();p++) {
    const valarray<bool>& partition =(*p)->first;
 
    Partition pi(names,partition,mask);
    unsigned p_count = (*p)->second.count;

    if (valid(pi))
      partitions.push_back(pair<Partition,unsigned>(pi,p_count));
  }

  return partitions;
}


vector<pair<Partition,unsigned> > 
get_Ml_partitions_and_counts(const tree_sample& sample,double l) 
{
  valarray<bool> mask(true,sample.names().size());
  return get_Ml_partitions_and_counts(sample,l,mask);
}

vector<Partition> 
remove_counts(const vector<pair<Partition,unsigned> >& partitions_and_counts) 
{
  vector<Partition> partitions;
  for(int i=0;i<partitions_and_counts.size();i++)
    partitions.push_back(partitions_and_counts[i].first);

  return partitions;
}


vector<Partition>
get_Ml_partitions(const tree_sample& sample,double l) 
{
  return remove_counts(get_Ml_partitions_and_counts(sample,l));
}

void add_unique(list<valarray<bool> >& masks,const list<valarray<bool> >& old_masks,
		const valarray<bool>& mask) 
{
  // don't add the mask unless contains internal partitions (it could be all 0)
  if (n_elements(mask) < 4) return;

  // don't add the mask if we already have that mask
  foreach(m,masks)
    if (equal(*m,mask)) return;

  foreach(m,old_masks)
    if (equal(*m,mask)) return;

  // otherwise, add the mask
  masks.push_front(mask);
}


void add_unique(list<valarray<bool> >& masks,const valarray<bool>& mask) 
{
  return add_unique(masks,list<valarray<bool> >(),mask);
}


/// find out which partitions imply the sub-partitions, prefering the most likely.
vector<int> match(const vector<pair<Partition,unsigned> >& full_partitions,
		  const vector<pair<Partition,unsigned> >& sub_partitions)
{
  vector<int> m(sub_partitions.size(),-1);

  for(int i=0;i<sub_partitions.size();i++) 
    for(int j=0;j<full_partitions.size();j++) 
      if (implies(full_partitions[j].first,sub_partitions[i].first))
        if (m[i] == -1)
	  m[i] = j;
	else if (full_partitions[j].second > full_partitions[m[i]].second)
	  m[i] = j;

  return m;
}


// also construct list "who is implied by whom"

//consider only pulling out combinations of branches pointing to the same node

vector<pair<Partition,unsigned> > 
get_Ml_sub_partitions_and_counts(const tree_sample& sample,double l,const valarray<bool>& mask,
				 double min_rooting,int depth) 
{
  // get list of branches to consider cutting
  vector<Partition> partitions_c50 = get_Ml_partitions(sample, 0.5);
  SequenceTree c50 = get_mf_tree(sample.names(),partitions_c50);
  vector<const_branchview> branches = branches_from_leaves(c50);  

  // construct unit masks
  list< valarray<bool> > unit_masks;
  for(int b=0;b<branches.size();b++)
    add_unique(unit_masks, mask and branch_partition(c50,branches[b]) );

  // construct beginning masks
  list<valarray<bool> > masks = unit_masks;
  list<valarray<bool> > old_masks = unit_masks;

  // start collecting partitions at M[l]
  vector<pair<Partition,unsigned> > partitions = get_Ml_partitions_and_counts(sample,l,mask);

  // any good mask should be combined w/ other good masks
  list<valarray<bool> > good_masks;
  for(int iterations=0;not masks.empty();iterations++)
  {
    vector<pair<Partition,unsigned> > full_partitions = partitions;

    //cerr<<"iteration: "<<iterations<<"   depth: "<<depth<<"   masks: "<<masks.size()<<endl;    
    list<valarray<bool> > new_good_masks;
    list<valarray<bool> > new_unit_masks;

    // get sub-partitions for each mask
    vector<Partition> all_sub_partitions;
    foreach(m,masks) 
    {
      // get sub-partitions of *m 
      vector<pair<Partition,unsigned> > sub_partitions = get_Ml_partitions_and_counts(sample,l,*m);
    
      // match up sub-partitions and full partitions
      vector<int> parents = match(full_partitions,sub_partitions);

      // check for partitions with increased support when *m is unplugged
      double rooting=1.0;
      for(int i=0;i<sub_partitions.size();i++) 
      {
	if (not informative(sub_partitions[i].first))
	  continue;
	    
	double r = 1;
	if (parents[i] == -1) {
	  r = 0;
	}
	else {
	  r = full_partitions[parents[i]].second/double(sub_partitions[i].second);
	  assert(r <= 1.0);
	}
	if (r < min_rooting) {
	  add_unique(new_unit_masks,unit_masks,sub_partitions[i].first.group1);
	  add_unique(new_unit_masks,unit_masks,sub_partitions[i].first.group2);
	}

	rooting = std::min(rooting,r);

	// Store the new sub-partitions we found
	if (r < 0.999 or 
	    (parents[i] != -1 and statistics::odds_ratio(sub_partitions[i].second,
							 full_partitions[parents[i]].second,
							 sample.size(),
							 10) > 1.1)
	    )
	  partitions.push_back(sub_partitions[i]);
      }

      // check if any of our branches make this branch badly rooted
      if (rooting < min_rooting)
	new_good_masks.push_front(*m);
    }

    old_masks.insert(old_masks.end(),masks.begin(),masks.end());
    masks.clear();
    masks = new_unit_masks;

    // FIXME!! We need to find a way to consider only masks which are
    // 'close' togther - defined in terms of the number and support 
    // of branches that are in the between them.

    // fixme - do a convolution here - e.g. 2->1+1 3->1+2 4 ->1+3,2+2
    // otherwise we do 1  - 1,2 - 1,2,3,4 - 1,2,3,4,5,6,7,8
    good_masks.insert(good_masks.end(),new_good_masks.begin(),new_good_masks.end());
    foreach(i,new_good_masks)
      foreach(j,good_masks)
        if (not equal(*i,*j))
          add_unique(masks,old_masks,*i and *j);

    // what will we operate on next time? 
    // - perhaps change to look at pairs of branches connected to a node
    // - perhaps depth 3 could be pairs of branches of distance 1
    // - should I use the M[0.5] tree here, or the M[l] tree?
    if (iterations < depth-1) {

      foreach(i,old_masks)
	foreach(j,unit_masks)
	  add_unique(masks,old_masks,*i and *j);

      // old good_masks were considered with unit_masks last_time
      foreach(i,new_good_masks)
      	foreach(j,unit_masks)
      	  add_unique(masks,old_masks,*i and *j);

      // old good_masks were considered with unit_masks already
      foreach(i,old_masks)
	foreach(j,new_good_masks)
	  add_unique(masks,old_masks,*i and *j);
    }
  }

  return partitions;
}

vector<pair<Partition,unsigned> > 
get_Ml_sub_partitions_and_counts(const tree_sample& sample,double l,double min_rooting,int depth) 
{
  valarray<bool> mask(true,sample.names().size());
  return get_Ml_sub_partitions_and_counts(sample,l,mask,min_rooting,depth);
  
}

vector<Partition> 
get_Ml_sub_partitions(const tree_sample& sample,double l,double min_rooting,int depth) 
{
  return remove_counts(get_Ml_sub_partitions_and_counts(sample,l,min_rooting,depth));
}


bool p_equal(const vector<Partition>& P1,const vector<Partition>& P2) 
{
  if (P1.size() != P2.size()) return false;

  for(int i=0;i<P1.size();i++)
    if (not includes(P2,P1[i]))
      return false;

  return true;
}

bool p_contains(const vector<vector<Partition> >& partitions, const vector<Partition>& P)
{
  for(int i=0;i<partitions.size();i++)
    if (p_equal(P,partitions[i]))
      return true;

  return false;
}

void load_partitions(const string& filename, vector<vector<Partition> >& partitions) 
{
  std::ifstream file(filename.c_str());

  string line;
  while(file) {
    vector<Partition> P;

    while(getline(file,line) and line.size()) {
      if (line[0] == '(') {
	SequenceTree T = standardized(line);
	vector<Partition> TP = partitions_from_tree(T);
	P.insert(P.end(),TP.begin(),TP.end());
      }
      else
	P.push_back(Partition(line));
    }

    if (P.size() and not p_contains(partitions,P))
      partitions.push_back(P);
  }
}

void write_partitions(std::ostream& o,const vector<Partition>& partitions)
{
  vector<Partition> full;
  vector<Partition> sub;
  for(int i=0;i<partitions.size();i++)
    if (partitions[i].full())
      full.push_back(partitions[i]);
    else
      sub.push_back(partitions[i]);

  if (full.size()) {
    SequenceTree consensus = get_mf_tree(partitions[0].names,full);
    o<<consensus.write(false)<<endl;
  }

  for(int i=0;i<sub.size();i++)
    o<<sub[i]<<endl;
}

bool is_strict_subset(const valarray<bool>& v1,const valarray<bool>& v2) {
  return is_subset(v1,v2) and not equal(v1,v2);
}

bool partition_wanders_over(const Partition& p1,const Partition& p2)
{
  return is_subset(p2.group1,p1.group2) and is_subset(p2.group2,p1.group2);
}

bool partition_less_than(const Partition& p1,const Partition& p2)
{
  return 
    is_strict_subset(p1.group1,p2.group1) and 
    is_strict_subset(p2.group2,p1.group2);
}

bool sub_conflict(Partition p1,Partition p2)
{
  if (not intersect(p1.mask(),p2.mask()))
    return false;

  if (partition_less_than(p1,p2) or partition_less_than(p1,p2.reverse()) or
      partition_less_than(p1.reverse(),p2) or partition_less_than(p1.reverse(),p2.reverse()))
    return false;

  if (partition_wanders_over(p1,p2) or partition_wanders_over(p1.reverse(),p2) or
      partition_wanders_over(p2,p1) or partition_wanders_over(p2.reverse(),p1))
    return false;

  return true;
}

bool is_leaf_partition(const Partition& p)
{
  return p.full() and (n_elements(p.group1) == 1 or n_elements(p.group2) == 1);
}

int get_n_conflicts(const ublas::matrix<int>& conflicts,
		    int n,
		    const vector<bool>& mask)
{
  assert(mask.size() == conflicts.size1());
  assert(mask.size() == conflicts.size2());

  int total = 0;
  for(int i=0;i<mask.size();i++)
    if (mask[i] and conflicts(n,i))
      total++;

  return total;
}

vector<bool> solve_conflicts(const ublas::matrix<int>& conflicts,
			     const ublas::matrix<int>& dominates,
			     vector<bool> invincible)
{
  const int N = invincible.size();
  vector<bool> survives(N,true);

  int n=0;
  for(int i=0;i<N;i++)
    if (invincible[i])
      n++;

  do {
    vector<int> n_conflicts(N,0);

    for(int i=0;i<N;i++)
      if (survives[i] and not invincible[i]) {
	n_conflicts[i]  = get_n_conflicts(conflicts,i,survives);
	n_conflicts[i] -= get_n_conflicts(dominates,i,survives);
	assert(n_conflicts[i] >= 0);
      }

    int die = argmax(n_conflicts);

    if (not n_conflicts[die]) break;

    survives[die] = false;

  } while(true);

  return survives;
}

vector<Partition> get_moveable_tree(vector<Partition> partitions)
{
  // remove partitions that are implied by other partitions
  for(int i=partitions.size()-1;i>=0;i--) 
  {
    bool found = false;
    for(int j=i-1;j>=0 and not found;j--)
      if (implies(partitions[j],partitions[i]))
	found = true;

    if (found)
      partitions.erase(partitions.begin() + i);
  }

  const int N = partitions.size();

  // create and zero conflict matrix
  ublas::matrix<int> conflict(N,N);
  ublas::matrix<int> dominates(N,N);

  for(int i=0;i<N;i++)
    for(int j=0;j<N;j++) {
      conflict(i,j) = 0;
      dominates(i,j) = 0;
    }

  for(int i=0;i<N;i++)
    for(int j=0;j<N;j++) 
      if (i!=j) {
	if (sub_conflict(partitions[i],partitions[j]))
	  conflict(i,j) = 1;
	if (conflict(i,j) and is_strict_subset(partitions[j].mask(),partitions[i].mask()))
	  dominates(i,j) = 1;
      }
  
  // we can't remove leaf partitions
  vector<bool> invincible(N,true);
  for(int i=0;i<N;i++)
    invincible[i] = partitions[i].full(); //is_leaf_partition(partitions[i]);


  vector<bool> solution = solve_conflicts(conflict,dominates,invincible);


  vector<Partition> moveable;
  for(int i=0;i<solution.size();i++)
    if (solution[i])
      moveable.push_back(partitions[i]);

  return moveable;
}


// there are actually a lot of things we might want to do
// * skip certain lines (e.g. in front)
// * stop at a certain line
// * report no more than X trees (compress the trees)
// * 

struct string_to_standardized_tree_op: public accumulator<string>
{
  accumulator<SequenceTree>& tree_op;

  void operator()(const std::string& line);

  string_to_standardized_tree_op(accumulator<SequenceTree>& o):tree_op(o) {};
};

void string_to_standardized_tree_op::operator()(const string& line)
{
  try {
    SequenceTree T = standardized(line);
    tree_op(T);
  }
  catch (std::exception& e) {
    std::cerr<<"Badly formed tree (ignored): "<<e.what()<<endl;
  }
}


void scan_trees(std::istream& file,int skip,int subsample,int max,
		accumulator<SequenceTree>& op)
{
  string_to_standardized_tree_op string_op(op);

  scan_lines(file,skip,subsample,max,string_op);

  op.finalize();
}
