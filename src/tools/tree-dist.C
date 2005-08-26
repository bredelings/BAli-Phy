#include <map>
#include "tree-dist.H"
#include "rng.H"
#include "statistics.H"

using std::vector;
using std::list;
using std::valarray;

using std::map;
using std::pair;

using std::string;
using std::endl;
using std::cerr;
using std::cout;

Partition partition_from_names(const vector<string>& allnames, const vector<string>& inames) 
{
  valarray<bool> pbits(false,allnames.size());

  for(int i=0; i<inames.size(); i++) {
    if (includes(allnames,inames[i]))
      pbits[find_index(allnames,inames[i])] = true;
    else
      throw myexception()<<"Can't find taxon '"<<inames[i]<<"' in taxa set.";
  }

  return Partition(allnames,pbits);
}


Partition::Partition(const Partition& p,const valarray<bool>& mask)
  :names(p.names),
   group1(p.group1 and mask),
   group2(p.group2 and mask)
{
  assert(mask.size() = p.group1.size());
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

bool informative(const Partition& p) {
  return n_elements(p.group1) > 1 and n_elements(p.group2) > 1;
}

SequenceTree standardized(const string& t) {
  SequenceTree T;
  T.parse(t);
  standardize(T);
  return T;
}
  
SequenceTree standardized(const string& t,const vector<string>& remove) {
  SequenceTree T;
  T.parse(t);
  standardize(T,remove);
  return T;
}


//FIXME - return mapping of leaf nodes?  Of all nodes?
void standardize(SequenceTree& T) {

  vector<string> names = T.get_sequences();
    
  std::sort(names.begin(),names.end());

  vector<int> mapping = compute_mapping(T.get_sequences(),names);

  T.standardize(mapping);
}

void standardize(SequenceTree& T,const vector<string>& remove) {
  for(int i=0;i<remove.size();i++)
    delete_node(T,remove[i]);
  standardize(T);
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

  for(int i=0;i<partitions.size();i++)
    T.induce_partition(partitions[i].group1);

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
bool implies(const Partition& p1, const Partition& p2) {
  if (implies(p1.group1,p2.group1) and implies(p1.group2,p2.group2)) return true;

  if (implies(p1.group2,p2.group1) and implies(p1.group1,p2.group2)) return true;

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

valarray<bool> tree_sample::supports_topology(const string& t) const 
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

valarray<bool> tree_sample::supports_partition(const Partition& P) const 
{
  valarray<bool> result(size());

  for(int i=0;i<result.size();i++) 
  {
    // Get a tree with the same topology
    const vector<valarray<bool> > & T = topologies[ which_topology[i] ].partitions;
    
    result[i] = implies(T,P);
  }
  return result;
}

valarray<bool> tree_sample::supports_partitions(const vector<Partition>& partitions) const 
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


tree_sample::tree_sample(std::istream& file,const vector<string>& remove,int skip,int max,int subsample) 
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
    SequenceTree T;
    try {
      // This should make all the branch & node numbers the same if the topology is the same
      T = standardized(line,remove);
    }
    catch (std::exception& e) {
      std::cerr<<"Exception: "<<e.what()<<endl;
      std::cerr<<" Quitting read of tree file"<<endl;
      break;
    }

    if (not leaf_names.size()) leaf_names = T.get_sequences();

    // This should be a standard string representation
    string t = RootedSequenceTree(T,T.directed_branch(0).target()).write(false);
      
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
  
  cout<<" n_trees = "<<size()<<"   n_topologies = "<<topologies.size()<<" topologies."<<endl;
    
  //---------------  Sort topologies by count  ---------------//
  order.resize(topologies.size());
  for(int i=0;i<order.size();i++)
    order[i] = i;
  
  sort(order.begin(),order.end(),ordering(topologies));
}


struct compare_complete_partitions {
  bool operator()(const valarray<bool>& p1,const valarray<bool>& p2) const {
    assert(p1.size() == p2.size());
    //    assert(p1[0]);
    //    assert(p2[0]);
    
    for(int i=0;i<p1.size();i++) {
      if (p2[i] and not p1[i])
	return true;
      if (p1[i] and not p2[i])
	return false;
    }
    return false;
  }
};

struct p_count {
  int count;
  int last_tree;
  p_count(): count(0),last_tree(-1) {}
};

#if (defined(__GNUC__) && (__GNUC__ > 4) )

#include <tr1/unordered_map>

namespace std { namespace tr1 {

  template<> struct hash<std::valarray<bool> > 
  {
    vector<int> constants;

    size_t operator()(const std::valarray<bool>& v) const 
    {
      size_t total=0;
      assert(constants.size() == v.size());
      
      for(int i=0;i<v.size();i++)
	if (v[i]) total += constants[i];
      return total;
    }
    
    hash(int n) 
      :constants(n)
    {
      for(int i=0;i<constants.size();i++)
	constants[i] = uniform_unsigned_long();
    }
  };

} } 

namespace std {

  template<>
  struct equal_to<valarray<bool> > {
    bool operator()(const std::valarray<bool>& v1,const std::valarray<bool>& v2) const {
      assert(v1.size() == v2.size());
      return ::equal(v1,v2);
    }
  };
}


// improve: we spend a lot of time creating and destroying the hash table

vector<Partition> get_Ml_partitions(const tree_sample& sample,double l,const valarray<bool>&  mask) 
{
  // find the first bit
  int first=0;
  while(first<mask.size() and not mask[first])
    first++;
  assert(first < mask.size());

  // make sure l is in range and handle l==1
  if (l < 0.5)
    throw myexception()<<"Consensus level for majority tree must be >= 0.5";
  if (l > 1.0)
    throw myexception()<<"Consensus level for majority tree must be <= 1.0";

  // use a sorted list of <partition,count>, sorted by partition.
  typedef std::tr1::unordered_map<valarray<bool>,p_count> container_t;
  int container_size = 16 * sample.size()*sample.topologies[0].T.n_branches();
  int n_leaves = sample.topologies[0].T.n_leaves();
  container_t counts(container_size,std::tr1::hash<valarray<bool> >(n_leaves));

  // use a linked list of pointers to <partition,count> records.
  list< pair<const valarray<bool>, p_count>* > majority;

  vector<string> names = sample.topologies[0].T.get_sequences();

  int count = 0;

  for(int i=0;i<sample.topologies.size();i++) {
    const SequenceTree& T = sample.topologies[i].T;

    int delta = sample.topologies[i].count;

    int min_old = 1+(int)(l*count);
    min_old = std::min(min_old,count);

    count += delta;
    int min_new = 1+(int)(l*count);
    min_new = std::min(min_new,count);

    // for each partition in the next tree
    std::valarray<bool> partition(T.n_leaves()); 
    for(int b=T.n_leaves();b<T.n_branches();b++) 
    {
      // Compute the standard bitmask for the partition
      partition = T.partition(b);
      if (not partition[first])
	partition = (not partition) and mask;
      else
	partition = partition and mask;
      
      assert(partition.size() == T.n_leaves());

      // Look up record for this partition
      container_t::iterator i_record = counts.find(partition);
      if (i_record == counts.end()) {
	counts.insert(container_t::value_type(partition,p_count()));
	i_record = counts.find(partition);
	assert(i_record != counts.end());
      }
      container_t::value_type* p_record = &(*i_record);

      // Update counts
      p_count& pc = p_record->second;
      int& C2 = pc.count;
      int C1 = C2;
      if (pc.last_tree != i) {
	pc.last_tree=i;
	C2 += delta;
      }
      
      // add the partition if it wasn't good before, but is now
      if (C1<min_old and C2 >= min_new)
	majority.push_back(p_record);
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

  vector<Partition> partitions;
  partitions.reserve( 2*names.size() );
  for(typeof(majority.begin()) p = majority.begin();p != majority.end();p++) {
    const valarray<bool>& partition =(*p)->first;
 
    Partition pi(names,partition,mask);

    if (informative(pi))
      partitions.push_back(pi);
  }

  return partitions;
}

#else
vector<Partition> get_Ml_partitions(const tree_sample& sample,double l,const valarray<bool>&  mask) 
{
  // find the first bit
  int first=0;
  while(first<mask.size() and not mask[first])
    first++;
  assert(first < mask.size());

  if (l < 0.5)
    throw myexception()<<"Consensus level for majority tree must be >= 0.5";
  if (l > 1.0)
    throw myexception()<<"Consensus level for majority tree must be <= 1.0";

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
    for(int b=names.size();b<T.size();b++) 
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

  vector<Partition> partitions;
  partitions.reserve( 2*names.size() );
  for(typeof(majority.begin()) p = majority.begin();p != majority.end();p++) {
    const valarray<bool>& partition =(*p)->first;
 
    Partition pi(names,partition,mask);

    if (informative(pi))
      partitions.push_back(pi);
  }

  return partitions;
}
#endif

vector<Partition> get_Ml_partitions(const tree_sample& sample,double l) {
  valarray<bool> mask(true,sample.names().size());
  return get_Ml_partitions(sample,l,mask);
}

void add_unique(list<valarray<bool> >& masks,const valarray<bool>& mask) 
{
  // don't add the mask unless contains internal partitions (it could be all 0)
  if (n_elements(mask) < 4) return;

  // don't add the mask if we already have that mask
  foreach(m,masks)
    if (equal(*m,mask)) return;

  // otherwise, add the mask
  masks.push_front(mask);
}


//consider only pulling out combinations of branches pointing to the same node

vector<Partition> get_Ml_sub_partitions(const tree_sample& sample,double l,int depth) 
{
  // get list of branches to consider cutting
  vector<Partition> partitions_c50 = get_Ml_partitions(sample, 0.5);
  SequenceTree MF = get_mf_tree(sample.names(),partitions_c50);
  vector<const_branchview> branches = branches_from_leaves(MF);  

  // start collecting partitions at M[l]
  vector<Partition> partitions = get_Ml_partitions(sample,l);

  // construct unit masks
  list< valarray<bool> > unit_masks;
  for(int b=0;b<branches.size();b++)
    add_unique(unit_masks, branch_partition(MF,branches[b]) );

  // construct beginning masks
  list<valarray<bool> > masks = unit_masks;

  // any good mask should be combined w/ other good masks
  list<valarray<bool> > good_masks;
  for(int iterations=0;not masks.empty();iterations++)
  {
    //std::cerr<<"Analyzing "<<masks.size()<<" masks."<<std::endl;;
    list<valarray<bool> > new_good_masks;

    // get sub-partitions for each mask
    vector<Partition> all_sub_partitions;
    foreach(m,masks) 
    {
      // get sub-partitions of *m 
      vector<Partition> sub_partitions = get_Ml_partitions(sample,l,*m);
    
      // remove sub-partitions that are implied by partitions
      sub_partitions = unimplied_partitions(partitions, sub_partitions);

      // if this isn't empty, then record this mask
      if (not sub_partitions.empty())
	new_good_masks.push_front(*m);

      // store the new sub-partitions we found
      merge_partitions(all_sub_partitions,sub_partitions);
    }

    // store all sub-partitions we found this round
    merge_partitions(partitions,all_sub_partitions);

    masks.clear();

    // fixme - do a convolution here - e.g. 2->1+1 3->1+2 4 ->1+3,2+2
    // otherwise we do 1  - 1,2 - 1,2,3,4 - 1,2,3,4,5,6,7,8
    good_masks.insert(good_masks.end(),new_good_masks.begin(),new_good_masks.end());
    foreach(i,new_good_masks)
      foreach(j,good_masks)
        add_unique(masks,*i and *j);

    // what will we operate on next time? 
    // - perhaps change to look at pairs of branches connected to a node
    // - perhaps depth 3 could be pairs of branches of distance 1
    // - should I use the M[0.5] tree here, or the M[l] tree?
    if (iterations < depth-1) {
      list<valarray<bool> > temp;
      foreach(i,new_good_masks)
	foreach(j,unit_masks)
	  add_unique(temp,*i and *j);
      masks = temp;
    }
  }



  return partitions;
}
