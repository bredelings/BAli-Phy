#include <map>
#include "tree-dist.H"
#include "rng.H"

using std::vector;
using std::list;
using std::valarray;

using std::map;
using std::pair;

using std::string;
using std::endl;
using std::cerr;
using std::cout;

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
  
void standardize(SequenceTree& T) {

  map<string,int,lstr> sequences;

  for(int i=0;i<T.get_sequences().size();i++) {
    sequences.insert(pair<string,int>(T.get_sequences()[i],i));
  }

  vector<int> newnames(T.n_leaves());

  int i=0;
  foreach(s,sequences) {
    pair<string,int> p = *s;
    newnames[p.second] = i;
    i++;
  }

  //  cerr<<t<<endl;
  //  cerr<<T.write()<<endl;
  //  cerr<<T.write(false)<<endl;
  T.SequenceTree::standardize(newnames);
  //  cerr<<T.write(false)<<endl;
}

void standardize(SequenceTree& T,const vector<string>& remove) {
  for(int i=0;i<remove.size();i++)
    delete_node(T,remove[i]);
  standardize(T);
}

valarray<bool> branch_partition(const Tree& T,int b) {
  int parent = T.branch(b).target();
  int child = T.branch(b).source();

  valarray<bool> temp = T.partition(parent,child);
  valarray<bool> p(T.n_leaves());
  for(int i=0;i<p.size();i++)
    p[i] = temp[i];

  return p;
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
    o<<endl;
  }
  else {
    for(int i=0;i<P.size();i++)
      if (P.group2[i]) o<<P.names[i]<<" ";
    
    o<<"| ";
    
    for(int i=0;i<P.size();i++)
      if (P.group1[i]) o<<P.names[i]<<" ";
    o<<endl;
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
bool contains_partition(const SequenceTree& T,const Partition& p) {
  bool result = false;
  for(int b=0;b<T.n_branches() and not result;b++) {
    valarray<bool> bp = branch_partition(T,b);

    if (implies(bp,p)) return true;
  }
  return false;
}

int tree_sample::get_index(const string& t) const {
  typeof(index.begin()) here = index.find(t);

  if (here == index.end())
    return -1;
  else
    return index[t];
}

valarray<bool> tree_sample::supports_topology(const string& t) const {
  typeof(index.begin()) here = index.find(t);

  if (here == index.end())
    return valarray<bool>(false,trees.size());

  int t_i = index[t];

  valarray<bool> result(trees.size());

  for(int i=0;i<result.size();i++)
    result[i] = (t_i == which_topology[i]);

  return result;
}

valarray<bool> tree_sample::supports_partition(const Partition& P) const {
  valarray<bool> result(trees.size());

  for(int i=0;i<result.size();i++) {

    // Get a tree with the same topology
    const SequenceTree& T = topologies[ which_topology[i] ].T;

    
    result[i] = contains_partition(T,P);
  }
  return result;
}

valarray<bool> tree_sample::supports_partitions(const vector<Partition>& partitions) const 
{
  valarray<bool> result(trees.size());

  for(int i=0;i<result.size();i++) {

    // Get a tree with the same topology
    const SequenceTree& T = topologies[ which_topology[i] ].T;
    
    result[i] = true;
    for(int p=0;p<partitions.size() and result[i];p++)
      result[i] = contains_partition(T,partitions[p]);
  }
  return result;
}

struct ordering {
  const vector<tree_sample::topology_record>& v;

  // decreasing order of count
  bool operator()(int i,int j) {return v[i].count > v[j].count;}
  
  ordering(const vector<tree_sample::topology_record>& v_):v(v_) {}
};


tree_sample::topology_record::topology_record(const SequenceTree& ST,
					      const string& s)
  :topology(s),T(ST),
   mean(vector<double>(T.n_branches(),0)),
   var(vector<double>(T.n_branches(),0)),
   count(0)
{ }


tree_sample::tree_sample(std::istream& file,const vector<string>& remove,int skip,int max) 
{
  //--------- Load the trees (as strings) from STDIN ------//
  int lines=0;
  string line;
  while(getline(file,line)) {
    lines++;
    if (lines > skip)
      trees.push_back(line);
  // quit if we've read in 'max' trees
    if (max >= 0 and trees.size() == max) break;
  }

  if (trees.size() == 0)
    throw myexception()<<"No trees were read in!";
  
  cout<<"# Loaded "<<trees.size()<<" trees"<<endl;

  //--------- Count how many of each topology -----------//
  foreach(mytree,trees) {
      
    // This should make all the branch & node numbers the same if the topology is the same
    SequenceTree T = standardized(*mytree,remove);
    // FIXME - this should be a standard string representation
    string t = add_root(T,0).write(false);
      
    typeof(index.begin()) here = index.find(t);
      
    // it if hasn't been seen before, insert it
    if (here == index.end()) {
      topologies.push_back(topology_record(T,t));

      index[t] = topologies.size()-1;              // add to map of  (topology->index)
    }
      
    // determine which topology we map to
    int i = index[t];
    which_topology.push_back(i);
    topologies[i].count++;

    // update the 1st and 2nd branch length moments for that topology
    for(int b=0;b<T.n_branches();b++) {
      topologies[i].mean[b] +=     T.branch(b).length();
      topologies[i].var[b]  += pow(T.branch(b).length(),2);
    }
  }

  //----------- Normalize the expectations --------------//
  for(int i=0;i<topologies.size();i++) {
    for(int b=0;b<topologies[i].T.n_branches();b++) {
      // compute E b
      topologies[i].mean[b] /= topologies[i].count;

      // compute E b^2
      topologies[i].var[b] /= topologies[i].count;

      // compute Var b
      topologies[i].var[b] -= pow(topologies[i].mean[b],2);

      // set the branch lengths to the expected length
      topologies[i].T.branch(b).set_length(topologies[i].mean[b]);
    }
  }

  cout<<"#   There were "<<topologies.size()<<" topologies."<<endl;
    
  //---------------  Sort topologies by count  ---------------//
  order.resize(topologies.size());
  for(int i=0;i<order.size();i++)
    order[i] = i;
  
  sort(order.begin(),order.end(),ordering(topologies));
}


int find_partition(const valarray<bool>& p1, const vector<valarray<bool> >& pv) {
  valarray<bool> np1 = not p1;
  for(int i=0;i<pv.size();i++) {
    if (equal(pv[i],p1) or equal(pv[i],np1))
      return i;
  }
  return -1;
}

double branch_distance(const SequenceTree& T1, const SequenceTree& T2) {
  assert(T1.n_leaves() == T2.n_leaves());

  vector<double> d1(T1.n_branches());
  vector< valarray<bool> > part1(T1.n_branches(),valarray<bool>(false,T1.n_leaves()));

  vector<double> d2(T2.n_branches());
  vector< valarray<bool> > part2(T2.n_branches(),valarray<bool>(false,T1.n_leaves()));

  // get partitions and lengths for T1
  for(int b=0;b<T1.n_branches();b++) {
    d1[b] = T1.branch(b).length();
    part1[b] = branch_partition(T1,b);
  }

  // get partitions and lengths for T2
  for(int b=0;b<T2.n_branches();b++) {
    d2[b] = T2.branch(b).length();
    part2[b] = branch_partition(T2,b);
  }

  // Accumulate distances for T1 partitions
  double total=0;
  for(int i=0;i<part1.size();i++) {
    int found = find_partition(part1[i],part2);
    if (found == -1)
      total += std::abs(d1[i]);
    else {
      total += std::abs(d1[i] - d2[found]);
    }
  }

  // Accumulate distances for T2 partitions
  for(int i=0;i<part1.size();i++) {
    int found = find_partition(part1[i],part2);
    if (found == -1)
      total += std::abs(d1[i]);
    else
      ; // this is already counted in the previous loop
  }
  return total;
}

double topology_distance(const SequenceTree& T1, const SequenceTree& T2) {
  SequenceTree T1A = T1;
  for(int b=0;b<T1A.n_branches();b++)
    T1A.branch(b).set_length(0.5);

  SequenceTree T2A = T2;
  for(int b=0;b<T2A.n_branches();b++)
    T2A.branch(b).set_length(0.5);

  return branch_distance(T1A,T2A);
}

struct compare_complete_partitions {
  bool operator()(const valarray<bool>& p1,const valarray<bool>& p2) const {
    if (p1.size() != p2.size())
      std::cerr<<"p1.size() = "<<p1.size()<<" and p2.size() = "<<p2.size()<<"\n";
    assert(p1.size() == p2.size());
    assert(p1[0]);
    assert(p2[0]);
    
    for(int i=0;i<p1.size();i++) {
      if (p2[i] and not p1[i])
	return true;
      if (p1[i] and not p2[i])
	return false;
    }
    return false;
  }
};

vector<Partition> strict_consensus_partitions(const tree_sample& sample) 
{
  vector<Partition> partitions;

  const SequenceTree& T = sample.topologies[0].T;

  vector<string> names = T.get_sequences();

  // Get the partitions in the first tree
  for(int b=T.n_leaves();b<T.n_branches();b++) {
      std::valarray<bool> partition = branch_partition(T,b);

      partitions.push_back(Partition(names,partition));
  }

  for(int i=1;i<sample.topologies.size();i++) 
  {
    const SequenceTree& T = sample.topologies[i].T;
    
    for(int j=0;j<partitions.size();) {
      if (not contains_partition(T,partitions[j]))
	partitions.erase(partitions.begin()+j);
      else
	j++;
    }

    if (not partitions.size()) break;
  }
  return partitions;
}

vector<Partition> get_Ml_partitions(const tree_sample& sample,double l) {
  if (l < 0.5)
    throw myexception()<<"Consensus level for majority tree must be > 0.5";
  if (l > 1.0)
    throw myexception()<<"Consensus level for majority tree must be < 1.0";

  if (l == 1.0)
    return strict_consensus_partitions(sample);

  // use a sorted list of <partition,count>, sorted by partition.
  map<valarray<bool>,int,compare_complete_partitions > counts;

  // use a linked list of pointers to <partition,count> records.
  list<map<valarray<bool>,int,compare_complete_partitions >::iterator > majority;

  vector<string> names = sample.topologies[0].T.get_sequences();

  for(int i=0;i<sample.size();i++) {
    const SequenceTree& T = sample.topologies[ sample.which_topology[i] ].T;

    int min_old = 1+(int)(l*i);
    int min_new = 1+(int(l*(i+1)));

    // for partition in the next tree
    for(int b=T.n_leaves();b<T.n_branches();b++) {
      std::valarray<bool> partition = branch_partition(T,b);

      if (not partition[0])
	partition = not partition;
      
      assert(partition.size() == T.n_leaves());
      int& C = counts[partition];
      
      // add the partition if it wasn't good before, but is now
      if (C<min_old and C+1 >= min_new)
	majority.push_back(counts.find(partition));

      // increment the count
      C++;
    }


    // for partition in the majority tree
    for(typeof(majority.begin()) p = majority.begin();p != majority.end();) {
      if ((*p)->second < min_new) {
	typeof(p) old = p;
	p++;
	majority.erase(old);
      }
      else
	p++;
    }
  }

  vector<Partition> partitions;
  partitions.reserve(majority.size());
  for(typeof(majority.begin()) p = majority.begin();p != majority.end();p++) {
    const valarray<bool>& partition =(*p)->first;
 
    partitions.push_back(Partition(names,partition) );
  }

  return partitions;
}

