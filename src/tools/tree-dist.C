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
  }
  std::valarray<bool> rmask = not(P.group1 or P.group2);
  if (statistics::count(rmask)) {
    o<<" [";
    for(int i=0;i<P.size();i++) {
      if (rmask[i]) o<<P.names[i]<<" ";
    }
    o<<"]";
  }

  return o<<endl;
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
bool implies(const SequenceTree& T,const Partition& p) {
  bool result = false;
  for(int b=0;b<T.n_branches() and not result;b++) {
    valarray<bool> bp = branch_partition(T,b);

    if (implies(bp,p)) return true;
  }
  return false;
}

bool implies(const SequenceTree& T,const std::vector<Partition>& partitions) 
{
  for(int p=0;p<partitions.size();p++)
    if (not implies(T,partitions[p]))
      return false;
  return true;
}

int which_partition(const SequenceTree& T, const Partition& p) {
  for(int b=0; b<T.n_branches(); b++) {
    valarray<bool> bp = branch_partition(T,b);
    if( implies(bp,p) )
      return b;
  }
  throw myexception(string("Partition not found in tree!"));
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
    
    result[i] = implies(T,P);
  }
  return result;
}

valarray<bool> tree_sample::supports_partitions(const vector<Partition>& partitions) const 
{
  valarray<bool> result(trees.size());

  for(int i=0;i<result.size();i++) {

    // Get a tree with the same topology
    const SequenceTree& T = topologies[ which_topology[i] ].T;
    
    result[i] = implies(T,partitions);
  }
  return result;
}

double tree_sample::PP(const Partition& P) const 
{
  int count=0;
  for(int t=0;t<topologies.size();t++) 
    if (implies(topologies[t].T,P))
	count += topologies[t].count;
   
  return double(count)/size();
}

double tree_sample::PP(const vector<Partition>& partitions) const 
{
  int count=0;
  for(int t=0;t<topologies.size();t++) {
    if (implies(topologies[t].T,partitions))
      count += topologies[t].count;
  }
   
  return double(count)/size();
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

  int lines=0;
  string line;
  while(getline(file,line)) {

    // don't start if we haven't skipped enough trees
    if (lines++ < skip) break;

    // quit if we've read in 'max' trees
    if (max >= 0 and trees.size() == max) break;

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

    // This should be a standard string representation
    string t = add_root(T,0).write(false);
      
    // If it hasn't been seen before, insert it
    if (index.find(t) == index.end()) {
      topologies.push_back(topology_record(T,t));

      index[t] = topologies.size()-1;              // add to map of  (topology->index)
    }
      
    //----------- Add tree to distribution -------------//
    trees.push_back(line);
    int i = index[t];
    which_topology.push_back(i);
    topologies[i].count++;

    // update the 1st and 2nd branch length moments for that topology
    for(int b=0;b<T.n_branches();b++) {
      topologies[i].mean[b] +=   T.branch(b).length();
      topologies[i].var[b]  +=   T.branch(b).length()*T.branch(b).length();
    }
  }

  if (trees.size() == 0)
    throw myexception()<<"No trees were read in!";
  
  cout<<"# Loaded "<<trees.size()<<" trees"<<endl;

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


struct compare_complete_partitions {
  bool operator()(const valarray<bool>& p1,const valarray<bool>& p2) const {
    if (p1.size() != p2.size())
      std::cerr<<"p1.size() = "<<p1.size()<<" and p2.size() = "<<p2.size()<<"\n";
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

vector<Partition> strict_consensus_partitions(const tree_sample& sample,const valarray<bool>& mask) 
{
  vector<Partition> partitions;

  const SequenceTree& T = sample.topologies[0].T;

  vector<string> names = T.get_sequences();

  // Get the partitions in the first tree
  for(int b=T.n_leaves();b<T.n_branches();b++) {
      std::valarray<bool> partition = branch_partition(T,b);

      partitions.push_back(Partition(names,partition,mask));
  }

  for(int i=1;i<sample.topologies.size();i++) 
  {
    const SequenceTree& T = sample.topologies[i].T;
    
    for(int j=0;j<partitions.size();) {
      if (not implies(T,partitions[j]))
	partitions.erase(partitions.begin()+j);
      else
	j++;
    }

    if (not partitions.size()) break;
  }
  return partitions;
}

vector<Partition> strict_consensus_partitions(const tree_sample& sample) {
  valarray<bool> mask(true,sample.topologies[0].T.n_leaves());
  return strict_consensus_partitions(sample,mask);
}

vector<Partition> get_Ml_partitions(const tree_sample& sample,double l,const valarray<bool>&  mask) 
{
  // find the first bit
  int first=0;
  while(first<mask.size() and not mask[first])
    first++;
  assert(first < mask.size());

  if (l < 0.5)
    throw myexception()<<"Consensus level for majority tree must be > 0.5";
  if (l > 1.0)
    throw myexception()<<"Consensus level for majority tree must be < 1.0";

  if (l == 1.0)
    return strict_consensus_partitions(sample,mask);

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

      if (not partition[first])
	partition = (not partition) and mask;
      else
	partition = partition and mask;
      
      assert(partition.size() == T.n_leaves());

      // FIXME - we are doing the lookup twice
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
 
    if (statistics::count(partition) < 2) continue;
    if (statistics::count((not partition) and mask) < 2) continue;

    partitions.push_back(Partition(names,partition,mask) );
  }

  return partitions;
}

vector<Partition> get_Ml_partitions(const tree_sample& sample,double l) {
  valarray<bool> mask(true,sample.topologies[0].T.n_leaves());
  return get_Ml_partitions(sample,l,mask);
}

vector<Partition> get_Ml_sub_partitions(const tree_sample& sample,double l,double r) {
  vector<Partition> partitions = get_Ml_partitions(sample,l);

  vector<double> support(partitions.size());
  for(int i=0;i<partitions.size();i++)
    support[i] = sample.PP(partitions[i]);

  // break branches in the ***M_0.5*** tree? (more branches we could break - if we are only breaking 1)

  SequenceTree MF = get_mf_tree(sample.topologies[0].T.get_sequences(),partitions);

  vector<const_branchview> branches = branches_from_leaves(MF);  

  for(int b=0;b<branches.size();b++) {
    // get sub-partitions and support
    valarray<bool> mask = branch_partition(MF,branches[b]);
    vector<Partition> sub_partitions = get_Ml_partitions(sample,l,mask);
    vector<double> sub_support(sub_partitions.size());
    for(int i=0;i<sub_partitions.size();i++) {
      sub_support[i] = sample.PP(sub_partitions[i]);
      assert(sub_support[i] >= l);
    }

    // check if we are already implied
    for(int i=0;i<sub_partitions.size();i++) {
      bool ok = true;
      for(int j=0;j<partitions.size() and ok;j++)
	if (implies(partitions[j],sub_partitions[i])) {
	  ok = statistics::odds(sub_support[i])/statistics::odds(support[j]) > r;
	  if (ok)
	    ok = (sub_support[i]-support[j])*sample.size() > 10;
	}

      if (ok)
	partitions.push_back(sub_partitions[i]);
    }
  }

  return partitions;
}


