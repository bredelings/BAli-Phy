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


SequenceTree standardized(const string& t) {
  SequenceTree T;
  T.parse(t);
  return standardized(T);
}
  
SequenceTree standardized(const string& t,const vector<string>& remove) {
  SequenceTree T;
  T.parse(t);
  return standardized(T,remove);
}
  
SequenceTree standardized(SequenceTree T) {

  T.unroot();

  map<string,int,lstr> sequences;

  for(int i=0;i<T.get_sequences().size();i++) {
    sequences.insert(pair<string,int>(T.get_sequences()[i],i));
  }

  vector<int> newnames(T.leaves());

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
  return T;
}

SequenceTree standardized(SequenceTree T,const vector<string>& remove) {
  for(int i=0;i<remove.size();i++)
    delete_node(T,remove[i]);

  return standardized(T);
}

valarray<bool> branch_partition(const tree& T,int b) {
  int parent = T.branch(b).parent();
  int child = T.branch(b).child();

  valarray<bool> temp = T.partition(parent,child);
  valarray<bool> p(T.leaves());
  for(int i=0;i<p.size();i++)
    p[i] = temp[i];

  return p;
}

bool equal(const valarray<bool>& v1,const valarray<bool>& v2) {
  assert(v1.size() == v2.size());
  bool match = true;
  for(int i=0;i<v1.size() && match;i++)
    if (v1[i] != v2[i])
      match = false;
  return match;
}

std::ostream& operator<<(std::ostream& o, const Partition& P) {
  // 
  for(int i=0;i<P.size();i++)
    if (P.split[i] and P.mask[i])
      o<<P.names[i]<<" ";

  o<<"| ";

  for(int i=0;i<P.size();i++)
    if (not P.split[i] and P.mask[i])
      o<<P.names[i]<<" ";
  o<<endl;

  return o;
}

bool operator==(const Partition& p1, const Partition& p2) {
  return (p1.names == p2.names) and
    equal(p1.split,p2.split) and
    equal(p1.mask,p2.mask);
}

/// Does the grouping of all nodes bm, imply *this?
bool Partition::implied_by(const valarray<bool>& bm) const {
  assert(bm.size() == split.size());
  return (equal(split,bm&mask) or  equal(split,(not bm)&mask));
}
// There should be a way to check 'consistent' (might imply),
// not consistent (implies not *this)

/// Does any branch in T imply the partition p?
bool contains_partition(const SequenceTree& T,const Partition& p) {
  bool result = false;
  for(int b=0;b<T.branches() and not result;b++) {
    valarray<bool> bp = branch_partition(T,b);

    if (p.implied_by(bp))
      result = true;
  }
  return result;
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
    const SequenceTree& T = tree_mean[ which_topology[i] ];

    
    result[i] = contains_partition(T,P);
  }
  return result;
}

struct ordering {
  vector<int> count;

  // decreasing order of count
  bool operator()(int i,int j) {return count[i] > count[j];}
  
  ordering(const vector<int>& v):count(v) {}
};



tree_sample::tree_sample(std::istream& file,const vector<string>& remove,int skip) 
{
  //--------- Load the trees (as strings) from STDIN ------//
  int lines=0;
  string line;
  while(getline(file,line)) {
    lines++;
    if (lines > skip)
      trees.push_back(line);
  }

  if (trees.size() == 0)
    throw myexception()<<"No trees were read in!";
  
  cout<<"# Loaded "<<trees.size()<<" trees"<<endl;

  //--------- Count how many of each topology -----------//
  foreach(mytree,trees) {
      
    SequenceTree T = standardized(*mytree,remove);
    string t = T.write(false);
      
    typeof(index.begin()) here = index.find(t);
      
    // it if hasn't been seen before, insert it
    if (here == index.end()) {
      tree_mean.push_back(T);                      // add to tree list (E)
      tree_var.push_back(T);                       // add to tree list (Var)
      for(int b=0;b<T.branches();b++) {
	tree_mean.back().branch(b).length() = 0;
	tree_var.back().branch(b).length() = 0;
      }
      topologies.push_back(t);                     // add to topology list (Var)
      count.push_back(0);                          // add to count list (Var)
      index[t] = topologies.size()-1;              // add to map of  (topology->index)
    }
      
    // determine which topology we map to
    int i = index[t];
    which_topology.push_back(i);
    count[i]++;

    // update the 1st and 2nd branch length moments for that topology
    for(int b=0;b<T.branches();b++) {
      tree_mean[i].branch(b).length() += T.branch(b).length();
      tree_var[i].branch(b).length() += pow(T.branch(b).length(),2);
    }
  }

  //----------- Normalize the expectations --------------//
  for(int i=0;i<topologies.size();i++) {
    for(int b=0;b<tree_mean[i].branches();b++) {
      // compute E b
      tree_mean[i].branch(b).length() /= count[i];

      // compute E b^2
      tree_var[i].branch(b).length() /= count[i];

      // compute Var b
      tree_var[i].branch(b).length() -= pow(tree_mean[i].branch(b).length(),2);
    }
  }

  cout<<"#   There were "<<topologies.size()<<" topologies."<<endl;
    
  //---------------  Sort topologies by count  ---------------//
  order.resize(topologies.size());
  for(int i=0;i<order.size();i++)
    order[i] = i;
  
  sort(order.begin(),order.end(),ordering(count));
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
  for(int b=0;b<T1A.branches();b++)
    T1A.branch(b).length() = 0.5;

  SequenceTree T2A = T2;
  for(int b=0;b<T2A.branches();b++)
    T2A.branch(b).length() = 0.5;

  return branch_distance(T1A,T2A);
}

