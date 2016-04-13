#include "TreeInterface.H"
#include "models/parameters.H"

using std::vector;
using std::string;
using boost::dynamic_bitset;

int TreeInterface::n_nodes() const {
  return P->TC->parameters_for_tree_node.size();
}

int TreeInterface::n_leaves() const {
  return P->TC->n_leaves;
}

int TreeInterface::n_leaf_branches() const {
  return (n_branches() == 1)?1:n_leaves();
}

int TreeInterface::n_branches() const {
  return P->TC->parameters_for_tree_branch.size()/2;
}

int TreeInterface::degree(int n) const {
  if (P->branches_from_affected_node[n])
    return P->branches_from_affected_node[n]->size();
  else
    return P->TC->parameters_for_tree_node[n].size();
}

int TreeInterface::branch_out(int n, int i) const {
  if (P->branches_from_affected_node[n])
    return (*P->branches_from_affected_node[n])[i];
  
  int p = P->TC->parameters_for_tree_node[n][i];
  if (p == -1)
    return n;
  else
    return P->get_parameter_value(p).as_int();
}

int TreeInterface::branch_in(int n, int i) const {
  return reverse(branch_out(n,i));
}

int TreeInterface::neighbor(int n, int i) const {
  return target(branch_out(n,i));
}

vector<int> TreeInterface::neighbors(int n) const {
  vector<int> nodes(degree(n));
  for(int i=0;i<nodes.size();i++)
    nodes[i] = neighbor(n, i);
  return nodes;
}

vector<int> TreeInterface::branches_out(int n) const {
  vector<int> branches(degree(n));
  for(int i=0;i<branches.size();i++)
    branches[i] = branch_out(n, i);
  return branches;
}

vector<int> TreeInterface::branches_in(int n) const {
  vector<int> branches(degree(n));
  for(int i=0;i<branches.size();i++)
    branches[i] = reverse(branch_out(n, i));
  return branches;
}

int TreeInterface::reverse(int b) const
{
  int B = n_branches();
  return (b + B) % (2*B);
}

void TreeInterface::append_branches_before(int b, vector<int>& branches) const
{
  int n = source(b);
  for(int i=0;i<degree(n);i++)
  {
    int b2 = branch_out(n,i);
    if (b2 != b)
      branches.push_back(reverse(b2));
  }
}


void TreeInterface::append_branches_after(int b, vector<int>& branches) const
{
  b = reverse(b);
  
  int n = source(b);
  for(int i=0;i<degree(n);i++)
  {
    int b2 = branch_out(n,i);
    if (b2 != b)
      branches.push_back(b2);
  }
}

vector<int> TreeInterface::branches_before(int b) const
{
  int n = source(b);
  if (degree(n) == 1) return {};

  vector<int> branches;
  branches.reserve(4);
  append_branches_before(b, branches);
  return branches;
}

vector<int> TreeInterface::branches_after(int b) const
{
  int n = target(b);
  if (degree(n) == 1) return {};

  vector<int> branches;
  branches.reserve(4);
  append_branches_after(b, branches);
  return branches;
}

vector<int> TreeInterface::all_branches_before_inclusive(int b) const
{
  vector<int> branches;
  branches.reserve(n_branches());
  branches.push_back(b);
  for(int i=0;i<branches.size();i++)
  {
    int b2 = branches[i];
    append_branches_before(b2, branches);
  }
  return branches;
}

vector<int> TreeInterface::all_branches_after_inclusive(int b) const
{
  vector<int> branches;
  branches.reserve(n_branches());
  branches.push_back(b);
  for(int i=0;i<branches.size();i++)
  {
    int b2 = branches[i];
    append_branches_after(b2, branches);
  }
  return branches;
}

vector<int> TreeInterface::all_branches_from_node(int n) const
{
  vector<int> branches;
  branches.reserve( n_branches() );

  for(int i=0;i<degree(n);i++)
    branches.push_back(branch_out(n,i));

  for(int i=0;i<branches.size();i++)
    append_branches_after(branches[i], branches);

  return branches;
}

vector<int> TreeInterface::all_branches_toward_node(int n) const
{
  vector<int> branches;
  branches.reserve( n_branches() );

  for(int i=0;i<degree(n);i++)
    branches.push_back(branch_in(n,i));

  for(int i=0;i<branches.size();i++)
    append_branches_before(branches[i], branches);

  std::reverse(branches.begin(), branches.end());

  return branches;
}

dynamic_bitset<> TreeInterface::partition(int b) const
{
  dynamic_bitset<> nodes(n_nodes());
  vector<int> branches = all_branches_after_inclusive(b);
  for(int b: branches)
    nodes.set(target(b));
  return nodes;
}

  /// Is 'n' contained in the subtree delineated by 'b'?
bool TreeInterface::subtree_contains(int b,int n) const {
  return partition(b)[n];
}

/// Is 'b2' contained in the subtree delineated by 'b1'?
bool TreeInterface::subtree_contains_branch(int b1,int b2) const 
{
  auto p = partition(b1);
  return p[source(b2)] and p[target(b2)];
}


int TreeInterface::source(int b) const {
  int p = P->TC->parameters_for_tree_branch[b].first;
  if (p == -1)
    return b;
  else
    return P->get_parameter_value(p).as_int();
}

int TreeInterface::target(int b) const {
  int p = P->TC->parameters_for_tree_branch[b].second;
  if (p == -1)
    return b - n_branches();
  else
    return P->get_parameter_value(p).as_int();
}
  
int TreeInterface::undirected(int b) const {
  return (b % n_branches());
}

bool TreeInterface::is_connected(int n1, int n2) const
{
  return (search_branch(n1, n2) != -1);
}

bool TreeInterface::is_leaf_node(int n) const {
  return degree(n) == 1;
}

bool TreeInterface::is_internal_node(int n) const {
  return not is_leaf_node(n);
}

bool TreeInterface::is_leaf_branch(int b) const {
  return is_leaf_node(source(b)) or is_leaf_node(target(b));
}
bool TreeInterface::is_internal_branch(int b) const {
  return not is_leaf_branch(b);
}
  
int TreeInterface::search_branch(int n1, int n2) const
{
  for(int i=0;i<degree(n1);i++)
  {
    int b = branch_out(n1,i);
    int n = target(b);
    if (n == n2) return b;
  }
  return -1;
}

int TreeInterface::find_branch(int n1, int n2) const
{
  int b = search_branch(n1,n2);
  if (b == -1)
    std::abort();
  return b;
}

int TreeInterface::find_undirected_branch(int n1, int n2) const
{
  return undirected(find_branch(n1,n2));
}

int TreeInterface::find_branch(const tree_edge& e) const
{
  return find_branch(e.node1, e.node2);
}

int TreeInterface::find_undirected_branch(const tree_edge& e) const
{
  return undirected(find_branch(e));
}

tree_edge TreeInterface::edge(int b) const
{
  return {source(b), target(b)};
}

tree_edge TreeInterface::edge(int n1, int n2) const
{
  return edge(find_branch(n1,n2));
}

double TreeInterface::branch_length(int b) const
{
  b %= n_branches();
  return P->get_parameter_value(P->TC->branch_length_parameters[b]).as_double();
}

void TreeInterface::set_branch_length(int b, double l)
{
  b %= n_branches();
  const_cast<Parameters*>(P)->context::set_parameter_value(P->TC->branch_length_parameters[b], l);
}

vector<int> branches_from_leaves(const TreeInterface& t) 
{
  vector<int> branch_list;
  branch_list.reserve(2*t.n_branches());
  vector<bool> visited(2*t.n_branches(),false);

  for(int i=0;i<t.n_leaves();i++) {
    branch_list.push_back(i);
    visited[i] = true;
  }

  for(int i=0;i<branch_list.size();i++) 
  {
    int b = branch_list[i];

    // because we are on the list, we are 'visited'
    assert(visited[b]);

    // check branches-after to see if any are ready
    for(int b2: t.branches_after(b))
    {
      // if we are already valid, then ignore
      if (visited[b2]) continue;

      // check if all branches-before are valid
      bool ready = true;
      for(int k: t.branches_before(b2))
	if (not visited[k]) ready = false;

      // if so, then 
      if (ready) {
	branch_list.push_back(b2);
	visited[b2] = true;
      }
    }
  }
  assert(branch_list.size() == 2*t.n_branches());

  return branch_list;
}

vector<dynamic_bitset<>> get_partitions(const TreeInterface& t)
{
  vector<int> branch_list = t.all_branches_from_node(0);
  std::reverse(branch_list.begin(), branch_list.end());

  // set up cached partition masks
  vector<dynamic_bitset<>> partitions(2*t.n_branches());
  for(auto& p: partitions)
    p.resize(t.n_nodes());

  // compute partition masks
  for(int b: branch_list)
  {
    if (not t.is_leaf_node(t.target(b)))
      for(int b2: t.branches_after(b))
	partitions[b] |= partitions[b2];

    partitions[b][t.target(b)] = true;

    partitions[t.reverse(b)] = ~partitions[b]; 
  }

  return partitions;
}

unsigned topology_distance(const TreeInterface& T1, const TreeInterface& T2) 
{
  assert(T1.n_leaves() == T2.n_leaves());

  unsigned l1 = T1.n_leaf_branches();
  unsigned l2 = T2.n_leaf_branches();

  unsigned n1 = T1.n_branches() - l1;
  unsigned n2 = T2.n_branches() - l2;

  auto partitions1 = get_partitions(T1);
  auto partitions2 = get_partitions(T2);

  std::set<dynamic_bitset<>> informative1;
  std::set<dynamic_bitset<>> informative2;
  
  for(auto& partition: partitions1)
    partition.resize(T1.n_leaves());

  for(auto& partition: partitions2)
    partition.resize(T2.n_leaves());

  // get partitions and lengths for T1
  for(int i=0;i<n1;i++)
    informative1.insert(std::move(partitions1[i+l1]));

  // get partitions and lengths for T2
  for(int i=0;i<n1;i++)
    informative2.insert(std::move(partitions2[i+l1]));

  // Accumulate distances for T1 partitions
  unsigned shared=0;
  for(const auto& partition: informative1)
    if (informative2.count(partition) or informative2.count(~partition))
      shared++;

  return (n1-shared) + (n2-shared);
}

double tree_length(const TreeInterface& t)
{
  double total = 0;
  for(int b=0; b < t.n_branches(); b++)
    total += t.branch_length(b);
  return total;
}

string write_branch(const TreeInterface& T, int b, const vector<string>& names);

string write_branches_and_node(const TreeInterface& T, const vector<int>& branches, int node, const vector<string>& names)
{
  string output;
  
  if (branches.size())
  {
    output = "(";
    for(int i=0;i<branches.size();i++) {
      output += write_branch(T, branches[i], names);
      if (i+1 < branches.size())
	output += ',';
    }
    output += ")";
  }

  // Print the name (it might be empty)
  if (names[node].size())
    output += names[node];

  return output;
}

string write_branch(const TreeInterface& T, int b, const vector<string>& names)
{
  // If this is an internal node, then print the subtrees
  return write_branches_and_node(T, T.branches_after(b), T.target(b), names);
}

string write(const TreeInterface& T, int root, const vector<string>& names)
{
  return write_branches_and_node(T, T.branches_out(root), root, names) + ";";
}

/// Return a Newick string representation of the tree 'T' with names 'names', including branch lengths by default.
std::string write(const TreeInterface& T, const std::vector<double>& L, const std::vector<std::string>& names)
{
  int root = T.target(0);

  vector<int> branches = T.all_branches_from_node(root);
  vector<string> names2 = names;
  for(int b: branches)
  {
    int n = T.target(b);
    names2[n] = "'" + names2[n] + "':" + std::to_string(L[T.undirected(b)]);
  }
  return write(T, root, names2);
}

/// Return a Newick string representation of the tree 'T' with names 'names', including branch lengths by default.
std::string write(const TreeInterface& T, const std::vector<std::string>& names, bool print_lengths)
{
  int root = T.target(0);
  if (print_lengths)
  {
    vector<double> L;
    for(int i=0;i<T.n_branches();i++)
      L.push_back(T.branch_length(i));
    return write(T, L, names);
  }
  else
    return write(T, root, names);
}

vector<int> edges_connecting_to_node(const Tree& T, int n);

/*
 * Here, we fix up the nodes in the Haskell tree 
 */
void TreeInterface::read_tree_node(const Tree& T, int n)
{
  assert(P->TC->parameters_for_tree_node[n].size() == T.node(n).degree());

  // These are the edges we seek to impose.
  vector<int> edges = edges_connecting_to_node(T,n);
  assert(edges.size() == T.node(n).degree());

  // These are the current edges.
  for(int i=0;i<edges.size();i++)
    const_cast<Parameters*>(P)->context::set_parameter_value(P->TC->parameters_for_tree_node[n][i], edges[i]);
}

void TreeInterface::read_tree(const Tree& T)
{
  for(int n=0; n < T.n_nodes(); n++)
    if (not T.node(n).is_leaf_node())
      read_tree_node(T, n);

  for(int b=0; b < 2*T.n_branches(); b++)
  {
    if (not T.directed_branch(b).source().is_leaf_node())
      const_cast<Parameters*>(P)->context::set_parameter_value(P->TC->parameters_for_tree_branch[b].first,  (int)T.directed_branch(b).source());
    if (not T.directed_branch(b).target().is_leaf_node())
      const_cast<Parameters*>(P)->context::set_parameter_value(P->TC->parameters_for_tree_branch[b].second, (int)T.directed_branch(b).target());
  }
}

void TreeInterface::reconnect_branch(int s1, int t1, int t2)
{
  int b1 = find_branch(s1,t1);
  int b2 = reverse(b1);

  auto& affected_nodes = const_cast<Parameters*>(P)->affected_nodes;
  auto& branches_from_affected_node = const_cast<Parameters*>(P)->branches_from_affected_node;
  
  if (not branches_from_affected_node[t1])
  {
    affected_nodes.push_back(t1);
    auto v = new vector<int>();
    for(int i=0; i<degree(t1); i++)
      v->push_back(branch_out(t1, i));
    branches_from_affected_node[t1] = v;
  }
  remove_element(*branches_from_affected_node[t1], b2);

  if (not branches_from_affected_node[t2])
  {
    affected_nodes.push_back(t2);
    auto v = new vector<int>();
    for(int i=0; i<degree(t2); i++)
      v->push_back(branch_out(t2, i));
    branches_from_affected_node[t2] = v;
  }
  branches_from_affected_node[t2]->push_back(b2);

  // Update branch source and target nodes
  const_cast<Parameters*>(P)->context::set_parameter_value(P->TC->parameters_for_tree_branch[b1].second, t2);
  const_cast<Parameters*>(P)->context::set_parameter_value(P->TC->parameters_for_tree_branch[b2].first,  t2);
}

void TreeInterface::begin_modify_topology()
{
#ifndef NDEBUG
  for(auto p: P->branches_from_affected_node)
    assert(not p);
  assert(P->affected_nodes.empty());
#endif
}

void TreeInterface::end_modify_node(int n)
{
  assert(P->branches_from_affected_node[n]);
  assert(P->TC->parameters_for_tree_node[n].size() == P->branches_from_affected_node[n]->size());

  // These are the current edges.
  const auto& branches = *P->branches_from_affected_node[n];

  assert(branches.size() == P->TC->parameters_for_tree_node[n].size());
  for(int i=0;i<branches.size();i++)
    const_cast<Parameters*>(P)->context::set_parameter_value(P->TC->parameters_for_tree_node[n][i], branches[i]);

  delete P->branches_from_affected_node[n];
  const_cast<Parameters*>(P)->branches_from_affected_node[n] = nullptr;
}

void TreeInterface::end_modify_topology()
{
  for(int n: P->affected_nodes)
    end_modify_node(n);
  
  const_cast<Parameters*>(P)->affected_nodes.clear();

#ifndef NDEBUG
  for(auto p: P->branches_from_affected_node)
    assert(not p);
  assert(P->affected_nodes.empty());
#endif
}

void check_tree(const Tree& T, const TreeInterface& t)
{
#ifndef NDEBUG
  for(int b=0; b < 2*T.n_branches(); b++)
  {
    assert(T.directed_branch(b).source() == t.source(b));
    assert(T.directed_branch(b).target() == t.target(b));
  }

  for(int n=0; n < T.n_nodes(); n++)
  {
    if (T.node(n).is_leaf_node()) continue;
    
    vector<int> VV = t.branches_out(n);

    vector<const_branchview> v = sorted_neighbors(T.node(n));
    vector<int> vv;
    for(const auto& bv: v)
      vv.push_back(bv);

    assert(VV.size() == v.size());
    for(int elem: v)
      assert(includes(VV,elem));
    for(int elem: VV)
      assert(includes(vv,elem));
  }
#endif
}

