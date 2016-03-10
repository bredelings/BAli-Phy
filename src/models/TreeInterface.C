#include "TreeInterface.H"
#include "models/parameters.H"

using std::vector;
using boost::dynamic_bitset;

int TreeInterface::n_nodes() const {
  return P->TC->parameters_for_tree_node.size();
}

int TreeInterface::n_leaves() const {
  return _n_leaves;
}

int TreeInterface::n_branches() const {
  return P->TC->parameters_for_tree_branch.size()/2;
}

int TreeInterface::degree(int n) const {
  return P->TC->parameters_for_tree_node[n].size();
}

int TreeInterface::branch_out(int n, int i) const {
  int p = P->TC->parameters_for_tree_node[n][i];
  if (p == -1)
    return n;
  else
    return P->get_parameter_value(p).as_int();
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

boost::dynamic_bitset<> TreeInterface::partition(int b) const
{
  boost::dynamic_bitset<> nodes(n_nodes());
  vector<int> branches = all_branches_after_inclusive(b);
  for(int b: branches)
    nodes.set(target(b));
  return nodes;
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

TreeInterface::TreeInterface(const Parameters* p)
  :P(p)
{
  for(int n=0;n<n_nodes();n++)
    if (is_leaf_node(n))
      _n_leaves++;
}

