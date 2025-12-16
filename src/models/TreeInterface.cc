#include "tree/tree.H"
#include "TreeInterface.H"
#include "models/parameters.H"
#include "util/range.H"
#include "util/set.H"
#include "util/log-level.H"
#include "computation/expression/bool.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/list.H"
#include "computation/expression/constructor.H"
#include "computation/machine/graph_register.H"
#include "computation/machine/gcobject.H"

#include "immer/set.hpp"
typedef Box<immer::set<int>> IntSet;

using std::vector;
using std::string;
using std::map;
using std::optional;
using boost::dynamic_bitset;

// We use the context_ptr class to avoid pointing to index_var regs, which
// could disappear.
//
// What if... the route to the constant structure is modifiable, and the route changes???
// Suppose I make a pointer: {memory, context_index, reg} === {context, tree_reg}
// To deref a field, we would look up the reg, get the field, and evaluate.

tree_constants::tree_constants(context_ref& C, const expression_ref& E)
    :tree_constants(C, E.as_reg_var())
{
}


tree_constants::tree_constants(context_ref& C, int tree_reg)
    :tree_exp(reg_var(tree_reg)),
     n_leaves(0)
{
    auto tree = context_ptr(C, tree_reg);

    //------------------------- Create the tree structure -----------------------//

    while(not has_constructor(tree.head(), "Graph.Graph"))
    {
	if (has_constructor(tree.head(), "Graph.WithBranchLengths"))
	{
	    // We assume that the path to the array isn't changeable... ???
	    branch_durations_reg = tree[1].result().get_reg();

	    tree = tree[0];
	}

	if (has_constructor(tree.head(), "Forest.WithNodeTimes"))
	{
	    // We assume that the path to the array isn't changeable... ???
	    node_times_reg = tree[1].result().get_reg();

	    tree = tree[0];
	}

	if (has_constructor(tree.head(), "Forest.WithRoots"))
	{
	    assert(tree.size() == 3);

	    // We need to evaluate this to avoid getting an index_var.
	    roots_reg = tree[1].get_reg();

	    // We need to evaluate this to avoid getting an index_var.
	    away_from_root_reg = tree[2].get_reg();

	    tree = tree[0];
	}

	if (has_constructor(tree.head(), "Forest.WithBranchRates"))
	{
	    assert(tree.size() == 2);

	    // We need to evaluate this to avoid getting an index_var.
	    branch_rate_regs.push_back( tree[1].get_reg() );

	    tree = tree[0];
	}

	if (has_constructor(tree.head(), "Tree.Tree"))
	{
	    tree = tree[0];
	}

	if (has_constructor(tree.head(), "Forest.Forest"))
	{
	    tree = tree[0];
	}
    }

    assert(has_constructor(tree.head(),"Graph.Graph"));
    assert(tree.size() == 6);

    auto edges_out_of_node = tree[0];
    expression_ref tmp = edges_out_of_node.value();
    for(auto [node, _]: tmp.as_<IntMap>())
    {
        auto tmp = edges_out_of_node[node][1];

        param m_edges = tmp;

        parameters_for_tree_node.insert({node, m_edges});

        if (tmp.value().as_<IntSet>().size() < 2) n_leaves++;
    }

    auto nodes_for_edge    = tree[1];
    tmp = nodes_for_edge.value();
    for(auto [edge, _]: tmp.as_<IntMap>())
    {
        auto info = nodes_for_edge[edge];

        param m_source = info[0];
        param m_target = info[1];

        parameters_for_tree_branch.insert({edge, {m_source, m_target} });
    }

    labels_reg = tree[2].get_reg();
}

std::optional<int> TreeInterface::branch_durations_reg() const
{
    return get_tree_constants().branch_durations_reg;
}

std::optional<int> TreeInterface::roots_reg() const
{
    return get_tree_constants().roots_reg;
}

std::optional<int> TreeInterface::node_times_reg() const
{
    return get_tree_constants().node_times_reg;
}

std::optional<int> TreeInterface::away_from_root_reg() const
{
    return get_tree_constants().away_from_root_reg;
}

const std::vector<int>& TreeInterface::branch_rate_regs() const
{
    return get_tree_constants().branch_rate_regs;
}

int TreeInterface::n_nodes() const {
    return get_tree_constants().parameters_for_tree_node.size();
}

int TreeInterface::n_leaves() const {
    return get_tree_constants().n_leaves;
}

int TreeInterface::n_leaf_branches() const {
    return (n_branches() == 1)?1:n_leaves();
}

vector<int> TreeInterface::leaf_nodes() const
{
    vector<int> lnodes;

    for(int node: nodes())
    {
        if (is_leaf_node(node))
            lnodes.push_back(node);
    }

    return lnodes;
}

vector<int> TreeInterface::internal_nodes() const
{
    vector<int> inodes;

    for(int node: nodes())
    {
        if (is_internal_node(node))
            inodes.push_back(node);
    }

    return inodes;
}

// Leaf branches, pointing away from the leaves.
vector<int> TreeInterface::leaf_branches() const
{
    vector<int> lbranches;
    for(int b: branches())
        if (is_leaf_branch(b))
        {
            // Point away from leaves.
            if (not is_leaf_node(source(b)))
                b = reverse(b);

            lbranches.push_back(b);
        }
    return lbranches;
}

vector<int> TreeInterface::internal_branches() const
{
    vector<int> ibranches;
    for(int b: branches())
        if (is_internal_branch(b))
            ibranches.push_back(b);
    return ibranches;
}

std::vector<int> TreeInterface::branches() const
{
    vector<int> branches;
    for(auto& [b,_]: get_tree_constants().parameters_for_tree_branch)
    {
        if (b == undirected(b))
            branches.push_back(b);
    }
    return branches;
}

std::vector<int> TreeInterface::directed_branches() const
{
    vector<int> branches;
    for(auto& [b,_]: get_tree_constants().parameters_for_tree_branch)
        branches.push_back(b);
    return branches;
}

std::vector<int> TreeInterface::nodes() const
{
    vector<int> nodes;
    for(auto& [n,_]: get_tree_constants().parameters_for_tree_node)
        nodes.push_back(n);
    return nodes;
}

map<int,string> TreeInterface::labels() const
{
    assert(get_tree_constants().labels_reg);

    auto labels_ref = context_ptr(get_const_context(), *get_tree_constants().labels_reg);

    expression_ref labels_map = labels_ref.value();
    assert(labels_map.is_a<IntMap>());
    
    map<int,string> labels;
    for(auto& [node,_]: labels_map.as_<IntMap>())
    {
        auto l = label(node);
        assert(l);
        labels.insert({node,*l});
    }
    return labels;
}

std::optional<string> TreeInterface::label(int n) const
{
    if (not get_tree_constants().labels_reg) return {};

    auto labels_ref = context_ptr(get_const_context(), *get_tree_constants().labels_reg);

    assert(labels_ref.value().is_a<IntMap>());

    auto label = labels_ref[n]; // Maybe Text

    if (label.size() == 0)        // Nothing
        return "A"+std::to_string(n);

    assert(label.size() == 1);    // Just Text
    label = label[0];             // Text
    assert(label.size() == 1);     // Text CPPString
    label = label[0];
    return label.value().as_<String>();
}

int TreeInterface::n_branches() const {
    return get_tree_constants().parameters_for_tree_branch.size()/2;
}

int TreeInterface::degree(int n) const {
    return get_tree_constants().parameters_for_tree_node.at(n).get_value(get_const_context()).as_<IntSet>().size();
}

int TreeInterface::branch_out(int n, int i) const
{
    auto out_edges = get_tree_constants().parameters_for_tree_node.at(n).get_value(get_const_context()).as_<IntSet>();
    auto iter = out_edges.begin();
    for(int j=0;j<i;j++)
        iter++;

    return *iter;
}

int TreeInterface::branch_in(int n, int i) const {
    return reverse(branch_out(n,i));
}

int TreeInterface::neighbor(int n, int i) const {
    return target(branch_out(n,i));
}

vector<int> TreeInterface::neighbors(int n) const {
    auto nodes = branches_out(n);
    for(int& n: nodes)
	n = target(n);
    return nodes;
}

vector<int> TreeInterface::branches_out(int n) const {
    auto out_edges = get_tree_constants().parameters_for_tree_node.at(n).get_value(get_const_context()).as_<IntSet>();
    vector<int> branches;
    for(int b: out_edges)
	branches.push_back(b);
    return branches;
}

vector<int> TreeInterface::branches_in(int n) const {
    auto branches = branches_out(n);
    for(int& b: branches)
	b = reverse(b);
    return branches;
}

void TreeInterface::append_branches_before(int b, vector<int>& branches) const
{
    int n = source(b);
    auto out_edges = get_tree_constants().parameters_for_tree_node.at(n).get_value(get_const_context()).as_<IntSet>();
    for(int b2: out_edges)
    {
	if (b2 != b)
	    branches.push_back(reverse(b2));
    }
}


void TreeInterface::append_branches_after(int b, vector<int>& branches) const
{
    b = reverse(b);
  
    int n = source(b);
    auto out_edges = get_tree_constants().parameters_for_tree_node.at(n).get_value(get_const_context()).as_<IntSet>();
    for(int b2: out_edges)
    {
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

void TreeInterface::append_all_branches_before(int b, vector<int>& branches) const
{
    int i=branches.size();
    append_branches_before(b, branches);
    for(;i<branches.size();i++)
    {
	int b2 = branches[i];
	append_branches_before(b2, branches);
    }
}

void TreeInterface::append_all_branches_after(int b, vector<int>& branches) const
{
    int i=branches.size();
    append_branches_after(b, branches);
    for(;i<branches.size();i++)
    {
	int b2 = branches[i];
	append_branches_after(b2, branches);
    }
}

vector<int> TreeInterface::all_branches_before_inclusive(int b) const
{
    vector<int> branches;
    branches.reserve(n_branches());
    branches.push_back(b);
    append_all_branches_before(b, branches);
    return branches;
}

vector<int> TreeInterface::all_branches_before(int b) const
{
    vector<int> branches;
    branches.reserve(n_branches());
    append_all_branches_before(b, branches);
    return branches;
}

vector<int> TreeInterface::all_branches_after_inclusive(int b) const
{
    vector<int> branches;
    branches.reserve(n_branches());
    branches.push_back(b);
    append_all_branches_after(b, branches);
    return branches;
}

vector<int> TreeInterface::all_branches_after(int b) const
{
    vector<int> branches;
    branches.reserve(n_branches());
    append_all_branches_after(b, branches);
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

std::unordered_set<int> TreeInterface::partition(int b) const
{
    std::unordered_set<int> nodes;
    for(int b: all_branches_after_inclusive(b))
	nodes.insert(target(b));
    return nodes;
}

/// Is 'n' contained in the subtree delineated by 'b'?
bool TreeInterface::subtree_contains(int b,int n) const {
    return partition(b).contains(n);
}

/// Is 'b2' contained in the subtree delineated by 'b1'?
bool TreeInterface::subtree_contains_branch(int b1,int b2) const 
{
    auto p = partition(b1);
    return p.contains(source(b2)) and p.contains(target(b2));
}


int TreeInterface::source(int b) const {
    return std::get<0>(get_tree_constants().parameters_for_tree_branch.at(b)).get_value(get_const_context()).as_int();
}

int TreeInterface::target(int b) const {
    return std::get<1>(get_tree_constants().parameters_for_tree_branch.at(b)).get_value(get_const_context()).as_int();
}

int TreeInterface::reverse(int b) const
{
    assert(b != 0);
    return -b;
}

int TreeInterface::undirected(int b) const {
    return std::max(b, reverse(b));
}

bool TreeInterface::is_connected(int n1, int n2) const
{
    return search_branch(n1, n2).has_value();
}

bool TreeInterface::is_leaf_node(int n) const {
    return degree(n) <= 1;
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

optional<int> TreeInterface::search_branch(int n1, int n2) const
{
    auto out_edges = get_tree_constants().parameters_for_tree_node.at(n1).get_value(get_const_context()).as_<IntSet>();
    for(int b: out_edges)
	if (target(b) == n2) return b;

    return {};
}

int TreeInterface::find_branch(int n1, int n2) const
{
    auto b = search_branch(n1,n2);
    if (not b)
	std::abort();
    else
	return *b;
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

bool TreeInterface::is_rooted() const
{
    if (roots_reg())
        return true;
    else
        return false;
}

vector<int> TreeInterface::roots() const
{
    auto& C = get_const_context();
    int r = roots_reg().value();
    auto roots_evec = context_ptr(C,r).list_to_vector();
    return (vector<int>)roots_evec;
}

int TreeInterface::root() const
{
    auto rs = roots();
    if (rs.size() != 1)
	throw myexception()<<"TreeInterface::root(): asking for single root, but there are "<<rs.size()<<" roots.";

    return rs[0];
}

bool TreeInterface::away_from_root(int b) const
{
    assert(is_rooted());

    int array_reg = *away_from_root_reg();

    auto& C = get_const_context();

    auto away = context_ptr(C, array_reg);

    return is_bool_true(away[b].value());
}

bool TreeInterface::toward_root(int b) const
{
    return not away_from_root(b);
}

std::optional<int> TreeInterface::parent_branch_for_node(int n) const
{
    for(auto& b: branches_out(n))
        if (toward_root(b))
            return reverse(b);

    assert(n == root());

    return {};
}

std::optional<int> TreeInterface::parent_of_node(int n) const
{
    auto b = parent_branch_for_node(n);
    if (b)
        return source(*b);
    else
        return {};
}

std::vector<int> TreeInterface::children_of_node(int n) const
{
    vector<int> children;
    for(auto& b: branches_out(n))
        if (away_from_root(b))
            children.push_back(target(b));
    return children;
}

bool TreeInterface::has_branch_lengths() const
{
    if (branch_durations_reg())
        return true;
    else
        return false;
}

double TreeInterface::branch_rate(int b) const
{
    b = undirected(b);

    double rate = 1;
    for(int r: branch_rate_regs())
    {
        auto rates = context_ptr(get_const_context(), r);

        rate *= rates[b].value().as_double();
    }

    return rate;
}

map<int,double> TreeInterface::branch_lengths() const
{
    assert(has_branch_lengths());

    auto branch_to_reg = context_ptr{get_const_context(), branch_durations_reg().value()};

    auto length_regs = branch_to_reg.value();

    map<int,double> lengths;
    for(auto& [b,r]: length_regs.as_<IntMap>())
        lengths.insert({b, branch_rate(b) * branch_to_reg[b].value().as_double()});
    return lengths;
}

double TreeInterface::branch_length(int b) const
{
    // FIXME: handle branch lengths on time trees!

    b = undirected(b);

    if (branch_durations_reg())
    {
        int array_reg = *branch_durations_reg();

        auto lengths = context_ptr{get_const_context(), array_reg};

        return lengths[b].value().as_double() * branch_rate(b);
    }
    else if (has_node_times())
    {
        double T = std::abs(node_time(source(b)) - node_time(target(b)));
        return T * branch_rate(b);
    }
    else
        throw myexception()<<"TreeInterface::branch_length( ): tree has no branch lengths or node ages";
}

bool TreeInterface::can_set_branch_length(int b) const
{
    assert(has_branch_lengths());

    b = undirected(b);

    int array_reg = *branch_durations_reg();

    auto lengths = context_ptr{get_const_context(), array_reg};

    auto length = lengths[b];

    return (bool)length.move_to_modifiable();
}

void TreeInterface::set_branch_length(int b, double l)
{
    auto& C = get_context();

    assert(has_branch_lengths());

    b = undirected(b);

    int array_reg = *branch_durations_reg();

    auto lengths = context_ptr{C, array_reg};

    auto length = lengths[b];

    C.set_modifiable_value(length.get_reg(), l/branch_rate(b));
}

bool TreeInterface::has_node_times() const
{
    if (node_times_reg())
        return true;
    else
        return false;
}

double TreeInterface::node_time(int n) const
{
    int times_reg = *node_times_reg();

    auto times = context_ptr{get_const_context(), times_reg};

    return times[n].value().as_double();
}

bool TreeInterface::can_set_node_time(int n) const
{
    assert(has_node_times());

    int times_reg = *node_times_reg();

    auto times = context_ptr{get_const_context(), times_reg};

    auto time = times[n];

    return (bool)time.move_to_modifiable();
}

void TreeInterface::set_node_time(int n, double t)
{
    auto& C = get_context();

    int times_reg = *node_times_reg();

    auto times = context_ptr{C, times_reg};

    auto time = times[n];

    assert(time.move_to_modifiable());

    C.set_modifiable_value(time.get_reg(), t);
}

const tree_constants& ParametersTreeInterface::get_tree_constants() const
{
    return *P->TC;
}

const context_ref& ParametersTreeInterface::get_const_context() const
{
    return *P;
}

context_ref& ParametersTreeInterface::get_context()
{
    return *const_cast<Parameters*>(P);
}

const tree_constants& ModifiablesTreeInterface::get_tree_constants() const
{
    return *TC;
}

const context_ref& ModifiablesTreeInterface::get_const_context() const
{
    return C;
}

context_ref& ModifiablesTreeInterface::get_context()
{
    return const_cast<context_ref&>(C);
}

ModifiablesTreeInterface::ModifiablesTreeInterface(context_ref& c, int tree_reg)
    :C(c),
     TC(new tree_constants(c,reg_var(tree_reg)))
{
}

ModifiablesTreeInterface::~ModifiablesTreeInterface()
{
    delete TC;
}

vector<int> branches_from_leaves(const TreeInterface& t) 
{
    // Start with the leaf branches.
    vector<int> branch_list = t.leaf_branches();

    // Mark the leaf branches visited
    std::set<int> visited;
    for(auto& b: branch_list)
        visited.insert(b);

    for(int i=0;i<branch_list.size();i++) 
    {
	int b = branch_list[i];

	// because we are on the list, we are 'visited'
	assert(visited.contains(b));

	// check branches-after to see if any are ready
	for(int b2: t.branches_after(b))
	{
	    // if we are already valid, then ignore
	    if (visited.contains(b2)) continue;

	    // check if all branches-before are valid
	    bool ready = true;
	    for(int k: t.branches_before(b2))
		if (not visited.contains(k)) ready = false;

	    // if so, then 
	    if (ready) {
		branch_list.push_back(b2);
		visited.insert(b2);
	    }
	}
    }
    assert(branch_list.size() == 2*t.n_branches());

    return branch_list;
}

/*
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

    std::set<dynamic_bitset<>> partitions1;
    std::set<dynamic_bitset<>> partitions2;
  
    for(auto partition: get_partitions(T1))
    {
	partition.resize(T1.n_leaves());
        partitions1.insert(std::move(partition));
    }

    for(auto partition: get_partitions(T2))
    {
	partition.resize(T2.n_leaves());
        partitions2.insert(std::move(partition));
    }

    // Accumulate distances for T1 partitions
    unsigned shared=0;
    for(const auto& partition: partitions1)
	if (partitions2.count(partition) or partitions2.count(~partition))
	    shared++;

    return partitions1.size() + partitions2.size() - 2*shared;
}
*/

double tree_length(const TreeInterface& t)
{
    double total = 0;
    for(auto b: t.branches())
	total += t.branch_length(b);
    return total;
}

string write_branch(const TreeInterface& T, int b, const map<int,string>& names);

string write_branches_and_node(const TreeInterface& T, const vector<int>& branches, int node, const map<int,string>& names)
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
    if (names.at(node).size())
	output += names.at(node);

    return output;
}

string write_branch(const TreeInterface& T, int b, const map<int,string>& names)
{
    // If this is an internal node, then print the subtrees
    return write_branches_and_node(T, T.branches_after(b), T.target(b), names);
}

string write(const TreeInterface& T, int root, const map<int,string>& names)
{
    return write_branches_and_node(T, T.branches_out(root), root, names) + ";";
}

/// Return a Newick string representation of the tree 'T' with names 'names', including branch lengths by default.
std::string write(const TreeInterface& T, int root, const map<int,double>& L, const map<int,string>& names)
{
    vector<int> branches = T.all_branches_from_node(root);
    auto names2 = names;
    for(int b: branches)
    {
	int n = T.target(b);
	names2.at(n) = names2.at(n) +":" + std::to_string(L.at(T.undirected(b)));
    }
    return write(T, root, names2);
}

/// Return a Newick string representation of the tree 'T' with names 'names', including branch lengths by default.
std::string write(const TreeInterface& T, map<int,string> names, bool print_lengths)
{
    int root = -1;
    if (T.n_nodes() > 2)
	root = T.internal_nodes()[0];
    else
        root = T.nodes()[0];

    for(auto& [b,name]: names)
	name = escape_for_newick(name);

    if (print_lengths)
	return write(T, root, T.branch_lengths(), names);
    else
	return write(T, root, names);
}

std::string write(const TreeInterface& T)
{
    return write(T, T.labels(), T.has_branch_lengths());
}

bool TreeInterface::reconnect_branch(int s1, int t1, int t2)
{
    int b1 = find_branch(s1,t1);
    int b2 = reverse(b1);

    auto out_t1 = get_tree_constants().parameters_for_tree_node.at(t1);
    auto out_t2 = get_tree_constants().parameters_for_tree_node.at(t2);

    auto out_t1_set = out_t1.get_value(get_const_context()).as_<IntSet>();
    out_t1_set = out_t1_set.erase(b2);

    auto out_t2_set = out_t2.get_value(get_const_context()).as_<IntSet>();
    out_t2_set = out_t2_set.insert(b2);

    // Update branch source and target nodes
    auto target_b1 = std::get<1>(get_tree_constants().parameters_for_tree_branch.at(b1));
    auto source_b2 = std::get<0>(get_tree_constants().parameters_for_tree_branch.at(b2));
    auto& C = get_context();
    
    if (out_t1.is_modifiable(C) and out_t2.is_modifiable(C) and target_b1.is_modifiable(C) and source_b2.is_modifiable(C))
    {
	out_t1.set_value(C, out_t1_set);
	out_t2.set_value(C, out_t2_set);
	target_b1.set_value(C, t2);
	source_b2.set_value(C, t2);
	return true;
    }
    else
	return false;
}

// This could create loops it we don't check that the subtrees are disjoint.
// br{1,2} point out of the subtrees.  b{1,2} point into the subtrees, towards the other subtree.
bool tryNNI(TreeInterface& T, int br1, int br2)
{
    int b1 = T.reverse(br1);
    int b2 = T.reverse(br2);

    int s1 = T.source(b1);
    int t1 = T.target(b1);

    int s2 = T.source(b2);
    int t2 = T.target(b2);

    //  assert(not t().subtree_contains(br1,s2));
    //  assert(not t().subtree_contains(br2,s1));

    if (T.reconnect_branch(s1,t1,t2))
    {
	if (not T.reconnect_branch(s2,t2,t1)) throw myexception()<<"NNI: can modify first branch, but not second branch!";
	return true;
    }
    else
	return false;
}

bool is_degree3_edge(const TreeInterface& t, const tree_edge& e)
{
    int d1 = t.degree(e.node1);
    int d2 = t.degree(e.node2);
    return (d1 == 3 or d2 == 3);
}

bool is_degree3_edge(const TreeInterface& t, int b)
{
    return is_degree3_edge(t, t.edge(b));
}

