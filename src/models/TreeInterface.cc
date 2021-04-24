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

using std::vector;
using std::string;
using boost::dynamic_bitset;

tree_constants::tree_constants(context_ref& C, const expression_ref& E)
    :tree_exp(E),
     n_leaves(0)
{
    int tree_reg = E.as_<reg_var>().target;

    //------------------------- Create the tree structure -----------------------//
    auto tree_closure = C.lazy_evaluate_reg(tree_reg);
    if (log_verbose >= 3)
        std::cerr<<"tree = "<<tree_closure.print()<<"\n\n";

    if (has_constructor(tree_closure.exp, "Tree.BranchLengthTree"))
    {
        // We assume that the path to the array isn't changeable... ???
        auto [_, r] = C.incremental_evaluate(tree_closure.reg_for_slot(1));
        branch_durations_array_reg = r;

        tree_reg = tree_closure.reg_for_slot(0);
        tree_closure = C.lazy_evaluate_reg(tree_reg);
    }

    if (has_constructor(tree_closure.exp, "Tree.TimeTree"))
    {
        // We assume that the path to the array isn't changeable... ???
        auto [_, r] = C.incremental_evaluate(tree_closure.reg_for_slot(1));
        node_times_array_reg = r;

        tree_reg = tree_closure.reg_for_slot(0);
        tree_closure = C.lazy_evaluate_reg(tree_reg);
    }

    if (has_constructor(tree_closure.exp, "Tree.LabelledTree"))
    {
        assert(tree_closure.exp.sub().size() == 2);
        // FIXME - set labels!

        tree_reg = tree_closure.reg_for_slot(0);
        tree_closure = C.lazy_evaluate_reg(tree_reg);
    }

    if (has_constructor(tree_closure.exp, "Tree.RootedTree"))
    {
        assert(tree_closure.exp.sub().size() == 3);

        // We need to evaluate this to avoid getting an index_var.
        auto [r_root, _1] = C.incremental_evaluate(tree_closure.reg_for_slot(1));
        root_reg = r_root;

        // We need to evaluate this to avoid getting an index_var.
        auto [r_away, _2] = C.incremental_evaluate(tree_closure.reg_for_slot(2));
        away_from_root_array_reg = r_away;

        tree_reg = tree_closure.reg_for_slot(0);
        tree_closure = C.lazy_evaluate_reg(tree_reg);
    }

    auto tree_structure = C.maybe_modifiable_structure(tree_reg);
    assert(has_constructor(tree_structure,"Tree.Tree"));
    assert(tree_structure.sub().size() == 3);

    auto edges_out_of_node = tree_structure.sub()[0];
    auto nodes_for_edge    = tree_structure.sub()[1];
    int n_nodes            = tree_structure.sub()[2].as_int();
    int n_branches         = n_nodes - 1;

    if (log_verbose >= 3)
        std::cerr<<"num_branches = "<<C.evaluate_expression({var("Parameters.maybe_modifiable_structure"), {var("Tree.numBranches"), tree_exp}})<<"\n\n";;

    for(int n=0; n < n_nodes; n++)
    {
        auto edges = list_to_evector(edges_out_of_node.sub()[n]);
        assert(edges);

        vector<param> m_edges;
        for(auto& edge: *edges)
            m_edges.push_back(get_param(C,edge));

        parameters_for_tree_node.push_back ( m_edges );

        if (m_edges.size() < 2) n_leaves++;
    }

    for(int b=0; b < 2*n_branches; b++)
    {
        auto nodes = nodes_for_edge.sub()[b];

        assert(has_constructor(nodes,"(,,,)"));
        param m_source = get_param(C, nodes.sub()[0]);
        param m_source_index = get_param(C, nodes.sub()[1]);
        param m_target = get_param(C, nodes.sub()[2]);

        parameters_for_tree_branch.push_back( std::tuple<param, param, param>{m_source, m_source_index, m_target} );
    }
}

tree_constants::tree_constants(context_ref& C, const vector<string>& labels, const expression_ref& E)
    :tree_constants(C, E)
{
    node_labels = labels;
    int n_nodes = parameters_for_tree_node.size();
    assert(node_labels.size() == n_nodes);
}

std::optional<int> TreeInterface::branch_durations_array_reg() const
{
    return get_tree_constants().branch_durations_array_reg;
}

std::optional<int> TreeInterface::root_reg() const
{
    return get_tree_constants().root_reg;
}

std::optional<int> TreeInterface::node_times_array_reg() const
{
    return get_tree_constants().node_times_array_reg;
}

std::optional<int> TreeInterface::away_from_root_array_reg() const
{
    return get_tree_constants().away_from_root_array_reg;
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

vector<int> TreeInterface::leaf_branches() const
{
    vector<int> branches;
    for(int node=0; node<n_nodes(); node++)
    {
        if (degree(node) == 0) continue;

        int b = branch_out(node, 0);

        if (not is_leaf_branch(b)) continue;

        if (is_leaf_node(target(b)))
        {
            // both directions are leaf branches, just return one.
            if (source(b) > target(b))
                continue;
        }

        branches.push_back(b);
    }
    return branches;
}

int TreeInterface::n_branches() const {
    return get_tree_constants().parameters_for_tree_branch.size()/2;
}

int TreeInterface::degree(int n) const {
    if (branches_from_affected_node(n))
	return branches_from_affected_node(n)->size();
    else
	return get_tree_constants().parameters_for_tree_node[n].size();
}

int TreeInterface::branch_out(int n, int i) const
{
    if (branches_from_affected_node(n))
	return (*branches_from_affected_node(n))[i];
  
    return get_tree_constants().parameters_for_tree_node[n][i].get_value(get_const_context()).as_int();
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
    return std::get<0>(get_tree_constants().parameters_for_tree_branch[b]).get_value(get_const_context()).as_int();
}

int TreeInterface::source_index(int b) const {
    return std::get<1>(get_tree_constants().parameters_for_tree_branch[b]).get_value(get_const_context()).as_int();
}

int TreeInterface::target(int b) const {
    return std::get<2>(get_tree_constants().parameters_for_tree_branch[b]).get_value(get_const_context()).as_int();
}

int TreeInterface::undirected(int b) const {
    return (b % n_branches());
}

bool TreeInterface::is_connected(int n1, int n2) const
{
    return (search_branch(n1, n2) != -1);
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

bool TreeInterface::has_root() const
{
    if (root_reg())
        return true;
    else
        return false;
}

int TreeInterface::root() const
{
    int r = *root_reg();
    return get_const_context().evaluate_reg(r).as_int();
}

bool TreeInterface::away_from_root(int b) const
{
    assert(has_root());

    int array_reg = *away_from_root_array_reg();

    auto& C = get_const_context();

    auto& M = C.get_memory();

    int r = M[array_reg].reg_for_slot(b);

    return is_bool_true(C.evaluate_reg(r));
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
    if (branch_durations_array_reg())
        return true;
    else
        return false;
}

double TreeInterface::branch_length(int b) const
{
    b %= n_branches();

    int array_reg = *branch_durations_array_reg();

    auto& C = get_const_context();

    auto& M = C.get_memory();
    int r = M[array_reg].reg_for_slot(b);

    return C.evaluate_reg(r).as_double();
}

bool TreeInterface::can_set_branch_length(int b) const
{
    b %= n_branches();

    int array_reg = *branch_durations_array_reg();

    auto& C = get_const_context();

    auto& M = C.get_memory();
    int r = M[array_reg].reg_for_slot(b);

    auto m = C.find_modifiable_reg(r);
    return bool(m);
}

void TreeInterface::set_branch_length(int b, double l)
{
    b %= n_branches();

    int array_reg = *branch_durations_array_reg();

    auto& C = get_context();

    auto& M = C.get_memory();
    int r = M[array_reg].reg_for_slot(b);

    auto m = C.find_modifiable_reg(r);
    C.set_modifiable_value(*m, l);
}

bool TreeInterface::has_node_times() const
{
    if (node_times_array_reg())
        return true;
    else
        return false;
}

double TreeInterface::node_time(int n) const
{
    int array_reg = *node_times_array_reg();

    auto& C = get_const_context();

    auto& M = C.get_memory();
    int r = M[array_reg].reg_for_slot(n);

    return C.evaluate_reg(r).as_double();
}

bool TreeInterface::can_set_node_time(int n) const
{
    int array_reg = *node_times_array_reg();

    auto& C = get_const_context();

    auto& M = C.get_memory();
    int r = M[array_reg].reg_for_slot(n);

    auto m = C.find_modifiable_reg(r);
    return bool(m);
}

void TreeInterface::set_node_time(int n, double t)
{
    int array_reg = *node_times_array_reg();

    auto& C = get_context();

    auto& M = C.get_memory();
    int r = M[array_reg].reg_for_slot(n);

    auto m = C.find_modifiable_reg(r);
    C.set_modifiable_value(*m, t);
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

std::vector<int>& ParametersTreeInterface::affected_nodes()
{
    return const_cast<Parameters*>(P)->affected_nodes;
}

const std::vector<int>& ParametersTreeInterface::affected_nodes() const
{
    return P->affected_nodes;
}

std::vector<std::vector<int>*>& ParametersTreeInterface::branches_from_affected_nodes()
{
    return const_cast<Parameters*>(P)->branches_from_affected_node;
}

const std::vector<std::vector<int>*>& ParametersTreeInterface::branches_from_affected_nodes() const
{
    return P->branches_from_affected_node;
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

std::vector<int>& ModifiablesTreeInterface::affected_nodes()
{
    return affected_nodes_;
}

const std::vector<int>& ModifiablesTreeInterface::affected_nodes() const
{
    return affected_nodes_;
}

std::vector<std::vector<int>*>& ModifiablesTreeInterface::branches_from_affected_nodes()
{
    return branches_from_affected_nodes_;
}

const std::vector<std::vector<int>*>& ModifiablesTreeInterface::branches_from_affected_nodes() const
{
    return branches_from_affected_nodes_;
}

ModifiablesTreeInterface::ModifiablesTreeInterface(context_ref& c, int tree_reg)
    :C(c),
     TC(new tree_constants(c,reg_var(tree_reg))),
     branches_from_affected_nodes_(n_nodes())
{
}

ModifiablesTreeInterface::~ModifiablesTreeInterface()
{
    delete TC;
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
std::string write(const TreeInterface& T, int root, const std::vector<double>& L, const std::vector<std::string>& names)
{
    vector<int> branches = T.all_branches_from_node(root);
    vector<string> names2 = names;
    for(int b: branches)
    {
	int n = T.target(b);
	names2[n] = names2[n] +":" + std::to_string(L[T.undirected(b)]);
    }
    return write(T, root, names2);
}

/// Return a Newick string representation of the tree 'T' with names 'names', including branch lengths by default.
std::string write(const TreeInterface& T, const std::vector<std::string>& names, bool print_lengths)
{
    int root = 0;
    if (T.n_nodes() > 2)
	root = T.target(0);

    vector<string> names2;
    for(auto& name: names)
	names2.push_back(escape_for_newick(name));
    if (print_lengths)
    {
	vector<double> L;
	for(int i=0;i<T.n_branches();i++)
	    L.push_back(T.branch_length(i));
	return write(T, root, L, names2);
    }
    else
	return write(T, root, names2);
}

vector<int> edges_connecting_to_node(const Tree& T, int n);

/*
 * Here, we fix up the nodes in the Haskell tree 
 */
void TreeInterface::read_tree_node(const Tree& T, int n)
{
    assert(get_tree_constants().parameters_for_tree_node[n].size() == T.node(n).degree());

    // These are the edges we seek to impose.
    vector<int> edges = edges_connecting_to_node(T,n);
    assert(edges.size() == T.node(n).degree());

    // These are the current edges.
    for(int i=0;i<edges.size();i++)
    {
	int b = edges[i];
	get_tree_constants().parameters_for_tree_node[n][i].set_value(get_context(), b);
	std::get<1>(get_tree_constants().parameters_for_tree_branch[b]).set_value(get_context(), i);
    }
}

void TreeInterface::read_tree(const Tree& T)
{
    for(int n=0; n < T.n_nodes(); n++)
        read_tree_node(T, n);

    for(int b=0; b < 2*T.n_branches(); b++)
    {
        std::get<0>(get_tree_constants().parameters_for_tree_branch[b]).set_value(get_context(), (int)T.directed_branch(b).source());
        std::get<2>(get_tree_constants().parameters_for_tree_branch[b]).set_value(get_context(), (int)T.directed_branch(b).target());
    }
}

void TreeInterface::reconnect_branch(int s1, int t1, int t2)
{
    int b1 = find_branch(s1,t1);
    // Handle case where we have a loop from a leaf node s1 to a leaf node s1 -- ensure that we select the original branch away from s1.
    if (is_leaf_node(s1))
    {
        int b1_ = s1;
        if (source(b1_) == s1 and target(b1_) == t1)
            b1 = b1_;
    }
    if (is_leaf_node(t1))
    {
        std::abort();
    }
    int b2 = reverse(b1);

    if (not branches_from_affected_node(t1))
    {
	affected_nodes().push_back(t1);
	auto v = new vector<int>();
	for(int i=0; i<degree(t1); i++)
	    v->push_back(branch_out(t1, i));
	branches_from_affected_node(t1) = v;
    }
    remove_element(*branches_from_affected_node(t1), b2);

    if (not branches_from_affected_node(t2))
    {
	affected_nodes().push_back(t2);
	auto v = new vector<int>();
	for(int i=0; i<degree(t2); i++)
	    v->push_back(branch_out(t2, i));
	branches_from_affected_node(t2) = v;
    }
    branches_from_affected_node(t2)->push_back(b2);

    // Update branch source and target nodes
    std::get<2>(get_tree_constants().parameters_for_tree_branch[b1]).set_value(get_context(), t2);
    std::get<0>(get_tree_constants().parameters_for_tree_branch[b2]).set_value(get_context(), t2);
}

void TreeInterface::begin_modify_topology()
{
#ifndef NDEBUG
    for(auto p: branches_from_affected_nodes())
	assert(not p);
    assert(affected_nodes().empty());
#endif
}

void TreeInterface::end_modify_node(int n)
{
    assert(branches_from_affected_node(n));
    assert(get_tree_constants().parameters_for_tree_node[n].size() == branches_from_affected_node(n)->size());

    // These are the current edges.
    const auto& branches = *branches_from_affected_node(n);

    assert(branches.size() == get_tree_constants().parameters_for_tree_node[n].size());
    for(int i=0;i<branches.size();i++)
    {
	int b = branches[i];
	get_tree_constants().parameters_for_tree_node[n][i].set_value(get_context(), b);
	std::get<1>(get_tree_constants().parameters_for_tree_branch[b]).set_value(get_context(), i);
    }

    delete branches_from_affected_node(n);
    branches_from_affected_node(n) = nullptr;
}

void TreeInterface::end_modify_topology()
{
    for(int n: affected_nodes())
	end_modify_node(n);
  
    affected_nodes().clear();

#ifndef NDEBUG
    for(auto p: branches_from_affected_nodes())
	assert(not p);
    assert(affected_nodes().empty());
#endif
}

#ifndef NDEBUG
void check_tree(const Tree& T, const TreeInterface& t)
{
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
}
#else
void check_tree(const Tree&, const TreeInterface&)
{
}
#endif


// This could create loops it we don't check that the subtrees are disjoint.
// br{1,2} point out of the subtrees.  b{1,2} point into the subtrees, towards the other subtree.
void NNI(TreeInterface& T, int br1, int br2)
{
    int b1 = T.reverse(br1);
    int b2 = T.reverse(br2);

    int s1 = T.source(b1);
    int t1 = T.target(b1);

    int s2 = T.source(b2);
    int t2 = T.target(b2);

    //  assert(not t().subtree_contains(br1,s2));
    //  assert(not t().subtree_contains(br2,s1));

    T.begin_modify_topology();
    T.reconnect_branch(s1,t1,t2);
    T.reconnect_branch(s2,t2,t1);
    T.end_modify_topology();
}
