/*
  Copyright (C) 2007-2009 Benjamin Redelings

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

#include "mctree.H"
#include "util/rng.H"
#include "util/range.H"
#include "util/string/join.H" 
#include "util/string/convert.H"
#include <filesystem>

extern int log_verbose;

using std::vector;
using std::string;
using std::cout;
using std::cerr;
using std::endl;
using std::ostream;

using boost::dynamic_bitset;

namespace fs = std::filesystem;

vector<int> MC_tree::map_graph_nodes_to_tree_nodes()
{
    //---------------------- map graph nodes to tree nodes ----------------//
    vector<int> graph_node_to_tree_node_mapping(C,-1);

    // first label nodes corresponding to full splits in the tree
    for(int i=0;i<partitions.size();i++) 
    {
	int graph_node = mapping[i];
	if (graph_node_to_tree_node_mapping[graph_node] != -1) continue;

	// find which branch of the tree, if any, this partition corresponds to.
	int b = which_branch(T,partitions[i]);
	if (b == -1) continue;
    
	int tree_node = T.directed_branch(b).target();
	graph_node_to_tree_node_mapping[graph_node] = tree_node;
    }

    // then propagate labels to cover partial splits
    bool fully_labelled = true;
    for(int i=0;i<partitions.size();i++)
    {
	fully_labelled = true;
	for(int i=0;i<partitions.size();i++) 
	{
	    int graph_node1 = mapping[i];
	    int graph_node2 = mapping[reverse(i)];

	    // For partial splits, propagate the node name from the other end of the branch
	    if (graph_node_to_tree_node_mapping[graph_node2] == -1) {
		graph_node_to_tree_node_mapping[graph_node2] = graph_node_to_tree_node_mapping[graph_node1];
		fully_labelled = false;
	    }

	    // For full or partial splits, propagate our node name onto nodes we wander over
	    if (directly_wanders[i] and graph_node_to_tree_node_mapping[graph_node1] != -1) {
		for(int j=0;j<partitions.size();j++) 
		    if (directly_wanders_over(i,j)) {
			int graph_node3 = mapping[j];
			int graph_node4 = mapping[reverse(j)];
			graph_node_to_tree_node_mapping[graph_node3] = graph_node_to_tree_node_mapping[graph_node1];
			graph_node_to_tree_node_mapping[graph_node4] = graph_node_to_tree_node_mapping[graph_node1];
		    }
	    }

	    // check: branch endpoints should have the same names if and only if the branches have a full split
	    if (graph_node_to_tree_node_mapping[graph_node1] != -1 and
		graph_node_to_tree_node_mapping[graph_node2] != -1)
	    {
		if (partitions[i].full())
		    assert(graph_node_to_tree_node_mapping[graph_node1] != graph_node_to_tree_node_mapping[graph_node2]);
		else
		    assert(graph_node_to_tree_node_mapping[graph_node1] == graph_node_to_tree_node_mapping[graph_node2]);
	    }
	}
	if (fully_labelled) break;
    }
    if (!fully_labelled) throw myexception()<<"Mapping tree nodes to graph nodes failed for multiconnected tree.";

    return graph_node_to_tree_node_mapping;
}


// Actually, this assumes that connected-ness is a clique relation.
// This function doesn't find cliques in a general connectedness matrix.

vector<int> get_cliques(const matrix<int>& connected)
{
    const int N = connected.size1();
    vector<int> mapping(N,-1);

    int n_cliques = 0;

    for(int i=0;i<N;i++) {
	if (mapping[i] != -1) continue;

	vector<int> nodes;
	nodes.push_back(i);
	mapping[i] = n_cliques;

	for(int j=0;j<N;j++)
	    if (connected(i,j)) {
		assert(connected(j,i));
		assert(mapping[j] == -1);
		mapping[j] = n_cliques;
		nodes.push_back(j);
	    }

	for(int j=0;j<nodes.size();j++)
	    for(int k=0;k<j;k++)
		assert(connected(nodes[j],nodes[k]));

	n_cliques++;
    }

    for(int i=0;i<N;i++)
	assert(mapping[i] != -1);

    return mapping;
}

MC_tree::MC_tree(const PTree& tree)
    :N(-1),
     C(-1),
     names_(tree.names),
     partitions(tree.partitions)
{
    // Remove uninformative branches, then add leaf branches in front
    add_leaf_partitions(names(), partitions);

    N = partitions.size();
    reverse_.resize(2*N,-1);

    // Split full and partial partitions into groups
    vector<partition> full;
    vector<partition> partial;
    for(int i=0;i<partitions.size();i++)
	if (partitions[i].full())
	    full.push_back(partitions[i]);
	else
	    partial.push_back(partitions[i]);

    // this SHOULD already have standardized (sorted) leaf order from the partitions
    T = get_mf_tree(names_, full);
    assert(full.size() == T.n_branches());

    // Add the full partitions - from the tree
    partitions.clear();
    for(int b=0;b<2*T.n_branches();b++) {
	partitions.push_back(partition_from_branch(T,b));
	reverse_[b] = T.directed_branch(b).reverse();
    }

    // Add the partial partitions
    for(int i=0;i<partial.size();i++) {
	int B1 = partitions.size();
	int B2 = B1+1;

	partitions.push_back(partial[i]);
	partitions.push_back(::reverse(partial[i]));

	reverse_[B1] = B2;
	reverse_[B2] = B1;
    }

    assert(partitions.size() == 2*N);

    // left_of
    left_of.resize(partitions.size(), partitions.size());
    for(int i=0;i<partitions.size();i++)
	for(int j=0;j<partitions.size();j++)
	    left_of(i,j) = partition_less_than(partitions[i],partitions[j])?1:0;
  
    // wanders_over
    wanders_over.resize(partitions.size(),partitions.size());
    for(int i=0;i<partitions.size();i++)
	for(int j=0;j<partitions.size();j++)
	    wanders_over(i,j) = partition_wanders_over(partitions[i],partitions[j])?1:0;
  
    // directly_left_of
    directly_left_of.resize(partitions.size(),partitions.size());
    directly_left_of = left_of;
    for(int i=0;i<partitions.size();i++)
	for(int j=0;j<partitions.size();j++)
	    if (left_of(i,j))
		for(int k=0;k<partitions.size();k++)
		    if (left_of(i,k) and left_of(k,j))
			directly_left_of(i,j)=0;

    // directly_wanders_over
    directly_wanders_over.resize(partitions.size(),partitions.size());
    directly_wanders_over = wanders_over;
    for(int i=0;i<partitions.size();i++)
	for(int j=0;j<partitions.size();j++)
	    if (wanders_over(i,j))
		for(int k=0;k<partitions.size();k++)
		    if (left_of(i,k) and wanders_over(k,j))
			directly_wanders_over(i,j)=0;
  
    // directly wanders
    directly_wanders = vector<int>(partitions.size(),0);
    for(int i=0;i<partitions.size();i++)
	for(int j=0;j<partitions.size();j++)
	    if (directly_wanders_over(i,j))
		directly_wanders[i]++;

    for(int i=0;i<directly_wanders.size();i++)
	directly_wanders[i] /= 2;

    // connected_to
    connected_to.resize(partitions.size(),partitions.size());
    for(int i=0;i<partitions.size();i++)
	for(int j=0;j<partitions.size();j++)
	    if (directly_wanders[i] or directly_wanders[j])
		connected_to(i,j)=0;
	    else
		connected_to(i,j) = directly_left_of(i, reverse(j) );
  
    /*
      for(int i=0;i<partitions.size();i++)
      for(int j=0;j<i;j++)
      if (connected_to(i,j)) {
      cerr<<"("<<partitions[i]<<") <-> ("<<partitions[j]<<")\n";
      assert(connected_to(j,i));
      }
    */

    // map nodes to clique indices
    mapping = get_cliques(connected_to);
  
    C = max(mapping)+1;

    // clear connected
    connected.resize(C, C);
    for(int i=0;i<C;i++)
	for(int j=0;j<C;j++)
	    connected(i,j) = 0;

    // add 1 edge (not 2) for each partition
    dynamic_bitset<> visited(partitions.size());
    for(int i=0;i<partitions.size();i++) 
    {
	if (visited[i]) continue;
	visited[i] = true;
	visited[reverse(i)] = true;

	branch_order.push_back(i);
    }

    assert(branch_order.size() == n_branches());


    for(int i=0;i<branch_order.size();i++)
    {
	int b = branch_order[i];

	assert(not connected_to(b,reverse(b)));

	int n1 = mapping[b];
	int n2 = mapping[reverse(b)];

	assert(n1 != n2);

	connected(n1, n2) = 1;
	connected(n2, n1) = 1;

	edges.push_back(mc_tree_edge(n1,n2,1,b));
    }

    // mark connection possibilities for wandering edges
    for(int i=0;i<partitions.size();i++) 
	for(int j=0;j<partitions.size();j++) 
	    if (directly_wanders_over(i,j)) {
		assert(directly_wanders_over(i,reverse(j)));
		connected(mapping[i],mapping[j])=2;
	    }
  
    // add a wandering edge for each connection point
    for(int i=0;i<C;i++)
	for(int j=0;j<C;j++)
	    if (connected(i,j)==2)
		edges.push_back(mc_tree_edge(i,j,2,-1));

    //---------------------- map graph nodes to tree nodes ----------------//
    graph_node_to_tree_node_ = map_graph_nodes_to_tree_nodes();
}

int MC_tree::branch_to_node(int n) const
{
    if (n >= n_nodes())
	throw myexception()<<"I only have "<<n_nodes()<<" nodes, can't handle node "<<n;
    for(int i=0;i<2*n_branches();i++)
    {
	if (mapping[i] == n)
	    return i;
    }
    throw myexception()<<"Couldn't find any branches pointing to node '"<<n<<"'";
}

int MC_tree::find_branch(const partition& p) const
{
    for(int i=0;i<partitions.size();i++)
	if (directed_equals(p, partitions[i]))
	    return i;

    return -1;
}

int MC_tree::score() const 
{
    int total=0;

    for(int i=0;i<n_branches();i++)
    {
	int b = branch_order[i];

	if (informative(partitions[b]))
	    total += partitions[b].mask().count();
    }

    return total;
}

int MC_tree::degree(int n) const
{
    unsigned count=0;
    for(int b=0;b<mapping.size();b++)
	if (mapping[b] == n and not directly_wanders[b])
	    count++;
    return count;
}

bool MC_tree::is_leaf_node(int n) const
{
    return (degree(n) == 1);
}

int MC_tree::leaf(int i) const
{
    return mapping[reverse(i)];
}

ostream& operator<<(ostream& o, const MC_tree& T)
{
    o<<T.T.write(false)<<endl;

    for(int i=0;i<T.branch_order.size();i++)
    {
	int b = T.branch_order[i];

	if (T.partitions[b].full()) continue;

	o<<Partition(T.names(),T.partitions[b])<<endl;
    }
    return o;
}


// FIXME - page="8.5,11" ?

/// \brief Generate a graphviz DOT file for the multiconnected tree
///
/// \param T The multiconnected tree
/// \param name The figure name
///
void draw_graph(const MC_tree& T,const string& name)
{
    const int N = T.n_nodes();

    cout<<"digraph "<<name<<" { \n\
\n\
      nodesep=1.0\n\
      ratio=auto\n\
\n\
      edge[style=\"setlinewidth(2)\"]\n\
      node[shape=plaintext,width=auto,fontname=Helvitica,fontsize=10]\n\n";

    // edges
    for(int i=0;i<T.edges.size();i++) {
	const mc_tree_edge& e = T.edges[i];
	cout<<"      N"<<e.from<<" -> N"<<e.to;

	vector<string> attributes;
	vector<string> styles;

	if (e.type == 1) {
	    attributes.push_back("arrowhead=none");
	    const partition& p = T.partitions[e.partition];
	    if (informative(p))
		styles.push_back("setlinewidth(2)");
	    if (not p.full())
		attributes.push_back("color=green2");
	}
	else {
	    styles.push_back("dashed");
	    int b = T.branch_to_node(e.from);
	    double w = 1.0/(T.directly_wanders[b] + 1);
	    attributes.push_back(string("weight=")+convertToString(w));
	    attributes.push_back(string("w=")+convertToString(w));
	}

	if (styles.size()) {
	    string style = "style=\"" + join(styles,',') + "\"";
	    attributes.push_back(style);
	}
    
	cout<<" ["<<join(attributes,',')<<"]";
	cout<<"\n";
    }
    cout<<endl;

    // leaf names
    vector<string> names = T.names();
    for(int i=0;i<names.size();i++)
	cout<<"      N"<<T.leaf(i)<<" [label=\""<<names[i]<<"\"]\n";
    cout<<endl;

    for(int i=0;i<N;i++)
	if (not T.is_leaf_node(i))
	    cout<<"      N"<<i<<" [label=\"\",shape=circle,hight=0.02,width=0.02,fontsize=1]\n";
    cout<<endl;

    cout<<"}\n";

}

/// \brief Load a multiconnected tree from file 'filename'
///
/// \param filename The name of the file
MC_tree load_MC_tree(const std::string& filename)
{
    //----------- Load Partitions ---------------//
    auto trees = load_partitions(filename);

    return MC_tree(check_MC_partitions(trees[0]));
}

PTree check_MC_partitions(const PTree& tree)
{
    return check_MC_partitions(tree.names, tree.partitions);
}

PTree check_MC_partitions(const vector<string>& names, const vector<partition>& partitions)
{
    //--------- Check Partitions --------------//
    for(int i=1;i<partitions.size();i++) 
	if (partitions[i].size() != names.size())
	    throw myexception()<<"Partition "<<i+1<<" has "<<partitions[i].size()<<" taxa instead of "<<partitions[0].size();

    //---- Throw out conflicting partitions ----//
    vector<partition> mc_partitions = partitions;
    mc_partitions = get_moveable_tree(partitions);
    // check that the tree is an MC tree

    vector<partition> i1 = select(partitions,informative);
    vector<partition> i2 = select(mc_partitions,informative);
    int full = count(i2,&partition::full);
    int total = i2.size();
    int partial = total - full;
    if (i1.size() != i2.size() and log_verbose)
	cerr<<"mctree.C: Removing "<<i1.size() - i2.size()<<"/"<<i1.size()<<" informative partitions to yield an MC  tree."<<endl;
    if (log_verbose)
	cerr<<"mctree.C: There are "<<partial<<" (partial) + "<<full<<" (full) = "<<total<<" (total) informative partitions."<<endl;
  
    return {names,mc_partitions};
}

void check_partitions_informative(const vector<partition>& partitions)
{
    //--------- Check Partitions --------------//
    for(int i=0;i<partitions.size();i++)
	if (not informative(partitions[i]))
	    throw myexception()<<"Partition "<<i+1<<" is not informative.";
}

string get_graph_name(string filepath)
{
    return fs::path(filepath).filename().stem().string();
}

bool partition_wanders_over(const partition& p1,const partition& p2)
{
    return p2.group1.is_subset_of(p1.group2) and p2.group2.is_subset_of(p1.group2);
}

bool partition_less_than(const partition& p1,const partition& p2)
{
    return 
	p1.group1.is_proper_subset_of(p2.group1) and 
	p2.group2.is_proper_subset_of(p1.group2);
}

/// \brief Check that splits p1 and p2 related in a valid way (pairwise)
///
/// \param p1 The first split
/// \param p2 The second split
///
bool sub_conflict(partition p1,partition p2)
{
    if (not p1.mask().intersects(p2.mask()))
	return false;

    if (partition_less_than(p1,p2) or partition_less_than(p1,reverse(p2)) or
	partition_less_than(reverse(p1),p2) or partition_less_than(reverse(p1),reverse(p2)))
	return false;

    if (partition_wanders_over(p1,p2) or partition_wanders_over(reverse(p1),p2) or
	partition_wanders_over(p2,p1) or partition_wanders_over(reverse(p2),p1))
	return false;

    return true;
}

bool is_leaf_partition(const partition& p)
{
    return p.full() and (p.group1.count() == 1 or (p.group2.count() == 1));
}

int get_n_conflicts(const matrix<int>& conflicts,
		    int n,
		    const dynamic_bitset<>& mask)
{
    assert(mask.size() == conflicts.size1());
    assert(mask.size() == conflicts.size2());

    int total = 0;
    for(int i=0;i<mask.size();i++)
	if (mask[i] and conflicts(n,i))
	    total++;

    return total;
}

// How do we find an optimal set of resolved partitions here?
// We can now discover more partitions with lots of wandering, so
//  branches can wander further.
// How about... prefer branches that wander over the fewest number of other branches
//  + how do we weight wandering versus conflicting?  That is, if a branch conflicts
//    with fewer branches, but 


// I guess the over-all goal is (could be) to find an MC tree that has the smallest
// number of BF trees extending it...


std::pair<dynamic_bitset<>, int> solve_conflicts(const matrix<int>& conflicts,
						 const matrix<int>& dominates,
						 dynamic_bitset<> invincible,
						 const vector<int>& goodness)
{
    const int N = invincible.size();
    dynamic_bitset<> survives(N);
    survives.flip();

    // we should be able to GENERATE restricted version of splits that might be interesting.
    for(int i=0;i<N;i++)
	for(int j=0;j<N;j++)
	    if (dominates(i,j))
		survives[j] = false;

    for(int i=0;i<N;i++)
	for(int j=0;j<N;j++)
	    if (conflicts(i,j) and invincible[i])
		survives[j] = false;

//    int n=0;
//    for(int i=0;i<N;i++)
//	if (invincible[i])
//	    n++;

    do {
	vector<int> n_conflicts(N,0);

	// We would LIKE to find the largest of the branches that this branch conflicts
	// with that do not conflict with each other.

	int m = 0;
	vector<int> maxes;
	for(int i=0;i<N;i++)
	    if (survives[i] and not invincible[i]) 
	    {
		// here we find out how many branches each branch conflicts with...
		n_conflicts[i]  = get_n_conflicts(conflicts,i,survives);

		// .. that aren't sub-branches of itself.
		// DOES THIS HELP?
		n_conflicts[i] -= get_n_conflicts(dominates,i,survives);
		assert(n_conflicts[i] >= 0);

		// HOWEVER...we DO double-count sub-branches of neighbors.
		if (n_conflicts[i] > m) {
		    m = n_conflicts[i];
		    maxes.clear();
		}

		if (n_conflicts[i] == m)
		    maxes.push_back(i);
	    }

	// stop removing partitions if largest number of conflicts is 0.
	if (m == 0) break;


	int die_index = uniform_int(0, maxes.size()-1 );
	if (die_index >= maxes.size()) die_index--;

	int die = maxes[die_index];

	survives[die] = false;

    } while(true);

    int score = 0;
    for(int i=0;i<N;i++)
	if (survives[i])
	    score += goodness[i];

    if (log_verbose)
	cerr<<"solution: score = "<<score<<endl;


    return std::pair< dynamic_bitset<>,int>(survives,score);
}

/// \brief Find the largest subset of the given splits that contains no conflicts
///
/// \param partitions The collection of (possibly partial) splits
vector<partition> get_moveable_tree(vector<partition> partitions)
{
    // remove partitions that are implied by other partitions
    for(int i=partitions.size()-1;i>=0;i--) 
    {
	bool found = false;
	for(int j=partitions.size()-1;j>=0 and not found;j--)
	    if (j != i and implies(partitions[j],partitions[i]))
		found = true;

	if (found)
	    partitions.erase(partitions.begin() + i);
    }

    const int N = partitions.size();

    // create and zero conflict matrix
    matrix<int> conflict(N,N);
    matrix<int> dominates(N,N);

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
		if (conflict(i,j) and partitions[j].mask().is_proper_subset_of(partitions[i].mask()))
		    dominates(i,j) = 1;
	    }
  
    // we can't remove leaf partitions
    dynamic_bitset<> invincible(N);
    for(int i=0;i<N;i++)
	invincible[i] = partitions[i].full(); //is_leaf_partition(partitions[i]);

    vector<int> goodness(N,0);
    for(int i=0;i<goodness.size();i++)
	if (informative(partitions[i]))
	    goodness[i] = partitions[i].mask().count();

    dynamic_bitset<> solution;
    int score = 0;
    for(int i=0;i<100;i++) 
    {
	std::pair<dynamic_bitset<>,int> s_pair = solve_conflicts(conflict,dominates,invincible,goodness);
	if (i==0) solution = s_pair.first;
	else if (s_pair.second > score) 
	{
	    score = s_pair.second;
	    solution = s_pair.first;
	}
    }

    vector<partition> moveable;
    for(int i=0;i<solution.size();i++)
	if (solution[i])
	    moveable.push_back(partitions[i]);

    return moveable;
}
