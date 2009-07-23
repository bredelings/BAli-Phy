#include "mctree.H"
#include "mytypes.H"

using namespace std;

// Actually, this assumes that connected-ness is a clique relation.
// This function doesn't find cliques in a general connectedness matrix.

vector<int> get_cliques(const ublas::matrix<int>& connected)
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

MC_tree::MC_tree(const vector<Partition>& p)
  :N(-1),
   C(-1),
   partitions(p)
{
  if (not p.size())
    throw myexception()<<"Can't create an MC tree from an empty partition list.";

  names_ = p[0].names;

  // remove uninformative branches
  for(int i=0;i<partitions.size();)
    if (not informative(partitions[i]))
      partitions.erase(partitions.begin()+i);
    else
      i++;

  // adds leaf branches
  for(int i=0;i<n_leaves();i++) {
    valarray<bool> m(true,n_leaves());
    m[i] = false;
    partitions.insert(partitions.begin()+i,Partition(names(),m));
  }

  N = partitions.size();
  reverse_.resize(2*N,-1);

  // Split full and partial partitions into groups
  vector<Partition> full;
  vector<Partition> partial;
  for(int i=0;i<partitions.size();i++)
    if (partitions[i].full())
      full.push_back(partitions[i]);
    else
      partial.push_back(partitions[i]);

  // this SHOULD already have standardized (sorted) leaf order from the partitions
  T = get_mf_tree(names_, full);
  assert(full.size() == T.n_branches());

  // Add the full partitions
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
    partitions.push_back(partial[i].reverse());

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
  valarray<bool> visited(false,partitions.size());
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

    edges.push_back(edge(n1,n2,1,b));
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
	edges.push_back(edge(i,j,2,-1));
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

bool directed_equal(const Partition& p1, const Partition& p2)
{
  return 
    ((p1.names == p2.names) and 
    equal(p1.group1,p2.group1) and
    equal(p1.group2,p2.group2)
    );
}

int MC_tree::find_branch(const Partition& p) const
{
  for(int i=0;i<partitions.size();i++)
    if (directed_equal(p, partitions[i]))
      return i;

  return -1;
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

    o<<T.partitions[b]<<endl;
  }
  return o;
}


// FIXME - page="8.5,11" ?
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
    const edge& e = T.edges[i];
    cout<<"      N"<<e.from<<" -> N"<<e.to;

    vector<string> attributes;
    vector<string> styles;

    if (e.type == 1) {
      attributes.push_back("arrowhead=none");
      const Partition& p = T.partitions[e.partition];
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

MC_tree load_MC_tree(const std::string& filename)
{
  //----------- Load Partitions ---------------//
  vector<vector<Partition> > partitions1;
  load_partitions(filename, partitions1);

  if (not partitions1.size())
    throw myexception()<<"Can't create an MC tree from an empty partition list.";

  vector<Partition> partitions = partitions1[0];
  //  check_partitions_informative(partitions);

  return MC_tree(check_MC_partitions(partitions));
}

vector<Partition> check_MC_partitions(const vector<Partition>& partitions)
{
  //--------- Check Partitions --------------//
  vector<string> names = partitions[0].names;
  for(int i=0;i<partitions.size();i++) {
    
    if (partitions[i].size() != names.size())
      throw myexception()<<"Partition "<<i+1<<" has "<<partitions[i].size()<<" taxa instead of "<<partitions[0].size();
    
    if (partitions[i].names != names)
      throw myexception()<<"Partition "<<i+1<<" has different taxa than partition 1!";
  }

  //---- Throw out conflicting partitions ----//
  vector<Partition> mc_partitions = partitions;
  mc_partitions = get_moveable_tree(partitions);
  // check that the tree is an MC tree

  vector<Partition> i1 = select(partitions,informative);
  vector<Partition> i2 = select(mc_partitions,informative);
  int full = count(i2,&Partition::full);
  int total = i2.size();
  int partial = total - full;
  if (i1.size() != i2.size() and log_verbose)
    cerr<<"mctree.C: Removing "<<i1.size() - i2.size()<<"/"<<i1.size()<<" informative partitions to yield an MC  tree."<<endl;
  if (log_verbose)
    cerr<<"mctree.C: There are "<<partial<<" (partial) + "<<full<<" (full) = "<<total<<" (total) informative partitions."<<endl;
  
  return mc_partitions;
}

void check_partitions_informative(const vector<Partition>& partitions)
{
  //--------- Check Partitions --------------//
  for(int i=0;i<partitions.size();i++)
    if (not informative(partitions[i]))
      throw myexception()<<"Partition "<<i+1<<" is not informative.";
}

string get_graph_name(string filename)
{
  return remove_extension(get_basename(filename));
}

