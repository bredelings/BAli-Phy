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
  :partitions(p)
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
  directly_wanders = vector<bool>(partitions.size(),false);
  for(int i=0;i<partitions.size();i++)
    for(int j=0;j<partitions.size();j++)
      if (directly_wanders_over(i,j))
	directly_wanders[i] = true;

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
  
  int C = max(mapping)+1;

  n_nodes = C;

  // clear connected
  connected.resize(C, C);
  for(int i=0;i<C;i++)
    for(int j=0;j<C;j++)
      connected(i,j) = 0;

  // add 1 edge (not 2) for each partition
  valarray<bool> visited(partitions.size());
  for(int i=0;i<partitions.size();i++) 
  {
    assert(not connected_to(i,reverse(i)));
    if (visited[i]) continue;

    visited[i] = true;
    visited[reverse(i)] = true;

    int n1 = mapping[i];
    int n2 = mapping[reverse(i)];

    assert(n1 != n2);

    connected(n1, n2) = 1;
    connected(n2, n1) = 1;

    edges.push_back(edge(n1,n2,1,i));
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


// FIXME - page="8.5,11" ?
void draw_graph(const MC_tree& T,const string& name)
{
  const int N = T.n_nodes;

  cout<<"digraph "<<name<<" { \n\
\n\
      nodesep=1.0\n\
      ratio=auto\n\
\n\
      node[shape=plaintext,width=auto]\n\n";

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
      attributes.push_back("weight=0");
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
    cout<<"      N"<<i<<" [label=\""<<names[i]<<"\"]\n";
  cout<<endl;

  for(int i=names.size();i<N;i++)
    cout<<"      N"<<i<<" [label=\"\",shape=circle,hight=0.02,width=0.02,fontsize=1]\n";
  cout<<endl;

  cout<<"}\n";

}

