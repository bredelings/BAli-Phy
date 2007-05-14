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
  // add reversed branches
  const int N = partitions.size();
  for(int i=0;i<N;i++)
    partitions.push_back(partitions[i].reverse());

  // left_of
  ublas::matrix<int> left_of(2*N,2*N);
  for(int i=0;i<2*N;i++)
    for(int j=0;j<2*N;j++)
      left_of(i,j) = partition_less_than(partitions[i],partitions[j])?1:0;
  
  // wanders_over
  ublas::matrix<int> wanders_over(2*N,2*N);
  for(int i=0;i<2*N;i++)
    for(int j=0;j<2*N;j++)
      wanders_over(i,j) = partition_wanders_over(partitions[i],partitions[j])?1:0;
  
  // directly_left_of
  ublas::matrix<int> directly_left_of = left_of;
  for(int i=0;i<2*N;i++)
    for(int j=0;j<2*N;j++)
      if (left_of(i,j))
	for(int k=0;k<2*N;k++)
	  if (left_of(i,k) and left_of(k,j))
	    directly_left_of(i,j)=0;
  

  // directly_wanders_over
  ublas::matrix<int> directly_wanders_over = wanders_over;
  for(int i=0;i<2*N;i++)
    for(int j=0;j<2*N;j++)
      if (wanders_over(i,j))
	for(int k=0;k<2*N;k++)
	  if (left_of(i,k) and wanders_over(k,j))
	    directly_wanders_over(i,j)=0;
  
  // directly wanders
  vector<bool> directly_wanders(2*N,false);
  for(int i=0;i<2*N;i++)
    for(int j=0;j<2*N;j++)
      if (directly_wanders_over(i,j))
	directly_wanders[i] = true;

  // connected_to
  ublas::matrix<int> connected_to(2*N,2*N);
  for(int i=0;i<2*N;i++)
    for(int j=0;j<2*N;j++)
      if (directly_wanders[i] or directly_wanders[j])
	connected_to(i,j)=0;
      else
	connected_to(i,j) = directly_left_of(i, (j+N)%(2*N) );
  
  /*
    for(int i=0;i<2*N;i++)
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

  // connected
  ublas::matrix<int> connected(C,C);
  for(int i=0;i<C;i++)
    for(int j=0;j<C;j++)
      connected(i,j) = 0;

  for(int i=0;i<N;i++) {
    assert(not connected_to(i,i+N));
    assert(mapping[i] != mapping[i+N]);
    connected(mapping[i],mapping[i+N])=1;
    connected(mapping[i+N],mapping[i])=1;
    edges.push_back(edge(mapping[i],mapping[i+N],1,i));
  }

  for(int i=0;i<2*N;i++) 
    for(int j=0;j<2*N;j++) 
      if (directly_wanders_over(i,j)) {
	assert(directly_wanders_over(i,(j+N)%(2*N)));
	connected(mapping[i],mapping[j])=2;
      }
  
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

