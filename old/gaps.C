#include <cmath>
#include "gaps.H"

struct edge {
  int node1;
  int node2;
  edge();
  edge(int n1,int n2): node1(n1),node2(n2) {}
};

bool operator==(const edge& e1, const edge& e2) {
  return (e1.node1 == e2.node1) && (e1.node2 == e2.node2);
}


const double gap_init = exp(-10);
const double gap_extend = exp(-1);

typedef vector<edge>  possibility;

vector<possibility> add(const vector<possibility>& v1,const vector<possibility>& v2) {
  vector<possibility> v3 = v1;
  v3.insert(v3.end(),v2.begin(),v2.end());
  return v3;
}


vector<possibility> multiply(const vector<possibility>& v1,const vector<possibility>& v2) {
  vector<possibility> v3;
  for(int i=0;i<v1.size();i++) {
    for(int j=0;j<v2.size();j++) {
      possibility p1 = v1[i];
      const possibility& p2 = v2[j];
      p1.insert(p1.begin(),p2.begin(),p2.end());
      v3.push_back(p1);
    }
  }
  return v3;
}

vector<edge> get_children(const tree& T,const edge& e,const TreeFunc<int>& present) {
  vector<edge> v;
  const node& n= T[e.node2];
  vector<int> neighbors = get_neighbors(n);
  for(int i=0;i<neighbors.size();i++) 
    if (neighbors[i] != e.node1 && present(neighbors[i])!= -1)
      v.push_back(edge(e.node2,neighbors[i]));

  return v;
}

vector<possibility> get_pos_subtree(const tree& T,const edge& e,const TreeFunc<int>& present) {
  vector<possibility> v;
  possibility p;
  p.push_back(e);
  v.push_back(p);

  vector<edge> children = get_children(T,e,present);
  if (children.size()==0)
    ;
  else if (children.size()==1)
    v = add(v,get_pos_subtree(T,children[0],present));
  else if (children.size() == 2) {
    v = add(v, multiply(get_pos_subtree(T,children[0],present),
			get_pos_subtree(T,children[1],present)) 
	    );
  }
  else
    assert(0); // This can't happen
  return v;
}



/* FIXME - 
           1. find largest and smallest LEAF node to be marked #1 
           2. find parent - this largest node marked #1, 
              because parent of all children marked #1
           3. to connect other nodes, simply walk up tree while
              current node not marked #1
           4. same with connecting #0 nodes
*/

TreeFunc<int> mark_tree2(const vector<int>& present_leaf,const tree& T) {
  //Step 0: Set all nodes as not-considered
  TreeFunc<int> present(T,-1);

  //Step 1: Load leaf information
  for(int i=0;i<present_leaf.size();i++) 
    present(i) = present_leaf[i];

  //Step 2: Connect the cluster of 'present' nodes
  int top = -1;
  for(int i=0;i<present_leaf.size();i++) {
    if (present(i) != 1) continue;

    if (top == -1)
      top = i;
    else {
      int here=i;
      while(!T.ancestor(here,top)) {
	here = T[here].parent->name;
	present(here) = 1;
      }
      int parent = here;
      
      here = top;
      while(here != parent) {
	here = T[here].parent->name;
	present(here) = 1;
      }
      
      if (parent>top) top=parent;
    }
  }
  
  assert(top != -1); //at least one node must be present

  // Step 3: connect the 'missing' nodes to the cluster of 'present' nodes
  for(int i=0;i<present_leaf.size();i++) {
    if (present_leaf[i] != 0) continue;

    int here=i;
    int parent;
    while(!T.ancestor(here,top)) {
      here = T[here].parent->name;
      if (present(here) != -1)
	goto done;
      present(here) = 0;
    }
    parent = here;
    
    here = top;
    while(here != parent) {
      here = T[here].parent->name;
      present(here) = 0;
    }
  done: continue;
  }

  return present;
}


TreeFunc<int> mark_tree(const vector<int>& present_leaf,const tree& T) {
  //Step 0: Set all nodes as not-considered
  TreeFunc<int> present(T,-1);

  //Step 1: Look at all pairs of present
  //         All nodes on paths connecting them are present also
  int present_node = -1;
  for(int i=0;i<present_leaf.size();i++) {
    present(i) = present_leaf[i];
    if (present_leaf[i] == 1) {
      present_node = i;
      break;
    }
  }
  assert(present_node != -1);  //at least one node must be present

  // connect the cluster of 'present' nodes
  for(int i=present_node+1;i<present_leaf.size();i++) {
    if (present(i) != 1) continue;

    vector<int> path = T.path(i,present_node);
    for(int j=0;j<path.size();j++) 
      present(path[j])=1;
  }

  // connect the 'missing' nodes to the cluster of 'present' nodes
  for(int i=0;i<present_leaf.size();i++) {
    if (present_leaf[i] != 0) continue;

    vector<int> path = T.path(i,present_node);
    for(int j=0;j<path.size();j++) 
      if (present(path[j]) == -1) 
	present(path[j])=0;
    
  }
  return present;
}



vector<possibility> get_possibilities(const vector<int>& present,const tree& T) {
  vector<possibility> v;
  bool any_missing=false;
  for(int i=0;i<present.size();i++) {
    if (present[i] == 0) {
      any_missing=true;
      break;
    }
  }
  if (!any_missing) return v;

  TreeFunc<int> f = mark_tree2(present,T);

  vector<edge> indel_edges;
  for(int i=0;i<T.num_nodes()-1;i++) {
    if (f(i) != 1) continue;

    vector<int> neighbors = get_neighbors(T[i]);

    for(int j=0;j<neighbors.size();j++) {
      int neighbor = neighbors[j];
      if (!f(neighbor))
	indel_edges.push_back(edge(i,neighbor)); // can't get added twice
    }
  }

  
  // Because of tree topology, each (directed) edge is the root of a subtree that is
  // not connected to the other subtrees.

  assert(indel_edges.size() >0);
  v = get_pos_subtree(T,indel_edges[0],f);

  for(int i=1;i<indel_edges.size();i++) 
    v = multiply(v,get_pos_subtree(T,indel_edges[i],f));


  return v;
}

int num_shared(const possibility& prev_edges,const possibility& edges) {
  int extended = 0;
  for(int l=0;l<edges.size();l++) 
    for(int m=0;m<prev_edges.size();m++)
      if (edges[l] == prev_edges[m]) {
	extended++;
	break;
      }
  return extended;
}

double sum(const possibility* edges,const vector<possibility>& prev,
	   const vector<double>& accum) {
  double sum=0;

  if (edges) {
    if (!prev.size())
      sum = pow(gap_init,edges->size());
    else {
      for(int i=0;i<prev.size();i++) {
	const possibility& prev_edges = prev[i];
	
	int extended = num_shared(prev_edges,*edges);
	int init = edges->size() - extended;
	sum += accum[i+1] * pow(gap_extend,extended) * pow(gap_init,init);
      }      
    }
  }
  else {
    if (!prev.size())
      sum = 1.0;
    else {
      for(int i=0;i<prev.size();i++)
	sum += accum[i+1];
    }
  }

  return sum;
}


void do_sum(vector<double>& accum,const vector<double>& accum2,
	    const vector<possibility>& possibilities,const vector<possibility>& possibilities2) {

  // for each configuration, sum over possibilites for previous column
  double total = 0.0;
  for(int i=1;i<accum.size();i++) {
    const possibility* edges = 0;
    if (possibilities.size())
      edges = &possibilities[i-1];
    accum[i] = sum(edges,possibilities2,accum2);
    total += accum[i];
  }

  for(int i=1;i<accum.size();i++) {
    accum[i] /= total;
  }
  accum[0] = accum2[0] + log(total);
}

vector<int> get_boundaries(const alignment& A,int first,int last) {

  const int sign = (first<=last)?1:-1;
  const int end = last + sign;

  vector<int> boundary(A.num_sequences(),end);

  bool done=false;
  for(int column=first;column != end;column += sign) {
    done = true;
    for(int i=0;i<A.num_sequences();i++) {
      if (boundary[i] != end) continue;
      done = false;

      if (A(column,i) == alphabet::gap) continue;
      for(int j=0;j<A.num_sequences();j++) {
	if (i==j) continue;
	if (A(column,j) == alphabet::gap && boundary[j] == end) continue;

	boundary[i] = column;
	break;
      }
    }
  }

  return boundary;
}


// Version 1.0
//  1. Ignore branch lengths - just use penalty!
//  2. Don't sum over possibilities - just take max!
//  3. FIXME: Can't yet deal with unaligned ends
double prior(const alignment& A,const tree& T) {

  assert(A.num_sequences() == T.leaves());
  vector<int> start = get_boundaries(A,0,A.length()-1);
  vector<int> end   = get_boundaries(A,A.length()-1,0);


  //Step 1: construct the list of possible indels for each column
  vector< vector<possibility> > possibilities(A.length()+1);
  for(int column=0;column<A.length();column++) {
    vector<int> present(A.num_sequences());
    for(int i=0;i<A.num_sequences();i++) {
      present[i] = (A(column,i) != alphabet::gap);

      if (!present[i] && !(start[i] <= column && column <= end[i]) )
	present[i] = -1;
    }
    possibilities[column] = get_possibilities(present,T);
  }

  // Step 2: Move across the alignment, updating a running max or sum
  //  * Since each row depends only on the previous row, we can keep the max
  //    up to the current point only in terms of the choice for the previous
  //    row as an unbound variable
  //  * Actually, there might be an insertion INSIDE this current insertion
  //    so we need to deal w/ that also.


  vector< vector<double> > accumulation(A.length()+1);
  for(int i=0;i<accumulation.size();i++) {
    int size = possibilities[i].size();
    if (!size) size=1;
    accumulation[i].resize(size+1,0);
  }

  {  // First column gets special handling
    vector<double> start(2);start[0]=0.0;start[1]=1.0;
    do_sum(accumulation[0],start,
	   possibilities[0],vector<possibility>());
  }

  for(int column=1;column < accumulation.size();column++) 
    do_sum(accumulation[column],accumulation[column-1],
	   possibilities[column],possibilities[column-1]);
  
  return accumulation[A.length()][0];
}



