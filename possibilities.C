#include "possibilities.H"

bool operator==(const edge& e1, const edge& e2) {
  return (e1.node1 == e2.node1) && (e1.node2 == e2.node2);
}

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



TreeFunc<int> mark_tree(const vector<int>& present_leaf,const tree& T) {
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


vector<possibility> get_possibilities(const vector<int>& present_leaf,const tree& T) {
  vector<possibility> v;

  bool any_missing=false;
  for(int i=0;i<present_leaf.size();i++) {
    if (present_leaf[i] == 0) {
      any_missing=true;
      break;
    }
  }
  if (!any_missing) return v;

  TreeFunc<int> present = mark_tree(present_leaf,T);

  vector<edge> indel_edges;
  for(int i=0;i<T.num_nodes()-1;i++) {
    if (present(i) != 1) continue;

    vector<int> neighbors = get_neighbors(T[i]);

    for(int j=0;j<neighbors.size();j++) {
      int neighbor = neighbors[j];
      if (!present(neighbor))
	indel_edges.push_back(edge(i,neighbor)); // can't get added twice
    }
  }

  // Because of tree topology, each (directed) edge is the root of a subtree that is
  // not connected to the other subtrees.

  assert(indel_edges.size() >0);
  v = get_pos_subtree(T,indel_edges[0],present);

  for(int i=1;i<indel_edges.size();i++) 
    v = multiply(v,get_pos_subtree(T,indel_edges[i],present));

  return v;
}
