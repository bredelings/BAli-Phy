#include "unrooted-tree.C"

using namespace unrooted_tree;

TraversalNode* subnode_pointing_to(RealNode* n,int i) {
  for(int i=0;i<n->subnodes.size();i++) {
    if (n->subnodes[i]->next->name == i) {
      return n->subnodes[i];
    }
  }
  return 0;
}

void connect(TraversalNode* n1,TraversalNode* n2) {
  n1->next = n2;
  n2->prev = n1;
}


void Tree::exchange(subtree s1,subtree s2) {
  //  assert(subtrees contain no overlapping nodes);
  int A1 = s1[0];
  int A2 = s1[1];

  int B1 = s2[0];
  int B2 = s2[1];

  TraversalNode* a = subnode_pointing_to(nodes[A1],A2);
  TraversalNode* b = a->next;

  TraversalNode* c = subnode_pointing_to(nodes[A2],A1);
  TraversalNode* d = c->next;

  TraversalNode* e = subnode_pointing_to(nodes[B1],B2);
  TraversalNode* f = e->next;

  TraversalNode* g = subnode_pointing_to(nodes[B2],B1);
  TraversalNode* h = g->next;

  connect(a,f);
  connect(e,b);
  connect(c,h);
  connect(g,d);
}

void Tree::SPR(subtree s1, edge e) {
  int A1 = s1[0];
  int A2 = s1[1];

  int B1 = e[0];
  int B2 = e[1];

  TraversalNode* x = subnode_pointing_to(nodes[A1],A2);
  TraversalNode* y = subnode_pointing_to(nodes[A2],A1);
  TraversalNode* z = (the other one);

  
}
