#ifndef UNROOTED_TREE_H
#define UNROOTED_TREE_H

#include <vector>

namespace unrooted_tree {
  
  struct TraversalNode;

  struct RealNode {
    int name;
    std::vector<TraversalNode*> subnodes;
  };

  struct TraversalNode {
    RealNode* supernode;
    TraversalNode* next;
    TraversalNode* prev;
  };

  typedef int edge[2];
  typedef edge subtree;

  class NodeView {
    const RealNode* n;
  public:
    bool leaf() const {return subnodes.size() == 1;}
    int name() const {return n->name;}

    vector<int> neighbors() const {
      vector<int> nodes;
      nodes.reserve(3);
      for(int i=0;i<n->subnodes.size();i++)
	nodes.push_back(n->subnodes[i].next->supernode->name;);
      return nodes;
    }

  };


  class Tree {
    std::vector<RealNode*> nodes;
  public:
    int n_nodes() const;
    int n_leaves() const;
    int n_leafbranches() const;
    int n_branches() const;

    branchview branch(int i);

    const_branchview branch(int i) const;

    const nodeview operator[](int i) const;

    bool connected(int node1, int node2) const;
    int find_branch(int node1, int node2) const;

    void exchange(subtree s1,subtrees2);
    void SPR(subtree s1,edge e);
  };

  // This is the unrooted tree class
  class UnrootedTree: public Tree {
    std::vector<edge> edges;


  public:

  };

  class TreeIterator {
    int i;
  public:
    operator++();
    operator--();
    TreeIterator(Tree&);
  };

  class ConstTreeIterator {
    int i;
  public:
    operator++();
    operator--();
    TreeIterator(const Tree&);
  };

  

};

#endif
