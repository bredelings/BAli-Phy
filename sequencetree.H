#ifndef SEQUENCETREE_H
#define SEQUENCETREE_H

#include <vector>
#include <string>
#include <istream>
#include "tree.H"


/***************** class SequenceTree ****************/
class SequenceTree: public tree {
  std::vector<std::string> sequences;
  std::string write(int n,bool) const;

protected:
  void remove_subtree(node*);

public:
  void remove_subtree(int i) {return tree::remove_subtree(i);}

  std::string& seq(int i) {return sequences[i];}
  const std::string& seq(int i) const {return sequences[i];}

  std::vector<std::string>& get_sequences() {return sequences;}
  const std::vector<std::string>& get_sequences() const {return sequences;}

  std::vector<int> standardize(bool=true);
  std::vector<int> standardize(const std::vector<int>&,bool=true);

  int index(const std::string&) const;

  std::string write(bool=true) const;

  void read(const std::string&);
  void read(std::istream&);
  void parse(const std::string&);

  SequenceTree() {}
  SequenceTree(const std::string& s);
  SequenceTree(const SequenceTree& T1, const SequenceTree& T2);
  SequenceTree(const SequenceTree& T1, double b1,const SequenceTree& T2,double b2);

  SequenceTree(std::istream&);   //load a tree from a file
  virtual ~SequenceTree() {}
};

inline SequenceTree operator+(const SequenceTree& t1,const SequenceTree& t2) {
  SequenceTree t3(t1,t2);

  return t3;
}

inline std::ostream& operator <<(std::ostream& o,const SequenceTree& T) {
  return o<<T.write();
}

bool same_topology(const SequenceTree&,const SequenceTree&);

SequenceTree RandomTree(const std::vector<std::string>&,double=1.0);

void delete_node(SequenceTree& T,const std::string& name);

#endif
