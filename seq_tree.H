#ifndef SEQ_TREE_H
#define SEQ_TREE_H

#include "sequence.H"
#include "etree.H"

class SequenceTree: public tree {
  vector<string> sequences;
public:
  const string& seq(int i) const {return sequences[i];}
  const vector<string>& get_sequences() const {return sequences;}

  explicit SequenceTree(const string&);
  SequenceTree(const sequence& s);
  SequenceTree(const SequenceTree& T1, const SequenceTree& T2);
};

inline SequenceTree operator+(const SequenceTree& t1,const SequenceTree& t2) {
  SequenceTree t3(t1,t2);

  return t3;
}

#endif
