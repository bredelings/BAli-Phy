#ifndef RECONSTRUCT_H
#define RECONSTRUCT_H
#include <vector>
#include "alignment.H"
#include "tree.H"

vector<int> getorder(const alignment& A,const vector<int>& border,const tree& T);
vector<int> getnodes(const vector<int>& branches,const tree& T);

#endif
