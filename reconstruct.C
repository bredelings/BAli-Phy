#include "reconstruct.H"

#include <valarray>
#include <cassert>

using std::valarray;

bool any_true(const valarray<bool>& v) {
  for(int i=0;i<v.size();i++)
    if (v[i]) return true;
  return false;
}

bool higher(const valarray<bool>& v1,const valarray<bool>& v2) {
  assert(v1.size() == v2.size());
  for(int i= v1.size()-1;i>=0;i--)
    if (v1[i] and not v2[i])
      return true;
  return false;
}

vector<int> getorder(const alignment& A,const vector<int>& border,const tree& T) {

  vector<int> columns;
  vector<std::valarray<bool> > AP;                     // alignments present

  //----- Record which sub-alignments present per column ------//
  for(int column=0;column<A.length();column++) {

    std::valarray<bool> ap(false,border.size());
    for(int i=0;i<border.size();i++) {
      int np = T.branch(border[i]).parent();
      int nc = T.branch(border[i]).child();
      if (not A.gap(column,np) or A.gap(column,nc))
	ap[i] = true;
    }

    AP.push_back(ap);
    if (any_true(ap))
      columns.push_back(column);
  }

  //-------- Re-order unordered columns by AP order ---------//
  for(int i=0;i<columns.size()-1;) {
    valarray<bool> ap1 = AP[columns[i  ]];
    valarray<bool> ap2 = AP[columns[i+1]];
    if (not (any_true(ap1&ap2)) and higher(ap1,ap2)) {
      std::swap(columns[i],columns[i+1]);
      if (i>0) i--;
    }
    else
      i++;
  }

  return columns;
}

vector<int> getnodes(const vector<int>& branches,const tree& T) {

  // For each node, is it on one of the branches?
  valarray<bool> narray(false,T.num_nodes());
  for(int i=0;i<branches.size();i++) {
    int p = T.branch(branches[i]).parent();
    int c = T.branch(branches[i]).child();

    narray[p] = true;
    narray[c] = true;
  }

  // get a list of nodes that is, mentioned only once
  vector<int> nodes;
  for(int i=0;i<T.num_nodes()-1;i++)
    if (narray[i])
      nodes.push_back(i);

  return nodes;
}


alignment project(const alignment& A1,const vector<int>& border,const tree& T) {
  alignment A2;

  // get the list of nodes that is mentioned
  const vector<int> nodes = getnodes(border,T);
  for(int i=0;i<nodes.size();i++)
    A2.add_sequence(A1.seq(nodes[i]));

  // the the ordered list of columns that mention these nodes
  vector<int> columns = getorder(A1,border,T);
  A2.changelength(columns.size());

  // fill in the new alignment with the right values
  for(int column=0;column<A2.length();column++) 
    for(int i=0;i<nodes.size();i++)
      A2(column,i) = A1(columns[column],nodes[i]);

  return A2;
}

