#include <map>
#include <set>
#include <vector>
#include "mytypes.H"
#include "alignment.H"

struct Edge 
{
  int s1;
  int x1;

  int s2;
  int x2;

  unsigned count;
  double p;
};

struct edge_comp {
  bool operator()(const Edge& E1, const Edge& E2) {
    return E1.p  > E2.p;
  }
};

class Edges: public std::multiset<Edge,edge_comp>
{
  vector<vector<vector<std::multiset<Edge,edge_comp>::iterator> > > lookup;
public:
  void build_index();

  double PP(int s1,int x1,int s2,int x2) const;
  int index_in_sequence(int s1,int x1,int s2) const;

  Edges(const vector<int>& L);
};

void add_edges(Edges& E, const vector< ublas::matrix<int> >& Ms,
	       int s1,int s2,int L1, int L2);

class index_matrix: public ublas::matrix<int> 
{
  vector<vector<int> > column_index;

public:

  int columns;
  int unknowns;

  int  index(int i,int j) const {return (*this)(i,j);}
  int& index(int i,int j)       {return (*this)(i,j);}

  int  column(int i,int j) const {assert(j>=0); return column_index[i][j];}
  int& column(int i,int j)       {assert(j>=0); return column_index[i][j];}

  int length(int i) const {return column_index[i].size();}

  bool columns_conflict(int c1, int c2) const;

  bool consistent(int c1, int s,int x, const Edges& E,double cutoff) const;
  bool consistent(int c1, int c2,const Edges& E, double cutoff) const;

  void merge_columns(int c1,int c2);
  std::map<unsigned,std::pair<unsigned,unsigned> > merge(const Edges& E,double p,bool strict);

  unsigned n_unknown() const;

  unsigned n_columns() const;

  index_matrix(int C, const vector<int>& L)
    :ublas::matrix<int>(C,L.size()),columns(C),unknowns(0)
  {
    for(int i=0;i<L.size();i++)
      column_index.push_back(vector<int>(L[i]));
  }
};

index_matrix unaligned_matrix(const std::vector<int>& L);

unsigned count_unknowns(const index_matrix& M,int c);

ublas::matrix<int> get_ordered_matrix(const index_matrix& M);

alignment get_alignment(const ublas::matrix<int>& M, alignment& A1);
