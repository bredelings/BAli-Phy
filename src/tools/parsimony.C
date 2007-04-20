#include "util.H"
#include "parsimony.H"
using namespace std;

Matrix unit_cost_matrix(unsigned size)
{
  Matrix cost(size,size);
  for(int i=0;i<size;i++) {
    cost(i,i) = 0;
    for(int j=0;j<i;j++)
      cost(i,j) = cost(j,i) = 1;
  }
  return cost;
}

Matrix unit_cost_matrix(const alphabet& a)
{
  return unit_cost_matrix(a.size());
}

unsigned n_nuc_differences(const Triplets& T,int i,int j)
{
  unsigned n=0;
  for(int pos=0;pos<3;pos++)
    if (T.sub_nuc(i,pos) != T.sub_nuc(j,pos))
      n++;
  return n;
}

Matrix nucleotide_cost_matrix(const Triplets& T)
{
  Matrix cost(T.size(), T.size());

  for(int i=0;i<cost.size1();i++)
    for(int j=0;j<cost.size2();j++)
      cost(i,j) = n_nuc_differences(T,i,j);

  return cost;
}


Matrix amino_acid_cost_matrix(const Codons& C)
{
  Matrix cost(C.size(), C.size());

  for(int i=0;i<cost.size1();i++)
    for(int j=0;j<cost.size2();j++)
      if (C.translate(i) == C.translate(j))
	cost(i,j) = 0;
      else
	cost(i,j) = 1;
      
  return cost;
}


double n_mutations(const alphabet& a, const vector<int>& letters, const SequenceTree& T,const Matrix& cost) 
{
  const int A = a.size();

  assert(letters.size() == T.n_leaves());
  assert(cost.size1() == A);
  assert(cost.size2() == A);

  vector< vector<double> > n_muts(T.n_nodes(),vector<double>(A,0));

  int root = T.directed_branch(0).target();
  vector<const_branchview> branches = branches_toward_node(T,root);

  vector<double> temp(A);

  double max_cost = 0;
  
  for(int i=0;i<A;i++)
    for(int j=0;j<A;j++)
      max_cost = std::max(cost(i,j), max_cost);
    
  // set the leaf costs
  for(int s=0;s<T.n_leaves();s++)
    if (a.is_letter_class(letters[s]))
      for(int l=0;l<A;l++)
	if (a.matches(l,letters[s]))
	  n_muts[s][l] = 0;
	else
	  n_muts[s][l] = max_cost;

  for(int i=0;i<branches.size();i++) 
  {
    int s = branches[i].source();
    int t = branches[i].target();

    for(int l=0;l<A;l++) {
      for(int k=0;k<A;k++)
	temp[k] = n_muts[s][k]+cost(k,l);
      n_muts[t][l] += min(temp);
    }
  }

  return min(n_muts[root]);
}

double n_mutations(const alphabet& a, const vector<int>& letters, const SequenceTree& T) 
{
  return n_mutations(a,letters,T,unit_cost_matrix(a));
}


double n_mutations(const alignment& A, const SequenceTree& T,const Matrix& cost)
{
  const alphabet& a = A.get_alphabet();

  vector<int> letters(T.n_leaves());

  double tree_length = 0;
  for(int c=0;c<A.length();c++) {
    for(int i=0;i<T.n_leaves();i++)
      letters[i] = A(c,i);
    double length = n_mutations(a,letters,T,cost);
    tree_length += length;
  }

  return tree_length;
}

double n_mutations(const alignment& A, const SequenceTree& T)
{
  return n_mutations(A,T,unit_cost_matrix(A.get_alphabet()));
}
