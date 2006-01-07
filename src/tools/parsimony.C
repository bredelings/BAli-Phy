#include "util.H"
#include "parsimony.H"
using namespace std;


unsigned n_mutations(const alphabet& a, const vector<int>& letters, const SequenceTree& T) 
{
  const int A = a.size();

  assert(letters.size() == T.n_leaves());

  vector< vector<int> > n_muts(T.n_nodes(),vector<int>(A,0));

  int root = T.directed_branch(0).target();
  vector<const_branchview> branches = branches_toward_node(T,root);

  vector<int> temp(A);

  // set the leaf lengths
  for(int s=0;s<T.n_leaves();s++)
    if (a.is_letter_class(letters[s]))
      for(int l=0;l<A;l++)
	n_muts[s][l] = not a.matches(l,letters[s]);

  for(int i=0;i<branches.size();i++) 
  {
    int s = branches[i].source();
    int t = branches[i].target();

    for(int l=0;l<A;l++)
      temp[l] = n_muts[s][l]+1;

    for(int l=0;l<A;l++) {
      temp[l]--;
      n_muts[t][l] += min(temp);
      temp[l]++;
    }
  }

  return min(n_muts[root]);
}

unsigned n_mutations(const alignment& A, const SequenceTree& T)
{
  const alphabet& a = A.get_alphabet();

  vector<int> letters(T.n_leaves());

  unsigned tree_length = 0;
  for(int c=0;c<A.length();c++) {
    for(int i=0;i<T.n_leaves();i++)
      letters[i] = A(c,i);
    unsigned length = n_mutations(a,letters,T);
    tree_length += length;
  }

  return tree_length;
}
