/* Version 2: based on operating on multiple alignments */

#include "mytypes.H"
#include "etree.H"
#include "substitution.H"
#include "alignment.H"


/**************** probabilities *********************/

double gap_init;
double gap_extend;

struct possibility {
  vector<edge> edges;
};

vector<possibility> get_possibilities(const vector<bool>& missing,const tree& T) {
  vector<possibility> v;
  bool any_missing=false;
  for(int i=0;i<missing.size();i++) {
    if (missing[i]) {
      any_missing=true;
      break;
    }
  }
  if (!any_missing) return v;

  //Step 0: Set all nodes as un-marked
  TreeFunc<int> f(T);
  for(int i=0;i<missing.size();i++)
    f(i)=0;

  //Step 1: Look at all pairs of non-missing nodes:
  //         All nodes on paths connecting them are non-missing also
  for(int i=0;i<missing.size();i++) {
    if (missing[i]) continue;

    for(int j=0;j<i;j++) {
      if (missing[j]) continue;

      vector<int> path = T.path(i,j);
      for(int k=0;k<path.size();k++) 
	f(path[k])=1;
    }
  }

  //Step 2: Look at all unmarked nodes that are next to marked nodes
  vector<edge> indel_edges;
  for(int i=0;i<T.num_nodes();i++) {
    if (!f(i)) continue;

    vector<int> neighbors = get_neighbors(T[i]);

    for(int j=0;j<neighbors.size();j++) {
      int neighbor = neighbors[j];
      if (!f(neighbor))
	indel_edges.push_back(edge(i,neighbor)); // can't get added twice
    }
  }

  
  // Because of tree topology, each (directed) edge is the root of a subtree that is
  // not connected to the other subtrees.

  //Step 3 - Expand out all the actual possibility
  //  Hmmm... should we use the roots of the subtrees to REPRESENT all the possibilities,
  //   since they can be computed from the roots? (FIXME - actually code step 3)

  possibility p;
  p.edges = indel_edges;

  v.push_back(p);
  return v;
}

// Version 1.0
//  1. Ignore branch lengths - just use penalty!
//  2. Don't sum over possibilities - just take max!
double prior(const alignment& A,const tree& T) {
  vector<vector<possibility> > possibilities(A.length());

  //Step 1: construct the list of possible indels for each column
  for(int column=0;column<A.length();column++) {
    vector<bool> missing(A.num_sequences());
    for(int i=0;i<A.num_sequences();i++) {
      if (A(column,i) == alphabet::gap) 
	missing[i]=true;
      else
	missing[i]=false;
    }
    possibilities[column] = get_possibilities(missing,T);
  }

  //Step 2: Look at all the possibilities - use a maximization 
  // Step 2.1: Divide into blocks
  vector<int> starts;
  vector<int> ends;

  int in_region=false;
  for(int column=0;column<A.length();column++) {
    vector<possibility>& here = possibilities[column];
    if (here.size() ==0 || here.size()==1) { // no indels, or the indels are certain
      if (in_region) ends.push_back(column);
      in_region=false;
    }
    else {
      if (!in_region) starts.push_back(column);
      in_region=true;
    }
  }
  if (in_region) ends.push_back(A.length());



  // Step 2.2: Maximize each block, sum blocks


  // Implicit Step 3: For each assignment, extend the gaps.
  // Part 4: Edge handling:
  return 1.0;
}




// ln(P(A,Data)) = ln(P(A)) + ln(P(Data|A))
// FIXME - convert to ln(P)
double probability(const alignment& A,const tree& T) {
  double p = prior(A,T);  // should A include the tree, or vice versa?
  p *= substitution(A,T);

  return p;
}




int main() {

  alphabet nucleotides("ATGC","DNA");

  sequence human        (nucleotides,"hemoglobin_nuc_hs");
  sequence chimp        (nucleotides,"hemoglobin_nuc_cz");
  sequence gorilla      (nucleotides,"hemoglobin_nuc_go");
  sequence orangutan    (nucleotides,"hemoglobin_nuc_or");

  alphabet amino_acids("ARNDCQEGHILKMFPTWYV","PAM");

  sequence human_aa     (amino_acids,"hemoglobin_hs");
  sequence chimp_aa     (amino_acids,"hemoglobin_cz");
  sequence gorilla_aa   (amino_acids,"hemoglobin_go");
  sequence orangutan_aa (amino_acids,"hemoglobin_or");

  alignment A;
  A << human << chimp << gorilla << orangutan;

  tree t = human + chimp;
  t = (human + chimp) + (gorilla + orangutan);
  tree t2 = tree( tree(human,1,chimp,2),1,tree(gorilla,2,orangutan,2),2);

  double p = probability(A,t);
  return 1;

  while(1) {
    alignment A1;
    // propose a new alignment

    double p1 = probability(A1,t);
  }

  return 1;
}
/****************************************************************
 Iterator:


  here = iterator(node,direction)
  here = there      // here.(node,direction) = there.(node,direction)

  
  *here = *there    // make here.(node,direction) go to wherever there.(node,dirction) does.
  *here = new node; // make here.(node,direction) point to the new node;



  subtree(here) = subtree(there); //the subtree starting from here.(node,direction) 
  subtree(here) = t2;

-------------
  2 levels:

 Level 1:

 *here = new node;    // *(here.pointer) = new node;

 iterator there(t2);
 *here = *there;      // *(here.pointer) = t2.root;

 Level 2:
 
  *here = new node;   // *(here.pointer) = new node; (*(here.pointer))->parent = here.node;

  subtree(here) = t2; // 
    t3 = t2.copy();
    iterator there(t3);
    


            this should probably connect up 
*/
