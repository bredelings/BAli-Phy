#include <vector>
#include "setup.H"
#include "util.H"
#include "rates.H"

using std::vector;
using std::valarray;

void link(alignment& A,SequenceTree& T) {

  /*------ Make sure A has enough leaf sequences ----------*/
  if (A.num_sequences() < T.leaves())
    throw myexception()<<"Tree has "<<T.leaves()<<" leaves but Alignment only has "
		       <<A.num_sequences()<<" sequences.";

  /*----- If we just have leaf sequences, add internal sequences --------*/
  if (A.num_sequences() == T.leaves()) {
    sequence s(A.get_alphabet());
    s.resize(A.length());
    for(int column=0;column<A.length();column++)
      s[column] = alphabet::not_gap;
    for(int i=T.leaves();i<T.num_nodes()-1;i++)
      A.add_sequence(s);
  }

  /*----- Make sure we have the right number of ancestral sequences -----*/
  if (A.num_sequences() > T.num_nodes() -1)
    throw myexception(string("More sequences than tree nodes!"));
  else if (A.num_sequences() < T.num_nodes() -1)
    throw myexception(string("Not enough ancestral sequences!"));
  else // A.num_sequenes()/2 + 1 == T.leaves()
    ; // FIXME - check that these are all gaps or Felsenstein wildcards
  
  /*----- Remap leaf indices for T onto A's leaf sequence indices -----*/
  vector<int> mapping(T.leaves());
  for(int i=0;i<T.leaves();i++) {
    int target = -1;
    for(int j=0;j<T.leaves();j++) {
      if (T.seq(i) == A.seq(j).name) {
	target = j;
	break;
      }
    }
    if (target == -1)
      throw myexception()<<"Couldn't find sequence \""<<T.seq(i)<<"\" in alignment";
    mapping[i] = target;
  }


  T.standardize(mapping);


}



void load_A(Arguments& args,alignment& A) {
  vector<alphabet> alphabets;
  alphabets.push_back(alphabet("DNA nucleotides","AGTC","NYR"));
  alphabets.push_back(alphabet("RNA nucleotides","AGUC","NYR"));
  alphabets.push_back(alphabet("Amino Acids","ARNDCQEGHILKMFPSTWYV","X"));
  
  /* ----- Try to load alignment ------ */
  if (not args.set("align")) 
    throw myexception("Alignment file not specified! (align=<filename>)");
  
  A.load(alphabets,args["align"]);
  
  remove_empty_columns(A);
  
  if (A.num_sequences() == 0)
    throw myexception()<<"Alignment file "<<args["align"]<<"didn't contain any sequences!";
}


void load_T(Arguments& args,const alignment& A,SequenceTree& T,bool random_tree_ok) {
  /*------ Try to load tree -------------*/
  if (not args.set("tree")) {
    if (not random_tree_ok)
      throw myexception()<<"Tree file not specified! (tree=<filename>)";

    // FIXME - this assumes that alignment doesn't specify internal nodes...
    vector<string> s;
    for(int i=0;i<A.num_sequences();i++)
      s.push_back(A.seq(i).name);
    T = RandomTree(s,0.05);
  }
  else 
    T.read(args["tree"]);
}


void load_A_and_T(Arguments& args,alignment& A,SequenceTree& T,bool random_tree_ok)
{
  load_A(args, A);
  load_T(args, A, T, random_tree_ok);

  /*------ Link Alignment and Tree ----------*/
  link(A,T);

  /*-------- Analyze 'internal'-------*/
  if (args.set("internal")) {
    if (args["internal"] == "+")
      for(int column=0;column< A.length();column++) {
	for(int i=T.leaves();i<A.size2();i++) 
	  A(column,i) = alphabet::not_gap;
      }
    else if (args["internal"] == "search")
      assert(0); // FIXME - not programmed yet
    else if (args["internal"] == "guess") 
      for(int column=0;column< A.length();column++) {
	vector<int> present_leaf(T.leaves());
	for(int i=0;i<T.leaves();i++)
	  present_leaf[i] = not A.gap(column,i);
	TreeFunc<int> present = mark_tree(present_leaf,T);
	for(int i=T.leaves();i<A.size2();i++) {
	  if (present(i))
	    A(column,i) = alphabet::not_gap;
	  else
	    A(column,i) = alphabet::gap;
	}
      }
  }

}
