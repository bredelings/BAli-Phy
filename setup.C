#include <vector>
#include "setup.H"
#include "util.H"
#include "rates.H"
#include "alphabet.H"

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
    for(int i=T.leaves();i<T.num_nodes()-1;i++) {
      s.name = string("A") + convertToString(i);
      A.add_sequence(s);
    }
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
  vector<OwnedPointer<alphabet> > alphabets;
  if (args.set("Use Codons")) {
    {
      ifstream genetic_code("Data/genetic_code_dna.dat");
      if (not genetic_code)
	throw myexception()<<"Couldn't open file 'Data/genetic_code_dna.dat'";
      Translation_Table T(Codons(DNA()),AminoAcids(),genetic_code);
      genetic_code.close();

      alphabets.push_back(T.getCodons());
    }

    {
      ifstream genetic_code("Data/genetic_code_rna.dat");
      if (not genetic_code)
	throw myexception()<<"Couldn't open file 'Data/genetic_code_rna.dat'";
      Translation_Table T(Codons(RNA()),AminoAcids(),genetic_code);
      genetic_code.close();

      alphabets.push_back(T.getCodons());
    }
  }
  else {
    alphabets.push_back(DNA());
    alphabets.push_back(RNA());
    alphabets.push_back(AminoAcids());
  }
  
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
  T.unroot();
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


alignment standardize(const alignment& A, const SequenceTree& T) {
  alignment A2 = A;
  SequenceTree T2 = T;
  vector<int> mapping = T2.standardize();
  vector<int> imapping = invert(mapping);

  for(int i=0;i<A.num_sequences();i++) {
    if (imapping[i] == i) continue;

    A2.seq(i) = A.seq(imapping[i]);
    for(int column=0;column<A2.length();column++)
      A2(column,i) = A(column,imapping[i]);
  }
  return A2;
}


valarray<double> empirical_frequencies(Arguments& args,const alignment& A) {
  const alphabet& a = A.get_alphabet();

  // Count the occurrence of the different letters
  valarray<double> counts(0.0, a.size());
  for(int i=0;i<A.length();i++) {
    for(int j=0;j<A.size2();j++) {
      if (alphabet::letter(A(i,j)))
	counts[A(i,j)]++;
    }
  }

  counts = counts/counts.sum() * A.length();

  // Setup the default frequences for the pseudocounts (uniform)
  double pseudocount = 5+4*a.size();

  if (args.set("CFNF")) {
    pseudocount = 100*counts.size();
  }

  valarray<double> frequencies = A.get_alphabet().get_frequencies_from_counts(counts,pseudocount);

  return frequencies;
}


