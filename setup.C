#include <vector>
#include "setup.H"
#include "util.H"
#include "rates.H"
#include "alphabet.H"

using std::vector;
using std::valarray;

bool bit_set(const valarray<bool>& v) {
  for(int i=0;i<v.size();i++)
    if (v[i]) return true;
  return false;
}


/// Check that any two present nodes are connected by a path of present nodes
bool all_characters_connected(const SequenceTree& T,const valarray<bool> present) {
  assert(present.size() == T.n_nodes()-1);

  for(int b=0;b<T.n_branches();b++) {
    int n1 = T.branch(b).parent();
    int n2 = T.branch(b).child();
    valarray<bool> group1 = T.partition(n1,n2);
    valarray<bool> group2 = not group1;

    if (bit_set(present and group1) and bit_set(present and group2))
      if (not (present[n1] and present[n2]))
	return false;
  }
  return true;
}


void check_alignment(const alignment& A,const SequenceTree& T) {
  //----- Check that internal nodes don't have letters ------//
  for(int i=T.leaves();i<T.n_nodes()-1;i++) 
    for(int column=0;column<A.length();column++)
      if (alphabet::letter(A(column,i)) )
	throw myexception()<<"Found a letter in column "<<column
			   <<" of internal sequence '"<<A.seq(i).name
			   <<"': only - and * are allowed";
  

  //---- Check that internal node states are consistent ----//
  for(int column=0;column<A.length();column++) {
    valarray<bool> present(T.n_nodes()-1);
    for(int i=0;i<T.n_nodes()-1;i++) 
      present[i] = not A.gap(column,i);

    if (not all_characters_connected(T,present))
      throw myexception()<<"Internal node states are inconsistent in column "<<column;
  }
}


void link(alignment& A,SequenceTree& T,bool internal_sequences) {

  //------ Make sure A has enough leaf sequences ----------//
  if (A.num_sequences() < T.leaves())
    throw myexception()<<"Tree has "<<T.leaves()<<" leaves but Alignment only has "
		       <<A.num_sequences()<<" sequences.";

  //----- If we just have leaf sequences, add internal sequences --------//
  else if (A.num_sequences() == T.leaves()) {
    if (internal_sequences) {
      sequence s(A.get_alphabet());
      s.resize(A.length());
      for(int column=0;column<A.length();column++)
	s[column] = alphabet::not_gap;
      for(int i=T.leaves();i<T.num_nodes()-1;i++) {
	s.name = string("A") + convertToString(i);
	A.add_sequence(s);
      }
    }
  }

  //----- Make sure we have the right number of ancestral sequences -----//
  else {
    if (not internal_sequences)
      throw myexception()<<"More sequences than leaf nodes!";

    if (A.num_sequences() > T.num_nodes() -1)
      throw myexception()<<"More sequences than tree nodes!";
    else if (A.num_sequences() < T.num_nodes() -1)
      throw myexception()<<"Not enough ancestral sequences!";
  }
  
  //----- Remap leaf indices for T onto A's leaf sequence indices -----//
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


  //------ Check to see that internal nodes satisfy constraints ------//
  check_alignment(A,T);
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

  //------ Link Alignment and Tree ----------//
  link(A,T);

  //-------- Randomize alignment? -------//
  if (args.set("randomize_alignment"))
    A = randomize(A,T.leaves());
  
  //---------- Analyze 'internal'---------//
  if (args["internal"] == "+" or args.set("randomize_alignment"))
    for(int column=0;column< A.length();column++) {
      for(int i=T.leaves();i<A.size2();i++) 
	A(column,i) = alphabet::not_gap;
    }
}

/// Reorder internal sequence of A to correspond to standardized node names for T
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


/// Estimate the empirical frequencies of different letters from the alignment, with pseudocounts
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


