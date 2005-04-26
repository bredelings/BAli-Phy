#include <vector>
#include "setup.H"
#include "util.H"
#include "rates.H"
#include "alphabet.H"
#include "alignment-util.H"
#include "tree-util.H"
#include "substitution-index.H"

using std::vector;
using std::valarray;
using std::cout;
using std::cerr;
using std::endl;

using namespace boost::program_options;

//FIXME - standardize them BOTH by leaf taxa names?

/// Reorder internal sequences of A to correspond to standardized node names for T
alignment standardize(const alignment& A, const SequenceTree& T) {
  alignment A2 = A;
  SequenceTree T2 = T;

  // standardize NON-LEAF node and branch names in T
  vector<int> mapping = T2.standardize();

  // FIXME - remove imapping and do this forwards instead of backwards?
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
valarray<double> empirical_frequencies(const variables_map& args,const alignment& A) {
  const alphabet& a = A.get_alphabet();

  // Count the occurrence of the different letters
  valarray<double> counts(0.0, a.size());
  for(int i=0;i<A.length();i++) {
    valarray<double> column_counts(0.0, a.size());
    for(int j=0;j<A.size2();j++) {
      if (alphabet::letter(A(i,j)))
	column_counts[A(i,j)]++;
    }

    double total = column_counts.sum();
    if (total > 1)
      column_counts /= total;
    counts += column_counts;
  }

  // Setup the default frequences for the pseudocounts (uniform)
  double pseudocount = 5*(a.size()+1);

  if (args.count("CFNF")) {
    pseudocount = 100*counts.size();
  }

  valarray<double> frequencies = A.get_alphabet().get_frequencies_from_counts(counts,pseudocount);

  return frequencies;
}


/// Make a random tree which matches the alignment;
SequenceTree get_random_T(const alignment& A) {
  // FIXME - this assumes that alignment doesn't specify internal nodes...
  vector<string> s;
  for(int i=0;i<A.num_sequences();i++)
    s.push_back(A.seq(i).name);
  SequenceTree T = RandomTree(s,0.05);
  return T;
}


/// Remap T leaf indices to match A: check the result
void link(alignment& A,SequenceTree& T,bool internal_sequences) {

  //------ IF sequences < leaf nodes THEN complain ---------//
  if (A.n_sequences() < T.n_leaves())
    throw myexception()<<"Tree has "<<T.n_leaves()<<" leaves but Alignment only has "
		       <<A.n_sequences()<<" sequences.";

  //----- IF sequences = leaf nodes THEN maybe add internal sequences.
  else if (A.n_sequences() == T.n_leaves()) {
    if (internal_sequences)
      A = add_internal(A,T);
  }
  //----- IF sequences > leaf nodes THEN maybe complain -------//
  else {
    if (not internal_sequences)
      throw myexception()<<"More sequences than leaf nodes!";

    if (A.num_sequences() > T.n_nodes())
      throw myexception()<<"More sequences than tree nodes!";
    else if (A.num_sequences() < T.n_nodes())
      throw myexception()<<"Less sequences than tree nodes!";
  }
  
  //---------- double-check that we have the right number of sequences ---------//
  if (internal_sequences)
    assert(A.size2() == T.n_nodes());
  else
    assert(A.size2() == T.n_leaves());


  //----- Remap leaf indices for T onto A's leaf sequence indices -----//
  vector<int> mapping(T.n_leaves());
  for(int i=0;i<T.n_leaves();i++) {
    int target = -1;
    for(int j=0;j<T.n_leaves();j++) {
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


  //---- Check to see that internal nodes satisfy constraints ----//
  check_alignment(A,T,internal_sequences);
}

// FIXME - should I make this more generic, so that it doesn't rely on a file?
void load_A_and_T(const variables_map& args,alignment& A,SequenceTree& T,bool internal_sequences)
{
  A = load_A(args,internal_sequences);

  T = load_T(args);

  //------------- Link Alignment and Tree -----------------//
  link(A,T,internal_sequences);

  //---------------- Randomize alignment? -----------------//
  if (args.count("randomize-alignment"))
    A = randomize(A,T.n_leaves());
  
  //------------------ Analyze 'internal'------------------//
  if ((args.count("internal") and args["internal"].as<string>() == "+")
      or args.count("randomize-alignment"))
    for(int column=0;column< A.length();column++) {
      for(int i=T.n_leaves();i<A.size2();i++) 
	A(column,i) = alphabet::not_gap;
    }

  //---- Check that internal sequence satisfy constraints ----//
  check_alignment(A,T,internal_sequences);
}

void load_A_and_random_T(const variables_map& args,alignment& A,SequenceTree& T,bool internal_sequences)
{
  A = load_A(args,internal_sequences);

  T = get_random_T(A);

  //------------- Link Alignment and Tree -----------------//
  link(A,T,internal_sequences);

  //---------------- Randomize alignment? -----------------//
  if (args.count("randomize-alignment"))
    A = randomize(A,T.n_leaves());
  
  //------------------ Analyze 'internal'------------------//
  if ((args.count("internal") and args["internal"].as<string>() == "+")
      or args.count("randomize-alignment"))
    for(int column=0;column< A.length();column++) {
      for(int i=T.n_leaves();i<A.size2();i++) 
	A(column,i) = alphabet::not_gap;
    }

  //---- Check that internal sequence satisfy constraints ----//
  check_alignment(A,T,internal_sequences);
}

OwnedPointer<IndelModel> get_imodel(const variables_map& args) {
  //-------------Choose an indel model--------------//
  OwnedPointer<IndelModel> imodel;

  if (args["imodel"].as<string>() == "simple")
    imodel = SimpleIndelModel();
  else if (args["imodel"].as<string>() == "new")
    imodel = NewIndelModel();
  else
    throw myexception()<<"Unrecognized indel model '"<<args["imodel"].as<string>()<<"'";
  
  if (args.count("gaps") and args["gaps"].as<string>() == "star")
    imodel->full_tree = false;
  else
    imodel->full_tree = true;

  return imodel;
}
