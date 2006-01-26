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

  for(int i=0;i<A.n_sequences();i++) {
    if (imapping[i] == i) continue;

    A2.seq(i) = A.seq(imapping[i]);
    for(int column=0;column<A2.length();column++)
      A2(column,i) = A(column,imapping[i]);
  }
  return A2;
}

int letter_count(const alignment& A,int l) 
{
  // Count the occurrence of the different letters
  int count=0;
  for(int i=0;i<A.length();i++)
    for(int j=0;j<A.n_sequences();j++)
      if (A(i,j) == l)
	count++;

  return count;
}


valarray<double> letter_counts(const alignment& A) 
{
  const alphabet& a = A.get_alphabet();

  // Count the occurrence of the different letters
  valarray<double> counts(0.0, a.size());
  for(int i=0;i<A.length();i++) {
    for(int j=0;j<A.n_sequences();j++) {
      if (a.is_letter(A(i,j)))
	counts[A(i,j)]++;
    }
  }

  return counts;
}


/// Estimate the empirical frequencies of different letters from the alignment, with pseudocounts
valarray<double> empirical_frequencies(const variables_map& args,const alignment& A) 
{
  const alphabet& a = A.get_alphabet();

  // Count the occurrence of the different letters
  valarray<double> counts = letter_counts(A);

  valarray<double> frequencies(a.size());

  // empirical frequencies
  if (not args.count("frequencies"))
    frequencies = A.get_alphabet().get_frequencies_from_counts(counts,A.n_sequences()/2);

  // uniform frequencies
  else if (args["frequencies"].as<string>() == "uniform")
    frequencies = 1.0/a.size();

  // triplet frequencies <- nucleotide frequencies
  else if (args["frequencies"].as<string>() == "nucleotides") {
    const Triplets* T = dynamic_cast<const Triplets*>(&a);

    if (not T) throw myexception()<<"You can only specify nucleotide frequencies on Triplet or Codon alphabets.";
    valarray<double> N_counts = get_nucleotide_counts_from_codon_counts(*T,counts);
    valarray<double> fN = T->getNucleotides().get_frequencies_from_counts(N_counts,A.n_sequences()/2);

    frequencies = get_codon_frequencies_from_independant_nucleotide_frequencies(*T,fN);
  }

  // specified frequencies
  else {
    vector<double> f = split<double>(args["frequencies"].as<string>(),',');

    if (f.size() != a.size())
      throw myexception()<<"You specified "<<f.size()<<" frequencies, but there are "
			 <<a.size()<<" letters of the alphabet!";

    for(int i=0;i<f.size();i++)
      frequencies[i] = f[i];
  }

  return frequencies;
}


/// Make a random tree which matches the alignment;
SequenceTree get_random_T(const alignment& A) {
  // FIXME - this assumes that alignment doesn't specify internal nodes...
  vector<string> s;
  for(int i=0;i<A.n_sequences();i++)
    s.push_back(A.seq(i).name);
  SequenceTree T = RandomTree(s,0.05);
  return T;
}


/// Remap T leaf indices to match A: check the result
void link(alignment& A,SequenceTree& T,bool internal_sequences) 
{
  if (internal_sequences and not is_Cayley(T))
    throw myexception()<<"Cannot link a non-Cayley tree to an alignment with internal sequences.";

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
    if (not internal_sequences) {
      alignment A2 = chop_internal(A);
      if (A2.n_sequences() == T.n_leaves()) {
	A = A2;
      }
      else
	throw myexception()<<"More sequences than leaf nodes!";
    } 
    else if (A.n_sequences() > T.n_nodes())
      throw myexception()<<"More sequences than tree nodes!";
    else if (A.n_sequences() < T.n_nodes())
      throw myexception()<<"Less sequences than tree nodes!";
  }
  
  //---------- double-check that we have the right number of sequences ---------//
  if (internal_sequences)
    assert(A.n_sequences() == T.n_nodes());
  else
    assert(A.n_sequences() == T.n_leaves());


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

/// Remap T leaf indices to match A: check the result
void link(alignment& A,RootedSequenceTree& T,bool internal_sequences) 
{
  if (internal_sequences and not is_Cayley(T))
    throw myexception()<<"Cannot link a non-Cayley tree to an alignment with internal sequences.";

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

    if (A.n_sequences() > T.n_nodes())
      throw myexception()<<"More sequences than tree nodes!";
    else if (A.n_sequences() < T.n_nodes())
      throw myexception()<<"Less sequences than tree nodes!";
  }
  
  //---------- double-check that we have the right number of sequences ---------//
  if (internal_sequences)
    assert(A.n_sequences() == T.n_nodes());
  else
    assert(A.n_sequences() == T.n_leaves());


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

// FIXME - we might still want to link things if
// (a) they have nodes of degree 2
// (b) they have nodes of degree >3
// (c) however, this linking would be limitted to leaf nodes...
// So, we should do some kind of check that when we have internal sequences, we have
// a Cayley tree.

// FIXME - should I make this more generic, so that it doesn't rely on a file?
void load_A_and_T(const variables_map& args,alignment& A,RootedSequenceTree& T,bool internal_sequences)
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
      for(int i=T.n_leaves();i<A.n_sequences();i++) 
	A(column,i) = alphabet::not_gap;
    }

  //---- Check that internal sequence satisfy constraints ----//
  check_alignment(A,T,internal_sequences);
}

void load_A_and_T(const variables_map& args,alignment& A,SequenceTree& T,bool internal_sequences)
{
  RootedSequenceTree RT;
  load_A_and_T(args,A,RT,internal_sequences);
  T = RT;
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
      for(int i=T.n_leaves();i<A.n_sequences();i++) 
	A(column,i) = alphabet::not_gap;
    }

  //---- Check that internal sequence satisfy constraints ----//
  check_alignment(A,T,internal_sequences);
}

void set_parameters(Model& M, const variables_map& args) 
{
  //-------------- Specify fixed parameters ----------------//
  vector<string>   fix;
  if (args.count("fix"))
    fix = args["fix"].as<vector<string> >();

  vector<string> unfix;
  if (args.count("unfix"))
    unfix = args["unfix"].as<vector<string> >();

  vector<string> doset;
  if (args.count("set"))
    doset = args["set"].as<vector<string> >();

  // separate out 'set' operations from 'fixed'
  for(int i=0;i<fix.size();i++) {
    vector<string> parse = split(fix[i],'=');
    
    if (parse.size() > 1) {
      doset.push_back(fix[i]);
      fix[i] = parse[0];
    }
  }

  // separate out 'set' operations from 'unfixed'
  for(int i=0;i<unfix.size();i++) {
    vector<string> parse = split(unfix[i],'=');
    
    if (parse.size() > 1) {
      doset.push_back(unfix[i]);
      unfix[i] = parse[0];
    }
  }

  // fix parameters
  for(int i=0;i<fix.size();i++) {
    int p=-1;
    if (p=find_parameter(M,fix[i]),p!=-1)
      M.fixed(p,true);
  }

  // unfix parameters
  for(int i=0;i<unfix.size();i++) {
    int p=-1;
    if (p=find_parameter(M,unfix[i]),p!=-1)
      M.fixed(p,false);
  }

  // set parameters
  for(int i=0;i<doset.size();i++) {
    //parse
    vector<string> parse = split(doset[i],'=');
    if (parse.size() != 2)
      throw myexception()<<"Ill-formed initial condition '"<<doset[i]<<"'.";

    string name = parse[0];
    double value = convertTo<double>(parse[1]);

    int p=-1;
    if (p=find_parameter(M,name),p!=-1)
      M.parameter(p,value);
  }

}

OwnedPointer<IndelModel> get_imodel(const variables_map& args) {
  //-------------Choose an indel model--------------//
  OwnedPointer<IndelModel> imodel;

  if (args["imodel"].as<string>() == "simple")
    imodel = SimpleIndelModel();
  else if (args["imodel"].as<string>() == "fragment-based")
    imodel = NewIndelModel(false);
  else if (args["imodel"].as<string>() == "fragment-based+T")
    imodel = NewIndelModel(true);
  else
    throw myexception()<<"Unrecognized indel model '"<<args["imodel"].as<string>()<<"'";
  
  if (args.count("gaps") and args["gaps"].as<string>() == "star")
    imodel->full_tree = false;
  else
    imodel->full_tree = true;

  set_parameters(*imodel,args);

  return imodel;
}

