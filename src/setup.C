#include <vector>
#include "setup.H"
#include "util.H"
#include "rates.H"
#include "alphabet.H"
#include "alignment-util.H"

using std::vector;
using std::valarray;
using std::cout;
using std::cerr;
using std::endl;

/// Reorder internal sequences of A to correspond to standardized node names for T
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

  counts /= (counts.sum() * A.length());

  // Setup the default frequences for the pseudocounts (uniform)
  double pseudocount = 5+4*a.size();

  if (args.set("CFNF")) {
    pseudocount = 100*counts.size();
  }

  valarray<double> frequencies = A.get_alphabet().get_frequencies_from_counts(counts,pseudocount);

  return frequencies;
}


/// Load an alignment from command line args align=filename
void load_A(Arguments& args,alignment& A) {
  OwnedPointer<AminoAcids> AA = AminoAcids();
  if (args.set("Use Stop"))
    *AA = AminoAcidsWithStop();
  
  vector<OwnedPointer<alphabet> > alphabets;
  if (args["alphabet"] == "Codons") {
    {
      string dna_filename = args["datadir"] + "/" + "genetic_code_dna.dat";
      alphabets.push_back(Codons(DNA(),*AA,dna_filename));
    }

    {
      string rna_filename = args["datadir"] + "/" + "genetic_code_rna.dat";
      alphabets.push_back(Codons(RNA(),*AA,rna_filename));
    }
  }
  else {
    alphabets.push_back(DNA());
    alphabets.push_back(RNA());
    alphabets.push_back(*AA);
  }
  
  /* ----- Try to load alignment ------ */
  if (not args.set("align")) 
    throw myexception("Alignment file not specified! (align=<filename>)");
  
  A.load(alphabets,args["align"]);
  
  remove_empty_columns(A);
  
  if (A.num_sequences() == 0)
    throw myexception()<<"Alignment file "<<args["align"]<<" didn't contain any sequences!";
}


/// Load a tree from command line args align=filename
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
  else {
    RootedSequenceTree RT;
    RT.read(args["tree"]);
    T = remove_root( RT );
  }
}


void link(alignment& A,SequenceTree& T,bool internal_sequences) {

  //------ Make sure A at least has enough leaf sequences ----------//
  if (A.num_sequences() < T.n_leaves())
    throw myexception()<<"Tree has "<<T.n_leaves()<<" leaves but Alignment only has "
		       <<A.num_sequences()<<" sequences.";

  else if (A.num_sequences() == T.n_leaves()) {
    //------- If we just have leaf sequences, add internal sequences -----------//
    if (internal_sequences) {
      // add the sequences
      for(int i=T.n_leaves();i<T.n_nodes();i++) {
	sequence s;
	s.name = string("A") + convertToString(i);
	A.add_sequence(s);
      }

      // set them to all wildcards
      for(int column=0;column<A.length();column++)
	for(int i=T.n_leaves();i<T.n_nodes();i++)
	  A(column,i) = alphabet::not_gap;
    }
  }

  //----- If we have ancestral sequences, make sure we have the right number -----//
  else {
    if (A.num_sequences() > T.n_nodes()) {
      if (internal_sequences)
	throw myexception()<<"More sequences than tree nodes!";
      else
	throw myexception()<<"More sequences than tree nodes!\n Not removing ancestral sequences";
    }
    else if (A.num_sequences() < T.n_nodes())
      if (internal_sequences)
	throw myexception()<<"Less sequences than tree nodes!";
      else
	throw myexception()<<"Less sequences than tree nodes!\n Not removing ancestral sequences";
    else {
      if (not internal_sequences) {

	check_internal_sequences_composition(A,T.n_leaves());

	while(A.size2() > T.n_leaves())
	  A.del_sequence(T.n_leaves());
      }
    }
  }
  
  //---------- Check that we have the right number of sequences ---------//
  if (internal_sequences)
    assert(A.size2() == T.n_nodes());
  else
    assert(A.size2() == T.n_leaves());

  //------- Check that internal sequences don't contain letters --------//
  if (internal_sequences)
    check_internal_sequences_composition(A,T.n_leaves());

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


  //------ Check to see that internal nodes satisfy constraints ------//
  if (internal_sequences)
    check_internal_sequences_composition(A,T.n_leaves());
}



void load_A_and_T(Arguments& args,alignment& A,SequenceTree& T,bool internal_sequences)
{
  bool random_tree_ok = args.set("random_tree_ok");

  load_A(args, A);
  if (not internal_sequences)
    A = chop_internal(A);

  load_T(args, A, T, random_tree_ok);

  //------------- Link Alignment and Tree -----------------//
  link(A,T,internal_sequences);

  //---------------- Randomize alignment? -----------------//
  if (args.set("randomize_alignment"))
    A = randomize(A,T.n_leaves());
  
  //------------------ Analyze 'internal'------------------//
  if (args["internal"] == "+" or args.set("randomize_alignment"))
    for(int column=0;column< A.length();column++) {
      for(int i=T.n_leaves();i<A.size2();i++) 
	A(column,i) = alphabet::not_gap;
    }

  //---- Check that internal sequence satisfy constraints ----//
  if (internal_sequences) {
    check_internal_sequences_composition(A,T.n_leaves());
    check_internal_nodes_connected(A,T);
  }
}

OwnedPointer<IndelModel> get_imodel(Arguments& args) {
  //-------------Choose an indel model--------------//
  OwnedPointer<IndelModel> imodel;

  if (not args.set("imodel")) args["imodel"] = "new";
  
  if (args["imodel"] == "simple")
    imodel = SimpleIndelModel();
  else if (args["imodel"] == "new")
    imodel = NewIndelModel();
  else
    throw myexception()<<"Unrecognized indel model '"<<args["imodel"]<<"'";
  
  if (args["gaps"]== "star") {
    imodel->full_tree = false;
  }
  else
    imodel->full_tree = true;

  vector<double> p = imodel->parameters();
  for(int i=0;i<p.size();i++) {
    if (args.set(imodel->parameter_name(i)))
      p[i] = convertTo<double>(args[imodel->parameter_name(i)]);
  }
  imodel->parameters(p);
    

  return imodel;
}
