#include "setup.H"
#include "util.H"

using std::valarray;

substitution::MultiRateModel* get_smodel(Arguments& args, const alphabet& a,const valarray<double>& default_frequencies) {

  /*------ Define some alphabets for reference ------*/
  alphabet dna("DNA nucleotides","AGTC","N");
  alphabet rna("RNA nucleotides","AGUC","N");
  alphabet amino_acids("Amino Acids","ARNDCQEGHILKMFPSTWYV","X");

  /*------ Get the base model (Reversible Markov) ------*/
  substitution::ReversibleModel* base_smodel = 0;
    
  if (args.set("smodel") and args["smodel"] == "EQU")
    base_smodel = new substitution::EQU(a);
  else if (a == dna)
    base_smodel = new substitution::HKY(dna);
  else if (a == rna)
    base_smodel = new substitution::HKY(rna);
  else if (a == amino_acids) {
    string filename = "wag";
    if (args.set("Empirical"))
      filename = args["Empirical"];
    filename = string("Data/") + filename + ".dat";

    base_smodel = new substitution::Empirical(amino_acids,filename);
  }
  else
    assert(0);
    
  /*------ Set frequencies for base model ------*/
  if (args.set("frequencies")) {
    vector<double> f = split<double>(args["frequencies"],',');
    assert(f.size() == a.size());

    valarray<double> f2(f.size());
    for(int i=0;i<f.size();i++)
      f2[i] = f[i];
    base_smodel->frequencies(f2);
  }
  else 
    base_smodel->frequencies(default_frequencies);
    
  /*------ Get the multi-rate model over the base model ------*/
  substitution::MultiRateModel *full_smodel = 0;
  if (args.set("gamma")) {
    int n=4;
    if (args["gamma"] != "gamma")
      n = convertTo<int>(args["gamma"]);
    full_smodel = new substitution::GammaRateModel(*base_smodel,n);
  }
  else 
    full_smodel = new substitution::SingleRateModel(*base_smodel);
  delete base_smodel;

  if (args.set("INV")) {
    substitution::MultiRateModel *temp = full_smodel;
    full_smodel = new substitution::INV_Model(*full_smodel);
    delete temp;
  }

  /*------ Set the parameters for all levels of the model ------*/
  if (args.set("parameters")) {
    vector<double> p = split<double>(args["parameters"],',');
    assert(p.size() == full_smodel->parameters().size());
    full_smodel->parameters(p);
  }

  /* ---------- How does the tree fit in? ------------*/
  if (args["letters"]== "star") 
    full_smodel->full_tree = false;
  else
    full_smodel->full_tree = true;
      
  return full_smodel;
}

substitution::MultiRateModel* get_smodel(Arguments& args, const alignment& A) {
  return get_smodel(args,A.get_alphabet(),empirical_frequencies(A));
}

void link(alignment& A,SequenceTree& T) {

  /*------ Make sure A has enough leaf sequences ----------*/
  if (A.num_sequences() < T.leaves())
    throw myexception(string("Tree has ") + convertToString(T.leaves()) + "leaves but Alignment only has " + convertToString(A.num_sequences()) + "sequences.");

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
    ; // FIXME - check that these are all gap or non-nodes
  
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
      throw myexception(string("Couldn't find sequence \"")+T.seq(i)+"\" in alignment");
    mapping[i] = target;
  }


  T.standardize(mapping);


}

