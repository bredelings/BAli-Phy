#include "myexception.H"
#include "alignment.H"
#include "arguments.H"

void do_setup(Arguments& args,alignment& A,SequenceTree& T) {
  /* ----- Alphabets to try ------ */
  alphabet dna("DNA nucleotides","AGTC","N");
  alphabet rna("RNA nucleotides","AGUC","N");
  alphabet amino_acids("Amino Acids","ARNDCQEGHILKMFPSTWYV","X");

  /* ----- Try to load alignment ------ */
  if (not args.set("align")) 
    throw myexception("Alignment file not specified! (align=<filename>)");

  try {
    A.load(dna,args["align"]);
  }
  catch (bad_letter& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;

    try {
      A.load(rna,args["align"]);
    }
    catch (bad_letter& e) {
      std::cerr<<"Exception: "<<e.what()<<endl;
      
      A.load(amino_acids,args["align"]);
    }
  }

  remove_empty_columns(A);

  if (A.num_sequences() == 0) 
    throw myexception(string("Alignment file") + args["align"] + "didn't contain any sequences!");
    
  /*------ Try to load tree -------------*/
  if (not args.set("tree")) {
    vector<string> s;
    for(int i=0;i<A.num_sequences();i++)
      s.push_back(A.seq(i).name);
    T = RandomTree(s,0.05);
  }
  else 
    T.read(args["tree"]);

  /*------ Link Alignment and Tree ----------*/

  if (A.num_sequences() < T.leaves())
    throw myexception(string("Tree has ") + convertToString(T.leaves()) + "leaves but Alignment only has " + convertToString(A.num_sequences()) + "sequences.");

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

  /*------- Fill in internal nodes ---------*/
  if (A.num_sequences() == T.num_nodes() - 1)
    ;
  else if (A.num_sequences() == T.leaves()) {
    sequence s(A.get_alphabet());
    s.resize(A.length());
    for(int column=0;column<A.length();column++)
      s[column] = alphabet::not_gap;
    for(int i=T.leaves();i<T.num_nodes()-1;i++)
      A.add_sequence(s);
  }
  else if (A.num_sequences() > T.num_nodes())
    throw myexception(string("More sequences than tree nodes!"));
  else
    throw myexception(string("Not enough ancestral sequences!"));
}



int main(int argc,char* argv[]) { 
  try {
    Arguments args;
    args.read(argc,argv);
    args.print(std::cout);


    /*----------- Load alignment and tree ---------*/
    alignment A;
    SequenceTree T;
    do_setup(args,A,T);
    
    /*---------- Load sampled alignments ----------*/
    // FIXME read PHYLIP alignments - we'll assume that they
    // correctly specify the length
    
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;
}
