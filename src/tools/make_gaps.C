#include "alignment.H"
#include "arguments.H"
#include "tree.H"
#include "rng.H"
#include "myexception.H"

int main(int argc,char* argv[]) { 
  try {
    /*--------------Load the Sequences and Tree------------------*/

    Arguments args;
    args.read(argc,argv);
  
    if (not args.set("align"))
      throw myexception("Alignment file not specified! (align=<filename>)");

    alphabet dna("DNA nucleotides","AGTC","N");
    alignment A(dna,args["align"]);

    if (not args.set("tree"))
      throw myexception("Alignment file not specified! (tree=<filename>)");
      
    SequenceTree T;
    T.read(args["tree"]);

    /*--------------Make the alignment here------------------*/
    matrix<char> A;
    
  }
  catch (std::exception& e) {
    std::cerr<<"make_gaps: Error! "<<e.what()<<endl;
    return 1;
  }
  return 0;
}
