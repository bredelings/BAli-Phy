#include <string>
#include "myexception.H"
#include "alignment.H"
#include "arguments.H"

int main(int argc,char* argv[]) { 
  try {
    Arguments args;
    args.read(argc,argv);
    args.print(std::cerr);

    /*----------- Load alignment and tree ---------*/
    alignment A;

    /* ----- Alphabets to try ------ */
    vector<alphabet> alphabets;
    alphabets.push_back(alphabet("DNA nucleotides","AGTC","N"));
    alphabets.push_back(alphabet("RNA nucleotides","AGUC","N"));
    alphabets.push_back(alphabet("Amino Acids","ARNDCQEGHILKMFPSTWYV","X"));
    
    A.load_phylip(alphabets,std::cin);

    A.print_fasta(std::cout);
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;
}
