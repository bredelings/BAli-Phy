#include <string>
#include "myexception.H"
#include "alignment.H"
#include "arguments.H"
#include "sequence-format.H"
#include "setup.H"

int main(int argc,char* argv[]) { 
  try {
    Arguments args;
    args.read(argc,argv);
    args.print(std::cerr);

    alignment A;
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
    
    A.load_sequences(alphabets,sequence_format::read_phylip,std::cin);
    
    remove_empty_columns(A);
    
    if (A.num_sequences() == 0)
      throw myexception()<<"Alignment file "<<args["align"]<<"didn't contain any sequences!";

    A.print_fasta(std::cout);
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}
