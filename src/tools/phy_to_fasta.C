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
    OwnedPointer<AminoAcids> AA = AminoAcids();
    if (args.set("Use Stop"))
      *AA = AminoAcidsWithStop();

    vector<OwnedPointer<alphabet> > alphabets;
    if (args.set("Use Codons")) {
      string dna_filename = args["datadir"] + "/" + "genetic_code_dna.dat";
      string rna_filename = args["datadir"] + "/" + "genetic_code_rna.dat";

      Codons DNA_codons(DNA(),*AA,dna_filename);
      Codons RNA_codons(RNA(),*AA,rna_filename);
	
      alphabets.push_back(DNA_codons);
      alphabets.push_back(RNA_codons);
    }
    else {
      alphabets.push_back(DNA());
      alphabets.push_back(RNA());
      alphabets.push_back(AminoAcids());
    }
    
    A.load(alphabets,sequence_format::read_phylip,std::cin);
    
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
