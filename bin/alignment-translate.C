#include <iostream>
#include <fstream>
#include <string>
#include "tree.H"
#include "alignment.H"
#include "arguments.H"
#include "util.H"
#include "setup.H"


using std::cout;
using std::cerr;
using std::endl;

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  try {
    cerr.precision(10);
    cout.precision(10);
    
    vector<OwnedPointer<alphabet> > alphabets;
    Translation_Table *Tp=0;
    {
      ifstream genetic_code("Data/genetic_code_dna.dat");
      if (not genetic_code)
	throw myexception()<<"Couldn't open file 'Data/genetic_code_dna.dat'";
      Tp = new Translation_Table(Codons(DNA()),AminoAcids(),genetic_code);

      genetic_code.close();

      alphabets.push_back(Tp->getCodons());
    }

    {
      ifstream genetic_code("Data/genetic_code_rna.dat");
      if (not genetic_code)
	throw myexception()<<"Couldn't open file 'Data/genetic_code_rna.dat'";
      Tp = new Translation_Table(Codons(RNA()),AminoAcids(),genetic_code);
      genetic_code.close();

      alphabets.push_back(Tp->getCodons());
    }

    // ----- Try to load alignment ------ //
    if (not args.set("align")) 
      throw myexception("Alignment file not specified! (align=<filename>)");
    
    alignment A1;
    A1.load(alphabets,args["align"]);
    
    remove_empty_columns(A1);
    
    if (A1.num_sequences() == 0)
      throw myexception()<<"Alignment file "<<args["align"]<<"didn't contain any sequences!";

    //------- Re-root the tree appropriately  --------//
    alignment A2;
    for(int i=0;i<A1.size2();i++) {
      sequence S(Tp->getAminoAcids());
      S.name = A1.seq(i).name;
      for(int column=0;column<A1.length();column++)
	S.push_back((*Tp)[A1(column,i)]);
      A2.add_sequence(S);
    }

    A2.print_phylip(std::cout,true);
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
