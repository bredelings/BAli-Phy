#include <iostream>
#include <fstream>
#include <string>
#include "alignment.H"
#include "alignment-util.H"
#include "util.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;

//FIXME - make this handle un-aligned gaps...
// diagnose sequences which are not a multiple of 3
// look for reading frames?  start codons?
// translate just the sequences before translating
// the ALIGNMENT of the sequences to print out

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("data-dir", value<string>()->default_value("Data"),"data directory")
    ("align", value<string>(),"file with sequences and initial alignment")
    ("alphabet",value<string>()->default_value("Codons"),"set to 'Codons' to prefer codon alphabets")
    ("with-stop","include stop codons in amino-acid alphabets")
    ;

  // positional options
  positional_options_description p;
  p.add("align", 1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-translate <alignment-file> [OPTIONS]\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}


int main(int argc,char* argv[]) 
{ 

  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //------- Try to load alignment --------//
    alignment A1 = load_A(args);

    OwnedPointer<Codons> C = *dynamic_cast<const Codons*>(&A1.get_alphabet());
    OwnedPointer<AminoAcids> AA = C->getAminoAcids();
    
    //------- Re-root the tree appropriately  --------//
    alignment A2;
    for(int i=0;i<A1.size2();i++) {
      sequence S;
      S.name = A1.seq(i).name;
      for(int column=0;column<A1.length();column++) {
	int cc = A1(column,i);
	int aa = C->translate(cc);
	S += AA->lookup(aa);
      }
      A2.add_sequence(S);
    }

    std::cout<<A2<<std::endl;
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
