#include <iostream>
#include <fstream>
#include <string>
#include "alignment.H"
#include "alignment-util.H"
#include "util.H"
#include "setup.H"
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
    ("alphabet",value<string>()->default_value("Codons"),"set to 'Codons+stop' to allow stop codons")
    ("frame",value<int>()->default_value(0),"frame 0, 1, or 2")
    ;

  variables_map args;     
  store(parse_command_line(argc, argv, all), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-translate [OPTIONS] < sequence-file [OPTIONS]\n";
    cout<<"Translate a DNA/RNA alignment into amino acids.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  load_bali_phy_rc(args,all);

  return args;
}


int main(int argc,char* argv[]) 
{ 

  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //------- Try to load sequences --------//
    vector<sequence> sequences = sequence_format::read_guess(std::cin);

    if (sequences.size() == 0)
      throw myexception()<<"Alignment file read from STDIN  didn't contain any sequences!";

    //------- Convert sequences to specified reading frame --------//
    int frame = args["frame"].as<int>();

    if (frame < 0 or frame > 2)
      throw myexception()<<"You may only specify frame 0, 1, or 2: "<<frame<<" is right out.";
    
    for(int i=0;i<sequences.size();i++) {
      if (sequences.size() > frame)
	sequences[i].erase(sequences[i].begin(),sequences[i].begin()+frame);
      unsigned newsize = sequences[i].size();
      newsize -= newsize%3;
      sequences[i].resize(newsize);
    }

    //----------- Convert sequences to codons ------------//
    alignment A1;
    A1.load(load_alphabets(args), sequences);

    OwnedPointer<Codons> C = *dynamic_cast<const Codons*>(&A1.get_alphabet());
    OwnedPointer<AminoAcids> AA = C->getAminoAcids();
    
    //------- Convert sequence codons to amino acids  --------//
    alignment A2(*AA);
    for(int i=0;i<A1.n_sequences();i++) {
      sequence S;
      S.name = A1.seq(i).name;
      S.comment = A1.seq(i).comment;
      for(int column=0;column<A1.length();column++) {
	int cc = A1(column,i);
	int aa = alphabet::not_gap;

	if (cc < C->size())
	  aa = C->translate(cc);
	else {
	  vector<int> ccs;
	  vector<int> aas;
	  for(int i=0;i<C->size();i++)
	    if (C->matches(i,cc))
	      ccs.push_back(i);
	  for(int i=0;i<ccs.size();i++)
	    aas.push_back(C->translate(ccs[i]));

	  vector<int> uniq_aas;
	  for(int i=0;i<aas.size();i++)
	    if (i == 0 or aas[i] != aas[i-1])
	      uniq_aas.push_back(aas[i]);

	  if (uniq_aas.size() == 1)
	    aa = uniq_aas[0];
	  //	  else
	  //	    cerr<<"Codon class maps to "<<uniq_aas.size()<<" amino acids."<<endl;
	}

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
