#include <string>
#include "myexception.H"
#include "alignment.H"
#include "sequence-format.H"
#include "alignment-util.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("align", value<string>(),"file with sequences and initial alignment")
    ("tree",value<string>(),"file with initial tree")
    ("with-stop","include stop codons in amino-acid alphabets")
    ;

  // positional options
  positional_options_description p;
  p.add("align", 1);
  p.add("tree", 2);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    std::cout<<"Usage: alignment-convert <alignment-file> <tree-file> ... [OPTIONS]\n";
    std::cout<<all<<"\n";
    exit(0);
  }

  return args;
}


int main(int argc,char* argv[]) { 
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    alignment A = load_A(args);
    
    if (not args.count("output"))
      throw myexception()<<"Output format not specified";

    if (args["output"].as<string>() == "phylip")
      A.print_phylip(std::cout);
    else if (args["output"].as<string>() == "fasta")
      A.print_fasta(std::cout);
    else
      throw myexception()<<"Don't recognized requested format '"<<args["output"].as<string>()<<"'";
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}
