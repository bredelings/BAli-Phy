#include <iostream>
#include <vector>
#include <string>
#include "alphabet.H"
#include "alignment.H"
#include "alignmentutil.H"
#include "clone.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using namespace std;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("tag", value<string>(),"only read alignments preceded by 'align[<tag>'")
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
    cout<<"Usage: alignment-find [OPTIONS] < in-file \n";
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

    /* --------------- Determine the tag ---------------- */
    string tag = "align[";
    if (args.count("tag"))
      tag += args["tag"].as<string>();

    /* --------------- Alphabets to try ---------------- */
    vector<OwnedPointer<alphabet> > alphabets;
    alphabets.push_back(DNA());
    alphabets.push_back(RNA());
    alphabets.push_back(AminoAcids());

    /*---------------- Find the alignment ----------------*/
    alignment A = find_last_alignment(std::cin, tag, alphabets);

    /*---------------- Print it out ----------------*/
    std::cout<<A;
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
