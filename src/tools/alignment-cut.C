#include <iostream>
#include <fstream>
#include <string>
#include "alignment.H"
#include "alignment-util.H"
#include "util.H"
#include "sequence-format.H"
#include <boost/program_options.hpp>

using namespace sequence_format;
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
  options_description invisible("Invisible options");
  invisible.add_options()
    ("range", value<vector<string> >()->composing(),"range to keep")
    ;

  options_description visible("All options");
  visible.add_options()
    ("help", "produce help message")
    ;

  options_description all("All options");
  all.add(visible).add(invisible);

  // positional options
  positional_options_description p;
  p.add("range", -1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);    

  bool error = false;

  if (not args.count("range")) {
    std::cerr<<"Error: no ranges specified!\n";
    error = true;
  }

  if (args.count("help") or error) {
    cout<<"Usage: alignment-cut [range1] {[range2] ...} < sequence-file \n";
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

    //------- Try to load sequences --------//
    vector<sequence> sequences = sequence_format::read_guess(std::cin);

    if (sequences.size() == 0)
      throw myexception()<<"Alignment file read from STDIN  didn't contain any sequences!";

    for(int i=1;i<sequences.size();i++)
      if (sequences[i].size() != sequences[0].size())
	throw myexception()<<"Sequence #"<<i+1<<" '"<<sequences[i].name<<"' has length "
			   <<sequences[i].size()<<" != "<<sequences[0].size();

    const unsigned L = sequences[0].size();

    //------- Start computing result --------//
    vector<sequence> results = sequences;
    for(int i=0;i<sequences.size();i++)
      results[i].string::operator=("");


    vector<string> ranges = args["range"].as<vector<string> >();

    for(int i=0;i<ranges.size();i++) {
      vector<string> range = split(ranges[i],'-');

      if (range.size() != 2)
	throw myexception()<<"Malformed range '"<<ranges[i]<<"'";

      int begin = convertTo<int>(range[0])-1;
      int end       = convertTo<int>(range[1])-1;

      if (begin < 0)
	throw myexception()<<"Bad range '"<<ranges[i]<<"': begins before 1!";

      if (begin >= L)
	begin = L-1;

      if (end < 0)
	throw myexception()<<"Bad range '"<<ranges[i]<<"': ends before 1!";

      if (end >= L)
	end = L-1;

      if (end < begin)
	throw myexception()<<"Bad range '"<<ranges[i]<<"': begins after end!";

      for(int k=0;k<sequences.size();k++)
	results[k] += sequences[k].substr(begin,end-begin+1);
    }

    write_fasta(std::cout,results);
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
