#include <iostream>
#include "alignment.H"
#include "alignment-util.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("data-dir", value<string>()->default_value("Data"),"data directory")
    ("align", value<string>(),"file with sequences and initial alignment")
    ("alphabet",value<string>(),"set to 'Codons' to prefer codon alphabets")
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
    cout<<"Usage: alignment-indices <alignment-file> [OPTIONS]\n";
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
    alignment A = load_A(args);

    ublas::matrix<int> MA = M(A);

    //------- Write the sequence names ------//
    for(int i=0;i<A.size2();i++) {
      cout<<A.seq(i).name;
      if (i==A.size2()-1)
	cout<<std::endl;
      else
	cout<<" ";
    }
      
    const alphabet& a = A.get_alphabet();

    //------- Write the columns ------//
    for(int c=0;c<MA.size1();c++) {
      // write the indices
      for(int i=0;i<MA.size2();i++) {
	if (MA(c,i) == -1)
	  cout<<"-";
	else
	  cout<<MA(c,i);
	cout<<" ";
      } 

      // start a comment
      cout<<"    #  ";

      // write the letters
      for(int i=0;i<MA.size2();i++)
	cout<<a.lookup(A(c,i))<<" ";
      cout<<std::endl;
    }
  }
  catch (std::exception& e) {
    cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;
}
