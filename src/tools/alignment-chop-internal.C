#include <iostream>
#include <fstream>
#include <string>
#include "tree.H"
#include "alignment.H"
#include "arguments.H"
#include "util.H"
#include "setup.H"
#include "findroot.H"

using std::cout;
using std::cerr;
using std::endl;

using std::string;

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  try {
    cerr.precision(10);
    cout.precision(10);
    
    //----------- Load alignment  ---------//
    alignment A;
    load_A(args,A);

    /*------- Print out the alignment -------*/
    int N = (A.size2()+2)/2;

    alignment A2;
    for(int i=0;i<N;i++) {
      sequence s(A.seq(i));
      s.resize(A.length());
      for(int column=0;column<A.length();column++)
	s[column] = A(column,i);
      A2.add_sequence(s);
    }

    A2.print_phylip(std::cout,true);

  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
