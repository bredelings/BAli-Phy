#include <iostream>
#include <fstream>
#include <string>
#include "tree.H"
#include "alignment.H"
#include "alignment-util.H"
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

    alignment A2 = chop_internal(A);
    std::cout<<A2<<std::endl;

  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
