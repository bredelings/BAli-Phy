#include <iostream>
#include "sequencetree.H"
#include "arguments.H"
#include "myexception.H"
#include "util.H"

using namespace std;

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);
  args.print(cerr);

  try {
    if (not args.set("tree"))
      throw myexception("Tree file not specified! (tree=<filename>)");
    
    RootedSequenceTree T;
    T.read(args["tree"]);
    
    T.get_sequences() = truncate_names( T.get_sequences() );

    cout<<T<<endl;
  }
  catch (exception& e) {
    cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
