#include <iostream>
#include "tree.H"
#include "sequencetree.H"
#include "setup.H"
#include "arguments.H"
#include "myexception.H"

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);
  args.print(std::cerr);

  try {
    if (not args.set("tree"))
      throw myexception("Tree file not specified! (tree=<filename>)");
    
    SequenceTree T;
    T.read(args["tree"]);
    
    if (not args.set("root"))
      throw myexception("new root not specified! (root=<sequence name>)");

    int leaf=-1;
    for(int i=0;i<T.n_leaves();i++) {
      if (T.seq(i) == args["root"]) {
	leaf=i;
	break;
      }
    }

    if (leaf == -1)
      throw myexception("root not found in tree!");

    std::cout<<add_root(T,leaf)<<endl;
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
