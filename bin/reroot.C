#include <iostream>
#include "tree.H"
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
      throw myexception("new root specified! (root=<sequence name>)");

    int leaf=-1;
    for(int i=0;i<T.leaves();i++) {
      if (T.seq(i) == args["root"]) {
	leaf=i;
	break;
      }
    }

    if (leaf == -1)
      throw myexception("root not found in tree!");

    T.reroot(leaf);
    
    std::cout<<T<<endl;
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;

}
