#include <iostream>
#include "arguments.H"
#include "alignment.H"
#include "alignment-util.H"
#include "setup.H"

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  try {
    /* ----- Try to load alignment ------ */

    if (not args.set("align")) 
      throw myexception("Alignment file not specified! (align=<filename>)");

    /* ----- Alphabets to try ------ */
    alignment A;
    load_A(args,A);

    alignment MA = M(A);


    for(int i=0;i<MA.size2();i++) {
      std::cout<<MA.seq(i).name;
      if (i==MA.size2()-1)
	std::cout<<std::endl;
      else
	std::cout<<" ";
    }
      
    
    for(int c=0;c<MA.length();c++) {
      for(int i=0;i<MA.size2();i++) {
	if (MA(c,i) == -1)
	  std::cout<<"-";
	else
	  std::cout<<MA(c,i);
	if (i==MA.size2()-1)
	  std::cout<<std::endl;
	else
	  std::cout<<" ";
      } 
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;
}
