#include <iostream>
#include "arguments.H"
#include "alignment.H"
#include "alignment-util.H"
#include "setup.H"

using std::cout;
using std::cerr;

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  try {
    /* ----- Try to load alignment ------ */

    if (not args.set("align")) 
      throw myexception("Alignment file not specified! (align=<filename>)");

    alignment A;
    load_A(args,A);

    alignment MA = M(A);


    //------- Write the sequence names ------//
    for(int i=0;i<MA.size2();i++) {
      cout<<MA.seq(i).name;
      if (i==MA.size2()-1)
	cout<<std::endl;
      else
	cout<<" ";
    }
      
    const alphabet& a = MA.get_alphabet();

    //------- Write the columns ------//
    for(int c=0;c<MA.length();c++) {
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
