#include <iostream>
#include <vector>
#include <string>
#include "arguments.H"
#include "alphabet.H"
#include "alignment.H"
#include "alignmentutil.H"
#include "clone.H"

using namespace std;

int main(int argc,char* argv[]) { 
  try {
    Arguments args;
    args.read(argc,argv);
    args.print(std::cerr);

    /* --------------- Determine the tag ---------------- */
    string tag = "align[MAP";
    if (args.set("tag"))
      tag = args["tag"];

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
