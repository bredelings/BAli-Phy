#include <iostream>
#include <fstream>
#include "tree.H"
#include "alignment.H"
#include "arguments.H"
#include "rng.H"

double objective_function(const alignment& A,const tree& T,const vector<int>& mapping) {
  double total = 0;
  for(int i=0;i<T.leaves()-1;i++) {
    total += T.distance(mapping[i],mapping[i+1]);
  }
  return total;
}


vector<int> optimize_mapping(const alignment& A,const tree& T) {
  vector<int> mapping(T.leaves());
  for(int i=0;i<T.leaves();i++)
    mapping[i] = i;

  double y = objective_function(A,T,mapping);
  int iterations=0;
  while(1) {
    int i = myrandom(T.leaves());
    int j = myrandom(T.leaves()-1);
    if (j >=i) j++;
    
    vector<int> mapping2 = mapping;
    std::swap(mapping2[i],mapping2[j]);
    double y2 = objective_function(A,T,mapping2);

    if (y2 < y) {
      mapping = mapping2;
      y = y2;
      iterations=0;
    }
    else
      iterations++;

    if (iterations > 100) break;
  }
  return mapping;
}

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  try {
    SequenceTree T;
    if (not args.set("tree"))
      throw myexception("Tree file not specified! (tree=<filename>)");
    T.read(args["tree"]);

    /* ----- Try to load alignment ------ */

    if (not args.set("align")) 
      throw myexception("Alignment file not specified! (align=<filename>)");

    /* ----- Alphabets to try ------ */
    vector<alphabet> alphabets;
    alphabets.push_back(alphabet("DNA nucleotides","AGTC","N"));
    alphabets.push_back(alphabet("RNA nucleotides","AGUC","N"));
    alphabets.push_back(alphabet("Amino Acids","ARNDCQEGHILKMFPSTWYV","X"));
    std::ifstream afile(args["align"].c_str());    
    alignment A;
    A.load_phylip(alphabets,afile);
    afile.close();

    /*------ Link Alignment and Tree ----------*/
    if (A.num_sequences() < T.leaves())
      throw myexception("Tree has more sequences that alignment!");
    
    vector<int> mapping(T.leaves());
    for(int i=0;i<T.leaves();i++) {
      int target = -1;
      for(int j=0;j<T.leaves();j++) {
	if (T.seq(i) == A.seq(j).name) {
	  target = j;
	  break;
	}
      }
      if (target == -1)
	throw myexception(string("Couldn't find sequence \"")+T.seq(i)+"\" in alignment");
      mapping[i] = target;
    }

    T.standardize(mapping);

    /*------- Find mapping which puts nearby things together -------*/
    mapping = optimize_mapping(A,T);

    for(int i=0;i<mapping.size();i++)
      std::cout<<T.seq(mapping[i])<<" ";
    std::cout<<std::endl;
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;

}
