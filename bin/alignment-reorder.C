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

struct lstr {
  bool operator()(const string& s1, const string& s2) const {
   return strcmp(s1.c_str(), s2.c_str()) < 0;
  }
};



/// Find a mapping from v1 to v2
vector<int> find_mapping(const vector<string>& v1,const vector<string>& v2) {
  assert(v1.size() == v2.size());
  vector<int> mapping(v1.size(),-1);
  for(int i=0;i<v1.size();i++) {

    // found v1[i] in v2
    int found = -1;
    for(int j=0;j<v2.size() and found == -1;j++) {
      if (v1[i] == v2[j])
	found = j;
    }

    mapping[i] = found;
    assert(found != -1);
  }
  return mapping;
}

/// get an order list of leaves under T[n]
vector<int> get_leaf_order(const tree& T,int n) {
  if (T[n].leaf()) {
    vector<int> mapping;
    mapping.push_back(n);
    return mapping;
  }

  vector<int> lmapping;
  if (T[n].has_left())
    lmapping = get_leaf_order(T,T[n].left());
  vector<int> rmapping;
  if (T[n].has_right())
    rmapping = get_leaf_order(T,T[n].right());
  
  lmapping.insert(lmapping.end(),rmapping.begin(),rmapping.end());
  return lmapping;
}

/// get an order list of the leaves of T
vector<int> get_leaf_order(const tree& T) {
  int root = T.num_nodes() - 1;
  vector<int> mapping = get_leaf_order(T,root);
  assert(mapping.size() == T.leaves());
  return mapping;
}


int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  try {
    cerr.precision(10);
    cout.precision(10);
    
    /* ----- Try to load tree ------ */
    SequenceTree T;
    if (not args.set("tree"))
      throw myexception("Tree file not specified! (tree=<filename>)");
    T.read(args["tree"]);

    /* ----- Try to load alignment ------ */

    if (not args.set("align")) 
      throw myexception("Alignment file not specified! (align=<filename>)");

    vector<alphabet> alphabets;
    alphabets.push_back(alphabet("DNA nucleotides","AGTC","N"));
    alphabets.push_back(alphabet("RNA nucleotides","AGUC","N"));
    alphabets.push_back(alphabet("Amino Acids","ARNDCQEGHILKMFPSTWYV","X"));
    std::ifstream afile(args["align"].c_str());    
    alignment A;
    A.load_phylip(alphabets,afile);
    afile.close();

    /*------ Link Alignment and Tree ----------*/
    link(A,T);


    /*------- Re-root the tree appropriately  --------*/

    int rootb=-1;
    double rootd = -1;
    find_root(T,rootb,rootd);
    std::cerr<<"root branch = "<<rootb<<std::endl;
    std::cerr<<"x = "<<rootd<<std::endl;
    for(int i=0;i<T.leaves();i++)
      std::cerr<<T.seq(i)<<"  "<<rootdistance(T,i,rootb,rootd)<<std::endl;
    T.branch(rootb).length() = 4.0;

    T.reroot(rootb);   // we don't care about the lengths anymore
    

    /*----- Standardize order by alphabetical order of names ----*/
    vector<string> names = T.get_sequences();
    
    std::sort(names.begin(),names.end(),lstr());

    vector<int> mapping1 = find_mapping(T.get_sequences(),names);

    T.standardize(mapping1);

    /*-------- Compute final mapping  -------*/
    vector<int> mapping2 = get_leaf_order(T);


    vector<int> mapping = compose(mapping1,mapping2);

    for(int i=0;i<mapping.size();i++)
      cerr<<T.seq(mapping[i])<<" ";
    cerr<<std::endl;

    cerr<<"tree = "<<T.write()<<"\n";

    /*------- Print out the alignment -------*/
    alignment A2 = A;
    for(int i=0;i<T.leaves();i++) {
      sequence s(A.seq(mapping[i]));
      s.resize(A.length());
      for(int column=0;column<A.length();column++)
	s[column] = A(column,mapping[i]);
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
