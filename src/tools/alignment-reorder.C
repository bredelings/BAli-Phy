#include <iostream>
#include <fstream>
#include <string>
#include "tree.H"
#include "alignment.H"
#include "arguments.H"
#include "alignment-util.H"
#include "util.H"
#include "setup.H"
#include "findroot.H"

using std::cout;
using std::cerr;
using std::endl;

using std::string;

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

/// get an ordered list of leaves under T[n]
vector<int> get_leaf_order(const RootedTree& T,int b) {
  if (T.directed_branch(b).target().is_leaf_node()) {
    vector<int> mapping;
    mapping.push_back( T.directed_branch(b).target() );
    return mapping;
  }

  vector<const_branchview> branches;
  append(T.directed_branch(b).branches_after(),branches);

  vector<int> lmapping = get_leaf_order(T,branches[0]);
  vector<int> rmapping = get_leaf_order(T,branches[1]);

  lmapping.insert(lmapping.end(),rmapping.begin(),rmapping.end());

  return lmapping;
}

/// get an order list of the leaves of T
vector<int> get_leaf_order(const RootedTree& RT) {
  vector<const_branchview> branches;
  append(RT.root().branches_out(),branches);

  vector<int> lmapping = get_leaf_order(RT,branches[0]);
  vector<int> rmapping = get_leaf_order(RT,branches[1]);

  lmapping.insert(lmapping.end(),rmapping.begin(),rmapping.end());

  assert(lmapping.size() == RT.n_leaves());
  return lmapping;
}


int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  try {
    cerr.precision(10);
    cout.precision(10);
    
    //----------- Load alignment and tree ---------//
    alignment A;
    SequenceTree UT;
    load_A_and_T(args,A,UT,false);
    
    //------- Re-root the tree appropriately  --------//

    int rootb=-1;
    double rootd = -1;
    find_root(UT,rootb,rootd);
    std::cerr<<"root branch = "<<rootb<<std::endl;
    std::cerr<<"x = "<<rootd<<std::endl;
    for(int i=0;i<UT.n_leaves();i++)
      std::cerr<<UT.seq(i)<<"  "<<rootdistance(UT,i,rootb,rootd)<<std::endl;

    RootedSequenceTree T = add_root(UT,rootb);  // we don't care about the lengths anymore

    //----- Standardize order by alphabetical order of names ----//
    vector<string> names = T.get_sequences();
    
    std::sort(names.begin(),names.end(),lstr());

    vector<int> mapping1 = find_mapping(T.get_sequences(),names);

    T.standardize(mapping1);


    //-------- Compute final mapping  -------//
    vector<int> mapping2 = get_leaf_order(T);

    //    vector<int> mapping = compose(mapping1,mapping2);
    //    vector<int> mapping = mapping2;
    vector<int> mapping = compose(mapping2,invert(mapping1));

    for(int i=0;i<mapping2.size();i++)
      cerr<<T.seq(mapping2[i])<<" ";
    cerr<<std::endl;

    cerr<<"tree = "<<T.write()<<"\n";

    //------- Print out the alignment -------//
    vector<sequence> sequences = A.get_sequences();
    alignment A2(A.get_alphabet());
    for(int i=0;i<T.n_leaves();i++)
      A2.add_sequence(sequences[mapping[i]]);

    std::cout<<A2;

  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
