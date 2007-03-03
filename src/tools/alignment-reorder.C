#include <iostream>
#include <fstream>
#include <string>
#include "tree.H"
#include "alignment.H"
#include "alignment-util.H"
#include "util.H"
#include "setup.H"
#include "findroot.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;
using std::valarray;

using std::string;

int first_bit(const valarray<bool>& v) 
{
  for(int i=0;i<v.size();i++) {
    if (v[i]) return i;
  }
  return v.size();
}

struct branch_order {
  const Tree& T;

  bool operator()(int b1,int b2) const {
    if (subtree_height(T,b1) < subtree_height(T,b2))
      return true;
    if (subtree_height(T,b1) > subtree_height(T,b2))
      return false;
    return first_bit(T.partition(b1)) < first_bit(T.partition(b2));
  }

  branch_order(const Tree& T_): T(T_) {}
};


/// get an ordered list of leaves under T[n]
vector<int> get_leaf_order(const Tree& T,int b) 
{
  vector<int> mapping;

  if (T.directed_branch(b).target().is_leaf_node()) {
    mapping.push_back( T.directed_branch(b).target() );
    return mapping;
  }

  // get sorted list of branches
  vector<const_branchview> branches;
  append(T.directed_branch(b).branches_after(),branches);
  std::sort(branches.begin(),branches.end(),branch_order(T));

  // accumulate results
  for(int i=0;i<branches.size();i++) {
    vector<int> sub_mapping = get_leaf_order(T,branches[i]);
    mapping.insert(mapping.end(),sub_mapping.begin(),sub_mapping.end());
  }

  return mapping;
}

/// get an order list of the leaves of T
vector<int> get_leaf_order(const RootedTree& RT) 
{
  vector<int> mapping;

  // get sorted list of branches
  vector<const_branchview> branches;
  append(RT.root().branches_out(),branches);
  std::sort(branches.begin(),branches.end(),branch_order(RT));

  // accumulate results
  for(int i=0;i<branches.size();i++) {
    vector<int> sub_mapping = get_leaf_order(RT,branches[i]);
    mapping.insert(mapping.end(),sub_mapping.begin(),sub_mapping.end());
  }

  assert(mapping.size() == RT.n_leaves());
  return mapping;
}


variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("align", value<string>(),"file with sequences and initial alignment")
    ("tree",value<string>(),"file with initial tree")
    ("use-root","use the root specified in the tree file")
    ("alphabet",value<string>(),"Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.")
    ;

  // positional options
  positional_options_description p;
  p.add("align", 1);
  p.add("tree", 2);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  // store(parse_command_line(argc, argv, desc), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: alignment-reorder <alignment-file> <tree-file>\n";
    cout<<"Reorder sequences so that related sequences are adjacent.\n\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}


int main(int argc,char* argv[]) 
{ 
  try {
    cerr.precision(10);
    cout.precision(10);
    
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //----------- Load alignment and tree ---------//
    alignment A;
    RootedSequenceTree T;
    load_A_and_T(args,A,T,false);
    
    //------- Re-root the tree appropriately  --------//
    if (not args.count("use-root")) {
      int rootb=-1;
      double rootd = -1;
      find_root(T,rootb,rootd);
      std::cerr<<"root branch = "<<rootb<<std::endl;
      std::cerr<<"x = "<<rootd<<std::endl;
      for(int i=0;i<T.n_leaves();i++)
	std::cerr<<T.seq(i)<<"  "<<rootdistance(T,i,rootb,rootd)<<std::endl;
      
      T = add_root((SequenceTree)T,rootb);  // we don't care about the lengths anymore
    }

    //----- Standardize order by alphabetical order of names ----//
    vector<string> names = T.get_sequences();
    
    std::sort(names.begin(),names.end());

    vector<int> mapping1 = compute_mapping(T.get_sequences(),names);

    T.standardize(mapping1);


    //-------- Compute the mapping  -------//
    vector<int> mapping2 = get_leaf_order(T);

    // re-phrase the mapping in terms of the order of sequence in A
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
