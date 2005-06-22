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

using std::valarray;

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::endl;

using std::string;

int min(const vector<int>& v) {
  int m = v[0];
  for(int i=1;i<v.size();i++)
    if (v[i]<m)
      m = v[i];
  return m;
};

int n_mutations(const alphabet& a, const vector<int>& letters, const Tree& T) 
{
  const int A = a.size();

  assert(letters.size() == T.n_leaves());

  vector< vector<int> > n_muts(T.n_nodes(),vector<int>(A,0));

  int root = T.directed_branch(0).target();
  vector<const_branchview> branches = branches_toward_node(T,root);

  vector<int> temp(A);

  for(int i=0;i<branches.size();i++) 
  {
    int s = branches[i].source();
    int t = branches[i].target();
    if (branches[i].source().is_leaf_node()) {
      assert(0 <= s and s < letters.size());
      if (alphabet::letter(letters[s])) {
	for(int l=0;l<A;l++)
	  n_muts[t][l] += 1;
	n_muts[t][letters[s]]--;
      }
    }
    else {
      for(int l=0;l<A;l++)
	temp[l] = n_muts[s][l]+1;

      for(int l=0;l<A;l++) {
	temp[l]--;
	n_muts[t][l] += min(temp);
	temp[l]++;
      }
    }
  }

  return min(n_muts[root]);
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
    ("reroot","estimate the root of the tree")
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
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}


bool is_informative(const valarray<int>& count,int level) 
{
  int n = 0;
  for(int l=0;l<count.size();l++)
    if (count[l] > level) n++;

  if (level == 0) assert(n > 0);
  return n>1;
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
    if (args.count("tree"))
      load_A_and_T(args,A,T,false);
    else
      A = load_A(args,false);
    const alphabet& a = A.get_alphabet();
    
    //------- Re-root the tree appropriately  --------//

    if (args.count("reroot")) {
      int rootb=-1;
      double rootd = -1;
      find_root(T,rootb,rootd);
      std::cerr<<"root branch = "<<rootb<<std::endl;
      std::cerr<<"x = "<<rootd<<std::endl;
      for(int i=0;i<T.n_leaves();i++)
	std::cerr<<T.seq(i)<<"  "<<rootdistance(T,i,rootb,rootd)<<std::endl;
      
      T = add_root((SequenceTree)T,rootb);  // we don't care about the lengths anymore
    }

    //----- Count informative/non-constant sites ----//
    valarray<bool> informative(A.length());
    valarray<bool> different(A.length());

    valarray<int> count(a.size());
    for(int c=0;c<A.length();c++) {
      count = 0;
      for(int i=0;i<A.n_sequences();i++) {
	int l = A(c,i);
	if (alphabet::letter(l))
	  count[l]++;
      }

      different[c] =  is_informative(count,0);
      informative[c] = is_informative(count,1);
    }
    
    int n_different = n_elements(different);
    int n_informative = n_elements(informative);


    cout.precision(3);

    std::cout<<"Alignment: "<<A.length()<<" columns of "<<A.n_sequences()<<" sequences\n";
    std::cout<<"  "<<n_different<<" ("<<double(n_different)/A.length()*100<<"%) sites are not constant.\n";
    std::cout<<"  "<<n_informative<<" ("<<double(n_informative)/A.length()*100<<"%) are informative.\n";

    //------------ Get Tree Lengths ------------//
    int tree_length = 0;
    if (args.count("tree")) {
      vector<int> letters(A.n_sequences());
      for(int c=0;c<A.length();c++) {
	for(int i=0;i<A.n_sequences();i++)
	  letters[i] = A(c,i);
	int length = n_mutations(a,letters,T);
	tree_length += length;
	std::cout<<length;
	if (informative[c]) std::cout<<"  [informative]";
	std::cout<<"\n";
      }
      std::cout<<"tree length = "<<tree_length<<"\n";
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
