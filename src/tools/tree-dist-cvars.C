#include <iostream>
#include <string>
#include <vector>
#include <cmath>

#include "sequencetree.H"
#include "arguments.H"
#include "util.H"
#include "tree-dist.H"

using namespace std;



void write_header(const SequenceTree& T) {

  vector<string> names;
  for(int n1=0;n1<T.n_nodes()-1;n1++)
    for(int n2=0;n2<n1;n2++) {
      string field = string("D")+convertToString(n1)+"-"+convertToString(n2);
      names.push_back(field);
    }

  
  std::cout<<join(names,',')<<endl;
}


void write_out(const SequenceTree& T) {

  vector<string> lengths;
  for(int n1=0;n1<T.n_nodes()-1;n1++)
    for(int n2=0;n2<n1;n2++)
      lengths.push_back(convertToString(T.distance(n1,n2)));
  
  std::cout<<join(lengths,',')<<endl;
}


int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  try {
    bool topology_only = false;
    if (args.set("topology"))
      topology_only = true;


    bool header=false;

    // read in the trees
    string line;
    while(getline(cin,line)) {
      SequenceTree T = standardized(line); 
      if (topology_only) 
	for(int b=0;b<T.branches();b++)
	  T.branch(b).length() = 1.0;

      if (not header) {
	header=true;
	write_header(T);
      }
      write_out(T);
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;

}
