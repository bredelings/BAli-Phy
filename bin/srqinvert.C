#include <iostream>
#include <vector>
#include "myexception.H"
using namespace std;

int main(int argc,char* argv[]) { 
  try {

    int i;
    while(cin>>i) {
      if (i < 0 or i> 1)
	throw myexception()<<"Found an entry not 0 or 1!";
      std::cout<<(1-i)<<"\n";
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
