#include <iostream>
#include <vector>
#include "arguments.H"

using namespace std;

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  int total=0;
  int i=0;

  vector<double> plot;
  plot.push_back(0);

  while(cin>>i) {
    total += i;
    plot.push_back(total);
  } 

  double scale=1.0;
  if (args.set("scale"))
    scale = plot.size()-1;

  for(int i=0;i<plot.size();i++) 
    cout<<i/scale<<"   "<<plot[i]/total<<endl;
}
