#include <iostream>
#include <vector>

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using namespace std;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("no-scale","don't scale")
    ;

  variables_map args;     
  store(parse_command_line(argc, argv, all), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: srq-to-plot [OPTIONS] < in-file\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}



// assumptions: the value of 'key' is increasing

int main(int argc,char* argv[]) {
  try {

    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    int total=0;
    int i=0;

    vector<double> plot;
    plot.push_back(0);

    while(cin>>i) {
      total += i;
      plot.push_back(total);
    } 

    double scalex=plot.size()-1;
    double scaley=total;
    if (args.count("no-scale")) {
      scalex = 1;
      scaley = 1;
    }

    for(int i=0;i<plot.size();i++) 
      cout<<i/scalex<<"   "<<plot[i]/scaley<<endl;
  }
  catch (exception& e) {
    cerr<<"Exception: "<<e.what()<<std::endl;
    exit(1);
  }
}
