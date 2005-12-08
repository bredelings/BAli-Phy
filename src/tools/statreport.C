#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <valarray>
#include <cmath>

#include "statistics.H"

#include <boost/program_options.hpp>

using namespace std;

namespace po = boost::program_options;
using po::variables_map;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description invisible("Invisible options");
  invisible.add_options()
    ("name", value<string>(),"variable name")
    ;

  options_description visible("All options");
  visible.add_options()
    ("help", "Produce help message")
    ("mean", "Show mean and standard deviation")
    ("median", "Show median and confidence level")
    ("confidence",value<double>()->default_value(0.95),"Confidence level")
    ("precision", value<unsigned>()->default_value(3),"Number of significant figures")
    ;

  options_description all("All options");
  all.add(visible).add(invisible);

  // positional options
  positional_options_description p;
  p.add("name", 1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);    

  bool error = false;

  if (not args.count("name"))
    error = true;

  if (args.count("help") or error) {
    cerr<<"Usage: statreport <variable name> [OPTIONS] < values-file \n";
    cerr<<visible<<"\n";
    if (error)
      exit(1);
    else
      exit(0);
  }

  return args;
}


int main(int argc,char* argv[]) { 
  try {
    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    cout.precision(args["precision"].as<unsigned>());

    string label = args["name"].as<string>();

    // Read in the values
    vector<double> values;
    double d;
    while(cin>>d)
      values.push_back(d);
    cerr<<"Read in "<<values.size()<<" values\n";

    // Translate vector to valarray...
    valarray<double> values2(values.size());
    for(int i=0;i<values2.size();i++)
      values2[i] = values[i];
    
    // Print out mean and standard deviation
    if (args.count("mean")) {
      cout<<" E "<<label<<" = "<<statistics::average(values2);
      cout<<"  [+- "<<sqrt(statistics::Var(values2))<<"]"<<endl;
    }

    // Print out median and confidence interval
    if (args.count("median") or not args.count("mean")) {
      double P = args["confidence"].as<double>();

      pair<double,double> interval = statistics::confidence_interval(values2,P);
      cout<<"   "<<label<<" = "<<statistics::median(values2);
      cout<<"  ("<<interval.first<<","<<interval.second<<")"<<endl;
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}


