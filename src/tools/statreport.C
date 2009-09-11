#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <valarray>
#include <cmath>

#include "util.H"
#include "statistics.H"
#include "stats-table.H"

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
    ("mask", value<string>(),"which fields to print")
    ;

  options_description visible("All options");
  visible.add_options()
    ("help", "Produce help message")
    ("skip",value<int>()->default_value(0),"number of trees to skip")
    ("max",value<int>(),"maximum number of trees to read")
    ("mean", "Show mean and standard deviation")
    ("median", "Show median and confidence level")
    ("autocorrelation", "Show autocorrelation time and effective sample size.")
    ("confidence",value<double>()->default_value(0.95),"Confidence level")
    ("precision", value<unsigned>()->default_value(4),"Number of significant figures")
    ("verbose","Output more log messages on stderr.")
    ;

  options_description all("All options");
  all.add(invisible).add(visible);

  // positional options
  positional_options_description p;
  p.add("mask", 1);

  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);

  if (args.count("help")) {
    cout<<"Usage: statreport [OPTIONS] < data-file \n";
    cout<<"Compute summary statistics for Tracer-format data files.\n\n";
    cout<<visible<<"\n";
    exit(0);
  }

  if (args.count("verbose")) log_verbose = 1;

  return args;
}

bool constant(const vector<double>& values)
{
  for(int i=1;i<values.size();i++)
    if (values[i] != values[0])
      return false;
  return true;
}


void show_stats(variables_map& args, const stats_table& data,int index)
{
  const string& name = data.names()[index];
  const vector<double>& values = data.column(index);

  using namespace statistics;

  if (constant(values)) {
    cout<<"   "<<name<<" = "<<values[0]<<endl;
    return;
  }

  // Translate vector to valarray...
  valarray<double> values2(values.size());
  for(int i=0;i<values2.size();i++)
    values2[i] = values[i];
    
  // Print out mean and standard deviation
  if (args.count("mean")) {
    cout<<" E "<<name<<" = "<<average(values2);
    cout<<"  [+- "<<sqrt(Var(values2))<<"]"<<endl;
  }

  // Print out median and confidence interval
  if (args.count("median") or not args.count("mean")) {
    double P = args["confidence"].as<double>();
    
    pair<double,double> interval = confidence_interval(values2,P);
    cout<<"   "<<name<<" ~ "<<median(values2);
    if ((1.0-P)*values2.size() >= 10.0)
      cout<<"  ("<<interval.first<<","<<interval.second<<")"<<endl;
    else
      cout<<"  (NA,NA)"<<endl;
  }

  double tau = autocorrelation_time(values2);

  string spacer;spacer.append(name.size()-1,' ');

  cout<<"   "<<spacer<<"t @ "<<tau;
  cout<<"   Ne = "<<values2.size()/tau<<endl;

  cout<<endl;
}

vector<bool> get_mask(const string& s,vector<bool> mask)
{
  vector<string> bounds = split(s,':');
  if (bounds.size() != 2)
    throw myexception()<<"Can't understand the column range '"<<s<<"'.";

  unsigned start = 1;
  unsigned end = mask.size();
  if (bounds[0].size())
    start = convertTo<unsigned>(bounds[0]);
  if (bounds[1].size())
    start = convertTo<unsigned>(bounds[1]);
  start = max(1U,start);
  end = min((unsigned)(mask.size()),end);

  for(int i=0;i<mask.size();i++)
    if (start <=i+1 and i+1 <= end)
      ;
    else
      mask[i]=false;

  return mask;
}

int main(int argc,char* argv[]) 
{ 
  try {
    //----------- Parse command line  -----------//
    variables_map args = parse_cmd_line(argc,argv);

    cout.precision(args["precision"].as<unsigned>());

    int skip = args["skip"].as<int>();

    int max = -1;
    if (args.count("max"))
      max = args["max"].as<int>();

    //------------ Read Data ---------------//
    stats_table data(std::cin,skip,max);

    if (data.n_rows() == 0)
      throw myexception()<<"No data line read in!";

    if (log_verbose) cerr<<"statreport: Read in "<<data.n_rows()<<" lines.\n";

    //------------ Parse column mask ----------//
    vector<bool> mask(data.n_columns(),true);
    
    if (args.count("mask"))
      mask = get_mask(args["mask"].as<string>(),mask);

    //------------ Generate Report ----------//
    for(int i=0;i<data.n_columns();i++) 
      if (mask[i]) {
	show_stats(args, data, i);
	cout<<endl;
      }
  }
  catch (std::exception& e) {
    std::cerr<<"statreport: Error! "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}


