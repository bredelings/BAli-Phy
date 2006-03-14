#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <valarray>
#include <cmath>

#include "util.H"
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
    ("mask", value<string>(),"which fields to print")
    ;

  options_description visible("All options");
  visible.add_options()
    ("help", "Produce help message")
    ("skip",value<int>()->default_value(0),"number of trees to skip")
    ("max",value<int>(),"maximum number of trees to read")
    ("mean", "Show mean and standard deviation")
    ("median", "Show median and confidence level")
    ("confidence",value<double>()->default_value(0.95),"Confidence level")
    ("precision", value<unsigned>()->default_value(3),"Number of significant figures")
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
    cerr<<"Usage: statreport [OPTIONS] < data-file \n";
    cerr<<visible<<"\n";
    exit(0);
  }

  return args;
}

bool constant(const vector<double>& values)
{
  for(int i=1;i<values.size();i++)
    if (values[i] != values[0])
      return false;
  return true;
}


void show_stats(variables_map& args, const string& name,const vector<double>& values)
{
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
    cout<<" E "<<name<<" = "<<statistics::average(values2);
    cout<<"  [+- "<<sqrt(statistics::Var(values2))<<"]"<<endl;
  }



  // Print out median and confidence interval
  if (args.count("median") or not args.count("mean")) {
    double P = args["confidence"].as<double>();
    
    pair<double,double> interval = statistics::confidence_interval(values2,P);
    cout<<"   "<<name<<" ~ "<<statistics::median(values2);
    if ((1.0-P)*values2.size() >= 10.0)
      cout<<"  ("<<interval.first<<","<<interval.second<<")"<<endl;
    else
      cout<<"  (NA,NA)"<<endl;
  }
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

    //------------ Parse column names ----------//
    string line;
    getline(std::cin,line);

    vector<string> headers = split(line,'\t');

    if (headers.size() == 0)
      throw myexception()<<"No column names provided!";

    for(int i=0;i<headers.size();i++)
      if (headers[i].size() == 0)
	throw myexception()<<"The "<<i<<"th column name is blank!";


    //------------ Parse column mask ----------//
    vector<bool> mask(headers.size(),true);
    
    if (args.count("mask"))
      mask = get_mask(args["mask"].as<string>(),mask);

    //------------ Read Data ---------------//
    vector< vector<double> > data(headers.size());
    
    int skip = args["skip"].as<int>();

    int max = -1;
    if (args.count("max"))
      max = args["max"].as<int>();

    int line_number=0;
    while(getline(cin,line)) 
    {
      line_number++;

      if (line_number <= skip) continue;

      vector<double> v = split<double>(line,'\t');

      if (v.size() != headers.size())
	throw myexception()<<"Found "<<v.size()<<"/"<<headers.size()<<" values on line "<<line_number<<".";

      for(int i=0;i<v.size();i++)
	data[i].push_back(v[i]);

      if (max != -1 and data[0].size() >= max)
	break;
    }

    if (data[0].size() == 0)
      throw myexception()<<"No data line read in!";

    cerr<<"Read in "<<data[0].size()<<" lines.\n";

    for(int i=0;i<headers.size();i++) 
      if (mask[i]) {
	show_stats(args,headers[i],data[i]);
	cout<<endl;
      }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}


