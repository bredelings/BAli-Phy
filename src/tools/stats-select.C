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
    ("fields", value<vector<string> >(),"fields to keep")
    ;

  options_description visible("All options");
  visible.add_options()
    ("help", "Produce help message")
    ("no-header","Suppress the line of field names.")
    ;

  options_description all("All options");
  all.add(invisible).add(visible);

  // positional options
  positional_options_description p;
  p.add("fields", -1);

  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);    

  if (args.count("help")) {
    cerr<<"Usage: stats-select [OPTIONS] < data-file \n";
    cerr<<visible<<"\n";
    exit(0);
  }

  return args;
}

int main(int argc,char* argv[]) 
{ 
  try {
    //----------- Parse command line  -----------//
    variables_map args = parse_cmd_line(argc,argv);

    if (not args.count("fields")) 
      throw myexception()<<"No fields selected.";

    //------------ Parse column names ----------//
    string line;
    getline(std::cin,line);

    vector<string> headers = parse_header(line);

    //------------ Parse column mask ----------//
    vector<int> field_index;
    
    vector<string> fields = args["fields"].as<vector<string> >();
    
    for(int i=0;i<fields.size();i++) 
    {
      int loc = find_index(headers,fields[i]);
      if (loc == -1)
	throw myexception()<<"Can't find field '"<<fields[i]<<" in table.";
      field_index.push_back(loc);
    }
    
    //------------ Print  column names ----------//
    if (not args.count("no-header"))
      for(int i=0;i<field_index.size();i++) 
      {
	cout<<headers[field_index[i]];
	
	if (i == field_index.size()-1)
	  cout<<"\n";
	else
	  cout<<"\t";
      }

    //------------ Read Data ---------------//
    vector< vector<double> > data(headers.size());
    
    vector<string> v;

    int line_number=0;
    while(getline(cin,line)) 
    {
      line_number++;

      v = split(line,'\t');

      if (v.size() != headers.size())
	throw myexception()<<"Found "<<v.size()<<"/"<<headers.size()<<" values on line "<<line_number<<".";

      for(int i=0;i<field_index.size();i++) 
      {
	cout<<v[field_index[i]];

	if (i == field_index.size()-1)
	  cout<<"\n";
	else
	  cout<<"\t";
      }
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}


