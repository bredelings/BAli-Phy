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
    ("columns", value<vector<string> >(),"columns to keep")
    ;

  options_description visible("All options");
  visible.add_options()
    ("help", "Produce help message")
    ("no-header","Suppress the line of column names.")
    ("remove,r","Remove selected columns, instead of keeping them.")
    ;

  options_description all("All options");
  all.add(invisible).add(visible);

  // positional options
  positional_options_description p;
  p.add("columns", -1);

  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: stats-select [OPTIONS] column-name [column-name ...] < data-file \n";
    cout<<"Select columns from a Tracer-format data file.\n\n";
    cout<<visible<<"\n";
    exit(0);
  }

  return args;
}

int main(int argc,char* argv[]) 
{ 
  try {
    //----------- Parse command line  -----------//
    variables_map args = parse_cmd_line(argc,argv);

    //------------ Parse column names ----------//
    vector<string> headers = read_header(std::cin);

    //------------ Parse column mask ----------//
    vector<int> column_index;
    if (not args.count("columns"))
      column_index = iota<int>(headers.size());
    else 
    {
      vector<string> columns = args["columns"].as<vector<string> >();
    
      for(int i=0;i<columns.size();i++)
      {
	int loc = find_index(headers,columns[i]);
	if (loc == -1)
	  throw myexception()<<"Can't find column '"<<columns[i]<<" in table.";
	column_index.push_back(loc);
      }
    }
    
    //------------ Invert column mask -----------//

    if (args.count("remove")) {
      vector<int> others;
      for(int i=0;i<headers.size();i++)
	if (not includes(column_index,i))
	  others.push_back(i);
      column_index = others;
    }

    //------------ Print  column names ----------//
    if (not args.count("no-header"))
      for(int i=0;i<column_index.size();i++) 
      {
	cout<<headers[column_index[i]];
	
	if (i == column_index.size()-1)
	  cout<<"\n";
	else
	  cout<<"\t";
      }

    //------------ Read Data ---------------//
    vector< vector<double> > data(headers.size());
    
    vector<string> v;

    int line_number=0;
    string line;
    while(getline(cin,line)) 
    {
      line_number++;

      v = split(line,'\t');

      if (v.size() != headers.size())
	throw myexception()<<"Found "<<v.size()<<"/"<<headers.size()<<" values on line "<<line_number<<".";

      for(int i=0;i<column_index.size();i++) 
      {
	cout<<v[column_index[i]];

	if (i == column_index.size()-1)
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


