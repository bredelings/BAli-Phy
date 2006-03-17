#include <iostream>
#include <string>
#include <vector>
#include <cassert>
#include "myexception.H"
#include "util.H"

#include <boost/program_options.hpp>

using namespace std;

namespace po = boost::program_options;
using po::variables_map;

string getvalue(const string& line,int pos1) {
  int pos2 = pos1;
  int depth = 0;

  while(pos2<line.size() and not (line[pos2] == ' ' and depth == 0))
  {
    if (line[pos2] == '(')
      depth++;
    if (line[pos2] == ')')
      depth--;
    pos2++;
  }

  return line.substr(pos1,pos2-pos1);
}

string get_largevalue(const string& line,int pos1) {
  return line.substr(pos1);
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description invisible("Invisible options");
  invisible.add_options()
    ("fields", value<vector<string> >(),"Fields to select")
    ;

  options_description visible("All options");
  visible.add_options()
    ("help", "Produce help message.")
    ("no-header","Suppress the line of field names.")
    ("large","Read one large value to the end of the line.")
    ;

  // positional options
  positional_options_description p;
  p.add("fields", -1);

  options_description all("All options");
  all.add(invisible).add(visible);

  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);    

  if (args.count("help")) {
    cerr<<"Usage: pickout [OPTIONS] field1 [field2 ... ] < data-file \n";
    cerr<<visible<<"\n";
    exit(0);
  }

  return args;
}

int main(int argc,char* argv[]) 
{ 
  try{
    //----------- Parse command line  -----------//
    variables_map args = parse_cmd_line(argc,argv);

    vector<string> patterns = args["fields"].as<vector<string> >();

    if (not patterns.size())
      throw myexception()<<"No patterns specified.";

    // print headers
    if (not args.count("no-header"))
      cout<<join(patterns,'\t')<<endl;

    // modify patterns
    for(int i=0;i<patterns.size();i++)
      patterns[i] += " = ";

    string line;
    vector<int> matches(patterns.size());  
    vector<string> words(patterns.size());
    while(getline(cin,line)) 
    {
      // Locate each occurrence in the line
      bool linematches=true;
      for(int i=0;i<patterns.size();i++) {
	matches[i] = line.find(patterns[i]);
	//      cout<<"   "<<patterns[i]<<": "<<matches[i]<<endl;
	if (matches[i] == -1) {
	  linematches=false;
	  break;
	}
      }
      if (not linematches) continue;
      
      if (args.count("large"))
	for(int i=0;i<patterns.size();i++)
	  words[i] = get_largevalue(line,matches[i] + patterns[i].size());
      else
	for(int i=0;i<patterns.size();i++)
	  words[i] = getvalue(line,matches[i] + patterns[i].size());

      cout<<join(words,'\t')<<"\n";
    }
  }
  catch (std::exception& e) {
    cerr<<"Error: "<<e.what()<<endl;
    exit(1);
  }

}
