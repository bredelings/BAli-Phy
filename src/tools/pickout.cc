/*
   Copyright (C) 2004-2008 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

#include <iostream>
#include <string>
#include <vector>
#include <cassert>
#include "myexception.H"
#include "util.H"
#include "io.H"

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

string get_multivalue(const string& line1,int pos1,std::istream& file) 
{
  string result = line1.substr(pos1);
  string line;
  while (portable_getline(file,line) and line.size()) {
    result += "\n";
    result += line;
  }
  return result;
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
    ("no-header,n","Suppress the line of field names.")
    ("large","The last value goes to the end of the line.")
    ("multi-line","The last continues until a blank line.")
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
    cout<<"Usage: pickout [OPTIONS] field1 [field2 ... ] < data-file \n";
    cout<<visible<<"\n";
    exit(0);
  }

  if (not args.count("fields")) {
    throw myexception()<<"No fields specified!";
    exit(1);
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
    while(portable_getline(cin,line)) 
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
      
      for(int i=0;i<patterns.size()-1;i++)
	words[i] = getvalue(line,matches[i] + patterns[i].size());
      if (args.count("large"))
	words.back() = get_largevalue(line,matches.back() + patterns.back().size());
      else if (args.count("multi-line"))
	words.back() = get_multivalue(line,matches.back() + patterns.back().size(),cin);
      else
	words.back() = getvalue(line,matches.back() + patterns.back().size());

      cout<<join(words,'\t')<<"\n";
    }
  }
  catch (std::exception& e) {
    cerr<<"pickout: Error! "<<e.what()<<endl;
    exit(1);
  }

}
