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

#include <optional>
#include <iostream>
#include <string>
#include <vector>
#include "util/assert.hh"
#include "util/myexception.hh"
#include "util/string/join.hh"
#include "util/io.hh"

#include <CLI/CLI.hpp>

using namespace std;

using std::optional;

string get_value_quoted(const string& line, int pos1)
{
    // FIXME: handle quotes here.  Should these be \" or ""?
    assert(pos1 < line.size() and line[pos1] == '"');

    for(int pos2 = pos1+1; pos2 < line.size(); pos2++)
        if (line[pos2] == '"')
            return line.substr(pos1,pos2-pos1+1);
    throw myexception()<<"Unterminated quoted field in line:\n  |"<<line<<"\n";
}

string getvalue(const string& line,int pos1)
{
    if (pos1 >= line.size()) return "";

    if (line[pos1] == '"') return get_value_quoted(line,pos1);

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

int main(int argc,char* argv[]) 
{ 
    try{
	//----------- Parse command line  -----------//
        CLI::App app{"Generate a table from key = value lines on stdin.", "pickout"};
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        vector<string> patterns;
        bool no_header = false, large = false, multi_line = false;
        app.add_option("FIELD", patterns, "Fields to select, in output order")->required()->type_name("");
        app.add_flag("-n,--no-header", no_header, "Suppress the line of field names");
        app.add_flag("--large", large, "Take the last requested value through the end of its line");
        app.add_flag("--multi-line", multi_line, "Continue the last requested value until an empty line");
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            // Let CLI11 print help or diagnostics, retaining the tool's 0/1 exit statuses.
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }

	// print headers
	if (not no_header)
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
	    if (large)
		words.back() = get_largevalue(line,matches.back() + patterns.back().size());
	    else if (multi_line)
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
