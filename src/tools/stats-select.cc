/*
  Copyright (C) 2006,2008 Benjamin Redelings

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
#include "util/assert.hh"
#include "util/string/join.H"
#include <vector>
#include <valarray>
#include <cmath>

#include "statistics.H"
#include "stats-table.H"
#include "util/string/split.H"

#include <boost/program_options.hpp>
#include "util/owned-ptr.H"

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
	("help,h", "Produce help message")
	("no-header","Suppress the line of column names.")
	("select,s",value<vector<string> >()->composing(),"Select on key=value pairs")
	("remove,r","Remove selected columns, instead of keeping them.")
	("add,a",value<vector<string> >()->composing(),"Remove selected columns, instead of keeping them.")
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
	cout<<"Select columns from a Tracer-format data file.\n\n";
	cout<<"Usage: stats-select [OPTIONS] column-name [column-name ...] < data-file \n";
	cout<<visible<<"\n";
	exit(0);
    }

    return args;
}

template <typename T>
struct table_row_function
{
    virtual table_row_function* clone() const =0;

    virtual T operator()(const TableBase&, const vector<string>& row) const =0;

    string name;

    table_row_function(const string& s)
	:name(s)
	{}

    virtual ~table_row_function() {};
};

struct key_value_condition: public table_row_function<bool>
{
    int key_index;
    string value;

    key_value_condition* clone() const {return new key_value_condition(*this);}

    bool operator()(const TableBase&, const vector<string>& row) const;

    key_value_condition(const TableBase&, const string&);

    virtual ~key_value_condition() {};
};

bool key_value_condition::operator()(const TableBase&, const vector<string>& row) const
{
    return row[key_index] ==  value;
}

key_value_condition::key_value_condition(const TableBase& t, const string& condition)
    :table_row_function<bool>(condition)
{
    vector<string> parse = split(condition,'=');
    if (parse.size() != 2)
	throw myexception()<<"I can't understand the condition '"<<condition<<"' as a key=value pair.";
      
    key_index = t.find_column_index(parse[0]);

    value = parse[1];
}

int main(int argc,char* argv[]) 
{ 
    std::cout.precision(15);
    try {
	//----------- Parse command line  -----------//
	variables_map args = parse_cmd_line(argc,argv);

	//---------------- Read Data ----------------//
	vector<string> keep;
	if (args.count("columns"))
	    keep = args["columns"].as<vector<string> >();

	vector<string> remove;
	if (not args.count("columns") or args.count("remove"))
	    std::swap(remove,keep);

        // FIXME: hhis requires reading the whole table before writing anything.
        //        But we only want to read the whole table at once when we are computing statistics on it.
	TableReader table(std::cin,0,1,-1,remove,keep);


	//----------- Parse conditions ------------//
	vector< owned_ptr<table_row_function<bool> > > conditions;

	if (args.count("select"))
	{
	    vector<string> selections = args["select"].as<vector<string> >();

	    for(const auto& selection: selections)
		conditions.push_back(key_value_condition(table, selection));
	}
    
	//------------ Print  column names ----------//
	if (not args.count("no-header"))
	{
	    vector<string> headers;
	    for(int i=0;i<table.n_columns();i++)
		headers.push_back(table.names()[i]);

	    write_header(std::cout, headers);
	}

	//------------ Write new table ---------------//
	while(auto row = table.get_row())
	{
	    // skip rows that we are not selecting
	    bool ok = true;
	    for(int i=0; i<conditions.size() and ok; i++)
		if (not (*conditions[i])(table,*row))
		    ok = false;
	    if (not ok) continue;

            join(std::cout, *row,'\t')<<"\n";
	}
    }
    catch (std::exception& e) {
	std::cerr<<"stats-select: Error! "<<e.what()<<endl;
	exit(1);
    }

    return 0;
}


