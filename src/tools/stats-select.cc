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
#include "util/string/join.hh"
#include <vector>
#include <valarray>
#include <cmath>

#include "statistics.hh"
#include "stats-table.hh"
#include "util/string/split.hh"

#include <CLI/CLI.hpp>
#include "util/owned-ptr.hh"

using namespace std;

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
	CLI::App app{"Select columns and rows from a statistics table on stdin.", "stats-select"};
	app.usage("Usage: stats-select [OPTIONS] [COLUMN ...] < data-file");
	app.get_formatter()->long_option_alignment_ratio(0.2f);
	vector<string> columns, selections;
	bool no_header = false, remove_columns = false;
	app.add_flag("--no-header", no_header, "Suppress the line of column names");
	app.add_option("-s,--select", selections, "Keep rows matching KEY=VALUE")
	    ->type_name("KEY=VALUE")->type_size(1)->expected(1)->allow_extra_args(false)->take_all();
	app.add_flag("-r,--remove", remove_columns, "Remove listed columns instead of keeping them");
	app.add_option("COLUMN", columns, "Input column names or numeric ranges")->type_name("");
	try
	{
	    app.parse(argc, argv);
	}
	catch (const CLI::ParseError& error)
	{
	    app.exit(error);
	    return error.get_exit_code() == 0 ? 0 : 1;
	}

	//---------------- Read Data ----------------//
	vector<string> keep = columns;

	vector<string> remove;
	if (columns.empty() or remove_columns)
	    std::swap(remove,keep);

	// Evaluate row conditions against the input table, including columns omitted
	// from the output.
	TableReader table(std::cin,0,1,-1,{},{});
	auto output_indices = get_indices_for_names(table.names(), remove, keep);


	//----------- Parse conditions ------------//
	vector< owned_ptr<table_row_function<bool> > > conditions;

	for(const auto& selection: selections)
	    conditions.push_back(key_value_condition(table, selection));
    
	//------------ Print  column names ----------//
	if (not no_header)
	{
	    write_header(std::cout, apply_indices(table.names(), output_indices));
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

	    join(std::cout, apply_indices(*row, output_indices),'\t')<<"\n";
	}
    }
    catch (std::exception& e) {
	std::cerr<<"stats-select: Error! "<<e.what()<<endl;
	exit(1);
    }

    return 0;
}


