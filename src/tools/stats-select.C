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
#include <cassert>
#include <vector>
#include <valarray>
#include <cmath>

#include "util.H"
#include "statistics.H"
#include "stats-table.H"

#include <boost/program_options.hpp>
#include "owned-ptr.H"

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
    ("select,s",value<vector<string> >()->composing(),"Select on key=value pairs")
    ("remove,r","Remove selected columns, instead of keeping them.")
    ("add,a","Remove selected columns, instead of keeping them.")
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

template <typename T>
struct table_row_function
{
  virtual table_row_function* clone() const =0;

  virtual T operator()(const stats_table&, int row) const =0;

  string name;

  table_row_function(const string& s)
    :name(s)
  {}
};

struct select_column_function: public table_row_function<double>
{
  int index;

  select_column_function* clone() const {return new select_column_function(*this);}

  double operator()(const stats_table& t, int row) const
  {
    return t.column(index)[row];
  }

  select_column_function(const stats_table& t, const string& name)
    :table_row_function<double>(name), index(t.find_column_index(name))
  {
    if (index == -1)
      throw myexception()<<"Can't find column '"<<name<<" in table.";
  }
};

struct sum_of_fields: public table_row_function<double>
{
  vector<int> indices;

  sum_of_fields* clone() const {return new sum_of_fields(*this);}

  double operator()(const stats_table&,int row) const;

  sum_of_fields(const stats_table&, const string&);
};

double sum_of_fields::operator()(const stats_table& t,int row) const
{
  double sum = 0;
  for(int i=0;i<indices.size();i++)
    sum += t.column(indices[i])[row];
  return sum;
}

sum_of_fields::sum_of_fields(const stats_table& t, const string& name)
  :table_row_function<double>(name)
{
  vector<string> names = split(name,'+');

  for(int i=0; i < names.size(); i++)
  {
    int index = t.find_column_index(names[i]);
    
    if (index == -1)
      throw myexception()<<"Can't find column '"<<name<<" in table.";

    indices.push_back(index);
  }
}

struct key_value_condition: public table_row_function<bool>
{
  int key_index;
  double value;

  key_value_condition* clone() const {return new key_value_condition(*this);}

  bool operator()(const stats_table&,int row) const;

  key_value_condition(const stats_table&, const string&);
};

bool key_value_condition::operator()(const stats_table& t,int row) const
{
  return (t.column(key_index)[row] == value);
}

key_value_condition::key_value_condition(const stats_table& t, const string& condition)
  :table_row_function<bool>(condition)
{
  vector<string> parse = split(condition,'=');
  if (parse.size() != 2)
    throw myexception()<<"I can't understand the condition '"<<condition<<"' as a key=value pair.";
      
  key_index = t.find_column_index(parse[0]);
  if (key_index == -1)
    throw myexception()<<"Can't find column '"<<parse[0]<<"' in table.";

  value = convertTo<double>(parse[1]);
}

int main(int argc,char* argv[]) 
{ 
  try {
    //----------- Parse command line  -----------//
    variables_map args = parse_cmd_line(argc,argv);

    //---------------- Read Data ----------------//
    stats_table table(std::cin,0,1,-1,{},{});

    //------------ Parse column names ----------//
    vector< owned_ptr<table_row_function<double> > > column_functions;

    if (not args.count("columns") or args.count("remove"))
    {
      vector<string> remove;
      if (args.count("remove"))
	remove = args["columns"].as<vector<string> >();

      for(int i=0;i<table.n_columns();i++)
      {
	const string& name = table.names()[i];
	if (not includes(remove,name))
	  column_functions.push_back(select_column_function(table, name));
      }
    }
    else 
    {
      vector<string> column_names = args["columns"].as<vector<string> >();

      if (args.count("add"))
	for(int i=0;i<table.n_columns();i++)
	{
	  const string& name = table.names()[i];
	  column_functions.push_back(select_column_function(table, name));
	}

      for(int i=0;i<column_names.size();i++)
	column_functions.push_back(sum_of_fields(table,column_names[i]));
    }

    //----------- Parse conditions ------------//
    vector< owned_ptr<table_row_function<bool> > > conditions;

    if (args.count("select"))
    {
      vector<string> selections = args["select"].as<vector<string> >();

      for(int i=0;i<selections.size();i++) 
	conditions.push_back(key_value_condition(table, selections[i]));
    }
    
    //------------ Print  column names ----------//
    if (not args.count("no-header"))
    {
      vector<string> headers;
      for(int i=0;i<column_functions.size();i++)
	headers.push_back(column_functions[i]->name);

      write_header(std::cout, headers);
    }

    //------------ Write new table ---------------//
    for(int r=0; r<table.n_rows(); r++)
    {
      // skip rows that we are not selecting
      bool ok = true;
      for(int i=0; i<conditions.size() and ok; i++)
	if (not (*conditions[i])(table,r))
	  ok = false;
      if (not ok) continue;

      vector<double> values;
      for(int i=0; i<column_functions.size(); i++)
	values.push_back((*column_functions[i])(table,r));

      std::cout<<join(values,"\t")<<"\n";
    }
  }
  catch (std::exception& e) {
    std::cerr<<"stats-select: Error! "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}


