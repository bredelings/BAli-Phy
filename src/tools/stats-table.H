/*
  Copyright (C) 2006,2009 Benjamin Redelings

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

#ifndef STATS_TABLE_H
#define STATS_TABLE_H

#include <vector>
#include <string>
#include <iostream>
#include <boost/lexical_cast.hpp>
#include "util/mapping.H"
#include "util/io.H"
#include "util/json.hh"

class TableBase
{
public:
    virtual const std::vector<std::string>& names() const = 0;

    std::optional<int> maybe_find_column_index(const std::string& s) const;

    int find_column_index(const std::string& s) const;

    int n_columns() const;

    virtual ~TableBase() = default;
};

class TableReader: virtual public TableBase
{
    std::vector<std::string> names_;

    std::istream& file;

    int skip;

    int subsample;

    int last;

    std::vector<int> indices;

    std::optional<std::string> saved_line;

    bool is_json;

    int line_number = 0;

    int n_lines = 0;

    std::optional<std::string> getline_();

    std::optional<std::string> getline();

public:

    /// Access the column names
    const std::vector<std::string>& names() const {return names_;}

    std::optional<std::vector<std::string>> get_row();

    TableReader(std::istream&, int, int, int, const std::vector<std::string>&,const std::vector<std::string>&);
};


template <typename T>
class Table: virtual public TableBase
{
    std::vector<std::string> names_;

    /// List of data for each column
    std::vector< std::vector<T> > data_;

    /// Load data from a file
    void load_file(std::istream&,int,int,int,const std::vector<std::string>&,const std::vector<std::string>&);
public:
    /// Access the column names
    const std::vector<std::string>& names() const {return names_;}

    /// Access the data for the i-th column
    std::vector<T>& column(int i)             { assert(0 <= i and i < data_.size()); return data_[i];}
    /// Access the data for the i-th column
    const std::vector<T>& column(int i) const { assert(0 <= i and i < data_.size()); return data_[i];}

    /// Add a row of data
    void add_row(const std::vector<T>& row);

    /// How many rows does the Table contain?
    int n_rows() const {return column(0).size();}

    /// Remove burnin samples
    void chop_first_rows(int n);

    /// Load the Table from a file
    Table(std::istream&,int,int,int,const std::vector<std::string>&,const std::vector<std::string>&);
};

template <typename T>
void Table<T>::add_row(const std::vector<T>& row)
{
    assert(row.size() == n_columns());

    for(int i=0;i<row.size();i++)
	column(i).push_back(row[i]);
}

template <typename T>
void Table<T>::chop_first_rows(int n)
{
    for(int i=0;i<data_.size();i++)
	column(i).erase(column(i).begin(), column(i).begin()+n);
}

template <typename T>
void Table<T>::load_file(std::istream& file, int skip, int subsample, int last,
			 const std::vector<std::string>& ignore,const std::vector<std::string>& select)
{
    Table<std::string> t(file, skip, subsample, last, ignore, select);
    names_ = t.names();

    // Create space for the results
    data_.resize(t.n_columns());
    for(int i=0;i<t.n_columns();i++)
	data_[i].resize(t.n_rows());

    for(int row=0;row<t.n_rows();row++)
	for(int col=0;col<t.n_columns();col++)
	    try{
		column(col)[row] = boost::lexical_cast<T>(t.column(col)[row]);
	    }
	    catch (const std::exception& ee)
	    {
		for(int i=0;i<t.n_columns();i++)
		    column(i).resize(row);
	  
		std::cerr<<"On line "<<row+1<<", column "<<col+1<<": can't convert the following entry to a number:\n '"<<(t.column(col)[row])<<"'\n Skipping rest of file.\n  ";
	    }
}

template<> void Table<std::string>::load_file(std::istream&,int,int,int,const std::vector<std::string>&,const std::vector<std::string>&);

template <typename T>
Table<T>::Table(std::istream& file, int skip, int subsample, int last, const std::vector<std::string>& ignore, const std::vector<std::string>& select)
{
    load_file(file,skip,subsample,last,ignore,select);
}

template <>
Table<double>::Table(std::istream& file, int skip, int subsample, int last, const std::vector<std::string>& ignore, const std::vector<std::string>& select);

/// Load and store a table of doubles with named columns
typedef Table<double> stats_table;

std::vector<std::string> parse_header(const std::string&);

std::vector<std::string> read_header(std::istream&);

void write_header(std::ostream&,const std::vector<std::string>&);

std::vector<std::string> parameter_names(const json::object& j);

std::vector<json::value> parameter_values(const json::object& j);

bool has_children(const std::string& s);

json::object unnest_json(const json::object& j);
json::object unnest_json(const json::object&& j);

#endif
