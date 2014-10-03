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
#include "util.H"
#include "io.H"

template <typename T>
class Table
{
  /// The list of column names
  std::vector<std::string> names_;

  /// List of data for each column
  std::vector< std::vector<T> > data_;

  /// Load data from a file
  void load_file(std::istream&,int,int,int,const std::vector<std::string>&,const std::vector<std::string>&);
public:
  /// Access the column names
  const std::vector<std::string>& names() const {return names_;}

  /// Access the data for the i-th column
  const std::vector<T>& column(int i) const {return data_[i];}

  int find_column_index(const std::string& s) const;

  /// Add a row of data
  void add_row(const std::vector<T>& row);

  /// How many rows does the Table contain?
  int n_rows() const {return data_[0].size();}

  /// How many columns does the Table contain?
  int n_columns() const {return names_.size();}

  /// Remove burnin samples
  void chop_first_rows(int n);

  /// Load the Table from a file
  Table(std::istream&,int,int,int,const std::vector<std::string>&,const std::vector<std::string>&);

  /// Load the Table from a file by name
  Table(const std::string&,int,int,int,const std::vector<std::string>&,const std::vector<std::string>&);
};

template <typename T>
int Table<T>::find_column_index(const std::string& s) const
{
  return find_index(names_, s);
}

template <typename T>
void Table<T>::add_row(const std::vector<T>& row)
{
  assert(row.size() == n_columns());

  for(int i=0;i<row.size();i++)
    data_[i].push_back(row[i]);
}

template <typename T>
void Table<T>::chop_first_rows(int n)
{
  for(int i=0;i<data_.size();i++)
    data_[i].erase(data_[i].begin(), data_[i].begin()+n);
}

template <typename T>
void Table<T>::load_file(std::istream& file, int skip, int subsample, int max,
			 const std::vector<std::string>& ignore,const std::vector<std::string>& select)
{
  Table<std::string> t(file, skip, subsample, max, ignore, select);
  names_ = t.names();

  // Create space for the results
  data_.resize(t.n_columns());
  for(int i=0;i<t.n_columns();i++)
    data_[i].resize(t.n_rows());

  for(int row=0;row<t.n_rows();row++)
    for(int column=0;column<t.n_columns();column++)
	try{
	  data_[column][row] = boost::lexical_cast<T>(t.column(column)[row]);
	}
	catch (const std::exception& ee)
	{
	  for(int i=0;i<t.n_columns();i++)
	    data_[i].resize(row);
	  
	  std::cerr<<"On line "<<row+1<<", column "<<column+1<<": can't convert the following entry to a number:\n '"<<(t.column(column)[row])<<"'\n Skipping rest of file.\n  ";
	}
}

template<> void Table<std::string>::load_file(std::istream&,int,int,int,const std::vector<std::string>&,const std::vector<std::string>&);

template <typename T>
Table<T>::Table(std::istream& file, int skip, int subsample, int max, const std::vector<std::string>& ignore, const std::vector<std::string>& select)
{
  load_file(file,skip,subsample,max,ignore,select);
}

template <typename T>
Table<T>::Table(const std::string& filename, int skip, int subsample, int max, const std::vector<std::string>& ignore, const std::vector<std::string>& select)
{
  checked_ifstream file(filename,"statistics file");

  load_file(file,skip,subsample,max,ignore,select);
}

/// Load and store a table of doubles with named columns
typedef Table<double> stats_table;

std::vector<std::string> parse_header(const std::string&);

std::vector<std::string> read_header(std::istream&);

void write_header(std::ostream&,const std::vector<std::string>&);

#endif
