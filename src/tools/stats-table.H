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

/// Load and store a table of doubles with named columns
class stats_table
{
  /// The list of column names
  std::vector<std::string> names_;

  /// List of data for each column
  std::vector< std::vector<double> > data_;

  /// Load data from a file
  void load_file(std::istream&,int,int,int,const std::vector<std::string>&,const std::vector<std::string>&);

public:
  /// Access the column names
  const std::vector<std::string>& names() const {return names_;}

  /// Access the data for the i-th column
  const std::vector<double>& column(int i) const {return data_[i];}

  int find_column_index(const std::string& s) const;

  /// Add a row of data
  void add_row(const std::vector<double>& row);

  /// How many rows does the table contain?
  int n_rows() const {return data_[0].size();}

  /// How many columns does the table contain?
  int n_columns() const {return names_.size();}

  /// Remove burnin samples
  void chop_first_rows(int n);

  /// Load the table from a file
  stats_table(std::istream&,int,int,int,const std::vector<std::string>&,const std::vector<std::string>&);

  /// Load the table from a file by name
  stats_table(const std::string&,int,int,int,const std::vector<std::string>&,const std::vector<std::string>&);
};

std::vector<std::string> parse_header(const std::string&);

std::vector<std::string> read_header(std::istream&);

void write_header(std::ostream&,const std::vector<std::string>&);

#endif
