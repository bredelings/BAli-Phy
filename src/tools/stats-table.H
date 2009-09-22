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
  void load_file(std::istream&,int,int,int);

public:
  /// Access the column names
  const std::vector<std::string>& names() const {return names_;}

  /// Access the data for the i-th column
  const std::vector<double>& column(int i) const {return data_[i];}

  /// Add a row of data
  void add_row(const std::vector<double>& row);

  /// How many rows does the table contain?
  int n_rows() const {return data_[0].size();}

  /// How many columns does the table contain?
  int n_columns() const {return names_.size();}

  /// Remove burnin samples
  void chop_first_rows(int n);

  /// Load the table from a file
  stats_table(std::istream&,int,int,int);

  /// Load the table from a file by name
  stats_table(const std::string&,int,int,int);
};

std::vector<std::string> parse_header(const std::string&);

std::vector<std::string> read_header(std::istream&);

void write_header(std::ostream&,const std::vector<std::string>&);

#endif
