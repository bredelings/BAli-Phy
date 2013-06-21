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

#include <fstream>

#include "stats-table.H"
#include "util.H"
#include "myexception.H"
#include "io.H"

#include <boost/dynamic_bitset.hpp>

using namespace std;

using boost::dynamic_bitset;

vector<string> parse_header(const string& line)
{
  vector<string> headers = split(line,'\t');

  if (headers.size() == 0)
    throw myexception()<<"No column names provided!";

  for(int i=0;i<headers.size();i++)
    if (headers[i].size() == 0)
      throw myexception()<<"The "<<i<<"th column name is blank!";

  return headers;
}


vector<string> read_header(std::istream& file)
{
  string line;
  while (file) 
  {
    portable_getline(file,line);

    // Skip comments lines - but what is a comment line?
    if (line.size() >= 2 and line[0] == '#' and line[1] == ' ')
      continue;
    else
      break;
  }

  return parse_header(line);
}

void write_header(std::ostream& o, const vector<string>& headers)
{
  for(int i=0;i<headers.size();i++) 
  {
    cout<<headers[i];
      
    if (i == headers.size()-1)
      o<<"\n";
    else
      o<<"\t";
  }
}

int stats_table::find_column_index(const string& s) const
{
  return find_index(names_, s);
}

void stats_table::add_row(const vector<double>& row)
{
  assert(row.size() == n_columns());

  for(int i=0;i<row.size();i++)
    data_[i].push_back(row[i]);
}

dynamic_bitset<>
get_mask(const vector<string>& strings,const vector<string>& names)
{
  dynamic_bitset<> mask(names.size());

  for(int i=0;i<strings.size();i++) 
  {
    const string& s = strings[i];

    // This is a field name
    if (s.find(':') == -1) 
    {
      int index = find_index(names,s);
      if (index == -1)
	throw myexception()<<"No field named '"<<s<<"'";
      mask[index] = true;
    }
    // This is a numeric range of fields
    else {
      vector<string> bounds = split(s,':');
      if (bounds.size() != 2) 
	throw myexception()<<"Can't understand the column range '"<<s<<"'.";

      unsigned start = 1;
      unsigned end = mask.size();
      if (bounds[0].size())
	start = convertTo<unsigned>(bounds[0]);
      if (bounds[1].size())
	start = convertTo<unsigned>(bounds[1]);
      start = max(1U,start);
      end = min((unsigned)(mask.size()),end);

      for(int i=0;i<mask.size();i++)
	if (start <=i+1 and i+1 <= end)
	  mask[i]=true;
    }
  }

  return mask;
}

vector<int> get_mapping_from_mask(const dynamic_bitset<>& mask)
{
  vector<int> mapping;
  for(int i=0;i<mask.size();i++)
    if (mask[i])
      mapping.push_back(i);
  return mapping;
}

//FIXME - can we use scan_lines?
//        This would add sub-sampling automatically.

void stats_table::load_file(istream& file,int skip,int subsample, int max,
			    const vector<string>& ignore, const vector<string>& select)
{
  // Read in headers from file
  names_ = read_header(file);

  // Construct mask of names to remove
  dynamic_bitset<> mask(names_.size());
  mask.flip();
  if (ignore.size())
    mask &= ~get_mask(ignore, names_);

  if (select.size())
    mask &=  get_mask(select, names_);

  vector<int> mapping = get_mapping_from_mask(mask);

  names_ = apply_indices(names_, mapping);

  data_.resize(names_.size());

  // Read in data
  int n_lines=0;
  string line;
  vector<double> v(names_.size());
  for(int line_number=0;portable_getline(file,line);line_number++) 
  {
    // don't start if we haven't skipped enough trees
    if (line_number < skip) continue;

    // skip trees unless they are a multiple of 'subsample'
    if ((line_number-skip) % subsample != 0) continue;

    // quit if we've read in 'max' trees
    if (max >= 0 and n_lines == max) break;

    // This is the 'op'
    {
      // should this be protected by a try { } catch(...) {} block?
      try
      {
	vector<string> entries = split(line,'\t');
	for(int i=0;i<names_.size();i++)
	  v[i] = convertTo<double>(entries[mapping[i]]);
      }
      catch (...)
      {
	// +2 = +1 (start indexing at 1) +1 (count the header line)
	std::cerr<<"Error: bad data on line "<<line_number+2<<", giving up.\n Line = '"<<line<<"'";
	break;
      }

      if (v.size() != n_columns())
	throw myexception()<<"Found "<<v.size()<<"/"<<n_columns()<<" values on line "<<line_number<<".";
    
      add_row(v);
    }

    n_lines++;
  }
}

void remove_first_elements(vector<double>& v,int n)
{
  if (n >= v.size()) {
    v.clear();
    return;
  }

  for(int i=0;i<v.size()-n;i++)
    v[i] = v[i+n];

  v.resize(v.size()-n);
}

void stats_table::chop_first_rows(int n)
{
  for(int i=0;i<data_.size();i++)
    remove_first_elements(data_[i],n);
}

stats_table::stats_table(istream& file, int skip, int subsample, int max, const vector<string>& ignore, const vector<string>& select)
{
  load_file(file,skip,subsample,max,ignore,select);
  if (log_verbose) cerr<<"STDIN: Read in "<<n_rows()<<" lines.\n";
}

stats_table::stats_table(const string& filename, int skip, int subsample, int max, const vector<string>& ignore, const vector<string>& select)
{
  checked_ifstream file(filename,"statistics file");

  load_file(file,skip,subsample,max,ignore,select);
  if (log_verbose) cerr<<filename<<": Read in "<<n_rows()<<" lines.\n";
}
