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
#include <boost/lexical_cast.hpp>

using boost::lexical_cast;
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

bool is_numeric_range(const string& s, unsigned n, unsigned& start, unsigned& end)
{
  vector<string> bounds = split(s,':');

  if (bounds.size() != 2) return false;

  start = 1;
  end = n;

  if (bounds[0].size())
  {
    if (not can_be_converted_to<unsigned>(bounds[0],start)) return false;
  }

  if (bounds[1].size())
  {
    if (not can_be_converted_to<unsigned>(bounds[1],end)) return true;
  }

  start = max(1U,start);
  end = min(n,end);
}

dynamic_bitset<>
get_mask(const vector<string>& strings,const vector<string>& names)
{
  dynamic_bitset<> mask(names.size());

  for(int i=0;i<strings.size();i++) 
  {
    const string& s = strings[i];

    int index = find_index(names,s);

    if (index != -1)
    {
      // This is a field name
      mask[index] = true;
      continue;
    }

    unsigned start = -1;
    unsigned end = -1;
    if (is_numeric_range(s, mask.size(), start, end))
    {
      // This is a numeric range of fields
      for(int i=0;i<mask.size();i++)
	if (start <=i+1 and i+1 <= end)
	  mask[i]=true;
      continue;
    }

    throw myexception()<<"No field named '"<<s<<"', and its not a numeric range either.";
  }

  return mask;
}

vector<int> get_indices_from_mask(const dynamic_bitset<>& mask)
{
  vector<int> indices;
  for(int i=0;i<mask.size();i++)
    if (mask[i])
      indices.push_back(i);
  return indices;
}

bool read_entries(const string& line, const vector<int>& indices, char delim, vector<string>& entries)
{
  int i=0; // position in line
  int j=0; // which field
  int k=0; // position in 'indices'
  while (k<indices.size())
  {
    // Locate the character after the end of the current field
    int i2 = line.find(delim,i+1);
    if (i2 == -1)
      i2 = line.size();

    // If the current field is the next field we want, convert it to double.
    if (j == indices[k])
      entries[k++] = line.substr(i,i2-i);

    if (i2 == line.size() and k < indices.size())
      throw myexception()<<"Only read "<<k<<" entries!";

    // The next field starts after the delimiter between fields.
    i = i2 + 1;
    j++;
  }
  return true;
}


template<> 
void Table<string>::load_file(istream& file,int skip,int subsample, int max,
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

  vector<int> indices = get_indices_from_mask(mask);

  names_ = apply_indices(names_, indices);

  data_.resize(names_.size());

  // Read in data
  int n_lines=0;
  string line;
  vector<string> v(names_.size());
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
	read_entries(line,indices,'\t',v);
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
