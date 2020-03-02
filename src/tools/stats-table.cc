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
#include "util/myexception.H"
#include "util/io.H"
#include "util/string/split.H"
#include "util/string/convert.H"
#include "models/path.H"

#include <boost/dynamic_bitset.hpp>

using std::optional;

using std::string;
using std::vector;
using std::pair;

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
	std::cout<<headers[i];
      
	if (i == headers.size()-1)
	    o<<"\n";
	else
	    o<<"\t";
    }
}

optional<pair<unsigned,unsigned>> is_numeric_range(const string& s, unsigned n)
{
    vector<string> bounds = split(s,':');

    if (bounds.size() != 2) return {};

    unsigned start = 1;
    unsigned end = n;

    if (bounds[0].size())
    {
	if (auto s = can_be_converted_to<unsigned>(bounds[0]))
	    start = *s;
	else
	    return {};
    }

    if (bounds[1].size())
    {
	if (auto e = can_be_converted_to<unsigned>(bounds[1]))
	    end = *e;
	else
	    return {};
    }

    start = std::max(1U,start);
    end = std::min(n,end);
    return pair<unsigned,unsigned>{start,end};
}

dynamic_bitset<>
get_mask(const vector<string>& strings,const vector<string>& names)
{
    dynamic_bitset<> mask(names.size());

    for(auto& s: strings)
    {
	if (auto index = find_index(names, s))
	{
	    // This is a field name
	    mask[*index] = true;
	    continue;
	}
	else if (auto range = is_numeric_range(s, mask.size()))
	{
	    // This is a numeric range of fields
	    for(int i=0;i<mask.size();i++)
		if (range->first <=i+1 and i+1 <= range->second)
		    mask[i]=true;
	    continue;
	}
	else
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


vector<int> get_indices_for_names(const vector<string>& names, const vector<string>& ignore, const vector<string>& select)
{
    // Construct mask of names to remove
    dynamic_bitset<> mask(names.size());
    mask.flip();
    if (ignore.size())
	mask &= ~get_mask(ignore, names);

    if (select.size())
	mask &=  get_mask(select, names);

    return get_indices_from_mask(mask);
}

std::optional<int> TableBase::maybe_find_column_index(const std::string& s) const
{
    return find_index(names(), s);
}

int TableBase::find_column_index(const std::string& s) const
{
    if (auto index = maybe_find_column_index(s))
	return *index;
    else
	throw myexception()<<"Can't find column '"<<s<<"' in table!";
}

int TableBase::n_columns() const
{
    return names().size();
}

// Return a line if (a) we don't already have a saved one and (b) the operation succeeds.
optional<string> TableReader::getline_()
{
    if (saved_line)
    {
        optional<string> line;
        std::swap(saved_line,line);
        return line;
    }

    string line;
    portable_getline(file, line);
    if (file)
        return line;
    else
        return {};
}

optional<string> TableReader::getline()
{
    while(auto line = getline_())
    {
        int cur_line = line_number;

        line_number++;

        // don't start if we haven't skipped enough trees
        if (skip and cur_line < skip) continue;

        // skip trees unless they are a multiple of 'subsample'
        if ((cur_line-skip) % subsample != 0) continue;

        // quit if we've read in 'last' line
        if (last > 0 and cur_line >= last) return {};

        n_lines++;

        return line;
    }

    return {};
}

optional<vector<string>> TableReader::get_row()
{
    if (auto line = getline())
    {
        vector<string> v(indices.size());

        try
        {
            if (is_json)
            {
                json j = json::parse(*line);
                auto values = parameter_values_children(j);
                for(int i=0;i<indices.size();i++)
                    v[i] = values[indices[i]].dump();
            }
            else
                read_entries(*line, indices, '\t', v);
        }
        catch (...)
        {
            // +2 = +1 (start indexing at 1) +1 (count the header line)
            std::cerr<<"Error: bad data on line "<<line_number+2<<", giving up.\n Line = '"<<*line<<"'";
            return {};
        }

        if (v.size() != n_columns())
            throw myexception()<<"Found "<<v.size()<<"/"<<n_columns()<<" values on line "<<line_number<<".";

        return v;
    }
    else
        return {};
}

TableReader::TableReader(std::istream& f, int sk, int sub, int lst, const vector<string>& ignore, const vector<string>& select)
    :file(f), skip(sk), subsample(sub), last(lst)
{
    // 1. Check if this is a JSON file.
    is_json = (file.peek() == '{');

    // 2. Read in headers from file
    if (is_json)
    {
        string line;
        portable_getline(file,line);
        json header = json::parse(line);
        if (not header.count("version"))
            throw myexception()<<"JSON log file does not have a valid header line: no \"version\" field.";

        portable_getline(file,line);
        names_ = parameter_names_children(json::parse(line));
        saved_line = line;
    }
    else
        names_ = read_header(file);

    // 3. Select fields and determine header names.
    for(auto& name: names_)
        name = translate_structures(name);
    names_ = short_parameter_names(names_);
    indices = get_indices_for_names(names_, ignore, select);
    names_ = apply_indices(names_, indices);
}




template<> 
void Table<string>::load_file(std::istream& file,int skip,int subsample, int last,
			      const vector<string>& ignore, const vector<string>& select)
{
    TableReader reader(file, skip, subsample, last, ignore, select);
    names_ = reader.names();
    data_.resize(names_.size());

    while(auto row = reader.get_row())
        add_row(*row);
}

// We should be able to collapse this to some kind of visitor pattern!

vector<string> parameter_names(const json& children);

vector<string> parameter_names_children(const json& children)
{
    vector<string> all_names;
    for(auto& [key, value]: children.items())
    {
	vector<string> names = parameter_names(value);
	for(auto& name: names)
	    all_names.push_back(key + "/" + name);

	if (value.find("value") != value.end())
	{
	    json v = *value.find("value");
	    if (v.is_array())
	    {
		// FIXME we are not looking looking into the value for "value" / "children"
		for(int i=0;i<v.size();i++)
		    all_names.push_back(key+"["+std::to_string(i+1)+"]");
	    }
	    else if (v.is_object())
	    {
		// FIXME we are not looking looking into value2 for "value" / "children"
		for(auto& [key2,value2]: v.items())
		    all_names.push_back(key+"["+key2+"]");
	    }
	    else
		all_names.push_back(key);
	}
    }
    return all_names;
}

vector<string> parameter_names(const json& j)
{
    auto children = j.find("children");
    if (children == j.end())
	return {};
    else
	return parameter_names_children(*children);
}

vector<json> parameter_values(const json& children);

vector<json> parameter_values_children(const json& children)
{
    vector<json> all_values;
    for(auto& [key, value]: children.items())
    {
	vector<json> values = parameter_values(value);
	for(auto& value: values)
	    all_values.push_back(std::move(value));

	if (value.find("value") != value.end())
	{
	    json v = *value.find("value");
	    if (v.is_array())
	    {
		// FIXME we are not looking looking into the value for "value" / "children"
		for(int i=0;i<v.size();i++)
		    all_values.push_back(v[i]);
	    }
	    else if (v.is_object())
	    {
		// FIXME we are not looking looking into value2 for "value" / "children"
		for(auto& [key2,value2]: v.items())
		    all_values.push_back(value2);
	    }
	    else
		all_values.push_back(v);
	}
    }
    return all_values;
}

vector<json> parameter_values(const json& j)
{
    auto children = j.find("children");
    if (children == j.end())
	return {};
    else
	return parameter_values_children(*children);
}


void unnest_json(const string & path, const json& indirection_json, json& out)
{
    if (auto value = indirection_json.find("value"); value != indirection_json.end())
        out[path] = *value;

    if (auto children = indirection_json.find("children"); children != indirection_json.end())
        for(auto& [key, value]: children->items())
            unnest_json(path+"/"+key, value, out);
}

json unnest_json(const json& j)
{
    json out;
    for(auto& [key, value]: j.items())
        unnest_json(key, value, out);
    return out;
}

void unnest_json(const string & path, const json&& indirection_json, json& out)
{
    if (auto value = indirection_json.find("value"); value != indirection_json.end())
        out[path] = std::move(*value);

    if (auto children = indirection_json.find("children"); children != indirection_json.end())
        for(auto& [key, value]: children->items())
            unnest_json(path+"/"+key, std::move(value), out);
}

json unnest_json(const json&& j)
{
    json out;
    for(auto& [key, value]: j.items())
        unnest_json(key, std::move(value), out);
    return out;
}

