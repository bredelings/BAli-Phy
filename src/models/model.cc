/*
  Copyright (C) 2004-2006,2009-2012 Benjamin Redelings

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

#include <set>
#include <map>

#include "util/ptree.H"

#include "util/string/join.H"
#include "util/string/split.H"
#include "util/myexception.H"
#include "models/model.H"
#include "models/path.H"
#include "computation/program.H"
#include "computation/module.H"
#include "computation/loader.H"

#include "tools/stats-table.H"              // for has_children( )

using std::vector;
using std::string;

key_map_t global_settings;

void load_settings(const vector<string>& key_value_strings)
{
    for(const auto& key_value_pair: key_value_strings )
    {
	vector<string> parse = split(key_value_pair,'=');
	if (parse.size() != 2)
	    throw myexception()<<"Ill-formed key-value pair '"<<key_value_pair<<"'.";

	string key = parse[0];

	global_settings[key] = json::parse(parse[1], {}, {.allow_infinity_and_nan=true});
    }
}

