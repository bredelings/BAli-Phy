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

///
/// \file model.H
///
/// \brief This file handles settings, and also parameter names.
///


#ifndef MODEL_H
#define MODEL_H

#include <map>                                      // for map, _Rb_tree_con...
#include <string>                                   // for string
#include <vector>                                   // for vector
#include "util/json.hh"

void load_settings(const std::vector<std::string>& key_value_strings);

typedef std::map<std::string, json::value> key_map_t;

extern key_map_t global_settings;

template<typename T>
T get_setting(const std::string& key, const T& t)
{ 
    auto loc = global_settings.find(key);
    if (loc != global_settings.end())
        return json::value_to<T>(loc->second);
    else
        return t;
}

inline bool setting_exists(const std::string& key) { return global_settings.count(key);}

inline const json::value& lookup_setting(const std::string& key) { return global_settings.find(key)->second;}

#endif
