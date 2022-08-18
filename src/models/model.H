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
/// \brief This file defines a generic Model class.
///


#ifndef MODEL_H
#define MODEL_H

#include <iostream>                                 // for ostream
#include <map>                                      // for map, _Rb_tree_con...
#include <string>                                   // for string
#include <utility>                                  // for pair
#include <vector>                                   // for vector
#include <boost/filesystem/path.hpp>
#include "computation/context.H"                    // for context
#include "computation/expression/expression_ref.H"  // for EVector, expressi...
#include "computation/module.H"                     // for Module
#include "util/cow-ptr.H"                           // for cow_ptr
#include "util/json.hh"                             // for json
#include "util/math/log-double.H"                   // for log_double_t, ope...
#include "util/bounds.H"
class Program;

// Variable State: (i) Dirichlet Process or (ii) Augmenting and unaugmenting
//
// As long as we can express the probability as a fixed expression of this
// variable state then I think its OK.
//
// This could be tricky for the dirichlet process, unless we can make the entire 
// vector a parameter.... and ALSO make its elements parameters?
//
// It almost seems like we could use a list...?


/// \brief An object that depends on a number of parameters, and updates itself when they change.
///
/// Model objects currently wrap a context object.  They add the ability to
/// call recalc() to fixup things after changing a parameter.
///
class Model: public context
{
    /// \brief Recalculate internal data based on current parameters
    ///
    ////  \param indices The indices of the parameters that changed.

public:
    typedef std::map<std::string, json> key_map_t;

    /// An object for key-value pairs.
    cow_ptr< key_map_t > keys;

    bool contains_key(const std::string& key) const { return keys->count(key);}
    json lookup_key(const std::string& key) const { return keys->find(key)->second;}

    template<typename T>
    T load_value(const std::string& key, const T& t) const 
	{ 
	    auto loc = keys->find(key);
	    if (loc != keys->end())
		return loc->second;
	    else
		return t;
	}

    /// Make a copy of this object
    virtual Model* clone() const {return new Model(*this);}

    Model& operator=(const Model&) = default;
    Model& operator=(Model&&) = default;

    /// Construct an empty model
    Model(const Program& P, const key_map_t& k = {});

    /// Construct an empty model
    Model(const context_ref& C, const key_map_t& k = {});

    Model(const Model&) = default;
    Model(Model&&) = default;

    virtual ~Model() {};
};

Model::key_map_t parse_key_map(const std::vector<std::string>& key_value_strings);

std::vector<std::string> short_parameter_names(const std::vector<std::string>& names);

std::map<std::string,std::map<std::string,int>> parameters_with_extension(const std::vector<std::string>& M, std::string name);

std::vector<int> flatten(const std::map<std::string,std::map<std::string,int>>& names);

std::string show_probability_expression(const expression_ref&);

std::vector< std::string > show_probability_expressions(const context& C);

void execute_file(const std::shared_ptr<module_loader>& L, const boost::filesystem::path& filename);
#endif
