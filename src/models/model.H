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

#include <vector>
#include <string>
#include <iostream>
#include <utility>
#include <map>

#include "bounds.H"
#include "object.H"
#include "computation/computation.H"
#include "computation/operation.H"
#include "computation/context.H"
#include "cow-ptr.H"

class Model;

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
  virtual void recalc() {}

public:

  /// An object for key-value pairs.
  cow_ptr< std::map<std::string,double> > keys;

  bool contains_key(const std::string& key) const { return keys->count(key);}
  double lookup_key(const std::string& key) const { return keys->find(key)->second;}
  double load_value(const std::string& key,double v) const 
  { 
    auto loc = keys->find(key);
    if (loc != keys->end())
      return loc->second;
    else
      return v;
  }

  /// Make a copy of this object
  virtual Model* clone() const {return new Model(*this);}

  virtual double get_beta() const {return 1.0;}

  virtual void set_beta(double) {std::abort();}

  /// the prior for the model, on the same scale as proposal densities
  virtual log_double_t prior() const;
  virtual log_double_t probability() const {return prior() * likelihood();}
  virtual log_double_t likelihood() const {return 1.0;}

  virtual log_double_t heated_prior() const {return prior();}
  virtual log_double_t heated_likelihood() const {return likelihood();}
  virtual log_double_t heated_probability() const {return heated_prior() * heated_likelihood();}

  bool has_bounds(int i) const;
  const Bounds<double>& get_bounds(int i) const;

  /// Get the value of a modifiable
  template <typename T>
  const T& get_modifiable_value_as(int i) const {
    return get_modifiable_value(i).as_<T>();
  }
  /// Get SOME model modifiable values
  std::vector<expression_ref> get_modifiable_values(const std::vector<int>&) const;

  /// Get SOME model modifiable values
  template <typename T>
  std::vector<T> get_modifiable_values_as(const std::vector<int>& indices) const
  {
    std::vector<T> values(indices.size());
    
    for(int i=0;i<values.size();i++)
      values[i] = get_modifiable_value_as<const T>(indices[i]);
    
    return values;  
  }

  /// Get A model parameter value
  template <typename T>
  const T& get_parameter_value_as(int i) const {
    return get_parameter_value(i).as_<T>();
  }
  /// Get SOME model parameter values
  template <typename T>
  std::vector<T> get_parameter_values_as(const std::vector<int>& indices) const
  {
    std::vector<T> values(indices.size());
    
    for(int i=0;i<values.size();i++)
      values[i] = get_parameter_value_as<const T>(indices[i]);
    
    return values;  
  }

  /// Get SOME model parameter values
  std::vector<expression_ref> get_parameter_values(const std::vector<int>&) const;

  /// Get ALL model parameter values
  template <typename T>
  std::vector<T> get_parameter_values_as() const
  {
    std::vector<T> values(n_parameters());
    
    for(int i=0;i<values.size();i++)
      values[i] = get_parameter_value_as<T>(i);
    
    return values;  
  }
  /// Get ALL model parameter values
  std::vector<expression_ref > get_parameter_values() const;

  /// Set the value of a modifiable
  void set_modifiable_value(int m, const expression_ref& value);

  /// Set A model parameter value
          void set_parameter_value(int p, const expression_ref& value);
  /// Set A model parameter value
          void set_parameter_value(const std::string& p, const expression_ref& value);
  /// Set SOME of the model parameter values
          void set_parameter_values(const std::vector<int>& indices,const std::vector<Double>& p);
  /// Set SOME of the model parameter values
          void set_parameter_values(const std::vector<int>& indices,const std::vector<expression_ref>& p);

  /// Construct an empty model
  Model(const module_loader&);

  virtual ~Model() {};
};

void show_parameters(std::ostream& o,const Model& M);

std::string show_parameters(const Model& M);

bool has_parameter(const Model& M, const std::string& name);

std::vector<std::string> parameter_names(const Model& M);

std::vector<std::string> short_parameter_names(const Model& M);

std::vector<std::string> short_parameter_names(std::vector<std::string> names);

std::vector<int> parameters_with_extension(const Model& M, std::string name);

std::vector<int> parameters_with_extension(const std::vector<std::string>& M, std::string name);

std::string show_probability_expression(const expression_ref&);

std::vector< std::string > show_probability_expressions(const context& C);
#endif
