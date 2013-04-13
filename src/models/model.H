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

#include "mytypes.H"
#include "bounds.H"
#include "object.H"
#include "computation/computation.H"
#include "computation/operation.H"
#include "computation/context.H"

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
protected:

  std::vector< int > bounds;
  std::vector< int> prior_note_index;

  int prior_index;

  void add_parameter_(const std::string&);
  int add_parameter(const std::string&);
  int add_parameter(const std::string&,const object_ref&);
  int add_parameter(const std::string&,const object_ref&,const Bounds<double>&);
  
  /// \brief Recalculate internal data based on current parameters
  ///
  ////  \param indices The indices of the parameters that changed.
  virtual void recalc() {}

public:

  /// Make a copy of this object
  virtual Model* clone() const {return new Model(*this);}

  /// the prior for the model, on the same scale as proposal densities
  virtual efloat_t prior() const;

  /// Names of the parameters
  bool is_random_variable(int i) const;

  bool has_bounds(int i) const;
  const Bounds<double>& get_bounds(int i) const;
  void set_bounds(int i,const expression_ref&);
  void set_bounds(int i,const Bounds<double>&);

  /// Get the value of a modifiable
  template <typename T>
  bool modifiable_has_type(int index) const {return boost::dynamic_pointer_cast<const T>(get_modifiable_value(index));}

  /// Get the value of a modifiable
  template <typename T>
  const T& get_modifiable_value_as(int i) const {
    return *convert<const T>(get_modifiable_value(i));
  }
  /// Get SOME model modifiable values
  std::vector<object_ref> get_modifiable_values(const std::vector<int>&) const;

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
  bool parameter_has_type(int index) const {return (bool)boost::dynamic_pointer_cast<const T>(get_parameter_value(index));}

  /// Get A model parameter value
  template <typename T>
  const T& get_parameter_value_as(int i) const {
    return *convert<const T>(get_parameter_value(i));
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
  std::vector<object_ptr<const Object> > get_parameter_values(const std::vector<int>&) const;

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
  std::vector<object_ptr<const Object> > get_parameter_values() const;

  /// Set the value of a modifiable
  void set_modifiable_value(int m, const object_ref& value);

  /// Set A model parameter value
          void set_parameter_value(int p,Double value);
  /// Set A model parameter value
          void set_parameter_value(int p, const object_ptr<const Object>& value);
  /// Set A model parameter value
          void set_parameter_value(const std::string& p, const object_ptr<const Object>& value);
  /// Set SOME of the model parameter values
          void set_parameter_values(const std::vector<int>& indices,const std::vector<Double>& p);
  /// Set SOME of the model parameter values
          void set_parameter_values(const std::vector<int>& indices,const std::vector<object_ptr<const Object> >& p);

  virtual std::vector<std::string> show_priors() const;

  int add_note(const expression_ref&);

  void process_note(int);

  // Add a Module, including any notes and declared parameters.
  std::vector<int> add_submodel(const Module&);

  std::vector<int> add_submodel(const Model_Notes&);

  /// Construct an empty model
  Model(const module_loader&);

  Model(const module_loader&, const std::vector<expression_ref>&);

  virtual ~Model() {};
};

void show_parameters(std::ostream& o,const Model& M);

bool has_parameter(const Model& M, const std::string& name);

std::vector<std::string> parameter_names(const Model& M);

std::vector<std::string> short_parameter_names(const Model& M);

std::vector<std::string> short_parameter_names(std::vector<std::string> names);

std::vector<int> parameters_with_extension(const Model& M, std::string name);

std::vector<int> parameters_with_extension(const std::vector<std::string>& M, std::string name);

std::string show_probability_expression(const expression_ref&);

std::vector< std::string > show_probability_expressions(const context& C);
#endif
