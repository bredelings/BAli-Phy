/*
   Copyright (C) 2004-2006,2009-2010 Benjamin Redelings

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

#include "clone.H"
#include "mytypes.H"
#include "bounds.H"

// FIXME - the way to have a model class with members as submodels is to
//   1. hard-code the SubModels(m) function to return the models you want,
//      instead of arranging to put pointers to member objects into a vector.
//   2. but then when you modify the class member models, you need to notify the parent..
//   3. we COULD have an upwards-pointing chain as well...

std::string parameter_name(const std::string& prefix,int i,int n);

struct Parameter
{
  std::string name;
  double value;
  Bounds<double> bounds;
  bool fixed;

  Parameter(const std::string&, double);
  Parameter(const std::string&, double, bool);
  Parameter(const std::string&, double, const Bounds<double>&, bool=false);
};


// Rewrite:
// - A supermodel is actually a function on a collection of sub-expressions.
// - Currently models carry their own parameters, but in the future the expressions should
//   be separate from the state. (see Context.C)
// - When adding a submodel, we are essentially
//   (i)  registering a sub-expression to be calculated
//   (ii) mapping the parameters of the sub-expression onto either
//     (ii.a) constants
//     (ii.b) parameters of the *main* expression
// - Therefore, to add a submodel, and also all of its parameters as separate parameters, I would:
//   (i)  Go through add get add parameters with group "n", and get the resulting uniquified names.   
//   (ii) Construct the mapping between submodel parameters and top model parameters or constants.
//   (iii) Add the submodel with the specified mapping.

/// \brief An object that depends on a number of parameters, and updates itself when they change.
///
/// Model objects
///   - depend on some number of parameters (all of type double)
///   - update themselves when their parameters change.
///
/// Two Model objects cannot (easily) share a parameter, because 
/// each Model manages and 'owns' its own parameters.  This ownership
/// means that a Model specifies:
/// - a prior distribution on its parameter vector
/// - a name (a string) for each parameter
/// - an attribute (a boolean) that determinies whether each parameter is fixed or variable
///
class Model: public Cloneable 
{
protected:
  /// Parameter values
  std::vector< Parameter > parameters_;

  virtual int add_parameter(const Parameter&);
  
  /// \brief Recalculate internal data based on changing one parameter
  ///
  /// \param i The parameter that changed.
  void recalc_one(int i);

  /// \brief Recalculate internal data based on current parameters
  ///
  ////  \param indices The indices of the parameters that changed.
  virtual void recalc(const std::vector<int>& /*indices*/) {}

public:
  /// Recalculate all internal data based on current parameters
  virtual void recalc_all();

  /// Make a copy of this object
  virtual Model* clone() const =0;

  /// the prior for the model, on the same scale as 'fiddle()'
  virtual efloat_t prior() const {return 1.0;}

  /// Names of the parameters
  const std::string& parameter_name(int i) const {return parameters_[i].name;}

  bool is_fixed(int i) const                {return parameters_[i].fixed;}
  virtual void set_fixed(int i,bool f)      {parameters_[i].fixed = f;}

  const Bounds<double>& get_bounds(int i) const {return parameters_[i].bounds;}
  virtual void set_bounds(int i,const Bounds<double>& b) {parameters_[i].bounds = b;}

  /// Get A model parameter value
  double get_parameter_value(int i) const {return parameters_[i].value;}
  /// Get SOME model parameter values
  std::vector<double> get_parameter_values(const std::vector<int>&) const;
  /// Get ALL model parameter values
  std::vector<double> get_parameter_values() const;

  /// Set A model parameter value
  virtual void set_parameter_value(int p,double value);
  /// Set SOME of the model parameter values
  virtual void set_parameter_values(const std::vector<int>& indices,const std::vector<double>& p);
  /// Set SOME of the model parameter values
  virtual void set_parameter_values(const std::vector<int>& indices,std::vector<double>::const_iterator& p);
  /// Set ALL model parameter values
  virtual void set_parameter_values(const std::vector<double>& p);

  /// Get A model parameter
  const Parameter& get_parameter(int i) const {return parameters_[i];}
  /// Get ALL model parameters
  const std::vector<Parameter>& get_parameters() const {return parameters_;}
  /// Set A model parameter value
  virtual void set_parameter(int p,const Parameter& P);
  /// Set ALL model parameters
  virtual void set_parameters(const std::vector<Parameter>& P);

  unsigned n_parameters() const {return parameters_.size();}

  /// The model's name
  virtual std::string name() const =0;

  std::string header() const;

  std::string state() const;

  virtual void check() const {}

  /// Construct an empty model
  Model() {}

  virtual ~Model() {};
};


/// \brief Abstract class for Model objects that are built out of other Model objects.
///
/// A Model object that implements the SuperModel interface can
/// contain other Model objects as parts - child Model parameters are
/// mapped to parameters in the parent Model.  However, two child
/// Models cannot (easily) share a parameter; this follows from the
/// fact that each child Model 'owns' its parameters.
///
/// Names of child Model parameters are unchanged in the parent Model
/// as long as they are unique.  However, if two child Models contain
/// parameters with the same name, the parameters are disambiguated
/// using the name of the child Model as a prefix, separated by '::'.
///
/// Changes to child Model parameters are propagated down into child
/// Model objects using the various write( ) functions.  Values from
/// child Models are propagated into parameters in the parent Models
/// using the read() function.
///

struct arg_expression
{
  int parent_index;
  double constant_value;
  bool is_constant() const {return parent_index == -1;}
  bool is_term_ref() const {return parent_index != -1;}

  arg_expression():parent_index(-1),constant_value(0) {}
  arg_expression(int i):parent_index(i),constant_value(0) {}
};

struct model_slot
{
  int model_index;
  int slot;
  model_slot():model_index(-1),slot(-1) {}
  model_slot(int m,int s):model_index(m),slot(s) {}
};

class SuperModel: virtual public Model 
{
protected:
  std::vector<std::vector<arg_expression> > slot_expressions_for_submodel;
  std::vector<std::vector<model_slot> > model_slots_for_index;

  std::vector<int> first_index_of_model;
  std::vector<int> model_of_index;

  /// Is parameter 'index' used by model m?
  bool parameter_is_used_by_model(int index, int m) const;
  /// Is parameter 'index' use by the top level model?
  bool is_super_parameter(int index) const;

  // How many parameters are used at the top level?
  int n_super_parameters() const;
  int add_super_parameter(const Parameter&);

  int add_parameter(const Parameter&);

  int register_last_submodel(const std::vector<arg_expression>&);
  // Register a submodel, creating separate parameter for each submodel parameter
  int register_submodel(const std::vector<arg_expression>&);
  // Register a submodel, creating separate parameter for each submodel parameter
  int register_submodel(const std::string&);

  /// The prior for the parameters of the upper model
  virtual efloat_t super_prior() const {return 1.0;}
  
  /// Load parameter values from all sub-models.  Values may conflict!
  void read();
  /// Load parameter values from this in sub-model m.  Values may conflict!
  void read_from_submodel(int);
  /// Propagate changes to child model parameters down to child Model objects.
  void write();
  /// Propagate changes to child model parameters down to child Model objects.
  void write_to_submodel(int m);
  /// Store ONE parameter down into submodels
  void write_one(int i, const Parameter& P);
  /// Store ONE parameter value down into submodels
  void write_value(int i,double p);
  /// Store SOME parameters down into submodels
  void write_values(const std::vector<int>&,std::vector<double>::const_iterator&);

public:
  /// Make a copy of ourselves
  SuperModel* clone() const =0;
  
  efloat_t prior() const;
  
  virtual void set_fixed(int i,bool f) {Model::set_fixed(i,f); write();}
  virtual void set_bounds(int i,const Bounds<double>& b) {Model::set_bounds(i,b); write();}

  /// Set A model parameter
  void set_parameter_value(int p,double value);
  /// Set SOME of the model parameters
  void set_parameter_values(const std::vector<int>& indices,const std::vector<double>& p);
  /// Set SOME of the model parameters
  void set_parameter_values(const std::vector<int>& indices,std::vector<double>::const_iterator& p);
  /// Set ALL model parameters
  void set_parameter_values(const std::vector<double>& p);

  /// Set A model parameter
  void set_parameter(int i, const Parameter& P);
  /// Set ALL model parameters
  void set_parameters(const std::vector<Parameter>& P);

  /// Access a child Model as type T
  template<typename T>
  const T& SubModelAs(int i) const {
    const T* t = dynamic_cast<const T*>(&SubModels(i));
    assert(t);
    return *t;
  }

  /// Access a child Model as type T
  template<typename T>
  T& SubModelAs(int i) {
    T* t = dynamic_cast<T*>(&SubModels(i));
    assert(t);
    return *t;
  }

  /// Access a child Model
  virtual const Model& SubModels(int) const=0;

  /// Access a child Model
  virtual Model& SubModels(int)=0;

  /// How many child models are there?
  int n_submodels() const;

  void check() const;

  SuperModel();
};

/// This is a substitution model built over a submodel of a particular type
template <class SubModel_t>
class SuperModelOver: virtual public SuperModel {
  
protected:
  /// The sub-models this model is build upon
  std::vector< owned_ptr<SubModel_t> > sub_models;

  int insert_submodel(const std::string& name, const SubModel_t& M)
  {
    sub_models.push_back(M);
    return register_submodel(name);
  }
  
  /// Reference the submodels
  const SubModel_t& SubModels(int i) const {return *sub_models[i];}
  SubModel_t& SubModels(int i) {return *sub_models[i];}

public:
  /// Make a copy of ourselves
  SuperModelOver<SubModel_t>* clone() const =0;
};

int find_parameter(const Model& M,const std::string& name);

void show_parameters(std::ostream& o,const Model& M);

bool has_parameter(const Model& M, const std::string& name);

std::vector<std::string> parameter_names(const Model& M);

std::vector<std::string> short_parameter_names(const Model& M);

std::vector<std::string> short_parameter_names(std::vector<std::string> names);

std::vector<int> parameters_with_extension(const Model& M, std::string name);
#endif
