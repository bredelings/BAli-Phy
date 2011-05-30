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
#include "cow-ptr.H"
#include "object.H"
#include "operator.H"
#include "operation.H"
#include "computation.H"

// FIXME - the way to have a model class with members as submodels is to
//   1. hard-code the SubModels(m) function to return the models you want,
//      instead of arranging to put pointers to member objects into a vector.
//   2. but then when you modify the class member models, you need to notify the parent..
//   3. we COULD have an upwards-pointing chain as well...

std::string parameter_name(const std::string& prefix,int i,int n);

struct Parameter
{
  std::string name;
  polymorphic_cow_ptr<Object> value;
  Bounds<double> bounds;
  bool fixed;
  mutable bool changed;

  Parameter(const std::string&, const Object&);
  Parameter(const std::string&, const Object&, bool);
  Parameter(const std::string&, const Object&, const Bounds<double>&, bool=false);
};


// TODO
// 1. Make models take generic Object's as inputs.
//    o make substitution models take alphabets as arguments, and thus store them in the argument list
// 2. Make models into functions.
//    o separate result objects from model objects
//    o make the model objects inherit from the result objects
//    o inheritance of data members (e.g. Q) becomes inheritance of data members in the result objects
// 3. ??
//    o make all stored objects into models?
//    o make supermodels into an Op plus models, as arguments?
//    o allow models to have computed objects as parts.
//    o allow sharing between models.
//    o pull priors out of models ->
//      + in fact, priors are objects that RETURN models, a "density" model, and a "simulate" model.
// 4. Make model evaluation lazy.
//    o before: remove read().
//    o before: remove set_rate()
//    o before: remove (set_)frequencies( )
//    o during: make recalc() a constant function.
//    o during: write parameters to submodels {now, simply}.  Recalc later.
//    o why do set_rate() and frequencies() cause problems? We should just do them on copies.
// 5. Fix multimodel to return base_model(i,m) instead of base_model(m)?
//
// TODO:
//
// 1. Convert to lazy calculation
// 2. make alphabets a model argument
// 3. go, break all the models by moving their functionality
//    to a result type.
// 4.  This will identify the callers that need to be fixed.
// E-mail sibon.


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
//   (i)  Go adding parameters with group "n", and get the resulting uniquified names.   
//   (ii) Construct the mapping between submodel parameters and top model parameters or constants.
//   (iii) Add the submodel with the specified mapping.
//   (iv) This has been done.
// - We may still need objects to have calculable attributes, like
//   (i) bounds  [only for numeric objects?]
//   (ii) being fixed or not fixed.
//   (iii) type?
//   (iv) bounds which some objects may not have...
//   (v) priors... etc.
//   In short, we need to have other objects which reference variables in two ways
//   * being ABOUT a variable, or variables (as in priors)
//   * refering to a function of the variables.
//   * priors do both, in a sense.

/// \brief An object that depends on a number of parameters, and updates itself when they change.
///
/// Model objects
///   - depend on some number of parameters (currently, all of type double)
///   - update themselves when their parameters change.
///
/// Two Model objects cannot (easily) share a parameter, because 
/// each Model manages and 'owns' its own parameters.  This ownership
/// means that a Model specifies:
/// - a prior distribution on its parameter vector
/// - a name (a string) for each parameter
/// - an attribute (a boolean) that determinies whether each parameter is fixed or variable
///
/// Next: make models out-of-date unless we call model.update();
///
class Model: public Operator
{
protected:
  mutable bool valid;

  /// Parameter values
  std::vector< Parameter > parameters_;

  void validate() const;

  void invalidate() const;

  /// Mark this parameter as being possibly different than the last computed value
  void modify_parameter(int) const;
  /// Mark these parameters as being possibly different than the last computed value
  void modify_parameters(const std::vector<int>&);
  /// Mark all parameters as being possibly different than the last computed value
  void modify_all_parameters() const;
  /// Which parameters have been modified?
  std::vector<int> modified_parameters() const;

  virtual int add_parameter(const Parameter&);
  
  /// \brief Recalculate internal data based on current parameters
  ///
  ////  \param indices The indices of the parameters that changed.
  virtual void recalc(const std::vector<int>& /*indices*/) {}

public:
  /// Recalculate all internal data based on current parameters
  void recalc_all();

  /// Make a copy of this object
  virtual Model* clone() const =0;

  /// the prior for the model, on the same scale as proposal densities
  virtual efloat_t prior() const {return 1.0;}

  /// Names of the parameters
  const std::string& parameter_name(int i) const {return parameters_[i].name;}

  bool is_fixed(int i) const                {return parameters_[i].fixed;}
  void set_fixed(int i,bool f)      {parameters_[i].fixed = f;}

  const Bounds<double>& get_bounds(int i) const {return parameters_[i].bounds;}
  void set_bounds(int i,const Bounds<double>& b) {parameters_[i].bounds = b;}

  /// Get A model parameter value
  boost::shared_ptr<const Object> get_parameter_value(int i) const {return parameters_[i].value;}

  template <typename T>
  bool parameter_has_type(int index) const {return dynamic_pointer_cast<const T>(get_parameter_value(index));}

  /// Get A model parameter value
  template <typename T>
  const T& get_parameter_value_as(int i) const {
    const Object& O = *get_parameter_value(i);
    return dynamic_cast<const T&>(O);
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
  std::vector<boost::shared_ptr<const Object> > get_parameter_values(const std::vector<int>&) const;

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
  std::vector<boost::shared_ptr<const Object> > get_parameter_values() const;

  /// Store ONE parameter value down into submodels
  virtual void write_value(int i, const boost::shared_ptr<const Object>& p);

  /// Set A model parameter value
          void set_parameter_value(int p,Double value);
  /// Set A model parameter value
          void set_parameter_value(int p, const boost::shared_ptr<const Object>& value);
  /// Set SOME of the model parameter values
          void set_parameter_values(const std::vector<int>& indices,const std::vector<Double>& p);
  /// Set SOME of the model parameter values
          void set_parameter_values(const std::vector<int>& indices,const std::vector<boost::shared_ptr<const Object> >& p);
  /// Set ALL model parameter values
          void set_parameter_values(const std::vector<Double>& p);
  /// Set ALL model parameter values
          void set_parameter_values(const std::vector<boost::shared_ptr<const Object> >& p);

  /// Get A model parameter
  const Parameter& get_parameter(int i) const {return parameters_[i];}

  unsigned n_parameters() const {return parameters_.size();}

  /// The model's name
  virtual std::string name() const =0;

  virtual void check() const {}

  // Evaluate the model and return the result
  boost::shared_ptr<const Object> evaluate() const;

  bool is_valid() const {return valid;}
  virtual void update();

  /// Construct an empty model
  Model();

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
  int sub_model_index;
  boost::shared_ptr<const Object> constant_value;

  bool is_constant() const {return parent_index == -1 and sub_model_index == -1;}
  bool is_term_ref() const {return parent_index != -1;}
  bool is_submodel_ref() const {return sub_model_index != -1;}

  arg_expression():parent_index(-1), sub_model_index(-1) {}
  arg_expression(int i):parent_index(i), sub_model_index(-1) {}
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
  /// Store ONE parameter value down into submodels
  void write_value(int i, const boost::shared_ptr<const Object>& p);

public:
  /// Make a copy of ourselves
  SuperModel* clone() const =0;
  
  efloat_t prior() const;

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

  virtual void update();

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

class expression_ref;

class OpModel: virtual public Model, public OperationArgs
{
  /// by index, what does each input parameter do?
  std::vector<std::vector<model_slot> > model_slots_for_index;

  /// these are the instantiated sub_models
  std::vector< polymorphic_cow_ptr< Model > > sub_models;
  /// how do we assemble the inputs for each sub_model
  std::vector<std::vector<arg_expression> > slot_expressions_for_submodel;

  /// what is the top-level operation
  boost::shared_ptr< const Operation > Op;
  /// how do we assemble the inputs for the top-level op?
  std::vector<arg_expression> slot_expressions_for_op;

  void write_value(int i, const boost::shared_ptr<const Object>& p);
public:

  OpModel* clone() const {return new OpModel(*this);}

  //prior

  std::string name() const;

  void check() const;

  boost::shared_ptr<const Object> evaluate(int);
  
  boost::shared_ptr<const Object> evaluate();

  OpModel(const expression_ref&);
};

template <typename X>
class OpModelOf: public OpModel, public X
{
  void recalc(const std::vector<int>& /*indices*/ )
  {
    X::operator=( dynamic_cast<const X&>( *evaluate() ) );
  }

public:
  OpModelOf<X>* clone() const {return new OpModelOf<X>(*this);}

  OpModelOf(const expression_ref& e):OpModel(e) { }
};

int find_parameter(const Model& M,const std::string& name);

void show_parameters(std::ostream& o,const Model& M);

bool has_parameter(const Model& M, const std::string& name);

std::vector<std::string> parameter_names(const Model& M);

std::vector<std::string> short_parameter_names(const Model& M);

std::vector<std::string> short_parameter_names(std::vector<std::string> names);

std::vector<int> parameters_with_extension(const Model& M, std::string name);
#endif
