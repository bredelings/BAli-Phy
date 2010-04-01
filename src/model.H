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

// FIXME - the way to have a model class with members as submodels is to
//   1. hard-code the SubModels(m) function to return the models you want,
//      instead of arranging to put pointers to member objects into a vector.
//   2. but then when you modify the class member models, you need to notify the parent..
//   3. we COULD have an upwards-pointing chain as well...

std::string parameter_name(const std::string& prefix,int i,int n);

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
  std::vector<double> parameters_;
  /// Parameter names
  std::vector<std::string> parameter_names_;
  /// Is this parameter fixed?
  std::vector<bool> fixed_;

  virtual void add_parameter(const std::string&,double);
  
  /// \brief Recalculate internal data based on changing one parameter
  ///
  /// \param p The parmeter that changed.
  void recalc_one(int p);

  /// \brief Recalculate internal data based on current parameters
  ///
  ////  \param indices The indices of the parameters that changed.
  virtual void recalc(const std::vector<int>& indices) {}

public:
  /// Recalculate all internal data based on current parameters
  virtual void recalc_all();

  /// Make a copy of this object
  virtual Model* clone() const =0;

  /// the prior for the model, on the same scale as 'fiddle()'
  virtual efloat_t prior() const {return 1.0;}

  bool fixed(int i) const                {return fixed_[i];}
  virtual void fixed(int i,bool f)       {fixed_[i] = f;}

  const std::vector<bool>& fixed() const {return fixed_;}
  virtual void fixed(const std::vector<bool>& f) { assert(fixed_.size() == f.size()); fixed_ = f; }

  /// Get A model parameter
  virtual double parameter(int p) const {return parameters_[p];}
  /// Get SOME model parameters
  virtual std::vector<double> parameters(const std::vector<int>&) const;
  /// Get ALL model parameters
  virtual const std::vector<double>& parameters() const {return parameters_;}

  /// Set A model parameter
  virtual void parameter(int p,double value);
  /// Set SOME of the model parameters
  virtual void parameters(const std::vector<int>& indices,const std::vector<double>& p);
  /// Set SOME of the model parameters
  virtual void parameters(const std::vector<int>& indices,std::vector<double>::const_iterator& p);
  /// Set ALL model parameters
  virtual void parameters(const std::vector<double>& p);

  unsigned n_parameters() const {return parameters_.size();}

  /// The model's name
  virtual std::string name() const =0;

  /// Names of the parameters
  const std::string& parameter_name(int i) const {return parameter_names_[i];}

  std::string header() const;

  std::string state() const;

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
class SuperModel: virtual public Model 
{
protected:
  std::vector<std::string> short_parameter_names;
  std::vector<std::string> model_prefix;
  std::vector<int> first_index_of_model;
  std::vector<int> model_of_index;

  int n_super_parameters() const;

  void add_parameter(const std::string&,double);
  void add_super_parameter(const std::string&,double);
  virtual void add_submodel(const std::string&,const Model&);

  /// The prior for the parameters of the upper model
  virtual efloat_t super_prior() const {return 1.0;}
  
  /// Load parameters from sub-models
  void read();

  /// Propagate changes to child model parameters down to child Model objects.
  void write();
  /// Store ONE parameter down into submodels
  void write(int i,double p);
  /// Store SOME parameters down into submodels
  void write(const std::vector<int>&,std::vector<double>::const_iterator&);

  void prefix_names();

public:
  /// Make a copy of ourselves
  SuperModel* clone() const =0;
  
  efloat_t prior() const;
  
  bool fixed(int i) const {return Model::fixed(i);}
  virtual void fixed(int i,bool f) {Model::fixed(i,f); write();}
  virtual void fixed(const std::vector<bool>& f) {Model::fixed(f); write();}
  std::vector<bool> fixed() const {return Model::fixed();}

  /// Get A model parameter
  double parameter(int p) const {return Model::parameter(p); }
  /// Get SOME model parameters
  virtual std::vector<double> parameters(const std::vector<int>& indices) const {return Model::parameters(indices);}
  /// Get ALL model parameters
  const std::vector<double>& parameters() const {return Model::parameters();}

  /// Set A model parameter
  void parameter(int p,double value);
  /// Set SOME of the model parameters
  void parameters(const std::vector<int>& indices,const std::vector<double>& p);
  /// Set SOME of the model parameters
  void parameters(const std::vector<int>& indices,std::vector<double>::const_iterator& p);
  /// Set ALL model parameters
  void parameters(const std::vector<double>& p);

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

  SuperModel();
};

/// This is a substitution model built over a submodel of a particular type
template <class SubModel_t>
class SuperModelOver: virtual public SuperModel {
  
protected:
  /// The sub-models this model is build upon
  std::vector< owned_ptr<SubModel_t> > sub_models;

  void insert_submodel(const std::string& name, const SubModel_t& M)
  {
    sub_models.push_back(M);
    add_submodel(name,M);
  }
  
  /// Reference the submodels
  const SubModel_t& SubModels(int i) const {return *sub_models[i];}
  SubModel_t& SubModels(int i) {return *sub_models[i];}
  int n_submodels() const {return sub_models.size();}
  
public:
  /// Make a copy of ourselves
  SuperModelOver<SubModel_t>* clone() const =0;
};

int find_parameter(const Model& M,const std::string& name);

void show_parameters(std::ostream& o,const Model& M);

bool has_parameter(const Model& M, const std::string& name);

std::vector<int> parameters_with_extension(const Model& M, std::string name);
#endif
