#ifndef MODEL_H
#define MODEL_H

#include <vector>
#include <string>

#include "clone.H"
#include "mytypes.H"

// FIXME - the way to have a model class with members as submodels is to
//   1. hard-code the SubModels(m) function to return the models you want,
//      instead of arranging to put pointers to member objects into a vector.
//   2. but then when you modify the class member models, you need to notify the parent..
//   3. we COULD have an upwards-pointing chain as well...

std::string parameter_name(const string& prefix,int i,int n);

class Model: public Cloneable 
{
protected:
  /// Model parameters
  std::vector<double> parameters_;
  
  /// Recalculate internal data based on changing one parameter
  void recalc_one(int);
  /// Recalculate internal data based on current parameters
  virtual void recalc(const vector<int>&) {}

  void set_n_parameters(int);

  std::vector<bool> fixed_;

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
  virtual std::string parameter_name(int i) const =0;

  std::string header() const;

  std::string state() const;

  /// Construct an empty model
  Model() {}

  virtual ~Model() {};
};


/// Abstract class for substitution models built on submodels
class SuperModel: virtual public Model 
{
protected:
  int n_super_parameters;

  vector<int> first_index_of_model;
  vector<int> model_of_index;

  /// The prior for the parameters of the upper model
  virtual efloat_t super_prior() const {return 1.0;}
  
  virtual std::string super_parameter_name(int) const { std::abort(); }

  /// Load parameters from sub-models
  void read();

  /// Store ALL parameters down into submodels
  void write();
  /// Store ONE parameter down into submodels
  void write(int i,double p);
  /// Store SOME parameters down into submodels
  void write(const std::vector<int>&,vector<double>::const_iterator&);

  void set_super_parameters(int);

public:
  /// Make a copy of ourselves
  SuperModel* clone() const =0;
  
  std::string parameter_name(int i) const;
  
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

  /// Access the submodel
  template<typename T>
  const T& SubModelAs(int i) const {
    const T* t = dynamic_cast<const T*>(&SubModels(i));
    assert(t);
    return *t;
  }

  template<typename T>
  T& SubModelAs(int i) {
    T* t = dynamic_cast<T*>(&SubModels(i));
    assert(t);
    return *t;
  }

  virtual const Model& SubModels(int) const=0;
  virtual Model& SubModels(int)=0;
  virtual int n_submodels() const=0;
};

/// This is a substitution model built over a submodel of a particular type
template <class SubModel_t>
class SuperModelOver: virtual public SuperModel {
  
protected:
  /// The sub-models this model is build upon
  std::vector< OwnedPointer<SubModel_t> > sub_models;
  
  /// Reference the submodels
  const SubModel_t& SubModels(int i) const {return *sub_models[i];}
  SubModel_t& SubModels(int i) {return *sub_models[i];}
  int n_submodels() const {return sub_models.size();}
  
public:
  /// Make a copy of ourselves
  SuperModelOver<SubModel_t>* clone() const =0;
  
  /// Construct a SuperModel based on model 'M', with 'dp' extra parameters
  SuperModelOver(const std::vector< OwnedPointer<SubModel_t> >& models,int dp)
    :sub_models(models)
  {
    set_super_parameters(dp);
    
    read();
  }

};

/// Abstract class for substitution models built on a single submodel
class NestedModel: virtual public SuperModel {
  
protected:
  /// Access the submodel
  virtual const Model& SubModel() const=0;
  virtual Model& SubModel()=0;
  
public:
  /// Make a copy of ourselves
  NestedModel* clone() const =0;
};


/// This is a substitution model built over a submodel of a particular type
template <class SubModel_t>
class NestedModelOver: virtual public NestedModel, public SuperModelOver<SubModel_t> {
  
protected:
  /// Access the submodel
  const SubModel_t& SubModel() const {return SubModels(0);}
  SubModel_t& SubModel() {return SubModels(0);}
  
public:
  /// Make a copy of ourselves
  NestedModelOver<SubModel_t>* clone() const =0;
  
  /// Construct a Nested model based on model 'M', with 'dp' extra parameters
  NestedModelOver(const SubModel_t& M,int dp)
    :SuperModelOver<SubModel_t>(std::vector< OwnedPointer<SubModel_t> >(1,M),dp)
  { }
};

int find_parameter(const Model& M,const std::string& name);



#endif
