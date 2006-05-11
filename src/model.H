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

class Model: public Cloneable {
protected:
  /// Model parameters
  std::vector<double> parameters_;
  
  /// Recalculate internal data based on current parameters
  virtual void recalc() {}

  void set_n_parameters(int);

  std::vector<bool> fixed_;

public:

  /// Make a copy of this object
  virtual Model* clone() const =0;

  /// the prior for the model, on the same scale as 'fiddle()'
  virtual efloat_t prior() const {return 1.0;}

  bool fixed(int i) const {return fixed_[i];}

  const std::vector<bool>& fixed() const {return fixed_;}

  virtual void fixed(int i,bool f) 
  {
    fixed_[i] = f;
  }

  virtual void fixed(const std::vector<bool>& f)
  {
    assert(fixed_.size() == f.size());
    fixed_ = f;
  }

  /// Get a model parameter
  virtual double parameter(int p) const {
    return parameters_[p];
  }
  /// Get the model parameters
  virtual const std::vector<double>& parameters() const {return parameters_;}

  /// Set A model parameter
  virtual void parameter(int p,double value);
  /// Set SOME of the model parameters
  virtual void parameters(const std::vector<int>& indices,const std::vector<double>& p);
  /// Set ALL model parameters
  virtual void parameters(const std::vector<double>& p);

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
  std::vector<double> super_parameters_;

  /// The prior for the parameters of the upper model
  virtual efloat_t super_prior() const {return 1.0;}
  
  virtual std::string super_parameter_name(int i) const=0;

  virtual const std::vector<double>& super_parameters() const {return super_parameters_;}

  virtual void super_parameters(const std::vector<double>& p) {
    assert(p.size() == super_parameters_.size());
    super_parameters_ = p;
    recalc();
  }
  
  /// Load parameters from sub-models
  void read();

  /// Store parameters down into submodels
  void write();

public:
  /// Make a copy of ourselves
  SuperModel* clone() const =0;
  
  std::string parameter_name(int i) const;
  
  efloat_t prior() const;
  
  bool fixed(int i) const {return Model::fixed(i);}

  virtual void fixed(int i,bool f) {Model::fixed(i,f); write();}

  virtual void fixed(const std::vector<bool>& f) {Model::fixed(f); write();}

  std::vector<bool> fixed() const {return Model::fixed();}

  /// Get a model parameter
  double parameter(int p) const {
    return Model::parameter(p);
  }
  /// Get the model parameters
  const std::vector<double>& parameters() const {
    return Model::parameters();
  }

  /// Set A model parameter
  void parameter(int p,double value);
  /// Set SOME of the model parameters
  void parameters(const std::vector<int>& indices,const std::vector<double>& p);
  /// Set ALL model parameters
  void parameters(const std::vector<double>& p);

  /// Access the submodel
  virtual const Model& SubModels(int) const=0;
  virtual Model& SubModels(int)=0;
  virtual int n_submodels() const=0;
};

/// This makes supermodels from derived classes of ::Model
template <class DerivedModel_t>
class SuperDerivedModel: virtual public SuperModel, virtual public DerivedModel_t {
public:  

  /// Make a copy of ourselves
  SuperDerivedModel* clone() const =0;

  /// Access the submodel
  const DerivedModel_t& SubModels(int) const=0;
  DerivedModel_t& SubModels(int)=0;
};


/// This is a substitution model built over a submodel of a particular type
template <class SubModel_t>
class SuperModelOver: virtual public SuperModel {
  
  /// The sub-models this model is build upon
  std::vector< OwnedPointer<SubModel_t> > sub_models;
  
protected:
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
    super_parameters_.resize(dp);

    int total=0;
    for(int m=0;m<n_submodels();m++)
      total += SubModels(m).parameters().size();

    set_n_parameters(total + dp);
    
    read();
  }

};

template <class SubModel_t, class DerivedModel_t>
class SuperDerivedModelOver: public SuperModelOver<SubModel_t>, 
			     virtual public SuperDerivedModel<DerivedModel_t> 
{
public:

  const SubModel_t& SubModels(int i) const {return SuperModelOver<SubModel_t>::SubModels(i);}
  SubModel_t& SubModels(int i) {return SuperModelOver<SubModel_t>::SubModels(i);}

  /// Make a copy of ourselves
  SuperDerivedModelOver<SubModel_t,DerivedModel_t>* clone() const =0;

  //  std::string parameter_name(int i) const {
  //    return SuperModel::parameter_name(i);
  //  }

  SuperDerivedModelOver(const std::vector< OwnedPointer<SubModel_t> >& models,int dp)
    : SuperModelOver<SubModel_t>(models,dp)
  {}
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

/// This is a substitution model built over a submodel of a particular type
template <class SubModel_t, class DerivedModel_t>
class NestedDerivedModelOver: virtual public NestedModel, 
		       public SuperDerivedModelOver<SubModel_t,DerivedModel_t> {
  
protected:
  /// Access the submodel
  const SubModel_t& SubModel() const {return SubModels(0);}
  SubModel_t& SubModel() {return SubModels(0);}
  
public:
  /// Make a copy of ourselves
  NestedDerivedModelOver<SubModel_t,DerivedModel_t>* clone() const =0;
  
  /// Construct a Nested model based on model 'M', with 'dp' extra parameters
  NestedDerivedModelOver(const SubModel_t& M,int dp)
    :SuperDerivedModelOver<SubModel_t,DerivedModel_t>(std::vector< OwnedPointer<SubModel_t> >(1,M),dp)
  { }
};

int find_parameter(const Model& M,const std::string& name);



#endif
