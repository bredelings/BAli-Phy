#ifndef SMODEL_OBJECTS
#define SMODEL_OBJECTS

#include <vector>
#include <string>
#include <boost/shared_ptr.hpp>
#include <boost/numeric/ublas/symmetric.hpp>
#include "alphabet.H"
#include "eigenvalue.H"
#include "cached_value.H"
#include "cow-ptr.H"
#include <valarray>

template <typename T,typename U>
std::valarray<T> get_varray(const std::vector<U>& v1) 
{
  std::valarray<T> v2(v1.size());
  for(int i=0;i<v2.size();i++)
    v2[i] = v1[i];
  return v2;
}

template <typename T,typename U>
std::vector<T> get_vector(const std::valarray<U>& v1) 
{
  std::vector<T> v2(v1.size());
  for(int i=0;i<v2.size();i++)
    v2[i] = v1[i];
  return v2;
}

template <typename T,typename U>
std::vector<T> get_vector(const std::vector<U>& v1) 
{
  std::vector<T> v2(v1.size());
  for(int i=0;i<v2.size();i++)
    v2[i] = v1[i];
  return v2;
}

template <typename T, typename U>
void set_varray(std::vector<U>& v1,int start,const std::valarray<T>& v2) 
{
  assert(start>=0);
  assert(v2.size() > 0);
  assert(start + v2.size() <= v1.size());

  //copy from valarray
  for(int i=0;i<v2.size();i++)
    v1[start + i] = v2[i];
}

template <typename T, typename U>
void set_varray(std::vector<U>& v1,int start,const std::vector<T>& v2) 
{
  assert(start>=0);
  assert(v2.size() > 0);
  assert(start + v2.size() <= v1.size());

  //copy from valarray
  for(int i=0;i<v2.size();i++)
    v1[start + i] = v2[i];
}

namespace substitution
{
  //------------------------- Markov Substitution Model ----------------------------//
  struct SModelObject: virtual public Object
  {
  protected:
    boost::shared_ptr<const alphabet> a;

  public:
    std::vector<unsigned> state_letters_;

    const std::vector<unsigned>& state_letters() const {return state_letters_;}

    unsigned n_states() const {return state_letters_.size();}

    const alphabet& Alphabet() const {return *a;}

    boost::shared_ptr<const alphabet> get_alphabet() const {return a;}

    int n_letters() const {return a->size();}

    SModelObject(const alphabet&);

    SModelObject(const alphabet&,int);
  };

  //------------------------ Exchangeability Model ----------------------//


  struct ExchangeModelObject: virtual public Object
  {
    ublas::symmetric_matrix<double> S;

    virtual ExchangeModelObject* clone() const {return new ExchangeModelObject(*this);}

    double operator()(int i,int j) const {return S(i,j);}

    int n_states() const {return S.size1();}

    ExchangeModelObject(int n):S(n,n)
    { }
  };

  struct AlphabetExchangeModelObject: public SModelObject, public ExchangeModelObject
  {
    virtual AlphabetExchangeModelObject* clone() const {return new AlphabetExchangeModelObject(*this);}

    AlphabetExchangeModelObject(const alphabet& a);
    AlphabetExchangeModelObject(const alphabet& a, int n);
  };

  struct ReversibleFrequencyModelObject: public SModelObject
  {
    ReversibleFrequencyModelObject* clone() const {return new ReversibleFrequencyModelObject(*this);}

    Matrix R;

    double operator()(int i,int j) const {return R(i,j);}

    std::vector<double> pi;

    ReversibleFrequencyModelObject(const alphabet& a)
      : SModelObject(a),
	R(a.size(),a.size()), pi(a.size())
    { }
  };


  class ReversibleAdditiveObject: public SModelObject
  {
  public:
    virtual ReversibleAdditiveObject* clone() const =0;

    virtual double rate() const =0;

    virtual void set_rate(double) =0;

    virtual Matrix transition_p(double t) const = 0;

    virtual std::valarray<double> frequencies() const =0;

    ReversibleAdditiveObject(const alphabet& a);

    ReversibleAdditiveObject(const alphabet& a, int n);
  };

  class ReversibleAdditiveCollectionObject: virtual public Object
  {
  protected:
    std::vector< polymorphic_cow_ptr<ReversibleAdditiveObject> > parts;

  public:
    virtual ReversibleAdditiveCollectionObject* clone() const {return new ReversibleAdditiveCollectionObject(*this);}

    boost::shared_ptr<const alphabet> get_alphabet() const;

    const alphabet& Alphabet() const {return *get_alphabet();}

    const std::vector<unsigned>& state_letters() const;
    
    unsigned n_states() const {return state_letters().size();}

    /// How many different models do we contain?
    int n_parts() const;

    /// Return the i-th branch model
    const ReversibleAdditiveObject& part(int i) const;
    /// Return the i-th branch model
          ReversibleAdditiveObject& part(int i);

    double rate() const;
    void set_rate(double);

    /// The transition probability matrix over time t for the i-th branch model
    Matrix transition_p(double t,int i) const;

    /// Get the equilibrium frequencies.  Currently all branch models must have the same frequencies.
    std::valarray<double> frequencies() const;

    ReversibleAdditiveCollectionObject() {};

    ReversibleAdditiveCollectionObject(const ReversibleAdditiveObject& O)
      :parts(1,ptr(O))
    {}
  };

  class ReversibleMarkovModelObject: public ReversibleAdditiveObject
  {
  protected:
    mutable cached_value<EigenValues> eigensystem;

    void recalc_eigensystem() const;

  public:
    /// Rate matrix
    Matrix Q;

    std::vector<double> pi;

    virtual ReversibleMarkovModelObject* clone() const {return new ReversibleMarkovModelObject(*this);}

    std::valarray<double> frequencies() const;

    double rate() const;

    void set_rate(double);

    void invalidate_eigensystem();

    const EigenValues& get_eigensystem() const;

    /// The transition probability matrix - which we can now compute
    Matrix transition_p(double t) const;

    ReversibleMarkovModelObject(const alphabet& a);

    ReversibleMarkovModelObject(const alphabet& a,int n);
  };

  struct F81_Object: public ReversibleMarkovModelObject
  {
  public:
    double alpha_;

    /// Make a copy of this object
    virtual F81_Object* clone() const {return new F81_Object(*this);}

    double rate() const;
    void set_rate(double);

    /// The transition probability matrix - which we can now compute
    Matrix transition_p(double t) const;

    F81_Object(const alphabet& a);
    F81_Object(const alphabet& a, const std::valarray<double>&);
  };
  
  class DiscreteDistribution: virtual public Object
  {
  public:
    std::vector< double > fraction;
    // If we really want to change this, make it polymorphic_cow_ptr<Object>, so we can assign from shared_ptr<const Object>
    std::vector< boost::shared_ptr<const Object> > values;

    DiscreteDistribution* clone() const {return new DiscreteDistribution(*this);}

    int size() const;

    DiscreteDistribution(int s);
  };

  //Hmmm [FIXME] ... The peeling algorithm assumes independence across branches
  //                 and a reversible markov chain across each branch.
  //
  //                 The reversible Markov chain on a branch can be generated from
  //                 a CONTINUOUS TIME Markov Chain - OR something else.
  //
  //                 Right now I am requiring an Independant, Additive, Reversible
  //                 PROCESS, but of course the gamma_branch model doesn't fit
  //                 that requirement.  So we are faking it...

  //NOTE: - a linear combination of RA models is RA, but not RAI
  //Do I really need to special-case this so that multi-models of RA models are
  //known to be RA?  Would prefer not to use templates unless necessary/helpful.

  /// A mixture of reversible additive models

  /// Umm... is this reversible?
  /// Also, what if we have two base classes: ReversibleAdditive, and F81?
  class MultiModelObject: virtual public Object
  {
  public:
    boost::shared_ptr<const alphabet> a;

    std::vector< polymorphic_cow_ptr<ReversibleAdditiveCollectionObject> > base_models;

    std::vector< double > fraction;

    boost::shared_ptr<const alphabet> get_alphabet() const;

    const alphabet& Alphabet() const {return *get_alphabet();}

    virtual MultiModelObject* clone() const {return new MultiModelObject(*this);}

    double rate() const;
    void set_rate(double);

    /// The number of components
    int n_base_models() const {return base_models.size();}
    void resize(int i);

    /// The number of different branch models per components
    int n_parts() const;

    /// Access the base models
    const ReversibleAdditiveCollectionObject& base_model(int m) const {return *base_models[m];}
          ReversibleAdditiveCollectionObject& base_model(int m)       {return *base_models[m];}

    /// Get the probability of each base models
    const std::vector<double>& distribution() const {return fraction;}

    const std::vector<unsigned>& state_letters() const {return base_model(0).state_letters();}

    std::valarray<double> frequencies() const;

    unsigned n_states() const {return state_letters().size();}

    /// Get a transition probability matrix for time 't', averaging over models
    Matrix transition_p(double t) const;

    /// Get a transition probability matrix for time 't' and model 'm'
    Matrix transition_p(double t,int i, int m) const {return base_model(m).transition_p(t,i);}

    MultiModelObject();

    MultiModelObject(int n);

    virtual ~MultiModelObject() {}
  };


  Matrix frequency_matrix(const MultiModelObject&);
} 
#endif
