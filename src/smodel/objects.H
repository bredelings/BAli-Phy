#ifndef SMODEL_OBJECTS
#define SMODEL_OBJECTS

#include <vector>
#include <string>
#include <boost/shared_ptr.hpp>
#include <boost/numeric/ublas/symmetric.hpp>
#include "sequence/alphabet.H"
#include "math/eigenvalue.H"
#include "cached_value.H"
#include "cow-ptr.H"
#include <valarray>
#include "computation/expression.H"

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
    object_ptr<const alphabet> a;

  public:
    std::vector<unsigned> state_letters_;

    const std::vector<unsigned>& state_letters() const {return state_letters_;}

    unsigned n_states() const {return state_letters_.size();}

    const alphabet& Alphabet() const {return *a;}

    object_ptr<const alphabet> get_alphabet() const {return a;}

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

    int size() const {return S.size1();}

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

    virtual Matrix transition_p(double t) const = 0;

    virtual std::valarray<double> frequencies() const =0;

    ReversibleAdditiveObject(const alphabet& a);

    ReversibleAdditiveObject(const alphabet& a, int n);
  };

  class ReversibleMarkovModelObject: public ReversibleAdditiveObject
  {
  public:
    /// Rate matrix
    Matrix Q;

    std::vector<double> pi;

    virtual ReversibleMarkovModelObject* clone() const {return new ReversibleMarkovModelObject(*this);}

    std::valarray<double> frequencies() const;

    double rate() const;

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

    /// The transition probability matrix - which we can now compute
    Matrix transition_p(double t) const;

    F81_Object(const alphabet& a);
    F81_Object(const alphabet& a, const std::valarray<double>&);
  };
} 
#endif
