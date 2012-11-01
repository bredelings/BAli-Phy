#include "smodel/objects.H"
#include "smodel/operations.H"

#include "math/exponential.H"

using std::vector;
using std::valarray;
using std::string;
using boost::shared_ptr;

namespace substitution
{
  SModelObject::SModelObject(const alphabet& A)
    :a(ptr(A)),
     state_letters_(A.size())
  { 
    for(int i=0;i<n_states();i++)
      state_letters_[i] = i;
  }

  SModelObject::SModelObject(const alphabet& A, int n)
    :a(ptr(A)),
     state_letters_(n)
  { 
    if (n%A.size() != 0)
      throw myexception()<<"Cannot construct a model with "<<A.size()<<" letters and "<<n<<" states!";

    for(int i=0;i<n_states();i++)
      state_letters_[i] = i%n_letters();
  }


  AlphabetExchangeModelObject::AlphabetExchangeModelObject(const alphabet& a)
    :SModelObject(a),ExchangeModelObject(a.size())
  {
  }

  AlphabetExchangeModelObject::AlphabetExchangeModelObject(const alphabet& a, int n)
    :SModelObject(a,n),ExchangeModelObject(n)
  {
  }

  ReversibleAdditiveObject::ReversibleAdditiveObject(const alphabet& a)
    :SModelObject(a)
  { }

  ReversibleAdditiveObject::ReversibleAdditiveObject(const alphabet& a, int n)
    :SModelObject(a,n)
  { }


  std::valarray<double> ReversibleMarkovModelObject::frequencies() const {return get_varray<double>(pi);}

  ReversibleMarkovModelObject::ReversibleMarkovModelObject(const alphabet& A)
    :ReversibleAdditiveObject(A),
     Q(A.size(), A.size()),
     pi(A.size())
  { }

  ReversibleMarkovModelObject::ReversibleMarkovModelObject(const alphabet& A,int n)
    :ReversibleAdditiveObject(A,n),
     Q(A.size(), A.size()),
     pi(n)
  { }


  // Q(i,j) = S(i,j)*pi[j]   for i!=j
  // Q(i,i) = -sum_{i!=j} S(i,j)*pi[j]

  // We want to set S(i,i) so that Q(i,j) = S(i,j)*pi[j] for all i,j
  // Then Q = S*D, and we can easily compute the exponential
  // So, S(i,j) = Q(i,i)/pi[i]

  double ReversibleMarkovModelObject::rate() const 
  {
    const unsigned N = n_states();
    
    double scale=0;

    if (N == Alphabet().size()) 
    {
      for(int i=0;i<Q.size1();i++) 
	scale -= frequencies()[i]*Q(i,i);
    }
    else 
    {
      const vector<unsigned>& smap = state_letters();

      for(int s1=0;s1<N;s1++)
      {
	double temp = 0;
	for(int s2=0;s2<N;s2++)
	  if (smap[s1] != smap[s2])
	    temp += Q(s1,s2);

	scale += temp*frequencies()[s1];
      }
    }

    return scale/Alphabet().width();
  }

  Matrix ReversibleMarkovModelObject::transition_p(double t) const 
  {
    vector<double> pi2(n_states());
    const valarray<double> f = frequencies();
    assert(pi2.size() == f.size());
    for(int i=0;i<pi2.size();i++)
      pi2[i] = f[i];
    return exp(*Get_Eigensystem_Function(Q,pi), pi2,t);
  }

  //------------------------ F81 Model -------------------------//

  Matrix F81_Object::transition_p(double t) const
  {
    const unsigned N = n_states();

    Matrix E(N,N);

    const double exp_a_t = exp(-alpha_ * t);

    for(int i=0;i<N;i++)
      for(int j=0;j<N;j++)
	E(i,j) = pi[j] + (((i==j)?1.0:0.0) - pi[j])*exp_a_t;

    return E;
  }

  double F81_Object::rate() const
  {
    const unsigned N = n_states();

    double sum=0;
    for(int i=0;i<N;i++)
      sum += pi[i]*(1.0-pi[i]);

    return sum*alpha_;
  }

  F81_Object::F81_Object(const alphabet& a)
    :ReversibleMarkovModelObject(a),
     alpha_(1)
  {
    const int N = a.size();

    for(int i=0;i<N;i++)
      pi[i] = 1.0/N;

    for(int i=0;i<N;i++)
      for(int j=0;j<N;j++)
	Q(i,j) = (pi[j] - ((i==j)?1:0))*alpha_;
  }

  F81_Object::F81_Object(const alphabet& a, const valarray<double>& v)
    :ReversibleMarkovModelObject(a),
     alpha_(1)
  { 
    const int N = a.size();

    pi = get_vector<double>(v);

    for(int i=0;i<N;i++)
      for(int j=0;j<N;j++)
	Q(i,j) = (pi[j] - ((i==j)?1:0))*alpha_;
  }
}
