#include "proposals.H"
#include "tree.H"
#include "probability.H"
#include "rng.H"

using std::valarray;

valarray<double> sensible(valarray<double> n)
{
  for(int i=0;i<n.size();i++)
    if (n[i] < 1.0) n[i] = 1.0;
  return n;
}

double dirichlet_fiddle(valarray<double>& p2,double N)
{
  valarray<double> p1 = p2;
  p2 = dirichlet(sensible(p1*N));
  return dirichlet_pdf(p1,sensible(p2*N))/dirichlet_pdf(p2,sensible(p1*N));
}

valarray<double> read(const valarray<double>& p, const valarray<bool>& mask)
{
  valarray<double> sub(n_elements(mask));

  for(int i=0,j=0;i<p.size();i++)
    if (mask[i])
      sub[j++] = p[i];

  return sub;
}

void write(const valarray<double>& sub, valarray<double>& p, const valarray<bool>& mask)
{
  for(int i=0,j=0;i<p.size();i++)
    if (mask[i])
      p[i] = sub[j++];
}


double dirichlet_fiddle(valarray<double>& p,double N, const valarray<bool>& mask)
{
  assert(p.size() == mask.size());

  valarray<double> sub = read(p,mask);

  double ratio = 1;

  if (sub.size()) {
    double sum = sub.sum();

    sum /= sum;
    ratio = dirichlet_fiddle(sub,N);
    sub *= sum;

    write(sub,p,mask);
  }

  return ratio;
}


double dirichlet_fiddle_old(valarray<double>& p2,double sigma)
{
  valarray<double> p1=p2;
  for(int i=0;i<p2.size();i++)
    p2[i] *= exp(gaussian(0,sigma));

  p2 /= p2.sum();

  return 1.0;
}

double dirichlet_fiddle_old(valarray<double>& p2,double sigma,const valarray<bool>& mask) 
{
  valarray<double> p1=p2;
  p2 /= p2.sum();

  double total1=0;
  double total2=0;
  for(int i=0;i<p2.size();i++)
    if (mask[i]) {
      total1 += p2[i];
      p2[i] *= exp(gaussian(0,sigma));
      total2 += p2[i];
    }

  double factor = total1/total2;

  for(int i=0;i<p2.size();i++)
    if (mask[i]) 
      p2[i] *= factor;

  return 1.0;
}

double scale_gaussian(double& x, double sigma)
{
  double scale = exp( gaussian(0,sigma) );
  x *= scale;
  return scale;
}

template <typename T>
valarray<T> read(const vector<T>& v,const vector<int>& indices)
{
  valarray<T> values(indices.size());
  for(int i=0;i<indices.size();i++)
    values[i] = v[indices[i]];
  return values;
}


template <typename T>
valarray<T> write(vector<T>& v,const vector<int>& indices,const valarray<T>& values)
{
  assert(indices.size() == values.size());
  for(int i=0;i<indices.size();i++)
    v[indices[i]] = values[i];
  return values;
}

double frequency_proposal(alignment& A, Parameters& P)
{
  const SequenceTree& T = P.T;
  const alphabet& a = P.SModel().Alphabet();

  double N_guess = A.length() * a.size() * (2.0 - exp(-T.n_branches() * P.branch_mean()));
  double N = loadvalue(P.keys,"pi_dirichlet_N",N_guess);

  vector<int> indices;
  for(int i=0;i<P.parameters().size();i++) 
  {
    string name = P.parameter_name(i);
    if (name.size() > 2 and name.substr(0,2) == "pi")
      indices.push_back(i);
  }

  vector<double> parameters = P.parameters();
  valarray<double> values = read(parameters,indices);
  valarray<bool> fixed = read(P.fixed(),indices);

  valarray<double> values1 = values;
  double ratio  = dirichlet_fiddle(values,N,not fixed);

  write(parameters,indices,values);
  P.parameters(parameters);

  return ratio;
}

