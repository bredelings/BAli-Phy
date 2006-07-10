#include "proposals.H"
#include "tree.H"
#include "probability.H"
#include "rng.H"
#include "util.H"

using std::valarray;

valarray<double> convert(const vector<double>& v1)
{
  valarray<double> v2(v1.size());
  for(int i=0;i<v1.size();i++)
    v2[i] = v1[i];
  return v2;
}

vector<double> convert(const valarray<double>& v1)
{
  vector<double> v2(v1.size());
  for(int i=0;i<v1.size();i++)
    v2[i] = v1[i];
  return v2;
}


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

double dirichlet_fiddle(vector<double>& p,double N)
{
  valarray<double> x = convert(p);
  double ratio = dirichlet_fiddle(x,N);
  p = convert(x);
  return ratio;
}

vector<double> read(const vector<double>& p, const vector<bool>& mask)
{
  vector<double> sub(n_elements(mask));

  for(int i=0,j=0;i<p.size();i++)
    if (mask[i])
      sub[j++] = p[i];

  return sub;
}

void write(const vector<double>& sub, vector<double>& p, const vector<bool>& mask)
{
  for(int i=0,j=0;i<p.size();i++)
    if (mask[i])
      p[i] = sub[j++];
}


double dirichlet_fiddle(vector<double>& p,double N, const vector<bool>& mask)
{
  assert(p.size() == mask.size());

  vector<double> sub = read(p,mask);

  double ratio = 1;

  if (sub.size()) {
    double s = sum(sub);

    scale(sub, 1.0/s);
    ratio = dirichlet_fiddle(sub,N);
    scale(sub, s);

    write(sub,p,mask);
  }

  return ratio;
}

double scale_gaussian(double& x, double sigma)
{
  double scale = exp( gaussian(0,sigma) );
  x *= scale;
  return scale;
}

double scale_gaussian2(vector<double>& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"scale_gaussian: expected one dimension, got "<<x.size()<<".";
  if (p.size() != 1) 
    throw myexception()<<"scale_gaussian: expected one parameter, got "<<p.size()<<".";

  return scale_gaussian(x[0],p[0]);
}


double shift_gaussian(vector<double>& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"shift_gaussian: expected one dimension, got "<<x.size()<<".";
  if (p.size() != 1) 
    throw myexception()<<"shift_gaussian: expected one parameter, got "<<p.size()<<".";

  double& f = x[0];
  double  sigma = p[0];

  f += gaussian(0,sigma);

  return 1.0;
}

double shift_laplace(vector<double>& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"shift_laplace: expected one dimension, got "<<x.size()<<".";

  if (p.size() != 1) 
    throw myexception()<<"shift_laplace: expected one parameter, got "<<p.size()<<".";

  double& f = x[0];
  double  s = p[0];

  f += laplace(0,s);

  return 1.0;
}

double shift_cauchy(vector<double>& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"shift_cauchy: expected one dimension, got "<<x.size()<<".";
  if (p.size() != 1) 
    throw myexception()<<"shift_cauchy: expected one parameter, got "<<p.size()<<".";

  double& f = x[0];
  double  s = p[0];

  f += cauchy(0,s);

  return 1.0;
}

double shift_delta(vector<double>& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"shift_delta: expected one dimension, got "<<x.size()<<".";
  if (p.size() != 1) 
    throw myexception()<<"shift_delta: expected one parameter, got "<<p.size()<<".";

  double& lambda_O = x[0];
  double  sigma = p[0];

  double pdel =  lambda_O-logdiff(0,lambda_O);
  double rate =  log(-logdiff(0,pdel));

  rate        += gaussian(0,sigma);
  pdel        =  logdiff(0,-exp(rate));
  lambda_O    =  pdel - logsum(0,pdel);

  return 1;
}

double shift_epsilon(vector<double>& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"shift_epsilon: expected one dimension, got "<<x.size()<<".";
  if (p.size() != 1) 
    throw myexception()<<"shift_epsilon: expected one parameter, got "<<p.size()<<".";

  double& lambda_E = x[0];
  double  sigma = p[0];

  double E_length = lambda_E - logdiff(0,lambda_E);
  E_length += gaussian(0,sigma);
  lambda_E = E_length - logsum(0,E_length);

  return 1;
}


double dirichlet_proposal(std::vector<double>& x,const std::vector<double>& p)
{
  if (p.size() != 1) 
    throw myexception()<<"dirichlet_proposal: expected one parameter, got "<<p.size()<<".";

  double N = p[0];

  if (N <= 0)
    throw myexception()<<"dirichlet_proposal: parameter N <= 0!";

  double s = sum(x);

  scale(x, 1.0/s);
  double ratio = dirichlet_fiddle(x,N*x.size());
  scale(x, s);

  return ratio;
}

/*
double sorted_proposal(std::valarray<double>& x,const std::vector<double>& p)
{
    // log-laplace fiddle the omega parameters
    for(int i=0;i<;i++)
      if (not fixed(i+fraction.size())) {
	double scale = shift_laplace(0,0.1);
	ratio *= exp(scale);
	double w = log(omega(i)) + scale;

	double max = 0;
	double min = 0;

	int wmin = any_set(fixed_, fraction.size() + i - 1, fraction.size()-1);
	if (wmin != -1) {
	  wmin -= fraction.size();
	  min = log(omega(wmin));
	}
	    
	int wmax = any_set(fixed_, fraction.size() + i + 1, 2*fraction.size());
	if (wmax != -1) {
	  wmax -= fraction.size();
	  max = log(omega(wmax));
	}

	if (wmin != -1 and wmax != -1)
	  w = wrap(w,min,max);
	else if (wmin != -1) 
	  w = reflect_left(w,min);
	else if (wmax != -1) 
	  w = reflect_right(w,max);

	super_parameters_[i+fraction.size()] = exp(w);
      }

    // we really should SORT the parameters now...
    std::sort(super_parameters_.begin()+fraction.size(),
	      super_parameters_.begin()+2*fraction.size());
}
*/

/*
  double MultiFrequencyModel::super_fiddle(int) 
  {
    // FIXME - ??? Does this still work after modifying dirichlet_fiddle?
    const double N = 10;

    // get factor by which to modify bin frequencies
    valarray<double> C(fraction.size());
    for(int m=0;m<fraction.size();m++)
      C[m] = exp(gaussian(0,0.1));

    int n1 =(int)( myrandomf()*Alphabet().size());
    int n2 =(int)( myrandomf()*Alphabet().size());

    double ratio = 1;

    for(int l=0;l<Alphabet().size();l++) 
    {
      valarray<double> a = get_a(l);
      a *= C;
      a /= a.sum();
      if (l==n1 or l==n2)
	ratio *= ::dirichlet_fiddle(a,N);
      set_a(l,a);
    }

    read();
    recalc();

    return ratio;
  }

*/

double less_than::operator()(std::vector<double>& x,const std::vector<double>& p) const
{
  double ratio = (*proposal)(x,p);
  for(int i=0;i<x.size();i++) 
    x[i] = reflect_less_than(x[i],max);
  return ratio;
}

less_than::less_than(double m, const Proposal_Fn& P)
  :max(m),
   proposal(P)
{ }

double more_than::operator()(std::vector<double>& x,const std::vector<double>& p) const
{
  double ratio = (*proposal)(x,p);
  for(int i=0;i<x.size();i++) 
    x[i] = reflect_more_than(x[i],min);
  return ratio;
}

more_than::more_than(double m, const Proposal_Fn& P)
  :min(m),
   proposal(P)
{ }

double between::operator()(std::vector<double>& x,const std::vector<double>& p) const
{
  double ratio = (*proposal)(x,p);
  for(int i=0;i<x.size();i++)
    x[i] = wrap(x[i],min,max);
  return ratio;
}

between::between(double m1, double m2, const Proposal_Fn& P)
  :min(m1), max(m2),
   proposal(P)
{ }


double log_scaled::operator()(std::vector<double>& x,const std::vector<double>& p) const
{
  if (x.size() != 1) 
    throw myexception()<<"log_scaled: expected one dimension, got "<<x.size()<<".";
  if (x[0] < 0.0) 
    throw myexception()<<"log_scaled: x[0] is negative!";
  if (x[0] == 0.0) 
    throw myexception()<<"log_scaled: x[0] is zero!";

  double x1 = x[0];

  x[0] = log(x[0]);
  double r = (*proposal)(x,p);
  x[0] = wrap<double>(x[0],-500,500);
  x[0] = exp(x[0]);

  double x2 = x[0];

  return r * x2/x1;
}

log_scaled::log_scaled(const Proposal_Fn& P)
  :proposal(P)
{ }


double LOD_scaled::operator()(std::vector<double>& x,const std::vector<double>& p) const
{
  if (x.size() != 1) 
    throw myexception()<<"LOD_scaled: expected one dimension, got "<<x.size()<<".";
  if (x[0] < 0.0) 
    throw myexception()<<"LOD_scaled: x[0] is negative!";
  if (x[0] == 0.0) 
    throw myexception()<<"LOD_scaled: x[0] is zero!";

  double x1 = x[0];

  x[0] = log(x[0]/(1-x[0]));
  double r = (*proposal)(x,p);
  x[0] = exp(x[0])/(1+exp(x[0]));

  double x2 = x[0];

  return r * x2*(1.0-x2)/(x1*(1.0-x1));
}

LOD_scaled::LOD_scaled(const Proposal_Fn& P)
  :proposal(P)
{ }


double Proposal2::operator()(alignment&, Parameters& P) const
{
  vector<double> parameters = P.parameters();
  vector<bool> fixed = P.fixed();

  // Quit if this move alters a 'fixed' parameter
  for(int i=0;i<indices.size();i++)
    if (fixed[indices[i]])
      return 1.0;

  // Load parameter values from names
  vector<double> p(pnames.size());
  for(int i=0;i<p.size();i++)
    p[i] = loadvalue(P.keys, pnames[i]);

  // read, alter, and write parameter values
  vector<double> x = P.parameters(indices);
  double ratio = (*proposal)(x,p);
  P.parameters(indices,x);

  return ratio;
}

Proposal2::Proposal2(const Proposal_Fn& p,const std::string& s, const std::vector<string>& v,
	  const Parameters& P)
  :proposal(p),
   pnames(v)
{
  int index = find_parameter(P,s);
  if (index == -1)
    throw myexception()<<"Model has no parameter called '"<<s<<"' - can't create proposal for it.";
  indices.push_back(index);
}


Proposal2::Proposal2(const Proposal_Fn& p,const vector<std::string>& s, const std::vector<string>& v,
	  const Parameters& P)
  :proposal(p),
   pnames(v)
{
  for(int i=0;i<s.size();i++) {
    int index = find_parameter(P,s[i]);
    if (index == -1)
      throw myexception()<<"Model has no parameter called '"<<s[i]<<"' - can't create proposal for it.";
    if (not P.fixed(index))
      indices.push_back(index);
  }
  if (not indices.size())
    throw myexception()<<"Parameters are all fixed - bad proposal!";
}

