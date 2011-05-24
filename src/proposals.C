/*
   Copyright (C) 2006-2007 Benjamin Redelings

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

#include "proposals.H"
#include "tree.H"
#include "probability.H"
#include "rng.H"
#include "util.H"

using std::valarray;
using std::vector;
using std::string;

valarray<double> convert(const vector<Double>& v1)
{
  valarray<double> v2(v1.size());
  for(int i=0;i<v1.size();i++)
    v2[i] = v1[i];
  return v2;
}

vector<Double> convert(const valarray<double>& v1)
{
  vector<Double> v2(v1.size());
  for(int i=0;i<v1.size();i++)
    v2[i] = v1[i];
  return v2;
}


double dirichlet_fiddle(valarray<double>& p2,double N)
{
  valarray<double> p1 = p2;
  p2 = dirichlet(safe_count(p1*N));
  return dirichlet_pdf(p1,safe_count(p2*N))/dirichlet_pdf(p2,safe_count(p1*N));
}

double dirichlet_fiddle(vector<Double>& p,double N)
{
  valarray<double> x = convert(p);
  double ratio = dirichlet_fiddle(x,N);
  p = convert(x);
  return ratio;
}

double scale_gaussian(double& x, double sigma)
{
  double scale = exp( gaussian(0,sigma) );
  x *= scale;
  return scale;
}

double scale_gaussian(Double& x, double sigma)
{
  double scale = exp( gaussian(0,sigma) );
  x *= scale;
  return scale;
}

double scale_gaussian2(vector<Double>& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"scale_gaussian: expected one dimension, got "<<x.size()<<".";
  if (p.size() != 1) 
    throw myexception()<<"scale_gaussian: expected one parameter, got "<<p.size()<<".";

  return scale_gaussian(x[0],p[0]);
}


double shift_gaussian(vector<Double>& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"shift_gaussian: expected one dimension, got "<<x.size()<<".";
  if (p.size() != 1) 
    throw myexception()<<"shift_gaussian: expected one parameter, got "<<p.size()<<".";

  Double& f = x[0];
  double  sigma = p[0];

  f += gaussian(0,sigma);

  return 1.0;
}

double shift_laplace(vector<Double>& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"shift_laplace: expected one dimension, got "<<x.size()<<".";

  if (p.size() != 1) 
    throw myexception()<<"shift_laplace: expected one parameter, got "<<p.size()<<".";

  Double& f = x[0];
  double  s = p[0];

  f += laplace(0,s);

  return 1.0;
}

double shift_cauchy(vector<Double>& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"shift_cauchy: expected one dimension, got "<<x.size()<<".";
  if (p.size() != 1) 
    throw myexception()<<"shift_cauchy: expected one parameter, got "<<p.size()<<".";

  Double& f = x[0];
  double  s = p[0];

  f += cauchy(0,s);

  return 1.0;
}

double shift_delta(vector<Double>& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"shift_delta: expected one dimension, got "<<x.size()<<".";
  if (p.size() != 1) 
    throw myexception()<<"shift_delta: expected one parameter, got "<<p.size()<<".";

  Double& lambda_O = x[0];
  double  sigma = p[0];

  double pdel =  lambda_O-logdiff(0,lambda_O);
  double rate =  log(-logdiff(0,pdel));

  rate        += gaussian(0,sigma);
  pdel        =  logdiff(0,-exp(rate));
  lambda_O    =  pdel - logsum(0,pdel);

  return 1;
}

double shift_epsilon(vector<Double>& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"shift_epsilon: expected one dimension, got "<<x.size()<<".";
  if (p.size() != 1) 
    throw myexception()<<"shift_epsilon: expected one parameter, got "<<p.size()<<".";

  Double& lambda_E = x[0];
  double  sigma = p[0];

  double E_length = lambda_E - logdiff(0,lambda_E);
  E_length += cauchy(0,sigma);
  lambda_E = E_length - logsum(0,E_length);

  return 1;
}


double bit_flip(vector<Double>& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"shift_epsilon: expected one dimension, got "<<x.size()<<".";
  //  if (p.size() != 1) 
  //    throw myexception()<<"shift_epsilon: expected one parameter, got "<<p.size()<<".";

  Double& B = x[0];
  B = 1.0 - B;

  if (B < 0.5) B = 0;
  if (B > 0.5) B = 1;

  return 1;
}


double dirichlet_proposal(std::vector<Double>& x,const std::vector<double>& p)
{
  if (p.size() != 1) 
    throw myexception()<<"dirichlet_proposal: expected one parameter, got "<<p.size()<<".";

  double N = p[0];

  if (N <= 0)
    throw myexception()<<"dirichlet_proposal: parameter N <= 0! (N = "<<N<<", x.size() == "<<x.size()<<")";

  double s = sum(x);

  scale(x, 1.0/s);
  double ratio = dirichlet_fiddle(x, N*x.size());
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

double less_than::operator()(std::vector<Double>& x,const std::vector<double>& p) const
{
  double ratio = (*proposal)(x,p);
  for(int i=0;i<x.size();i++) 
    x[i] = reflect_less_than(double(x[i]),max);
  return ratio;
}

less_than::less_than(double m, const Proposal_Fn& P)
  :max(m),
   proposal(P)
{ }

double more_than::operator()(std::vector<Double>& x,const std::vector<double>& p) const
{
  double ratio = (*proposal)(x,p);
  for(int i=0;i<x.size();i++) 
    x[i] = reflect_more_than(double(x[i]),min);
  return ratio;
}

more_than::more_than(double m, const Proposal_Fn& P)
  :min(m),
   proposal(P)
{ }

double Between::operator()(std::vector<Double>& x,const std::vector<double>& p) const
{
  double ratio = (*proposal)(x,p);
  for(int i=0;i<x.size();i++)
    x[i] = wrap(double(x[i]),min,max);
  return ratio;
}

Between::Between(double m1, double m2, const Proposal_Fn& P)
  :min(m1), max(m2),
   proposal(P)
{ }


double log_scaled::operator()(std::vector<Double>& x,const std::vector<double>& p) const
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


double LOD_scaled::operator()(std::vector<Double>& x,const std::vector<double>& p) const
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


double sorted::operator()(std::vector<Double>& x,const std::vector<double>& p) const
{
  double ratio = (*proposal)(x,p);

  std::sort(x.begin(),x.end());

  return ratio;
}

sorted::sorted(const Proposal_Fn& P)
  :proposal(P)
{ }

double Proposal2::operator()(Probability_Model& P) const
{
  //  vector<Double> parameters = P.get_parameter_values();

  if (not indices.size())
    throw myexception()<<"Proposal2::operator() - No parameters to alter! (all parameters fixed?)";

  // Quit if this move alters a 'fixed' parameter
  for(int i=0;i<indices.size();i++)
    if (P.is_fixed(indices[i]))
      return 1.0;

  // Load parameter values from names
  vector<double> p(pnames.size());
  for(int i=0;i<p.size();i++)
    p[i] = loadvalue(P.keys, pnames[i]);

  // read, alter, and write parameter values
  vector<Double> x = P.get_parameter_values_as<Double>(indices);
  double ratio = (*proposal)(x,p);
  P.set_parameter_values(indices,x);

  return ratio;
}

Proposal2::Proposal2(const Proposal_Fn& p,const std::string& s, const std::vector<string>& v,
	  const Probability_Model& P)
  :proposal(p),
   pnames(v)
{
  int index = find_parameter(P,s);
  if (index == -1)
    throw myexception()<<"Model has no parameter called '"<<s<<"' - can't create proposal for it.";
  if (not P.is_fixed(index))
    indices.push_back(index);
}


Proposal2::Proposal2(const Proposal_Fn& p,const vector<std::string>& s, const std::vector<string>& v,
	  const Probability_Model& P)
  :proposal(p),
   pnames(v)
{
  for(int i=0;i<s.size();i++) {
    int index = find_parameter(P,s[i]);
    if (index == -1)
      throw myexception()<<"Model has no parameter called '"<<s[i]<<"' - can't create proposal for it.";
    if (not P.is_fixed(index))
      indices.push_back(index);
  }
}

