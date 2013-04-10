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
#include "tree/tree.H"
#include "probability/probability.H"
#include "rng.H"
#include "util.H"

using std::valarray;
using std::vector;
using std::string;

using boost::dynamic_pointer_cast;

valarray<double> convert_to_valarray(const vector<Double>& v1)
{
  valarray<double> v2(v1.size());
  for(int i=0;i<v1.size();i++)
    v2[i] = v1[i];
  return v2;
}

valarray<double> convert_to_valarray(const vector< object_ref >& v1)
{
  valarray<double> v2(v1.size());
  for(int i=0;i<v1.size();i++)
  {
    object_ptr<const Double> D = dynamic_pointer_cast<const Double>(v1[i]);
    v2[i] = *D;
  }
  return v2;
}

vector<Double> convert_to_Double_vector(const valarray<double>& v1)
{
  vector<Double> v2(v1.size());
  for(int i=0;i<v1.size();i++)
    v2[i] = v1[i];
  return v2;
}

template<typename T>
vector< object_ref > convert_to_object_vector(const vector<T>& v1)
{
  vector< object_ref > v2(v1.size());
  for(int i=0;i<v1.size();i++)
    v2[i] = object_ref(v1[i].clone());
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
  valarray<double> x = convert_to_valarray(p);
  double ratio = dirichlet_fiddle(x,N);
  p = convert_to_Double_vector(x);
  return ratio;
}

double dirichlet_fiddle(vector< object_ref >& p,double N)
{
  valarray<double> x = convert_to_valarray(p);
  double ratio = dirichlet_fiddle(x,N);
  p = convert_to_object_vector( convert_to_Double_vector(x) );
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

double scale_gaussian2(vector< object_ref >& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"scale_gaussian: expected one dimension, got "<<x.size()<<".";
  if (p.size() != 1) 
    throw myexception()<<"scale_gaussian: expected one parameter, got "<<p.size()<<".";

  Double x0 = *dynamic_pointer_cast<const Double>(x[0]);

  double ratio = scale_gaussian(x0,p[0]);

  x[0] = x0;

  return ratio;
}


double shift_gaussian(vector< object_ref >& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"shift_gaussian: expected one dimension, got "<<x.size()<<".";
  if (p.size() != 1) 
    throw myexception()<<"shift_gaussian: expected one parameter, got "<<p.size()<<".";

  Double x0 = *dynamic_pointer_cast<const Double>(x[0]);
  double sigma = p[0];

  x0 += gaussian(0,sigma);

  x[0] = x0;

  return 1.0;
}

double shift_laplace(vector< object_ref >& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"shift_laplace: expected one dimension, got "<<x.size()<<".";

  if (p.size() != 1) 
    throw myexception()<<"shift_laplace: expected one parameter, got "<<p.size()<<".";

  Double x0 = *dynamic_pointer_cast<const Double>(x[0]);
  double  s = p[0];

  x0 += laplace(0,s);

  x[0] = x0;

  return 1.0;
}

double shift_cauchy(vector< object_ref >& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"shift_cauchy: expected one dimension, got "<<x.size()<<".";
  if (p.size() != 1) 
    throw myexception()<<"shift_cauchy: expected one parameter, got "<<p.size()<<".";

  Double x0 = *dynamic_pointer_cast<const Double>(x[0]);
  double  s = p[0];

  x0 += cauchy(0,s);

  x[0] = x0;

  return 1.0;
}

double shift_delta(vector< object_ref >& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"shift_delta: expected one dimension, got "<<x.size()<<".";
  if (p.size() != 1) 
    throw myexception()<<"shift_delta: expected one parameter, got "<<p.size()<<".";

  Double lambda_O = *dynamic_pointer_cast<const Double>(x[0]);
  double  sigma = p[0];

  double pdel =  lambda_O-logdiff(0, lambda_O);
  double rate =  log(-logdiff(0,pdel));

  rate        += gaussian(0,sigma);
  pdel        =  logdiff(0,-exp(rate));
  lambda_O    =  pdel - logsum(0,pdel);

  x[0] = lambda_O;

  return 1;
}

double shift_epsilon(vector< object_ref >& x, const vector<double>& p)
{
  if (x.size() != 1) 
    throw myexception()<<"shift_epsilon: expected one dimension, got "<<x.size()<<".";
  if (p.size() != 1) 
    throw myexception()<<"shift_epsilon: expected one parameter, got "<<p.size()<<".";

  Double lambda_E = *dynamic_pointer_cast<const Double>(x[0]);
  double  sigma = p[0];

  double E_length = lambda_E - logdiff(0,lambda_E);
  E_length += cauchy(0,sigma);
  lambda_E = E_length - logsum(0,E_length);

  x[0] = lambda_E;

  return 1;
}


double bit_flip(vector< object_ref >& x, const vector<double>&)
{
  if (x.size() != 1) 
    throw myexception()<<"shift_epsilon: expected one dimension, got "<<x.size()<<".";
  //  if (p.size() != 1) 
  //    throw myexception()<<"shift_epsilon: expected one parameter, got "<<p.size()<<".";

  constructor B = *dynamic_pointer_cast<const constructor>(x[0]);

  if (B.f_name == "Prelude.True")
    B.f_name = "Prelude.False";
  else
    B.f_name = "Prelude.True";

  x[0] = B;

  return 1;
}


double discrete_uniform(vector< object_ref >& x, const vector<double>& v)
{
  assert(v.size() == 2);
  int l = (int)v[0];
  int u = (int)v[1];

  if (x.size() != 1) 
    throw myexception()<<"discrete_uniform: expected one dimension, got "<<x.size()<<".";

  int i1 = *convert<const Int>(x[0]);
  
  int i2 = l+(u-l+1)*uniform();

  x[0] = Int(i2);

  return 1;
}


double dirichlet_proposal(std::vector< object_ref >& x,const std::vector<double>& p)
{
  if (p.size() != 1) 
    throw myexception()<<"dirichlet_proposal: expected one parameter, got "<<p.size()<<".";

  double N = p[0];

  if (N <= 0)
    throw myexception()<<"dirichlet_proposal: parameter N <= 0! (N = "<<N<<", x.size() == "<<x.size()<<")";

  valarray<double> x2 = convert_to_valarray(x);

  double s = x2.sum();

  x2 /= s;
  double ratio = dirichlet_fiddle(x2, N*x.size());
  x2 *= s;

  x = convert_to_object_vector( convert_to_Double_vector(x2));

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

double less_than::operator()(std::vector< object_ref >& x,const std::vector<double>& p) const
{
  double ratio = (*proposal)(x,p);
  for(int i=0;i<x.size();i++) 
  {
    Double X = *dynamic_pointer_cast<const Double>(x[i]);
    X = reflect_less_than(double(X),max);
    x[i] = X;
  }
  return ratio;
}

less_than::less_than(double m, const Proposal_Fn& P)
  :max(m),
   proposal(P)
{ }

double more_than::operator()(std::vector< object_ref >& x,const std::vector<double>& p) const
{
  double ratio = (*proposal)(x,p);
  for(int i=0;i<x.size();i++) 
  {
    Double X = *dynamic_pointer_cast<const Double>(x[i]);
    X = reflect_more_than(double(X),min);
    x[i] = X;
  }
  return ratio;
}

more_than::more_than(double m, const Proposal_Fn& P)
  :min(m),
   proposal(P)
{ }

double Between::operator()(std::vector< object_ref >& x,const std::vector<double>& p) const
{
  double ratio = (*proposal)(x,p);
  for(int i=0;i<x.size();i++)
  {
    Double X = *dynamic_pointer_cast<const Double>(x[i]);
    X = wrap(double(X),min,max);
    x[i] = X;
  }
  return ratio;
}

Between::Between(double m1, double m2, const Proposal_Fn& P)
  :min(m1), max(m2),
   proposal(P)
{ }


double reflect(const Bounds<double>& b, double x)
{
  if (b.has_lower_bound and b.has_upper_bound)
    return wrap<double>(x, b.lower_bound, b.upper_bound);
  else if (b.has_lower_bound)
    return reflect_more_than(x, b.lower_bound);
  else if (b.has_upper_bound)
    return reflect_less_than(x, b.upper_bound);
  else
    return x;
}

double Reflect::operator()(std::vector< object_ref >& x,const std::vector<double>& p) const
{
  double ratio = (*proposal)(x,p);
  for(int i=0;i<x.size();i++)
  {
    double X = *dynamic_pointer_cast<const Double>(x[i]);
    x[i] = Double ( reflect(bounds, X ) );
  }
  return ratio;
}

Reflect::Reflect(const Bounds<double>& b, const Proposal_Fn& P)
  :bounds(b),
   proposal(P)
{ }


double log_scaled::operator()(std::vector< object_ref >& x,const std::vector<double>& p) const
{
  if (x.size() != 1) 
    throw myexception()<<"log_scaled: expected one dimension, got "<<x.size()<<".";

  double x1 = *dynamic_pointer_cast<const Double>(x[0]);

  if (x1 < 0.0) 
    throw myexception()<<"log_scaled: x[0] is negative!";
  if (x1 == 0.0) 
    throw myexception()<<"log_scaled: x[0] is zero!";

  // log-transform x[0], but not x1
  x[0] = Double( log(x1) );

  // perform the proposal on the log-scale
  double r = (*proposal)(x,p);

  // inverse-transform
  double x2 = *dynamic_pointer_cast<const Double>(x[0]);
  x2 = wrap<double>(x2,-500,500);
  x2 = exp(x2);

  x[0] = Double( x2 );

  // return the scaled proposal ratio
  return r * x2/x1;
}

log_scaled::log_scaled(const Proposal_Fn& P)
  :proposal(P)
{ }


double LOD_scaled::operator()(std::vector< object_ref >& x,const std::vector<double>& p) const
{
  if (x.size() != 1) 
    throw myexception()<<"LOD_scaled: expected one dimension, got "<<x.size()<<".";

  double x1 = *dynamic_pointer_cast<const Double>(x[0]);

  if (x1 < 0.0) 
    throw myexception()<<"LOD_scaled: x[0] is negative!";
  if (x1 == 0.0) 
    throw myexception()<<"LOD_scaled: x[0] is zero!";

  // LOD-transform x[0], but not x1
  x[0] = Double( log(x1/(1-x1)) );

  // perform the proposal on the LOD-scale
  double r = (*proposal)(x,p);

  // inverse-transform
  double x2 = *dynamic_pointer_cast<const Double>(x[0]);
  x2 = exp(x2)/(1+exp(x2));

  x[0] = Double( x2 );

  // return the scaled proposal ratio
  return r * x2*(1.0-x2)/(x1*(1.0-x1));
}

LOD_scaled::LOD_scaled(const Proposal_Fn& P)
  :proposal(P)
{ }


double sorted::operator()(std::vector< object_ref >& x,const std::vector<double>& p) const
{
  double ratio = (*proposal)(x,p);

  vector<double> x2(x.size());
  for(int i=0;i<x2.size();i++)
  {
    object_ptr<const Double> D = dynamic_pointer_cast<const Double>(x[i]);
    x2[i] = *D;
  }

  vector<int> order = iota<int>( x.size() );

  std::sort(order.begin(), order.end(), sequence_order<double>(x2));

  x = apply_indices(x, order);

  return ratio;
}

sorted::sorted(const Proposal_Fn& P)
  :proposal(P)
{ }

double Proposal2::operator()(Probability_Model& P) const
{
  //  vector< object_ref > parameters = P.get_parameter_values();

  if (not indices.size())
    throw myexception()<<"Proposal2::operator() - No parameters to alter! (all parameters fixed?)";

  // Load parameter values from names
  vector<double> p(pnames.size());
  for(int i=0;i<p.size();i++)
    p[i] = P.lookup_key(pnames[i]);

  // read, alter, and write parameter values
  vector< object_ptr<const Object> > y = P.get_parameter_values(indices);
  vector< object_ref > x(y.size());
  for(int i=0;i<x.size();i++)
    x[i] = y[i];

  double ratio = (*proposal)(x,p);

  for(int i=0;i<y.size();i++)
    y[i] = x[i];
  P.set_parameter_values(indices,y);

  return ratio;
}

std::set<int> Proposal2::get_affected_parameters(const owned_ptr<Probability_Model>&) const
{
  std::set<int> affected_parameters;
  for(int i: indices)
    affected_parameters.insert(i);
  return affected_parameters;
}

Proposal2::Proposal2(const Proposal_Fn& p,const std::string& s, const std::vector<string>& v,
	  const Probability_Model& P)
  :proposal(p),
   pnames(v)
{
  int index = P.find_parameter(s);
  if (index == -1)
    throw myexception()<<"Model has no parameter called '"<<s<<"' - can't create proposal for it.";
  indices.push_back(index);
}


Proposal2::Proposal2(const Proposal_Fn& p,const vector<std::string>& s, const std::vector<string>& v,
	  const Probability_Model& P)
  :proposal(p),
   pnames(v)
{
  for(int i=0;i<s.size();i++) {
    int index = P.find_parameter(s[i]);
    if (index == -1)
      throw myexception()<<"Model has no parameter called '"<<s[i]<<"' - can't create proposal for it.";
    indices.push_back(index);
  }
}

double Proposal2M::operator()(Probability_Model& P) const
{
  if (not indices.size())
    throw myexception()<<"Proposal2M::operator() - No modifiables to alter!";

  // read, alter, and write parameter values
  vector< object_ref > y = P.get_modifiable_values(indices);
  vector< object_ref > x(y.size());
  for(int i=0;i<x.size();i++)
    x[i] = y[i];

  double ratio = (*proposal)(x,parameters);

  for(int i=0;i<y.size();i++)
    P.set_modifiable_value(indices[i], x[i]);

  return ratio;
}

std::set<int> Proposal2M::get_affected_parameters(const owned_ptr<Probability_Model>&) const
{
  return {};
}

Proposal2M::Proposal2M(const Proposal_Fn& p,int  s, const vector<double>& v)
:Proposal2M(p,vector<int>{s},v)
{ }


Proposal2M::Proposal2M(const Proposal_Fn& p,const vector<int>& s, const vector<double>& v)
:proposal(p),
  indices(s),
  parameters(v)
{ }


double move_scale_branch(Probability_Model& P)
{
  Parameters& PP = dynamic_cast<Parameters&>(P);

  int index = PP.find_parameter("lambdaScaleBranch");

  Int scale_branch = PP.get_parameter_value_as<Int>(index);

  assert( scale_branch != -1);

  if (P.contains_key("lambda_search_all"))
    scale_branch = uniform()*PP.T().n_branches();
  else
  {
    int attribute_index = PP.T().find_undirected_branch_attribute_index_by_name("lambda-scale-branch");  
    
    assert(attribute_index != -1);
    
    vector<int> branches;
    for(int b=0;b<PP.T().n_branches();b++)
    {
      boost::any value = PP.T().branch(b).undirected_attribute(attribute_index);
      if (not value.empty())
	branches.push_back(b);
    }
    
    int i = uniform()*branches.size();
    scale_branch = branches[i];
  }

  PP.set_parameter_value(index, object_ref(scale_branch));

  return 1.0;
}

double move_subst_type_branch(Probability_Model& P)
{
  Parameters& PP = dynamic_cast<Parameters&>(P);

  int which_branch = -1;
  int B = PP.T().n_branches();
  for(int b=0;b<B;b++)
  {
    int index = P.find_parameter("Main.branchCat" + convertToString(b+1));
    int cat = P.get_parameter_value_as<Int>(index);
    if (cat == 1)
    {
      assert(which_branch == -1);
      which_branch = b;
    }
  }

  if (which_branch != -1)
  {
    int new_branch = uniform()*(B-1);
    if (new_branch >= which_branch)
      new_branch++;

    int index1 = P.find_parameter("Main.branchCat" + convertToString(which_branch+1));
    int index2 = P.find_parameter("Main.branchCat" + convertToString(new_branch+1));

    P.set_parameter_value(index1, object_ref(Int(0)));
    P.set_parameter_value(index2, object_ref(Int(1)));
    std::cerr<<"Moved subst type 1 from branch "<<which_branch<<" to branch "<<new_branch<<"\n";
  }

  return 1.0;
}
