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
#include "computation/expression/expression.H"
#include "rng.H"
#include "util.H"
#include "dp/dp-matrix.H"

using std::valarray;
using std::vector;
using std::string;

valarray<double> convert_to_valarray(const vector< expression_ref >& v1)
{
    valarray<double> v2(v1.size());
    for(int i=0;i<v1.size();i++)
	v2[i] = v1[i].as_double();
    return v2;
}

log_double_t dirichlet_fiddle(valarray<double>& p2,double N)
{
    valarray<double> p1 = p2;
    p2 = dirichlet(safe_count(p1*N));
    return dirichlet_pdf(p1,safe_count(p2*N))/dirichlet_pdf(p2,safe_count(p1*N));
}

log_double_t dirichlet_fiddle(vector< expression_ref >& p,double N)
{
    valarray<double> x = convert_to_valarray(p);
    log_double_t ratio = dirichlet_fiddle(x,N);
    for(int i=0; i<x.size(); i++)
	p[i] = x[i];
    return ratio;
}

log_double_t scale_gaussian(double& x, double sigma)
{
    auto scale = exp_to<log_double_t>( gaussian(0,sigma) );
    x *= scale;
    return scale;
}

log_double_t scale_gaussian2(vector< expression_ref >& x, const vector<double>& p)
{
    if (x.size() != 1) 
	throw myexception()<<"scale_gaussian: expected one dimension, got "<<x.size()<<".";
    if (p.size() != 1) 
	throw myexception()<<"scale_gaussian: expected one parameter, got "<<p.size()<<".";

    double x0 = x[0].as_double();

    log_double_t ratio = scale_gaussian(x0,p[0]);

    x[0] = x0;

    return ratio;
}


log_double_t shift_gaussian(vector< expression_ref >& x, const vector<double>& p)
{
    if (x.size() != 1) 
	throw myexception()<<"shift_gaussian: expected one dimension, got "<<x.size()<<".";
    if (p.size() != 1) 
	throw myexception()<<"shift_gaussian: expected one parameter, got "<<p.size()<<".";

    double x0 = x[0].as_double();
    double sigma = p[0];

    x0 += gaussian(0,sigma);

    x[0] = x0;

    return 1.0;
}

log_double_t shift_laplace(vector< expression_ref >& x, const vector<double>& p)
{
    if (x.size() != 1) 
	throw myexception()<<"shift_laplace: expected one dimension, got "<<x.size()<<".";

    if (p.size() != 1) 
	throw myexception()<<"shift_laplace: expected one parameter, got "<<p.size()<<".";

    double x0 = x[0].as_double();
    double  s = p[0];

    x0 += laplace(0,s);

    x[0] = x0;

    return 1.0;
}

log_double_t shift_cauchy(vector< expression_ref >& x, const vector<double>& p)
{
    if (x.size() != 1) 
	throw myexception()<<"shift_cauchy: expected one dimension, got "<<x.size()<<".";
    if (p.size() != 1) 
	throw myexception()<<"shift_cauchy: expected one parameter, got "<<p.size()<<".";
  
    double x0 = x[0].as_double();
    double  s = p[0];

    x0 += cauchy(0,s);

    x[0] = x0;

    return 1.0;
}

log_double_t shift_delta(vector< expression_ref >& x, const vector<double>& p)
{
    if (x.size() != 1) 
	throw myexception()<<"shift_delta: expected one dimension, got "<<x.size()<<".";
    if (p.size() != 1) 
	throw myexception()<<"shift_delta: expected one parameter, got "<<p.size()<<".";

    double lambda_O = x[0].as_double();
    double  sigma = p[0];

    double pdel =  lambda_O-logdiff(0, lambda_O);
    double rate =  log(-logdiff(0,pdel));

    rate        += gaussian(0,sigma);
    pdel        =  logdiff(0,-exp(rate));
    lambda_O    =  pdel - logsum(0,pdel);

    x[0] = lambda_O;

    return 1;
}

log_double_t shift_epsilon(vector< expression_ref >& x, const vector<double>& p)
{
    if (x.size() != 1) 
	throw myexception()<<"shift_epsilon: expected one dimension, got "<<x.size()<<".";
    if (p.size() != 1) 
	throw myexception()<<"shift_epsilon: expected one parameter, got "<<p.size()<<".";

    double lambda_E = x[0].as_double();
    double  sigma = p[0];

    double E_length = lambda_E - logdiff(0,lambda_E);
    E_length += cauchy(0,sigma);
    lambda_E = E_length - logsum(0,E_length);

    x[0] = lambda_E;

    return 1;
}


log_double_t bit_flip(vector< expression_ref >& x, const vector<double>&)
{
    if (x.size() != 1) 
	throw myexception()<<"shift_epsilon: expected one dimension, got "<<x.size()<<".";
    //  if (p.size() != 1) 
    //    throw myexception()<<"shift_epsilon: expected one parameter, got "<<p.size()<<".";

    constructor B = x[0].head().as_<constructor>();

    if (is_bool_true(B))
	x[0] = bool_false;
    else
	x[0] = bool_true;

    return 1;
}


log_double_t discrete_uniform(vector< expression_ref >& x, const vector<double>& v)
{
    assert(v.size() == 2);
    int l = (int)v[0];
    int u = (int)v[1];

    if (x.size() != 1) 
	throw myexception()<<"discrete_uniform: expected one dimension, got "<<x.size()<<".";

    //  int i1 = x[0].as_int();
  
    int i2 = l+(u-l+1)*uniform();

    x[0] = i2;

    return 1;
}


log_double_t dirichlet_proposal(std::vector< expression_ref >& x,const std::vector<double>& p)
{
    if (p.size() != 1) 
	throw myexception()<<"dirichlet_proposal: expected one parameter, got "<<p.size()<<".";

    double N = p[0];

    if (N <= 0)
	throw myexception()<<"dirichlet_proposal: parameter N <= 0! (N = "<<N<<", x.size() == "<<x.size()<<")";

    valarray<double> x2 = convert_to_valarray(x);

    double s = x2.sum();

    x2 /= s;
    log_double_t ratio = dirichlet_fiddle(x2, N*x.size());
    x2 *= s;

    for(int i=0;i<x.size();i++)
	x[i] = x2[i];

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

  int n1 =(int)( uniform()*Alphabet().size());
  int n2 =(int)( uniform()*Alphabet().size());

  log_double_t ratio = 1;

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

log_double_t less_than::operator()(std::vector< expression_ref >& x,const std::vector<double>& p) const
{
    auto ratio = proposal(x,p);
    for(int i=0;i<x.size();i++) 
    {
	double X = x[i].as_double();
	X = reflect_less_than(double(X),max);
	x[i] = X;
    }
    return ratio;
}

less_than::less_than(double m, const proposal_fn& P)
    :max(m),
     proposal(P)
{ }

log_double_t more_than::operator()(std::vector< expression_ref >& x,const std::vector<double>& p) const
{
    auto ratio = proposal(x,p);
    for(int i=0;i<x.size();i++) 
    {
	double X = x[i].as_double();
	X = reflect_more_than(double(X),min);
	x[i] = X;
    }
    return ratio;
}

more_than::more_than(double m, const proposal_fn& P)
    :min(m),
     proposal(P)
{ }

log_double_t Between::operator()(std::vector< expression_ref >& x,const std::vector<double>& p) const
{
    auto ratio = proposal(x,p);
    for(int i=0;i<x.size();i++)
    {
	double X = x[i].as_double();
	X = wrap(double(X),min,max);
	x[i] = X;
    }
    return ratio;
}

Between::Between(double m1, double m2, const proposal_fn& P)
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

log_double_t Reflect::operator()(std::vector< expression_ref >& x,const std::vector<double>& p) const
{
    auto ratio = proposal(x,p);
    for(int i=0;i<x.size();i++)
    {
	double X = x[i].as_double();
	x[i] = reflect(bounds, X );
    }
    return ratio;
}

Reflect::Reflect(const Bounds<double>& b, const proposal_fn& P)
    :bounds(b),
     proposal(P)
{ }


log_double_t log_scaled::operator()(std::vector< expression_ref >& x,const std::vector<double>& p) const
{
    if (x.size() != 1) 
	throw myexception()<<"log_scaled: expected one dimension, got "<<x.size()<<".";

    double x1 = x[0].as_double();

    if (x1 < 0.0) 
	throw myexception()<<"log_scaled: x[0] is negative!";
    if (x1 == 0.0) 
	throw myexception()<<"log_scaled: x[0] is zero!";

    // log-transform x[0], but not x1
    x[0] = log(x1);

    // perform the proposal on the log-scale
    auto r = proposal(x,p);

    // inverse-transform
    double x2 = x[0].as_double();
    x2 = wrap<double>(x2,-500,500);
    x2 = exp(x2);

    x[0] = x2;

    // return the scaled proposal ratio
    return r * x2/x1;
}

log_scaled::log_scaled(const proposal_fn& P)
    :proposal(P)
{ }


log_double_t LOD_scaled::operator()(std::vector< expression_ref >& x,const std::vector<double>& p) const
{
    if (x.size() != 1) 
	throw myexception()<<"LOD_scaled: expected one dimension, got "<<x.size()<<".";

    double x1 = x[0].as_double();

    if (x1 < 0.0) 
	throw myexception()<<"LOD_scaled: x[0] is negative!";
    if (x1 == 0.0) 
	throw myexception()<<"LOD_scaled: x[0] is zero!";

    // LOD-transform x[0], but not x1
    x[0] = log(x1/(1-x1));

    // perform the proposal on the LOD-scale
    auto r = proposal(x,p);

    // inverse-transform
    double x2 = x[0].as_double();
    x2 = exp(x2)/(1+exp(x2));

    x[0] = x2;

    // return the scaled proposal ratio
    return r * x2*(1.0-x2)/(x1*(1.0-x1));
}

LOD_scaled::LOD_scaled(const proposal_fn& P)
    :proposal(P)
{ }


log_double_t sorted::operator()(std::vector< expression_ref >& x,const std::vector<double>& p) const
{
    auto ratio = proposal(x,p);

    vector<double> x2(x.size());
    for(int i=0;i<x2.size();i++)
	x2[i] = x[i].as_double();

    vector<int> order = iota<int>( x.size() );

    std::sort(order.begin(), order.end(), sequence_order<double>(x2));

    x = apply_indices(x, order);

    return ratio;
}

sorted::sorted(const proposal_fn& P)
    :proposal(P)
{ }

log_double_t Proposal2::operator()(Model& P) const
{
    //  vector< expression_ref > parameters = P.get_parameter_values();

    if (not indices.size())
	throw myexception()<<"Proposal2::operator() - No parameters to alter! (all parameters fixed?)";

    // Load parameter values from names
    vector<double> p(pnames.size());
    for(int i=0;i<p.size();i++)
	p[i] = P.lookup_key(pnames[i]);

    // read, alter, and write parameter values
    vector< expression_ref > y = P.get_parameter_values(indices);
    vector< expression_ref > x(y.size());
    for(int i=0;i<x.size();i++)
	x[i] = y[i];

    auto ratio = proposal(x,p);

    for(int i=0;i<y.size();i++)
	y[i] = x[i];

    P.set_parameter_values(indices,y);

    return ratio;
}

Proposal2::Proposal2(const proposal_fn& p,const std::string& s, const std::vector<string>& v,
		     const Model& P)
    :proposal(p),
     pnames(v)
{
    auto index = P.maybe_find_parameter(s);
    if (not index)
	throw myexception()<<"Model has no parameter called '"<<s<<"' - can't create proposal for it.";
    indices.push_back(*index);
}


Proposal2::Proposal2(const proposal_fn& p,const vector<std::string>& s, const std::vector<string>& v,
		     const Model& P)
    :proposal(p),
     pnames(v)
{
    for(int i=0;i<s.size();i++) {
	auto index = P.maybe_find_parameter(s[i]);
	if (not index)
	    throw myexception()<<"Model has no parameter called '"<<s[i]<<"' - can't create proposal for it.";
	indices.push_back(*index);
    }
}

log_double_t Proposal2M::operator()(Model& P) const
{
    if (not indices.size())
	throw myexception()<<"Proposal2M::operator() - No modifiables to alter!";

    // read, alter, and write parameter values
    vector< expression_ref > y = P.get_modifiable_values(indices);
    vector< expression_ref > x(y.size());
    for(int i=0;i<x.size();i++)
	x[i] = y[i];

    auto ratio = proposal(x,parameters);

    for(int i=0;i<y.size();i++)
	P.set_modifiable_value(indices[i], x[i]);

    return ratio;
}

Proposal2M::Proposal2M(const proposal_fn& p,int  s, const vector<double>& v)
    :Proposal2M(p,vector<int>{s},v)
{ }


Proposal2M::Proposal2M(const proposal_fn& p,const vector<int>& s, const vector<double>& v)
    :proposal(p),
     indices(s),
     parameters(v)
{ }

log_double_t move_scale_branch(Model& /*P */)
{
/*
  Parameters& PP = dynamic_cast<Parameters&>(P);

  int index = PP.find_parameter("lambdaScaleBranch");

  int scale_branch = PP.get_parameter_value(index).as_int();

  assert( scale_branch != -1);

  if (P.contains_key("lambda_search_all"))
  scale_branch = uniform(0, PP.t().n_branches() - 1);
  else
  {
  int attribute_index = PP.t().find_undirected_branch_attribute_index_by_name("lambda-scale-branch");  
    
  assert(attribute_index != -1);
    
  vector<int> branches;
  for(int b=0;b<PP.t().n_branches();b++)
  {
  boost::any value = PP.t().branch(b).undirected_attribute(attribute_index);
  if (not value.empty())
  branches.push_back(b);
  }
    
  int i = uniform(0, branches.size() -1);
  scale_branch = branches[i];
  }

  PP.set_parameter_value(index, scale_branch);
*/

    return 1.0;
}

log_double_t move_subst_type_branch(Model& P)
{
    Parameters& PP = dynamic_cast<Parameters&>(P);

    int which_branch = -1;
    int B = PP.t().n_branches();
    for(int b=0;b<B;b++)
    {
	int index = P.find_parameter("Main.branchCat" + convertToString(b+1));
	int cat = P.get_parameter_value(index).as_int();
	if (cat == 1)
	{
	    assert(which_branch == -1);
	    which_branch = b;
	}
    }

    if (which_branch != -1)
    {
	int new_branch = uniform(0, B-2);
	if (new_branch >= which_branch)
	    new_branch++;

	int index1 = P.find_parameter("Main.branchCat" + convertToString(which_branch+1));
	int index2 = P.find_parameter("Main.branchCat" + convertToString(new_branch+1));

	P.set_parameter_value(index1, 0);
	P.set_parameter_value(index2, 1);
	std::cerr<<"Moved subst type 1 from branch "<<which_branch<<" to branch "<<new_branch<<"\n";
    }

    return 1.0;
}

#include <boost/shared_ptr.hpp>
#include "mcmc/sample.H"

// Can't we just send in any sigma parameters or whatever WITH the proposal?
boost::shared_ptr<DPmatrixSimple> sample_alignment_base(mutable_data_partition P, const indel::PairHMM& hmm, int b);
boost::shared_ptr<DPmatrixSimple> sample_alignment_base(mutable_data_partition P, int b);
boost::shared_ptr<DPmatrixSimple> sample_alignment_forward(data_partition P, const indel::PairHMM& hmm, int b);
vector<int> walk_tree_path_toward(const TreeInterface& t, int root);

log_double_t realign_and_propose_parameter(Model& P, int param, const vector<int>& partitions, const proposal_fn& proposal, const vector<double>& v)
{
    Parameters& PP = dynamic_cast<Parameters&>(P);
    Parameters P0 = PP;
    const Parameters& CPP = dynamic_cast<const Parameters&>(P);
    auto t = CPP.t();
    int B = CPP.t().n_branches();
    int N = CPP.n_data_partitions();

    // 1. Get the old branch HMMs;
    vector<vector<indel::PairHMM>> old_hmms(N);
    for(int i=0; i < N; i++)
    {
	if (CPP[i].has_IModel())
	    for(int b=0;b<2*B;b++)
		old_hmms[i].push_back(CPP[i].get_branch_HMM(b));
    }

    // 2. Read, alter, and write parameter values
    vector< expression_ref > x = P.get_parameter_values({param});

    log_double_t ratio = proposal(x, v);

    P.set_parameter_values({param},x);

    // 3. Realign all branches using the new parameter value

    // 3.1 Choose an order to visit the branches
    int toward_node = uniform(t.n_nodes()>2?t.n_leaves():0, t.n_nodes()-1);
    vector<int> branches = walk_tree_path_toward_and_away(t, toward_node);
    if (uniform() < 0.5)
	std::reverse(branches);

    for(int b: branches)
    {
	PP.select_root(b);

	auto t = PP.t();

	if (t.is_leaf_node(t.target(b)) and t.n_nodes() > 2)
	    b = t.reverse(b);

	for(int j : partitions)
	{
	    // 3.3 We want to do ratio *= Pr(A' -> A)/Pr(A -> A')
	    if (PP[j].variable_alignment())
	    {
		{
		    // Calculate the proposal probability for the backward move.
		    auto matrix_old = sample_alignment_forward(PP[j], old_hmms[j][b], b);
		    auto a_old = PP[j].get_pairwise_alignment(b);
		    vector<int> path_old = A2::get_path_from_pairwise_alignment(a_old);
		    ratio *= matrix_old->path_P(path_old);
		}
		{
		    // Resample the alignment and calculate the proposal probability for the forward more.
		    auto matrix_new = sample_alignment_base(PP[j], b);
		    auto a_new = PP[j].get_pairwise_alignment(b);
		    vector<int> path_new = A2::get_path_from_pairwise_alignment(a_new);

		    if (matrix_new->Pr_sum_all_paths() <= 0.0)
		    {
			if (log_verbose)
			    std::cerr<<"realign_and_propose_parameter: forward matrix has zero probability on branch "<<b<<"\n";
			P = P0;
			return 1;
		    }
		    else
			ratio /= matrix_new->path_P(path_new);
		}
	    }
	}
    }

    return ratio;
}
