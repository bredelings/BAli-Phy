/*
   Copyright (C) 2004-2010 Benjamin Redelings

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

///
/// \file   mcmc/setup.C
/// \brief  Provides routines to create default transition kernels and start a Markov chain.
///
/// The function do_sampling( ) creates transition kernel for known parameter names.
/// It then starts the Markov chain for the MCMC run and runs it for a specified
/// number of iterations.
///
/// \author Benjamin Redelings
/// 

#include "smodel/smodel.H"
#include "mcmc/setup.H"
#include "util.H"
#include "sample.H"
#include "alignment/alignment-util.H"
#include "alignment-constraint.H"
#include "timer_stack.H"
#include "bounds.H"
#include "AIS.H"

using boost::program_options::variables_map;
using boost::dynamic_bitset;
using std::vector;
using std::endl;
using std::string;
using std::ostream;


/// \brief Add a Metropolis-Hastings sub-move for parameter name to M
///
/// \param P       The model that contains the parameters
/// \param p       The proposal function
/// \param name    The name of the parameter to create a move for
/// \param pname   The name of the proposal width for this move
/// \param sigma   A default proposal width, in case the user didn't specify one
/// \param M       The group of moves to which to add the newly-created sub-move
/// \param weight  How often to run this move.
///
void add_multi_param_MH_move(Probability_Model& P,const Proposal_Fn& p, const vector<string>& names, const string& mname, const string& pname,double sigma, 
			     MCMC::MoveAll& M,double weight=1)
{
  if (not names.size()) std::abort();

  set_if_undef(P.keys, pname, sigma);
  Proposal2 move_mu(p, names, vector<string>(1,pname), P);

  if (not move_mu.get_indices().size()) std::abort();

  string move_name = string("MH_sample_")+mname;
  M.add(weight, MCMC::MH_Move(move_mu, move_name));
}

/// \brief Add a Metropolis-Hastings sub-move for each parameter in \a names to \a M
void add_MH_move(Probability_Model& P, const Proposal_Fn& proposal, const vector<string>& names, 
		 const vector<string>& pnames, const vector<double>& pvalues,
		 MCMC::MoveAll& M, double weight=1)
{
  // FIXME - Change names to vector<vector<string>>, and deduce a move name from pathname shared by all parameters.
  // FIXME - then replace the routine above with this one!

  // 1. Set the parameter values to their defaults, if they are not set yet.
  if (pnames.size() != pvalues.size()) std::abort();

  for(int i=0;i<pnames.size();i++)
    set_if_undef(P.keys, pnames[i], pvalues[i]);

  // 2. For each MCMC parameter, create a move for it.
  for(const auto& parameter_name: names) 
  {
    int index = P.find_parameter(parameter_name);
    assert(index != -1);
    if (P.is_fixed(index)) continue;

    Proposal2 proposal2(proposal, parameter_name, pnames, P);

    M.add(weight, MCMC::MH_Move(proposal2, "MH_sample_"+parameter_name));
  }
}

/// \brief Add a Metropolis-Hastings sub-move for parameter name to M
///
/// \param P       The model that contains the parameters
/// \param p       The proposal function
/// \param name    The name of the parameter to create a move for
/// \param pname   The name of the proposal width for this move
/// \param sigma   A default proposal width, in case the user didn't specify one
/// \param M       The group of moves to which to add the newly-created sub-move
/// \param weight  How often to run this move.
///
void add_MH_move(Probability_Model& P,const Proposal_Fn& proposal, const string& name, const string& pname,double sigma, 
		 MCMC::MoveAll& M,double weight=1)
{
  vector<int> indices = parameters_with_extension(P,name);
  vector<string> names;
  for(int i: indices)
    names.push_back(P.parameter_name(i));

  add_MH_move(P, proposal, names, {pname}, {sigma}, M, weight);
}


/// \brief Add a 1-D slice-sampling sub-move for parameter name to M
///
/// \param P             The model that contains the parameters.
/// \param name          The name of the parameter to create a move for.
/// \param M             The group of moves to which to add the newly-created sub-move
/// \param weight        How often to run this move.
///
void add_slice_move(Probability_Model& P, const string& parameter_name, MCMC::MoveAll& M, double weight = 1)
{
  int index = P.find_parameter(parameter_name);

  M.add(weight, MCMC::Parameter_Slice_Move(string("slice_sample_")+parameter_name, index, 1.0) );
}


/// \brief Add a 1-D slice-sampling sub-move for parameter name to M
///
/// \param P             The model that contains the parameters.
/// \param name          The name of the parameter to create a move for.
/// \param M             The group of moves to which to add the newly-created sub-move
/// \param weight        How often to run this move.
///
void add_slice_moves(Probability_Model& P, const string& name, 
		     MCMC::MoveAll& M,
		     double weight = 1)
{
  vector<int> indices = parameters_with_extension(P,name);
  for(int i=0;i<indices.size();i++) 
  {
    if (P.is_fixed(indices[i])) continue;

    M.add(weight, 
	  MCMC::Parameter_Slice_Move(string("slice_sample_")+P.parameter_name(indices[i]),
				     indices[i], 1.0)
	  );
  }
}

/// \brief Add a 1-D slice-sampling sub-move for parameter name to M on a transformed scale
///
/// \param P             The model that contains the parameters
/// \param name          The name of the parameter to create a move for
/// \param M             The group of moves to which to add the newly-created sub-move
/// \param f1            The function from the parameter's scale to the transformed scale.
/// \param f2            The inverse of f1.
/// \param weight        How often to run this move.
///
void add_slice_moves(Probability_Model& P, const string& name, 
		     MCMC::MoveAll& M,
		     double(&f1)(double),
		     double(&f2)(double),
		     double weight = 1
		     )
{
  vector<int> indices = parameters_with_extension(P,name);
  for(int i=0;i<indices.size();i++) 
  {
    if (P.is_fixed(indices[i])) continue;

    M.add(weight, 
	  MCMC::Parameter_Slice_Move(string("slice_sample_")+P.parameter_name(indices[i]),
				     indices[i],
				     1.0,f1,f2)
	  );
  }
}

#include "distribution-operations.H" // for prob_density

/// Find parameters with distribution name Dist
vector<vector<string> > get_distributed_parameters(const Probability_Model& P, const string& Dist)
{
  vector<vector<string> > names;

  expression_ref query = (distributed, match(1), Tuple((prob_density, match(0) , match(-1), match(-1)), match(2)));
  for(int i=0;i<P.n_notes();i++)
    if (is_exactly(P.get_note(i),"~"))
    {
      // If its a probability expression, then...
      vector<expression_ref> results; 
      if (not find_match(query, P.get_note(i), results))
      {
	std::cerr<<"Warning: Expression '"<<P.get_note(i)<<"' is not a resolved probability expression!\n";
	std::cerr<<"Warning:  Therefore we can't currently determine the distribution name!\n\n";
	std::cout<<"Warning: Expression '"<<P.get_note(i)<<"' is not a resolved probability expression!\n";
	std::cout<<"Warning:  Therefore we can't currently determine the distribution name!\n\n";
	//	throw myexception()<<"Expression '"<<P.get_note(i)<<"' is not a resolved probability expression!";
      }

      string dist_name = *assert_is_a<String>(results[0]);
      if (dist_name != Dist) continue;

      expression_ref rand_var = results[1];
      if (is_exactly(rand_var->head,":"))
      {
	vector<expression_ref> rand_vars = get_ref_vector_from_list(rand_var);
	vector<string> var_names;
	for(const auto& v: rand_vars)
	  var_names.push_back(v->print());
	names.push_back(var_names);
      }
      else
	names.push_back({rand_var->print()});
    }

  return names;
}


/// \brief Add a 1-D slice-sampling sub-move for a collection of parameters that sum to 1.
///
/// \param P             The model that contains the parameters
/// \param name          The name of the parameter to create a move for
/// \param pname         The name of the slice window width for this move
/// \param W             The default window size, if not specified in P.keys
/// \param M             The group of moves to which to add the newly-created sub-move
/// \param weight        How often to run this move.
///
void add_dirichlet_slice_moves(Probability_Model& P, const string& move_name, 
			       const vector<int>& indices,
			       MCMC::MoveAll& M,
			       double weight = 1
			       )
{
  if (indices.empty()) return;

  if (indices.size() == 1) throw myexception()<<"Dirichlet move with only 1 parameter?";

  /// Create a parent move that will choose one child each time it is called
  MCMC::MoveOne M2(move_name);

  /// Add the moves for individual components of the vector as child moves of M2
  for(int i=0;i<indices.size();i++)
  {
    M2.add(1,
	  MCMC::Dirichlet_Slice_Move(string("slice_sample_")+P.parameter_name(indices[i]),
				     indices,
				     i
				     )
	  );
  }

  /// Add the move to resample one of the components at random to M
  M.add(weight,M2);
}


/// \brief Add a 1-D slice-sampling sub-move for a collection of parameters that sum to 1.
///
/// \param P             The model that contains the parameters
/// \param name          The name of the parameter to create a move for
/// \param pname         The name of the slice window width for this move
/// \param W             The default window size, if not specified in P.keys
/// \param M             The group of moves to which to add the newly-created sub-move
/// \param weight        How often to run this move.
///
void add_dirichlet_slice_moves(Probability_Model& P, const string& name,
			       const vector<string>& parameter_names,
			       MCMC::MoveAll& M,
			       double weight = 1
			       )
{
  vector<int> indices;
  for(const string& p: parameter_names)
  {
    int index = P.find_parameter(p);
    if (index == -1) return;
    indices.push_back(index);
  }

  add_dirichlet_slice_moves(P,string("slice_sample_")+name, indices, M, weight);
}

MCMC::MoveAll get_scale_MH_moves(owned_ptr<Probability_Model>& P)
{
  MCMC::MoveAll MH_moves("parameters:scale:MH");
  for(int i=0;i<P.as<Parameters>()->n_branch_means();i++)
    add_MH_move(*P, log_scaled(Between(-20,20,shift_cauchy)),    "mu"+convertToString(i+1),
		"mu_scale_sigma",     0.6,  MH_moves);
  return MH_moves;
}

//FIXME - how to make a number of variants with certain things fixed, for burn-in?
// 0. Get an initial tree estimate using NJ or something? (as an option...)
// 1. First estimate tree and parameters with alignment fixed.
// 2. Then allow the alignment to change.

/// \brief Construct Metropolis-Hastings moves for scalar numeric parameters with a corresponding slice move
///
/// \param P   The model and state.
///
MCMC::MoveAll get_parameter_MH_moves(Parameters& P)
{
  MCMC::MoveAll MH_moves("parameters:MH");

  for(int i=0;i<P.n_branch_means();i++)
    add_MH_move(P, log_scaled(Between(-20,20,shift_cauchy)),    "mu"+convertToString(i+1),             "mu_scale_sigma",     0.6,  MH_moves);

  add_MH_move(P, log_scaled(Between(-20,20,shift_cauchy)),    "*.HKY.kappa",     "kappa_scale_sigma",  0.3,  MH_moves);
  add_MH_move(P, log_scaled(Between(-20,20,shift_cauchy)),    "*.rho",     "rho_scale_sigma",  0.2,  MH_moves);
  add_MH_move(P, log_scaled(Between(-20,20,shift_cauchy)),    "*.TN.kappaPur", "kappa_scale_sigma",  0.3,  MH_moves);
  add_MH_move(P, log_scaled(Between(-20,20,shift_cauchy)),    "*.TN.kappaPyr", "kappa_scale_sigma",  0.3,  MH_moves);
  add_MH_move(P, log_scaled(Between(-20,20,shift_cauchy)),    "*.M0.omega",  "omega_scale_sigma",  0.3,  MH_moves);
  add_MH_move(P, log_scaled(Between(0,20,shift_cauchy)),
	                                        "*.M2.omega",  "omega_scale_sigma",  0.3,  MH_moves);
  add_MH_move(P, log_scaled(Between(0,20,shift_cauchy)),
	                                        "*.M2a.omega1",  "omega_scale_sigma",  0.3,  MH_moves);
  add_MH_move(P, log_scaled(Between(0,20,shift_cauchy)),
	                                        "*.M2a.omega3",  "omega_scale_sigma",  0.3,  MH_moves);
  add_MH_move(P, log_scaled(Between(0,20,shift_cauchy)),
	                                        "*.M8b.omega1",  "omega_scale_sigma",  0.3,  MH_moves);
  add_MH_move(P, log_scaled(Between(0,20,shift_cauchy)),
	                                        "*.M8b.omega3",  "omega_scale_sigma",  0.3,  MH_moves);
  add_MH_move(P, Between(0,1,shift_cauchy),   "*.INV.p",         "INV.p_shift_sigma", 0.03, MH_moves);
  add_MH_move(P, Between(0,1,shift_cauchy),   "*.Beta.mu",         "beta.mu_shift_sigma", 0.03, MH_moves);
  add_MH_move(P, Between(0,1,shift_cauchy),   "*.f",              "f_shift_sigma",      0.1,  MH_moves);
  add_MH_move(P, Between(0,1,shift_cauchy),   "*.g",              "g_shift_sigma",      0.1,  MH_moves);
  add_MH_move(P, Between(0,1,shift_cauchy),   "*.h",              "h_shift_sigma",      0.1,  MH_moves);
  add_MH_move(P, log_scaled(Between(-20,20,shift_cauchy)),    "*.gamma.sigma/mu","gamma.sigma_scale_sigma",  0.25, MH_moves);
  add_MH_move(P, log_scaled(Between(-20,0,shift_cauchy)),    "*.Beta.varOverMu", "beta.Var_scale_sigma",  0.25, MH_moves);
  add_MH_move(P, log_scaled(Between(-20,20,shift_cauchy)),    "*.LogNormal.sigma_over_mu","log-normal.sigma_scale_sigma",  0.25, MH_moves);
  MH_moves.add(4,MCMC::SingleMove(scale_means_only,
				  "scale_means_only", {/*FIXME*/}, "mean")
		      );

  
  add_MH_move(P, shift_delta,                 "*.delta",       "lambda_shift_sigma",     0.35, MH_moves, 10);
  add_MH_move(P, Between(-40,0,shift_cauchy), "*.lambda",      "lambda_shift_sigma",    0.35, MH_moves, 10);
  add_MH_move(P, shift_epsilon,               "*.epsilon",     "epsilon_shift_sigma",   0.30, MH_moves, 10);

  add_MH_move(P, Between(-20,20,shift_cauchy), "lambdaScale",      "lambda_shift_sigma",    0.35, MH_moves, 10);

  // FIXME - this might not work very well until I make these auto-tuning
  vector<vector<string>> dirichlet_parameters = get_distributed_parameters(P,"Dirichlet");
  int i=1;
  for(const auto& p: dirichlet_parameters)
  {
    string mname = "dirichlet"+convertToString(i);
    string pname = "sigma"+convertToString(i++);
    add_multi_param_MH_move(P, dirichlet_proposal, p, mname, pname, 1.0, MH_moves,0.1);
  }

  /*
    Here are my hacky estimates of the jump size that is appropriate.

    set_if_undef(P.keys,"pi_dirichlet_N",1.0);
    unsigned total_length = 0;
    for(int i=0;i<P.n_data_partitions();i++)
    total_length += max(sequence_lengths(*P[i].A, P.T->n_leaves()));
    P.keys["pi_dirichlet_N"] *= total_length;

    set_if_undef(P.keys,"GTR_dirichlet_N",1.0);
    if (s==0) P.keys["GTR_dirichlet_N"] *= 100;
    add_MH_move(P, dirichlet_proposal,  prefix + "S.GTR.?", "GTR_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"v_dirichlet_N",1.0);
    if (s==0) P.keys["v_dirichlet_N"] *= total_length;
    add_MH_move(P, dirichlet_proposal,  prefix +  "v*", "v_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"b_dirichlet_N",1.0);
    if (s==0) P.keys["b_dirichlet_N"] *= total_length;
    add_MH_move(P, dirichlet_proposal,  prefix +  "b_*", "b_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"M2.f_dirichlet_N",1.0);
    if (s==0) P.keys["M2.f_dirichlet_N"] *= 10;
    add_MH_move(P, dirichlet_proposal,  prefix +  "M2.f*", "M2.f_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"M2a.f_dirichlet_N",1.0);
    if (s==0) P.keys["M2a.f_dirichlet_N"] *= 10;
    add_MH_move(P, dirichlet_proposal,  prefix +  "M2a.f*", "M2a.f_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"M3.f_dirichlet_N",1.0);
    if (s==0) P.keys["M3.f_dirichlet_N"] *= 10;
    add_MH_move(P, dirichlet_proposal,   prefix + "M3.f*", "M3.f_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"M8b.f_dirichlet_N",1.0);
    if (s==0) P.keys["M8b.f_dirichlet_N"] *= 10;
    add_MH_move(P, dirichlet_proposal,   prefix + "M8b.f*", "M8b.f_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"multi.p_dirichlet_N",1.0);
    if (s==0) P.keys["multi.p_dirichlet_N"] *= 10;
    add_MH_move(P, dirichlet_proposal,   prefix + "multi.p*", "multi:p_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"DP.f_dirichlet_N",1.0);
    if (s==0) P.keys["DP.f_dirichlet_N"] *= 10;
    add_MH_move(P, dirichlet_proposal,   prefix + "DP.f*", "DP.f_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"DP.rate_dirichlet_N",1.0);
    //FIXME - this should probably be 20*#rate_categories...
    if (s==0) P.keys["DP.rate_dirichlet_N"] *= 10*10;
    // add_MH_move(P, sorted(dirichlet_proposal), prefix + "DP.rate*", "DP.rate_dirichlet_N",     1,  parameter_moves);
    add_MH_move(P, dirichlet_proposal, prefix + "DP.rate*", "DP.rate_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"Mixture.p_dirichlet_N",1.0);
    if (s==0) P.keys["Mixture.p_dirichlet_N"] *= 10*10;
    add_MH_move(P, dirichlet_proposal,         prefix + "Mixture.p*", "Mixture.p_dirichlet_N",     1,  parameter_moves);
  */

  return MH_moves;
}

MCMC::MoveAll get_scale_slice_moves(Parameters& P)
{
  MCMC::MoveAll slice_moves("parameters:scale:MH");
  for(int i=0;i<P.n_branch_means();i++)
    add_slice_moves(P, "mu"+convertToString(i+1), slice_moves);
  return slice_moves;
}

void add_1D_slice_moves_for_distribution(Parameters& P, const string& dist_name, MCMC::MoveAll& M)
{
  vector<vector<string>> dist_parameters = get_distributed_parameters(P,dist_name);
  for(const auto& p: dist_parameters)
  {
    if (p.size() != 1)
      throw myexception()<<join(p,"&")<<" should not be jointly distributed!";
    
    add_slice_move(P, p[0], M);
  }
}

/// \brief Construct 1-D slice-sampling moves for (some) scalar numeric parameters
///
/// \param P   The model and state.
///
MCMC::MoveAll get_parameter_slice_moves(Parameters& P)
{
  MCMC::MoveAll slice_moves("parameters:slice");

  // scale parameters
  for(int i=0;i<P.n_branch_means();i++)
    add_slice_moves(P, "*.mu"+convertToString(i+1), slice_moves);

  // Add slice moves for continuous 1D distributions
  add_1D_slice_moves_for_distribution(P, "LogLaplace", slice_moves);
  add_1D_slice_moves_for_distribution(P, "Uniform", slice_moves);
  add_1D_slice_moves_for_distribution(P, "Laplace", slice_moves);
  add_1D_slice_moves_for_distribution(P, "Cauchy", slice_moves);
  add_1D_slice_moves_for_distribution(P, "LogNormal", slice_moves);
  add_1D_slice_moves_for_distribution(P, "Normal", slice_moves);
  add_1D_slice_moves_for_distribution(P, "Beta", slice_moves);
  add_1D_slice_moves_for_distribution(P, "Gamma", slice_moves);
  add_1D_slice_moves_for_distribution(P, "LogGamma", slice_moves);
  add_1D_slice_moves_for_distribution(P, "Exponential", slice_moves);
  add_1D_slice_moves_for_distribution(P, "LogExponential", slice_moves);

  /*    
  add_slice_moves(P, "*.HKY.kappa", slice_moves);
  add_slice_moves(P, "*.rho", slice_moves);
  add_slice_moves(P, "*.TN.kappa(pur)", slice_moves);
  add_slice_moves(P, "*.TN.kappa(pyr)", slice_moves);
  add_slice_moves(P, "*.M0.omega", slice_moves);
  add_slice_moves(P, "*.M2.omega", slice_moves);
  add_slice_moves(P, "*.M2a.omega1", slice_moves);
  add_slice_moves(P, "*.M2a.omega3", slice_moves);
  add_slice_moves(P, "*.M8b.omega3", slice_moves);
  add_slice_moves(P, "*.branch-site.w*", slice_moves);
  add_slice_moves(P, "*.branch-site.pos-w", slice_moves);
  add_slice_moves(P, "*.branch-site.pos-p", slice_moves);
  add_slice_moves(P, "*.INV.p", slice_moves);
  add_slice_moves(P, "*.f", slice_moves);
  add_slice_moves(P, "*.g", slice_moves);
  add_slice_moves(P, "*.h", slice_moves);
  add_slice_moves(P, "*.Beta.varOverMu", slice_moves);
  add_slice_moves(P, "*.Gamma.sigmaOverMu", slice_moves);
  add_slice_moves(P, "*.Beta.sigmaOverMu", slice_moves);
  add_slice_moves(P, "*.LogNormal.sigmaOverMu", slice_moves);
  */

  // imodel parameters
  add_slice_moves(P, "*.delta", slice_moves, 10);
  add_slice_moves(P, "*.lambda", slice_moves, 10);
  add_slice_moves(P, "*.epsilon", slice_moves,transform_epsilon,inverse_epsilon, 10);

  add_slice_moves(P, "lambdaScale", slice_moves, 10);
  add_slice_moves(P, "*.M3.omega*", slice_moves);

  vector<vector<string>> dirichlet_parameters = get_distributed_parameters(P,"Dirichlet");

  int i=1;
  for(const auto& p: dirichlet_parameters)
  {
    string name = "dirichlet"+convertToString(i++);
    add_dirichlet_slice_moves(P, name, p, slice_moves);
  }


  slice_moves.add(2,MCMC::Scale_Means_Only_Slice_Move("scale_means_only_slice",0.6));

  return slice_moves;
}

/// \brief Construct dynamic programming moves to sample alignments.
///
/// \param P   The model and state.
///
MCMC::MoveAll get_alignment_moves(Parameters& P)
{
  // args for branch-based stuff
  vector<int> branches(P.T->n_branches());
  for(int i=0;i<branches.size();i++)
    branches[i] = i;

  // args for node-based stuff
  vector<int> internal_nodes;
  for(int i=P.T->n_leaves();i<P.T->n_nodes();i++)
    internal_nodes.push_back(i);

  using namespace MCMC;

  //----------------------- alignment -------------------------//
  MoveAll alignment_moves("alignment");

  //--------------- alignment::alignment_branch ---------------//
  MoveEach alignment_branch_moves("alignment_branch_master");
  alignment_branch_moves.add(1.0,
			     MoveArgSingle("sample_alignments","alignment:alignment_branch",
					   sample_alignments_one,
					   branches)
			     );
  if (P.T->n_leaves() >2) {
    alignment_branch_moves.add(0.15,MoveArgSingle("sample_tri","alignment:alignment_branch:nodes",
						 sample_tri_one,
						 branches)
			       );
    alignment_branch_moves.add(0.1,MoveArgSingle("sample_tri_branch","alignment:nodes:length",
						 sample_tri_branch_one,
						 branches)
			       ,false);
    alignment_branch_moves.add(0.1,MoveArgSingle("sample_tri_branch_aligned","alignment:nodes:length",
						 sample_tri_branch_type_one,
						 branches)
			       ,false);
  }
  alignment_moves.add(1, alignment_branch_moves, false);
  alignment_moves.add(1, SingleMove(walk_tree_sample_alignments, "walk_tree_sample_alignments",{},"alignment:alignment_branch:nodes") );

  //---------- alignment::nodes_master (nodes_moves) ----------//
  MoveEach nodes_moves("nodes_master","alignment:nodes");
  if (P.T->n_leaves() >= 3)
    nodes_moves.add(10,MoveArgSingle("sample_node","alignment:nodes",
				   sample_node_move,
				   internal_nodes)
		   );
  if (P.T->n_leaves() >= 4)
    nodes_moves.add(1,MoveArgSingle("sample_two_nodes","alignment:nodes",
				   sample_two_nodes_move,
				   internal_nodes)
		   );

  int nodes_weight = (int)(loadvalue(P.keys,"nodes_weight",1.0)+0.5);

  alignment_moves.add(nodes_weight, nodes_moves);
 
  return alignment_moves;
}

/// \brief Construct moves to sample the tree
///
/// \param P   The model and state.
///
MCMC::MoveAll get_tree_moves(Parameters& P)
{
  // args for branch-based stuff
  vector<int> branches(P.T->n_branches());
  for(int i=0;i<branches.size();i++)
    branches[i] = i;

  // args for branch-based stuff
  vector<int> internal_branches;
  for(int i=P.T->n_leaves();i<P.T->n_branches();i++)
    internal_branches.push_back(i);

  using namespace MCMC;

  //-------------------- tree (tree_moves) --------------------//
  MoveAll tree_moves("tree");
  MoveAll topology_move("topology");
  MoveEach NNI_move("NNI");
  MoveOne SPR_move("SPR");

  bool has_imodel = P.variable_alignment();

  if (has_imodel)
    NNI_move.add(1,MoveArgSingle("three_way_NNI","alignment:nodes:topology",
				 three_way_topology_sample,
				 internal_branches)
		 );
  else
    NNI_move.add(1,MoveArgSingle("three_way_NNI","topology",
				 three_way_topology_sample,
				 internal_branches)
		 );

  NNI_move.add(1,MoveArgSingle("two_way_NNI","alignment:nodes:topology",
				    two_way_topology_sample,
				    internal_branches)
		    ,false
		    );

  //FIXME - doesn't yet deal with gaps=star
  if (has_imodel)
    NNI_move.add(0.001,MoveArgSingle("three_way_NNI_and_A","alignment:alignment_branch:nodes:topology",
				   three_way_topology_and_alignment_sample,
				     internal_branches)
		 ,false
		 );


  if (has_imodel) {
    SPR_move.add(1,SingleMove(sample_SPR_flat,"SPR_and_A_flat",{}, "topology:lengths:nodes:alignment:alignment_branch"));
    SPR_move.add(1,SingleMove(sample_SPR_nodes,"SPR_and_A_nodes",{}, "topology:lengths:nodes:alignment:alignment_branch"));
    SPR_move.add(5,SingleMove(sample_SPR_all,"SPR_and_A_all",{}, "topology:lengths:nodes:alignment:alignment_branch"));
  }
  else {
    SPR_move.add(1,SingleMove(sample_SPR_flat,"SPR_flat",{}, "topology:lengths"));
    SPR_move.add(1,SingleMove(sample_SPR_nodes,"SPR_nodes",{}, "topology:lengths"));
    SPR_move.add(10,SingleMove(sample_SPR_all,"SPR_all",{}, "topology:lengths"));
  }

  topology_move.add(1,NNI_move,false);
  topology_move.add(1,SPR_move);
  if (P.T->n_leaves() >3)
    tree_moves.add(1,topology_move);
  
  //-------------- tree::lengths (length_moves) -------------//
  MoveAll length_moves("lengths");
  MoveEach length_moves1("lengths1");

  length_moves1.add(1,MoveArgSingle("change_branch_length","lengths",
				   change_branch_length_move,
				   branches)
		   );
  length_moves1.add(1,MoveArgSingle("change_branch_length_multi","lengths",
				   change_branch_length_multi_move,
				   branches)
		   );

  length_moves1.add(0.01,MoveArgSingle("change_branch_length_and_T","lengths:nodes:topology",
				       change_branch_length_and_T,
				       internal_branches)
		    );
  length_moves.add(1,length_moves1,false);
  // FIXME - Do we really want to do this, under slice sampling?
  length_moves.add(1,SingleMove(walk_tree_sample_branch_lengths,
				"walk_tree_sample_branch_lengths",{},"lengths")
		   );

  tree_moves.add(1,length_moves);
  tree_moves.add(1,SingleMove(walk_tree_sample_NNI_and_branch_lengths,"walk_tree_NNI_and_lengths",{}, "topology:lengths"));

  if (has_imodel)
    tree_moves.add(2,SingleMove(walk_tree_sample_NNI,"walk_tree_NNI", {}, "topology:lengths"));
  else  // w/o integrating over 5way alignments, this move is REALLY cheap!
    tree_moves.add(4,SingleMove(walk_tree_sample_NNI,"walk_tree_NNI", {}, "topology:lengths"));

  if (has_imodel)
    tree_moves.add(0.5,SingleMove(walk_tree_sample_NNI_and_A,"walk_tree_NNI_and_A",{},"topology:lengths:nodes:alignment:alignment_branch"));

  return tree_moves;
}

/// \brief Construct Metropolis-Hastings moves for scalar numeric parameters without a corresponding slice move
///
/// \param P   The model and state.
///
MCMC::MoveAll get_parameter_MH_but_no_slice_moves(Parameters& P)
{
  using namespace MCMC;

  MoveAll parameter_moves("parameters");

  // Why 1.5?
  // Well, there's some danger here that we could flip in a periodic fashion, and only observe variable when its True (or only if its False).
  //  - It seems to be OK, though.  Why?
  //  - Note that this should only be an issue when this does not affect the likelihood.
  // Also, how hard would it be to make a Gibbs flipper?  We could (perhaps) run that once per iteration to avoid periodicity.

  vector<vector<string>> bernoulli_parameters = get_distributed_parameters(P,"Bernoulli");
  for(const auto& parameters: bernoulli_parameters)
  {
    assert(parameters.size() == 1);
    add_MH_move(P, bit_flip, parameters, {}, {}, parameter_moves, 1.5);
  }

  // FIXME - we need a proposal that sorts after changing
  //         then we can un-hack the recalc function in smodel.C
  //         BUT, this assumes that we have the DP::rate* names in *numerical* order
  //          whereas we probably find them in *lexical* order....
  //          ... or creation order?  That might be OK for now! 


  for(int i=0;;i++) {
    string name = "M3.omega" + convertToString(i+1);
    if (not has_parameter(P,name))
      break;
    
    add_MH_move(P, log_scaled(Between(-20,20,shift_cauchy)), name, "omega_scale_sigma", 1, parameter_moves);
    //    Proposal2 m(log_scaled(shift_cauchy), name, vector<string>(1,"omega_scale_sigma"), P);
    //    parameter_moves.add(1, MCMC::MH_Move(m,"sample_M3.omega"));
  }

  {
    int index = P.find_parameter("lambdaScaleBranch");
    if (index != -1 and (P.get_parameter_value_as<Int>(index) != -1 or P.keys.count("lambda_search_all")))
    {
      P.set_parameter_value(index, object_ref(Int(0)));
      Generic_Proposal m(move_scale_branch,{index});
      parameter_moves.add(1.0, MCMC::MH_Move(m,"sample_lambdaScaleBranch"));
    }
  }

  if (P.keys.count("sample_foreground_branch"))
  {
    Generic_Proposal m(move_subst_type_branch);
    parameter_moves.add(1.0, MCMC::MH_Move(m,"sample_foreground_branch"));
  }

  return parameter_moves;
}

void enable_disable_transition_kernels(MCMC::MoveAll& sampler, const variables_map& args)
{
  vector<string> disable;
  vector<string> enable;
  if (args.count("disable"))
    disable = split(args["disable"].as<string>(),',');
  if (args.count("enable"))
    enable = split(args["enable"].as<string>(),',');
  
  for(int i=0;i<disable.size();i++)
    sampler.disable(disable[i]);
  
  for(int i=0;i<enable.size();i++)
    sampler.enable(enable[i]);
}

/// Find the minimum branch length
double min_branch_length(const SequenceTree& T)
{
  double min_branch = T.branch(0).length();
  for(int i=1;i<T.n_branches();i++)
    min_branch = std::min(min_branch,T.branch(i).length());
  return min_branch;
}

/// Replace negative or zero branch lengths with saner values.
void set_min_branch_length(Parameters& P, double min_branch)
{
  const SequenceTree& T = *P.T;

  for(int b=0;b<T.n_branches();b++) 
    if (T.branch(b).length() < min_branch)
      P.setlength(b, min_branch);
}

void avoid_zero_likelihood(owned_ptr<Probability_Model>& P, ostream& out_log,ostream& /* out_both */)
{
  Parameters& PP = *P.as<Parameters>();

  for(int i=0;i<20 and P->likelihood() == 0.0;i++)
  {
    double min_branch = min_branch_length(*PP.T);
    out_log<<"  likelihood = "<<P->likelihood()<<"  min(T[b]) = "<<min_branch<<"\n";

    min_branch = std::max(min_branch, 0.0001);
    set_min_branch_length(PP, 2.0*min_branch);
  }
}

double fraction_non_gap(const Parameters& P)
{
  double total_char = 0;
  double total_cell = 0;
  for(int i=0;i<P.n_data_partitions();i++)
  {
    const alignment& A = *P[i].A;
    total_char += sum(sequence_lengths(A));
    total_cell += A.n_sequences()*A.length();
  }
  return total_char/total_cell;
}

void do_pre_burnin(const variables_map& args, owned_ptr<Probability_Model>& P,
		   ostream& out_log,ostream& out_both)
{
  using namespace MCMC;
  using namespace boost::chrono;

  int n_pre_burnin = args["pre-burnin"].as<int>();

  if (n_pre_burnin <= 0) return;

  duration_t t1 = total_cpu_time();
  out_both<<"Beginning pre-burnin: "<<n_pre_burnin<<" iterations."<<endl;

  if (P.as<Parameters>()->variable_alignment() and not args.count("tree"))
    P.as<Parameters>()->variable_alignment(false);

  MoveStats Stats;
  // 1. First choose the scale of the tree
  if (fraction_non_gap(*P.as<Parameters>()) > 1.0/20)
  {
    MoveAll pre_burnin("pre-burnin");

    pre_burnin.add(3,get_scale_slice_moves(*P.as<Parameters>()));

    // enable and disable moves
    enable_disable_transition_kernels(pre_burnin,args);

    for(int i=0;i<3;i++) {
      out_both<<" Tree size #"<<i+1<<"   prior = "<<P->prior()<<"   likelihood = "<<P->likelihood();
      out_both<<"   |T| = "<<Get_Tree_Length_Function()(P,0);
      for(int j=0;j<P.as<Parameters>()->n_branch_means();j++)
      {
	Parameters& PP = *P.as<Parameters>();
	out_both<<"     mu"<<j+1<<" = "<<PP.get_parameter_value_as<Double>(PP.branch_mean_index(j))<<endl;
      }
      show_parameters(out_log,*P);
      pre_burnin.iterate(P,Stats);
    }
  }
  else
    out_both<<"Skipping fixed-alignment tree-size pre-burnin: too many gaps in initial alignment!"<<std::endl;
  out_both<<endl;

  // 2. Then do an initial tree search - SPR - ignore indel information
  if (fraction_non_gap(*P.as<Parameters>()) > 1.0/20)
  {
    MoveAll pre_burnin("pre-burnin");
    pre_burnin.add(4,get_scale_slice_moves(*P.as<Parameters>()));
    pre_burnin.add(4,MCMC::SingleMove(scale_means_only,
				      "scale_means_only",{},"mean"));
    pre_burnin.add(1,SingleMove(walk_tree_sample_branch_lengths,
				"walk_tree_sample_branch_lengths",{},"tree:lengths"));
    pre_burnin.add(1,SingleMove(sample_SPR_search_all,"SPR_search_all", {},
				"tree:topology:lengths"));

    // enable and disable moves
    enable_disable_transition_kernels(pre_burnin,args);

    for(int i=0;i<n_pre_burnin;i++) {
      out_both<<" SPR #"<<i+1<<"   prior = "<<P->prior()<<"   likelihood = "<<P->likelihood();
      out_both<<"   |T| = "<<Get_Tree_Length_Function()(P,0);
      for(int j=0;j<P.as<Parameters>()->n_branch_means();j++)
      {
	Parameters& PP = *P.as<Parameters>();
	out_both<<"     mu"<<j+1<<" = "<<PP.get_parameter_value_as<Double>(PP.branch_mean_index(j))<<endl;
      }
      show_parameters(out_log,*P);
      pre_burnin.iterate(P,Stats);
    }
    out_both<<endl;
  }
  else
    out_both<<"Skipping fixed-alignment SPR pre-burnin: too many gaps in initial alignment!"<<std::endl;

    // 3. Then do a further tree search - NNI - w/ the actual model
  if (fraction_non_gap(*P.as<Parameters>()) > 1.0/20)
  {
    MoveAll pre_burnin("pre-burnin");

    pre_burnin.add(4,get_scale_slice_moves(*P.as<Parameters>()));
    pre_burnin.add(4,MCMC::SingleMove(scale_means_only,
				      "scale_means_only",{/*FIXME*/},"mean"));
    pre_burnin.add(1,SingleMove(walk_tree_sample_NNI_and_branch_lengths,
				"NNI_and_lengths",{},"tree:topology:lengths"));

    // enable and disable moves
    enable_disable_transition_kernels(pre_burnin,args);

    int n_pre_burnin2 = n_pre_burnin + (int)log(P.as<Parameters>()->T->n_leaves());
    for(int i=0;i<n_pre_burnin2;i++) {
      out_both<<" NNI #"<<i+1<<"   prior = "<<P->prior()<<"   likelihood = "<<P->likelihood();
      out_both<<"   |T| = "<<Get_Tree_Length_Function()(P,0);
      for(int j=0;j<P.as<Parameters>()->n_branch_means();j++)
      {
	Parameters& PP = *P.as<Parameters>();
	out_both<<"     mu"<<j+1<<" = "<<PP.get_parameter_value_as<Double>(PP.branch_mean_index(j))<<endl;
      }
      show_parameters(out_log,*P);
      pre_burnin.iterate(P,Stats);
    }
  }
  else
    out_both<<"Skipping fixed-alignment NNI pre-burnin: too many gaps in initial alignment!"<<std::endl;
  out_both<<endl;

  // Set all alignments that COULD be variable back to being variable.
  P.as<Parameters>()->variable_alignment(true);


  // 4. Then do an initial tree search - SPR - with variable alignment
  if (P->keys.find("pre-burnin-A") != P->keys.end())
  {
    // turn training on
    {
      Parameters& PP = *P.as<Parameters>();
      PP.set_parameter_value(PP.find_parameter("IModels.training"), new Bool(true));
    }

    MoveAll pre_burnin("pre-burnin+A");
    pre_burnin.add(4,get_scale_slice_moves(*P.as<Parameters>()));
    pre_burnin.add(4,MCMC::SingleMove(scale_means_only,
				      "scale_means_only",{/*FIXME*/}, "mean"));
    pre_burnin.add(1,SingleMove(walk_tree_sample_branch_lengths,
				"walk_tree_sample_branch_lengths",{}, "tree:lengths"));
    pre_burnin.add(1,SingleMove(sample_SPR_A_search_all,"SPR_search_all", {},
				"tree:topology:lengths"));

    // enable and disable moves
    enable_disable_transition_kernels(pre_burnin,args);

    for(int i=0;i<2*n_pre_burnin;i++) {
      out_both<<" SPR+A #"<<i+1<<"   prior = "<<P->prior()<<"   likelihood = "<<P->likelihood();
      out_both<<"   |A| = "<<Get_Total_Alignment_Length_Function()(P,0);
      out_both<<"   |T| = "<<Get_Tree_Length_Function()(P,0);
      for(int j=0;j<P.as<Parameters>()->n_branch_means();j++)
      {
	Parameters& PP = *P.as<Parameters>();
	out_both<<"     mu"<<j+1<<" = "<<PP.get_parameter_value_as<Double>(PP.branch_mean_index(j))<<endl;
      }
      show_parameters(out_log,*P);
      pre_burnin.iterate(P,Stats);
    }

    // turn training back off
    {
      Parameters& PP = *P.as<Parameters>();
      PP.set_parameter_value(PP.find_parameter("IModels.training"), new Bool(false));
    }
  }
  out_both<<endl;

  out_log<<Stats<<endl;
  duration_t t2 = total_cpu_time();
  out_log<<default_timer_stack.report()<<endl;
  out_both<<"Finished pre-burnin in "<<duration_cast<boost::chrono::duration<double> >(t2-t1)<<".\n"<<endl;


  for(int i=0; i<P.as<Parameters>()->n_branch_means(); i++)
  {
    if (P->get_parameter_value_as<Double>(P.as<Parameters>()->branch_mean_index(i)) > 0.5)
      P->set_parameter_value(P.as<Parameters>()->branch_mean_index(i), 0.5);
  }


}

/// \brief Create transition kernels and start a Markov chain
///
/// \param args            Contains command line arguments
/// \param P               The model and current state
/// \param max_iterations  The number of iterations to run (unless interrupted).
/// \param files           Files to log output into
///
void do_sampling(const variables_map& args,
		 owned_ptr<Probability_Model>& P,
		 long int max_iterations,
		 ostream& s_out,
		 const vector<owned_ptr<MCMC::Logger> >& loggers)
{
  using namespace MCMC;

  Parameters& PP = *P.as<Parameters>();

  bool has_imodel = PP.variable_alignment();

  if (has_imodel) {
    for(int i=0;i<PP.n_data_partitions();i++)
      check_internal_nodes_connected(*PP[i].A,PP[i].T());
  }

  //----------------------- alignment -------------------------//
  MoveAll alignment_moves = get_alignment_moves(PP);

  //------------------------- tree ----------------------------//
  MoveAll tree_moves = get_tree_moves(PP);

  //-------------- parameters (parameters_moves) --------------//
  MoveAll MH_but_no_slice_moves = get_parameter_MH_but_no_slice_moves(PP);
  MoveAll slice_moves = get_parameter_slice_moves(PP);
  MoveAll MH_moves = get_parameter_MH_moves(PP);

  //------------------ Construct the sampler  -----------------//
  int subsample = args["subsample"].as<int>();

  // full sampler
  Sampler sampler("sampler");

  for(int i=0;i<loggers.size();i++)
    sampler.add_logger(loggers[i]);
  if (has_imodel)
  {
    double factor = loadvalue(P->keys,"alignment_sampling_factor",1.0);
    std::cerr<<"alignment sampling factor = "<<factor<<"\n";
    sampler.add(factor,alignment_moves);
  }
  sampler.add(2,tree_moves);

  // FIXME - We certainly don't want to do MH_sample_mu[i] O(branches) times
  // - It does call the likelihood function, doesn't it?
  // FIXME -   However, it is probably not so important to resample most parameters in a way that is interleaved with stuff... (?)
  // FIXME -   Certainly, we aren't going to be interleaved with branches, anyway!
  sampler.add(5 + log(PP.T->n_branches()), MH_but_no_slice_moves);
  if (P->keys["enable_MH_sampling"] > 0.5)
    sampler.add(5 + log(PP.T->n_branches()),MH_moves);
  else
    sampler.add(1,MH_moves);
  // Question: how are these moves intermixed with the other ones?

  if (P->keys["disable_slice_sampling"] < 0.5)
    sampler.add(1,slice_moves);

  //------------------- Enable and Disable moves ---------------------------//
  enable_disable_transition_kernels(sampler,args);

  //------------------ Report status before starting MCMC -------------------//
  
  sampler.show_enabled(s_out);
  s_out<<"\n";

  //-------------------- Report alignment alignments -----------------------//
  for(int i=0;i<PP.n_data_partitions();i++)
    std::cout<<"Partition "<<i+1<<": using "<<PP[i].alignment_constraint.size1()<<" constraints.\n";

  for(int i=0;i<PP.n_data_partitions();i++) 
  {
    dynamic_bitset<> s2 = constraint_satisfied(PP[i].alignment_constraint,*PP[i].A);
    dynamic_bitset<> s1(s2.size());
    report_constraints(s1,s2,i);
  } 

  sampler.check_moves(P);
  if (PP.keys["AIS"] > 0.5) 
  {
    // before we do this, just run 20 iterations of a sampler that keeps the alignment fixed
    // - first, we need a way to change the tree on a sampler that has internal node sequences?
    // - well, this should be exactly the -t sampler.
    // - but then how do we copy stuff over?
    AIS_Sampler A(sampler);
    vector<double> beta(1,1);
    for(int i=0;i<50;i++)
      beta.push_back(beta.back()*0.9);
    beta.push_back(0);
    
    A.go(P,std::cerr,beta,10);
  }
  else
    sampler.go(P,subsample,max_iterations,s_out);
}
