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

#include "models/path.H"
#include "mcmc/setup.H"
#include "mcmc/logger.H"
#include "util/string/split.H"
#include "util/range.H"
#include "util/map.H"
#include "sample.H"
#include "alignment/alignment-util.H"
#include "alignment/alignment-util2.H"
#include "alignment/alignment-constraint.H"
#include "util/time.H"
#include "util/bounds.H"
#include "AIS.H"
#include "computation/expression/bool.H"
#include "computation/expression/constructor.H"

using boost::program_options::variables_map;
using boost::dynamic_bitset;
using std::optional;
using std::vector;
using std::endl;
using std::string;
using std::ostream;
using std::map;
using std::shared_ptr;

template <typename T>
using Bounds = Box<bounds<T>>;

/// \brief Add a Metropolis-Hastings sub-move for each parameter in \a names to \a M
void add_modifiable_MH_move(const context_ref& P, const string& name, const proposal_fn& proposal, int rv, const vector<double>& parameters,
                            MCMC::MoveAll& M, double weight=1)
{
    auto r_mod = P.find_modifiable_reg(rv);
    M.add(weight, MCMC::MH_Move( Proposal2M(proposal, *r_mod, parameters), name) );
}

double default_sampling_rate(const context_ref& /*M*/, const string& /*parameter_name*/)
{
    return 1.0;
}

bool add_modifiable_slice_move(int rv, const Bounds<double>& bounds, MCMC::MoveAll& M, double weight = 1.0)
{
    string name = "m_real_"+convertToString<int>(rv);

    M.add( weight, MCMC::Random_Variable_Slice_Move(name, rv, bounds, 1.0) );
    return true;
}

optional<int> scale_is_modifiable(const context_ref& M, int s)
{
    auto& P = dynamic_cast<const Parameters&>(M);
    return P.branch_scale(s).is_modifiable(M);
}

//FIXME - how to make a number of variants with certain things fixed, for burn-in?
// 0. Get an initial tree estimate using NJ or something? (as an option...)
// 1. First estimate tree and parameters with alignment fixed.
// 2. Then allow the alignment to change.

/// \brief Construct Metropolis-Hastings moves for scalar numeric parameters with a corresponding slice move
///
/// \param P   The model and state.
///
MCMC::MoveAll get_parameter_MH_moves(context_ref& M)
{
    MCMC::MoveAll MH_moves("parameters:MH");

    if (Parameters* P = dynamic_cast<Parameters*>(&M))
	for(int i=0;i<P->n_branch_scales();i++)
	    if (auto r = scale_is_modifiable(M, i))
	    {
		string name = "scale_scale_"+convertToString<int>(i+1);
		add_modifiable_MH_move(M, name, log_scaled(Between(-20,20,shift_cauchy)),    *r, {1.0}, MH_moves);
	    }

    // Add these moves disabled
    // add_alignment_and_parameter_moves(MH_moves, M, 1.0, false);

    return MH_moves;
}

MCMC::MoveAll get_scale_slice_moves(Parameters& P)
{
    MCMC::MoveAll slice_moves("parameters:scale:MH");
    for(int i=0;i<P.n_branch_scales();i++)
	if (auto r = scale_is_modifiable(P,i))
	    add_modifiable_slice_move(*r, lower_bound<double>(0), slice_moves);

    return slice_moves;
}

/// \brief Construct 1-D slice-sampling moves for (some) scalar numeric parameters
///
/// \param P   The model and state.
///
MCMC::MoveAll get_parameter_slice_moves(context_ref& M)
{
    MCMC::MoveAll slice_moves("parameters:slice");

    if (Parameters* P = dynamic_cast<Parameters*>(&M))
    {
	// scale parameters - do we need this?
	for(int i=0;i<P->n_branch_scales();i++)
	    if (auto r = scale_is_modifiable(M,i))
		add_modifiable_slice_move(*r, lower_bound<double>(0), slice_moves);
    }

    return slice_moves;
}

/// \brief Construct dynamic programming moves to sample alignments.
///
/// \param P   The model and state.
///
MCMC::MoveAll get_alignment_moves(Parameters& P)
{
    // args for branch-based stuff
    vector<int> branches(P.t().n_branches());
    for(int i=0;i<branches.size();i++)
	branches[i] = i;

    // args for node-based stuff
    vector<int> internal_nodes;
    for(int i=P.t().n_leaves();i<P.t().n_nodes();i++)
	internal_nodes.push_back(i);

    using namespace MCMC;

    //----------------------- alignment -------------------------//
    MoveAll alignment_moves("alignment");
    alignment_moves.add(1, SingleMove(walk_tree_sample_alignments, "walk_tree_sample_alignments","alignment:alignment_branch:nodes") );
    alignment_moves.add(0.1, SingleMove(realign_from_tips, "realign_from_tips","lengths:alignment:topology") );

    //---------- alignment::nodes_master (nodes_moves) ----------//
    MoveEach nodes_moves("nodes_master","alignment:nodes");
    if (P.t().n_leaves() >= 3)
	nodes_moves.add(10,MoveArgSingle("sample_node","alignment:nodes",
					 sample_node_move,
					 internal_nodes)
	    );
    if (P.t().n_leaves() >= 4)
	nodes_moves.add(1,MoveArgSingle("sample_two_nodes","alignment:nodes",
					sample_two_nodes_move,
					internal_nodes)
	    );

    int nodes_weight = (int)(P.load_value("nodes_weight",1.0)+0.5);

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
    vector<int> branches(P.t().n_branches());
    for(int i=0;i<branches.size();i++)
	branches[i] = i;

    // args for branch-based stuff
    vector<int> internal_branches;
    for(int i=P.t().n_leaves();i<P.t().n_branches();i++)
	internal_branches.push_back(i);

    using namespace MCMC;

    //-------------------- tree (tree_moves) --------------------//
    MoveAll tree_moves("tree");
    MoveAll topology_move("topology");
    MoveAll SPR_move("SPR");

    bool has_imodel = P.variable_alignment();

    if (has_imodel)
    {
	// Try to intelligently consider which branch to do the alignment integration on, based on the current alignment.
	SPR_move.add(1, SingleMove(sample_SPR_all,"SPR_and_A_all", "topology:lengths:nodes:alignment:alignment_branch"));

	// If the alignment is variable, then we really need to propose things in a way that doesn't depend on the current alignment.
	SPR_move.add(0.5, SingleMove(sample_SPR_flat,"SPR_and_A_flat", "topology:lengths:nodes:alignment:alignment_branch"));
	SPR_move.add(0.5, SingleMove(sample_SPR_nodes,"SPR_and_A_nodes", "topology:lengths:nodes:alignment:alignment_branch"));
    }
    else {
	SPR_move.add(0.1,SingleMove(sample_SPR_flat,"SPR_flat", "topology:lengths"));
	SPR_move.add(0.1,SingleMove(sample_SPR_nodes,"SPR_nodes", "topology:lengths"));
	SPR_move.add(1,SingleMove(sample_SPR_all,"SPR_all", "topology:lengths"));
    }

    topology_move.add(1,SPR_move);
    if (P.t().n_leaves() >3)
	tree_moves.add(1,topology_move);
  
    //-------------- tree::lengths (length_moves) -------------//
    MoveAll length_moves("lengths");


    // FIXME - Do we really want to do this, under slice sampling?
    length_moves.add(1,SingleMove(walk_tree_sample_branch_lengths,
				  "walk_tree_sample_branch_lengths","lengths")
	);

    tree_moves.add(1,length_moves);
    tree_moves.add(1,SingleMove(walk_tree_sample_NNI_and_branch_lengths,"walk_tree_NNI_and_lengths", "topology:lengths"));

    if (has_imodel)
	tree_moves.add(2,SingleMove(walk_tree_sample_NNI,"walk_tree_NNI", "topology:lengths"));
    else  // w/o integrating over 5way alignments, this move is REALLY cheap!
	tree_moves.add(4,SingleMove(walk_tree_sample_NNI,"walk_tree_NNI", "topology:lengths"));

    if (has_imodel)
	tree_moves.add(0.5,SingleMove(walk_tree_sample_NNI_and_A,"walk_tree_NNI_and_A","topology:lengths:nodes:alignment:alignment_branch"));

    return tree_moves;
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
double min_branch_length(const TreeInterface& t)
{
    double min_branch = t.branch_length(0);
    for(int i=1;i<t.n_branches();i++)
	min_branch = std::min(min_branch, t.branch_length(i));
    return min_branch;
}

/// Replace negative or zero branch lengths with saner values.
void set_min_branch_length(Parameters& P, double min_branch)
{
    auto t = P.t();

    for(int b=0;b<t.n_branches();b++) 
	if (t.branch_length(b) < min_branch)
	    P.setlength(b, min_branch);
}

void avoid_zero_likelihood(owned_ptr<Model>& P, ostream& out_log,ostream& /* out_both */)
{
    if (not P.as<Parameters>()) return;

    Parameters& PP = *P.as<Parameters>();

    for(int i=0;i<20 and P->likelihood() == 0.0;i++)
    {
	double min_branch = min_branch_length(PP.t());
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
	const alignment& A = P[i].A();
	total_char += sum(sequence_lengths(A));
	total_cell += A.n_sequences()*A.length();
    }
    return total_char/total_cell;
}

/// \brief Create transition kernels and start a Markov chain
///
/// \param args            Contains command line arguments
/// \param P               The model and current state
/// \param max_iterations  The number of iterations to run (unless interrupted).
/// \param files           Files to log output into
///
void do_sampling(const variables_map& args,
		 owned_ptr<Model>& P,
		 long int max_iterations,
		 ostream& s_out,
		 const vector<MCMC::Logger>& loggers)
{
    using namespace MCMC;

    bool is_parameters = P.as<Parameters>();

    //------------------ Construct the sampler  -----------------//
    int subsample = args["subsample"].as<int>();
  
    // full sampler
    Sampler sampler("sampler");

    MoveAll slice_moves = get_parameter_slice_moves(*P);
    MoveAll MH_moves = get_parameter_MH_moves(*P);

    if (is_parameters)
    {
	owned_ptr<Parameters> PP = P.as<Parameters>();
	//----------------------- alignment -------------------------//
	MoveAll alignment_moves = get_alignment_moves(*PP);
    
	//------------------------- tree ----------------------------//
	MoveAll tree_moves = get_tree_moves(*PP);
    
	//-------------- parameters (parameters_moves) --------------//
	if (PP->variable_alignment() and PP->t().n_nodes() > 1)
	{
	    double factor = P->load_value("alignment-sampling-factor",1.0);
	    std::cerr<<"alignment sampling factor = "<<factor<<"\n";
	    sampler.add(factor,alignment_moves);
	}
	if (PP->t().n_nodes() > 1)
	    sampler.add(2,tree_moves);

	// FIXME - We certainly don't want to do MH_sample_mu[i] O(branches) times
	// - It does call the likelihood function, doesn't it?
	// FIXME -   However, it is probably not so important to resample most parameters in a way that is interleaved with stuff... (?)
	// FIXME -   Certainly, we aren't going to be interleaved with branches, anyway!
	if (P->load_value("MH_sampling",false))
	    sampler.add(5 + log(1+PP->t().n_branches()),MH_moves);
	else
	    sampler.add(1,MH_moves);
    }
    else
    {
	sampler.add(1, MH_moves);
    }

    // Add slice moves.
    if (P->load_value("slice_sampling", true))
	sampler.add(1,slice_moves);

    for(int i=0;i<loggers.size();i++)
	sampler.add_logger(loggers[i]);

    //------------------- Enable and Disable moves ---------------------------//
    enable_disable_transition_kernels(sampler,args);

    //------------------ Report status before starting MCMC -------------------//
    sampler.show_enabled(s_out);
    s_out<<"\n";

    if (is_parameters)
    {
	owned_ptr<Parameters> PP = P.as<Parameters>();
	//-------------------- Report alignment constraints -----------------------//
//	for(int i=0;i<PP->n_data_partitions();i++)
//	    std::cout<<"Partition "<<i+1<<": using "<<(*PP)[i].alignment_constraint().size1()<<" constraints.\n";
    
//	for(int i=0;i<PP->n_data_partitions();i++) 
//	{
//	    dynamic_bitset<> s2 = constraint_satisfied((*PP)[i].alignment_constraint(), (*PP)[i].A());
//	    dynamic_bitset<> s1(s2.size());
//	    report_constraints(s1,s2,i);
//	} 
    }
    
    if (P->load_value("AIS",false))
    {
	// before we do this, just run 20 iterations of a sampler that keeps the alignment fixed
	// - first, we need a way to change the tree on a sampler that has internal node sequences?
	// - well, this should be exactly the -t sampler.
	// - but then how do we copy stuff over?
	AIS_Sampler A(sampler);
	vector<double> beta(1,1);
	int AIS_temp_levels = P->load_value("AIS:levels",50);
	double AIS_temp_factor = P->load_value("AIS:factor",0.95);

	if (AIS_temp_factor >= 1)
	    throw myexception()<<"AIS:factor must be less than 1!";
	if (AIS_temp_factor <= 0)
	    throw myexception()<<"AIS:factor must be greater than 0!";

	for(int i=0;i<AIS_temp_levels;i++)
	    beta.push_back(beta.back()*AIS_temp_factor);
	beta.push_back(0);
    
	A.go(P,std::cerr,beta);
    }
    else
	sampler.go(P,subsample,max_iterations,s_out);
}
