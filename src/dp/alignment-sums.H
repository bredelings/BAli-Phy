/*
  Copyright (C) 2004-2005,2007,2009 Benjamin Redelings

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
/// \file alignment-sums.H
///
/// \brief Defines functions for sampling from and summing over parts of the alignment.
///

#ifndef ALIGNMENTSUMS_H
#define ALIGNMENTSUMS_H

#include <vector>
#include <memory>

#include <memory>                  // for std::shared_ptr
#include <vector>                  // for std::vector
#include <optional>                // for std::optional
#include "dp-engine.H"             // for DPengine
#include "util/math/log-double.H"  // for log_double_t
#ifndef NDEBUG_DP
#include "models/parameters.H"
#endif
class Parameters;
class data_partition;
namespace A5 { struct hmm_order; }

/// Sum of likelihoods for columns which don't contain any characters in sequences mentioned in 'nodes'
log_double_t other_subst(const data_partition& P, const std::vector<int>& nodes);

/// Sum of branch priors for branches not connected to no
log_double_t other_prior(const data_partition& P, const std::vector<int>& nodes);


/// Routine for simultaneously sampling between several Parameter choices, and summing out some nodes
int sample_two_nodes_multi(std::vector<Parameters>& p,const std::vector< A5::hmm_order >& orders,
			   const std::vector<log_double_t>& rho);

std::optional<log_double_t> sample_two_nodes_ratio(std::vector<Parameters>& p, const std::vector<A5::hmm_order>& order, const std::vector<log_double_t>& rho);

/// Routine for simultaneously sampling between several Parameter choices, and summing out some nodes
int sample_tri_multi(std::vector<Parameters>& p,const std::vector< std::vector<int> >& nodes,
		     const std::vector<log_double_t>& rho);


/// Routine for simultaneously sampling between several Parameter choices, and summing out some nodes
int sample_tri_multi(std::vector<Parameters>& p,const std::vector< std::vector<int> >& nodes,
		     const std::vector<log_double_t>& rho, std::optional<int> bandwidth);

/// Routine for simultaneously sampling between several Parameter choices, and summing out some nodes
int sample_cube_multi(std::vector<Parameters>& p,const std::vector< std::vector<int> >& nodes,
		      const std::vector<log_double_t>& rho);

std::optional<log_double_t> tri_sample_alignment_ratio(Parameters& P, int node1, int node2, std::optional<int> bandwidth = {});

struct sample_A3_multi_calculation
{
    std::vector<Parameters>& p;
#ifndef NDEBUG_DP
    const Parameters P0;
#endif
    std::vector<std::vector<int> > nodes;
    std::vector<log_double_t> rho;
    std::vector<std::vector<std::shared_ptr<DPengine> > > Matrices;
    std::vector<log_double_t> Pr;
    log_double_t C1;
    std::optional<int> bandwidth;

    virtual std::pair<std::shared_ptr<DPengine>,log_double_t> compute_matrix(int,int) = 0;

    void run_dp();

    sample_A3_multi_calculation(std::vector<Parameters>&,const std::vector< std::vector<int> >& nodes_,
                                std::optional<int> b);

    void set_proposal_probabilities(const std::vector<log_double_t>&);

    int choose(bool=true);
    virtual ~sample_A3_multi_calculation() {};
};

struct sample_tri_multi_calculation: public sample_A3_multi_calculation
{
    virtual std::pair<std::shared_ptr<DPengine>,log_double_t> compute_matrix(int,int);

    sample_tri_multi_calculation(std::vector<Parameters>&,const std::vector< std::vector<int> >& nodes_,
				 std::optional<int> b);
};

struct sample_cube_multi_calculation: public sample_A3_multi_calculation
{
    virtual std::pair<std::shared_ptr<DPengine>,log_double_t> compute_matrix(int,int);

    sample_cube_multi_calculation(std::vector<Parameters>&,const std::vector< std::vector<int> >& nodes_,
                                  std::optional<int> b);
};

//------------------- Checking Routines ------------------------//

void check_match_P(const data_partition& P, log_double_t OS, log_double_t OP, const std::vector<int>& path, const DPengine& Matrices);


std::vector<log_double_t> sample_P(const data_partition& P, log_double_t P_choice, log_double_t rho,
				   const std::vector<int>& path, const DPengine& Matrices);

void check_sampling_probabilities(const std::vector< std::vector<log_double_t> >& PR);

#endif
