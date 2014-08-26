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
/// \file   mcmc/setup.H
/// \brief  Provides routines to create default transition kernels and start a Markov chain.
///
/// The function do_sampling( ) creates transition kernel for known parameter names.
/// It then starts the Markov chain for the MCMC run and runs it for a specified
/// number of iterations.
///
/// \author Benjamin Redelings
/// 

#ifndef SETUP_MCMC_H
#define SETUP_MCMC_H

#include <boost/program_options.hpp>
#include <boost/dynamic_bitset.hpp>
#include <vector>
#include <iostream>
#include <string>

#include "models/parameters.H"
#include "mcmc.H"
#include "slice-sampling.H"
#include "proposals.H"


void avoid_zero_likelihood(owned_ptr<Model>& P,std::ostream&, std::ostream&);

void do_pre_burnin(const boost::program_options::variables_map& args,
		   owned_ptr<Model>& P,std::ostream&, std::ostream&);

void do_sampling(const boost::program_options::variables_map& args,
		 owned_ptr<Model>& P,
		 long int max_iterations,
		 std::ostream& files,
		 const std::vector<owned_ptr<MCMC::Logger> >&);
#endif
