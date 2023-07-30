/*
  Copyright (C) 2011 Benjamin Redelings

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
/// \file   logger.H
/// \brief  Provides classes for constructing logging functions for MCMC samplers.
///
/// This file provides classes for constructing logging functions for MCMC samplers.
/// These loggers report samples from the running chain, as well as various statistics.
/// Writing to files is mostly separated from the generation of the text that will be
/// written.
///
/// \author Benjamin Redelings
/// 

#ifndef LOGGER_H
#define LOGGER_H

#include <iostream>
#include <vector>
#include <string>
#include <filesystem>

#include "models/parameters.H"

struct slice_function;

namespace MCMC {

    int alignment_length(const data_partition& P);

    int alignment_length(const Parameters& P);
}

#endif
