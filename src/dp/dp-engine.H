/*
   Copyright (C) 2004-2005,2007,2010 Benjamin Redelings

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

/**
 * @file dp-engine.H
 *
 * @brief This file contains a generic dynamic programming class.
 *
 */

#ifndef DPMATRIX_H
#define DPMATRIX_H

#include <optional>
#include <vector>
#include "dp_hmm.H"

/// A generic class for performing dynamic programming computations with an HMM
class DPengine: public dp_HMM 
{
protected:
  log_double_t Pr_total;

public:
  /// Sample a path from the HMM
  virtual std::optional<std::vector<int>> sample_path() const =0;

  /// Calculates the (log) probability of all possible paths through theHMM
  virtual log_double_t Pr_sum_all_paths() const;

  /// Calculates the (log) probability that this path is sampled by the generalized HMM
  virtual log_double_t path_P(const std::vector<int>& g_path) const =0;

  /// Calculate the (log) substitution probability along the path
  virtual log_double_t path_Q_subst(const std::vector<int>& g_path) const=0;

  /// Calculate the (log) probability of this path
  virtual log_double_t path_Q(const std::vector<int>& g_path) const {
    return path_GQ_path(g_path) * this->path_Q_subst(g_path);
  }

  void check_sampling_probability(const std::vector<int>& g_path) const;

  DPengine(const HMM&);
};

#endif
