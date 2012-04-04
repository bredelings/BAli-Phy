/*
   Copyright (C) 2004 Benjamin Redelings

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

#ifndef MATCACHE_H
#define MATCACHE_H

#include <vector>
#include <boost/shared_ptr.hpp>
#include "alphabet.H"
#include "mytypes.H"

struct Mat_Cache
{
  virtual const std::vector< Matrix>& transition_P(int b) const=0;

  virtual int n_base_models() const = 0;

  // Perhaps this should really be per-mixture-component.
  virtual int n_states() const = 0;

  // Perhaps this should also be per-mixture-component.
  virtual std::vector<unsigned> state_letters() const = 0;

  virtual std::vector<double> distribution() const = 0;

  virtual boost::shared_ptr<const Object> base_model(int m) const = 0;

  virtual std::vector<double> frequencies(int m) const = 0;

  virtual const alphabet& get_alphabet() const = 0;

  void WeightedFrequencyMatrix(Matrix&) const;

  Matrix WeightedFrequencyMatrix() const;

  void FrequencyMatrix(Matrix&) const;

  Matrix FrequencyMatrix() const;
};

#endif
