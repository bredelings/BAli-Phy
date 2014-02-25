/*
   Copyright (C) 2010 Benjamin Redelings

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
/// \file parameters.H
///
/// \brief This file defines the generic Probability_Model class.
///

#ifndef PROBABILITY_MODEL_H
#define PROBABILITY_MODEL_H

#include <map>
#include <string>
#include "models/model.H"
#include "cow-ptr.H"

// If we add a distribution class, note that we may need to have distribution EXPRESSIONS,
// so that we an construct a distribution by plugging into parameters or constant to the
// parameter slots for the distribution.

/// A Model with member functions for probability
class Probability_Model: public Model
{
public:
  /// An object for key-value pairs.
  cow_ptr< std::map<std::string,double> > keys;

  bool contains_key(const std::string& key) const { return keys->count(key);}
  double lookup_key(const std::string& key) const { return keys->find(key)->second;}
  double load_value(const std::string& key,double v) const 
  { 
    auto loc = keys->find(key);
    if (loc != keys->end())
      return loc->second;
    else
      return v;
  }

  virtual Probability_Model* clone() const =0;

  virtual double get_beta() const {return 1.0;}

  virtual void set_beta(double) {std::abort();}

  // The prior, likelihood, and probability
  //  (We inherit 'this->prior()' from class Model)
  virtual log_double_t probability() const {return prior() * likelihood();}
  virtual log_double_t likelihood() const {return 1.0;}

  virtual log_double_t heated_prior() const {return prior();}
  virtual log_double_t heated_likelihood() const {return likelihood();}
  virtual log_double_t heated_probability() const {return heated_prior() * heated_likelihood();}

  Probability_Model(const module_loader& L)
    :Model(L),keys(new std::map<std::string,double>)
  {}
};


#endif
