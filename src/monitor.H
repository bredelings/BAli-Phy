/*
   Copyright (C) 2004-2007 Benjamin Redelings

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

#ifndef MONITOR_H
#define MONITOR_H

#include <iostream>
#include "models/parameters.H"
#include <string>
#include "models/model.H"
#include "substitution/matcache.H"

void print_stats(std::ostream& o,const Probability_Model& M,bool=true);

void show_frequencies(std::ostream& o,const alphabet& a,const std::valarray<double>&);
void show_frequencies(std::ostream& o,const Mat_Cache& MModel);
void show_smodel(std::ostream& o, const Mat_Cache& MModel);
void show_smodels(std::ostream& o, const Parameters& P);

void report_mem();

#endif
