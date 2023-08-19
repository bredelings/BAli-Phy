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

/**
 * @file time.cc
 */

#include "util/time.H"
#include <sstream>
#include <iomanip>
#include "util/assert.hh"
#include "util/string/convert.H"

#include <boost/chrono.hpp>

#include <fmt/chrono.h>

namespace chrono = std::chrono;

using std::string;

/*
   FIXME: We aren't using std::chrono because only boost::chrono
   supplies a clock that measures CPU time in a cross-platform way.

   To generate a human-readable elapsed time using std::chrono see:
      https://stackoverflow.com/questions/22590821/convert-stdduration-to-human-readable-time
 */

using boost::chrono::process_user_cpu_clock;

const auto start_cpu_time = process_user_cpu_clock::now();

duration_t total_cpu_time()
{
    // std::chrono doesn't have any way of measuring cpu time.
    auto delta = process_user_cpu_clock::now() - start_cpu_time;
    return duration_t(delta.count());
}

string duration_string(duration_t t)
{
    typedef std::chrono::duration<float> fsec;

    double total_secs = fsec(t).count();

    auto d = chrono::duration_cast<chrono::days>(t);
    t -= d;

    auto h = chrono::duration_cast<chrono::hours>(t);
    t -= h;

    auto m = chrono::duration_cast<chrono::minutes>(t);
    t -= m;

    auto s = chrono::duration_cast<chrono::seconds>(t);
    t -= m;

    string out = "(" + fmt::format("{:.3f}",total_secs) + "s)";
    out = convertToString(s.count()) + "s " + out;

    if (m.count() or h.count() or d.count())
	out = convertToString(m.count()) + "m " + out;

    if (h.count() or d.count())
	out = convertToString(h.count()) + "h " + out;

    if (d.count())
	out = convertToString(d.count()) + "days " + out;

    return out;
}

