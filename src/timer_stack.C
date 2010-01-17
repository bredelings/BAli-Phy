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
 * @file timer_stack.C
 */

#include "timer_stack.H"
#include <sstream>
#include <iomanip>
#include <cassert>
#include "util.H"

#include "config.h"

#ifdef HAVE_SYS_RESOURCE_H
extern "C" {
#include <sys/resource.h>
}
#endif

using namespace std;

/// This timer stack is a global variable that is always available.
timer_stack default_timer_stack;

double total_time(const timeval& t)
{
  double T = t.tv_sec;
  T += double(t.tv_usec)/1000000;
  return T;
}

time_point_t total_cpu_time()
{
#ifdef HAVE_SYS_RESOURCE_H
  struct rusage R;        

  getrusage(RUSAGE_SELF, &R);

  return total_time(R.ru_utime)+total_time(R.ru_stime);  
#else
  return 0;
#endif
}

string duration(time_t T)
{
  time_t total = T;
  unsigned long seconds = T%60;
  T = (T - seconds)/60;

  unsigned long minutes = T%60;
  T  = (T - minutes)/60;

  unsigned long hours = T%24;
  T  = (T - hours)/24;

  unsigned long days = T;

  string s = convertToString(total) + " seconds";

  if (not minutes) return s;

  s = convertToString(minutes) + "m " +
      convertToString(seconds) + "s  (" + s + ")";

  if (not hours) return s;

  s = convertToString(hours) + "h " + s;

  if (not days) return s;

  s = convertToString(days) + "days " + s;

  return s;
}

region_profile& timer_stack::lookup_profile(const string& s)
{
  typedef map<string,region_profile> container_t;
  container_t::iterator record = total_times.find(s);
  if (record == total_times.end()) {
    total_times.insert(container_t::value_type(s,region_profile()));
    record = total_times.find(s);
    assert(record != total_times.end());
  }
  return record->second;
}

void timer_stack::add_duration(const string& s, duration_t d)
{
  lookup_profile(s).duration += d;
}

void timer_stack::inc_calls(const string& s)
{
  lookup_profile(s).n_calls++;
}

void timer_stack::credit_active_timers()
{
  assert(name_stack.size() == start_time_stack.size());

  double now = total_cpu_time();
  for(int i=0;i<n_active_timers();i++)
  {
    double elapsed = now - start_time_stack[i];
    add_duration(name_stack[i], elapsed);
    start_time_stack[i] = now;
  }
}

void timer_stack::push_timer(const string& s)
{
  name_stack.push_back(s);
  inc_calls(s);
  start_time_stack.push_back( total_cpu_time() );
}

void timer_stack::pop_timer()
{
  time_point_t end = total_cpu_time();
  time_point_t start = start_time_stack.back();

  add_duration(name_stack.back(), end - start);

  name_stack.pop_back();
  start_time_stack.pop_back();
}

string timer_stack::report()
{
  typedef map<string,region_profile> container_t;

  credit_active_timers();

  ostringstream o;

  double T = total_cpu_time();


  // FIXME -- all decimals here should be xxxxx.xx -- pad to same length to align the dots
  // FIXME - fix 1.13e+03 format to 1130 (or something).  Didn't I figure this out somewhere?
  o.precision(3);
  for(container_t::iterator i = total_times.begin();i != total_times.end();i++)
  {
    double t = i->second.duration;
    o<<setw(5)<<(t*100/T)<<"%"
     <<"         "<<setw(6)<<t<<" sec"
     <<"         "<<setw(8)<<i->second.n_calls
     <<"         "<<i->first<<"\n";
  }

  if (total_times.empty())
    o<<"   CPU time profiles: no data.\n";

  return o.str();
}
