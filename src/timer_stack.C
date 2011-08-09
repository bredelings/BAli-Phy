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

using namespace std;
using namespace boost::chrono;

/// This timer stack is a global variable that is always available.
timer_stack default_timer_stack;

duration_t total_cpu_time()
{
  return process_user_cpu_clock::now() - process_user_cpu_clock::time_point();
}

string duration_string(seconds t)
{
  unsigned long T = t.count();

  string s = convertToString(t);

  unsigned long seconds = T%60;
  T = (T - seconds)/60;

  // return if 0 minutes
  if (not T) return s;

  unsigned long minutes = T%60;
  T  = (T - minutes)/60;

  s = convertToString(minutes) + "m " +
      convertToString(seconds) + "s  (" + s + ")";

  // return if 0 hours
  if (not T) return s;

  unsigned long hours = T%24;
  T  = (T - hours)/24;

  s = convertToString(hours) + "h " + s;

  // return if 0 days
  if (not T) return s;

  unsigned long days = T;

  s = convertToString(days) + "days " + s;

  return s;
}

timer_stack::container_t::iterator timer_stack::lookup_profile(const string& s)
{
  container_t::iterator record = total_times.find(s);
  if (record == total_times.end()) {
    total_times.insert(container_t::value_type(s,region_profile()));
    record = total_times.find(s);
    assert(record != total_times.end());
  }
  return record;
}

void timer_stack::credit_active_timers()
{
  assert(record_stack.size() == start_time_stack.size());

  duration_t now = total_cpu_time();
  for(int i=0;i<n_active_timers();i++)
  {
    duration_t elapsed = now - start_time_stack[i];
    record_stack[i]->second.duration += elapsed;
    start_time_stack[i] = now;
  }
}

void timer_stack::push_timer(const string& s)
{
  start_time_stack.push_back( total_cpu_time() );
  container_t::iterator record = lookup_profile(s);
  record->second.n_calls++;
  record_stack.push_back(record);
}

void timer_stack::pop_timer()
{
  if (record_stack.empty()) throw myexception()<<"Trying to remove a non-existent timer!";
  duration_t start = start_time_stack.back();
  start_time_stack.pop_back();

  container_t::iterator record = record_stack.back();
  record_stack.pop_back();

  duration_t end = total_cpu_time();

  record->second.duration += (end-start);
}

string timer_stack::report()
{
  credit_active_timers();

  ostringstream o;

  duration_t T = total_cpu_time();

  vector<duration_t> times(total_times.size());
  vector<container_t::iterator> records(total_times.size());
  int j=0;
  for(container_t::iterator i = total_times.begin();i != total_times.end();i++,j++) {
    records[j] = i;
    times[j] = i->second.duration;
  }
  vector<int> order = iota<int>(total_times.size());
  sort(order.begin(), order.end(), sequence_order<duration_t>(times) );
  std::reverse(order.begin(), order.end());

  o.precision(3);
  for(int r=0;r<records.size();r++)
  {
    container_t::iterator i = records[order[r]];
    duration_t t = i->second.duration;

    o<<setw(5)<<(t*100/T)<<"%"
     <<"         "<<setw(6)<<t<<" sec"
     <<"         "<<setw(8)<<i->second.n_calls
     <<"         "<<i->first<<"\n";
  }

  if (total_times.empty())
    o<<"   CPU time profiles: no data.\n";

  return o.str();
}
