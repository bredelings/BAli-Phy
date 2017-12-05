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

#include "myexception.H"
#include <cstdlib>
#include <vector>
using namespace std;

#ifdef __linux__
#include <execinfo.h>


vector<string> stack_trace(int ignore) {
  // Acquire the stack trace...
  void * array[25];

  size_t entries = backtrace( array, sizeof( array ) / sizeof( void* ) );
  char ** symbols = backtrace_symbols( array, entries );

  if ( symbols == 0 ) {
    // Probable out of memory condition; we'll skip
    // over the full stack.
    entries = 0;
  }

  // Throw it into our Vector.  
  // We are skipping the top-most element, since its this function.
  vector<string> trace;
  trace.reserve( entries > ignore ? entries - ignore : 0 );
  for ( size_t i = ignore; i < entries; i++ )
    trace.push_back( symbols[i] );
  
  // Free up the allocated memory.
  free( symbols );

  return trace;
}

string show_stack_trace(int ignore) {
  vector<string> trace = stack_trace(ignore+1);

  string s;
  for(int i=0;i<trace.size();i++)
    s += trace[i] + "\n";

  return s;
}

#else

  string show_stack_trace(int) {return string();}

#endif
