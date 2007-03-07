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
