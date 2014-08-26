#include "startup/system.H"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_SYS_RESOURCE_H
extern "C" {
#include <sys/resource.h>
}
#endif

#include <string>

#include "util.H"

using std::string;
using std::ostream;
using std::endl;

#if defined(HAVE_SYS_RESOURCE_H)
string rlim_minutes(rlim_t val)
{
  if (val == RLIM_INFINITY)
    return "unlimited";
  else
    return convertToString<>(val/60) + " minutes";
}

void raise_cpu_limit(ostream& o)
{
  rlimit limits;

  getrlimit(RLIMIT_CPU,&limits);

  if (log_verbose) {
    o<<endl;
    o<<"OLD cpu time limits = "<<rlim_minutes(limits.rlim_cur)<<" / "<<rlim_minutes(limits.rlim_max)<<endl;
  }

  limits.rlim_cur = RLIM_INFINITY;

  setrlimit(RLIMIT_CPU,&limits);
  getrlimit(RLIMIT_CPU,&limits);

  if (log_verbose)
    o<<"NEW cpu time limits = "<<rlim_minutes(limits.rlim_cur)<<" / "<<rlim_minutes(limits.rlim_max)<<endl;
}
#else
void raise_cpu_limit(ostream& o) 
{
  o<<"Not checking CPU time limits..."<<endl;
}
#endif

