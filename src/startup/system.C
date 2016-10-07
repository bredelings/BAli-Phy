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
#include <signal.h>

#include "util.H"

using std::cerr;
using std::cout;
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
  //  o<<"Not checking CPU time limits..."<<endl;
}
#endif

void show_ending_messages(bool);

void die_on_signal(int sig)
{
  // Throwing exceptions from signal handlers is not allowed.  Bummer.
  cout<<"received signal "<<sig<<".  Dying."<<endl;
  cerr<<"received signal "<<sig<<".  Dying."<<endl;

  show_ending_messages(true);

  exit(3);
}

void block_signals()
{
#if !defined(_MSC_VER) && !defined(__MINGW32__)
  signal(SIGHUP,SIG_IGN);
  signal(SIGXCPU,SIG_IGN);
      
  struct sigaction sa_old;
  struct sigaction sa_new;
  sa_new.sa_handler = &die_on_signal;

  sigaction(SIGINT,NULL,&sa_old);
  if (sa_old.sa_handler != SIG_IGN)
    sigaction(SIGINT,&sa_new,NULL);

  sigaction(SIGTERM,NULL,&sa_old);
  if (sa_old.sa_handler != SIG_IGN)
    sigaction(SIGTERM,&sa_new,NULL);
#endif
}
