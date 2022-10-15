#include "system.H"
#include "util/string/convert.H"
#include "util/log-level.H"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_SYS_RESOURCE_H
extern "C" {
#include <sys/resource.h>
}
#endif

#include <string>
#include <csignal>


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

    if (log_verbose >= 1) {
        o<<"OLD cpu time limits = "<<rlim_minutes(limits.rlim_cur)<<" / "<<rlim_minutes(limits.rlim_max)<<endl;
    }

    limits.rlim_cur = RLIM_INFINITY;

    setrlimit(RLIMIT_CPU,&limits);
    getrlimit(RLIMIT_CPU,&limits);

    if (log_verbose >= 1)
    {
        o<<"NEW cpu time limits = "<<rlim_minutes(limits.rlim_cur)<<" / "<<rlim_minutes(limits.rlim_max)<<endl;
        o<<endl;
    }
}
#else
void raise_cpu_limit(ostream& /*o*/) 
{
    //  o<<"Not checking CPU time limits..."<<endl;
}
#endif

void show_ending_messages(bool);

sig_atomic_t fatal_error_in_progress = 0;

void die_on_signal(int sig)
{
    // 1. This could in theory get invoked recursively.  So handle that.
    if (fatal_error_in_progress)
        raise (sig);
    fatal_error_in_progress = 1;


    // 2. Payload.

    // 2.1 Throwing exceptions from signal handlers is not allowed.  Bummer.
    cout<<"received signal "<<sig<<".  Dying."<<endl;
    cerr<<"received signal "<<sig<<".  Dying."<<endl;

    // 2.2 Show messages.
    show_ending_messages(true);

    // 3. Supposedly this is cleaner than calling exit or abort
    //    because it sets the return code correctly.
    signal (sig, SIG_DFL);
    raise (sig);
}

void block_signals()
{
#if !defined(_MSC_VER) && !defined(__MINGW32__)
    // 1. IGNORE signals SIGHUP and SIGXCPU
    signal(SIGHUP,SIG_IGN);
    signal(SIGXCPU,SIG_IGN);
      
    // 2. Install a handler to print a message before quitting for signals SIGINT and SIGTERM.
    struct sigaction sa_old;
    struct sigaction sa_new;

    sa_new.sa_handler = &die_on_signal; // signal handler.
    sigemptyset(&sa_new.sa_mask);       // signals to block while the handler runs
    sa_new.sa_flags = 0;                // flags

    sigaction(SIGINT, nullptr, &sa_old);
    if (sa_old.sa_handler != SIG_IGN)
        sigaction(SIGINT,&sa_new, nullptr);

    sigaction(SIGTERM,nullptr,&sa_old);
    if (sa_old.sa_handler != SIG_IGN)
        sigaction(SIGTERM,&sa_new, nullptr);
#endif
}
