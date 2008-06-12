#include "version.H"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "revision.H"

#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION "UNKNOWN"
#endif

#include <iostream>
#include <string>

using namespace std;

void print_version_info(ostream& file) 
{
  file<<"VERSION: "<<PACKAGE_VERSION;
#ifdef REVISION
  string svn_rev = REVISION;
  if (not svn_rev.empty())
    file<<"   ["<<svn_rev<<"]";
#endif

#ifdef REVISION_DATE
  string svn_date = REVISION_DATE;
  if (not svn_date.empty())
    file<<"   ("<<svn_date<<")";
#endif

  file<<endl;
  file<<"BUILD: "<<__DATE__<<" "<<__TIME__<<endl;
#ifdef _ARCH_
  file<<"ARCH: "<<_ARCH_<<endl;
#endif

#ifdef __GNUC__
  file<<"COMPILER: GCC "<<__VERSION__<<endl;
#endif

#ifdef _CXXFLAGS_
  file<<"FLAGS: "<<_CXXFLAGS_<<endl;
#endif
}
