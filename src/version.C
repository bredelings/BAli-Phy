#include "version.H"
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION "UNKNOWN"
#endif

#include <iostream>
#include "revision.H"
#include <string>

using namespace std;

void print_version_info(ostream& file) 
{
  string svn_date = SVN_LAST_CHANGED_DATE;
  svn_date = svn_date.substr(1,25);
  string svn_rev = SVN_LAST_CHANGED_REV;
  svn_rev = svn_rev.substr(1,svn_rev.size()-3);

  file<<"VERSION: "<<PACKAGE_VERSION;
  file<<"    ("<<svn_rev<<")";
  file<<"    ("<<svn_date<<")\n";
  file<<"BUILD: "<<__DATE__<<" "<<__TIME__<<endl;
}
