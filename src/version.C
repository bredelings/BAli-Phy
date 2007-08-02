#include "version.H"
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION "UNKNOWN"
#endif

#include <iostream>

void print_version_info(std::ostream& file) 
{
  file<<"VERSION: "<<PACKAGE_VERSION<<"\nBUILD: "<<__DATE__<<" "<<__TIME__<<std::endl;
}
