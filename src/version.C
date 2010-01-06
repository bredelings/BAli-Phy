/*
   Copyright (C) 2007-2008 Benjamin Redelings

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

#include "version.H"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "git_version.h"

#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION "UNKNOWN"
#endif

#include <iostream>
#include <string>

using namespace std;

void print_version_info(ostream& file) 
{
  file<<"VERSION: "<<PACKAGE_VERSION;
#ifdef GIT_MESSAGE
  string git_rev = GIT_MESSAGE;
  if (not git_rev.empty()) {
    file<<"  ["<<git_rev<<"]";
#ifdef GIT_COMMIT_DATE
    file<<"  ("<<GIT_COMMIT_DATE<<")";
#endif
  }
#endif

  file<<endl;
  file<<"BUILD: "<<__DATE__<<" "<<__TIME__<<endl;
#ifdef _ARCH_
  file<<"ARCH: "<<_ARCH_<<endl;
#endif

#ifdef __GNUC__
  file<<"COMPILER: GCC "<<__VERSION__<<endl;
#endif

#ifdef CONFIG_FLAGS
  file<<"FLAGS: "<<CONFIG_FLAGS<<endl;
#endif
}
