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

#include "git-version.h"

#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION "UNKNOWN"
#endif

#include <iostream>
#include <string>
#include <sstream>

using namespace std;

string version()
{
    ostringstream s;
    s<<PACKAGE_VERSION;
    return s.str();
}

string git_rev()
{
    ostringstream s;
#ifdef GIT_MESSAGE
    s<<GIT_MESSAGE;
#endif
    return s.str();
}

string git_commit_date()
{
    ostringstream s;
#ifdef GIT_COMMIT_DATE
    s<<GIT_COMMIT_DATE;
#endif
    return s.str();
}


string build_date()
{
    ostringstream s;
    s<<__DATE__<<" "<<__TIME__;
    return s.str();
}

string arch()
{
    ostringstream s;
#ifdef _ARCH_
    s<<_ARCH_;
#endif
    return s.str();
}

string compiler()
{
    ostringstream s;
#ifdef _COMPILER_
    s<<_COMPILER_;
#elif __GNUC__
    s<<"GCC "<<__VERSION__;
#endif
    return s.str();
}

string config_flags()
{
    ostringstream s;
#ifdef CONFIG_FLAGS
    s<<CONFIG_FLAGS;
#endif
    return s.str();
}

void print_version_info(ostream& file) 
{
    file<<"VERSION: "<<version();
    if (not git_rev().empty())
    {
	if (git_rev()[0] == '[')
	    file<<"  "<<git_rev()<<"";
	else
	    file<<"  ["<<git_rev()<<"]";
    }
    if (git_commit_date().size())
	file<<"  ("<<git_commit_date()<<")";
    file<<endl;

    file<<"BUILD: "<<build_date()<<endl;
    if (arch().size())
	file<<"ARCH: "<<arch()<<endl;
    if (compiler().size())
	file<<"COMPILER: "<<compiler()<<endl;
    if (config_flags().size())
	file<<"FLAGS: "<<config_flags()<<endl;
}

json::object version_info() 
{
    json::object info;
    info["version"] = version();
    info["build-date"] = build_date();
    if (arch().size())
	info["arch"] = arch();
    if (compiler().size())
	info["compiler"] = compiler();
    if (config_flags().size())
	info["compile flags"] = config_flags();
    if (git_rev().size())
	info["revision"] = git_rev();
    if (git_commit_date().size())
	info["revision-date"] = git_commit_date();

    return info;
}
