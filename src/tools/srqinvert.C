/*
   Copyright (C) 2004,2008 Benjamin Redelings

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

#include <iostream>
#include <vector>
#include "myexception.H"
using namespace std;

int main(int argc,char* argv[]) { 
  try {

    int i;
    while(cin>>i) {
      if (i < 0 or i> 1)
	throw myexception()<<"Found an entry not 0 or 1!";
      std::cout<<(1-i)<<"\n";
    }
  }
  catch (std::exception& e) {
    std::cerr<<"srqinvert: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
