#include "version.H"

#ifndef BALI_PHY_VERSION
#define BALI_PHY_VERSION "UNKNOWN"
#endif

#include <iostream>

void print_version() 
{
  std::cout<<"VERSION: "<<BALI_PHY_VERSION<<"\nBUILD: "<<__DATE__<<" "<<__TIME__<<std::endl;
}
