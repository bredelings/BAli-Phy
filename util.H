#ifndef UTIL_H
#define UTIL_H

#include <string>
#include <vector>
#include <sstream>

template <class T>
T convertTo(const std::string& s) { 
  std::istringstream i(s);
  T t;
  i>>t;
  return t;
}

template <class T>
std::string convertToString(T t)
{
  std::ostringstream o;
  o.flags(o.flags() | std::ios::fixed);
  
  if (o << t)
    return o.str();
  // some sort of error handling goes here...
  return "conversion error";
} 

std::vector<std::string> split(const std::string&,char);

std::string strip(const std::string&,char);

std::vector<int> invert(const std::vector<int>&);

#endif
