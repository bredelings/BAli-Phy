#ifndef UTIL_H
#define UTIL_H

#include <string>
#include <vector>
#include <sstream>
#include <cassert>
#include <algorithm>

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

inline double wrap(double x,double max) {
  if (max == 0)
    return 0;

  // flip around to position x axis
  if (x < 0)
    x = -x;

  // map to [0,2*max)
  int n = (int)(x/(2.0*max));
  x -= n*2.0*max;

  if (x > max)
    x = max*2 - x;

  assert(x >= 0 and x <= max);
  return x;
}

template <typename T>
int find_index(const std::vector<T>& v,const T& t) {
  return std::find(v.begin(),v.end(),t) - v.begin();
}

std::vector<std::string> split(const std::string&,char);

template<class T>
std::vector<T> split(const std::string& s,char c) {
  std::vector<std::string> v = split(s,c);
  std::vector<T> vT;
  for(int i=0;i<v.size();i++)
    vT.push_back(convertTo<T>(v[i]));

  return vT;
}

std::string strip(const std::string&,char);
std::string strip(const std::string&,const std::string&);

std::vector<int> invert(const std::vector<int>&);

std::vector<int> compose(const std::vector<int>&,const std::vector<int>&);

std::vector<int> randomize(const std::vector<int>&);

#endif
