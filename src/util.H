#ifndef UTIL_H
#define UTIL_H

#include <string>
#include <vector>
#include <sstream>
#include <cassert>
#include <algorithm>
#include "myexception.H"

#define foreach(a,b) for(typeof((b).begin()) a=(b).begin();a != (b).end();a++)

// over-ride less<string> to make this a binary predicate?
struct lstr {
  bool operator()(const std::string& s1, const std::string& s2) const {
   return strcmp(s1.c_str(), s2.c_str()) < 0;
  }
};

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

template <class T>
std::vector<int> compute_mapping(const std::vector<T>& v1,const std::vector<T>& v2) {
  assert(v1.size() == v2.size());

  std::vector<int> mapping(v1.size());
  for(int i=0;i<v1.size();i++) {
    int target = -1;
    for(int j=0;j<v2.size();j++) {
      if (v1[i] == v2[j]) {
	target = j;
	break;
      }
    }
    if (target == -1)
      throw myexception()<<"Vector v2 does not contain all elements of v1!";
    mapping[i] = target;
  }

  return mapping;
}

template <class T>
std::vector<T> apply_mapping(const std::vector<T>& v1,const std::vector<int>& mapping) {
  std::vector<T> v2(v1.size());
  for(int i=0;i<v2.size();i++)
    v2[mapping[i]] = v1[i];

  return v2;
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

template <typename T>
bool includes(const std::vector<T>& v, const T& t) {
  return (std::find(v.begin(),v.end(),t) != v.end());
}

template <typename T>
bool includes(const std::vector<T>& v1, const std::vector<T>& v2) {
  for(int i=0;i<v2.size();i++)
    if (not includes(v1,v2[i]))
      return false;
  return true;
}

std::string join(const std::vector<std::string>&,char);

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

std::vector<std::string> truncate_names(const std::vector<std::string>&);

#endif
