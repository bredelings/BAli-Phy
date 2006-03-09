#ifndef UTIL_H
#define UTIL_H

#include <string>
#include <vector>
#include <sstream>
#include <cassert>
#include <algorithm>
#include "myexception.H"
#include <map>

#define foreach(a,b) for(typeof((b).begin()) a=(b).begin();a != (b).end();a++)

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

///Reflect x at boundaries 0 and max to get a number in [0,max]
template <typename T>
inline T wrap(T x,T max) {
  if (max == 0)
    return 0;

  // flip around to position x axis
  if (x < 0)
    x = -x;

  // map to [0,2*max)
  int n = (int)(x/(2*max));
  x -= n*2*max;

  if (x > max)
    x = max*2 - x;

  assert(x >= 0 and x <= max);
  return x;
}

/// Reflect x at boundaries min and max to get a number in [min,max]
template <typename T>
inline T wrap(T x, T min, T max) {
  return wrap(x-min,max-min)+min;
}

/// Get index of t in v
template <typename T>
int find_index(const std::vector<T>& v,const T& t) {
  return std::find(v.begin(),v.end(),t) - v.begin();
}

/// Does v include t?
template <typename T>
bool includes(const std::vector<T>& v, const T& t) {
  return (std::find(v.begin(),v.end(),t) != v.end());
}

/// Does v1 include every member of v2?
template <typename T>
bool includes(const std::vector<T>& v1, const std::vector<T>& v2) {
  for(int i=0;i<v2.size();i++)
    if (not includes(v1,v2[i]))
      return false;
  return true;
}

/// Concatenate members of v, separating by char c
std::string join(const std::vector<std::string>& v,char c);

template<class T>
std::string join(const std::vector<T>& vT, char c)
{
  std::vector<std::string> v;
  for(int i=0;i<vT.size();i++)
    v.push_back(convertToString(vT[i]));
  return join(v,c);
}

/// Split the line into pieces at c
std::vector<std::string> split(const std::string& line,char c);

/// Split the line into pieces at c and convert to T
template<class T>
std::vector<T> split(const std::string& s,char c) {
  std::vector<std::string> v = split(s,c);
  std::vector<T> vT;
  for(int i=0;i<v.size();i++)
    vT.push_back(convertTo<T>(v[i]));

  return vT;
}

/// Remove all occurrences of char c from line
std::string strip(const std::string& line,char c);
/// Remove all occurrences of each char in chars from line
std::string strip(const std::string& line,const std::string& chars);

/// invert the mapping
std::vector<int> invert(const std::vector<int>&);

/// compose the two mappings
std::vector<int> compose(const std::vector<int>&,const std::vector<int>&);

/// Return a random permutation of the list
std::vector<int> randomize(const std::vector<int>&);

/// Return the min of v
template<class T>
T min(const std::vector<T>& v) {
  T t = v[0];
  for(int i=1;i<v.size();i++)
    t = std::min(t,v[i]);

  return t;
}

/// Return the arg-min of v
template<class T>
unsigned argmin(const std::vector<T>& v) {
  unsigned argmin = 0;
  for(unsigned i=1;i<v.size();i++)
    if (v[i] < v[argmin])
      argmin = i;

  return argmin;
}

/// Return the min of v
template<class T>
T max(const std::vector<T>& v) {
  T t = v[0];
  for(int i=1;i<v.size();i++)
    t = std::max(t,v[i]);

  return t;
}

/// Return the arg-max of v
template<class T>
unsigned argmax(const std::vector<T>& v) {
  unsigned argmax = 0;
  for(unsigned i=1;i<v.size();i++)
    if (v[i] > v[argmax])
      argmax = i;

  return argmax;
}

/// Order i~j by v[i]~v[j]
template <typename T>
struct sequence_order {
  const std::vector<T>& v;

  sequence_order(const std::vector<T>& v1):v(v1) {}

  bool operator()(int i,int j) const {
    return v[i]<v[j];
  }
};

/// If m contains name, convert m[name] to T and return.  Otherwise return value.
template <typename U, typename T>
const U& loadvalue(std::map<std::string,U,T>& m,const std::string& name, const U& value) {
  if (m.find(name) != m.end() )
    return m[name];
  else
    return value;
}

/// get the next word starting at position i, return true if not done.
bool get_word(std::string& word, int& i,const std::string& s,
	      const std::string& delimiters,const std::string& whitespace);

template<typename T>
std::vector<T> select(const std::vector<T>& v1,bool pred(const T&))
{
  std::vector<T> v2;
  for(int i=0;i<v1.size();i++)
    if (pred(v1[i]))
      v2.push_back(v1[i]);
  return v2;
}

template<typename T>
std::vector<T> select(const std::vector<T>& v1,bool (T::*pred)() const)
{
  std::vector<T> v2;
  for(int i=0;i<v1.size();i++)
    if ((v1[i].*pred)())
      v2.push_back(v1[i]);
  return v2;
}

template<typename T>
unsigned count(const std::vector<T>& v1,bool pred(const T&))
{
  unsigned total=0;
  for(int i=0;i<v1.size();i++)
    if (pred(v1[i]))
      total++;
  return total;
}

template<typename T>
unsigned count(const std::vector<T>& v1,bool (T::*pred)() const)
{
  unsigned total=0;
  for(int i=0;i<v1.size();i++)
    if ((v1[i].*pred)())
      total++;
  return total;
}

#endif
