/*
  Copyright (C) 2004-2009 Benjamin Redelings

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

/**
 * @file util.H
 *
 * @brief This file contains a number of utility routines.
 *
 */

#ifndef UTIL_H
#define UTIL_H

#include <string>
#include <vector>
#include <sstream>
#include "util/assert.hh"
#include <algorithm>
#include "util/myexception.H"
#include <optional>
#include <map>
#include <set>
#include <typeinfo>
#include <functional>
#include "string_convert.H"

/// How many of v's bits are true?
int n_elements(const std::vector<bool>& v);

bool in_order(const std::vector<int>& order);

template<class T>
struct bad_mapping: public myexception
{
    T missing;

    int from;

    int size1;

    int size2;

    void flip() 
	{
	    from = 1-from;
	    std::swap(size1,size2);
	    gen_message();
	}

    void gen_message()
	{
	    clear();
	    if (from == 0)
		(*this)<<"Vector v2 (size="<<size2
		       <<") does not contain all elements of v1 (size="<<size1<<")!";
	    else if (from == 1)
		(*this)<<"Vector v1 (size="<<size1
		       <<") does not contain all elements of v2 (size="<<size2<<")!";
	}


    bad_mapping(const T& t,int f,int s1,int s2)
	: missing(t),from(f),size1(s1),size2(s2)
	{
	    gen_message();
	}

    bad_mapping(const T& t,int f,int s1, int s2, const std::string& s)
	: myexception(s),
	  missing(t),
	  from(f),
	  size1(s1),
	  size2(s2)
	{ }

    ~bad_mapping() {}
};


// construct mapping[] such that v2[mapping[i]] = v1[i]

template <class T>
std::vector<int> compute_mapping(const std::vector<T>& v1,const std::vector<T>& v2) 
{
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
	    throw bad_mapping<T>(v1[i],0,v1.size(),v2.size());
	mapping[i] = target;
    }

    if (v1.size() < v2.size())
	try {
	    compute_mapping(v2,v1);
	}
	catch(bad_mapping<T>& b) {
	    b.flip();
	    throw;
	}

    return mapping;
}

/// Apply an inverse mapping (= an ordering).
template <class T>
std::vector<T> apply_indices(const std::vector<T>& v,const std::vector<int>& indices)
{
    std::vector<T> v2(indices.size());
    for(int i=0;i<indices.size();i++)
	v2[i] = v[indices[i]];
    return v2;
}


template <class T>
std::vector<T> apply_mapping(const std::vector<T>& v1,const std::vector<int>& mapping) {
    std::vector<T> v2(v1.size());
    for(int i=0;i<v2.size();i++)
	v2[mapping[i]] = v1[i];

    return v2;
}

/// Get index of t in v
template <typename T>
std::optional<int> find_index(const std::vector<T>& v,const T& t) 
{
    typename std::vector<T>::const_iterator loc = std::find(v.begin(),v.end(),t);
    if (loc == v.end())
	return {};
    else
	return int(loc - v.begin());
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

template <typename T>
bool includes(const std::set<T>& s1, const std::set<T>& s2)
{
    return std::includes(s1.begin(), s1.end(), s2.begin(), s2.end());
}

template <typename T>
bool includes(const std::set<T>& s1, const T& t)
{
    return s1.find(t) != s1.end();
}

template <typename T,typename U>
bool includes(const std::map<T,U>& s1, const T& t)
{
    return s1.find(t) != s1.end();
}

template <typename T>
void add(std::set<T>& S1, const std::set<T>& S2)
{
    for(const auto& s:S2)
	S1.insert(s);
}

template <typename T>
void add(std::set<T>& S1, const std::multiset<T>& S2)
{
    std::set<T> result;
    std::merge(S1.begin(), S1.end(),
	       S2.begin(), S2.end(),
	       std::inserter(result, result.begin())
	);
    S1.swap(result);
}

template <typename T>
void remove(std::set<T>& S1, const std::set<T>& S2)
{
    std::set<T> result;
    std::set_difference(S1.begin(), S1.end(),
			S2.begin(), S2.end(),
			std::inserter(result, result.begin())
	);
    S1.swap(result);
}

template <typename T>
std::set<T> intersection(const std::set<T>& S1, const std::set<T>& S2)
{
    std::set<T> result;
    std::set_intersection(S1.begin(), S1.end(),
			  S2.begin(), S2.end(),
			  std::inserter(result, result.begin())
	);
    return result;
}


template <typename T>
std::ostream& operator<<(std::ostream& o, const std::optional<T>& v)
{
    if (v)
       return o<<*v;
    else
       return o<<"--";
}

// Print out elements of container v separated by string s
template<class V,typename S>
inline void join(std::ostream& o, const V& v, const S& sep)
{
    int i=0;
    for(const auto& elem: v)
    {
	if (i) o<<sep;
	o<<elem;
	i++;
    }
}

template<class V,typename S>
inline std::string join(const V& v, const S& sep)
{
    std::ostringstream o;
    join(o, v, sep);
    return o.str();
}

/// Split the line into pieces at c
std::vector<std::string> split(const std::string& line,char c);
std::vector<std::string> split(const std::string& line,const std::string& s);
std::vector<std::string> resplit(const std::string& line,const std::string& s);

/// Split the line into pieces at c and convert to T
template<class T>
std::vector<T> split(const std::string& s,char c) {
    std::vector<std::string> v = split(s,c);
    std::vector<T> vT;
    for(int i=0;i<v.size();i++)
	vT.push_back(convertTo<T>(v[i]));

    return vT;
}

template<class T>
std::vector<T> split(const std::string& s,const std::string& token) {
    std::vector<std::string> v = split(s,token);
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

bool is_identity(const std::vector<int>&);

template <typename T>
std::vector<T> range(T first, T size)
{
    typedef typename std::vector<T>::size_type size_type;
    std::vector<T> v(size);
    for(size_type i=0;i<v.size();i++)
	v[i] = first+i;
    return v;
}

template <typename T>
std::vector<T> iota(T size)
{
    return range<T>(0,size);
}

/// Return the min of v
template<class T>
T min(const std::vector<T>& v) {
    T t = v[0];
    for(int i=1;i<v.size();i++)
	t = std::min(t,v[i]);

    return t;
}

template <typename T>
int argmin(int n, std::function<T(int)> f)
{
    int min_index = 0;
    int min_score = f(0);
    for(int i=1;i<n;i++)
    {
	T score = f(i);
	if (score < min_score)
	{
	    min_index = i;
	    min_score = score;
	}
    }
    return min_index;
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
bool set_if_undef(std::map<std::string,U,T>& m,const std::string& name, const U& value) {
    if (m.find(name) != m.end() )
	return false;
    else {
	m[name] = value;
	return true;
    }
}

/// If m contains name, convert m[name] to T and return.  Otherwise return value.
template <typename U, typename T>
const U& loadvalue(const std::map<std::string,U,T>& m,const std::string& name, const U& value) 
{
    typename std::map<std::string,U,T>::const_iterator here = m.find(name);
    if (here != m.end() )
	return here->second;
    else
	return value;
}

/// If m contains name, convert m[name] to T and return.  Otherwise return value.
template <typename U, typename T>
const U& loadvalue(std::map<std::string,U,T>& m,const std::string& name) 
{
    typename std::map<std::string,U,T>::const_iterator here = m.find(name);
    if (here != m.end() )
	return here->second;
    else
	throw myexception()<<"Parameter '"<<name<<"' not set.";
}


/// If m contains name, convert m[name] to T and return.  Otherwise return value.
template <typename U, typename T>
bool defined(std::map<std::string,U,T>& m,const std::string& name) 
{
    return (m.find(name) != m.end());
}

bool contains_char(const std::string& s,char c);

bool starts_with(const std::string& s, const std::string& prefix);
bool ends_with(const std::string& s, const std::string& prefix);

/// get the next word starting at position i, return true if not done.
bool get_word(std::string& word, int& i,const std::string& s,
	      const std::string& delimiters,const std::string& whitespace);

bool get_word(std::string& word, int& i, std::vector<std::string>& comments, const std::string& s,
	      const std::string& delimiters,const std::string& whitespace);

template<typename T>
T sum(const std::vector<T>& v)
{
    T total = 0;
    for(int i=0;i<v.size();i++)
	total += v[i];
    return total;
}

template<typename T,typename U>
void scale_by(std::vector<T>& v, const U& S)
{
    for(int i=0;i<v.size();i++)
	v[i] *= S;
}

inline void normalize(std::vector<double>& v)
{
    scale_by(v, 1.0/sum(v));
}

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

template <typename T>
std::vector<T> select(const std::vector<T>& v,const std::vector<int>& indices)
{
    std::vector<T> v2(indices.size());
    for(int i=0;i<v2.size();i++)
	v2[i] = v[indices[i]];
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

template <typename T>
std::vector<T> read(const std::vector<T>& v,const std::vector<int>& indices)
{
    std::vector<T> values(indices.size());
    for(int i=0;i<indices.size();i++)
	values[i] = v[indices[i]];
    return values;
}


template <typename T>
std::vector<T> write(std::vector<T>& v,const std::vector<int>& indices,const std::vector<T>& values)
{
    assert(indices.size() == values.size());
    for(int i=0;i<indices.size();i++)
	v[indices[i]] = values[i];
    return values;
}

template <typename T>
struct index_value
{
    int index;
    T value;
    T check_min(int index2, T value2) 
	{
	    if (index == -1 or (value2 < value)) {
		index = index2;
		value = value2;
	    }
	    return value;
	}
    T check_max(int index2, T value2) 
	{
	    if (index == -1 or (value2 > value)) {
		index = index2;
		value = value2;
	    }
	    return value;
	}
    index_value():index(-1),value() {}
    index_value(const T& t):index(-1),value(t) {}
};
  
void parse_simple_range(const std::string& range,int L,int& begin,int& end);
void parse_modulo_range(const std::string& range,int L,int& begin,int& end,int& step);
std::vector<int> parse_multi_range(const std::string& range,int L);

std::string get_basename(std::string filename);

std::string remove_extension(std::string filename);

std::vector<std::string> get_arguments(std::string& s,char begin, char end);

int replace_element(std::vector<int>& v, const  int e1, int e2);

int remove_element(std::vector<int>& v, const  int e);

void remove_elements(std::vector<int>& v, const std::vector<int>& e);

void check_supplied_filenames(int n, const std::vector<std::string>& files, bool exact=true);

extern int log_verbose;

#include "util/matrix.H"

template <typename T>
std::ostream& operator<<(std::ostream& o, const matrix<T>& M)
{
    for(int i=0;i<M.size1();i++)
    {
	for(int j=0;j<M.size2();j++)
	    o<<M(i,j)<<" ";
	o<<"\n";
    }
    return o;
}

template <typename T>
std::ostream& operator<<(std::ostream& o, const std::vector<T>& v)
{
    for(int i=0;i<v.size();i++)
	o<<v[i]<<" ";
    o<<"\n";
  
    return o;
}

template <typename X>
std::vector<X> remove_duplicates(const std::vector<X>& xs1)
{
    std::vector<X> xs2;
    for(auto& x: xs1)
	if (not includes(xs2, x))
	    xs2.push_back(x);

    return xs2;
}

std::string sanitize_string(const std::string& s1);

template<typename V>
void truncate(V& v)
{
    (&v)->~V();
    new (&v) V;
}

template <typename T>
std::vector<T> flatten(const std::vector<std::vector<T>>& v1)
{
    std::vector<T> v2;
    for(auto& v: v1)
	for(auto& t: v)
	    v2.push_back(t);
    return v2;
}

template <typename T>
std::vector<T> flatten(std::vector<std::vector<T>>&& v1)
{
    std::vector<T> v2;
    for(auto& v: v1)
	for(auto& t: v)
	    v2.push_back(std::move(t));
    return v2;
}

namespace std
{
    template <typename T>
    struct reversion_wrapper { T& iterable; };

    template <typename T>
    auto begin (reversion_wrapper<T> w) { return std::rbegin(w.iterable); }

    template <typename T>
    auto end (reversion_wrapper<T> w) { return std::rend(w.iterable); }

    template <typename T>
    reversion_wrapper<T> reverse (T&& iterable) { return { iterable }; }
}

#endif
