#ifndef UTIL_SET_H
#define UTIL_SET_H

#include <vector>
#include <set>
#include <map>
#include <algorithm>

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

template <typename X>
std::vector<X> remove_duplicates(const std::vector<X>& xs1)
{
    std::vector<X> xs2;
    for(auto& x: xs1)
	if (not includes(xs2, x))
	    xs2.push_back(x);

    return xs2;
}


#endif
