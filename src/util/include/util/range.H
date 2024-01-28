#ifndef UTIL_RANGE_H
#define UTIL_RANGE_H

#include <vector>
#include <functional>
#include <algorithm>

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

template<typename T, typename F>
std::vector<T> select(std::vector<T> inVec, const F& predicate)
{
    std::vector<T> result;
    std::copy_if(inVec.begin(), inVec.end(), back_inserter(result), std::function<bool(const T&)>(predicate));
    return result;
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

int replace_element(std::vector<int>& v, const  int e1, int e2);

int remove_element(std::vector<int>& v, const  int e);

void remove_elements(std::vector<int>& v, const std::vector<int>& e);

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
