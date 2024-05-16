#ifndef MAPPING_H
#define MAPPING_H

#include <vector>
#include <optional>
#include <algorithm>
#include "util/assert.hh"
#include "util/myexception.H"

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

/// invert the mapping
std::vector<int> invert(const std::vector<int>&);

/// compose the two mappings
std::vector<int> compose(const std::vector<int>&,const std::vector<int>&);

bool is_identity(const std::vector<int>&);

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
#endif
