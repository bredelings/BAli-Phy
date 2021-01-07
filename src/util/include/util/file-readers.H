#ifndef FILE_READERS_H
#define FILE_READERS_H

#include <vector>
#include <optional>
#include <string>
#include <istream>
#include <utility>
#include <list>
#include <algorithm>
#include <functional>
#include <exception>
#include <iostream>
#include "util/log-level.H"


template <typename T>
struct reader
{
    typedef T value_type;

    virtual bool done() const = 0;

    virtual std::optional<T> next_one() = 0;
    virtual void skip_one() { next_one();}

    void skip(int n) { for(int i=0;i<n and not done();i++) skip_one();}
    std::optional<T> next(int n=1) { skip(n-1); return next_one();}

    virtual ~reader() = default;
};

template <typename T>
class file_reader: public reader<T>
{
protected:
    std::istream& file;
public:
    bool done() const {return not file;}

    file_reader(std::istream& f):file(f) {}
};

class line_reader: public file_reader<std::string>
{
public:
    std::optional<std::string> next_one();

    line_reader(std::istream&);
};

template <typename R>
class subsample: public reader<typename R::value_type>
{
    int factor;
    R stream;
public:
    bool done() const {return stream.done();}

    std::optional<typename R::value_type> next_one()
    {
        auto v = stream.next_one();
        if (v)
            stream.skip(factor-1);
        return v;
    }

    subsample(int i, const R& r)
        :factor(i), stream(r)
        { }
};

template <typename R1, typename R2>
class zip: public reader<std::pair<typename R1::value_type,typename R2::value_type>>
{
    R1 stream1;
    R2 stream2;

public:
    bool done() const {return stream1.done() or stream2.done();}

    std::optional<std::pair<typename R1::value_type,typename R2::value_type>> next_one()
    {
        auto v1 = stream1.next_one();
        auto v2 = stream2.next_one();
        if (v1 and v2)
            return {{*v1,*v2}};
        else
            return {};
    }

    zip(const R1& r1, const R2& r2)
        :stream1(r1),stream2(r2)
        { }
};

template <typename T>
void thin_by_half(std::vector<T>& v1)
{
    std::vector<T> v2;
    for(int i=0;i<v1.size()/2;i++)
        v2.push_back(std::move(v1[i*2]));
    std::swap(v1,v2);
}

inline int kill(int i, int total, int max)
{
    // We have this many extra Ts
    const int extra = total - max;
    return int( double(i+0.5)*total/extra);
}

template <typename T>
bool thin_down_to(std::vector<T>& v1, std::optional<int> M)
{
    if (not M) return false;

    int total = v1.size();
    int max = *M;
    if (total <= max) return false;

    assert(total <= max*2);

    int k = 0;
    int j = 0;
    std::vector<T> v2;
    for(int i=0;i<max;i++,j++)
    {
        while ( j == kill(k, total , max) )
        {
            j++;
            k++;
        }
        v2.push_back(std::move(v1[j]));
    }
    std::swap(v1, v2);
    assert(v1.size() == max);

    return true;
}

template <typename T>
void thin_by_half(std::list<T>& Ts)
{
    // Remove every other alignment
    for(auto loc = Ts.begin();loc!=Ts.end();)
    {
	auto j = loc++;

	Ts.erase(j);

	if (loc == Ts.end())  break;

	loc++;
    }
}

template <typename T>
bool thin_down_to(std::list<T>& Ts,int max)
{
    int total = Ts.size();
    if (total <= max or max == -1)  return false;

    assert(total <= max*2);

    // We have this many extra Ts
    const int extra = total - max;

    std::vector<int> kill(extra);
    for(int i=0;i<kill.size();i++)
	kill[i] = int( double(i+0.5)*total/extra);
    std::reverse(kill.begin(),kill.end());

    int i=0;
    for(auto loc = Ts.begin();loc!=Ts.end();i++) {
	if (i == kill.back()) {
	    kill.pop_back();
	    auto j = loc++;
	    Ts.erase(j);
	    total--;
	}
	else
	    loc++;
    }
    assert(kill.empty());
    return true;
}

template <typename T>
void insert_and_maybe_thin(T&& t, std::list<T>& Ts, int max, int& subsample)
{
    Ts.push_back(std::move(t));

    // If there are too many alignments
    if (max != -1 and Ts.size() > 2*max)
    {
	// start skipping twice as many alignments
	subsample *= 2;

	if (log_verbose >= 1) std::cerr<<"Went from "<<Ts.size();
	thin_by_half(Ts);
	if (log_verbose >= 1) std::cerr<<" to "<<Ts.size()<<" alignments.\n";
    }
}

template <typename T>
void load_more(std::list<T>& Ts,
	       std::function<std::optional<T>(void)> next,
	       std::function<void(int)> skip,
	       int max,
	       int subsample=1) 
{
    try {
	while(auto t = next())
	{
	    // add the T and thin if possible
	    insert_and_maybe_thin(std::move(*t), Ts, max, subsample);

	    // skip over Ts due to subsampling
	    skip(subsample-1);
	}
    }
    // If we had a problem reading elements, still do the thinning.
    catch (std::exception& e) {
	thin_down_to(Ts, max);

	throw;
    }
    thin_down_to(Ts, max);
}

#endif
