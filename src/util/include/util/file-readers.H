#ifndef FILE_READERS_H
#define FILE_READERS_H

#include <vector>
#include <optional>
#include <string>
#include <istream>
#include <utility>

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

#endif
