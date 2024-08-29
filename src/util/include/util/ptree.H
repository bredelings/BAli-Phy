#ifndef PTREE_H
#define PTREE_H

#include <iostream>
#include <vector>
#include <set>
#include <utility>
#include <vector>

#include <variant>
#include <optional>

#include "util/json.hh"
#include "util/myexception.H"

struct monostate{};
std::ostream& operator<<(std::ostream& o,const monostate&);
inline bool operator<(const monostate&, const monostate&) {return false;}
inline bool operator>(const monostate&, const monostate&) {return false;}
inline bool operator==(const monostate&, const monostate&) {return true;}

struct ptree;
struct ptree: public std::vector<std::pair<std::string,ptree>>
{
public:
    typedef std::variant<monostate,std::string,int,double,bool> value_t;
    value_t value;

    bool value_is_empty() const;

    bool is_null() const;

    template <typename T>     bool has_value() const {return false;}
    template <typename T>     bool is_a() const {return empty() and has_value<T>();}

    template <typename T>       T& get_value()       {return std::get<T>(value);}
    template <typename T> const T& get_value() const {return std::get<T>(value);}

    template <typename T> void put_value(const T& t) {value = t;}
    void put_value(const char* s) {put_value<std::string>(s);}

    template <typename T>       T& get(const std::string& key)       {return get_child(key).get_value<T>();}
    template <typename T> const T& get(const std::string& key) const {return get_child(key).get_value<T>();}

    void erase(const std::string& key);

    std::optional<int> get_child_index(const std::string& key) const;

    int count(const std::string& key) const;

    bool operator==(const ptree&) const;

    bool operator==(int i) const
    {
	return (is_a<int>() and get_value<int>() == i);
    }

    bool operator==(double d) const
    {
	return (is_a<double>() and get_value<double>() == d);
    }

    bool operator==(bool b) const
    {
	return (is_a<bool>() and get_value<bool>() == b);
    }

    bool operator==(const std::string& s) const
    {
	return (is_a<std::string>() and get_value<std::string>() == s);
    }

    bool operator==(const char* s) const
    {
	return (*this) == std::string(s);
    }

          ptree* get_child_optional(const std::string& key);
    const ptree* get_child_optional(const std::string& key) const;

    ptree& get_child(const std::string& key);
    const ptree& get_child(const std::string& key) const;

          ptree* get_path_optional(const std::vector<std::string>& path, int i=0);
    const ptree* get_path_optional(const std::vector<std::string>& path, int i=0) const;

    ptree& get_path(const std::vector<std::string>& path, int i=0);
    const ptree& get_path(const std::vector<std::string>& path, int i=0) const;

    int make_index(const std::string& key);
    ptree& make_child(const std::string& key);
    ptree& make_path(const std::vector<std::string>& path, int i=0);
    
    template <typename T>
    T get(const std::string& key, const T& def) const
    {
	if (auto c = get_child_optional(key))
	    return c->get_value<T>();
	else
	    return def;
    }

    template <typename T>
    std::optional<T> get_optional(const std::string& key) const
    {
	if (auto c = get_child_optional(key))
	    return c->get_value<T>();
	else
	    return {};
    }

    std::set<std::string> keys() const
    {
        std::set<std::string> ks;
        for(auto& [key,value]: *this)
            ks.insert(key);
        return ks;
    }

    operator bool () const;
    operator int () const;
    operator double () const;
    operator const std::string& () const;

    std::string show(bool pretty=true) const;

    ptree() {};

    ptree(const value_t& v):value(v) {};
    ptree(const char* s):value(std::string(s)) {};

    ptree(const std::vector<std::pair<std::string,ptree>>& x):std::vector<std::pair<std::string,ptree>>(x) { }

    ptree(const char* s,const std::vector<std::pair<std::string,ptree>>& x):std::vector<std::pair<std::string,ptree>>(x),value(std::string(s)) { }
    ptree(const value_t& v,const std::vector<std::pair<std::string,ptree>>& x):std::vector<std::pair<std::string,ptree>>(x),value(v) { }
};

template <>  inline bool ptree::has_value<bool>()   const {return value.index() == 4;}
template <>  inline bool ptree::has_value<int>()    const {return value.index() == 2;}
template <>  inline bool ptree::has_value<double>() const {return value.index() == 3;}
template <>  inline bool ptree::has_value<std::string>() const {return value.index() == 1;}

std::string show(const ptree& pt, std::optional<int> depth = 0);

inline auto index(const ptree& p, int i)
{
    if (i > p.size())
        throw myexception()<<"Can't get entry "<<i<<" for tree with size "<<p.size();
    auto it = p.begin();
    for(int j=0;j<i;j++)
        it++;
    return *it;
}

inline ptree array_index(const ptree& p, int i)
{
    return index(p,i).second;
}

std::ostream& operator<<(std::ostream& o, const ptree::value_t& v);

#endif
