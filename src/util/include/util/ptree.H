#ifndef PTREE_H
#define PTREE_H

#include <iostream>
#include <vector>
#include <set>
#include <utility>
#include <vector>

#include <compare>
#include <initializer_list>
#include <variant>
#include <optional>

#include "util/OrderedDouble.H"
#include "util/json.hh"
#include "util/myexception.H"

struct ptree
{
public:
    typedef std::pair<std::string,ptree> child_t;
    typedef std::vector<child_t> children_t;
    typedef std::variant<std::monostate,std::string,int,OrderedDouble,bool> value_t;

    value_t value;

private:
    children_t children_;

public:
    children_t& children() {return children_;}
    const children_t& children() const {return children_;}

    bool value_is_empty() const;

    bool is_null() const;

    template <typename T>     bool has_value() const {return false;}
    template <typename T>     bool is_a() const {return children().empty() and has_value<T>();}

    template <typename T>       T& get_value()       {return std::get<T>(value);}
    template <typename T> const T& get_value() const {return std::get<T>(value);}

    template <typename T> void put_value(const T& t) {value = t;}
    void put_value(const char* s) {put_value<std::string>(s);}

    template <typename T>       T& get(const std::string& key)       {return get_child(key).get_value<T>();}
    template <typename T> const T& get(const std::string& key) const {return get_child(key).get_value<T>();}

    void erase(const std::string& key);

    std::optional<int> get_child_index(const std::string& key) const;

    int count(const std::string& key) const;

    bool operator==(const ptree&) const = default;

    bool operator==(int i) const
    {
	return (is_a<int>() and get_value<int>() == i);
    }

    bool operator==(double d) const
    {
	return (is_a<double>() and std::get<OrderedDouble>(value) == OrderedDouble(d));
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

    std::strong_ordering operator<=>(const ptree& b) const;

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
        for(auto& [key,value]: children())
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
    ptree(bool x):value(x) {};
    ptree(int x):value(x) {};
    ptree(double x):value(OrderedDouble(x)) {};

    ptree(const children_t& x):children_(x) { }
    ptree(std::initializer_list<child_t> x):children_(x) { }

    ptree(const char* s,const children_t& x):value(std::string(s)),children_(x) { }
    ptree(const char* s,std::initializer_list<child_t> x):value(std::string(s)),children_(x) { }
    ptree(bool v,const children_t& x):value(v),children_(x) { }
    ptree(bool v,std::initializer_list<child_t> x):value(v),children_(x) { }
    ptree(int v,const children_t& x):value(v),children_(x) { }
    ptree(int v,std::initializer_list<child_t> x):value(v),children_(x) { }
    ptree(double v,const children_t& x):value(OrderedDouble(v)),children_(x) { }
    ptree(double v,std::initializer_list<child_t> x):value(OrderedDouble(v)),children_(x) { }
    ptree(const value_t& v,const children_t& x):value(v),children_(x) { }
    ptree(const value_t& v,std::initializer_list<child_t> x):value(v),children_(x) { }
};

inline bool operator<(const ptree& a, const ptree& b)
{
    return a.operator<=>(b) < 0;
}

template <>  inline bool ptree::has_value<bool>()   const {return value.index() == 4;}
template <>  inline bool ptree::has_value<int>()    const {return value.index() == 2;}
template <>  inline bool ptree::has_value<double>() const {return value.index() == 3;}
template <>  inline bool ptree::has_value<std::string>() const {return value.index() == 1;}

template <> inline       double& ptree::get_value<double>()       {return std::get<OrderedDouble>(value).value;}
template <> inline const double& ptree::get_value<double>() const {return std::get<OrderedDouble>(value).value;}

template <> inline void ptree::put_value<double>(const double& t) {value = OrderedDouble(t);}

std::string show(const ptree& pt, std::optional<int> depth = 0);

inline auto index(const ptree& p, int i)
{
    if (i > p.children().size())
        throw myexception()<<"Can't get entry "<<i<<" for tree with size "<<p.children().size();
    auto it = p.children().begin();
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
