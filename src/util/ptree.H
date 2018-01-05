#ifndef PTREE_H
#define PTREE_H

#include <iostream>
#include <vector>
#include <utility>
#include <vector>

#include <boost/variant.hpp>
#include <boost/optional.hpp>

struct monostate{};
std::ostream& operator<<(std::ostream& o,const monostate&);
inline bool operator==(const monostate&, const monostate&) {return true;}
inline bool operator!=(const monostate&, const monostate&) {return false;}

struct ptree;
struct ptree: public std::vector<std::pair<std::string,ptree>>
{
public:
    boost::variant<monostate,std::string,int,double,bool> value;

    bool value_is_empty() const;

    template <typename T>     bool has_value() const {return false;}
    template <typename T>     bool is_a() const {return empty() and has_value<T>();}

    template <typename T>       T& get_value()       {return boost::get<T>(value);}
    template <typename T> const T& get_value() const {return boost::get<T>(value);}

    template <typename T> void put_value(const T& t) {value = t;}
    void put_value(const char* s) {put_value<std::string>(s);}

    template <typename T>       T& get(const std::string& key)       {return get_child(key).get_value<T>();}
    template <typename T> const T& get(const std::string& key) const {return get_child(key).get_value<T>();}

    void erase(const std::string& key);

    int get_child_index(const std::string& key) const;

    int count(const std::string& key) const;

    bool operator==(const ptree&) const;
    bool operator!=(const ptree&) const;

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

    boost::optional<ptree&> get_child_optional(const std::string& key);
    boost::optional<const ptree&> get_child_optional(const std::string& key) const;

    ptree& get_child(const std::string& key);
    const ptree& get_child(const std::string& key) const;

    template <typename T>
    T get(const std::string& key, const T& def) const
    {
	if (auto c = get_child_optional(key))
	    return c->get_value<T>();
	else
	    return def;
    }

    template <typename T>
    boost::optional<T> get_optional(const std::string& key) const
    {
	if (auto c = get_child_optional(key))
	    return c->get_value<T>();
	else
	    return boost::none;
    }


    operator bool () const;
    operator int () const;
    operator double () const;
    operator const std::string& () const;

    std::string show() const;

    ptree() {};
    ptree(int i):value(i) {};
    ptree(bool b):value(b) {};
    ptree(double d):value(d) {};
    ptree(const std::string& s):value(s) {};
    ptree(const char* s):value(std::string(s)) {};
};

template <>  inline bool ptree::has_value<bool>()   const {return value.which() == 4;};
template <>  inline bool ptree::has_value<int>()    const {return value.which() == 2;};
template <>  inline bool ptree::has_value<double>() const {return value.which() == 3;};
template <>  inline bool ptree::has_value<std::string>() const {return value.which() == 1;};

std::string show(const ptree& pt, int depth=0);

#endif
