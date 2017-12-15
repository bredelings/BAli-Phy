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

struct ptree;
struct ptree: public std::vector<std::pair<std::string,ptree>>
{
    boost::variant<monostate,int,double,std::string> value;
    template <typename T>       T& get_value()       {return boost::get<T>(value);}
    template <typename T> const T& get_value() const {return boost::get<T>(value);}
    template <typename T> void put_value(const T& t) {value = t;}

    bool value_is_empty() const {return value.which() == 0;}

    int get_child_index(const std::string& key)
    {
	for(int i=0;i<size();i++)
	    if ((*this)[i].first == key)
		return i;
	return -1;
    }

    boost::optional<ptree&> get_child_optional(const std::string& key)
    {
	int index = get_child_index(key);
	if (index == -1) return boost::none;
	return (*this)[index].second;
    }

    ptree() {};
    ptree(int i):value(i) {};
    ptree(double d):value(d) {};
    ptree(const std::string& s):value(s) {};
};

std::string show(const ptree& pt, int depth=0);

#endif
