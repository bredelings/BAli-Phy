#ifndef UTIL_JSON_HH
#define UTIL_JSON_HH
#include <boost/json.hpp>
namespace json = boost::json;

inline void update(json::object& j1, const json::object& j2)
{
    for(auto& [key,value]: j2)
	j1[key] = value;
}

inline void update(json::object& j1, json::object&& j2)
{
    for(auto& [key,value]: j2)
	j1[key] = std::move(value);
}

#endif
