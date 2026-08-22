#ifndef UTIL_CMDLINE_H
#define UTIL_CMDLINE_H

#include <vector>
#include <string>
#include <map>
#include <optional>
#include <boost/program_options.hpp>

void check_supplied_filenames(int n, const std::vector<std::string>& files, bool exact=true);

void parse_simple_range(const std::string& range,int L,int& begin,int& end);
void parse_modulo_range(const std::string& range,int L,int& begin,int& end,int& step);
std::vector<int> parse_multi_range(const std::string& range,int L);

std::vector<std::string> get_arguments(std::string& s,char begin, char end);

std::vector<std::string> parse_string_list(const std::string& values);

template <typename T>
std::optional<T> get_arg(const boost::program_options::variables_map& args, const std::string& key)
{
    if (args.count(key))
	return args.at(key).as<T>();
    else
	return {};
}

template <typename T>
const T& get_arg_default(const boost::program_options::variables_map& args, const std::string& key, const T& t)
{
    if (args.count(key))
	return args.at(key).as<T>();
    else
	return t;
}


std::vector<std::string> get_string_list(const boost::program_options::variables_map& args, const std::string& key);


#endif
