#ifndef RULES_H
#define RULES_H

#include <string>
#include <boost/property_tree/ptree.hpp>
#include <boost/optional.hpp>

typedef boost::property_tree::ptree Rule;

typedef boost::property_tree::ptree type_t;

boost::optional<Rule> get_rule_for_func(const std::string& s);

Rule require_rule_for_func(const std::string& s);

boost::property_tree::ptree get_arg(const Rule& rule, const std::string& arg_name);

std::string get_keyword_for_positional_arg(const std::string& head, int i);

type_t get_result_type(const std::string& func);

type_t get_result_type(const boost::property_tree::ptree& func);

type_t get_type_for_arg(const Rule& rule, const std::string& arg);

#endif
