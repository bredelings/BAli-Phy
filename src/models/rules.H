#ifndef RULES_H
#define RULES_H

#include <string>
#include <boost/property_tree/ptree.hpp>
#include <boost/optional.hpp>
#include <boost/filesystem/operations.hpp>

typedef boost::property_tree::ptree Rule;

typedef boost::property_tree::ptree type_t;

boost::property_tree::ptree get_arg(const Rule& rule, const std::string& arg_name);

type_t get_type_for_arg(const Rule& rule, const std::string& arg);

std::string get_keyword_for_positional_arg(const Rule& rule, int i);

class Rules: public std::vector<Rule>
{
    std::vector<boost::filesystem::path> path_list;

    Rule load_rule(const std::string& s) const;

    void add_rule(const std::string& s);

    std::vector<std::string> find_all_rules() const;
    boost::optional<boost::filesystem::path> get_path_for_function(const std::string& name) const;
public:
    boost::optional<Rule> get_rule_for_func(const std::string& s) const;
    Rule require_rule_for_func(const std::string& s) const;

    type_t get_result_type(const std::string& func) const;
    type_t get_result_type(const boost::property_tree::ptree& func) const;

    Rules(const std::vector<boost::filesystem::path>& path_list);
};

#endif
