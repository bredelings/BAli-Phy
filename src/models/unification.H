#ifndef UNIFICATION_H
#define UNIFICATION_H

#include <boost/property_tree/ptree.hpp>
#include <boost/optional.hpp>
#include <set>
#include <vector>
#include <map>

typedef boost::property_tree::ptree term_t;

class equations
{
    bool valid = true;

    std::vector<std::pair<std::set<std::string>,boost::optional<term_t>>> values;

    bool has_record(const std::string& x) const;

    std::vector<std::pair<std::set<std::string>, boost::optional<term_t>>>::const_iterator find_record(const std::string& x) const;

    std::vector<std::pair<std::set<std::string>, boost::optional<term_t>>>::iterator find_record(const std::string& x);

    void remove_record_for(const std::string& x);

public:
    boost::optional<term_t> value_of_var(const std::string& x) const;

    bool add_condition(const std::string& x, const term_t& T);
    bool add_var_condition(const std::string& x, const std::string& y);

    bool unify(const term_t& T, const term_t& U);

    const std::vector<std::pair<std::set<std::string>,boost::optional<term_t>>>& get_values() const;

    void eliminate_variable(const std::string&);

    void eliminate_except(const std::set<std::string>&);

    std::set<std::string> referenced_vars() const;

    operator bool() const;
    equations() {};
    explicit equations(bool s):valid(s) {};
};

// given two terms, what equations do we need to unify them?
equations unify(const term_t& T1, const term_t& T2);

std::set<std::string> find_variables_in_type(const term_t& p);

std::map<std::string, term_t> alpha_rename(const std::set<std::string>& vars, const std::set<std::string>& vars_to_avoid);

void substitute(const equations& equations, term_t& T);

void substitute(const std::map<std::string, term_t>& replace, term_t& T);

equations operator&&(const equations& p1, const equations& p2);

#endif
