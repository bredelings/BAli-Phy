#ifndef UNIFICATION_H
#define UNIFICATION_H

#include <boost/property_tree/ptree.hpp>
#include <boost/optional.hpp>
#include <set>
#include <list>
#include <map>

typedef boost::property_tree::ptree term_t;


template<> struct std::less<term_t> {
    bool operator()(const term_t& a, const term_t& b) const;
};

class equations
{
    bool valid = true;

    // Use a list here so that we can modify the list without invalidating iterators.
    std::list<std::pair<std::set<std::string>,boost::optional<term_t>>> values;

    std::set<term_t> constraints;

    bool has_record(const std::string& x) const;

    std::list<std::pair<std::set<std::string>, boost::optional<term_t>>>::const_iterator find_record(const std::string& x) const;

    std::list<std::pair<std::set<std::string>, boost::optional<term_t>>>::iterator find_record(const std::string& x);

    void remove_record_for(const std::string& x);

public:
    boost::optional<term_t> value_of_var(const std::string& x) const;

    bool add_condition(const std::string& x, const term_t& T);
    bool add_var_condition(const std::string& x, const std::string& y);

    bool unify(const term_t& T, const term_t& U);

    const std::list<std::pair<std::set<std::string>,boost::optional<term_t>>>& get_values() const;

    const std::set<term_t>& get_constraints() const;

    void add_constraint(const term_t&);

    void eliminate_variable(const std::string&);

    void eliminate_except(const std::set<std::string>&);

    std::set<std::string> referenced_vars() const;

    std::string show() const;

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

bool is_wildcard(const boost::property_tree::ptree& p);

std::string show(const equations& E);

#endif
