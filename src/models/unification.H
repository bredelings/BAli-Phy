#ifndef UNIFICATION_H
#define UNIFICATION_H

#include <boost/property_tree/ptree.hpp>
#include <boost/optional.hpp>
#include <set>

typedef boost::property_tree::ptree equations_t;
typedef boost::property_tree::ptree term_t;

// given two terms, what equations do we need to unify them?
boost::optional<equations_t> unify(const term_t& p1, const term_t& p2);

bool can_unify(const term_t& p1, const term_t& p2);

std::set<std::string> find_variables(const term_t& p);

equations_t alpha_rename(const std::set<std::string>& vars, const std::set<std::string>& vars_to_avoid);

void substitute(const equations_t& equations, term_t& p);

boost::optional<equations_t> merge_equations(const boost::optional<equations_t>& p1, const boost::optional<equations_t>& p2);
#endif
