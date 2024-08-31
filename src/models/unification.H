#ifndef UNIFICATION_H
#define UNIFICATION_H

#include <set>
#include <list>
#include <map>
#include "util/ptree.H"

typedef ptree term_t;

// GCC5 can't handle 'struct std::less<>'
namespace std
{
template<> struct less<term_t> {
    bool operator()(const term_t& a, const term_t& b) const;
};
}

class equations
{
    bool valid = true;

    // Use a list here so that we can modify the list without invalidating iterators.
    std::list<std::pair<std::set<std::string>,std::optional<term_t>>> values;

    std::set<term_t> constraints;

    bool has_record(const std::string& x) const;

    std::list<std::pair<std::set<std::string>, std::optional<term_t>>>::const_iterator find_record(const std::string& x) const;

    std::list<std::pair<std::set<std::string>, std::optional<term_t>>>::iterator find_record(const std::string& x);

    void remove_record_for(const std::string& x);

public:
    void clear();

    bool occurs_check() const;

    friend void substitute(const equations& E, term_t& T);

    std::optional<term_t> value_of_var(const std::string& x) const;

    bool add_condition(const std::string& x, const term_t& T);
    bool add_var_condition(const std::string& x, const std::string& y);

    bool unify(const term_t& T, const term_t& U);

    const std::list<std::pair<std::set<std::string>,std::optional<term_t>>>& get_values() const;

    const std::set<term_t>& get_constraints() const;

    void add_constraint(const term_t&);

    std::map<std::string,term_t> eliminate_variable(const std::string&);

    std::map<std::string,term_t> eliminate_except(const std::set<std::string>&);

    std::set<std::string> referenced_vars() const;

    std::string show() const;

    operator bool() const;
    equations() {};
    explicit equations(bool s):valid(s) {};
};

// given two terms, what equations do we need to unify them?
equations unify(const term_t& T1, const term_t& T2);

std::set<std::string> find_variables_in_type(const term_t& p);

struct FVSource
{
    mutable int index = 0;
    term_t get_fresh_type_var(const std::string& s) const {return term_t(s + "#" + std::to_string(index++));}
};

std::map<std::string, term_t> alpha_rename(const std::set<std::string>& vars, const FVSource&);

void substitute(const equations& equations, term_t& T);

void substitute(const std::map<std::string, term_t>& replace, term_t& T);

equations operator&&(const equations& p1, const equations& p2);

bool is_wildcard(const ptree& p);

std::string show(const equations& E);

std::pair<term_t, std::vector<term_t>> get_type_apps(term_t type);
term_t make_type_app(const term_t& t1, const term_t& t2);
term_t make_type_apps(term_t type, const std::vector<term_t>& args);

ptree get_type_head(term_t type);

#endif
