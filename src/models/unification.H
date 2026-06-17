#ifndef UNIFICATION_H
#define UNIFICATION_H

#include <set>
#include <list>
#include <map>
#include "models/model-type.H"

using type_t = CM::Type;

class equations
{
    std::vector<std::pair<type_t,type_t>> failed;

    // Use a list here so that we can modify the list without invalidating iterators.
    std::list<std::pair<std::set<std::string>,std::optional<type_t>>> values;

    std::set<type_t> constraints;

    bool has_record(const std::string& x) const;

    std::list<std::pair<std::set<std::string>, std::optional<type_t>>>::const_iterator find_record(const std::string& x) const;

    std::list<std::pair<std::set<std::string>, std::optional<type_t>>>::iterator find_record(const std::string& x);

    void remove_record_for(const std::string& x);

public:
    void clear();

    bool valid() const;

    bool occurs_check() const;

    friend void substitute(const equations& E, type_t& T);

    std::optional<type_t> value_of_var(const std::string& x) const;

    bool add_condition(const std::string& x, const type_t& T);
    bool add_var_condition(const std::string& x, const std::string& y);

    bool unify(const type_t& T, const type_t& U);

    const std::list<std::pair<std::set<std::string>,std::optional<type_t>>>& get_values() const;

    const std::set<type_t>& get_constraints() const;

    void add_constraint(const type_t&);

    std::map<std::string,type_t> eliminate_variable(const std::string&);

    std::map<std::string,type_t> eliminate_except(const std::set<std::string>&);

    std::set<std::string> referenced_vars() const;

    std::string show() const;

    operator bool() const;
    equations() {};
};

// Given two types, what equations do we need to unify them?
equations unify(const type_t& T1, const type_t& T2);


struct FVSource
{
    mutable int index = 0;
    type_t get_fresh_type_var(const std::string& s) const
    {
        // Fresh type variables must remain recognizable by is_type_variable().
        const std::string prefix = (!s.empty() and s[0] >= 'a' and s[0] <= 'z') ? s : "t";
        auto v = type_t(prefix + "#" + std::to_string(index++));
        assert(CM::is_type_variable(v));
        return v;
    }
};

std::map<std::string, type_t> alpha_rename(const std::set<std::string>& vars, const FVSource&);

void substitute(const equations& equations, type_t& T);

void substitute(const std::map<std::string, type_t>& replace, type_t& T);

equations operator&&(const equations& p1, const equations& p2);

#endif
