#ifndef ENV_H
#define ENV_H

#include <string>
#include <map>
#include "immer/map.hpp" // for immer::map
#include "haskell/haskell.H"
#include "util/set.H"
#include "kind.H"

typedef std::string ID;

typedef immer::map<ID, Hs::Type> value_env;

typedef value_env global_value_env;

typedef value_env local_value_env;

// The GIE maps classes to a list of instances for them.
// It DOES NOT allow free type variables.
// In practice it should be implemented as (k,C) -> info instead of name => type
// Each instance corresponds to a dictionary function (dfun) with NO free type variables.
// For example:
//   instance Eq a => Eq [a] where
// leads to
//   dEqList :: forall a. Eq a => Eq [a]

typedef value_env constr_env;

typedef std::map<Core::Var, Hs::Type> GIE;
typedef std::vector<std::pair<Core::Var, Hs::Type>> LIE;
LIE& operator+=(LIE& lie1, const LIE& lie2);

template <typename T>
std::vector<T>& operator+=(std::vector<T>& v1, const std::vector<T>& v2)
{
    v1.insert(v1.end(), v2.begin(), v2.end());
    return v1;
}

template <typename T>
std::vector<T> operator+(const std::vector<T>& v1, const std::vector<T>& v2)
{
    auto v3 = v1;
    v3 += v2;
    return v3;
}

std::string print(const value_env& env);
void add_no_overlap(value_env& e1, const value_env& e2);
value_env plus_no_overlap(const value_env& e1, const value_env& e2);
void add_prefer_right(value_env& e1, const value_env& e2);
value_env plus_prefer_right(const value_env& e1, const value_env& e2);

value_env operator+(const value_env&, const value_env&);
value_env& operator+=(value_env&, const value_env&);

struct type_con_info
{
    Hs::Kind kind;
    int arity;
//    Hs::Type operator() (const vector<Hs::Type>& args) const;
// -- for type synonmys, we need the means to apply the constructor to (exactly) k arguments, for arity k.
// -- for data / newtypes, we need to means to apply up to k arguments.
// -- perhaps we need to store the KIND, and not just the arity?
};

typedef std::map<ID, type_con_info> type_con_env;

void add_no_overlap(type_con_env& e1, const type_con_env& e2);
type_con_env plus_no_overlap(const type_con_env& e1, const type_con_env& e2);

type_con_env& operator+=(type_con_env&, const type_con_env);
type_con_env operator+(const type_con_env&, const type_con_env&);

struct TypeSynonymInfo
{
    ID name;
    std::vector<Hs::TypeVar> type_vars;
    Hs::Type result;

    int arity() const;
    Hs::Type expand(const std::vector<Hs::Type>&) const;
};

typedef std::map<std::string, TypeSynonymInfo> TypeSynonymEnv;

struct ClassInfo
{
    ID name;
    std::vector<Hs::TypeVar> type_vars;

    // Maybe change this to vector<pair<Type,string>>, 
    // FIXME: Should we record here the names of functions to extract 
    Hs::Context context;

    local_value_env members;

    GIE superclass_extractors;

    std::map<ID, Hs::Var> default_methods;

    // FIXME - we need the order of the constraints, and the order of the members
    std::vector<std::pair<ID,Hs::Type>> fields;
};

typedef std::map<ID, ClassInfo> ClassEnv;

std::set<Hs::TypeVar> free_type_variables(const value_env& env);

std::set<Hs::MetaTypeVar> free_meta_type_variables(const value_env& env);

std::set<Hs::MetaTypeVar> free_meta_type_variables(const LIE& env);

template <typename T>
std::set<T> operator-(const std::set<T>& s1, const std::set<T>& s2)
{
    return minus(s1,s2);
}


#endif
