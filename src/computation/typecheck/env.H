#ifndef ENV_H
#define ENV_H

#include <set>
#include <string>
#include <map>
#include "immer/map.hpp" // for immer::map
#include "haskell/haskell.H" // for Hs::Var, Hs::Type, Hs::Kind, Hs::TypeVar, Hs::Context, Hs::MetaTypeVar
#include "constraints.H" // for LIE
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

typedef std::map<Core::Var, Hs::Type> GIE;

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

struct DataConInfo
{
    std::vector<Hs::TypeVar> uni_tvs;
    std::vector<Hs::TypeVar> exi_tvs;
    std::vector<Hs::Type> top_constraints; // The "stupid theta"
    std::vector<Hs::Type> written_constraints;
    std::vector<Hs::Type> gadt_eq_constraints;
    std::vector<Hs::Type> field_types;
    std::vector<Hs::Type> all_constraints() const;
    std::vector<Hs::Type> dictionary_constraints() const;
    std::vector<Hs::Type> equality_constraints() const;
    Hs::TypeCon data_type;
    int dict_arity() const;
    int arity() const;
    Hs::Type result_type() const;

    Hs::Type constructor_type() const;
};

typedef immer::map<ID, DataConInfo> constr_env;

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

    std::map<Hs::TypeCon,bool> associated_type_families;
};

struct TypeFamEqnInfo
{
    std::vector<Hs::Type> args;
    Hs::Type rhs;
    std::vector<Hs::TypeVar> free_tvs;
};

// This index indicates a unique name for each instance, and also indicates the evidence for an instance.
typedef int TypeFamEqnId;

struct TypeFamInfo
{
    std::vector<Hs::TypeVar> args;
    Hs::Kind result_kind;

    std::optional<std::string> associated_class;
    bool closed = false;

    std::map<TypeFamEqnId, TypeFamEqnInfo> equations;

    int arity() const {return args.size();}

    TypeFamInfo(const std::vector<Hs::TypeVar>&, const Hs::Kind&, const std::optional<std::string>& = {});
};


typedef std::map<Hs::TypeCon, TypeFamInfo> TypeFamEnv;

typedef std::map<ID, ClassInfo> ClassEnv;

typedef std::map<ID, Hs::Type> signature_env;

std::set<Hs::TypeVar> free_type_variables(const value_env& env);

std::set<Hs::TypeVar> free_type_variables(const LIE& env);

std::set<Hs::MetaTypeVar> free_meta_type_variables(const value_env& env);

std::set<Hs::MetaTypeVar> free_meta_type_variables(const LIE& env);

template <typename T>
std::set<T> operator-(const std::set<T>& s1, const std::set<T>& s2)
{
    return minus(s1,s2);
}

#endif
