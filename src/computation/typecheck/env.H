#ifndef ENV_H
#define ENV_H

#include <set>
#include <string>
#include <map>
#include "immer/set.hpp" // for immer::map
#include "immer/map.hpp" // for immer::map
#include "haskell/haskell.H" // for Hs::Var, Type, Hs::Kind, TypeVar, Hs::Context, Hs::MetaTypeVar
#include "haskell/coretype.H"
#include "util/set.H"
#include "kind.H"
#include "computation/instance_info.H"
#include "computation/data_con_info.H"

#include <cereal/types/map.hpp>
#include <cereal/types/utility.hpp>

typedef std::string ID;

typedef immer::map<Hs::Var, Type> value_env;

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

typedef std::map<Core::Var, Type> GIE;

std::string print(const value_env& env);
void add_no_overlap(value_env& e1, const value_env& e2);
value_env plus_no_overlap(const value_env& e1, const value_env& e2);
void add_prefer_right(value_env& e1, const value_env& e2);
value_env plus_prefer_right(const value_env& e1, const value_env& e2);

value_env operator+(const value_env&, const value_env&);
value_env& operator+=(value_env&, const value_env&);

struct type_con_info
{
    Kind kind;
    int arity;
//    Type operator() (const vector<Type>& args) const;
// -- for type synonmys, we need the means to apply the constructor to (exactly) k arguments, for arity k.
// -- for data / newtypes, we need to means to apply up to k arguments.
// -- perhaps we need to store the KIND, and not just the arity?
};

typedef std::map<ID, type_con_info> TypeConEnv;

void add_no_overlap(TypeConEnv& e1, const TypeConEnv& e2);
TypeConEnv plus_no_overlap(const TypeConEnv& e1, const TypeConEnv& e2);

TypeConEnv& operator+=(TypeConEnv&, const TypeConEnv);
TypeConEnv operator+(const TypeConEnv&, const TypeConEnv&);

struct TypeSynonymInfo
{
    ID name;
    std::vector<TypeVar> type_vars;
    Type result;

    int arity() const;
    Type expand(const std::vector<Type>&) const;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(name, type_vars, result);
    }
};

typedef std::map<std::string, TypeSynonymInfo> TypeSynonymEnv;

struct DataConInfo
{
    std::vector<TypeVar> uni_tvs;
    std::vector<TypeVar> exi_tvs;
    std::vector<Type> top_constraints; // The "stupid theta"
    std::vector<Type> written_constraints;
    std::vector<Type> gadt_eq_constraints;
    std::vector<Type> field_types;
    std::vector<Type> all_constraints() const;
    std::vector<Type> dictionary_preds() const;
    std::vector<Type> equality_preds() const;
    TypeCon data_type;
    int dict_arity() const;
    int arity() const;
    Type result_type() const;

    Type constructor_type() const;
};

typedef immer::map<ID, DataConInfo> DataConEnv;

struct ClassInfo
{
    ID name;
    std::vector<TypeVar> type_vars;

    // Maybe change this to vector<pair<Type,string>>, 
    // FIXME: Should we record here the names of functions to extract 
    Context context;

    local_value_env members;

    GIE superclass_extractors;

    // Why are we storing an Hs::Var here - Would a string be sufficient?
    std::map<std::string, Hs::Var> default_methods;

    // FIXME - we need the order of the constraints, and the order of the members
    // Why are we storing an Hs::Var here - Would a string be sufficient?
    std::vector<std::pair<Hs::Var,Type>> fields;

    // Each family is associated with a single optional default instance.
    std::map<TypeCon, std::optional<TypeFamilyInstanceDecl>> associated_type_families;

    template <class Archive>
    void serialize(Archive& ar)
    {
	// PROBLEM: members is an immer::map, which we don't know how to serialize
	// PROBLEM: members, default_methods, and fields contain Hs::Var, which we don't know how to serialize.
	// PROBLEM: superclass_extractors has Core::Var, which we also don't know how to serialize.
	// IDEA: make pair<string,optional<int>> class.
	ar(name, type_vars, context, /*members, superclass_extractors, default_methods, fields ,*/ associated_type_families);
    }
};

struct TypeFamEqnInfo
{
    std::vector<Type> args;
    Type rhs;
    std::vector<TypeVar> free_tvs;
};

// This index indicates a unique name for each instance, and also indicates the evidence for an instance.
typedef int TypeFamEqnId;

struct TypeFamInfo
{
    std::vector<TypeVar> args;
    Kind result_kind;

    std::optional<std::string> associated_class;
    bool closed = false;

    int arity() const {return args.size();}

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(args, result_kind, associated_class, closed);
    }

    TypeFamInfo(const std::vector<TypeVar>&, const Kind&, const std::optional<std::string>& = {});
};

struct DataFamInfo
{
    std::vector<TypeVar> args;
    Kind result_kind;

    std::optional<std::string> associated_class;

    int arity() const {return args.size();}

    DataFamInfo(const std::vector<TypeVar>&, const Kind&, const std::optional<std::string>& = {});

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(args, result_kind, associated_class);
    }
};


typedef std::map<TypeCon, TypeFamInfo> TypeFamEnv;

typedef std::map<ID, ClassInfo> ClassEnv;

typedef std::map<Hs::Var, Type> signature_env;

std::set<TypeVar> free_type_variables(const value_env& env);

std::set<MetaTypeVar> free_meta_type_variables(const value_env& env);

template <typename T>
std::set<T> operator-(const std::set<T>& s1, const std::set<T>& s2)
{
    return minus(s1,s2);
}

typedef immer::set<TypeVar> VarSet;
typedef immer::map<TypeVar, TypeVar> VarEnv;

struct RenameTyvarEnv2
{
    VarSet out_vars;
    VarEnv left;
    VarEnv right;

    TypeVar map_left(const TypeVar&) const;
    TypeVar map_right(const TypeVar&) const;
};

RenameTyvarEnv2 rename_binders2(RenameTyvarEnv2 env, const std::vector<TypeVar>& tvs1, const std::vector<TypeVar>& tvs2);

RenameTyvarEnv2 rename_binder2(RenameTyvarEnv2 env, const TypeVar& tv1, const TypeVar& tv2);

std::tuple<RenameTyvarEnv2,TypeVar> rename_binder2_var(RenameTyvarEnv2 env, const TypeVar& tv1, const TypeVar& tv2);

VarEnv extendVarEnv(VarEnv env, const TypeVar& tv_in, const TypeVar& tv_out);

VarSet extendVarSet(VarSet set, const TypeVar& tv_out);

#endif
