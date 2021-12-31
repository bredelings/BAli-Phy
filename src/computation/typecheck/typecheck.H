#ifndef TYPECHECK_H
#define TYPECHECK_H

#include <string>
#include <vector>
#include <map>
#include "immer/map.hpp" // for immer::map
#include "kind.H"

#include "computation/parser/haskell.H"

typedef immer::map<std::string, Hs::Type> value_env;

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
typedef global_value_env global_instance_env;

// The LIE maps local dictionary variables to the constraint for which they are a dictionary.
// It DOES allow free type variables.
typedef local_value_env local_instance_env;

typedef value_env constr_env;

std::string print(const value_env& env);
void add_no_overlap(value_env& e1, const value_env& e2);
value_env plus_no_overlap(const value_env& e1, const value_env& e2);
void add_prefer_right(value_env& e1, const value_env& e2);
value_env plus_prefer_right(const value_env& e1, const value_env& e2);

struct type_con_info
{
    kind k;
    int arity;
//    Hs::Type operator() (const vector<Hs::Type>& args) const;
// -- for type synonmys, we need the means to apply the constructor to (exactly) k arguments, for arity k.
// -- for data / newtypes, we need to means to apply up to k arguments.
// -- perhaps we need to store the KIND, and not just the arity?
};

typedef std::map<std::string, type_con_info> type_con_env;

void add_no_overlap(type_con_env& e1, const type_con_env& e2);
type_con_env plus_no_overlap(const type_con_env& e1, const type_con_env& e2);

struct class_info
{
    std::string name;
    std::string emitted_name;
    std::vector<Hs::TypeVar> type_vars;

    // Maybe change this to vector<pair<Type,string>>, 
    // FIXME: Should we record here the names of functions to extract 
    Hs::Context context;

    global_value_env methods;
};

typedef std::map<std::string, class_info> class_env;

class Module;
void typecheck(const Module& m, const Hs::ModuleDecls& M);
#endif
