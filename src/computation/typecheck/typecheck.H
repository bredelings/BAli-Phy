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

typedef value_env constr_env;

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

class Module;
void typecheck(const Module& m, const Hs::ModuleDecls& M);
#endif
