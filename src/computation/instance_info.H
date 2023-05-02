#ifndef INSTANCE_INFO_H
#define INSTANCE_INFO_H

#include <map>
#include <vector>
#include "haskell/core.H"
#include "haskell/coretype.H"
#include "parser/location.hh"

struct InstanceInfo
{
    // forall tvs. constraints => class_con args.

    yy::location loc;
    std::vector<TypeVar> tvs;
    std::vector<Type> constraints;
    TypeCon class_con;
    std::vector<Type> args;

    bool incoherent = false;
    bool overlappable = false;
    bool overlapping = false;

    Type type() const;
};

struct EqInstanceInfo
{
    // forall tvs. lhs ~ rhs

    yy::location loc;
    std::vector<TypeVar> tvs;

    // TypeCon lhs_tyfam_tycon;

    Type lhs;
    Type rhs;

    Type type() const;
};

typedef std::map<Core::Var, InstanceInfo> InstanceEnv;
typedef std::map<Core::Var, EqInstanceInfo> EqInstanceEnv;

#endif
