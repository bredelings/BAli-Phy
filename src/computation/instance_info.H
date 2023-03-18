#ifndef INSTANCE_INFO_H
#define INSTANCE_INFO_H

#include <map>
#include <vector>
#include "haskell/core.H"
#include "haskell/coretype.H"

struct InstanceInfo
{
    // forall tvs. constraints => class_con args.

    std::vector<TypeVar> tvs;
    std::vector<Type> constraints;
    TypeCon class_con;
    std::vector<Type> args;

    Type type() const;
};

typedef std::map<Core::Var, InstanceInfo> InstanceEnv;

#endif
