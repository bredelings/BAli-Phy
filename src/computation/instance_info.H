#ifndef INSTANCE_INFO_H
#define INSTANCE_INFO_H

#include <map>
#include <vector>
#include "haskell/core.H"
#include "haskell/coretype.H"
#include "parser/location.hh"

#include "computation/core/ast.H"

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

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(/*loc, */ tvs, constraints, class_con, args, incoherent, overlapping, overlappable);
    }
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

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(/*loc, */ tvs, lhs, rhs);
    }
};

typedef std::map<Core2::Var<>, InstanceInfo> InstanceEnv;
typedef std::map<Core2::Var<>, EqInstanceInfo> EqInstanceEnv;

#endif
