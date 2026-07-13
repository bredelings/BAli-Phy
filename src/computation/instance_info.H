#ifndef INSTANCE_INFO_H
#define INSTANCE_INFO_H

#include <map>
#include <vector>
#include "computation/core/type.H"
#include "parser/location.hh"

#include "computation/core/ast.H"

struct InstanceInfo
{
    // forall tvs. constraints => class_con args.

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
	ar(tvs, constraints, class_con, args, incoherent, overlapping, overlappable);
    }
};

struct TypeFamEqnInfo
{
    std::vector<TypeVar> quantified_tvs;
    TypeCon family;
    std::vector<Type> lhs_args;
    Type rhs;

    Type lhs() const;
    Type type() const;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(quantified_tvs, family, lhs_args, rhs);
    }
};

struct EqInstanceInfo
{
    TypeFamEqnInfo equation;

    Type type() const;

    template <class Archive>
    void serialize(Archive& ar)
    {
	ar(equation);
    }
};

typedef std::map<Core::Var<>, InstanceInfo> InstanceEnv;
typedef std::map<Core::Var<>, EqInstanceInfo> EqInstanceEnv;

#endif
