#ifndef TYPECHECK_EXPECTED_H
#define TYPECHECK_EXPECTED_H

#include <string>
#include <vector>
#include <optional>
#include <map>
#include <variant>
#include "computation/haskell/coretype.H"

struct Check
{
    Type type;

    std::string print() const;

    Check(const Type& t):type(t) {}
};

class Infer
{
    const int level_;
    std::shared_ptr<Type> type_ref;

public:

    int level() const;

    void set_type(const Type& t)
    {
        assert(type_ref->empty());
        *type_ref = t;
        assert(not type_ref->empty());
    }

    std::optional<Type> type() const
    {
        if (type_ref->empty())
            return {};
        else
            return *type_ref;
    }

    std::string print() const;

    Infer(int l);
};

class Expected
{
    mutable std::variant<Infer, Check> value;
public:

    Infer* infer() const
    {
        if (std::holds_alternative<Infer>(value))
            return &std::get<Infer>(value);
        else
            return nullptr;
    }

    const Check* check() const
    {
        if (std::holds_alternative<Check>(value))
            return &std::get<Check>(value);
        else
            return nullptr;
    }

    Type check_type() const
    {
        assert(check());
        return check()->type;
    }


    std::optional<Type> read_type_maybe() const;

    Type read_type() const;

    std::string print() const;

    Expected(const Check& c):value(c) {}
    Expected(const Infer& i):value(i) {}
};


std::vector<Type> read_types(const std::vector<Expected>& exp_types);
std::vector<Expected> check_types(const std::vector<Type>& types);

#endif
