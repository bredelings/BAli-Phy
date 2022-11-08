#ifndef TYPECHECK_EXPECTED_H
#define TYPECHECK_EXPECTED_H

#include <string>
#include <vector>
#include <optional>
#include <map>
#include <variant>
#include "computation/haskell/type.H"

struct Check
{
    Hs::Type type;

    std::string print() const;

    Check(const Hs::Type& t):type(t) {}
};

class Infer
{
    int level;
    std::shared_ptr<Hs::Type> type_ref;

public:
    void set_type(const Hs::Type& t)
    {
        assert(type_ref->empty());
        *type_ref = t;
        assert(not type_ref->empty());
    }

    std::optional<Hs::Type> type() const
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
    bool check() const {return std::holds_alternative<Check>(value);}
    bool infer() const {return std::holds_alternative<Infer>(value);}

    Hs::Type check_type() const
    {
        assert(check());
        return std::get<Check>(value).type;
    }

    std::optional<Hs::Type> inferred_type() const
    {
        assert(infer());
        return std::get<Infer>(value).type();
    }

    void infer_type(const Hs::Type& t)  const
    {
        assert(infer());
        std::get<Infer>(value).set_type(t);
    }

    std::optional<Hs::Type> read_type_maybe() const;

    Hs::Type read_type() const;

    std::string print() const;

    Expected(const Check& c):value(c) {}
    Expected(const Infer& i):value(i) {}
};


std::vector<Hs::Type> read_types(const std::vector<Expected>& exp_types);
std::vector<Expected> check_types(const std::vector<Hs::Type>& types);

#endif
