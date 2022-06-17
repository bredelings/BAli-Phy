#ifndef HASKELL_VAR
#define HASKELL_VAR

#include "computation/object.H"
#include "computation/parser/located.H"

#include "type.H"

namespace Haskell
{

struct Var: public Object
{
    Located<std::string> name;
    std::optional<int> index;

    std::optional<Type> type;

    bool operator==(const Object&) const;
    bool operator==(const Var&) const;
    bool operator<(const Var&) const;

    Var* clone() const { return new Var(*this);}
    std::string print() const;
    std::string print_with_type() const;

    Var(const Located<std::string>& s):name(s) {}
    Var(const Located<std::string>& s, const Type& t):name(s),type(t) {}
};

struct Con: public Object
{
    Located<std::string> name;

    std::optional<int> arity;

    Con* clone() const { return new Con(*this);}
    std::string print() const;

    Con(const Located<std::string>& s):name(s) {}
    Con(const Located<std::string>& s, int a):name(s),arity(a) {}
};

}

#endif
