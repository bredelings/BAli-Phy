#ifndef HASKELL_VAR
#define HASKELL_VAR

#include "computation/object.H"
#include "computation/parser/located.H"

#include "type.H"
#include "coretype.H"
#include "core.H"

namespace Haskell
{

struct Var: public Object
{
    Located<std::string> name;
    std::optional<int> index;

    std::optional<::Type> type;

    Core::wrapper wrap;

    bool operator==(const Object&) const;
    bool operator==(const Var&) const;
    bool operator<(const Var&) const;

    bool is_sym() const;
    std::string print_without_parens() const;
    std::string print() const;
    std::string print_with_type() const;

    Var* clone() const { return new Var(*this);}

    Var(const Located<std::string>& s):name(s) {}
    Var(const Located<std::string>& s, const ::Type& t):name(s),type(t) {}
};

struct Con: public Object
{
    Located<std::string> name;

    std::optional<int> arity;

    Core::wrapper wrap;

    bool operator==(const Object&) const;
    bool operator==(const Con&) const;

    bool is_sym() const;

    Con* clone() const { return new Con(*this);}
    std::string print() const;

    Con(const Located<std::string>& s):name(s) {}
    Con(const Located<std::string>& s, int a):name(s),arity(a) {}
};

}

namespace std
{
    template <>
    class hash < Hs::Var >{
    public :
        size_t operator()(const Hs::Var &x) const
        {
            return std::hash<std::string>()(unloc(x.name));
        }
    };
}

#endif
