#ifndef HASKELL_VAR
#define HASKELL_VAR

#include <memory>
#include "computation/object.H"
#include "computation/parser/located.H"

#include "type.H"
#include "coretype.H"
#include "core.H"

struct VarInfo;

namespace Haskell
{

struct Var: public Object
{
    std::string name;

    std::optional<::Type> type;

    Core::wrapper wrap;

    std::shared_ptr<const VarInfo> info;

    bool operator==(const Object&) const;
    bool operator==(const Var&) const;
    bool operator<(const Var&) const;

    bool is_sym() const;
    std::string print_without_parens() const;
    std::string print() const;
    std::string print_with_type() const;

    template <class Archive>
    void serialize(Archive& ar)
    {
	// We should only be serializing Hs::Var for its use in defining variable names in ClassInfo
	assert(not type);
	assert(not info);
	ar(name);
    }

    Var* clone() const { return new Var(*this);}

    Var(const std::string& s):name(s) {}
    Var(const std::string& s, const ::Type& t):name(s),type(t) {}

    Var() = default;
};

typedef Located<Var> LVar;

struct Con: public Object
{
    std::string name;

    std::optional<int> arity;

    Core::wrapper wrap;

    bool operator==(const Object&) const;
    bool operator==(const Con&) const;

    bool is_sym() const;

    Con* clone() const { return new Con(*this);}
    std::string print() const;

    Con(const std::string& s):name(s) {}
    Con(const std::string& s, int a):name(s),arity(a) {}
};

typedef Located<Con> LCon;

}

namespace std
{
    template <>
    class hash < Hs::Var >{
    public :
        size_t operator()(const Hs::Var &x) const
        {
            return std::hash<std::string>()(x.name);
        }
    };
}

#endif
