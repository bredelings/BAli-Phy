#ifndef HASKELL_VAR
#define HASKELL_VAR

#include <functional>
#include <optional>
#include <memory>
#include <ostream>
#include <string>
#include "computation/parser/located.H"

#include "computation/core/wrap.H"

namespace Haskell
{

struct Var
{
    std::string name;
    // Frontend-only marker; serialized Vars are declaration names that no longer pass through the renamer.
    bool name_is_resolved = false;

    Core::wrapper wrap;
    bool operator==(const Var&) const;
    bool operator<(const Var&) const;

    bool is_sym() const;
    std::string print_without_parens() const;
    std::string print() const;

    template <class Archive>
    void serialize(Archive& ar)
    {
	// We should only be serializing Hs::Var for its use in defining variable names in ClassInfo
	ar(name);
    }

    Var(const std::string& s):name(s) {}

    // Construct a compiler-generated reference whose name already identifies its declaration.
    static Var resolved(const std::string& s)
    {
        Var var(s);
        var.name_is_resolved = true;
        return var;
    }

    Var() = default;
};

typedef Located<Var> LVar;

struct Con
{
    std::string name;

    std::optional<int> arity;

    Core::wrapper wrap;
    bool operator==(const Con&) const;

    bool is_sym() const;
    std::string print() const;

    Con(const std::string& s):name(s) {}
    Con(const std::string& s, int a):name(s),arity(a) {}
};

typedef Located<Con> LCon;

inline std::ostream& operator<<(std::ostream& o, const Var& v) { return o << v.print(); }

inline std::ostream& operator<<(std::ostream& o, const Con& c) { return o << c.print(); }

}

namespace Hs = Haskell;

namespace std
{
    template <>
    class hash < Haskell::Var >{
    public :
        size_t operator()(const Haskell::Var &x) const
        {
            return std::hash<std::string>()(x.name);
        }
    };
}

#endif
