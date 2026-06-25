#ifndef HASKELL_LITERAL
#define HASKELL_LITERAL


#include <string>
#include <optional>
#include <variant>
#include "computation/parser/located.H"
#include "Integer.H"
#include "var.H"

namespace Haskell
{

struct Char
{
    char32_t value;

    bool operator==(const Char&) const = default;
    Char(char32_t c):value(c) {}
};

struct Integer
{
    integer value;
    bool operator==(const Integer&) const;

    std::optional<Hs::Var> fromIntegerOp;
    Integer(integer i): value(i) {}
    Integer(integer i, const Hs::Var& op): value(i), fromIntegerOp(op) {}
};

integer integerFromString(const std::string& s);

struct String
{
    std::string value;
    bool operator==(const String&) const = default;

    std::optional<Hs::Var> fromStringOp;
    String(const std::string& s):value(s) {}
    String(const std::string& s, const Hs::Var& op):value(s), fromStringOp(op) {}
};

struct Floating
{
    rational value;

    bool operator==(const Floating&) const = default;

    std::optional<Hs::Var> fromRationalOp;

    Floating(rational r):value(r) {}
    Floating(rational r, const Hs::Var& op):value(r),fromRationalOp(op) {}
};

rational rationalFromString(const std::string& s);
rational rationalFromDouble(double x);

struct BoxedInteger
{
    integer value;
    bool operator==(const BoxedInteger&) const = default;
};

struct Literal
{
    std::variant<Char, Integer, String, Floating, BoxedInteger> literal;

    bool operator==(const Literal&) const;

    // In a pattern, we need an equals op to compare with.
    // Can we get away with a expression like (\x -> x == (fromInteger lit)) ??
    // Or do we need a Wrapper, that takes a var, and generates (var == (fromInteger lit))?
    std::optional<Hs::Var> equalsOp; // ??
    std::string print() const;

    std::optional<char32_t> is_Char() const;
    std::optional<integer> is_Integer() const;
    std::optional<std::string> is_String() const;
    std::optional<rational> is_Floating() const;
    std::optional<integer> is_BoxedInteger() const;

    template <typename T>
    Literal(const T& t): literal(t) {}
};

}

#endif
