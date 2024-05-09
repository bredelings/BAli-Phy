#ifndef HASKELL_LITERAL
#define HASKELL_LITERAL


#include <string>
#include <optional>
#include <variant>
#include "computation/object.H"
#include "computation/parser/located.H"
#include "Integer.H"

// Can we eliminate this? -- for the Ops
#include "computation/expression/expression_ref.H"

namespace Haskell
{

struct Char
{
    char value;

    bool operator==(const Char&) const = default;
    Char(char c):value(c) {}
};

struct Integer
{
    integer value;
    bool operator==(const Integer&) const;

    expression_ref fromIntegerOp;
    Integer(integer i): value(i) {}
    Integer(integer i, const expression_ref& op): value(i), fromIntegerOp(op) {}
};

integer integerFromString(const std::string& s);

struct String
{
    std::string value;
    bool operator==(const String&) const = default;
    String(const std::string& s):value(s) {}
};

struct Floating
{
    rational value;

    bool operator==(const Floating&) const = default;

    expression_ref fromRationalOp;

    Floating(rational r):value(r) {}
    Floating(rational r, const expression_ref& op):value(r),fromRationalOp(op) {}
};

rational rationalFromString(const std::string& s);

struct BoxedInteger
{
    integer value;
    bool operator==(const BoxedInteger&) const = default;
};

struct Literal: public Object
{
    std::variant<Char, Integer, String, Floating, BoxedInteger> literal;

    bool operator==(const Literal&) const;
    bool operator==(const Object&) const override;

    // In a pattern, we need an equals op to compare with.
    // Can we get away with a expression like (\x -> x == (fromInteger lit)) ??
    // Or do we need a Wrapper, that takes a var, and generates (var == (fromInteger lit))?
    expression_ref equalsOp; // ??

    Literal* clone() const override {return new Literal(*this);}
    std::string print() const override;

    std::optional<char> is_Char() const;
    std::optional<integer> is_Integer() const;
    std::optional<std::string> is_String() const;
    std::optional<rational> is_Floating() const;
    std::optional<integer> is_BoxedInteger() const;

    template <typename T>
    Literal(const T& t): literal(t) {}
};

}

#endif
