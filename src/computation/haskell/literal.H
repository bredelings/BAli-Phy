#ifndef HASKELL_LITERAL
#define HASKELL_LITERAL


#include <string>
#include <optional>
#include <variant>
#include "computation/object.H"
#include "computation/parser/located.H"

// Can we eliminate this? -- for the Ops
#include "computation/expression/expression_ref.H"

namespace Haskell
{

struct Char
{
    char value;
};

struct Integer
{
    int value;
    expression_ref fromIntegerOp;
    Integer(int i): value(i) {}
    Integer(int i, const expression_ref& op): value(i), fromIntegerOp(op) {}
};

struct String
{
    std::string value;
};

struct Double
{
    double value;
    expression_ref fromRationalOp;
    Double(double d): value(d) {}
    Double(double d, const expression_ref& op): value(d), fromRationalOp(op) {}
};

struct BoxedInteger
{
    int value;
};

struct Literal: public Object
{
    std::variant<Char, Integer, String, Double, BoxedInteger> literal;

    // In a pattern, we need an equals op to compare with.
    // Can we get away with a expression like (\x -> x == (fromInteger lit)) ??
    // Or do we need a Wrapper, that takes a var, and generates (var == (fromInteger lit))?
    expression_ref equalsOp; // ??

    Literal* clone() const {return new Literal(*this);}
    std::string print() const;

    std::optional<char> is_Char() const;
    std::optional<int> is_Integer() const;
    std::optional<std::string> is_String() const;
    std::optional<double> is_Double() const;
    std::optional<int> is_BoxedInteger() const;

    template <typename T>
    Literal(const T& t): literal(t) {}
};

}

#endif
