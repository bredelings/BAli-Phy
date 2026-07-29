#ifndef COMPUTATION_FIXITY_H
#define COMPUTATION_FIXITY_H

#include <string>

namespace Infix
{

enum class Associativity {left, none, right, unknown};

// Fixity belongs to an entity such as a constructor or variable, rather than to
// every entity that happens to have the same name.
struct Fixity
{
    Associativity associativity;
    int precedence;

    // Preserve the established associativity-then-precedence compiled-module layout.
    template <class Archive>
    void serialize(Archive& ar)
    {
        ar(associativity, precedence);
    }
};

struct Operator
{
    std::string name;
    Fixity fixity;
};

// Describes how two adjacent operators divide an unresolved infix chain.
enum class Comparison
{
    associate_left,
    associate_right,
    conflict
};

// Compare adjacent operator fixities using Haskell's precedence and associativity rules.
inline Comparison compare(const Fixity& left, const Fixity& right)
{
    if (left.precedence > right.precedence)
        return Comparison::associate_left;
    if (left.precedence < right.precedence)
        return Comparison::associate_right;
    if (left.associativity == right.associativity and left.associativity == Associativity::left)
        return Comparison::associate_left;
    if (left.associativity == right.associativity and left.associativity == Associativity::right)
        return Comparison::associate_right;
    return Comparison::conflict;
}

}

#endif
