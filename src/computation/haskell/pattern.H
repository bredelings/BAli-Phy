#ifndef HASKELL_PATTERN
#define HASKELL_PATTERN

#include "computation/object.H"
#include "computation/parser/located.H"

// Can we eliminate this?
#include "computation/expression/expression_ref.H"

namespace Haskell
{

typedef expression_ref Pattern;
typedef expression_ref Pat;

std::string parenthesize_pattern(const Pattern& p);

struct WildcardPattern: public Object
{
    WildcardPattern* clone() const { return new WildcardPattern(*this);}
    std::string print() const;
};

struct LazyPattern: public Object
{
    Pattern pattern;

    LazyPattern* clone() const { return new LazyPattern(*this);}
    std::string print() const;
    LazyPattern(const expression_ref& p):pattern(p) {}
};

struct StrictPattern: public Object
{
    Pattern pattern;
    StrictPattern* clone() const { return new StrictPattern(*this);}
    std::string print() const;
    StrictPattern(const expression_ref& p):pattern(p) {}
};

struct AsPattern: public Object
{
    // We need this to be Var until desugaring, at which point it becomes var.
    expression_ref var;
    Pattern pattern;

    AsPattern* clone() const { return new AsPattern(*this);}
    std::string print() const;
    AsPattern(const expression_ref& v, const expression_ref& p):var(v),pattern(p) {}
};

// var pattern
// constructor pattern
// list pattern
// tuple pattern
// literal pattern (int,  char, double, string)    

}
#endif
