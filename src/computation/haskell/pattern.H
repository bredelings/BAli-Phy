#ifndef HASKELL_PATTERN
#define HASKELL_PATTERN

#include "computation/object.H"
#include "computation/parser/located.H"

// Can we eliminate this?
#include "computation/expression/expression_ref.H"

#include "var.H"
#include "literal.H"

namespace Haskell
{

typedef expression_ref Pattern;
typedef expression_ref Pat;

std::string parenthesize_pattern(const Pattern& p);

struct VarPattern: public Object
{
    Var v;

    VarPattern* clone() const { return new VarPattern(*this);}
    std::string print() const;
};

struct ConPattern: public Object
{
    Con head;
    std::vector<Pattern> args;

    ConPattern* clone() const { return new ConPattern(*this);}
    std::string print() const;
};

struct TypedPattern: public Object
{
    Pattern pat;
    Type type;

    TypedPattern* clone() const { return new TypedPattern(*this);}
    std::string print() const;
};

struct LiteralPattern: public Object
{
    Literal lit;

    LiteralPattern* clone() const { return new LiteralPattern(*this);}
    std::string print() const;
};

struct WildcardPattern: public Object
{
    WildcardPattern* clone() const { return new WildcardPattern(*this);}
    std::string print() const;
};

struct ListPattern: public Object
{
    std::vector<Pattern> pats;
    ListPattern* clone() const { return new ListPattern(*this);}
    std::string print() const;
};

struct TuplePattern: public Object
{
    std::vector<Pattern> pats;
    TuplePattern* clone() const { return new TuplePattern(*this);}
    std::string print() const;
};

struct LazyPattern: public Object
{
    Pattern pattern;

    LazyPattern* clone() const { return new LazyPattern(*this);}
    std::string print() const;
    LazyPattern(const Pattern& p):pattern(p) {}
};

struct StrictPattern: public Object
{
    Pattern pattern;
    StrictPattern* clone() const { return new StrictPattern(*this);}
    std::string print() const;
    StrictPattern(const Pattern& p):pattern(p) {}
};

struct AsPattern: public Object
{
    // We need this to be Var until desugaring, at which point it becomes var.
    expression_ref var;
    Pattern pattern;

    AsPattern* clone() const { return new AsPattern(*this);}
    std::string print() const;
    AsPattern(const expression_ref& v, const Pattern& p):var(v),pattern(p) {}
};

// var pattern
// constructor pattern
// list pattern
// tuple pattern
// literal pattern (int,  char, double, string)    

}
#endif
