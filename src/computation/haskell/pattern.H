#ifndef HASKELL_PATTERN
#define HASKELL_PATTERN

#include "computation/object.H"
#include "computation/parser/located.H"

// Can we eliminate this?
#include "computation/expression/expression_ref.H"

#include "core.H"

#include "var.H"
#include "literal.H"

// Do we need an InfixPat? Maybe.
// We could try and convert expression_ref to pattern AFTER handling infix expressions,
// but then how would we store AsPatterns in the meantime?

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
    ConPattern(const Con& h, const std::vector<Pattern>& as):head(h),args(as) {}
};

struct TypedPattern: public Object
{
    Pattern pat;
    Type type;
    Core::wrapper wrap;

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
    std::vector<Pattern> elements;
    ListPattern* clone() const { return new ListPattern(*this);}
    std::string print() const;
};

struct TuplePattern: public Object
{
    std::vector<Pattern> elements;
    TuplePattern* clone() const { return new TuplePattern(*this);}
    std::string print() const;
    TuplePattern() {}
    TuplePattern(const std::vector<Pattern>& es): elements(es) {}
};

Pattern tuple_pattern(const std::vector<Pattern>& es);

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
    Var var;
    Pattern pattern;

    AsPattern* clone() const { return new AsPattern(*this);}
    std::string print() const;
    AsPattern(const Var& v, const Pattern& p):var(v),pattern(p) {}
};

// var pattern
// constructor pattern
// list pattern
// tuple pattern
// literal pattern (int,  char, double, string)    

}
#endif
