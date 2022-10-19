#ifndef HASKELL_PATTERN
#define HASKELL_PATTERN

#include <vector>
#include <set>
#include <string>

#include "computation/object.H"
#include "computation/parser/located.H"

// Can we eliminate this?
#include "computation/expression/expression_ref.H"

#include "core.H"

#include "var.H"
#include "literal.H"
#include "type.H"

// Currently we translate expressions into these pattern objects in rename/pattern.
// But we are not using VarPattern or LiteralPattern.
// We do this AFTER we do deal with infix expression.

// Ideally we'd translate expression-or-pattern things to patterns BEFORE renaming.
// Then we'd need to to infix handling for patterns separately from expressions.

// We need to handle i.e.
//   x : y : z
//   x : y : - 1
// Now, ":" is not a pattern (or not obviously).
// And "-" is also not a pattern.

// There are a few problems:
// 1. Parsing things that could turn out to be either an expression or a pattern.
// 2. Storing the parsing result of something unresolved.
// 3. Performing infix resolution on patterns.


// I'd like to say that an AsPattern takes a Var and a Pattern.
// However, the parser has "aexp: qvar @ aexp; ...".
// We could do the disambiguation before we construct the AsPattern object.
// But the "aexp" node would still need to return an ExpressionOrPattern kind of thing.
// Could it just return variant<Expression,Pattern>?
// Or, pair<Expression,Pattern>, where the Expression is an error object if it cannot be an expression?

// Looks like GHC parses x@p as an expression, and stores in an intermediate thing.
// See AsPat, mkHsAsPatPV.
// So, ECP is an expression, command, or pattern.
//  - sometimes we know what it is.
//  - sometimes we don't.
// For e1 op e2, the op is specifically stored as an op.
// For an expression, this goes to (OppApp annotations e1 op e2)
// For a pattern, this goes to (PatBuilderOppApp p1 op p2 annotations)
//
// So... basically they have written a second parser that gives more specific error messages...
namespace Haskell
{

typedef expression_ref Pattern;
typedef expression_ref Pat;

std::string parenthesize_pattern(const Pattern& p);

struct VarPattern: public Object
{
    Var var;

    VarPattern* clone() const { return new VarPattern(*this);}
    std::string print() const;

    VarPattern(const Var& v):var(v) {}
};

struct ConPattern: public Object
{
    Con head;
    std::vector<Pattern> args;

    // Extra stuff for typechecking
    std::vector<Hs::TypeVar> universal_tyvars;        // What do we do with this?
    std::vector<Hs::TypeVar> existential_tyvars;      // What do we do with this?
    std::vector<std::pair<Core::Var, Type>> givens;
    std::shared_ptr<Core::Decls> ev_binds;

    // get given variables for dictionaries only (nothing for type equalities)
    std::vector<Core::Var> dict_args() const;
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

    LiteralPattern(const Literal& l): lit(l) {}
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

    ListPattern() {}
    ListPattern(const std::vector<Pattern>& es):elements(es) {}
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
    Var var;
    Pattern pattern;

    AsPattern* clone() const { return new AsPattern(*this);}
    std::string print() const;
    AsPattern(const Var& v, const Pattern& p):var(v),pattern(p) {}
};

std::set<Hs::Var> vars_in_pattern(const Hs::Pattern& pat);
std::set<Hs::Var> vars_in_patterns(const std::vector<Hs::Pattern>& pats);

}
#endif
