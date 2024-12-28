#ifndef HASKELL_PATTERN
#define HASKELL_PATTERN

#include <vector>
#include <set>
#include <string>

#include "computation/object.H"
#include "computation/parser/located.H"

// Can we eliminate this?
#include "computation/expression/expression_ref.H"

#include "computation/core/ast.H"

#include "core.H"

#include "var.H"
#include "literal.H"
#include "type.H"
#include "coretype.H"

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
typedef Located<Pat> LPat;
typedef std::vector<LPat> LPats;

std::string parenthesize_pattern(const Pat& p);
std::string parenthesize_pattern(const LPat& p);

struct VarPattern: public Object
{
    LVar var;

    VarPattern* clone() const { return new VarPattern(*this);}
    std::string print() const;

    VarPattern(const LVar& v):var(v) {}
};

struct ConPattern: public Object
{
    LCon head;
    LPats args;

    // Extra stuff for typechecking
    std::vector<::TypeVar> universal_tyvars;        // What do we do with this?
    std::vector<::TypeVar> existential_tyvars;      // What do we do with this?
    std::vector<Core2::Var<>> given_dict_vars;
    std::shared_ptr<const Core2::Decls<>> ev_binds;

    // get given variables for dictionaries only (nothing for type equalities)
    ConPattern* clone() const { return new ConPattern(*this);}
    std::string print() const;
    ConPattern(const LCon& h, const LPats& as):head(h),args(as) {}
};


struct TypedPattern: public Object
{
    LPat pat;
    LType type;
    Core::wrapper wrap;

    TypedPattern* clone() const { return new TypedPattern(*this);}
    std::string print() const;
};

struct LiteralPattern: public Object
{
    Literal lit;
    bool operator==(const LiteralPattern&) const;
    bool operator==(const Object&) const override;

    Hs::Var equalsOp = {"Data.Eq.=="};

    LiteralPattern* clone() const override { return new LiteralPattern(*this);}
    std::string print() const override;

    LiteralPattern(const Literal& l): lit(l) {}
};

struct WildcardPattern: public Object
{
    WildcardPattern* clone() const { return new WildcardPattern(*this);}
    std::string print() const;
};

struct ListPattern: public Object
{
    LPats elements;
    ListPattern* clone() const { return new ListPattern(*this);}
    std::string print() const;

    ListPattern() {}
    ListPattern(const LPats& es):elements(es) {}
};

struct TuplePattern: public Object
{
    LPats elements;
    TuplePattern* clone() const { return new TuplePattern(*this);}
    std::string print() const;
    TuplePattern() {}
    TuplePattern(const LPats& es): elements(es) {}
};

Pattern tuple_pattern(const LPats& es);

struct LazyPattern: public Object
{
    LPat pattern;

    LazyPattern* clone() const { return new LazyPattern(*this);}
    std::string print() const;
    LazyPattern(const LPat& p):pattern(p) {}
};

struct StrictPattern: public Object
{
    LPat pattern;
    StrictPattern* clone() const { return new StrictPattern(*this);}
    std::string print() const;
    StrictPattern(const LPat& p):pattern(p) {}
};

struct AsPattern: public Object
{
    LVar var;
    LPat pattern;

    AsPattern* clone() const { return new AsPattern(*this);}
    std::string print() const;
    AsPattern(const LVar& v, const LPat& p):var(v),pattern(p) {}
};

std::set<LVar> vars_in_pattern(const LPat& pat);
std::set<LVar> vars_in_patterns(const LPats& pats);

ConPattern to_con_pat(const TuplePattern& T);
ConPattern to_con_pat(const ListPattern& L);
ConPattern to_con_pat(const std::string& s);

ConPattern TruePat();
ConPattern FalsePat();
ConPattern ConsPat(const LPat&, const LPat&);
ConPattern NilPat();

}
#endif
