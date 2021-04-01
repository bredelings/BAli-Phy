#ifndef HASKELL_H
#define HASKELL_H

#include <string>
#include <utility>
#include "computation/object.H"
#include "computation/expression/expression_ref.H"
#include "located.H"

namespace Haskell
{

// HsDecl = type or class declaration
//          instance declaration
//          deriving declaration
//          value declaration
//          signature declaration
//          standalone kind signature
//          default declaration
//          foreign declaration

// HsBindLR<idLeft,idRight> = FunBind (variable or function bindings)
//                              fun_extra :: free variables of the definition, after the renamer but before the typechecker
//                              fun_id :: LIdP<idLeft>
//                              fun_matches :: MatchGroup<idRight,LHsExpr<IdRight>>
//                            PatBind
//                              pat_extra ::
//                              pat_lhs :: LPat<idLeft>
//                              pat_rhs :: GRHSs<idRight,LHsExpr<IdRight>>
//                            VarBind  (apparently introduced by the typechecker)
//                              var_extra ::
//                              var_id :: IdP<idLeft>
//                              var_rhs :: LHsExpr<idRight>  --
//                            AbsBinds (also output by the typechecker to record *typechecked* and *generalized* bindings)
//
/*
 HsExpr =   HsVar LIdP
          | HsUnboundVar OccName  -- "hole" or unbound variable
          | HsConLikeOut ConLike
          | HsRecFld AmbiguousFieldOcc
          | -- overloaded label
          | -- implicit parameter
          | HsOverLit HsOverLit -- Overloaded literals
          | HsLit HsLit -- Simple (non-overloaded) literals
          | HsLam (MatchGroup (LHsExpr))
          | -- lambda case
          | HsApp -- Application
          | -- visible type application
          | OpApp LHsExpr LHsExpr LHsExpr -- ??
          | NegApp LHsExpr SyntaxExpr -- negation .. why SyntaxExpr?
          | HsPar LHsExpr -- parenthesized expression
          | SectionL LHsExpr LHsExpr -- left operation section
          | SectionR LHsExpr LHsExpr -- right operator section
          | ExplicitTuple [LHsTupArg] Boxity -- tuple and sections
          | -- unboxed sum types
          | HsCase LHsExpr (MatchGroup LHsExpr) -- case expressions
          | HsIf LHsExpr LHsExpr LHsExpr -- if expression
          | -- multi-if
          | HsLet LHsLocalBinds LHsExpr -- let expression
          | HsDo (HsStmtContext [ExprLStmt] -- do expression
          | ExplicitList [LHsExpr]
          | RecordCon LIdP (constructor name) HsRecordBinds (the fields)
          | RecordUpd ???
          | ExprWithTySig LHsExpr LHsSigWcType NoGhcTc
          | ArithSeq ArithSeqInfo

LPat = WildPat          -- wildcard pattern
     | VarPat LIdP      -- variable pattern
     | LazyPat LPat     -- lazy pattern
     | AsPat LIdP LPat  -- as pattern
     | ParPat LPat      -- parenthesized pattern (?)
     | BangPat LPat     -- bang pattern
     | ListPat [LPat]   -- list pattern
     | TuplePat [LPat] Boxity -- tuple patterns
     | SumPat           -- sum pattern ??
     | ConPat { pat_con :: located<ConLike> HsConPatDetails }
     | ViewPat          -- view pattern
     | SplicePat        -- splice pattern
     | LitPat (HsLit)   -- non-overloaded literal
     | NPat             -- natural pattern ??
     | NPlusKPat        -- n+k patterns
     | SigPat LPat (HsPatSigType NoGhcTc) -- pattern with a type signature?
*/

typedef expression_ref Type;
typedef expression_ref Pattern;
typedef expression_ref Expression;

struct Context
{
    std::vector<Haskell::Type> constraints;
    Context* clone() const { return new Context(*this);}

    std::string print() const;
    Context() = default;
    Context(const Context&) = default;
    Context(const std::vector<Haskell::Type>& cs): constraints(cs) {}
};

struct ClassDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    std::string name;
    std::vector<expression_ref> type_vars;
    Context context;

    Located<expression_ref> decls;
    // std::vector<Located<expression_ref>> signatures;
    // std::vector<Located<expression_ref>> default_decls;

    ClassDecl* clone() const { return new ClassDecl(*this);}
    ClassDecl(const std::string& s,
              const std::vector<expression_ref>& as,
              const Context& ct,
              const Located<expression_ref>& ds)
        :name(s),
         type_vars(as),
         context(ct),
         decls(ds)
        {}
};

enum class Fixity {infixl, infix, infixr, unknown};

struct FixityDecl: public Object
{
    Fixity fixity;
    std::optional<int> precedence;
    std::vector<std::string> names;

//    std::string print()  const;
    FixityDecl* clone() const { return new FixityDecl(*this);}

    FixityDecl() = default;  // so this can be a token value.
    FixityDecl(Fixity f, std::optional<int> oi, const std::vector<std::string>& v): fixity(f), precedence(oi), names(v) {}
};

struct FieldDecl
{
    std::vector<std::string> field_names;
    Type type;

    FieldDecl() = default;       // so this can be a token value.
    FieldDecl(const std::vector<std::string>& ns, const Type& t):field_names(ns),type(t) {}
};


struct FieldDecls: public Object
{
    std::vector<FieldDecl> field_decls;

    FieldDecls* clone() const { return new FieldDecls(*this);}
    FieldDecls() = default;      // so this can be a token value.
    FieldDecls(const std::vector<FieldDecl>& ds):field_decls(ds) {}
};


enum class DataOrNewtype {data,newtype};

struct DataOrNewtypeDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    DataOrNewtype data_or_newtype;
    std::string name;
    std::vector<expression_ref> type_vars;
    Context context;

    std::vector<expression_ref> constructors;

    DataOrNewtypeDecl* clone() const { return new DataOrNewtypeDecl(*this);}

    DataOrNewtypeDecl(DataOrNewtype dn,
                      const std::string& s,
                      const std::vector<expression_ref>& as,
                      const Context& ct,
                      const std::vector<expression_ref>& cs)
        :data_or_newtype(dn),
         name(s),
         type_vars(as),
         context(ct),
         constructors(cs)
        {}
};

struct TypeSynonymDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    std::string name;
    std::vector<expression_ref> type_vars;

    Located<expression_ref> rhs_type;

    TypeSynonymDecl* clone() const { return new TypeSynonymDecl(*this);}

    TypeSynonymDecl(const std::string& s,
                    const std::vector<expression_ref>& tvs,
                    const Located<expression_ref>& rhs)
        :name(s),
         type_vars(tvs),
         rhs_type(rhs)
        {}
};

struct InstanceDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    // This should be something like Eq a => Eq [a]
    Located<expression_ref> type;
    Located<expression_ref> decls;

    InstanceDecl* clone() const { return new InstanceDecl(*this);}
    
    InstanceDecl(const Located<expression_ref>& t,
                 const Located<expression_ref>& d)
        :type(t),
         decls(d)
        {}
};


struct List: public Object
{
    std::vector<expression_ref> elements;
    List* clone() const { return new List(*this);}
    std::string print() const;
    List(const std::vector<expression_ref>& es): elements(es) {}
};

struct Tuple: public Object
{
    std::vector<expression_ref> elements;
    Tuple* clone() const { return new Tuple(*this);}
    std::string print() const;
    Tuple(const std::vector<expression_ref>& es): elements(es) {}
};

struct Stmts: public Object
{
    std::vector<expression_ref> stmts;

    Stmts* clone() const { return new Stmts(*this);}
    std::string print() const;

    Stmts() = default;
    Stmts(const std::vector<expression_ref>& v):stmts(v) {}
};

struct LetQual: public Object
{
    expression_ref binds;
    LetQual* clone() const { return new LetQual(*this);}
    std::string print()  const;
    LetQual(const expression_ref& b): binds(b) {}
};

struct SimpleQual: public Object
{
    expression_ref exp;
    SimpleQual* clone() const { return new SimpleQual(*this);}
    std::string print() const;
    SimpleQual(const expression_ref &e): exp(e) {};
};

struct PatQual: public Object
{
    expression_ref bindpat;
    expression_ref exp;
    PatQual* clone() const { return new PatQual(*this);}
    std::string print() const;
    PatQual(const expression_ref &bp, const expression_ref& e): bindpat(bp), exp(e) {};
};

struct RecStmt: public Object
{
    Stmts stmts;

    RecStmt* clone() const {return new RecStmt(*this);}
    std::string print() const;

    RecStmt(const Stmts& s):stmts(s) {}
};

struct ID: public Object
{
    std::string name;
    ID* clone() const { return new ID(*this);}
    std::string print() const;
    ID(const std::string& s):name(s) {}
    ID(const char* s):name(s) {}
};

struct WildcardPattern: public Object
{
    WildcardPattern* clone() const { return new WildcardPattern(*this);}
    std::string print() const;
};

struct LazyPattern: public Object
{
    expression_ref pattern;
    LazyPattern* clone() const { return new LazyPattern(*this);}
    std::string print() const;
    LazyPattern(const expression_ref& p):pattern(p) {}
};

struct AsPattern: public Object
{
    expression_ref var;      // FIXME: change to a located ID after we stop changing the type from ID to var.
    expression_ref pattern;

    AsPattern* clone() const { return new AsPattern(*this);}
    std::string print() const;
    AsPattern(const expression_ref& v, const expression_ref& p):var(v),pattern(p) {}
};

// at some point, we need to distinguish between TuplePattern and TupleExpression...    

struct TupleType: public Object
{
    std::vector<Type> element_types;

    TupleType* clone() const { return new TupleType(*this);}
    std::string print() const;
    TupleType(const std::vector<Type>& v):element_types(v) {}
};

struct ListType: public Object
{
    Type element_type;

    ListType* clone() const { return new ListType(*this);}
    std::string print() const;
    ListType(const Type& t):element_type(t) {};
};

struct TypeVar: public Object
{
    std::string name;

    TypeVar* clone() const { return new TypeVar(*this);}
    std::string print() const;
    TypeVar(const std::string& s):name(s) {}
};

struct TypeVarOfKind: public Object
{
    std::string name;
    Type kind;

    TypeVarOfKind* clone() const { return new TypeVarOfKind(*this);}
    std::string print() const;
    TypeVarOfKind(const std::string& s,const Type& k):name(s),kind(k) {}
};

struct TypeOfKind: public Object
{
    Type type;
    Type kind;

    TypeOfKind* clone() const { return new TypeOfKind(*this);}
    std::string print() const;
    TypeOfKind(const Type& t,const Type& k):type(t),kind(k) {}
};

struct TypeApp: public Object
{
    Type head;
    Type arg;

    TypeApp* clone() const { return new TypeApp(*this);}
    std::string print() const;
    TypeApp(const Type& t1, const Type& t2):head(t1), arg(t2) {}
};

struct ForallType: public Object
{
    std::vector<expression_ref> type_var_binders;
    Type type;

    ForallType* clone() const { return new ForallType(*this);}
    std::string print() const;
    ForallType(const std::vector<expression_ref>& tvb, const Type& t):type_var_binders(tvb), type(t) {}
};

struct ConstrainedType: public Object
{
    Context context;
    Type type;

    ConstrainedType* clone() const { return new ConstrainedType(*this);}
    std::string print() const;
    ConstrainedType(const Context& c, const Type& t):context(c), type(t) {}
};

enum class StrictLazy {strict,lazy};

struct StrictLazyType: public Object
{
    StrictLazy strict_lazy;
    Type type;

    StrictLazyType* clone() const { return new StrictLazyType(*this);}
    std::string print() const;
    StrictLazyType(const StrictLazy& sl, const Type& t):strict_lazy(sl), type(t) {}
};

std::pair<Type,std::vector<Type>> decompose_type_apps(Type t);

struct Do: public Object
{
    Stmts stmts;

    Do* clone() const { return new Do(*this);}
    std::string print() const;

    Do() = default;
    Do(const Stmts& s):stmts(s) {}
};

struct MDo: public Object
{
    Stmts stmts;

    MDo* clone() const { return new MDo(*this);}
    std::string print() const;

    MDo() = default;
    MDo(const Stmts& s):stmts(s) {}
};

struct Alt
{
    Pattern pattern;
    expression_ref rhs;

    std::string print() const;

    Alt() = default;
    Alt(const Pattern& p, const expression_ref& r):pattern(p), rhs(r) {};
};

struct Alts: public Object
{
    std::vector<Alt> alts;
    Alts* clone() const { return new Alts(*this);}
    std::string print() const;

    Alts() = default;
    Alts(const std::vector<Alt>& v):alts(v) {};
};


struct SimpleRHS: public Object
{
    expression_ref body;
    expression_ref decls;

    SimpleRHS* clone() const { return new SimpleRHS(*this);}
    std::string print() const;

    SimpleRHS(const expression_ref& b): body(b) {}
    SimpleRHS(const expression_ref& b, const expression_ref& ds): body(b), decls(ds) {}
};

}

namespace Hs = Haskell;

#endif
