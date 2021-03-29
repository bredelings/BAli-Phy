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

struct ClassDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    std::string name;
    std::vector<expression_ref> type_args;
    std::optional<expression_ref> context;

    Located<expression_ref> decls;
    // std::vector<Located<expression_ref>> signatures;
    // std::vector<Located<expression_ref>> default_decls;

    ClassDecl* clone() const { return new ClassDecl(*this);}
    ClassDecl(const std::string& s,
              const std::vector<expression_ref>& as,
              const std::optional<expression_ref> ct,
              const Located<expression_ref>& ds)
        :name(s),
         type_args(as),
         context(ct),
         decls(ds)
        {}
};

enum class DataOrNewtype {data,newtype};

struct DataOrNewtypeDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    DataOrNewtype data_or_newtype;
    std::string name;
    std::vector<expression_ref> type_args;
    std::optional<expression_ref> context;

    expression_ref constructors;

    DataOrNewtypeDecl* clone() const { return new DataOrNewtypeDecl(*this);}

    DataOrNewtypeDecl(DataOrNewtype dn,
                      const std::string& s,
                      const std::vector<expression_ref>& as,
                      const std::optional<expression_ref> ct,
                      const expression_ref& cs)
        :data_or_newtype(dn),
         name(s),
         type_args(as),
         context(ct),
         constructors(cs)
        {}
};

struct TypeSynonymDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    Located<expression_ref> type1;
    Located<expression_ref> type2;

    TypeSynonymDecl* clone() const { return new TypeSynonymDecl(*this);}

    TypeSynonymDecl(const Located<expression_ref>& t1,
                    const Located<expression_ref>& t2)
        :type1(t1),
         type2(t2)
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



}

namespace Hs = Haskell;

#endif
