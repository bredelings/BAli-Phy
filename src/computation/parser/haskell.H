#ifndef HASKELL_H
#define HASKELL_H

#include <string>
#include <utility>
#include <variant>
#include "computation/object.H"
#include "computation/expression/expression_ref.H"
#include "located.H"

/*
  - convert patterns to use Haskell::Var
  - use WildcardPattern instead of var(-1)
  - stop using Haskell::Var for constructors -- make a Haskell::Con
    + but... infixexp can have both a Var or a Con as an operator.
    + maybe we need to just read Names, and then convert them to Var or Con in rename?
    + look at Parser.y
  - make sections use Name for the op?
  - make infix use Name for the op?
  - remove make_foo( ) functions from parser.y in honor of Haskell::Foo( ).
*/

namespace Haskell
{

typedef expression_ref Kind;
typedef expression_ref Type;
typedef expression_ref Pattern;
typedef expression_ref Expression;


struct Var: public Object
{
    Located<std::string> name;
    std::optional<Type> type;
    std::optional<int> index;

    bool operator==(const Object&) const;
    bool operator==(const Var&) const;
    bool operator<(const Var&) const;

    Var* clone() const { return new Var(*this);}
    std::string print() const;
    std::string print_with_type() const;

    Var(const Located<std::string>& s):name(s) {}
    Var(const Located<std::string>& s, const Type& t):name(s),type(t) {}
};

struct TypeVar: public Object
{
    Located<std::string> name;
    std::optional<Kind> kind;
    std::optional<int> index;

    bool operator==(const Object&) const;
    bool operator==(const TypeVar&) const;
    bool operator<(const TypeVar&) const;

    TypeVar* clone() const { return new TypeVar(*this);}
    std::string print() const;
    std::string print_with_kind() const;

    TypeVar() = default;
    TypeVar(const Located<std::string>& s):name(s) {}
    TypeVar(const Located<std::string>& s, const Kind& k):name(s),kind(k) {}
};

struct TypeCon: public Object
{
    Located<std::string> name;
    std::optional<Kind> kind;
    std::optional<int> index;

    bool operator==(const Object&) const;
    bool operator==(const TypeCon&) const;
    bool operator<(const TypeCon&) const;

    TypeCon* clone() const { return new TypeCon(*this);}
    std::string print() const;
    std::string print_with_kind() const;

    TypeCon() = default;
    TypeCon(const Located<std::string>& s):name(s) {}
    TypeCon(const Located<std::string>& s, const Kind& k):name(s),kind(k) {}
};

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

struct Context
{
    std::vector<Haskell::Type> constraints;
    Context* clone() const { return new Context(*this);}

    std::string print() const;
    Context() = default;
    Context(const Context&) = default;
    Context(const std::vector<Haskell::Type>& cs): constraints(cs) {}
};


struct TypedExp: public Object
{
    Expression exp;
    Type type;

    TypedExp* clone() const { return new TypedExp(*this);}
    std::string print() const;

    TypedExp() = default;
    TypedExp(const Expression& e, const Type& t): exp(e), type(t) {}
};

struct TypeDecl: public Object
{
    std::vector<Haskell::Var> vars;
    Type type;

    TypeDecl* clone() const { return new TypeDecl(*this);}
    std::string print() const;

    TypeDecl() = default;
    TypeDecl(const std::vector<Haskell::Var>& vs, const Type& t): vars(vs), type(t) {}
};

struct BuiltinDecl: public Object
{
    std::string function_name;
    int n_args;
    std::string symbol_name;
    std::string plugin_name;

    BuiltinDecl* clone() const {return new BuiltinDecl(*this);}
    std::string print() const;

    BuiltinDecl(const std::string& n, int a, const std::string& o)
        :function_name(n), n_args(a), symbol_name(n), plugin_name(o)
    {};
    BuiltinDecl(const std::string& n, int a, const std::string& l, const std::string& o)
        :function_name(n), n_args(a), symbol_name(l), plugin_name(o)
    {};
};

struct ValueDecl: public Object
{
    expression_ref lhs;
    expression_ref rhs;

    bool operator==(const Object& O) const;
    bool operator==(const ValueDecl& V) const;
    ValueDecl* clone() const { return new ValueDecl(*this);}
    std::string print() const;

    ValueDecl() = default;
    ValueDecl(const expression_ref& l, const expression_ref& r): lhs(l), rhs(r) {}
};

struct Decls: public std::vector<expression_ref>
{
protected:
    bool is_top = false;
public:
    bool is_top_level() const {return is_top;}

    Decls* clone() const { return new Decls(*this);}
    std::string print() const;

    Decls(bool b=false): is_top(b) {}
    Decls(const vector<expression_ref>& ds,bool b=false): std::vector<expression_ref>(ds),is_top(b) {}
};

enum class Fixity {infixl, infix, infixr, unknown};

struct FixityDecl: public Object
{
    Fixity fixity;
    std::optional<int> precedence;
    std::vector<std::string> names;

    FixityDecl* clone() const { return new FixityDecl(*this);}
    std::string print()  const;

    FixityDecl() = default;  // so this can be a token value.
    FixityDecl(Fixity f, std::optional<int> oi, const std::vector<std::string>& v): fixity(f), precedence(oi), names(v) {}
};

struct FieldDecl
{
    std::vector<Haskell::Var> field_names;
    Type type;
    std::string print()  const;

    FieldDecl() = default;       // so this can be a token value.
    FieldDecl(const std::vector<Haskell::Var>& vs, const Type& t):field_names(vs),type(t) {}
};


struct FieldDecls: public Object
{
    std::vector<FieldDecl> field_decls;

    std::string print()  const;
    FieldDecls* clone() const { return new FieldDecls(*this);}
    FieldDecls() = default;      // so this can be a token value.
    FieldDecls(const std::vector<FieldDecl>& ds):field_decls(ds) {}
};

struct Constructor
{
    std::vector<TypeVar> forall;           // list of type variables we are forall-ing over.
    std::optional<Context> context;  // type constraints

    std::string name;                // the name of the constructor
    std::variant< std::vector<Type>, FieldDecls> fields;

    std::string print() const;

    bool is_record_constructor() const;

    int arity() const;

    Constructor() = default;

    Constructor(const std::vector<TypeVar>& tvs, std::optional<Context> c, const std::string& s, std::vector<Type>& as)
        :forall(tvs),
         context(c),
         name(s),
         fields(as)
   { }

    Constructor(const std::vector<TypeVar>& tvs, std::optional<Context> c, const std::string& s, const FieldDecls& fds)
        :forall(tvs),
         context(c),
         name(s),
         fields(fds)
   { }
};

enum class DataOrNewtype {data,newtype};

struct DataOrNewtypeDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    DataOrNewtype data_or_newtype;
    std::string name;
    std::vector<TypeVar> type_vars;
    Context context;

    std::vector<Constructor> constructors;

    // std::optional<derivings> derivings;

    DataOrNewtypeDecl* clone() const { return new DataOrNewtypeDecl(*this);}
    std::string print() const;

    DataOrNewtypeDecl(DataOrNewtype dn,
                      const std::string& s,
                      const std::vector<TypeVar>& tvs,
                      const Context& ct,
                      const std::vector<Constructor>& cs)
        :data_or_newtype(dn),
         name(s),
         type_vars(tvs),
         context(ct),
         constructors(cs)
        {}
};

struct TypeSynonymDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    std::string name;
    std::vector<TypeVar> type_vars;

    Located<expression_ref> rhs_type;

    TypeSynonymDecl* clone() const { return new TypeSynonymDecl(*this);}
    std::string print() const;

    TypeSynonymDecl(const std::string& s,
                    const std::vector<TypeVar>& tvs,
                    const Located<expression_ref>& rhs)
        :name(s),
         type_vars(tvs),
         rhs_type(rhs)
        {}
};

struct ClassDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    std::string name;
    std::vector<TypeVar> type_vars;
    Context context;

    std::optional<Located<Haskell::Decls>> decls;
    // std::vector<Located<expression_ref>> signatures;
    // std::vector<Located<expression_ref>> default_decls;

    ClassDecl* clone() const { return new ClassDecl(*this);}
    std::string print() const;

    ClassDecl(const std::string& s,
              const std::vector<TypeVar>& tvs,
              const Context& ct,
              const std::optional<Located<Haskell::Decls>>& ds)
        :name(s),
         type_vars(tvs),
         context(ct),
         decls(ds)
        {}
};

struct InstanceDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    // This should be something like Eq a => Eq [a]
    // Right now it could be either ForallType or ConstrainedType
    Haskell::Context context;
    std::string name;
    std::vector<Haskell::Type> type_args;

    std::optional<Located<Haskell::Decls>> decls;

    InstanceDecl* clone() const { return new InstanceDecl(*this);}
    std::string print() const;

    InstanceDecl(const Haskell::Context& c,
                 const std::string& n,
                 const std::vector<Haskell::Type> ta,
                 const std::optional<Located<Haskell::Decls>>& d)
        :context(c),
         name(n),
         type_args(ta),
         decls(d)
        {}
};

struct DefaultDecl: public Object
{
    std::vector<Haskell::Type> types;
    DefaultDecl() {}
    DefaultDecl* clone() const { return new DefaultDecl(*this);}
    std::string print() const;
    DefaultDecl(const std::vector<Haskell::Type>& ts):types(ts) {}
};


struct List: public Object
{
    std::vector<expression_ref> elements;
    List* clone() const { return new List(*this);}
    std::string print() const;
    List(const std::vector<expression_ref>& es): elements(es) {}
};

struct ListFrom: public Object
{
    expression_ref from;
    ListFrom* clone() const { return new ListFrom(*this);}
    std::string print() const;
    ListFrom(const expression_ref& e): from(e) {}
};

struct ListFromThen: public Object
{
    expression_ref from;
    expression_ref then;
    ListFromThen* clone() const { return new ListFromThen(*this);}
    std::string print() const;
    ListFromThen(const expression_ref& e1, const expression_ref& e2): from(e1), then(e2) {}
};

struct ListFromTo: public Object
{
    expression_ref from;
    expression_ref to;
    ListFromTo* clone() const { return new ListFromTo(*this);}
    std::string print() const;
    ListFromTo(const expression_ref& e1, const expression_ref& e2): from(e1), to(e2) {}
};

struct ListFromThenTo: public Object
{
    expression_ref from;
    expression_ref then;
    expression_ref to;
    ListFromThenTo* clone() const { return new ListFromThenTo(*this);}
    std::string print() const;
    ListFromThenTo(const expression_ref& e1, const expression_ref& e2, const expression_ref& e3): from(e1), then(e2), to(e3) {}
};

struct ListComprehension: public Object
{
    expression_ref body;
    std::vector<expression_ref> quals;
    ListComprehension* clone() const { return new ListComprehension(*this);}
    std::string print() const;
    ListComprehension(const expression_ref& e, const std::vector<expression_ref>& es): body(e), quals(es) {}
};

struct LeftSection: public Object
{
    expression_ref l_arg;
    expression_ref op;
    LeftSection* clone() const {return new LeftSection(*this);}
    std::string print() const;
    LeftSection(const expression_ref& e, const expression_ref& v):l_arg(e),op(v) {}
};

struct RightSection: public Object
{
    expression_ref op;
    expression_ref r_arg;
    RightSection* clone() const {return new RightSection(*this);}
    std::string print() const;
    RightSection(const expression_ref& v, const expression_ref& e):op(v), r_arg(e) {}
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
    Located<Haskell::Decls> binds;
    LetQual* clone() const { return new LetQual(*this);}
    std::string print()  const;
    LetQual(const Located<Haskell::Decls>& b): binds(b) {}
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

struct StrictPattern: public Object
{
    expression_ref pattern;
    StrictPattern* clone() const { return new StrictPattern(*this);}
    std::string print() const;
    StrictPattern(const expression_ref& p):pattern(p) {}
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
    std::vector<TypeVar> type_var_binders;
    Type type;

    ForallType* clone() const { return new ForallType(*this);}
    std::string print() const;
    ForallType(const std::vector<TypeVar>& tvb, const Type& t):type_var_binders(tvb), type(t) {}
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

struct Alts: public std::vector<Located<Alt>>
{
    std::string print() const;

    Alts() = default;
    Alts(const std::vector<Located<Alt>>& v): std::vector<Located<Alt>>(v) {};
};

struct CaseExp: public Object
{
    Expression object;
    Alts alts;

    CaseExp* clone() const { return new CaseExp(*this);}
    std::string print() const;

    CaseExp(const Expression& o, Alts as): object(o), alts(as) {}
};

struct GuardedRHS
{
    std::vector<expression_ref> guards;
    expression_ref body;

    std::string print() const;

    GuardedRHS() = default;
    GuardedRHS(const std::vector<expression_ref> v, const expression_ref& e)
        :guards(v),
         body(e)
    { }
};

struct MultiGuardedRHS: public Object
{
    std::vector<GuardedRHS> guarded_rhss;
    std::optional<Located<Haskell::Decls>> decls;

    MultiGuardedRHS* clone() const { return new MultiGuardedRHS(*this);}
    std::string print() const;

    MultiGuardedRHS(const std::vector<GuardedRHS>& v):guarded_rhss(v) {}
    MultiGuardedRHS(const std::vector<GuardedRHS>& v, const std::optional<Located<Haskell::Decls>>& ds):guarded_rhss(v), decls(ds) {}
};


struct SimpleRHS: public Object
{
    Located<expression_ref> body;
    std::optional<Located<Haskell::Decls>> decls;

    SimpleRHS* clone() const { return new SimpleRHS(*this);}
    std::string print() const;

    SimpleRHS(const Located<expression_ref>& b): body(b) {}
    SimpleRHS(const Located<expression_ref>& b, const std::optional<Located<Haskell::Decls>>& ds): body(b), decls(ds) {}
};

struct LambdaExp: public Object
{
    std::vector<Pattern> args;
    expression_ref body;

    LambdaExp* clone() const {return new LambdaExp(*this);}
    std::string print() const;

    LambdaExp(const std::vector<Pattern>& ps, const expression_ref& b): args(ps), body(b) {};
};


struct LetExp: public Object
{
    Located<Haskell::Decls> decls;
    Located<expression_ref> body;

    LetExp* clone() const {return new LetExp(*this);}
    std::string print() const;

    LetExp(const Located<Haskell::Decls>& d, const Located<expression_ref>& b): decls(d), body(b) {};
};

struct IfExp: public Object
{
    Located<expression_ref> condition;
    Located<expression_ref> true_branch;
    Located<expression_ref> false_branch;

    IfExp* clone() const {return new IfExp(*this);}
    std::string print() const;

    IfExp(const Located<expression_ref>& c, const Located<expression_ref>& t, const Located<expression_ref>& f)
        : condition(c),
          true_branch(t),
          false_branch(f)
        { }
};

struct ExportSubSpecSome
{
    std::vector<Located<std::string>> names;
    std::string print() const;
};

struct ExportSubSpecAll
{
    std::string print() const;
};

struct ExportSubSpec: public std::variant<ExportSubSpecSome, ExportSubSpecAll>
{
    std::string print() const;

    using variant::variant;
};

struct ExportSymbol
{
    Located<std::string> symbol;
    std::optional<ExportSubSpec> subspec;
    std::string print() const;
};

struct ExportModule
{
    Located<std::string> modid;
    std::string print() const;
};

struct Export: public std::variant<ExportSymbol,ExportModule>
{
    std::string print() const;

    using variant::variant;
};

struct ImpSpec
{
    bool hiding;
    std::vector<Export> imports;
    std::string print() const;
};

struct ImpDecl
{
    bool qualified;
    std::string modid;
    std::optional<std::string> as;
    std::optional<ImpSpec> impspec;

    std::string print() const;

    ImpDecl() = default;
    ImpDecl(bool b, const std::string& s, const std::optional<std::string>& a, const std::optional<ImpSpec>& is)
        :qualified(b),
         modid(s),
         as(a),
         impspec(is)
    {}
};

struct Module
{
    std::string modid;
    std::optional<std::vector<Export>> exports;
    std::vector<ImpDecl> impdecls;
    std::optional<Decls> topdecls;

    std::string print() const;

    Module() = default;
    Module(const std::string&s,
           const std::optional<std::vector<Export>>& e,
           const std::vector<ImpDecl>& i,
           const std::optional<Decls>& t)
        :modid(s),
         exports(e),
         impdecls(i),
         topdecls(t)
    {}
};

Type make_arrow_type(const Type& t1, const Type& t2);

}

namespace Hs = Haskell;

#endif
