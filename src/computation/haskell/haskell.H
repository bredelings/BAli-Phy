#ifndef HASKELL_H
#define HASKELL_H

#include <string>
#include <utility>
#include <variant>
#include <map>
#include <set>
#include "computation/object.H"
#include "computation/expression/expression_ref.H"
#include "computation/parser/located.H"

#include "core.H"
#include "var.H"
#include "literal.H"
#include "pattern.H"
#include "type.H"

/*
  - For renaming class decls, can we first run group_decls on them?
    + We need to know the set of bound names for the whole module in order to rename the RHSs, right?

  - To collect free_vars, maybe we can add a set<string>& free_vars argument to rename.
    + When we analyze a term, we can automatically add free vars into it.
    + When we exit (say) a lambda expression, we could remove the binders from the free_vars.
    + Should this be part of a monad, or should we pass the free_vars around?

  - handle top-level names using bound_var_info, instead of putting them in the symbol table?
    + top-level value names include: constructors, field names, class methods, builtins
    + we look up constructor names in a different table than variable names.  Should we split the tables?
    + are top-level names different than let/lambda/case bound names, because they don't shadow things
         from other modules?
    + when computing free variables, we should probably only return local variables (in `bound`)
         and top-level vars from the same module.

  - handle top-level fixities using a fixity env that we can modify as we go, similar to lower-level fixity info?
    + Maybe implement lower-level fixity info FIRST, then make it handle the top level, kind of?
    + Although fixities ultimately correspond to objects, don't locally declared fixities just correspond to names?
      Thus, for example, a type and a constructor with the same name will get the same fixity?
      Isn't this what allows us to parse infix expression in fundecls before we know what which term is the
         function being declared?

  - make a local fixity environment for use in infix handling.
  - rewrite infix handling (and add an Infix object)
    + there's a problem handling infix expressions of size 1?
    + fixities wouldn't be used after rename.
    + is fixity a property of objects, again?
  - instead of fetching a symbol for each op, just get its fixity
    + this should make introducing a fixity environment easier
    + however, we need the symbol name to complain if we have (.. op -e) and op has precedence >= 6

  - convert patterns expression into pattern AST nodes, like TuplePattern
    + qual: bindpat "<-" exp
    + alt:  pat alt_rhs
    + decl_no_th: infixexp_top rhs <| we'd have to fix this up after we figure out if infixexp_top is a pattern
    + decl_no_th: ! aexp rhs       <| ditto, I think?
  - make a ConstructorPattern ast node.

  - combine rename_infix( ) with rename( )
    + currently we create symbol info between the two.
    + is this equivalent to creating some name  environments, like tc_envs, and tc_bndrs
    + currently many of our rename_foo functions return a bound_var_info, which is much like a name environment.

  - divide value declarations into binding groups inside rename.
    + renaming expressions needs to return free variables, so we can compute the dependence structure
    + in some cases, we currently return the BOUND variables -- for example when we analyze decls.
      how does ghc handle this?

  - figure out how to make recursive variant types for e.g. Exp, Type, Pattern
    + rvariant<TypeVar, TypeCon, TypeApp, TupleType, ListType, StrictLazyType, ConstrainedType, ForallType>
    + we could also do (i) shared_ptr<variant< >>, (ii) variant<shared_ptr<Args>...>, or (ii) shared_ptr<Type>

  - split the symbol table into a symbol (variable) table, a constructor table, and a type table
    + then maybe split the type table into a class table?
    + but maybe we need a tycon table see whether a tycon is a type, newtype, data, or class?

  - make sections use Name for the op?
  - make infix use Name for the op?
*/

namespace Haskell
{

typedef expression_ref Expression;
typedef expression_ref Exp;
typedef expression_ref Qual;

struct ApplyExp: public Object
{
    Located<Exp> head;
    std::vector<Located<Exp>> args;

    std::vector<Core::wrapper> arg_wrappers;
    std::vector<Core::wrapper> res_wrappers;

    ApplyExp* clone() const { return new ApplyExp(*this);}
    std::string print() const;

    ApplyExp(const Located<Exp>& h, const std::vector<Located<Exp>>& as);
};

void flatten(ApplyExp& app);

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

std::string parenthesize_exp(const Expression& E);

struct Neg: public Object
{
    expression_ref negate_op;

    Neg* clone() const { return new Neg(*this);}
    std::string print() const;

    Neg() = default;
};

struct InfixExp: public Object
{
    std::vector<expression_ref> terms;

    InfixExp* clone() const { return new InfixExp(*this);}
    std::string print() const;

    InfixExp() = default;
    InfixExp(const std::vector<expression_ref>& ts): terms(ts) {}
};


struct TypedExp: public Object
{
    Expression exp;
    Type type;
    Core::wrapper wrap;

    TypedExp* clone() const { return new TypedExp(*this);}
    std::string print() const;

    TypedExp() = default;
    TypedExp(const Expression& e, const Type& t): exp(e), type(t) {}
};

struct SignatureDecl: public Object
{
    std::vector<Var> vars;
    Type type;

    SignatureDecl* clone() const { return new SignatureDecl(*this);}
    std::string print() const;

    SignatureDecl() = default;
    SignatureDecl(const std::vector<Var>& vs, const Type& t): vars(vs), type(t) {}
};

struct ForeignDecl: public Object
{
    std::string plugin_name;
    std::string symbol_name;
    std::string function_name;
    Type type;

    int n_args() const;

    ForeignDecl* clone() const {return new ForeignDecl(*this);}
    std::string print() const;

    ForeignDecl(const std::string& n, const std::string& o, const Type& t);
};

// I think that these can originally be only
// * FixityDecl
// * SignatureDecl
// * ValueDecl

// Eventually we replace ValueDecl with PatDecl and FunDecl,
// move SignatureDecls to a map, and remove FixityDecls.

struct Decls: public std::vector<expression_ref>
{
public:
    std::optional<bool> recursive;

    std::string print() const;

    Decls() = default;
    Decls(const vector<expression_ref>& ds): std::vector<expression_ref>(ds) {}
};

struct Binds: public std::vector<Decls>
{
    // FIXME: I should keep the SignatureDecl around for error messages.
    std::map<Var, Type> signatures;

    std::string print() const;

    using std::vector<Decls>::vector;
};

Binds operator+(const Binds& b1, const Binds& b2);

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
    std::vector<Var> field_names;
    Type type;
    std::string print()  const;

    FieldDecl() = default;       // so this can be a token value.
    FieldDecl(const std::vector<Var>& vs, const Type& t):field_names(vs),type(t) {}
};


struct FieldDecls: public Object
{
    std::vector<FieldDecl> field_decls;

    std::string print()  const;
    FieldDecls* clone() const { return new FieldDecls(*this);}
    FieldDecls() = default;      // so this can be a token value.
    FieldDecls(const std::vector<FieldDecl>& ds):field_decls(ds) {}
};

struct GADTConstructorDecl
{
    std::vector<Located<std::string>> con_names;
    Located<Type> type;
    std::string print() const;

    GADTConstructorDecl() = default; // so this can be a token value
    GADTConstructorDecl(const std::vector<Located<std::string>>& cs, const Located<Type>& t):con_names(cs), type(t) {}
};

struct GADTConstructorsDecl: public std::vector<GADTConstructorDecl>
{
    std::string print() const;
    using std::vector<GADTConstructorDecl>::vector;
};

struct ConstructorDecl
{
    std::vector<TypeVar> forall;           // list of type variables we are forall-ing over.
    std::optional<Context> context;  // type constraints

    std::string name;                // the name of the constructor
    std::variant< std::vector<Type>, FieldDecls> fields;

    std::vector<Type> get_field_types() const;

    std::string print() const;

    bool is_record_constructor() const;

    int arity() const;

    ConstructorDecl() = default;

    ConstructorDecl(const std::vector<TypeVar>& tvs, std::optional<Context> c, const std::string& s, std::vector<Type>& as)
        :forall(tvs),
         context(c),
         name(s),
         fields(as)
   { }

    ConstructorDecl(const std::vector<TypeVar>& tvs, std::optional<Context> c, const std::string& s, const FieldDecls& fds)
        :forall(tvs),
         context(c),
         name(s),
         fields(fds)
   { }
};

struct ConstructorsDecl: public std::vector<ConstructorDecl>
{
    std::optional<ConstructorDecl> find_constructor_by_name(const std::string& s) const;
    std::string print() const;
    using std::vector<ConstructorDecl>::vector;
};

// TODO: rename Constructor to ConstructorDecl?
// How to incorporate GADT declarations?

enum class DataOrNewtype {data,newtype};

struct DataOrNewtypeDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    // Should I factor out this group into a DataOrNewtypeHeader?
    DataOrNewtype data_or_newtype;
    std::string name;
    std::vector<TypeVar> type_vars;
    Context context;
    std::optional<Kind> kind_sig;

    // Do we need a variant where this is absent?
    std::variant<std::monostate,ConstructorsDecl, GADTConstructorsDecl> constructors;

    bool is_empty_decl() const;
    bool is_regular_decl() const;
    bool is_gadt_decl() const;

    const ConstructorsDecl& get_constructors() const;
          ConstructorsDecl& get_constructors();

    const GADTConstructorsDecl& get_gadt_constructors() const;
          GADTConstructorsDecl& get_gadt_constructors();

//  Instructors for deriving instances...
//    std::optional<derivings> derivings;

    DataOrNewtypeDecl* clone() const { return new DataOrNewtypeDecl(*this);}
    std::string print() const;

    DataOrNewtypeDecl(DataOrNewtype dn,
                      const std::string& s,
                      const std::vector<TypeVar>& tvs,
                      const Context& ct,
                      const std::optional<Kind>& k)
        :data_or_newtype(dn),
         name(s),
         type_vars(tvs),
         context(ct),
         kind_sig(k)
        {}

    DataOrNewtypeDecl(DataOrNewtype dn,
                      const std::string& s,
                      const std::vector<TypeVar>& tvs,
                      const Context& ct,
                      const std::optional<Kind>& k,
                      const ConstructorsDecl& cs)
        :data_or_newtype(dn),
         name(s),
         type_vars(tvs),
         context(ct),
         kind_sig(k),
         constructors(cs)
        {}

    DataOrNewtypeDecl(DataOrNewtype dn,
                      const std::string& s,
                      const std::vector<TypeVar>& tvs,
                      const Context& ct,
                      const std::optional<Kind>& k,
                      const GADTConstructorsDecl& gadt_cs)
        :data_or_newtype(dn),
         name(s),
         type_vars(tvs),
         context(ct),
         kind_sig(k),
         constructors(gadt_cs)
        {}
};

struct TypeSynonymDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    std::string name;
    std::vector<TypeVar> type_vars;

    Located<Type> rhs_type;

    int arity() const {return type_vars.size();}

    TypeSynonymDecl* clone() const { return new TypeSynonymDecl(*this);}
    std::string print() const;

    TypeSynonymDecl(const std::string& s,
                    const std::vector<TypeVar>& tvs,
                    const Located<Type>& rhs)
        :name(s),
         type_vars(tvs),
         rhs_type(rhs)
        {}
};

struct TypeFamilyInstanceEqn: public Object
{
    // std::vector<TypeVar> forall;
    TypeCon con;
    std::vector<Type> args;
    Type rhs;

    TypeFamilyInstanceEqn* clone() const { return new TypeFamilyInstanceEqn(*this);}
    std::string print() const;

    TypeFamilyInstanceEqn() = default;
    TypeFamilyInstanceEqn(const TypeCon& tc, const std::vector<Type>& as, const Type& t):
        con(tc), args(as), rhs(t)
    {}
};

struct TypeFamilyInstanceDecl: public TypeFamilyInstanceEqn
{
    TypeFamilyInstanceDecl* clone() const { return new TypeFamilyInstanceDecl(*this);}

    std::string print() const;

    using TypeFamilyInstanceEqn::TypeFamilyInstanceEqn;
    TypeFamilyInstanceDecl(const TypeFamilyInstanceEqn& e):TypeFamilyInstanceEqn(e) {}
};

struct TypeFamilyDecl: public Object
{
    TypeCon con;
    std::vector<TypeVar> args;
    std::optional<Kind> kind_sig;
    // optional injectivity clause
    std::optional<std::vector<TypeFamilyInstanceEqn>> where_instances;

    int arity() const {return args.size();}
    Kind result_kind() const;
    std::vector<Kind> arg_kinds() const;
    Kind kind() const;

    TypeFamilyDecl* clone() const { return new TypeFamilyDecl(*this);}

    std::string print() const;

    TypeFamilyDecl(const TypeCon& tc, const std::vector<TypeVar>& as, const std::optional<Kind>& ks,
                   const std::optional<std::vector<TypeFamilyInstanceEqn>>& es)
        :con(tc),
         args(as),
         kind_sig(ks),
         where_instances(es)
    { }
};

struct ClassDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    Context context;
    std::string name;
    std::vector<TypeVar> type_vars;

    std::vector<FixityDecl> fixity_decls;

    std::vector<TypeFamilyDecl> type_fam_decls;
    std::vector<TypeFamilyInstanceDecl> default_type_inst_decls;
    std::vector<SignatureDecl> sig_decls;
    Decls default_method_decls;

    ClassDecl* clone() const { return new ClassDecl(*this);}
    std::string print() const;

    ClassDecl(const Context& ct,
              const std::string& s,
              const std::vector<TypeVar>& tvs,
              const std::vector<FixityDecl>& fxs,
              const std::vector<TypeFamilyDecl>& tfds,
              const std::vector<TypeFamilyInstanceDecl>& tids,
              const std::vector<SignatureDecl>& sds,
              const Decls& dms)
        :context(ct),
         name(s),
         type_vars(tvs),
         fixity_decls(fxs),
         type_fam_decls(tfds),
         default_type_inst_decls(tids),
         sig_decls(sds),
         default_method_decls(dms)
        {}
};

struct InstanceDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    // This should be something like Eq a => Eq [a]
    // Right now it could be either ForallType or ConstrainedType
    Context context;
    Type constraint;

    std::vector<TypeFamilyInstanceDecl> type_inst_decls;
    Decls method_decls;

    InstanceDecl* clone() const { return new InstanceDecl(*this);}
    std::string print() const;

    InstanceDecl(const Context& c,
                 const Type& t,
                 const std::vector<TypeFamilyInstanceDecl>& tids,
                 const Decls& ds)
        :context(c),
         constraint(t),
         type_inst_decls(tids),
         method_decls(ds)
        {}
};

struct DefaultDecl: public Object
{
    std::vector<Type> types;
    DefaultDecl() {}
    DefaultDecl* clone() const { return new DefaultDecl(*this);}
    std::string print() const;
    DefaultDecl(const std::vector<Type>& ts):types(ts) {}
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
    expression_ref enumFromOp;
    ListFrom* clone() const { return new ListFrom(*this);}
    std::string print() const;
    ListFrom(const expression_ref& e): from(e) {}
};

struct ListFromThen: public Object
{
    expression_ref from;
    expression_ref then;
    expression_ref enumFromThenOp;
    ListFromThen* clone() const { return new ListFromThen(*this);}
    std::string print() const;
    ListFromThen(const expression_ref& e1, const expression_ref& e2): from(e1), then(e2) {}
};

struct ListFromTo: public Object
{
    expression_ref from;
    expression_ref to;
    expression_ref enumFromToOp;
    ListFromTo* clone() const { return new ListFromTo(*this);}
    std::string print() const;
    ListFromTo(const expression_ref& e1, const expression_ref& e2): from(e1), to(e2) {}
};

struct ListFromThenTo: public Object
{
    expression_ref from;
    expression_ref then;
    expression_ref to;
    expression_ref enumFromThenToOp;
    ListFromThenTo* clone() const { return new ListFromThenTo(*this);}
    std::string print() const;
    ListFromThenTo(const expression_ref& e1, const expression_ref& e2, const expression_ref& e3): from(e1), then(e2), to(e3) {}
};

struct ListComprehension: public Object
{
    expression_ref body;
    std::vector<Qual> quals;
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
    Tuple(const std::vector<expression_ref>& es): elements(es)
        {
            assert(es.size() != 1);
        }
};

Expression tuple(const std::vector<Expression>& es);

struct GuardedRHS
{
    std::vector<expression_ref> guards;
    expression_ref body;

    std::string print() const;
    std::string print_no_equals() const;

    GuardedRHS() = default;
    GuardedRHS(const std::vector<expression_ref> v, const expression_ref& e)
        :guards(v),
         body(e)
    { }
};

struct MultiGuardedRHS
{
    std::vector<GuardedRHS> guarded_rhss;
    // We might need to make this into a shared_ptr< > to avoid an infinite structure.
    std::optional<Located<Binds>> decls;

    std::string print() const;
    std::string print_no_equals() const;

    MultiGuardedRHS() = default;
    MultiGuardedRHS(const std::vector<GuardedRHS>& v):guarded_rhss(v) {}
    MultiGuardedRHS(const std::vector<GuardedRHS>& v, const std::optional<Located<Binds>>& ds):guarded_rhss(v), decls(ds) {}
};

MultiGuardedRHS SimpleRHS(const Located<expression_ref>&, const std::optional<Located<Binds>>& ds = {});

struct StrictValueDecl: public Object
{
    Located<expression_ref> lhs;
    MultiGuardedRHS rhs;

    StrictValueDecl* clone() const { return new StrictValueDecl(*this);}
    std::string print() const;

    StrictValueDecl() = default;
    StrictValueDecl(const Located<expression_ref>& l, const expression_ref& r): lhs(l), rhs(SimpleRHS({noloc,r})) {}
    StrictValueDecl(const Located<expression_ref>& l, const MultiGuardedRHS& r): lhs(l), rhs(r) {}
};

// Where do we make ValueDecl's with an rhs that is a plain expression?
// Can we just make a SimpleValueDecl from the plain expression using a SimpleRHS?

struct ValueDecl: public Object
{
    Located<expression_ref> lhs;
    MultiGuardedRHS rhs;

    bool operator==(const Object& O) const;
    bool operator==(const ValueDecl& V) const;
    ValueDecl* clone() const { return new ValueDecl(*this);}
    std::string print() const;

    ValueDecl() = default;
    ValueDecl(const Located<expression_ref>& l, const expression_ref& r): lhs(l), rhs(SimpleRHS({noloc,r})) {}
    ValueDecl(const Located<expression_ref>& l, const MultiGuardedRHS& r): lhs(l), rhs(r) {}
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
    Located<Binds> binds;
    LetQual* clone() const { return new LetQual(*this);}
    std::string print()  const;
    LetQual(const Located<Binds>& b): binds(b) {}
};

struct SimpleQual: public Object
{
    expression_ref exp;
    expression_ref andThenOp;

    SimpleQual* clone() const { return new SimpleQual(*this);}
    std::string print() const;
    SimpleQual(const expression_ref &e): exp(e) {};
};

struct PatQual: public Object
{
    Pattern bindpat;
    expression_ref bindOp;
    expression_ref failOp;

    expression_ref exp;
    PatQual* clone() const { return new PatQual(*this);}
    std::string print() const;
    PatQual(const expression_ref &bp, const expression_ref& e): bindpat(bp), exp(e) {};
};

struct RecStmt: public Object
{
    Stmts stmts;
    expression_ref mfixOp;

    RecStmt* clone() const {return new RecStmt(*this);}
    std::string print() const;

    RecStmt(const Stmts& s):stmts(s) {}
};

// at some point, we need to distinguish between TuplePattern and TupleExpression...    

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
    MultiGuardedRHS rhs;

    std::string print() const;

    Alt() = default;
    Alt(const Pattern& p, const MultiGuardedRHS& r):pattern(p), rhs(r) {};
};

struct Alts: public std::vector<Located<Alt>>
{
    std::string print() const;

    Alts() = default;
    Alts(const std::vector<Located<Alt>>& v): std::vector<Located<Alt>>(v) {};
};

struct LetExp: public Object
{
    Located<Binds> binds;
    Located<expression_ref> body;

    LetExp* clone() const {return new LetExp(*this);}
    std::string print() const;

    LetExp(const Located<Binds>& bs, const Located<expression_ref>& b): binds(bs), body(b) {};
};

LetExp simple_let(const Var&, const Expression&, const Expression&);

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

struct PatDecl: public Object
{
    PatDecl* clone() const {return new PatDecl(*this);}
    std::string print() const;

    Located<Pattern> lhs;
    MultiGuardedRHS rhs;

    // This holds vars discovered during renaming
    std::set<std::string> rhs_free_vars;

    PatDecl(const Located<Pattern>& p, const MultiGuardedRHS& r):lhs(p), rhs(r) {}
};

struct MRule
{
    std::string print() const;

    std::vector<Pattern> patterns;
    MultiGuardedRHS rhs;
    MRule(const std::vector<Pattern>& ps, const MultiGuardedRHS& r);
};

struct Matches: public std::vector<MRule>
{
    using std::vector<MRule>::vector;
};

struct LambdaExp: public Object
{
    MRule match;

    LambdaExp* clone() const {return new LambdaExp(*this);}
    std::string print() const;

    LambdaExp(const std::vector<Pattern>& ps, const expression_ref& b);
};


struct LambdaContext { };
struct CaseContext { };
struct FunctionContext
{
    std::string name;
};

struct MatchContext: std::variant<LambdaContext, CaseContext, FunctionContext>
{
    using std::variant<LambdaContext, CaseContext, FunctionContext>::variant;
    std::string print() const;
};

struct CaseExp: public Object
{
    Expression object;
    Matches alts;

    CaseExp* clone() const { return new CaseExp(*this);}
    std::string print() const;

    CaseExp(const Expression& o, const Alts& as);
    CaseExp(const Expression& o, const Matches& ms);
};

struct FunDecl: public Object
{
    FunDecl* clone() const {return new FunDecl(*this);}
    std::string print() const;

    Var v;
    Matches matches;

    // This holds vars discovered during renaming
    std::set<std::string> rhs_free_vars;

    FunDecl(const Var& vv, const Matches& ms): v(vv), matches(ms) {}
};

FunDecl simple_decl(const Var& v, const expression_ref&);
FunDecl simple_decl(const Var& v, const MultiGuardedRHS&);
FunDecl simple_fun_decl(const Var& v, const std::vector<Pattern>& pats, const expression_ref&);
FunDecl simple_fun_decl(const Var& v, const std::vector<Pattern>& pats, const MultiGuardedRHS&);

struct BindInfo
{
    // This is the true name of the id (can be qualified)
    Var outer_id;

    // This is the name inside the tuple (cannot be qualified)
    Var inner_id;

    // The pre-generalized type
    ::Type monotype;

    // The post-generalized type
    ::Type polytype;

    // Wrapper to apply types and dicts to the tuple entry.
    Core::wrapper wrap;

    BindInfo(const Var& o, const Var& i, const ::Type& m, const ::Type& p, const Core::wrapper& w)
        : outer_id(o), inner_id(i), monotype(m), polytype(p), wrap(w)
    {
        outer_id.type = polytype;
        inner_id.type = monotype;
    }
};

struct GenBind: public Object
{
    std::vector<::TypeVar> tv_args;

    // These variables MUST be type-annotated!
    std::vector<Core::Var> dict_args;

    // Information for calling each of the binders in the group.
    std::shared_ptr<const Core::Decls> dict_decls;

    // These are the actual declarations.
    Decls body;

    // Defaulting info for each binder
    std::map<Var, BindInfo> bind_infos;

    GenBind* clone() const {return new GenBind(*this);}
    std::string print() const;

    GenBind(const std::vector<::TypeVar>& v1,
            const std::vector<Core::Var>& v2,
            const std::shared_ptr<const Core::Decls>& ev,
            const Decls& d,
            const std::map<Var, BindInfo>& bi)
        :tv_args(v1),
         dict_args(v2),
         dict_decls(ev),
         body(d),
         bind_infos(bi)
    {
        assert(not bind_infos.empty());
    }
};

// We can initially put all the type or class decls into a single group.
// We can initially put all value decls into a single group.

struct ModuleDecls
{
    Decls type_decls;

    std::optional<DefaultDecl> default_decl;

    std::vector<FixityDecl> fixity_decls;

    Binds value_decls;

    std::vector<ForeignDecl> foreign_decls;

    ModuleDecls();
    ModuleDecls(const Decls& topdecls);
};

struct EvidenceDecls: public Object
{
    // Information for calling each of the binders in the group.
    std::shared_ptr<const Core::Decls> decls;

    Expression body;

    EvidenceDecls* clone() const {return new EvidenceDecls(*this);}
    std::string print() const;

    EvidenceDecls(const std::shared_ptr<const Core::Decls>& d, const Expression& e): decls(d), body(e) {}
};

Expression error(const std::string& s);

Con True();
Con False();
Con ConsCon();
Con Nil();
Con TupleCon(int n);

}

#endif
