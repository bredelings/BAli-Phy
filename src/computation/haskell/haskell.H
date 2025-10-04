#ifndef HASKELL_H
#define HASKELL_H

#include <string>
#include <utility>
#include <variant>
#include <map>
#include <set>
#include <tuple>
#include "computation/object.H"
#include "computation/expression/expression_ref.H"
#include "computation/parser/located.H"

#include "core.H"
#include "var.H"
#include "literal.H"
#include "pattern.H"
#include "type.H"

#include "computation/core/ast.H"
#include "computation/core/wrap.H"

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
    + rvariant<TypeVar, TypeCon, TypeApp, TupleType, ListType, StrictType, LazyType, ConstrainedType, ForallType>
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
typedef Located<Exp> LExp;
typedef expression_ref Qual;
typedef Located<Qual> LQual;

struct ApplyExp: public Object
{
    LExp head;
    LExp arg;

    Core2::wrapper arg_wrapper;
    Core2::wrapper res_wrapper;

    ApplyExp* clone() const { return new ApplyExp(*this);}
    std::string print() const;

    ApplyExp(const LExp& h, const LExp& a);
};

std::tuple<LExp,std::vector<LExp>> decompose_apps(const LExp&);

std::vector<LExp> flatten(const LExp&);

LExp apply(const std::vector<LExp>& terms);

LExp apply(const LExp& head, const std::vector<LExp>& args);

    
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
    std::optional<Hs::Var> negate_op;

    Neg* clone() const { return new Neg(*this);}
    std::string print() const;

    Neg() = default;
};

struct InfixExp: public Object
{
    std::vector<Located<Exp>> terms;

    InfixExp* clone() const { return new InfixExp(*this);}
    std::string print() const;

    InfixExp() = default;
    InfixExp(const std::vector<Located<expression_ref>>& ts): terms(ts) {}
};


struct TypedExp: public Object
{
    LExp exp;
    LType type;
    Core2::wrapper wrap;

    TypedExp* clone() const { return new TypedExp(*this);}
    std::string print() const;

    TypedExp() = default;
    TypedExp(const LExp& e, const LType& t): exp(e), type(t) {}
};

struct TypeSigDecl: public Object
{
    std::vector<LVar> vars;
    LType type;

    TypeSigDecl* clone() const { return new TypeSigDecl(*this);}
    std::string print() const;

    TypeSigDecl() = default;
    TypeSigDecl(const std::vector<LVar>& vs, const LType& t): vars(vs), type(t) {}
};

struct KindSigDecl: public Object
{
    std::vector<LTypeCon> tycons;
    Kind kind;

    KindSigDecl* clone() const { return new KindSigDecl(*this);}
    std::string print() const;

    KindSigDecl() = default;
    KindSigDecl(const std::vector<LTypeCon>& tcs, const Kind& k): tycons(tcs), kind(k) {}
};

struct ForeignDecl: public Object
{
    std::string plugin_name;
    std::string symbol_name;
    Hs::LVar function;
    LType type;

    ForeignDecl* clone() const {return new ForeignDecl(*this);}
    std::string print() const;

    ForeignDecl(const std::string& n, const Hs::LVar& f, const LType& t);
};

struct FieldBinding: public Object
{
    LVar field;
    std::optional<LExp> value;

    FieldBinding* clone() const { return new FieldBinding(*this);}
    std::string print() const;

    FieldBinding(const LVar& lv):field(lv) {}
    FieldBinding(const LVar& lv, const LExp& le):field(lv),value(le) {}
};

struct FieldBindings: public Object, std::vector<Located<FieldBinding>>
{
    bool dotdot = false;

    FieldBindings* clone() const { return new FieldBindings(*this);}
    std::string print() const;
};

// This could be
// * a record update    x {field = y} = case x of MkT a b c d e -> MkT a b y d e
// * a constructor    MkT {field = y} = MkT u u y u u where u = ??
// * a pattern        MkT {field = y} = MkT _ _ y _ _
// Then there are ".." and named field puns...

struct RecordExp: public Object
{
    LExp head;
    Located<FieldBindings> fbinds;

    RecordExp* clone() const { return new RecordExp(*this);}
    std::string print() const;

    RecordExp() = default;
    RecordExp(const LExp& e, const Located<FieldBindings>& fb): head(e), fbinds(fb) {}
};

// I think that these can originally be only
// * FixityDecl
// * TypeSigDecl
// * ValueDecl

// Eventually we replace ValueDecl with PatDecl and FunDecl,
// move TypeSigDecls to a map, and remove FixityDecls.

typedef expression_ref Decl;
typedef Located<Decl> LDecl;

enum class inline_pragma_t {INLINE, INLINABLE, NOINLINE};

std::ostream& operator<<(std::ostream& o, inline_pragma_t );    

struct InlinePragma: public Object
{
    Located<std::string> command_string;
    inline_pragma_t command;
    Located<std::string> var;
    InlinePragma* clone() const { return new InlinePragma(*this);}

    std::string print() const;

    InlinePragma(const Located<std::string>& s1, const Located<std::string>& s2);
};

    
struct Decls: public std::vector<LDecl>
{
public:
    std::optional<bool> recursive;

    std::string print() const;

    Decls() = default;
    Decls(const vector<LDecl>& ds): std::vector<LDecl>(ds) {}
};

struct Binds: public std::vector<Decls>
{
    // FIXME: I should keep the TypeSigDecl around for error messages.
    std::map<LVar, LType> signatures;

    std::map<LVar, inline_pragma_t> inline_sigs;

    std::string print() const;

    using std::vector<Decls>::vector;
};

typedef Located<Binds> LBinds;

Binds operator+(const Binds& b1, const Binds& b2);

enum class Fixity {infixl, infix, infixr, unknown};

struct FixityDecl: public Object
{
    Fixity fixity;
    std::optional<int> precedence;
    std::vector<Located<std::string>> names;

    FixityDecl* clone() const { return new FixityDecl(*this);}
    std::string print()  const;

    FixityDecl() = default;  // so this can be a token value.
    FixityDecl(Fixity f, std::optional<int> oi, const std::vector<Located<std::string>>& v): fixity(f), precedence(oi), names(v) {}
};

struct FieldDecl
{
    std::vector<LVar> field_names;
    LType type;
    std::string print()  const;

    FieldDecl() = default;       // so this can be a token value.
    FieldDecl(const std::vector<LVar>& vs, const LType& t):field_names(vs),type(t) {}
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

    // FIXME: handle GADT fielddecls Constr :: { name1 :: ArgType1, name2 :: ArgType2 } -> ResultType
    // Constraint: all constructors with the same field must have the same result type.
    LType type;
    std::string print() const;

    GADTConstructorDecl() = default; // so this can be a token value
    GADTConstructorDecl(const std::vector<Located<std::string>>& cs, const LType& t):con_names(cs), type(t) {}
};

struct GADTConstructorsDecl: public std::vector<GADTConstructorDecl>
{
    std::string print() const;
    using std::vector<GADTConstructorDecl>::vector;
};

struct ConstructorDecl
{
    std::vector<LTypeVar> forall;           // list of type variables we are forall-ing over.
    Context context;                        // type constraints

    std::optional<Hs::LCon> con;            // the name of the constructor
    std::variant< std::vector<LType>, FieldDecls> fields;

    std::vector<LType> get_field_types() const;

    std::string print() const;

    bool is_record_constructor() const;

    int arity() const;

    ConstructorDecl() = default;

    ConstructorDecl(const std::vector<LTypeVar>& tvs, const Context& ct, const Hs::LCon& c, std::vector<LType>& as)
        :forall(tvs),
         context(ct),
         con(c),
         fields(as)
   { }

    ConstructorDecl(const std::vector<LTypeVar>& tvs, const Context& ct, const Hs::LCon& c, const FieldDecls& fds)
        :forall(tvs),
         context(ct),
         con(c),
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

std::ostream& operator<<(std::ostream& o, DataOrNewtype);

struct DataDefn
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    // Should I factor out this group into a DataOrNewtypeHeader?
    DataOrNewtype data_or_newtype;
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

    std::string print() const;

    DataDefn(DataOrNewtype dn,
	    const Context& ct,
	    const std::optional<Kind>& k)
        :data_or_newtype(dn),
         context(ct),
         kind_sig(k)
        {}

    DataDefn(DataOrNewtype dn,
	    const Context& ct,
	    const std::optional<Kind>& k,
	    const ConstructorsDecl& cs)
        :data_or_newtype(dn),
         context(ct),
         kind_sig(k),
         constructors(cs)
        {}

    DataDefn(DataOrNewtype dn,
	     const Context& ct,
	     const std::optional<Kind>& k,
	     const GADTConstructorsDecl& gadt_cs)
        :data_or_newtype(dn),
         context(ct),
         kind_sig(k),
         constructors(gadt_cs)
        {}
};


struct DataOrNewtypeDecl: public Object, public DataDefn
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    // Should I factor out this group into a DataOrNewtypeHeader?
    LTypeCon con;
    std::vector<LTypeVar> type_vars;

    DataOrNewtypeDecl* clone() const { return new DataOrNewtypeDecl(*this);}
    std::string print() const;

    DataOrNewtypeDecl(const LTypeCon& c,
		      const std::vector<LTypeVar>& tvs,
		      const DataDefn& def)
        :DataDefn(def),
         con(c),
         type_vars(tvs)
        {}
};

struct TypeSynonymDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    LTypeCon con;
    std::vector<LTypeVar> type_vars;

    LType rhs_type;

    int arity() const {return type_vars.size();}

    TypeSynonymDecl* clone() const { return new TypeSynonymDecl(*this);}
    std::string print() const;

    TypeSynonymDecl(const LTypeCon& c,
                    const std::vector<LTypeVar>& tvs,
                    const Located<Type>& rhs)
        :con(c),
         type_vars(tvs),
         rhs_type(rhs)
        {}
};

struct TypeFamilyInstanceEqn: public Object
{
    Located<TypeCon> con;
    std::vector<LType> args;
    LType rhs;

    TypeFamilyInstanceEqn* clone() const { return new TypeFamilyInstanceEqn(*this);}
    std::string print() const;

    TypeFamilyInstanceEqn() = default;
    TypeFamilyInstanceEqn(const LTypeCon& tc, const std::vector<LType>& as, const LType& t):
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


// HsFamEqn pass (HsDataDefn pass)
// HsFamEqn pass rhs = {tycon :: Id pass, bndrs :: optional<vector<LTypeVar>>, pats :: vector<LHsTypeArg>, rhs :: rhs }
// - pats holds patterns on the LHS of a type instance, such as @k or (a::k)
// - optional<vector<LTypeVar>> determines whether we infer the binders (empty optional) or use the given forall, following
//   the forall-or-nothing rule.
//
// HsDataDefn pass = Maybe (LContext), Maybe (CType), Maybe Kind, constructors, deriving
// -- constructors<a> = Newtype a | DataType [a]
//
// See GHC/Tc/TyCl/Instance.hs


struct DataFamilyInstanceDecl: public Object
{
    Located<TypeCon> con;
    std::vector<LType> args;
    DataDefn rhs;
    
    DataFamilyInstanceDecl* clone() const {return new DataFamilyInstanceDecl(*this);}

    std::string print() const;

    DataFamilyInstanceDecl(const Located<TypeCon>& c, const std::vector<LType>& as, const DataDefn& d)
	:con(c), args(as), rhs(d)
	{}
};

enum FamilyInfo { TypeFamily, DataFamily };

struct FamilyDecl: public Object
{
    FamilyInfo info;
    LTypeCon con;
    std::vector<LTypeVar> args;
    std::optional<Kind> kind_sig;
    // optional injectivity clause
    std::optional<std::vector<TypeFamilyInstanceEqn>> where_instances;

    int arity() const {return args.size();}
    Kind result_kind() const;
    std::vector<Kind> arg_kinds() const;
    Kind kind() const;
    bool has_kind_notes() const;
    bool is_type_family() const {return info == TypeFamily;}
    bool is_data_family() const {return info == TypeFamily;}
    bool is_open_type_family() const {return is_type_family() and not where_instances;}
    bool is_closed_type_family() const {return is_type_family() and where_instances;}

    FamilyDecl* clone() const { return new FamilyDecl(*this);}

    std::string print() const;

    FamilyDecl(FamilyInfo i, const LTypeCon& tc, const std::vector<LTypeVar>& as, const std::optional<Kind>& ks,
                   const std::optional<std::vector<TypeFamilyInstanceEqn>>& es)
        :info(i),
	 con(tc),
         args(as),
         kind_sig(ks),
         where_instances(es)
    { }
};


struct FunDep: public Object
{
    std::vector<LTypeVar> lhs;
    std::vector<LTypeVar> rhs;

    FunDep* clone() const { return new FunDep(*this);}
    std::string print() const;

    FunDep() = default; // for the parser
    FunDep(const std::vector<LTypeVar>& ltvs, const std::vector<LTypeVar>& rtvs)
        :lhs(ltvs),
         rhs(rtvs)
        {}
};

struct ClassDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    Context context;
    LTypeCon con;
    std::vector<LTypeVar> type_vars;

    std::vector<FixityDecl> fixity_decls;

    std::vector<FunDep> fun_deps;
    std::vector<FamilyDecl> fam_decls;
    std::vector<TypeFamilyInstanceDecl> default_type_inst_decls;
    std::vector<TypeSigDecl> sig_decls;
    Decls default_method_decls;

    ClassDecl* clone() const { return new ClassDecl(*this);}
    std::string print() const;

    ClassDecl(const Context& ct,
              const LTypeCon& c,
              const std::vector<LTypeVar>& tvs,
              const std::vector<FixityDecl>& fxs,
              const std::vector<FunDep>& fds,
              const std::vector<FamilyDecl>& tfds,
              const std::vector<TypeFamilyInstanceDecl>& tids,
              const std::vector<TypeSigDecl>& sds,
              const Decls& dms)
        :context(ct),
         con(c),
         type_vars(tvs),
         fixity_decls(fxs),
         fun_deps(fds),
         fam_decls(tfds),
         default_type_inst_decls(tids),
         sig_decls(sds),
         default_method_decls(dms)
        {}
};

struct InstanceDecl: public Object
{
    // See PostProcess.hs: checkTyClHdr for checks and post-processing.

    std::optional<std::string> overlap_pragma;

    // This should be something like Eq a => Eq [a]
    // Right now it could be either ForallType or ConstrainedType
    LType polytype;

    std::vector<TypeFamilyInstanceDecl> type_inst_decls;
    Decls method_decls;

    InstanceDecl* clone() const { return new InstanceDecl(*this);}
    std::string print() const;

    InstanceDecl(const std::optional<std::string>& o,
                 const LType& t,
                 const std::vector<TypeFamilyInstanceDecl>& tids,
                 const Decls& ds)
        :overlap_pragma(o),
         polytype(t),
         type_inst_decls(tids),
         method_decls(ds)
        {}
};

struct DefaultDecl: public Object
{
    std::optional<Located<std::string>> maybe_class;
    std::vector<LType> types;
    DefaultDecl() {}
    DefaultDecl* clone() const { return new DefaultDecl(*this);}
    std::string print() const;
    DefaultDecl(const std::optional<Located<std::string>>& c, std::vector<LType>& ts):maybe_class(c), types(ts) {}
};


struct List: public Object
{
    std::vector<LExp> elements;
    List* clone() const { return new List(*this);}
    std::string print() const;
    List(const std::vector<LExp>& es): elements(es) {}
};

struct ListFrom: public Object
{
    LExp from;
    Hs::Var enumFromOp = {"Compiler.Enum.enumFrom"};
    ListFrom* clone() const { return new ListFrom(*this);}
    std::string print() const;
    ListFrom(const LExp& e): from(e) {}
};

struct ListFromThen: public Object
{
    LExp from;
    LExp then;
    Hs::Var enumFromThenOp = {"Compiler.Enum.enumFromThen"};
    ListFromThen* clone() const { return new ListFromThen(*this);}
    std::string print() const;
    ListFromThen(const LExp& e1, const LExp& e2): from(e1), then(e2) {}
};

struct ListFromTo: public Object
{
    LExp from;
    LExp to;
    Hs::Var enumFromToOp = {"Compiler.Enum.enumFromTo"};
    ListFromTo* clone() const { return new ListFromTo(*this);}
    std::string print() const;
    ListFromTo(const LExp& e1, const LExp& e2): from(e1), to(e2) {}
};

struct ListFromThenTo: public Object
{
    LExp from;
    LExp then;
    LExp to;
    Hs::Var enumFromThenToOp = {"Compiler.Enum.enumFromThenTo"};
    ListFromThenTo* clone() const { return new ListFromThenTo(*this);}
    std::string print() const;
    ListFromThenTo(const LExp& e1, const LExp& e2, const LExp& e3): from(e1), then(e2), to(e3) {}
};

struct ListComprehension: public Object
{
    LExp body;
    std::vector<LQual> quals;
    ListComprehension* clone() const { return new ListComprehension(*this);}
    std::string print() const;
    ListComprehension(const LExp& e, const std::vector<LQual>& es): body(e), quals(es) {}
};

struct LeftSection: public Object
{
    LExp l_arg;
    LExp op;
    LeftSection* clone() const {return new LeftSection(*this);}
    std::string print() const;
    LeftSection(const LExp& e, const LExp& v):l_arg(e),op(v) {}
};

struct RightSection: public Object
{
    LExp op;
    LExp r_arg;
    RightSection* clone() const {return new RightSection(*this);}
    std::string print() const;
    RightSection(const LExp& v, const LExp& e):op(v), r_arg(e) {}
};

struct Tuple: public Object
{
    std::vector<LExp> elements;
    Tuple* clone() const { return new Tuple(*this);}
    std::string print() const;
    Tuple(const std::vector<LExp>& es): elements(es)
        {
            assert(es.size() != 1);
        }
};

Exp tuple(const std::vector<LExp>& es);

struct GuardedRHS
{
    std::vector<LExp> guards;
    LExp body;

    std::string print() const;
    std::string print_no_equals() const;

    GuardedRHS() = default;
    GuardedRHS(const std::vector<LExp> v, const LExp& e)
        :guards(v),
         body(e)
    { }
};

struct MultiGuardedRHS
{
    std::vector<GuardedRHS> guarded_rhss;
    // We might need to make this into a shared_ptr< > to avoid an infinite structure.
    std::optional<LBinds> decls;

    std::string print() const;
    std::string print_no_equals() const;

    MultiGuardedRHS() = default;
    MultiGuardedRHS(const std::vector<GuardedRHS>& v):guarded_rhss(v) {}
    MultiGuardedRHS(const std::vector<GuardedRHS>& v, const std::optional<LBinds>& ds):guarded_rhss(v), decls(ds) {}
};

MultiGuardedRHS SimpleRHS(const Located<expression_ref>&, const std::optional<LBinds>& ds = {});

struct StrictValueDecl: public Object
{
    LExp lhs;
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
    LExp lhs;
    MultiGuardedRHS rhs;

    bool operator==(const Object& O) const;
    bool operator==(const ValueDecl& V) const;
    ValueDecl* clone() const { return new ValueDecl(*this);}
    std::string print() const;

    ValueDecl() = default;
    ValueDecl(const Located<expression_ref>& l, const expression_ref& r): lhs(l), rhs(SimpleRHS({noloc,r})) {}
    ValueDecl(const Located<expression_ref>& l, const MultiGuardedRHS& r): lhs(l), rhs(r) {}
};

typedef expression_ref Stmt;
typedef Located<Stmt> LStmt;

struct Stmts: public Object
{
    std::vector<LStmt> stmts;

    Stmts* clone() const { return new Stmts(*this);}
    std::string print() const;

    Stmts() = default;
    Stmts(const std::vector<LStmt>& v):stmts(v) {}
};

struct LetQual: public Object
{
    LBinds binds;

    LetQual* clone() const { return new LetQual(*this);}
    std::string print()  const;
    LetQual(const LBinds& b): binds(b) {}
};

struct SimpleQual: public Object
{
    LExp exp;
    Hs::Var andThenOp = {"Control.Monad.>>"};

    SimpleQual* clone() const { return new SimpleQual(*this);}
    std::string print() const;
    SimpleQual(const LExp &e): exp(e) {};
};

struct PatQual: public Object
{
    LPat bindpat;
    LExp exp;

    Hs::Var bindOp = {"Control.Monad.>>="};
    std::optional<Hs::Var> failOp = {{"Control.Monad.fail"}};

    PatQual* clone() const { return new PatQual(*this);}
    std::string print() const;
    PatQual(const LPat &bp, const LExp& e): bindpat(bp), exp(e) {};
};

struct RecStmt: public Object
{
    Stmts stmts;
    std::optional<Hs::Var> mfixOp;

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
    LPat pattern;
    MultiGuardedRHS rhs;

    std::string print() const;

    Alt() = default;
    Alt(const LPat& p, const MultiGuardedRHS& r):pattern(p), rhs(r) {};
};

struct Alts: public std::vector<Located<Alt>>
{
    std::string print() const;

    Alts() = default;
    Alts(const std::vector<Located<Alt>>& v): std::vector<Located<Alt>>(v) {};
};

struct LetExp: public Object
{
    LBinds binds;
    LExp body;

    LetExp* clone() const {return new LetExp(*this);}
    std::string print() const;

    LetExp(const LBinds& bs, const LExp& b): binds(bs), body(b) {};
};

LetExp simple_let(const LVar&, const LExp&, const Exp&);

struct IfExp: public Object
{
    LExp condition;
    LExp true_branch;
    LExp false_branch;

    IfExp* clone() const {return new IfExp(*this);}
    std::string print() const;

    IfExp(const LExp& c, const LExp& t, const LExp& f)
        : condition(c),
          true_branch(t),
          false_branch(f)
        { }
};

struct ExportSubSpec
{
    std::optional<std::vector<Located<std::string>>> names;
    std::string print() const;
};

// The 'type' seems to be different.
enum class ImpExpNs {module, type, pattern, default_};

std::string print(ImpExpNs);

struct Export
{
    std::optional<Located<ImpExpNs>> ns;
    Located<std::string> symbol;
    std::optional<ExportSubSpec> subspec;
    std::string print() const;
    bool is_module() const;
    bool is_value() const;
    bool is_type() const;
    bool is_default() const;
};

typedef Located<Export> LExport;

struct ImpSpec
{
    bool hiding;
    std::vector<LExport> imports;
    std::string print() const;
};

struct ImpDecl
{
    bool qualified;
    Located<std::string> modid;
    std::optional<Located<std::string>> as;
    std::optional<ImpSpec> impspec;

    std::string print() const;

    ImpDecl() = default;
    ImpDecl(bool b, const Located<std::string>& s, const std::optional<Located<std::string>>& a, const std::optional<ImpSpec>& is)
        :qualified(b),
         modid(s),
         as(a),
         impspec(is)
    {}
};

typedef Located<ImpDecl> LImpDecl;

struct Module
{
    Located<std::string> modid;
    std::optional<std::vector<LExport>> exports;
    std::vector<LImpDecl> impdecls;
    std::optional<Decls> topdecls;

    std::string print() const;

    Module() = default;
    Module(const Located<std::string>&s,
           const std::optional<std::vector<LExport>>& e,
           const std::vector<LImpDecl>& i,
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

    LPat lhs;
    MultiGuardedRHS rhs;

    // This holds vars discovered during renaming
    std::set<std::string> rhs_free_vars;

    PatDecl(const LPat& p, const MultiGuardedRHS& r):lhs(p), rhs(r) {}
};

struct MRule
{
    std::string print() const;

    std::vector<LPat> patterns;
    MultiGuardedRHS rhs;
    MRule(const std::vector<LPat>& ps, const MultiGuardedRHS& r);
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

    LambdaExp(const std::vector<LPat>& ps, const LExp& b);
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
    LExp object;
    Matches alts;

    CaseExp* clone() const { return new CaseExp(*this);}
    std::string print() const;

    CaseExp(const LExp& o, const Alts& as);
    CaseExp(const LExp& o, const Matches& ms);
};

struct FunDecl: public Object
{
    FunDecl* clone() const {return new FunDecl(*this);}
    std::string print() const;

    LVar v;
    Matches matches;

    // This holds vars discovered during renaming
    std::set<std::string> rhs_free_vars;

    FunDecl(const LVar& vv, const Matches& ms): v(vv), matches(ms) {}
};

FunDecl simple_decl(const LVar& v, const LExp&);
FunDecl simple_decl(const LVar& v, const MultiGuardedRHS&);
FunDecl simple_fun_decl(const LVar& v, const std::vector<LPat>& pats, const LExp&);
FunDecl simple_fun_decl(const LVar& v, const std::vector<LPat>& pats, const MultiGuardedRHS&);

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
    Core2::wrapper wrap;

    // Mark the generated core variable as exported.
    bool is_exported = false;

    BindInfo(const Var& o, const Var& i, const ::Type& m, const ::Type& p, const Core2::wrapper& w, bool e = false)
        : outer_id(o), inner_id(i), monotype(m), polytype(p), wrap(w), is_exported(e)
    { }
};

struct GenBind: public Object
{
    std::vector<::TypeVar> tv_args;

    // These variables MUST be type-annotated!
    std::vector<Core2::Var<>> dict_args;

    // Information for calling each of the binders in the group.
    std::vector<std::shared_ptr<const Core2::Decls<>>> dict_decls;

    // These are the actual declarations.
    Decls body;

    // Defaulting info for each binder
    std::map<Var, BindInfo> bind_infos;

    GenBind* clone() const {return new GenBind(*this);}
    std::string print() const;

    GenBind(const std::vector<::TypeVar>& v1,
            const std::vector<Core2::Var<>>& v2,
            const std::vector<std::shared_ptr<const Core2::Decls<>>>& ev,
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

    std::vector<Located<DefaultDecl>> default_decls;

    std::vector<FixityDecl> fixity_decls;

    Binds value_decls;

    std::vector<ForeignDecl> foreign_decls;

    ModuleDecls();
    ModuleDecls(const Decls& topdecls);
};

Expression error(const std::string& s);

Con True();
Con False();
Con ConsCon();
Con Nil();
Con TupleCon(int n);

std::tuple<std::vector<LTypeVar>, std::vector<LType>, LType> peel_top_gen(LType t);

}

#endif
