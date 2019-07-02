// A Bison parser, made by GNU Bison 3.4.1.

// Skeleton interface for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015, 2018-2019 Free Software Foundation, Inc.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// As a special exception, you may create a larger work that contains
// part or all of the Bison parser skeleton and distribute that work
// under terms of your choice, so long as that work isn't itself a
// parser generator using the skeleton or a modified version thereof
// as a parser skeleton.  Alternatively, if you modify or redistribute
// the parser skeleton itself, you may (at your option) remove this
// special exception, which will cause the skeleton and the resulting
// Bison output files to be licensed under the GNU General Public
// License without this special exception.

// This special exception was added by the Free Software Foundation in
// version 2.2 of Bison.


/**
 ** \file parser.hh
 ** Define the yy::parser class.
 */

// C++ LALR(1) parser skeleton written by Akim Demaille.

// Undocumented macros, especially those whose name start with YY_,
// are private implementation details.  Do not rely on them.

#ifndef YY_YY_PARSER_HH_INCLUDED
# define YY_YY_PARSER_HH_INCLUDED
// //                    "%code requires" blocks.
#line 11 "parser.y"

  # include <string>
  # include <iostream>
  # include <vector>
  # include "computation/expression/expression_ref.H"
  # include "computation/expression/var.H"
  # include "computation/expression/AST_node.H"
  # include "computation/operations.H"
  # include "computation/expression/list.H"
  # include "computation/expression/tuple.H"

  class driver;

  expression_ref make_module(const std::string& name, const expression_ref& exports, const expression_ref& body);
  expression_ref make_body(const std::vector<expression_ref>& imports, const std::vector<expression_ref>& topdecls);

  expression_ref make_exports(const std::vector<expression_ref>& exports);
  expression_ref make_infix(const std::string& infix, std::optional<int>& prec, std::vector<std::string>& ops);
  expression_ref make_builtin_expr(const std::string& name, int args, const std::string& s1, const std::string& s2);
  expression_ref make_builtin_expr(const std::string& name, int args, const std::string& s);

  expression_ref make_sig_vars(const std::vector<expression_ref>& sig_vars);
  expression_ref make_data_or_newtype(const std::string& d_or_n, const expression_ref& tycls_hdr, const std::vector<expression_ref>& constrs);
  expression_ref make_context(const expression_ref& context, const expression_ref& type);
  expression_ref make_tv_bndrs(const std::vector<expression_ref>& tv_bndrs);
  expression_ref make_tyapps(const std::vector<expression_ref>& tyapps);
  expression_ref make_id(const std::string& id);
  expression_ref make_type_id(const std::string& id);

  expression_ref make_rhs(const expression_ref& exp, const expression_ref& wherebinds);
  expression_ref make_gdrhs(const std::vector<expression_ref>& gdrhs, const expression_ref& wherebinds);
  expression_ref make_gdrh(const std::vector<expression_ref>& gdpats, const expression_ref& wherebinds);

  expression_ref make_typed_exp(const expression_ref& exp, const expression_ref& type);
  expression_ref make_infixexp(const std::vector<expression_ref>& args);
  expression_ref make_minus(const expression_ref& exp);
  expression_ref make_fexp(const std::vector<expression_ref>& args);

  expression_ref make_as_pattern(const std::string& var, const expression_ref& body);
  expression_ref make_lazy_pattern(const expression_ref& pat);
  expression_ref make_strict_pattern(const expression_ref& pat);

  expression_ref make_lambda(const std::vector<expression_ref>& pats, const expression_ref& body);
  expression_ref make_let(const expression_ref& binds, const expression_ref& body);
  expression_ref make_if(const expression_ref& cond, const expression_ref& alt_true, const expression_ref& alt_false);
  expression_ref make_case(const expression_ref& obj, const expression_ref& alts);
  expression_ref make_do(const std::vector<expression_ref>& stmts);
  expression_ref yy_make_tuple(const std::vector<expression_ref>& tup_exprs);

  expression_ref make_list(const std::vector<expression_ref>& items);
  expression_ref make_alts(const std::vector<expression_ref>& alts);
  expression_ref yy_make_alt(const expression_ref& pat, const expression_ref& alt_rhs);

  expression_ref make_stmts(const std::vector<expression_ref>& stmts);

  expression_ref yy_make_string(const std::string&);

#line 106 "parser.hh"

# include <cassert>
# include <cstdlib> // std::abort
# include <iostream>
# include <stdexcept>
# include <string>
# include <vector>

#if defined __cplusplus
# define YY_CPLUSPLUS __cplusplus
#else
# define YY_CPLUSPLUS 199711L
#endif

// Support move semantics when possible.
#if 201103L <= YY_CPLUSPLUS
# define YY_MOVE           std::move
# define YY_MOVE_OR_COPY   move
# define YY_MOVE_REF(Type) Type&&
# define YY_RVREF(Type)    Type&&
# define YY_COPY(Type)     Type
#else
# define YY_MOVE
# define YY_MOVE_OR_COPY   copy
# define YY_MOVE_REF(Type) Type&
# define YY_RVREF(Type)    const Type&
# define YY_COPY(Type)     const Type&
#endif

// Support noexcept when possible.
#if 201103L <= YY_CPLUSPLUS
# define YY_NOEXCEPT noexcept
# define YY_NOTHROW
#else
# define YY_NOEXCEPT
# define YY_NOTHROW throw ()
#endif

// Support constexpr when possible.
#if 201703 <= YY_CPLUSPLUS
# define YY_CONSTEXPR constexpr
#else
# define YY_CONSTEXPR
#endif
# include "location.hh"
#include <typeinfo>
#ifndef YYASSERT
# include <cassert>
# define YYASSERT assert
#endif


#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif

namespace yy {
#line 221 "parser.hh"




  /// A Bison parser.
  class parser
  {
  public:
#ifndef YYSTYPE
  /// A buffer to store and retrieve objects.
  ///
  /// Sort of a variant, but does not keep track of the nature
  /// of the stored data, since that knowledge is available
  /// via the current parser state.
  class semantic_type
  {
  public:
    /// Type of *this.
    typedef semantic_type self_type;

    /// Empty construction.
    semantic_type () YY_NOEXCEPT
      : yybuffer_ ()
      , yytypeid_ (YY_NULLPTR)
    {}

    /// Construct and fill.
    template <typename T>
    semantic_type (YY_RVREF (T) t)
      : yytypeid_ (&typeid (T))
    {
      YYASSERT (sizeof (T) <= size);
      new (yyas_<T> ()) T (YY_MOVE (t));
    }

    /// Destruction, allowed only if empty.
    ~semantic_type () YY_NOEXCEPT
    {
      YYASSERT (!yytypeid_);
    }

# if 201103L <= YY_CPLUSPLUS
    /// Instantiate a \a T in here from \a t.
    template <typename T, typename... U>
    T&
    emplace (U&&... u)
    {
      YYASSERT (!yytypeid_);
      YYASSERT (sizeof (T) <= size);
      yytypeid_ = & typeid (T);
      return *new (yyas_<T> ()) T (std::forward <U>(u)...);
    }
# else
    /// Instantiate an empty \a T in here.
    template <typename T>
    T&
    emplace ()
    {
      YYASSERT (!yytypeid_);
      YYASSERT (sizeof (T) <= size);
      yytypeid_ = & typeid (T);
      return *new (yyas_<T> ()) T ();
    }

    /// Instantiate a \a T in here from \a t.
    template <typename T>
    T&
    emplace (const T& t)
    {
      YYASSERT (!yytypeid_);
      YYASSERT (sizeof (T) <= size);
      yytypeid_ = & typeid (T);
      return *new (yyas_<T> ()) T (t);
    }
# endif

    /// Instantiate an empty \a T in here.
    /// Obsolete, use emplace.
    template <typename T>
    T&
    build ()
    {
      return emplace<T> ();
    }

    /// Instantiate a \a T in here from \a t.
    /// Obsolete, use emplace.
    template <typename T>
    T&
    build (const T& t)
    {
      return emplace<T> (t);
    }

    /// Accessor to a built \a T.
    template <typename T>
    T&
    as () YY_NOEXCEPT
    {
      YYASSERT (yytypeid_);
      YYASSERT (*yytypeid_ == typeid (T));
      YYASSERT (sizeof (T) <= size);
      return *yyas_<T> ();
    }

    /// Const accessor to a built \a T (for %printer).
    template <typename T>
    const T&
    as () const YY_NOEXCEPT
    {
      YYASSERT (yytypeid_);
      YYASSERT (*yytypeid_ == typeid (T));
      YYASSERT (sizeof (T) <= size);
      return *yyas_<T> ();
    }

    /// Swap the content with \a that, of same type.
    ///
    /// Both variants must be built beforehand, because swapping the actual
    /// data requires reading it (with as()), and this is not possible on
    /// unconstructed variants: it would require some dynamic testing, which
    /// should not be the variant's responsibility.
    /// Swapping between built and (possibly) non-built is done with
    /// self_type::move ().
    template <typename T>
    void
    swap (self_type& that) YY_NOEXCEPT
    {
      YYASSERT (yytypeid_);
      YYASSERT (*yytypeid_ == *that.yytypeid_);
      std::swap (as<T> (), that.as<T> ());
    }

    /// Move the content of \a that to this.
    ///
    /// Destroys \a that.
    template <typename T>
    void
    move (self_type& that)
    {
# if 201103L <= YY_CPLUSPLUS
      emplace<T> (std::move (that.as<T> ()));
# else
      emplace<T> ();
      swap<T> (that);
# endif
      that.destroy<T> ();
    }

# if 201103L <= YY_CPLUSPLUS
    /// Move the content of \a that to this.
    template <typename T>
    void
    move (self_type&& that)
    {
      emplace<T> (std::move (that.as<T> ()));
      that.destroy<T> ();
    }
#endif

    /// Copy the content of \a that to this.
    template <typename T>
    void
    copy (const self_type& that)
    {
      emplace<T> (that.as<T> ());
    }

    /// Destroy the stored \a T.
    template <typename T>
    void
    destroy ()
    {
      as<T> ().~T ();
      yytypeid_ = YY_NULLPTR;
    }

  private:
    /// Prohibit blind copies.
    self_type& operator= (const self_type&);
    semantic_type (const self_type&);

    /// Accessor to raw memory as \a T.
    template <typename T>
    T*
    yyas_ () YY_NOEXCEPT
    {
      void *yyp = yybuffer_.yyraw;
      return static_cast<T*> (yyp);
     }

    /// Const accessor to raw memory as \a T.
    template <typename T>
    const T*
    yyas_ () const YY_NOEXCEPT
    {
      const void *yyp = yybuffer_.yyraw;
      return static_cast<const T*> (yyp);
     }

    /// An auxiliary type to compute the largest semantic type.
    union union_type
    {
      // maybe_src
      // maybe_safe
      // optqualified
      char dummy1[sizeof (bool)];

      // "CHAR"
      // "PRIMCHAR"
      char dummy2[sizeof (char)];

      // "RATIONAL"
      // "PRIMDOUBLE"
      char dummy3[sizeof (double)];

      // module
      // body
      // body2
      // top
      // top1
      // maybeexports
      // export
      // qcname_ext_w_wildcard
      // qcname_ext
      // qcname
      // importdecl
      // maybeimpspec
      // impspec
      // topdecl
      // ty_decl
      // tycl_hdr
      // binds
      // wherebinds
      // opt_sig
      // opt_tyconsig
      // sigtype
      // sigtypedoc
      // ctype
      // ctypedoc
      // context
      // context_no_ops
      // type
      // typedoc
      // btype
      // tyapp
      // atype_docs
      // atype
      // tv_bndr
      // kind
      // constr
      // forall
      // constr_stuff
      // fielddecl
      // decl_no_th
      // decl
      // rhs
      // gdrh
      // sigdecl
      // exp
      // exp10_top
      // exp10
      // aexp
      // aexp1
      // aexp2
      // texp
      // list
      // transformqual
      // alt
      // alt_rhs
      // ifgdpats
      // gdpat
      // pat
      // bindpat
      // apat
      // stmt
      // qual
      // literal
      char dummy4[sizeof (expression_ref)];

      // "PRIMFLOAT"
      char dummy5[sizeof (float)];

      // "INTEGER"
      // "PRIMINTEGER"
      // "PRIMWORD"
      // commas
      char dummy6[sizeof (int)];

      // prec
      char dummy7[sizeof (std::optional<int>)];

      // maybe_pkg
      // maybeas
      char dummy8[sizeof (std::optional<std::string>)];

      // "VARID"
      // "CONID"
      // "VARSYM"
      // "CONSYM"
      // "QVARID"
      // "QCONID"
      // "QVARSYM"
      // "QCONSYM"
      // "IPDUPVARID"
      // "LABELVARID"
      // "STRING"
      // "PRIMSTRING"
      // infix
      // data_or_newtype
      // strict_mark
      // strictness
      // qcon
      // gen_qcon
      // con
      // sysdcon_no_list
      // sysdcon
      // conop
      // qconop
      // gtycon
      // ntgtycon
      // oqtycon
      // oqtycon_no_varcon
      // qtyconop
      // qtycondoc
      // qtycon
      // tycon
      // qtyconsym
      // tyconsym
      // op
      // varop
      // qop
      // qopm
      // hole_op
      // qvarop
      // qvaropm
      // tyvar
      // tyvarop
      // tyvarid
      // var
      // qvar
      // qvarid
      // varid
      // qvarsym
      // qvarsym_no_minus
      // qvarsym1
      // varsym
      // varsym_no_minus
      // special_id
      // special_sym
      // qconid
      // conid
      // qconsym
      // consym
      // modid
      char dummy9[sizeof (std::string)];

      // exportlist
      // exportlist1
      // qcnames
      // qcnames1
      // importdecls
      // importdecls_semi
      // topdecls
      // topdecls_semi
      // decls
      // decllist
      // sig_vars
      // sigtypes1
      // btype_no_ops
      // tyapps
      // comma_types0
      // comma_types1
      // tv_bndrs
      // constrs
      // constrs1
      // fielddecls
      // fielddecls1
      // gdrhs
      // infixexp
      // infixexp_top
      // fexp
      // tup_exprs
      // lexps
      // squals
      // guardquals
      // guardquals1
      // altslist
      // alts
      // alts1
      // gdpats
      // apats1
      // stmtlist
      // stmts
      char dummy10[sizeof (std::vector<expression_ref>)];

      // ops
      char dummy11[sizeof (std::vector<std::string>)];
    };

    /// The size of the largest semantic type.
    enum { size = sizeof (union_type) };

    /// A buffer to store semantic values.
    union
    {
      /// Strongest alignment constraints.
      long double yyalign_me;
      /// A buffer large enough to store any of the semantic values.
      char yyraw[size];
    } yybuffer_;

    /// Whether the content is built: if defined, the name of the stored type.
    const std::type_info *yytypeid_;
  };

#else
    typedef YYSTYPE semantic_type;
#endif
    /// Symbol locations.
    typedef location location_type;

    /// Syntax errors thrown from user actions.
    struct syntax_error : std::runtime_error
    {
      syntax_error (const location_type& l, const std::string& m)
        : std::runtime_error (m)
        , location (l)
      {}

      syntax_error (const syntax_error& s)
        : std::runtime_error (s.what ())
        , location (s.location)
      {}

      ~syntax_error () YY_NOEXCEPT YY_NOTHROW;

      location_type location;
    };

    /// Tokens.
    struct token
    {
      enum yytokentype
      {
        TOK_END = 0,
        TOK_UNDERSCORE = 258,
        TOK_AS = 259,
        TOK_CASE = 260,
        TOK_DATA = 261,
        TOK_DEFAULT = 262,
        TOK_DERIVING = 263,
        TOK_DO = 264,
        TOK_ELSE = 265,
        TOK_HIDING = 266,
        TOK_IF = 267,
        TOK_IMPORT = 268,
        TOK_IN = 269,
        TOK_INFIX = 270,
        TOK_INFIXL = 271,
        TOK_INFIXR = 272,
        TOK_INSTANCE = 273,
        TOK_LET = 274,
        TOK_MODULE = 275,
        TOK_NEWTYPE = 276,
        TOK_OF = 277,
        TOK_QUALIFIED = 278,
        TOK_THEN = 279,
        TOK_TYPE = 280,
        TOK_WHERE = 281,
        TOK_BUILTIN = 282,
        TOK_FORALL = 283,
        TOK_FOREIGN = 284,
        TOK_EXPORT = 285,
        TOK_LABEL = 286,
        TOK_DYNAMIC = 287,
        TOK_SAFE = 288,
        TOK_INTERRUPTIBLE = 289,
        TOK_UNSAFE = 290,
        TOK_MDO = 291,
        TOK_FAMILY = 292,
        TOK_ROLE = 293,
        TOK_STDCALL = 294,
        TOK_CCALL = 295,
        TOK_CAPI = 296,
        TOK_PRIM = 297,
        TOK_JAVASCRIPT = 298,
        TOK_PROC = 299,
        TOK_REC = 300,
        TOK_GROUP = 301,
        TOK_BY = 302,
        TOK_USING = 303,
        TOK_PATTERN = 304,
        TOK_STATIC = 305,
        TOK_STOCK = 306,
        TOK_ANYCLASS = 307,
        TOK_VIA = 308,
        TOK_UNIT = 309,
        TOK_SIGNATURE = 310,
        TOK_DEPENDENCY = 311,
        TOK_INLINE_PRAG = 312,
        TOK_SPECIALIZE_PRAG = 313,
        TOK_SPECIALIZE_INLINE_PRAG = 314,
        TOK_SOURCE_PRAG = 315,
        TOK_RULES_PRAG = 316,
        TOK_CORE_PRAG = 317,
        TOK_SCC_PRAG = 318,
        TOK_GENERATED_PRAG = 319,
        TOK_DEPRECATED_PRAG = 320,
        TOK_WARNING_PRAG = 321,
        TOK_UNPACK_PRAG = 322,
        TOK_NOUNPACK_PRAG = 323,
        TOK_ANN_PRAG = 324,
        TOK_MINIMAL_PRAG = 325,
        TOK_CTYPE_PRAG = 326,
        TOK_OVERLAPPING_PRAG = 327,
        TOK_OVERLAPPABLE_PRAG = 328,
        TOK_OVERLAPS_PRAG = 329,
        TOK_INCOHERENT_PRAG = 330,
        TOK_COMPLETE_PRAG = 331,
        TOK_CLOSE_PRAG = 332,
        TOK_DOTDOT = 333,
        TOK_COLON = 334,
        TOK_DCOLON = 335,
        TOK_EQUAL = 336,
        TOK_LAM = 337,
        TOK_LCASE = 338,
        TOK_VBAR = 339,
        TOK_LARROW = 340,
        TOK_RARROW = 341,
        TOK_AT = 342,
        TOK_TILDE = 343,
        TOK_DARROW = 344,
        TOK_MINUS = 345,
        TOK_BANG = 346,
        TOK_STAR = 347,
        TOK_lARROWTAIL = 348,
        TOK_rARROWTAIL = 349,
        TOK_LARROWTAIL = 350,
        TOK_RARROWTAIL = 351,
        TOK_DOT = 352,
        TOK_TYPEAPP = 353,
        TOK_OCURLY = 354,
        TOK_CCURLY = 355,
        TOK_VOCURLY = 356,
        TOK_VCCURLY = 357,
        TOK_OBRACK = 358,
        TOK_CBRACK = 359,
        TOK_OPABRACK = 360,
        TOK_CPABRACK = 361,
        TOK_OPAREN = 362,
        TOK_CPAREN = 363,
        TOK_OUBXPAREN = 364,
        TOK_CUBXPAREN = 365,
        TOK_OPARENBAR = 366,
        TOK_CPARENBAR = 367,
        TOK_SEMI = 368,
        TOK_COMMA = 369,
        TOK_BACKQUOTE = 370,
        TOK_SIMPLEQUOTE = 371,
        TOK_VARID = 372,
        TOK_CONID = 373,
        TOK_VARSYM = 374,
        TOK_CONSYM = 375,
        TOK_QVARID = 376,
        TOK_QCONID = 377,
        TOK_QVARSYM = 378,
        TOK_QCONSYM = 379,
        TOK_IPDUPVARID = 380,
        TOK_LABELVARID = 381,
        TOK_CHAR = 382,
        TOK_STRING = 383,
        TOK_INTEGER = 384,
        TOK_RATIONAL = 385,
        TOK_PRIMCHAR = 386,
        TOK_PRIMSTRING = 387,
        TOK_PRIMINTEGER = 388,
        TOK_PRINTWORD = 389,
        TOK_PRIMFLOAT = 390,
        TOK_PRIMDOUBLE = 391
      };
    };

    /// (External) token type, as returned by yylex.
    typedef token::yytokentype token_type;

    /// Symbol type: an internal symbol number.
    typedef int symbol_number_type;

    /// The symbol type number to denote an empty symbol.
    enum { empty_symbol = -2 };

    /// Internal symbol number for tokens (subsumed by symbol_number_type).
    typedef unsigned char token_number_type;

    /// A complete symbol.
    ///
    /// Expects its Base type to provide access to the symbol type
    /// via type_get ().
    ///
    /// Provide access to semantic value and location.
    template <typename Base>
    struct basic_symbol : Base
    {
      /// Alias to Base.
      typedef Base super_type;

      /// Default constructor.
      basic_symbol ()
        : value ()
        , location ()
      {}

#if 201103L <= YY_CPLUSPLUS
      /// Move constructor.
      basic_symbol (basic_symbol&& that);
#endif

      /// Copy constructor.
      basic_symbol (const basic_symbol& that);

      /// Constructor for valueless symbols, and symbols from each type.
#if 201103L <= YY_CPLUSPLUS
      basic_symbol (typename Base::kind_type t, location_type&& l)
        : Base (t)
        , location (std::move (l))
      {}
#else
      basic_symbol (typename Base::kind_type t, const location_type& l)
        : Base (t)
        , location (l)
      {}
#endif
#if 201103L <= YY_CPLUSPLUS
      basic_symbol (typename Base::kind_type t, bool&& v, location_type&& l)
        : Base (t)
        , value (std::move (v))
        , location (std::move (l))
      {}
#else
      basic_symbol (typename Base::kind_type t, const bool& v, const location_type& l)
        : Base (t)
        , value (v)
        , location (l)
      {}
#endif
#if 201103L <= YY_CPLUSPLUS
      basic_symbol (typename Base::kind_type t, char&& v, location_type&& l)
        : Base (t)
        , value (std::move (v))
        , location (std::move (l))
      {}
#else
      basic_symbol (typename Base::kind_type t, const char& v, const location_type& l)
        : Base (t)
        , value (v)
        , location (l)
      {}
#endif
#if 201103L <= YY_CPLUSPLUS
      basic_symbol (typename Base::kind_type t, double&& v, location_type&& l)
        : Base (t)
        , value (std::move (v))
        , location (std::move (l))
      {}
#else
      basic_symbol (typename Base::kind_type t, const double& v, const location_type& l)
        : Base (t)
        , value (v)
        , location (l)
      {}
#endif
#if 201103L <= YY_CPLUSPLUS
      basic_symbol (typename Base::kind_type t, expression_ref&& v, location_type&& l)
        : Base (t)
        , value (std::move (v))
        , location (std::move (l))
      {}
#else
      basic_symbol (typename Base::kind_type t, const expression_ref& v, const location_type& l)
        : Base (t)
        , value (v)
        , location (l)
      {}
#endif
#if 201103L <= YY_CPLUSPLUS
      basic_symbol (typename Base::kind_type t, float&& v, location_type&& l)
        : Base (t)
        , value (std::move (v))
        , location (std::move (l))
      {}
#else
      basic_symbol (typename Base::kind_type t, const float& v, const location_type& l)
        : Base (t)
        , value (v)
        , location (l)
      {}
#endif
#if 201103L <= YY_CPLUSPLUS
      basic_symbol (typename Base::kind_type t, int&& v, location_type&& l)
        : Base (t)
        , value (std::move (v))
        , location (std::move (l))
      {}
#else
      basic_symbol (typename Base::kind_type t, const int& v, const location_type& l)
        : Base (t)
        , value (v)
        , location (l)
      {}
#endif
#if 201103L <= YY_CPLUSPLUS
      basic_symbol (typename Base::kind_type t, std::optional<int>&& v, location_type&& l)
        : Base (t)
        , value (std::move (v))
        , location (std::move (l))
      {}
#else
      basic_symbol (typename Base::kind_type t, const std::optional<int>& v, const location_type& l)
        : Base (t)
        , value (v)
        , location (l)
      {}
#endif
#if 201103L <= YY_CPLUSPLUS
      basic_symbol (typename Base::kind_type t, std::optional<std::string>&& v, location_type&& l)
        : Base (t)
        , value (std::move (v))
        , location (std::move (l))
      {}
#else
      basic_symbol (typename Base::kind_type t, const std::optional<std::string>& v, const location_type& l)
        : Base (t)
        , value (v)
        , location (l)
      {}
#endif
#if 201103L <= YY_CPLUSPLUS
      basic_symbol (typename Base::kind_type t, std::string&& v, location_type&& l)
        : Base (t)
        , value (std::move (v))
        , location (std::move (l))
      {}
#else
      basic_symbol (typename Base::kind_type t, const std::string& v, const location_type& l)
        : Base (t)
        , value (v)
        , location (l)
      {}
#endif
#if 201103L <= YY_CPLUSPLUS
      basic_symbol (typename Base::kind_type t, std::vector<expression_ref>&& v, location_type&& l)
        : Base (t)
        , value (std::move (v))
        , location (std::move (l))
      {}
#else
      basic_symbol (typename Base::kind_type t, const std::vector<expression_ref>& v, const location_type& l)
        : Base (t)
        , value (v)
        , location (l)
      {}
#endif
#if 201103L <= YY_CPLUSPLUS
      basic_symbol (typename Base::kind_type t, std::vector<std::string>&& v, location_type&& l)
        : Base (t)
        , value (std::move (v))
        , location (std::move (l))
      {}
#else
      basic_symbol (typename Base::kind_type t, const std::vector<std::string>& v, const location_type& l)
        : Base (t)
        , value (v)
        , location (l)
      {}
#endif

      /// Destroy the symbol.
      ~basic_symbol ()
      {
        clear ();
      }

      /// Destroy contents, and record that is empty.
      void clear ()
      {
        // User destructor.
        symbol_number_type yytype = this->type_get ();
        basic_symbol<Base>& yysym = *this;
        (void) yysym;
        switch (yytype)
        {
       default:
          break;
        }

        // Type destructor.
switch (yytype)
    {
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        value.template destroy< bool > ();
        break;

      case 127: // "CHAR"
      case 131: // "PRIMCHAR"
        value.template destroy< char > ();
        break;

      case 130: // "RATIONAL"
      case 136: // "PRIMDOUBLE"
        value.template destroy< double > ();
        break;

      case 143: // module
      case 146: // body
      case 147: // body2
      case 148: // top
      case 149: // top1
      case 150: // maybeexports
      case 153: // export
      case 157: // qcname_ext_w_wildcard
      case 158: // qcname_ext
      case 159: // qcname
      case 164: // importdecl
      case 170: // maybeimpspec
      case 171: // impspec
      case 177: // topdecl
      case 179: // ty_decl
      case 186: // tycl_hdr
      case 204: // binds
      case 205: // wherebinds
      case 208: // opt_sig
      case 209: // opt_tyconsig
      case 210: // sigtype
      case 211: // sigtypedoc
      case 217: // ctype
      case 218: // ctypedoc
      case 219: // context
      case 220: // context_no_ops
      case 221: // type
      case 222: // typedoc
      case 223: // btype
      case 226: // tyapp
      case 227: // atype_docs
      case 228: // atype
      case 235: // tv_bndr
      case 240: // kind
      case 243: // constr
      case 244: // forall
      case 245: // constr_stuff
      case 248: // fielddecl
      case 253: // decl_no_th
      case 254: // decl
      case 255: // rhs
      case 257: // gdrh
      case 258: // sigdecl
      case 261: // exp
      case 264: // exp10_top
      case 265: // exp10
      case 269: // aexp
      case 270: // aexp1
      case 271: // aexp2
      case 272: // texp
      case 274: // list
      case 277: // transformqual
      case 283: // alt
      case 284: // alt_rhs
      case 286: // ifgdpats
      case 287: // gdpat
      case 288: // pat
      case 289: // bindpat
      case 290: // apat
      case 294: // stmt
      case 295: // qual
      case 342: // literal
        value.template destroy< expression_ref > ();
        break;

      case 135: // "PRIMFLOAT"
        value.template destroy< float > ();
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 345: // commas
        value.template destroy< int > ();
        break;

      case 172: // prec
        value.template destroy< std::optional<int> > ();
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.template destroy< std::optional<std::string> > ();
        break;

      case 117: // "VARID"
      case 118: // "CONID"
      case 119: // "VARSYM"
      case 120: // "CONSYM"
      case 121: // "QVARID"
      case 122: // "QCONID"
      case 123: // "QVARSYM"
      case 124: // "QCONSYM"
      case 125: // "IPDUPVARID"
      case 126: // "LABELVARID"
      case 128: // "STRING"
      case 132: // "PRIMSTRING"
      case 173: // infix
      case 184: // data_or_newtype
      case 214: // strict_mark
      case 215: // strictness
      case 299: // qcon
      case 300: // gen_qcon
      case 301: // con
      case 303: // sysdcon_no_list
      case 304: // sysdcon
      case 305: // conop
      case 306: // qconop
      case 307: // gtycon
      case 308: // ntgtycon
      case 309: // oqtycon
      case 310: // oqtycon_no_varcon
      case 311: // qtyconop
      case 312: // qtycondoc
      case 313: // qtycon
      case 314: // tycon
      case 315: // qtyconsym
      case 316: // tyconsym
      case 317: // op
      case 318: // varop
      case 319: // qop
      case 320: // qopm
      case 321: // hole_op
      case 322: // qvarop
      case 323: // qvaropm
      case 324: // tyvar
      case 325: // tyvarop
      case 326: // tyvarid
      case 327: // var
      case 328: // qvar
      case 329: // qvarid
      case 330: // varid
      case 331: // qvarsym
      case 332: // qvarsym_no_minus
      case 333: // qvarsym1
      case 334: // varsym
      case 335: // varsym_no_minus
      case 336: // special_id
      case 337: // special_sym
      case 338: // qconid
      case 339: // conid
      case 340: // qconsym
      case 341: // consym
      case 344: // modid
        value.template destroy< std::string > ();
        break;

      case 151: // exportlist
      case 152: // exportlist1
      case 155: // qcnames
      case 156: // qcnames1
      case 162: // importdecls
      case 163: // importdecls_semi
      case 175: // topdecls
      case 176: // topdecls_semi
      case 202: // decls
      case 203: // decllist
      case 212: // sig_vars
      case 213: // sigtypes1
      case 224: // btype_no_ops
      case 225: // tyapps
      case 231: // comma_types0
      case 232: // comma_types1
      case 234: // tv_bndrs
      case 241: // constrs
      case 242: // constrs1
      case 246: // fielddecls
      case 247: // fielddecls1
      case 256: // gdrhs
      case 262: // infixexp
      case 263: // infixexp_top
      case 268: // fexp
      case 273: // tup_exprs
      case 275: // lexps
      case 276: // squals
      case 278: // guardquals
      case 279: // guardquals1
      case 280: // altslist
      case 281: // alts
      case 282: // alts1
      case 285: // gdpats
      case 291: // apats1
      case 292: // stmtlist
      case 293: // stmts
        value.template destroy< std::vector<expression_ref> > ();
        break;

      case 174: // ops
        value.template destroy< std::vector<std::string> > ();
        break;

      default:
        break;
    }

        Base::clear ();
      }

      /// Whether empty.
      bool empty () const YY_NOEXCEPT;

      /// Destructive move, \a s is emptied into this.
      void move (basic_symbol& s);

      /// The semantic value.
      semantic_type value;

      /// The location.
      location_type location;

    private:
#if YY_CPLUSPLUS < 201103L
      /// Assignment operator.
      basic_symbol& operator= (const basic_symbol& that);
#endif
    };

    /// Type access provider for token (enum) based symbols.
    struct by_type
    {
      /// Default constructor.
      by_type ();

#if 201103L <= YY_CPLUSPLUS
      /// Move constructor.
      by_type (by_type&& that);
#endif

      /// Copy constructor.
      by_type (const by_type& that);

      /// The symbol type as needed by the constructor.
      typedef token_type kind_type;

      /// Constructor from (external) token numbers.
      by_type (kind_type t);

      /// Record that this symbol is empty.
      void clear ();

      /// Steal the symbol type from \a that.
      void move (by_type& that);

      /// The (internal) type number (corresponding to \a type).
      /// \a empty when empty.
      symbol_number_type type_get () const YY_NOEXCEPT;

      /// The token.
      token_type token () const YY_NOEXCEPT;

      /// The symbol type.
      /// \a empty_symbol when empty.
      /// An int, not token_number_type, to be able to store empty_symbol.
      int type;
    };

    /// "External" symbols: returned by the scanner.
    struct symbol_type : basic_symbol<by_type>
    {
      /// Superclass.
      typedef basic_symbol<by_type> super_type;

      /// Empty symbol.
      symbol_type () {}

      /// Constructor for valueless symbols, and symbols from each type.
#if 201103L <= YY_CPLUSPLUS
      symbol_type (int tok, location_type l)
        : super_type(token_type (tok), std::move (l))
      {
        YYASSERT (tok == token::TOK_END || tok == token::TOK_UNDERSCORE || tok == token::TOK_AS || tok == token::TOK_CASE || tok == token::TOK_DATA || tok == token::TOK_DEFAULT || tok == token::TOK_DERIVING || tok == token::TOK_DO || tok == token::TOK_ELSE || tok == token::TOK_HIDING || tok == token::TOK_IF || tok == token::TOK_IMPORT || tok == token::TOK_IN || tok == token::TOK_INFIX || tok == token::TOK_INFIXL || tok == token::TOK_INFIXR || tok == token::TOK_INSTANCE || tok == token::TOK_LET || tok == token::TOK_MODULE || tok == token::TOK_NEWTYPE || tok == token::TOK_OF || tok == token::TOK_QUALIFIED || tok == token::TOK_THEN || tok == token::TOK_TYPE || tok == token::TOK_WHERE || tok == token::TOK_BUILTIN || tok == token::TOK_FORALL || tok == token::TOK_FOREIGN || tok == token::TOK_EXPORT || tok == token::TOK_LABEL || tok == token::TOK_DYNAMIC || tok == token::TOK_SAFE || tok == token::TOK_INTERRUPTIBLE || tok == token::TOK_UNSAFE || tok == token::TOK_MDO || tok == token::TOK_FAMILY || tok == token::TOK_ROLE || tok == token::TOK_STDCALL || tok == token::TOK_CCALL || tok == token::TOK_CAPI || tok == token::TOK_PRIM || tok == token::TOK_JAVASCRIPT || tok == token::TOK_PROC || tok == token::TOK_REC || tok == token::TOK_GROUP || tok == token::TOK_BY || tok == token::TOK_USING || tok == token::TOK_PATTERN || tok == token::TOK_STATIC || tok == token::TOK_STOCK || tok == token::TOK_ANYCLASS || tok == token::TOK_VIA || tok == token::TOK_UNIT || tok == token::TOK_SIGNATURE || tok == token::TOK_DEPENDENCY || tok == token::TOK_INLINE_PRAG || tok == token::TOK_SPECIALIZE_PRAG || tok == token::TOK_SPECIALIZE_INLINE_PRAG || tok == token::TOK_SOURCE_PRAG || tok == token::TOK_RULES_PRAG || tok == token::TOK_CORE_PRAG || tok == token::TOK_SCC_PRAG || tok == token::TOK_GENERATED_PRAG || tok == token::TOK_DEPRECATED_PRAG || tok == token::TOK_WARNING_PRAG || tok == token::TOK_UNPACK_PRAG || tok == token::TOK_NOUNPACK_PRAG || tok == token::TOK_ANN_PRAG || tok == token::TOK_MINIMAL_PRAG || tok == token::TOK_CTYPE_PRAG || tok == token::TOK_OVERLAPPING_PRAG || tok == token::TOK_OVERLAPPABLE_PRAG || tok == token::TOK_OVERLAPS_PRAG || tok == token::TOK_INCOHERENT_PRAG || tok == token::TOK_COMPLETE_PRAG || tok == token::TOK_CLOSE_PRAG || tok == token::TOK_DOTDOT || tok == token::TOK_COLON || tok == token::TOK_DCOLON || tok == token::TOK_EQUAL || tok == token::TOK_LAM || tok == token::TOK_LCASE || tok == token::TOK_VBAR || tok == token::TOK_LARROW || tok == token::TOK_RARROW || tok == token::TOK_AT || tok == token::TOK_TILDE || tok == token::TOK_DARROW || tok == token::TOK_MINUS || tok == token::TOK_BANG || tok == token::TOK_STAR || tok == token::TOK_lARROWTAIL || tok == token::TOK_rARROWTAIL || tok == token::TOK_LARROWTAIL || tok == token::TOK_RARROWTAIL || tok == token::TOK_DOT || tok == token::TOK_TYPEAPP || tok == token::TOK_OCURLY || tok == token::TOK_CCURLY || tok == token::TOK_VOCURLY || tok == token::TOK_VCCURLY || tok == token::TOK_OBRACK || tok == token::TOK_CBRACK || tok == token::TOK_OPABRACK || tok == token::TOK_CPABRACK || tok == token::TOK_OPAREN || tok == token::TOK_CPAREN || tok == token::TOK_OUBXPAREN || tok == token::TOK_CUBXPAREN || tok == token::TOK_OPARENBAR || tok == token::TOK_CPARENBAR || tok == token::TOK_SEMI || tok == token::TOK_COMMA || tok == token::TOK_BACKQUOTE || tok == token::TOK_SIMPLEQUOTE || tok == 392 || tok == 393 || tok == 394 || tok == 395);
      }
#else
      symbol_type (int tok, const location_type& l)
        : super_type(token_type (tok), l)
      {
        YYASSERT (tok == token::TOK_END || tok == token::TOK_UNDERSCORE || tok == token::TOK_AS || tok == token::TOK_CASE || tok == token::TOK_DATA || tok == token::TOK_DEFAULT || tok == token::TOK_DERIVING || tok == token::TOK_DO || tok == token::TOK_ELSE || tok == token::TOK_HIDING || tok == token::TOK_IF || tok == token::TOK_IMPORT || tok == token::TOK_IN || tok == token::TOK_INFIX || tok == token::TOK_INFIXL || tok == token::TOK_INFIXR || tok == token::TOK_INSTANCE || tok == token::TOK_LET || tok == token::TOK_MODULE || tok == token::TOK_NEWTYPE || tok == token::TOK_OF || tok == token::TOK_QUALIFIED || tok == token::TOK_THEN || tok == token::TOK_TYPE || tok == token::TOK_WHERE || tok == token::TOK_BUILTIN || tok == token::TOK_FORALL || tok == token::TOK_FOREIGN || tok == token::TOK_EXPORT || tok == token::TOK_LABEL || tok == token::TOK_DYNAMIC || tok == token::TOK_SAFE || tok == token::TOK_INTERRUPTIBLE || tok == token::TOK_UNSAFE || tok == token::TOK_MDO || tok == token::TOK_FAMILY || tok == token::TOK_ROLE || tok == token::TOK_STDCALL || tok == token::TOK_CCALL || tok == token::TOK_CAPI || tok == token::TOK_PRIM || tok == token::TOK_JAVASCRIPT || tok == token::TOK_PROC || tok == token::TOK_REC || tok == token::TOK_GROUP || tok == token::TOK_BY || tok == token::TOK_USING || tok == token::TOK_PATTERN || tok == token::TOK_STATIC || tok == token::TOK_STOCK || tok == token::TOK_ANYCLASS || tok == token::TOK_VIA || tok == token::TOK_UNIT || tok == token::TOK_SIGNATURE || tok == token::TOK_DEPENDENCY || tok == token::TOK_INLINE_PRAG || tok == token::TOK_SPECIALIZE_PRAG || tok == token::TOK_SPECIALIZE_INLINE_PRAG || tok == token::TOK_SOURCE_PRAG || tok == token::TOK_RULES_PRAG || tok == token::TOK_CORE_PRAG || tok == token::TOK_SCC_PRAG || tok == token::TOK_GENERATED_PRAG || tok == token::TOK_DEPRECATED_PRAG || tok == token::TOK_WARNING_PRAG || tok == token::TOK_UNPACK_PRAG || tok == token::TOK_NOUNPACK_PRAG || tok == token::TOK_ANN_PRAG || tok == token::TOK_MINIMAL_PRAG || tok == token::TOK_CTYPE_PRAG || tok == token::TOK_OVERLAPPING_PRAG || tok == token::TOK_OVERLAPPABLE_PRAG || tok == token::TOK_OVERLAPS_PRAG || tok == token::TOK_INCOHERENT_PRAG || tok == token::TOK_COMPLETE_PRAG || tok == token::TOK_CLOSE_PRAG || tok == token::TOK_DOTDOT || tok == token::TOK_COLON || tok == token::TOK_DCOLON || tok == token::TOK_EQUAL || tok == token::TOK_LAM || tok == token::TOK_LCASE || tok == token::TOK_VBAR || tok == token::TOK_LARROW || tok == token::TOK_RARROW || tok == token::TOK_AT || tok == token::TOK_TILDE || tok == token::TOK_DARROW || tok == token::TOK_MINUS || tok == token::TOK_BANG || tok == token::TOK_STAR || tok == token::TOK_lARROWTAIL || tok == token::TOK_rARROWTAIL || tok == token::TOK_LARROWTAIL || tok == token::TOK_RARROWTAIL || tok == token::TOK_DOT || tok == token::TOK_TYPEAPP || tok == token::TOK_OCURLY || tok == token::TOK_CCURLY || tok == token::TOK_VOCURLY || tok == token::TOK_VCCURLY || tok == token::TOK_OBRACK || tok == token::TOK_CBRACK || tok == token::TOK_OPABRACK || tok == token::TOK_CPABRACK || tok == token::TOK_OPAREN || tok == token::TOK_CPAREN || tok == token::TOK_OUBXPAREN || tok == token::TOK_CUBXPAREN || tok == token::TOK_OPARENBAR || tok == token::TOK_CPARENBAR || tok == token::TOK_SEMI || tok == token::TOK_COMMA || tok == token::TOK_BACKQUOTE || tok == token::TOK_SIMPLEQUOTE || tok == 392 || tok == 393 || tok == 394 || tok == 395);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      symbol_type (int tok, char v, location_type l)
        : super_type(token_type (tok), std::move (v), std::move (l))
      {
        YYASSERT (tok == token::TOK_CHAR || tok == token::TOK_PRIMCHAR);
      }
#else
      symbol_type (int tok, const char& v, const location_type& l)
        : super_type(token_type (tok), v, l)
      {
        YYASSERT (tok == token::TOK_CHAR || tok == token::TOK_PRIMCHAR);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      symbol_type (int tok, double v, location_type l)
        : super_type(token_type (tok), std::move (v), std::move (l))
      {
        YYASSERT (tok == token::TOK_RATIONAL || tok == token::TOK_PRIMDOUBLE);
      }
#else
      symbol_type (int tok, const double& v, const location_type& l)
        : super_type(token_type (tok), v, l)
      {
        YYASSERT (tok == token::TOK_RATIONAL || tok == token::TOK_PRIMDOUBLE);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      symbol_type (int tok, float v, location_type l)
        : super_type(token_type (tok), std::move (v), std::move (l))
      {
        YYASSERT (tok == token::TOK_PRIMFLOAT);
      }
#else
      symbol_type (int tok, const float& v, const location_type& l)
        : super_type(token_type (tok), v, l)
      {
        YYASSERT (tok == token::TOK_PRIMFLOAT);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      symbol_type (int tok, int v, location_type l)
        : super_type(token_type (tok), std::move (v), std::move (l))
      {
        YYASSERT (tok == token::TOK_INTEGER || tok == token::TOK_PRIMINTEGER || tok == token::TOK_PRINTWORD);
      }
#else
      symbol_type (int tok, const int& v, const location_type& l)
        : super_type(token_type (tok), v, l)
      {
        YYASSERT (tok == token::TOK_INTEGER || tok == token::TOK_PRIMINTEGER || tok == token::TOK_PRINTWORD);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      symbol_type (int tok, std::string v, location_type l)
        : super_type(token_type (tok), std::move (v), std::move (l))
      {
        YYASSERT (tok == token::TOK_VARID || tok == token::TOK_CONID || tok == token::TOK_VARSYM || tok == token::TOK_CONSYM || tok == token::TOK_QVARID || tok == token::TOK_QCONID || tok == token::TOK_QVARSYM || tok == token::TOK_QCONSYM || tok == token::TOK_IPDUPVARID || tok == token::TOK_LABELVARID || tok == token::TOK_STRING || tok == token::TOK_PRIMSTRING);
      }
#else
      symbol_type (int tok, const std::string& v, const location_type& l)
        : super_type(token_type (tok), v, l)
      {
        YYASSERT (tok == token::TOK_VARID || tok == token::TOK_CONID || tok == token::TOK_VARSYM || tok == token::TOK_CONSYM || tok == token::TOK_QVARID || tok == token::TOK_QCONID || tok == token::TOK_QVARSYM || tok == token::TOK_QCONSYM || tok == token::TOK_IPDUPVARID || tok == token::TOK_LABELVARID || tok == token::TOK_STRING || tok == token::TOK_PRIMSTRING);
      }
#endif
    };

    /// Build a parser object.
    parser (driver& drv_yyarg);
    virtual ~parser ();

    /// Parse.  An alias for parse ().
    /// \returns  0 iff parsing succeeded.
    int operator() ();

    /// Parse.
    /// \returns  0 iff parsing succeeded.
    virtual int parse ();

#if YYDEBUG
    /// The current debugging stream.
    std::ostream& debug_stream () const YY_ATTRIBUTE_PURE;
    /// Set the current debugging stream.
    void set_debug_stream (std::ostream &);

    /// Type for debugging levels.
    typedef int debug_level_type;
    /// The current debugging level.
    debug_level_type debug_level () const YY_ATTRIBUTE_PURE;
    /// Set the current debugging level.
    void set_debug_level (debug_level_type l);
#endif

    /// Report a syntax error.
    /// \param loc    where the syntax error is found.
    /// \param msg    a description of the syntax error.
    virtual void error (const location_type& loc, const std::string& msg);

    /// Report a syntax error.
    void error (const syntax_error& err);

    // Implementation of make_symbol for each symbol type.
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_END (location_type l)
      {
        return symbol_type (token::TOK_END, std::move (l));
      }
#else
      static
      symbol_type
      make_END (const location_type& l)
      {
        return symbol_type (token::TOK_END, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_UNDERSCORE (location_type l)
      {
        return symbol_type (token::TOK_UNDERSCORE, std::move (l));
      }
#else
      static
      symbol_type
      make_UNDERSCORE (const location_type& l)
      {
        return symbol_type (token::TOK_UNDERSCORE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_AS (location_type l)
      {
        return symbol_type (token::TOK_AS, std::move (l));
      }
#else
      static
      symbol_type
      make_AS (const location_type& l)
      {
        return symbol_type (token::TOK_AS, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CASE (location_type l)
      {
        return symbol_type (token::TOK_CASE, std::move (l));
      }
#else
      static
      symbol_type
      make_CASE (const location_type& l)
      {
        return symbol_type (token::TOK_CASE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_DATA (location_type l)
      {
        return symbol_type (token::TOK_DATA, std::move (l));
      }
#else
      static
      symbol_type
      make_DATA (const location_type& l)
      {
        return symbol_type (token::TOK_DATA, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_DEFAULT (location_type l)
      {
        return symbol_type (token::TOK_DEFAULT, std::move (l));
      }
#else
      static
      symbol_type
      make_DEFAULT (const location_type& l)
      {
        return symbol_type (token::TOK_DEFAULT, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_DERIVING (location_type l)
      {
        return symbol_type (token::TOK_DERIVING, std::move (l));
      }
#else
      static
      symbol_type
      make_DERIVING (const location_type& l)
      {
        return symbol_type (token::TOK_DERIVING, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_DO (location_type l)
      {
        return symbol_type (token::TOK_DO, std::move (l));
      }
#else
      static
      symbol_type
      make_DO (const location_type& l)
      {
        return symbol_type (token::TOK_DO, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_ELSE (location_type l)
      {
        return symbol_type (token::TOK_ELSE, std::move (l));
      }
#else
      static
      symbol_type
      make_ELSE (const location_type& l)
      {
        return symbol_type (token::TOK_ELSE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_HIDING (location_type l)
      {
        return symbol_type (token::TOK_HIDING, std::move (l));
      }
#else
      static
      symbol_type
      make_HIDING (const location_type& l)
      {
        return symbol_type (token::TOK_HIDING, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_IF (location_type l)
      {
        return symbol_type (token::TOK_IF, std::move (l));
      }
#else
      static
      symbol_type
      make_IF (const location_type& l)
      {
        return symbol_type (token::TOK_IF, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_IMPORT (location_type l)
      {
        return symbol_type (token::TOK_IMPORT, std::move (l));
      }
#else
      static
      symbol_type
      make_IMPORT (const location_type& l)
      {
        return symbol_type (token::TOK_IMPORT, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_IN (location_type l)
      {
        return symbol_type (token::TOK_IN, std::move (l));
      }
#else
      static
      symbol_type
      make_IN (const location_type& l)
      {
        return symbol_type (token::TOK_IN, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_INFIX (location_type l)
      {
        return symbol_type (token::TOK_INFIX, std::move (l));
      }
#else
      static
      symbol_type
      make_INFIX (const location_type& l)
      {
        return symbol_type (token::TOK_INFIX, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_INFIXL (location_type l)
      {
        return symbol_type (token::TOK_INFIXL, std::move (l));
      }
#else
      static
      symbol_type
      make_INFIXL (const location_type& l)
      {
        return symbol_type (token::TOK_INFIXL, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_INFIXR (location_type l)
      {
        return symbol_type (token::TOK_INFIXR, std::move (l));
      }
#else
      static
      symbol_type
      make_INFIXR (const location_type& l)
      {
        return symbol_type (token::TOK_INFIXR, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_INSTANCE (location_type l)
      {
        return symbol_type (token::TOK_INSTANCE, std::move (l));
      }
#else
      static
      symbol_type
      make_INSTANCE (const location_type& l)
      {
        return symbol_type (token::TOK_INSTANCE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_LET (location_type l)
      {
        return symbol_type (token::TOK_LET, std::move (l));
      }
#else
      static
      symbol_type
      make_LET (const location_type& l)
      {
        return symbol_type (token::TOK_LET, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_MODULE (location_type l)
      {
        return symbol_type (token::TOK_MODULE, std::move (l));
      }
#else
      static
      symbol_type
      make_MODULE (const location_type& l)
      {
        return symbol_type (token::TOK_MODULE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_NEWTYPE (location_type l)
      {
        return symbol_type (token::TOK_NEWTYPE, std::move (l));
      }
#else
      static
      symbol_type
      make_NEWTYPE (const location_type& l)
      {
        return symbol_type (token::TOK_NEWTYPE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_OF (location_type l)
      {
        return symbol_type (token::TOK_OF, std::move (l));
      }
#else
      static
      symbol_type
      make_OF (const location_type& l)
      {
        return symbol_type (token::TOK_OF, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_QUALIFIED (location_type l)
      {
        return symbol_type (token::TOK_QUALIFIED, std::move (l));
      }
#else
      static
      symbol_type
      make_QUALIFIED (const location_type& l)
      {
        return symbol_type (token::TOK_QUALIFIED, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_THEN (location_type l)
      {
        return symbol_type (token::TOK_THEN, std::move (l));
      }
#else
      static
      symbol_type
      make_THEN (const location_type& l)
      {
        return symbol_type (token::TOK_THEN, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_TYPE (location_type l)
      {
        return symbol_type (token::TOK_TYPE, std::move (l));
      }
#else
      static
      symbol_type
      make_TYPE (const location_type& l)
      {
        return symbol_type (token::TOK_TYPE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_WHERE (location_type l)
      {
        return symbol_type (token::TOK_WHERE, std::move (l));
      }
#else
      static
      symbol_type
      make_WHERE (const location_type& l)
      {
        return symbol_type (token::TOK_WHERE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_BUILTIN (location_type l)
      {
        return symbol_type (token::TOK_BUILTIN, std::move (l));
      }
#else
      static
      symbol_type
      make_BUILTIN (const location_type& l)
      {
        return symbol_type (token::TOK_BUILTIN, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_FORALL (location_type l)
      {
        return symbol_type (token::TOK_FORALL, std::move (l));
      }
#else
      static
      symbol_type
      make_FORALL (const location_type& l)
      {
        return symbol_type (token::TOK_FORALL, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_FOREIGN (location_type l)
      {
        return symbol_type (token::TOK_FOREIGN, std::move (l));
      }
#else
      static
      symbol_type
      make_FOREIGN (const location_type& l)
      {
        return symbol_type (token::TOK_FOREIGN, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_EXPORT (location_type l)
      {
        return symbol_type (token::TOK_EXPORT, std::move (l));
      }
#else
      static
      symbol_type
      make_EXPORT (const location_type& l)
      {
        return symbol_type (token::TOK_EXPORT, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_LABEL (location_type l)
      {
        return symbol_type (token::TOK_LABEL, std::move (l));
      }
#else
      static
      symbol_type
      make_LABEL (const location_type& l)
      {
        return symbol_type (token::TOK_LABEL, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_DYNAMIC (location_type l)
      {
        return symbol_type (token::TOK_DYNAMIC, std::move (l));
      }
#else
      static
      symbol_type
      make_DYNAMIC (const location_type& l)
      {
        return symbol_type (token::TOK_DYNAMIC, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_SAFE (location_type l)
      {
        return symbol_type (token::TOK_SAFE, std::move (l));
      }
#else
      static
      symbol_type
      make_SAFE (const location_type& l)
      {
        return symbol_type (token::TOK_SAFE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_INTERRUPTIBLE (location_type l)
      {
        return symbol_type (token::TOK_INTERRUPTIBLE, std::move (l));
      }
#else
      static
      symbol_type
      make_INTERRUPTIBLE (const location_type& l)
      {
        return symbol_type (token::TOK_INTERRUPTIBLE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_UNSAFE (location_type l)
      {
        return symbol_type (token::TOK_UNSAFE, std::move (l));
      }
#else
      static
      symbol_type
      make_UNSAFE (const location_type& l)
      {
        return symbol_type (token::TOK_UNSAFE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_MDO (location_type l)
      {
        return symbol_type (token::TOK_MDO, std::move (l));
      }
#else
      static
      symbol_type
      make_MDO (const location_type& l)
      {
        return symbol_type (token::TOK_MDO, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_FAMILY (location_type l)
      {
        return symbol_type (token::TOK_FAMILY, std::move (l));
      }
#else
      static
      symbol_type
      make_FAMILY (const location_type& l)
      {
        return symbol_type (token::TOK_FAMILY, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_ROLE (location_type l)
      {
        return symbol_type (token::TOK_ROLE, std::move (l));
      }
#else
      static
      symbol_type
      make_ROLE (const location_type& l)
      {
        return symbol_type (token::TOK_ROLE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_STDCALL (location_type l)
      {
        return symbol_type (token::TOK_STDCALL, std::move (l));
      }
#else
      static
      symbol_type
      make_STDCALL (const location_type& l)
      {
        return symbol_type (token::TOK_STDCALL, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CCALL (location_type l)
      {
        return symbol_type (token::TOK_CCALL, std::move (l));
      }
#else
      static
      symbol_type
      make_CCALL (const location_type& l)
      {
        return symbol_type (token::TOK_CCALL, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CAPI (location_type l)
      {
        return symbol_type (token::TOK_CAPI, std::move (l));
      }
#else
      static
      symbol_type
      make_CAPI (const location_type& l)
      {
        return symbol_type (token::TOK_CAPI, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_PRIM (location_type l)
      {
        return symbol_type (token::TOK_PRIM, std::move (l));
      }
#else
      static
      symbol_type
      make_PRIM (const location_type& l)
      {
        return symbol_type (token::TOK_PRIM, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_JAVASCRIPT (location_type l)
      {
        return symbol_type (token::TOK_JAVASCRIPT, std::move (l));
      }
#else
      static
      symbol_type
      make_JAVASCRIPT (const location_type& l)
      {
        return symbol_type (token::TOK_JAVASCRIPT, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_PROC (location_type l)
      {
        return symbol_type (token::TOK_PROC, std::move (l));
      }
#else
      static
      symbol_type
      make_PROC (const location_type& l)
      {
        return symbol_type (token::TOK_PROC, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_REC (location_type l)
      {
        return symbol_type (token::TOK_REC, std::move (l));
      }
#else
      static
      symbol_type
      make_REC (const location_type& l)
      {
        return symbol_type (token::TOK_REC, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_GROUP (location_type l)
      {
        return symbol_type (token::TOK_GROUP, std::move (l));
      }
#else
      static
      symbol_type
      make_GROUP (const location_type& l)
      {
        return symbol_type (token::TOK_GROUP, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_BY (location_type l)
      {
        return symbol_type (token::TOK_BY, std::move (l));
      }
#else
      static
      symbol_type
      make_BY (const location_type& l)
      {
        return symbol_type (token::TOK_BY, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_USING (location_type l)
      {
        return symbol_type (token::TOK_USING, std::move (l));
      }
#else
      static
      symbol_type
      make_USING (const location_type& l)
      {
        return symbol_type (token::TOK_USING, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_PATTERN (location_type l)
      {
        return symbol_type (token::TOK_PATTERN, std::move (l));
      }
#else
      static
      symbol_type
      make_PATTERN (const location_type& l)
      {
        return symbol_type (token::TOK_PATTERN, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_STATIC (location_type l)
      {
        return symbol_type (token::TOK_STATIC, std::move (l));
      }
#else
      static
      symbol_type
      make_STATIC (const location_type& l)
      {
        return symbol_type (token::TOK_STATIC, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_STOCK (location_type l)
      {
        return symbol_type (token::TOK_STOCK, std::move (l));
      }
#else
      static
      symbol_type
      make_STOCK (const location_type& l)
      {
        return symbol_type (token::TOK_STOCK, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_ANYCLASS (location_type l)
      {
        return symbol_type (token::TOK_ANYCLASS, std::move (l));
      }
#else
      static
      symbol_type
      make_ANYCLASS (const location_type& l)
      {
        return symbol_type (token::TOK_ANYCLASS, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_VIA (location_type l)
      {
        return symbol_type (token::TOK_VIA, std::move (l));
      }
#else
      static
      symbol_type
      make_VIA (const location_type& l)
      {
        return symbol_type (token::TOK_VIA, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_UNIT (location_type l)
      {
        return symbol_type (token::TOK_UNIT, std::move (l));
      }
#else
      static
      symbol_type
      make_UNIT (const location_type& l)
      {
        return symbol_type (token::TOK_UNIT, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_SIGNATURE (location_type l)
      {
        return symbol_type (token::TOK_SIGNATURE, std::move (l));
      }
#else
      static
      symbol_type
      make_SIGNATURE (const location_type& l)
      {
        return symbol_type (token::TOK_SIGNATURE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_DEPENDENCY (location_type l)
      {
        return symbol_type (token::TOK_DEPENDENCY, std::move (l));
      }
#else
      static
      symbol_type
      make_DEPENDENCY (const location_type& l)
      {
        return symbol_type (token::TOK_DEPENDENCY, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_INLINE_PRAG (location_type l)
      {
        return symbol_type (token::TOK_INLINE_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_INLINE_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_INLINE_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_SPECIALIZE_PRAG (location_type l)
      {
        return symbol_type (token::TOK_SPECIALIZE_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_SPECIALIZE_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_SPECIALIZE_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_SPECIALIZE_INLINE_PRAG (location_type l)
      {
        return symbol_type (token::TOK_SPECIALIZE_INLINE_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_SPECIALIZE_INLINE_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_SPECIALIZE_INLINE_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_SOURCE_PRAG (location_type l)
      {
        return symbol_type (token::TOK_SOURCE_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_SOURCE_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_SOURCE_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_RULES_PRAG (location_type l)
      {
        return symbol_type (token::TOK_RULES_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_RULES_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_RULES_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CORE_PRAG (location_type l)
      {
        return symbol_type (token::TOK_CORE_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_CORE_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_CORE_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_SCC_PRAG (location_type l)
      {
        return symbol_type (token::TOK_SCC_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_SCC_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_SCC_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_GENERATED_PRAG (location_type l)
      {
        return symbol_type (token::TOK_GENERATED_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_GENERATED_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_GENERATED_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_DEPRECATED_PRAG (location_type l)
      {
        return symbol_type (token::TOK_DEPRECATED_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_DEPRECATED_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_DEPRECATED_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_WARNING_PRAG (location_type l)
      {
        return symbol_type (token::TOK_WARNING_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_WARNING_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_WARNING_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_UNPACK_PRAG (location_type l)
      {
        return symbol_type (token::TOK_UNPACK_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_UNPACK_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_UNPACK_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_NOUNPACK_PRAG (location_type l)
      {
        return symbol_type (token::TOK_NOUNPACK_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_NOUNPACK_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_NOUNPACK_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_ANN_PRAG (location_type l)
      {
        return symbol_type (token::TOK_ANN_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_ANN_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_ANN_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_MINIMAL_PRAG (location_type l)
      {
        return symbol_type (token::TOK_MINIMAL_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_MINIMAL_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_MINIMAL_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CTYPE_PRAG (location_type l)
      {
        return symbol_type (token::TOK_CTYPE_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_CTYPE_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_CTYPE_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_OVERLAPPING_PRAG (location_type l)
      {
        return symbol_type (token::TOK_OVERLAPPING_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_OVERLAPPING_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_OVERLAPPING_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_OVERLAPPABLE_PRAG (location_type l)
      {
        return symbol_type (token::TOK_OVERLAPPABLE_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_OVERLAPPABLE_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_OVERLAPPABLE_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_OVERLAPS_PRAG (location_type l)
      {
        return symbol_type (token::TOK_OVERLAPS_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_OVERLAPS_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_OVERLAPS_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_INCOHERENT_PRAG (location_type l)
      {
        return symbol_type (token::TOK_INCOHERENT_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_INCOHERENT_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_INCOHERENT_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_COMPLETE_PRAG (location_type l)
      {
        return symbol_type (token::TOK_COMPLETE_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_COMPLETE_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_COMPLETE_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CLOSE_PRAG (location_type l)
      {
        return symbol_type (token::TOK_CLOSE_PRAG, std::move (l));
      }
#else
      static
      symbol_type
      make_CLOSE_PRAG (const location_type& l)
      {
        return symbol_type (token::TOK_CLOSE_PRAG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_DOTDOT (location_type l)
      {
        return symbol_type (token::TOK_DOTDOT, std::move (l));
      }
#else
      static
      symbol_type
      make_DOTDOT (const location_type& l)
      {
        return symbol_type (token::TOK_DOTDOT, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_COLON (location_type l)
      {
        return symbol_type (token::TOK_COLON, std::move (l));
      }
#else
      static
      symbol_type
      make_COLON (const location_type& l)
      {
        return symbol_type (token::TOK_COLON, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_DCOLON (location_type l)
      {
        return symbol_type (token::TOK_DCOLON, std::move (l));
      }
#else
      static
      symbol_type
      make_DCOLON (const location_type& l)
      {
        return symbol_type (token::TOK_DCOLON, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_EQUAL (location_type l)
      {
        return symbol_type (token::TOK_EQUAL, std::move (l));
      }
#else
      static
      symbol_type
      make_EQUAL (const location_type& l)
      {
        return symbol_type (token::TOK_EQUAL, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_LAM (location_type l)
      {
        return symbol_type (token::TOK_LAM, std::move (l));
      }
#else
      static
      symbol_type
      make_LAM (const location_type& l)
      {
        return symbol_type (token::TOK_LAM, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_LCASE (location_type l)
      {
        return symbol_type (token::TOK_LCASE, std::move (l));
      }
#else
      static
      symbol_type
      make_LCASE (const location_type& l)
      {
        return symbol_type (token::TOK_LCASE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_VBAR (location_type l)
      {
        return symbol_type (token::TOK_VBAR, std::move (l));
      }
#else
      static
      symbol_type
      make_VBAR (const location_type& l)
      {
        return symbol_type (token::TOK_VBAR, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_LARROW (location_type l)
      {
        return symbol_type (token::TOK_LARROW, std::move (l));
      }
#else
      static
      symbol_type
      make_LARROW (const location_type& l)
      {
        return symbol_type (token::TOK_LARROW, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_RARROW (location_type l)
      {
        return symbol_type (token::TOK_RARROW, std::move (l));
      }
#else
      static
      symbol_type
      make_RARROW (const location_type& l)
      {
        return symbol_type (token::TOK_RARROW, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_AT (location_type l)
      {
        return symbol_type (token::TOK_AT, std::move (l));
      }
#else
      static
      symbol_type
      make_AT (const location_type& l)
      {
        return symbol_type (token::TOK_AT, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_TILDE (location_type l)
      {
        return symbol_type (token::TOK_TILDE, std::move (l));
      }
#else
      static
      symbol_type
      make_TILDE (const location_type& l)
      {
        return symbol_type (token::TOK_TILDE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_DARROW (location_type l)
      {
        return symbol_type (token::TOK_DARROW, std::move (l));
      }
#else
      static
      symbol_type
      make_DARROW (const location_type& l)
      {
        return symbol_type (token::TOK_DARROW, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_MINUS (location_type l)
      {
        return symbol_type (token::TOK_MINUS, std::move (l));
      }
#else
      static
      symbol_type
      make_MINUS (const location_type& l)
      {
        return symbol_type (token::TOK_MINUS, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_BANG (location_type l)
      {
        return symbol_type (token::TOK_BANG, std::move (l));
      }
#else
      static
      symbol_type
      make_BANG (const location_type& l)
      {
        return symbol_type (token::TOK_BANG, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_STAR (location_type l)
      {
        return symbol_type (token::TOK_STAR, std::move (l));
      }
#else
      static
      symbol_type
      make_STAR (const location_type& l)
      {
        return symbol_type (token::TOK_STAR, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_lARROWTAIL (location_type l)
      {
        return symbol_type (token::TOK_lARROWTAIL, std::move (l));
      }
#else
      static
      symbol_type
      make_lARROWTAIL (const location_type& l)
      {
        return symbol_type (token::TOK_lARROWTAIL, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_rARROWTAIL (location_type l)
      {
        return symbol_type (token::TOK_rARROWTAIL, std::move (l));
      }
#else
      static
      symbol_type
      make_rARROWTAIL (const location_type& l)
      {
        return symbol_type (token::TOK_rARROWTAIL, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_LARROWTAIL (location_type l)
      {
        return symbol_type (token::TOK_LARROWTAIL, std::move (l));
      }
#else
      static
      symbol_type
      make_LARROWTAIL (const location_type& l)
      {
        return symbol_type (token::TOK_LARROWTAIL, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_RARROWTAIL (location_type l)
      {
        return symbol_type (token::TOK_RARROWTAIL, std::move (l));
      }
#else
      static
      symbol_type
      make_RARROWTAIL (const location_type& l)
      {
        return symbol_type (token::TOK_RARROWTAIL, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_DOT (location_type l)
      {
        return symbol_type (token::TOK_DOT, std::move (l));
      }
#else
      static
      symbol_type
      make_DOT (const location_type& l)
      {
        return symbol_type (token::TOK_DOT, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_TYPEAPP (location_type l)
      {
        return symbol_type (token::TOK_TYPEAPP, std::move (l));
      }
#else
      static
      symbol_type
      make_TYPEAPP (const location_type& l)
      {
        return symbol_type (token::TOK_TYPEAPP, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_OCURLY (location_type l)
      {
        return symbol_type (token::TOK_OCURLY, std::move (l));
      }
#else
      static
      symbol_type
      make_OCURLY (const location_type& l)
      {
        return symbol_type (token::TOK_OCURLY, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CCURLY (location_type l)
      {
        return symbol_type (token::TOK_CCURLY, std::move (l));
      }
#else
      static
      symbol_type
      make_CCURLY (const location_type& l)
      {
        return symbol_type (token::TOK_CCURLY, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_VOCURLY (location_type l)
      {
        return symbol_type (token::TOK_VOCURLY, std::move (l));
      }
#else
      static
      symbol_type
      make_VOCURLY (const location_type& l)
      {
        return symbol_type (token::TOK_VOCURLY, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_VCCURLY (location_type l)
      {
        return symbol_type (token::TOK_VCCURLY, std::move (l));
      }
#else
      static
      symbol_type
      make_VCCURLY (const location_type& l)
      {
        return symbol_type (token::TOK_VCCURLY, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_OBRACK (location_type l)
      {
        return symbol_type (token::TOK_OBRACK, std::move (l));
      }
#else
      static
      symbol_type
      make_OBRACK (const location_type& l)
      {
        return symbol_type (token::TOK_OBRACK, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CBRACK (location_type l)
      {
        return symbol_type (token::TOK_CBRACK, std::move (l));
      }
#else
      static
      symbol_type
      make_CBRACK (const location_type& l)
      {
        return symbol_type (token::TOK_CBRACK, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_OPABRACK (location_type l)
      {
        return symbol_type (token::TOK_OPABRACK, std::move (l));
      }
#else
      static
      symbol_type
      make_OPABRACK (const location_type& l)
      {
        return symbol_type (token::TOK_OPABRACK, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CPABRACK (location_type l)
      {
        return symbol_type (token::TOK_CPABRACK, std::move (l));
      }
#else
      static
      symbol_type
      make_CPABRACK (const location_type& l)
      {
        return symbol_type (token::TOK_CPABRACK, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_OPAREN (location_type l)
      {
        return symbol_type (token::TOK_OPAREN, std::move (l));
      }
#else
      static
      symbol_type
      make_OPAREN (const location_type& l)
      {
        return symbol_type (token::TOK_OPAREN, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CPAREN (location_type l)
      {
        return symbol_type (token::TOK_CPAREN, std::move (l));
      }
#else
      static
      symbol_type
      make_CPAREN (const location_type& l)
      {
        return symbol_type (token::TOK_CPAREN, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_OUBXPAREN (location_type l)
      {
        return symbol_type (token::TOK_OUBXPAREN, std::move (l));
      }
#else
      static
      symbol_type
      make_OUBXPAREN (const location_type& l)
      {
        return symbol_type (token::TOK_OUBXPAREN, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CUBXPAREN (location_type l)
      {
        return symbol_type (token::TOK_CUBXPAREN, std::move (l));
      }
#else
      static
      symbol_type
      make_CUBXPAREN (const location_type& l)
      {
        return symbol_type (token::TOK_CUBXPAREN, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_OPARENBAR (location_type l)
      {
        return symbol_type (token::TOK_OPARENBAR, std::move (l));
      }
#else
      static
      symbol_type
      make_OPARENBAR (const location_type& l)
      {
        return symbol_type (token::TOK_OPARENBAR, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CPARENBAR (location_type l)
      {
        return symbol_type (token::TOK_CPARENBAR, std::move (l));
      }
#else
      static
      symbol_type
      make_CPARENBAR (const location_type& l)
      {
        return symbol_type (token::TOK_CPARENBAR, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_SEMI (location_type l)
      {
        return symbol_type (token::TOK_SEMI, std::move (l));
      }
#else
      static
      symbol_type
      make_SEMI (const location_type& l)
      {
        return symbol_type (token::TOK_SEMI, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_COMMA (location_type l)
      {
        return symbol_type (token::TOK_COMMA, std::move (l));
      }
#else
      static
      symbol_type
      make_COMMA (const location_type& l)
      {
        return symbol_type (token::TOK_COMMA, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_BACKQUOTE (location_type l)
      {
        return symbol_type (token::TOK_BACKQUOTE, std::move (l));
      }
#else
      static
      symbol_type
      make_BACKQUOTE (const location_type& l)
      {
        return symbol_type (token::TOK_BACKQUOTE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_SIMPLEQUOTE (location_type l)
      {
        return symbol_type (token::TOK_SIMPLEQUOTE, std::move (l));
      }
#else
      static
      symbol_type
      make_SIMPLEQUOTE (const location_type& l)
      {
        return symbol_type (token::TOK_SIMPLEQUOTE, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_VARID (std::string v, location_type l)
      {
        return symbol_type (token::TOK_VARID, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_VARID (const std::string& v, const location_type& l)
      {
        return symbol_type (token::TOK_VARID, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CONID (std::string v, location_type l)
      {
        return symbol_type (token::TOK_CONID, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_CONID (const std::string& v, const location_type& l)
      {
        return symbol_type (token::TOK_CONID, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_VARSYM (std::string v, location_type l)
      {
        return symbol_type (token::TOK_VARSYM, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_VARSYM (const std::string& v, const location_type& l)
      {
        return symbol_type (token::TOK_VARSYM, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CONSYM (std::string v, location_type l)
      {
        return symbol_type (token::TOK_CONSYM, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_CONSYM (const std::string& v, const location_type& l)
      {
        return symbol_type (token::TOK_CONSYM, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_QVARID (std::string v, location_type l)
      {
        return symbol_type (token::TOK_QVARID, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_QVARID (const std::string& v, const location_type& l)
      {
        return symbol_type (token::TOK_QVARID, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_QCONID (std::string v, location_type l)
      {
        return symbol_type (token::TOK_QCONID, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_QCONID (const std::string& v, const location_type& l)
      {
        return symbol_type (token::TOK_QCONID, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_QVARSYM (std::string v, location_type l)
      {
        return symbol_type (token::TOK_QVARSYM, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_QVARSYM (const std::string& v, const location_type& l)
      {
        return symbol_type (token::TOK_QVARSYM, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_QCONSYM (std::string v, location_type l)
      {
        return symbol_type (token::TOK_QCONSYM, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_QCONSYM (const std::string& v, const location_type& l)
      {
        return symbol_type (token::TOK_QCONSYM, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_IPDUPVARID (std::string v, location_type l)
      {
        return symbol_type (token::TOK_IPDUPVARID, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_IPDUPVARID (const std::string& v, const location_type& l)
      {
        return symbol_type (token::TOK_IPDUPVARID, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_LABELVARID (std::string v, location_type l)
      {
        return symbol_type (token::TOK_LABELVARID, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_LABELVARID (const std::string& v, const location_type& l)
      {
        return symbol_type (token::TOK_LABELVARID, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_CHAR (char v, location_type l)
      {
        return symbol_type (token::TOK_CHAR, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_CHAR (const char& v, const location_type& l)
      {
        return symbol_type (token::TOK_CHAR, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_STRING (std::string v, location_type l)
      {
        return symbol_type (token::TOK_STRING, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_STRING (const std::string& v, const location_type& l)
      {
        return symbol_type (token::TOK_STRING, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_INTEGER (int v, location_type l)
      {
        return symbol_type (token::TOK_INTEGER, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_INTEGER (const int& v, const location_type& l)
      {
        return symbol_type (token::TOK_INTEGER, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_RATIONAL (double v, location_type l)
      {
        return symbol_type (token::TOK_RATIONAL, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_RATIONAL (const double& v, const location_type& l)
      {
        return symbol_type (token::TOK_RATIONAL, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_PRIMCHAR (char v, location_type l)
      {
        return symbol_type (token::TOK_PRIMCHAR, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_PRIMCHAR (const char& v, const location_type& l)
      {
        return symbol_type (token::TOK_PRIMCHAR, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_PRIMSTRING (std::string v, location_type l)
      {
        return symbol_type (token::TOK_PRIMSTRING, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_PRIMSTRING (const std::string& v, const location_type& l)
      {
        return symbol_type (token::TOK_PRIMSTRING, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_PRIMINTEGER (int v, location_type l)
      {
        return symbol_type (token::TOK_PRIMINTEGER, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_PRIMINTEGER (const int& v, const location_type& l)
      {
        return symbol_type (token::TOK_PRIMINTEGER, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_PRINTWORD (int v, location_type l)
      {
        return symbol_type (token::TOK_PRINTWORD, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_PRINTWORD (const int& v, const location_type& l)
      {
        return symbol_type (token::TOK_PRINTWORD, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_PRIMFLOAT (float v, location_type l)
      {
        return symbol_type (token::TOK_PRIMFLOAT, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_PRIMFLOAT (const float& v, const location_type& l)
      {
        return symbol_type (token::TOK_PRIMFLOAT, v, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_PRIMDOUBLE (double v, location_type l)
      {
        return symbol_type (token::TOK_PRIMDOUBLE, std::move (v), std::move (l));
      }
#else
      static
      symbol_type
      make_PRIMDOUBLE (const double& v, const location_type& l)
      {
        return symbol_type (token::TOK_PRIMDOUBLE, v, l);
      }
#endif


  private:
    /// This class is not copyable.
    parser (const parser&);
    parser& operator= (const parser&);

    /// State numbers.
    typedef int state_type;

    /// Generate an error message.
    /// \param yystate   the state where the error occurred.
    /// \param yyla      the lookahead token.
    virtual std::string yysyntax_error_ (state_type yystate,
                                         const symbol_type& yyla) const;

    /// Compute post-reduction state.
    /// \param yystate   the current state
    /// \param yysym     the nonterminal to push on the stack
    state_type yy_lr_goto_state_ (state_type yystate, int yysym);

    /// Whether the given \c yypact_ value indicates a defaulted state.
    /// \param yyvalue   the value to check
    static bool yy_pact_value_is_default_ (int yyvalue);

    /// Whether the given \c yytable_ value indicates a syntax error.
    /// \param yyvalue   the value to check
    static bool yy_table_value_is_error_ (int yyvalue);

    static const short yypact_ninf_;
    static const short yytable_ninf_;

    /// Convert a scanner token number \a t to a symbol number.
    static token_number_type yytranslate_ (token_type t);

    // Tables.
  // YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
  // STATE-NUM.
  static const short yypact_[];

  // YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
  // Performed when YYTABLE does not specify something else to do.  Zero
  // means the default is an error.
  static const unsigned short yydefact_[];

  // YYPGOTO[NTERM-NUM].
  static const short yypgoto_[];

  // YYDEFGOTO[NTERM-NUM].
  static const short yydefgoto_[];

  // YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
  // positive, shift that token.  If negative, reduce the rule whose
  // number is the opposite.  If YYTABLE_NINF, syntax error.
  static const short yytable_[];

  static const short yycheck_[];

  // YYSTOS[STATE-NUM] -- The (internal number of the) accessing
  // symbol of state STATE-NUM.
  static const unsigned short yystos_[];

  // YYR1[YYN] -- Symbol number of symbol that rule YYN derives.
  static const unsigned short yyr1_[];

  // YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.
  static const unsigned char yyr2_[];


    /// Convert the symbol name \a n to a form suitable for a diagnostic.
    static std::string yytnamerr_ (const char *n);


    /// For a symbol, its name in clear.
    static const char* const yytname_[];
#if YYDEBUG
  // YYRLINE[YYN] -- Source line where rule number YYN was defined.
  static const unsigned short yyrline_[];
    /// Report on the debug stream that the rule \a r is going to be reduced.
    virtual void yy_reduce_print_ (int r);
    /// Print the state stack on the debug stream.
    virtual void yystack_print_ ();

    /// Debugging level.
    int yydebug_;
    /// Debug stream.
    std::ostream* yycdebug_;

    /// \brief Display a symbol type, value and location.
    /// \param yyo    The output stream.
    /// \param yysym  The symbol.
    template <typename Base>
    void yy_print_ (std::ostream& yyo, const basic_symbol<Base>& yysym) const;
#endif

    /// \brief Reclaim the memory associated to a symbol.
    /// \param yymsg     Why this token is reclaimed.
    ///                  If null, print nothing.
    /// \param yysym     The symbol.
    template <typename Base>
    void yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const;

  private:
    /// Type access provider for state based symbols.
    struct by_state
    {
      /// Default constructor.
      by_state () YY_NOEXCEPT;

      /// The symbol type as needed by the constructor.
      typedef state_type kind_type;

      /// Constructor.
      by_state (kind_type s) YY_NOEXCEPT;

      /// Copy constructor.
      by_state (const by_state& that) YY_NOEXCEPT;

      /// Record that this symbol is empty.
      void clear () YY_NOEXCEPT;

      /// Steal the symbol type from \a that.
      void move (by_state& that);

      /// The (internal) type number (corresponding to \a state).
      /// \a empty_symbol when empty.
      symbol_number_type type_get () const YY_NOEXCEPT;

      /// The state number used to denote an empty symbol.
      enum { empty_state = -1 };

      /// The state.
      /// \a empty when empty.
      state_type state;
    };

    /// "Internal" symbol: element of the stack.
    struct stack_symbol_type : basic_symbol<by_state>
    {
      /// Superclass.
      typedef basic_symbol<by_state> super_type;
      /// Construct an empty symbol.
      stack_symbol_type ();
      /// Move or copy construction.
      stack_symbol_type (YY_RVREF (stack_symbol_type) that);
      /// Steal the contents from \a sym to build this.
      stack_symbol_type (state_type s, YY_MOVE_REF (symbol_type) sym);
#if YY_CPLUSPLUS < 201103L
      /// Assignment, needed by push_back by some old implementations.
      /// Moves the contents of that.
      stack_symbol_type& operator= (stack_symbol_type& that);
#endif
    };

    /// A stack with random access from its top.
    template <typename T, typename S = std::vector<T> >
    class stack
    {
    public:
      // Hide our reversed order.
      typedef typename S::reverse_iterator iterator;
      typedef typename S::const_reverse_iterator const_iterator;
      typedef typename S::size_type size_type;

      stack (size_type n = 200)
        : seq_ (n)
      {}

      /// Random access.
      ///
      /// Index 0 returns the topmost element.
      T&
      operator[] (size_type i)
      {
        return seq_[size () - 1 - i];
      }

      /// Random access.
      ///
      /// Index 0 returns the topmost element.
      T&
      operator[] (int i)
      {
        return operator[] (size_type (i));
      }

      /// Random access.
      ///
      /// Index 0 returns the topmost element.
      const T&
      operator[] (size_type i) const
      {
        return seq_[size () - 1 - i];
      }

      /// Random access.
      ///
      /// Index 0 returns the topmost element.
      const T&
      operator[] (int i) const
      {
        return operator[] (size_type (i));
      }

      /// Steal the contents of \a t.
      ///
      /// Close to move-semantics.
      void
      push (YY_MOVE_REF (T) t)
      {
        seq_.push_back (T ());
        operator[] (0).move (t);
      }

      /// Pop elements from the stack.
      void
      pop (int n = 1) YY_NOEXCEPT
      {
        for (; 0 < n; --n)
          seq_.pop_back ();
      }

      /// Pop all elements from the stack.
      void
      clear () YY_NOEXCEPT
      {
        seq_.clear ();
      }

      /// Number of elements on the stack.
      size_type
      size () const YY_NOEXCEPT
      {
        return seq_.size ();
      }

      /// Iterator on top of the stack (going downwards).
      const_iterator
      begin () const YY_NOEXCEPT
      {
        return seq_.rbegin ();
      }

      /// Bottom of the stack.
      const_iterator
      end () const YY_NOEXCEPT
      {
        return seq_.rend ();
      }

      /// Present a slice of the top of a stack.
      class slice
      {
      public:
        slice (const stack& stack, int range)
          : stack_ (stack)
          , range_ (range)
        {}

        const T&
        operator[] (int i) const
        {
          return stack_[range_ - i];
        }

      private:
        const stack& stack_;
        int range_;
      };

    private:
      stack (const stack&);
      stack& operator= (const stack&);
      /// The wrapped container.
      S seq_;
    };


    /// Stack type.
    typedef stack<stack_symbol_type> stack_type;

    /// The stack.
    stack_type yystack_;

    /// Push a new state on the stack.
    /// \param m    a debug message to display
    ///             if null, no trace is output.
    /// \param sym  the symbol
    /// \warning the contents of \a s.value is stolen.
    void yypush_ (const char* m, YY_MOVE_REF (stack_symbol_type) sym);

    /// Push a new look ahead token on the state on the stack.
    /// \param m    a debug message to display
    ///             if null, no trace is output.
    /// \param s    the state
    /// \param sym  the symbol (for its value and location).
    /// \warning the contents of \a sym.value is stolen.
    void yypush_ (const char* m, state_type s, YY_MOVE_REF (symbol_type) sym);

    /// Pop \a n symbols from the stack.
    void yypop_ (int n = 1);

    /// Constants.
    enum
    {
      yyeof_ = 0,
      yylast_ = 5826,     ///< Last index in yytable_.
      yynnts_ = 205,  ///< Number of nonterminal symbols.
      yyfinal_ = 12, ///< Termination state number.
      yyterror_ = 1,
      yyerrcode_ = 256,
      yyntokens_ = 141  ///< Number of tokens.
    };


    // User arguments.
    driver& drv;
  };

  inline
  parser::token_number_type
  parser::yytranslate_ (token_type t)
  {
    // YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to
    // TOKEN-NUM as returned by yylex.
    static
    const token_number_type
    translate_table[] =
    {
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140
    };
    const unsigned user_token_number_max_ = 395;
    const token_number_type undef_token_ = 2;

    if (static_cast<int> (t) <= yyeof_)
      return yyeof_;
    else if (static_cast<unsigned> (t) <= user_token_number_max_)
      return translate_table[t];
    else
      return undef_token_;
  }

  // basic_symbol.
#if 201103L <= YY_CPLUSPLUS
  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (basic_symbol&& that)
    : Base (std::move (that))
    , value ()
    , location (std::move (that.location))
  {
    switch (this->type_get ())
    {
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        value.move< bool > (std::move (that.value));
        break;

      case 127: // "CHAR"
      case 131: // "PRIMCHAR"
        value.move< char > (std::move (that.value));
        break;

      case 130: // "RATIONAL"
      case 136: // "PRIMDOUBLE"
        value.move< double > (std::move (that.value));
        break;

      case 143: // module
      case 146: // body
      case 147: // body2
      case 148: // top
      case 149: // top1
      case 150: // maybeexports
      case 153: // export
      case 157: // qcname_ext_w_wildcard
      case 158: // qcname_ext
      case 159: // qcname
      case 164: // importdecl
      case 170: // maybeimpspec
      case 171: // impspec
      case 177: // topdecl
      case 179: // ty_decl
      case 186: // tycl_hdr
      case 204: // binds
      case 205: // wherebinds
      case 208: // opt_sig
      case 209: // opt_tyconsig
      case 210: // sigtype
      case 211: // sigtypedoc
      case 217: // ctype
      case 218: // ctypedoc
      case 219: // context
      case 220: // context_no_ops
      case 221: // type
      case 222: // typedoc
      case 223: // btype
      case 226: // tyapp
      case 227: // atype_docs
      case 228: // atype
      case 235: // tv_bndr
      case 240: // kind
      case 243: // constr
      case 244: // forall
      case 245: // constr_stuff
      case 248: // fielddecl
      case 253: // decl_no_th
      case 254: // decl
      case 255: // rhs
      case 257: // gdrh
      case 258: // sigdecl
      case 261: // exp
      case 264: // exp10_top
      case 265: // exp10
      case 269: // aexp
      case 270: // aexp1
      case 271: // aexp2
      case 272: // texp
      case 274: // list
      case 277: // transformqual
      case 283: // alt
      case 284: // alt_rhs
      case 286: // ifgdpats
      case 287: // gdpat
      case 288: // pat
      case 289: // bindpat
      case 290: // apat
      case 294: // stmt
      case 295: // qual
      case 342: // literal
        value.move< expression_ref > (std::move (that.value));
        break;

      case 135: // "PRIMFLOAT"
        value.move< float > (std::move (that.value));
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 345: // commas
        value.move< int > (std::move (that.value));
        break;

      case 172: // prec
        value.move< std::optional<int> > (std::move (that.value));
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.move< std::optional<std::string> > (std::move (that.value));
        break;

      case 117: // "VARID"
      case 118: // "CONID"
      case 119: // "VARSYM"
      case 120: // "CONSYM"
      case 121: // "QVARID"
      case 122: // "QCONID"
      case 123: // "QVARSYM"
      case 124: // "QCONSYM"
      case 125: // "IPDUPVARID"
      case 126: // "LABELVARID"
      case 128: // "STRING"
      case 132: // "PRIMSTRING"
      case 173: // infix
      case 184: // data_or_newtype
      case 214: // strict_mark
      case 215: // strictness
      case 299: // qcon
      case 300: // gen_qcon
      case 301: // con
      case 303: // sysdcon_no_list
      case 304: // sysdcon
      case 305: // conop
      case 306: // qconop
      case 307: // gtycon
      case 308: // ntgtycon
      case 309: // oqtycon
      case 310: // oqtycon_no_varcon
      case 311: // qtyconop
      case 312: // qtycondoc
      case 313: // qtycon
      case 314: // tycon
      case 315: // qtyconsym
      case 316: // tyconsym
      case 317: // op
      case 318: // varop
      case 319: // qop
      case 320: // qopm
      case 321: // hole_op
      case 322: // qvarop
      case 323: // qvaropm
      case 324: // tyvar
      case 325: // tyvarop
      case 326: // tyvarid
      case 327: // var
      case 328: // qvar
      case 329: // qvarid
      case 330: // varid
      case 331: // qvarsym
      case 332: // qvarsym_no_minus
      case 333: // qvarsym1
      case 334: // varsym
      case 335: // varsym_no_minus
      case 336: // special_id
      case 337: // special_sym
      case 338: // qconid
      case 339: // conid
      case 340: // qconsym
      case 341: // consym
      case 344: // modid
        value.move< std::string > (std::move (that.value));
        break;

      case 151: // exportlist
      case 152: // exportlist1
      case 155: // qcnames
      case 156: // qcnames1
      case 162: // importdecls
      case 163: // importdecls_semi
      case 175: // topdecls
      case 176: // topdecls_semi
      case 202: // decls
      case 203: // decllist
      case 212: // sig_vars
      case 213: // sigtypes1
      case 224: // btype_no_ops
      case 225: // tyapps
      case 231: // comma_types0
      case 232: // comma_types1
      case 234: // tv_bndrs
      case 241: // constrs
      case 242: // constrs1
      case 246: // fielddecls
      case 247: // fielddecls1
      case 256: // gdrhs
      case 262: // infixexp
      case 263: // infixexp_top
      case 268: // fexp
      case 273: // tup_exprs
      case 275: // lexps
      case 276: // squals
      case 278: // guardquals
      case 279: // guardquals1
      case 280: // altslist
      case 281: // alts
      case 282: // alts1
      case 285: // gdpats
      case 291: // apats1
      case 292: // stmtlist
      case 293: // stmts
        value.move< std::vector<expression_ref> > (std::move (that.value));
        break;

      case 174: // ops
        value.move< std::vector<std::string> > (std::move (that.value));
        break;

      default:
        break;
    }

  }
#endif

  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (const basic_symbol& that)
    : Base (that)
    , value ()
    , location (that.location)
  {
    switch (this->type_get ())
    {
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        value.copy< bool > (YY_MOVE (that.value));
        break;

      case 127: // "CHAR"
      case 131: // "PRIMCHAR"
        value.copy< char > (YY_MOVE (that.value));
        break;

      case 130: // "RATIONAL"
      case 136: // "PRIMDOUBLE"
        value.copy< double > (YY_MOVE (that.value));
        break;

      case 143: // module
      case 146: // body
      case 147: // body2
      case 148: // top
      case 149: // top1
      case 150: // maybeexports
      case 153: // export
      case 157: // qcname_ext_w_wildcard
      case 158: // qcname_ext
      case 159: // qcname
      case 164: // importdecl
      case 170: // maybeimpspec
      case 171: // impspec
      case 177: // topdecl
      case 179: // ty_decl
      case 186: // tycl_hdr
      case 204: // binds
      case 205: // wherebinds
      case 208: // opt_sig
      case 209: // opt_tyconsig
      case 210: // sigtype
      case 211: // sigtypedoc
      case 217: // ctype
      case 218: // ctypedoc
      case 219: // context
      case 220: // context_no_ops
      case 221: // type
      case 222: // typedoc
      case 223: // btype
      case 226: // tyapp
      case 227: // atype_docs
      case 228: // atype
      case 235: // tv_bndr
      case 240: // kind
      case 243: // constr
      case 244: // forall
      case 245: // constr_stuff
      case 248: // fielddecl
      case 253: // decl_no_th
      case 254: // decl
      case 255: // rhs
      case 257: // gdrh
      case 258: // sigdecl
      case 261: // exp
      case 264: // exp10_top
      case 265: // exp10
      case 269: // aexp
      case 270: // aexp1
      case 271: // aexp2
      case 272: // texp
      case 274: // list
      case 277: // transformqual
      case 283: // alt
      case 284: // alt_rhs
      case 286: // ifgdpats
      case 287: // gdpat
      case 288: // pat
      case 289: // bindpat
      case 290: // apat
      case 294: // stmt
      case 295: // qual
      case 342: // literal
        value.copy< expression_ref > (YY_MOVE (that.value));
        break;

      case 135: // "PRIMFLOAT"
        value.copy< float > (YY_MOVE (that.value));
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 345: // commas
        value.copy< int > (YY_MOVE (that.value));
        break;

      case 172: // prec
        value.copy< std::optional<int> > (YY_MOVE (that.value));
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.copy< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case 117: // "VARID"
      case 118: // "CONID"
      case 119: // "VARSYM"
      case 120: // "CONSYM"
      case 121: // "QVARID"
      case 122: // "QCONID"
      case 123: // "QVARSYM"
      case 124: // "QCONSYM"
      case 125: // "IPDUPVARID"
      case 126: // "LABELVARID"
      case 128: // "STRING"
      case 132: // "PRIMSTRING"
      case 173: // infix
      case 184: // data_or_newtype
      case 214: // strict_mark
      case 215: // strictness
      case 299: // qcon
      case 300: // gen_qcon
      case 301: // con
      case 303: // sysdcon_no_list
      case 304: // sysdcon
      case 305: // conop
      case 306: // qconop
      case 307: // gtycon
      case 308: // ntgtycon
      case 309: // oqtycon
      case 310: // oqtycon_no_varcon
      case 311: // qtyconop
      case 312: // qtycondoc
      case 313: // qtycon
      case 314: // tycon
      case 315: // qtyconsym
      case 316: // tyconsym
      case 317: // op
      case 318: // varop
      case 319: // qop
      case 320: // qopm
      case 321: // hole_op
      case 322: // qvarop
      case 323: // qvaropm
      case 324: // tyvar
      case 325: // tyvarop
      case 326: // tyvarid
      case 327: // var
      case 328: // qvar
      case 329: // qvarid
      case 330: // varid
      case 331: // qvarsym
      case 332: // qvarsym_no_minus
      case 333: // qvarsym1
      case 334: // varsym
      case 335: // varsym_no_minus
      case 336: // special_id
      case 337: // special_sym
      case 338: // qconid
      case 339: // conid
      case 340: // qconsym
      case 341: // consym
      case 344: // modid
        value.copy< std::string > (YY_MOVE (that.value));
        break;

      case 151: // exportlist
      case 152: // exportlist1
      case 155: // qcnames
      case 156: // qcnames1
      case 162: // importdecls
      case 163: // importdecls_semi
      case 175: // topdecls
      case 176: // topdecls_semi
      case 202: // decls
      case 203: // decllist
      case 212: // sig_vars
      case 213: // sigtypes1
      case 224: // btype_no_ops
      case 225: // tyapps
      case 231: // comma_types0
      case 232: // comma_types1
      case 234: // tv_bndrs
      case 241: // constrs
      case 242: // constrs1
      case 246: // fielddecls
      case 247: // fielddecls1
      case 256: // gdrhs
      case 262: // infixexp
      case 263: // infixexp_top
      case 268: // fexp
      case 273: // tup_exprs
      case 275: // lexps
      case 276: // squals
      case 278: // guardquals
      case 279: // guardquals1
      case 280: // altslist
      case 281: // alts
      case 282: // alts1
      case 285: // gdpats
      case 291: // apats1
      case 292: // stmtlist
      case 293: // stmts
        value.copy< std::vector<expression_ref> > (YY_MOVE (that.value));
        break;

      case 174: // ops
        value.copy< std::vector<std::string> > (YY_MOVE (that.value));
        break;

      default:
        break;
    }

  }



  template <typename Base>
  bool
  parser::basic_symbol<Base>::empty () const YY_NOEXCEPT
  {
    return Base::type_get () == empty_symbol;
  }

  template <typename Base>
  void
  parser::basic_symbol<Base>::move (basic_symbol& s)
  {
    super_type::move (s);
    switch (this->type_get ())
    {
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        value.move< bool > (YY_MOVE (s.value));
        break;

      case 127: // "CHAR"
      case 131: // "PRIMCHAR"
        value.move< char > (YY_MOVE (s.value));
        break;

      case 130: // "RATIONAL"
      case 136: // "PRIMDOUBLE"
        value.move< double > (YY_MOVE (s.value));
        break;

      case 143: // module
      case 146: // body
      case 147: // body2
      case 148: // top
      case 149: // top1
      case 150: // maybeexports
      case 153: // export
      case 157: // qcname_ext_w_wildcard
      case 158: // qcname_ext
      case 159: // qcname
      case 164: // importdecl
      case 170: // maybeimpspec
      case 171: // impspec
      case 177: // topdecl
      case 179: // ty_decl
      case 186: // tycl_hdr
      case 204: // binds
      case 205: // wherebinds
      case 208: // opt_sig
      case 209: // opt_tyconsig
      case 210: // sigtype
      case 211: // sigtypedoc
      case 217: // ctype
      case 218: // ctypedoc
      case 219: // context
      case 220: // context_no_ops
      case 221: // type
      case 222: // typedoc
      case 223: // btype
      case 226: // tyapp
      case 227: // atype_docs
      case 228: // atype
      case 235: // tv_bndr
      case 240: // kind
      case 243: // constr
      case 244: // forall
      case 245: // constr_stuff
      case 248: // fielddecl
      case 253: // decl_no_th
      case 254: // decl
      case 255: // rhs
      case 257: // gdrh
      case 258: // sigdecl
      case 261: // exp
      case 264: // exp10_top
      case 265: // exp10
      case 269: // aexp
      case 270: // aexp1
      case 271: // aexp2
      case 272: // texp
      case 274: // list
      case 277: // transformqual
      case 283: // alt
      case 284: // alt_rhs
      case 286: // ifgdpats
      case 287: // gdpat
      case 288: // pat
      case 289: // bindpat
      case 290: // apat
      case 294: // stmt
      case 295: // qual
      case 342: // literal
        value.move< expression_ref > (YY_MOVE (s.value));
        break;

      case 135: // "PRIMFLOAT"
        value.move< float > (YY_MOVE (s.value));
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 345: // commas
        value.move< int > (YY_MOVE (s.value));
        break;

      case 172: // prec
        value.move< std::optional<int> > (YY_MOVE (s.value));
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.move< std::optional<std::string> > (YY_MOVE (s.value));
        break;

      case 117: // "VARID"
      case 118: // "CONID"
      case 119: // "VARSYM"
      case 120: // "CONSYM"
      case 121: // "QVARID"
      case 122: // "QCONID"
      case 123: // "QVARSYM"
      case 124: // "QCONSYM"
      case 125: // "IPDUPVARID"
      case 126: // "LABELVARID"
      case 128: // "STRING"
      case 132: // "PRIMSTRING"
      case 173: // infix
      case 184: // data_or_newtype
      case 214: // strict_mark
      case 215: // strictness
      case 299: // qcon
      case 300: // gen_qcon
      case 301: // con
      case 303: // sysdcon_no_list
      case 304: // sysdcon
      case 305: // conop
      case 306: // qconop
      case 307: // gtycon
      case 308: // ntgtycon
      case 309: // oqtycon
      case 310: // oqtycon_no_varcon
      case 311: // qtyconop
      case 312: // qtycondoc
      case 313: // qtycon
      case 314: // tycon
      case 315: // qtyconsym
      case 316: // tyconsym
      case 317: // op
      case 318: // varop
      case 319: // qop
      case 320: // qopm
      case 321: // hole_op
      case 322: // qvarop
      case 323: // qvaropm
      case 324: // tyvar
      case 325: // tyvarop
      case 326: // tyvarid
      case 327: // var
      case 328: // qvar
      case 329: // qvarid
      case 330: // varid
      case 331: // qvarsym
      case 332: // qvarsym_no_minus
      case 333: // qvarsym1
      case 334: // varsym
      case 335: // varsym_no_minus
      case 336: // special_id
      case 337: // special_sym
      case 338: // qconid
      case 339: // conid
      case 340: // qconsym
      case 341: // consym
      case 344: // modid
        value.move< std::string > (YY_MOVE (s.value));
        break;

      case 151: // exportlist
      case 152: // exportlist1
      case 155: // qcnames
      case 156: // qcnames1
      case 162: // importdecls
      case 163: // importdecls_semi
      case 175: // topdecls
      case 176: // topdecls_semi
      case 202: // decls
      case 203: // decllist
      case 212: // sig_vars
      case 213: // sigtypes1
      case 224: // btype_no_ops
      case 225: // tyapps
      case 231: // comma_types0
      case 232: // comma_types1
      case 234: // tv_bndrs
      case 241: // constrs
      case 242: // constrs1
      case 246: // fielddecls
      case 247: // fielddecls1
      case 256: // gdrhs
      case 262: // infixexp
      case 263: // infixexp_top
      case 268: // fexp
      case 273: // tup_exprs
      case 275: // lexps
      case 276: // squals
      case 278: // guardquals
      case 279: // guardquals1
      case 280: // altslist
      case 281: // alts
      case 282: // alts1
      case 285: // gdpats
      case 291: // apats1
      case 292: // stmtlist
      case 293: // stmts
        value.move< std::vector<expression_ref> > (YY_MOVE (s.value));
        break;

      case 174: // ops
        value.move< std::vector<std::string> > (YY_MOVE (s.value));
        break;

      default:
        break;
    }

    location = YY_MOVE (s.location);
  }

  // by_type.
  inline
  parser::by_type::by_type ()
    : type (empty_symbol)
  {}

#if 201103L <= YY_CPLUSPLUS
  inline
  parser::by_type::by_type (by_type&& that)
    : type (that.type)
  {
    that.clear ();
  }
#endif

  inline
  parser::by_type::by_type (const by_type& that)
    : type (that.type)
  {}

  inline
  parser::by_type::by_type (token_type t)
    : type (yytranslate_ (t))
  {}

  inline
  void
  parser::by_type::clear ()
  {
    type = empty_symbol;
  }

  inline
  void
  parser::by_type::move (by_type& that)
  {
    type = that.type;
    that.clear ();
  }

  inline
  int
  parser::by_type::type_get () const YY_NOEXCEPT
  {
    return type;
  }

  inline
  parser::token_type
  parser::by_type::token () const YY_NOEXCEPT
  {
    // YYTOKNUM[NUM] -- (External) token number corresponding to the
    // (internal) symbol number NUM (which must be that of a token).  */
    static
    const unsigned short
    yytoken_number_[] =
    {
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395
    };
    return token_type (yytoken_number_[type]);
  }

} // yy
#line 4576 "parser.hh"





#endif // !YY_YY_PARSER_HH_INCLUDED
