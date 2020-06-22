// A Bison parser, made by GNU Bison 3.6.3.

// Skeleton interface for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015, 2018-2020 Free Software Foundation, Inc.

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

// DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
// especially those whose name start with YY_ or yy_.  They are
// private implementation details that can be changed or removed.

#ifndef YY_YY_PARSER_HH_INCLUDED
# define YY_YY_PARSER_HH_INCLUDED
// "%code requires" blocks.
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

  expression_ref make_sig_vars(const std::vector<std::string>& sig_vars);
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
  expression_ref make_mdo(const std::vector<expression_ref>& stmts);
  expression_ref yy_make_tuple(const std::vector<expression_ref>& tup_exprs);

  expression_ref make_list(const std::vector<expression_ref>& items);
  expression_ref make_alts(const std::vector<expression_ref>& alts);
  expression_ref yy_make_alt(const expression_ref& pat, const expression_ref& alt_rhs);

  expression_ref make_stmts(const std::vector<expression_ref>& stmts);

  expression_ref yy_make_string(const std::string&);

#line 108 "parser.hh"

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
#ifndef YY_ASSERT
# include <cassert>
# define YY_ASSERT assert
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
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

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
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
#line 242 "parser.hh"




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
      YY_ASSERT (sizeof (T) <= size);
      new (yyas_<T> ()) T (YY_MOVE (t));
    }

#if 201103L <= YY_CPLUSPLUS
    /// Non copyable.
    semantic_type (const self_type&) = delete;
    /// Non copyable.
    self_type& operator= (const self_type&) = delete;
#endif

    /// Destruction, allowed only if empty.
    ~semantic_type () YY_NOEXCEPT
    {
      YY_ASSERT (!yytypeid_);
    }

# if 201103L <= YY_CPLUSPLUS
    /// Instantiate a \a T in here from \a t.
    template <typename T, typename... U>
    T&
    emplace (U&&... u)
    {
      YY_ASSERT (!yytypeid_);
      YY_ASSERT (sizeof (T) <= size);
      yytypeid_ = & typeid (T);
      return *new (yyas_<T> ()) T (std::forward <U>(u)...);
    }
# else
    /// Instantiate an empty \a T in here.
    template <typename T>
    T&
    emplace ()
    {
      YY_ASSERT (!yytypeid_);
      YY_ASSERT (sizeof (T) <= size);
      yytypeid_ = & typeid (T);
      return *new (yyas_<T> ()) T ();
    }

    /// Instantiate a \a T in here from \a t.
    template <typename T>
    T&
    emplace (const T& t)
    {
      YY_ASSERT (!yytypeid_);
      YY_ASSERT (sizeof (T) <= size);
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
      YY_ASSERT (yytypeid_);
      YY_ASSERT (*yytypeid_ == typeid (T));
      YY_ASSERT (sizeof (T) <= size);
      return *yyas_<T> ();
    }

    /// Const accessor to a built \a T (for %printer).
    template <typename T>
    const T&
    as () const YY_NOEXCEPT
    {
      YY_ASSERT (yytypeid_);
      YY_ASSERT (*yytypeid_ == typeid (T));
      YY_ASSERT (sizeof (T) <= size);
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
      YY_ASSERT (yytypeid_);
      YY_ASSERT (*yytypeid_ == *that.yytypeid_);
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
#if YY_CPLUSPLUS < 201103L
    /// Non copyable.
    semantic_type (const self_type&);
    /// Non copyable.
    self_type& operator= (const self_type&);
#endif

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
      // cl_decl
      // ty_decl
      // inst_decl
      // tycl_hdr
      // decllist
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
      // inst_type
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
      // sig_vars
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

    /// Token kinds.
    struct token
    {
      enum token_kind_type
      {
        TOK_YYEMPTY = -2,
    TOK_END = 0,                   // "end of file"
    TOK_YYerror = 256,             // error
    TOK_YYUNDEF = 257,             // "invalid token"
    TOK_UNDERSCORE = 258,          // "_"
    TOK_AS = 259,                  // "as"
    TOK_CASE = 260,                // "case"
    TOK_CLASS = 261,               // "class"
    TOK_DATA = 262,                // "data"
    TOK_DEFAULT = 263,             // "default"
    TOK_DERIVING = 264,            // "deriving"
    TOK_DO = 265,                  // "do"
    TOK_ELSE = 266,                // "else"
    TOK_HIDING = 267,              // "hiding"
    TOK_IF = 268,                  // "if"
    TOK_IMPORT = 269,              // "import"
    TOK_IN = 270,                  // "in"
    TOK_INFIX = 271,               // "infix"
    TOK_INFIXL = 272,              // "infixl"
    TOK_INFIXR = 273,              // "infixr"
    TOK_INSTANCE = 274,            // "instance"
    TOK_LET = 275,                 // "let"
    TOK_MODULE = 276,              // "module"
    TOK_NEWTYPE = 277,             // "newtype"
    TOK_OF = 278,                  // "of"
    TOK_QUALIFIED = 279,           // "qualified"
    TOK_THEN = 280,                // "then"
    TOK_TYPE = 281,                // "type"
    TOK_WHERE = 282,               // "where"
    TOK_BUILTIN = 283,             // "builtin"
    TOK_FORALL = 284,              // "forall"
    TOK_FOREIGN = 285,             // "foreign"
    TOK_EXPORT = 286,              // "export"
    TOK_LABEL = 287,               // "label"
    TOK_DYNAMIC = 288,             // "dynamic"
    TOK_SAFE = 289,                // "safe"
    TOK_INTERRUPTIBLE = 290,       // "interruptible"
    TOK_UNSAFE = 291,              // "unsafe"
    TOK_MDO = 292,                 // "mdo"
    TOK_FAMILY = 293,              // "family"
    TOK_ROLE = 294,                // "role"
    TOK_STDCALL = 295,             // "stdcall"
    TOK_CCALL = 296,               // "ccall"
    TOK_CAPI = 297,                // "capi"
    TOK_PRIM = 298,                // "prim"
    TOK_JAVASCRIPT = 299,          // "javascript"
    TOK_PROC = 300,                // "proc"
    TOK_REC = 301,                 // "rec"
    TOK_GROUP = 302,               // "group"
    TOK_BY = 303,                  // "by"
    TOK_USING = 304,               // "using"
    TOK_PATTERN = 305,             // "pattern"
    TOK_STATIC = 306,              // "static"
    TOK_STOCK = 307,               // "stock"
    TOK_ANYCLASS = 308,            // "anyclass"
    TOK_VIA = 309,                 // "via"
    TOK_UNIT = 310,                // "unit"
    TOK_SIGNATURE = 311,           // "signature"
    TOK_DEPENDENCY = 312,          // "dependency"
    TOK_INLINE_PRAG = 313,         // "{-# INLINE"
    TOK_SPECIALIZE_PRAG = 314,     // "{-# SPECIALIZE"
    TOK_SPECIALIZE_INLINE_PRAG = 315, // "{-# SPECIALIZE_INLINE"
    TOK_SOURCE_PRAG = 316,         // "{-# SOURCE"
    TOK_RULES_PRAG = 317,          // "{-# RULES"
    TOK_CORE_PRAG = 318,           // "{-# CORE"
    TOK_SCC_PRAG = 319,            // "{-# SCC"
    TOK_GENERATED_PRAG = 320,      // "{-# GENERATED"
    TOK_DEPRECATED_PRAG = 321,     // "{-# DEPRECATED"
    TOK_WARNING_PRAG = 322,        // "{-# WARNING"
    TOK_UNPACK_PRAG = 323,         // "{-# UNPACK"
    TOK_NOUNPACK_PRAG = 324,       // "{-# NOUNPACK"
    TOK_ANN_PRAG = 325,            // "{-# ANN"
    TOK_MINIMAL_PRAG = 326,        // "{-# MINIMAL"
    TOK_CTYPE_PRAG = 327,          // "{-# CTYPE"
    TOK_OVERLAPPING_PRAG = 328,    // "{-# OVERLAPPING"
    TOK_OVERLAPPABLE_PRAG = 329,   // "{-# OVERLAPPABLE"
    TOK_OVERLAPS_PRAG = 330,       // "{-# OVERLAPS"
    TOK_INCOHERENT_PRAG = 331,     // "{-# INCOHERENT"
    TOK_COMPLETE_PRAG = 332,       // "{-# COMPLETE"
    TOK_CLOSE_PRAG = 333,          // "#-}"
    TOK_DOTDOT = 334,              // ".."
    TOK_COLON = 335,               // ":"
    TOK_DCOLON = 336,              // "::"
    TOK_EQUAL = 337,               // "="
    TOK_LAM = 338,                 // "\\"
    TOK_LCASE = 339,               // "lcase"
    TOK_VBAR = 340,                // "|"
    TOK_LARROW = 341,              // "<-"
    TOK_RARROW = 342,              // "->"
    TOK_AT = 343,                  // "@"
    TOK_TILDE = 344,               // "~"
    TOK_DARROW = 345,              // "=>"
    TOK_MINUS = 346,               // "-"
    TOK_BANG = 347,                // "!"
    TOK_STAR = 348,                // "*"
    TOK_lARROWTAIL = 349,          // "-<"
    TOK_rARROWTAIL = 350,          // ">-"
    TOK_LARROWTAIL = 351,          // "-<<"
    TOK_RARROWTAIL = 352,          // ">>-"
    TOK_DOT = 353,                 // "."
    TOK_TYPEAPP = 354,             // "TYPEAPP"
    TOK_OCURLY = 355,              // "{"
    TOK_CCURLY = 356,              // "}"
    TOK_VOCURLY = 357,             // "vocurly"
    TOK_VCCURLY = 358,             // "vccurly"
    TOK_OBRACK = 359,              // "["
    TOK_CBRACK = 360,              // "]"
    TOK_OPABRACK = 361,            // "[:"
    TOK_CPABRACK = 362,            // ":]"
    TOK_OPAREN = 363,              // "("
    TOK_CPAREN = 364,              // ")"
    TOK_OUBXPAREN = 365,           // "(#"
    TOK_CUBXPAREN = 366,           // "#)"
    TOK_OPARENBAR = 367,           // "(|"
    TOK_CPARENBAR = 368,           // "|)"
    TOK_SEMI = 369,                // ";"
    TOK_COMMA = 370,               // ","
    TOK_BACKQUOTE = 371,           // "`"
    TOK_SIMPLEQUOTE = 372,         // "'"
    TOK_VARID = 373,               // "VARID"
    TOK_CONID = 374,               // "CONID"
    TOK_VARSYM = 375,              // "VARSYM"
    TOK_CONSYM = 376,              // "CONSYM"
    TOK_QVARID = 377,              // "QVARID"
    TOK_QCONID = 378,              // "QCONID"
    TOK_QVARSYM = 379,             // "QVARSYM"
    TOK_QCONSYM = 380,             // "QCONSYM"
    TOK_IPDUPVARID = 381,          // "IPDUPVARID"
    TOK_LABELVARID = 382,          // "LABELVARID"
    TOK_CHAR = 383,                // "CHAR"
    TOK_STRING = 384,              // "STRING"
    TOK_INTEGER = 385,             // "INTEGER"
    TOK_RATIONAL = 386,            // "RATIONAL"
    TOK_PRIMCHAR = 387,            // "PRIMCHAR"
    TOK_PRIMSTRING = 388,          // "PRIMSTRING"
    TOK_PRIMINTEGER = 389,         // "PRIMINTEGER"
    TOK_PRINTWORD = 390,           // "PRIMWORD"
    TOK_PRIMFLOAT = 391,           // "PRIMFLOAT"
    TOK_PRIMDOUBLE = 392           // "PRIMDOUBLE"
      };
      /// Backward compatibility alias (Bison 3.6).
      typedef token_kind_type yytokentype;
    };

    /// Token kind, as returned by yylex.
    typedef token::yytokentype token_kind_type;

    /// Backward compatibility alias (Bison 3.6).
    typedef token_kind_type token_type;

    /// Symbol kinds.
    struct symbol_kind
    {
      enum symbol_kind_type
      {
        YYNTOKENS = 141, ///< Number of tokens.
        S_YYEMPTY = -2,
        S_YYEOF = 0,                             // "end of file"
        S_YYerror = 1,                           // error
        S_YYUNDEF = 2,                           // "invalid token"
        S_UNDERSCORE = 3,                        // "_"
        S_AS = 4,                                // "as"
        S_CASE = 5,                              // "case"
        S_CLASS = 6,                             // "class"
        S_DATA = 7,                              // "data"
        S_DEFAULT = 8,                           // "default"
        S_DERIVING = 9,                          // "deriving"
        S_DO = 10,                               // "do"
        S_ELSE = 11,                             // "else"
        S_HIDING = 12,                           // "hiding"
        S_IF = 13,                               // "if"
        S_IMPORT = 14,                           // "import"
        S_IN = 15,                               // "in"
        S_INFIX = 16,                            // "infix"
        S_INFIXL = 17,                           // "infixl"
        S_INFIXR = 18,                           // "infixr"
        S_INSTANCE = 19,                         // "instance"
        S_LET = 20,                              // "let"
        S_MODULE = 21,                           // "module"
        S_NEWTYPE = 22,                          // "newtype"
        S_OF = 23,                               // "of"
        S_QUALIFIED = 24,                        // "qualified"
        S_THEN = 25,                             // "then"
        S_TYPE = 26,                             // "type"
        S_WHERE = 27,                            // "where"
        S_BUILTIN = 28,                          // "builtin"
        S_FORALL = 29,                           // "forall"
        S_FOREIGN = 30,                          // "foreign"
        S_EXPORT = 31,                           // "export"
        S_LABEL = 32,                            // "label"
        S_DYNAMIC = 33,                          // "dynamic"
        S_SAFE = 34,                             // "safe"
        S_INTERRUPTIBLE = 35,                    // "interruptible"
        S_UNSAFE = 36,                           // "unsafe"
        S_MDO = 37,                              // "mdo"
        S_FAMILY = 38,                           // "family"
        S_ROLE = 39,                             // "role"
        S_STDCALL = 40,                          // "stdcall"
        S_CCALL = 41,                            // "ccall"
        S_CAPI = 42,                             // "capi"
        S_PRIM = 43,                             // "prim"
        S_JAVASCRIPT = 44,                       // "javascript"
        S_PROC = 45,                             // "proc"
        S_REC = 46,                              // "rec"
        S_GROUP = 47,                            // "group"
        S_BY = 48,                               // "by"
        S_USING = 49,                            // "using"
        S_PATTERN = 50,                          // "pattern"
        S_STATIC = 51,                           // "static"
        S_STOCK = 52,                            // "stock"
        S_ANYCLASS = 53,                         // "anyclass"
        S_VIA = 54,                              // "via"
        S_UNIT = 55,                             // "unit"
        S_SIGNATURE = 56,                        // "signature"
        S_DEPENDENCY = 57,                       // "dependency"
        S_INLINE_PRAG = 58,                      // "{-# INLINE"
        S_SPECIALIZE_PRAG = 59,                  // "{-# SPECIALIZE"
        S_SPECIALIZE_INLINE_PRAG = 60,           // "{-# SPECIALIZE_INLINE"
        S_SOURCE_PRAG = 61,                      // "{-# SOURCE"
        S_RULES_PRAG = 62,                       // "{-# RULES"
        S_CORE_PRAG = 63,                        // "{-# CORE"
        S_SCC_PRAG = 64,                         // "{-# SCC"
        S_GENERATED_PRAG = 65,                   // "{-# GENERATED"
        S_DEPRECATED_PRAG = 66,                  // "{-# DEPRECATED"
        S_WARNING_PRAG = 67,                     // "{-# WARNING"
        S_UNPACK_PRAG = 68,                      // "{-# UNPACK"
        S_NOUNPACK_PRAG = 69,                    // "{-# NOUNPACK"
        S_ANN_PRAG = 70,                         // "{-# ANN"
        S_MINIMAL_PRAG = 71,                     // "{-# MINIMAL"
        S_CTYPE_PRAG = 72,                       // "{-# CTYPE"
        S_OVERLAPPING_PRAG = 73,                 // "{-# OVERLAPPING"
        S_OVERLAPPABLE_PRAG = 74,                // "{-# OVERLAPPABLE"
        S_OVERLAPS_PRAG = 75,                    // "{-# OVERLAPS"
        S_INCOHERENT_PRAG = 76,                  // "{-# INCOHERENT"
        S_COMPLETE_PRAG = 77,                    // "{-# COMPLETE"
        S_CLOSE_PRAG = 78,                       // "#-}"
        S_DOTDOT = 79,                           // ".."
        S_COLON = 80,                            // ":"
        S_DCOLON = 81,                           // "::"
        S_EQUAL = 82,                            // "="
        S_LAM = 83,                              // "\\"
        S_LCASE = 84,                            // "lcase"
        S_VBAR = 85,                             // "|"
        S_LARROW = 86,                           // "<-"
        S_RARROW = 87,                           // "->"
        S_AT = 88,                               // "@"
        S_TILDE = 89,                            // "~"
        S_DARROW = 90,                           // "=>"
        S_MINUS = 91,                            // "-"
        S_BANG = 92,                             // "!"
        S_STAR = 93,                             // "*"
        S_lARROWTAIL = 94,                       // "-<"
        S_rARROWTAIL = 95,                       // ">-"
        S_LARROWTAIL = 96,                       // "-<<"
        S_RARROWTAIL = 97,                       // ">>-"
        S_DOT = 98,                              // "."
        S_TYPEAPP = 99,                          // "TYPEAPP"
        S_OCURLY = 100,                          // "{"
        S_CCURLY = 101,                          // "}"
        S_VOCURLY = 102,                         // "vocurly"
        S_VCCURLY = 103,                         // "vccurly"
        S_OBRACK = 104,                          // "["
        S_CBRACK = 105,                          // "]"
        S_OPABRACK = 106,                        // "[:"
        S_CPABRACK = 107,                        // ":]"
        S_OPAREN = 108,                          // "("
        S_CPAREN = 109,                          // ")"
        S_OUBXPAREN = 110,                       // "(#"
        S_CUBXPAREN = 111,                       // "#)"
        S_OPARENBAR = 112,                       // "(|"
        S_CPARENBAR = 113,                       // "|)"
        S_SEMI = 114,                            // ";"
        S_COMMA = 115,                           // ","
        S_BACKQUOTE = 116,                       // "`"
        S_SIMPLEQUOTE = 117,                     // "'"
        S_VARID = 118,                           // "VARID"
        S_CONID = 119,                           // "CONID"
        S_VARSYM = 120,                          // "VARSYM"
        S_CONSYM = 121,                          // "CONSYM"
        S_QVARID = 122,                          // "QVARID"
        S_QCONID = 123,                          // "QCONID"
        S_QVARSYM = 124,                         // "QVARSYM"
        S_QCONSYM = 125,                         // "QCONSYM"
        S_IPDUPVARID = 126,                      // "IPDUPVARID"
        S_LABELVARID = 127,                      // "LABELVARID"
        S_CHAR = 128,                            // "CHAR"
        S_STRING = 129,                          // "STRING"
        S_INTEGER = 130,                         // "INTEGER"
        S_RATIONAL = 131,                        // "RATIONAL"
        S_PRIMCHAR = 132,                        // "PRIMCHAR"
        S_PRIMSTRING = 133,                      // "PRIMSTRING"
        S_PRIMINTEGER = 134,                     // "PRIMINTEGER"
        S_PRINTWORD = 135,                       // "PRIMWORD"
        S_PRIMFLOAT = 136,                       // "PRIMFLOAT"
        S_PRIMDOUBLE = 137,                      // "PRIMDOUBLE"
        S_138_ = 138,                            // "#-"
        S_139_SPECIALISE_ = 139,                 // "{-# SPECIALISE"
        S_140_SPECIALISE_INLINE_ = 140,          // "{-# SPECIALISE_INLINE"
        S_YYACCEPT = 141,                        // $accept
        S_unit = 142,                            // unit
        S_module = 143,                          // module
        S_missing_module_keyword = 144,          // missing_module_keyword
        S_maybemodwarning = 145,                 // maybemodwarning
        S_body = 146,                            // body
        S_body2 = 147,                           // body2
        S_top = 148,                             // top
        S_top1 = 149,                            // top1
        S_maybeexports = 150,                    // maybeexports
        S_exportlist = 151,                      // exportlist
        S_exportlist1 = 152,                     // exportlist1
        S_export = 153,                          // export
        S_export_subspec = 154,                  // export_subspec
        S_qcnames = 155,                         // qcnames
        S_qcnames1 = 156,                        // qcnames1
        S_qcname_ext_w_wildcard = 157,           // qcname_ext_w_wildcard
        S_qcname_ext = 158,                      // qcname_ext
        S_qcname = 159,                          // qcname
        S_semis1 = 160,                          // semis1
        S_semis = 161,                           // semis
        S_importdecls = 162,                     // importdecls
        S_importdecls_semi = 163,                // importdecls_semi
        S_importdecl = 164,                      // importdecl
        S_maybe_src = 165,                       // maybe_src
        S_maybe_safe = 166,                      // maybe_safe
        S_maybe_pkg = 167,                       // maybe_pkg
        S_optqualified = 168,                    // optqualified
        S_maybeas = 169,                         // maybeas
        S_maybeimpspec = 170,                    // maybeimpspec
        S_impspec = 171,                         // impspec
        S_prec = 172,                            // prec
        S_infix = 173,                           // infix
        S_ops = 174,                             // ops
        S_topdecls = 175,                        // topdecls
        S_topdecls_semi = 176,                   // topdecls_semi
        S_topdecl = 177,                         // topdecl
        S_cl_decl = 178,                         // cl_decl
        S_ty_decl = 179,                         // ty_decl
        S_inst_decl = 180,                       // inst_decl
        S_overlap_pragma = 181,                  // overlap_pragma
        S_deriv_strategy_no_via = 182,           // deriv_strategy_no_via
        S_deriv_strategy_via = 183,              // deriv_strategy_via
        S_data_or_newtype = 184,                 // data_or_newtype
        S_opt_kind_sig = 185,                    // opt_kind_sig
        S_tycl_hdr = 186,                        // tycl_hdr
        S_capi_ctype = 187,                      // capi_ctype
        S_pattern_synonym_decl = 188,            // pattern_synonym_decl
        S_pattern_synonym_lhs = 189,             // pattern_synonym_lhs
        S_vars0 = 190,                           // vars0
        S_cvars1 = 191,                          // cvars1
        S_where_decls = 192,                     // where_decls
        S_pattern_synonym_sig = 193,             // pattern_synonym_sig
        S_decls = 194,                           // decls
        S_decllist = 195,                        // decllist
        S_binds = 196,                           // binds
        S_wherebinds = 197,                      // wherebinds
        S_strings = 198,                         // strings
        S_stringlist = 199,                      // stringlist
        S_opt_sig = 200,                         // opt_sig
        S_opt_tyconsig = 201,                    // opt_tyconsig
        S_sigtype = 202,                         // sigtype
        S_sigtypedoc = 203,                      // sigtypedoc
        S_sig_vars = 204,                        // sig_vars
        S_sigtypes1 = 205,                       // sigtypes1
        S_strict_mark = 206,                     // strict_mark
        S_strictness = 207,                      // strictness
        S_unpackedness = 208,                    // unpackedness
        S_ctype = 209,                           // ctype
        S_ctypedoc = 210,                        // ctypedoc
        S_context = 211,                         // context
        S_context_no_ops = 212,                  // context_no_ops
        S_type = 213,                            // type
        S_typedoc = 214,                         // typedoc
        S_btype = 215,                           // btype
        S_btype_no_ops = 216,                    // btype_no_ops
        S_tyapps = 217,                          // tyapps
        S_tyapp = 218,                           // tyapp
        S_atype_docs = 219,                      // atype_docs
        S_atype = 220,                           // atype
        S_inst_type = 221,                       // inst_type
        S_deriv_types = 222,                     // deriv_types
        S_comma_types0 = 223,                    // comma_types0
        S_comma_types1 = 224,                    // comma_types1
        S_bar_types2 = 225,                      // bar_types2
        S_tv_bndrs = 226,                        // tv_bndrs
        S_tv_bndr = 227,                         // tv_bndr
        S_kind = 228,                            // kind
        S_constrs = 229,                         // constrs
        S_constrs1 = 230,                        // constrs1
        S_constr = 231,                          // constr
        S_forall = 232,                          // forall
        S_constr_stuff = 233,                    // constr_stuff
        S_fielddecls = 234,                      // fielddecls
        S_fielddecls1 = 235,                     // fielddecls1
        S_fielddecl = 236,                       // fielddecl
        S_maybe_derivings = 237,                 // maybe_derivings
        S_derivings = 238,                       // derivings
        S_deriving = 239,                        // deriving
        S_deriv_clause_types = 240,              // deriv_clause_types
        S_decl_no_th = 241,                      // decl_no_th
        S_decl = 242,                            // decl
        S_rhs = 243,                             // rhs
        S_gdrhs = 244,                           // gdrhs
        S_gdrh = 245,                            // gdrh
        S_sigdecl = 246,                         // sigdecl
        S_activation = 247,                      // activation
        S_explicit_activation = 248,             // explicit_activation
        S_exp = 249,                             // exp
        S_infixexp = 250,                        // infixexp
        S_infixexp_top = 251,                    // infixexp_top
        S_exp10_top = 252,                       // exp10_top
        S_exp10 = 253,                           // exp10
        S_optSemi = 254,                         // optSemi
        S_scc_annot = 255,                       // scc_annot
        S_fexp = 256,                            // fexp
        S_aexp = 257,                            // aexp
        S_aexp1 = 258,                           // aexp1
        S_aexp2 = 259,                           // aexp2
        S_texp = 260,                            // texp
        S_tup_exprs = 261,                       // tup_exprs
        S_list = 262,                            // list
        S_lexps = 263,                           // lexps
        S_squals = 264,                          // squals
        S_transformqual = 265,                   // transformqual
        S_guardquals = 266,                      // guardquals
        S_guardquals1 = 267,                     // guardquals1
        S_altslist = 268,                        // altslist
        S_alts = 269,                            // alts
        S_alts1 = 270,                           // alts1
        S_alt = 271,                             // alt
        S_alt_rhs = 272,                         // alt_rhs
        S_gdpats = 273,                          // gdpats
        S_ifgdpats = 274,                        // ifgdpats
        S_gdpat = 275,                           // gdpat
        S_pat = 276,                             // pat
        S_bindpat = 277,                         // bindpat
        S_apat = 278,                            // apat
        S_apats1 = 279,                          // apats1
        S_stmtlist = 280,                        // stmtlist
        S_stmts = 281,                           // stmts
        S_stmt = 282,                            // stmt
        S_qual = 283,                            // qual
        S_fbinds = 284,                          // fbinds
        S_fbinds1 = 285,                         // fbinds1
        S_fbind = 286,                           // fbind
        S_qcon = 287,                            // qcon
        S_gen_qcon = 288,                        // gen_qcon
        S_con = 289,                             // con
        S_con_list = 290,                        // con_list
        S_sysdcon_no_list = 291,                 // sysdcon_no_list
        S_sysdcon = 292,                         // sysdcon
        S_conop = 293,                           // conop
        S_qconop = 294,                          // qconop
        S_gtycon = 295,                          // gtycon
        S_ntgtycon = 296,                        // ntgtycon
        S_oqtycon = 297,                         // oqtycon
        S_oqtycon_no_varcon = 298,               // oqtycon_no_varcon
        S_qtyconop = 299,                        // qtyconop
        S_qtycondoc = 300,                       // qtycondoc
        S_qtycon = 301,                          // qtycon
        S_tycon = 302,                           // tycon
        S_qtyconsym = 303,                       // qtyconsym
        S_tyconsym = 304,                        // tyconsym
        S_op = 305,                              // op
        S_varop = 306,                           // varop
        S_qop = 307,                             // qop
        S_qopm = 308,                            // qopm
        S_hole_op = 309,                         // hole_op
        S_qvarop = 310,                          // qvarop
        S_qvaropm = 311,                         // qvaropm
        S_tyvar = 312,                           // tyvar
        S_tyvarop = 313,                         // tyvarop
        S_tyvarid = 314,                         // tyvarid
        S_var = 315,                             // var
        S_qvar = 316,                            // qvar
        S_qvarid = 317,                          // qvarid
        S_varid = 318,                           // varid
        S_qvarsym = 319,                         // qvarsym
        S_qvarsym_no_minus = 320,                // qvarsym_no_minus
        S_qvarsym1 = 321,                        // qvarsym1
        S_varsym = 322,                          // varsym
        S_varsym_no_minus = 323,                 // varsym_no_minus
        S_special_id = 324,                      // special_id
        S_special_sym = 325,                     // special_sym
        S_qconid = 326,                          // qconid
        S_conid = 327,                           // conid
        S_qconsym = 328,                         // qconsym
        S_consym = 329,                          // consym
        S_literal = 330,                         // literal
        S_close = 331,                           // close
        S_modid = 332,                           // modid
        S_commas = 333                           // commas
      };
    };

    /// (Internal) symbol kind.
    typedef symbol_kind::symbol_kind_type symbol_kind_type;

    /// The number of tokens.
    static const symbol_kind_type YYNTOKENS = symbol_kind::YYNTOKENS;

    /// A complete symbol.
    ///
    /// Expects its Base type to provide access to the symbol kind
    /// via kind ().
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
      basic_symbol (basic_symbol&& that)
        : Base (std::move (that))
        , value ()
        , location (std::move (that.location))
      {
        switch (this->kind ())
    {
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        value.move< bool > (std::move (that.value));
        break;

      case 128: // "CHAR"
      case 132: // "PRIMCHAR"
        value.move< char > (std::move (that.value));
        break;

      case 131: // "RATIONAL"
      case 137: // "PRIMDOUBLE"
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
      case 178: // cl_decl
      case 179: // ty_decl
      case 180: // inst_decl
      case 186: // tycl_hdr
      case 195: // decllist
      case 196: // binds
      case 197: // wherebinds
      case 200: // opt_sig
      case 201: // opt_tyconsig
      case 202: // sigtype
      case 203: // sigtypedoc
      case 209: // ctype
      case 210: // ctypedoc
      case 211: // context
      case 212: // context_no_ops
      case 213: // type
      case 214: // typedoc
      case 215: // btype
      case 218: // tyapp
      case 219: // atype_docs
      case 220: // atype
      case 221: // inst_type
      case 227: // tv_bndr
      case 228: // kind
      case 231: // constr
      case 232: // forall
      case 233: // constr_stuff
      case 236: // fielddecl
      case 241: // decl_no_th
      case 242: // decl
      case 243: // rhs
      case 245: // gdrh
      case 246: // sigdecl
      case 249: // exp
      case 252: // exp10_top
      case 253: // exp10
      case 257: // aexp
      case 258: // aexp1
      case 259: // aexp2
      case 260: // texp
      case 262: // list
      case 265: // transformqual
      case 271: // alt
      case 272: // alt_rhs
      case 274: // ifgdpats
      case 275: // gdpat
      case 276: // pat
      case 277: // bindpat
      case 278: // apat
      case 282: // stmt
      case 283: // qual
      case 330: // literal
        value.move< expression_ref > (std::move (that.value));
        break;

      case 136: // "PRIMFLOAT"
        value.move< float > (std::move (that.value));
        break;

      case 130: // "INTEGER"
      case 134: // "PRIMINTEGER"
      case 135: // "PRIMWORD"
      case 333: // commas
        value.move< int > (std::move (that.value));
        break;

      case 172: // prec
        value.move< std::optional<int> > (std::move (that.value));
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.move< std::optional<std::string> > (std::move (that.value));
        break;

      case 118: // "VARID"
      case 119: // "CONID"
      case 120: // "VARSYM"
      case 121: // "CONSYM"
      case 122: // "QVARID"
      case 123: // "QCONID"
      case 124: // "QVARSYM"
      case 125: // "QCONSYM"
      case 126: // "IPDUPVARID"
      case 127: // "LABELVARID"
      case 129: // "STRING"
      case 133: // "PRIMSTRING"
      case 173: // infix
      case 184: // data_or_newtype
      case 206: // strict_mark
      case 207: // strictness
      case 287: // qcon
      case 288: // gen_qcon
      case 289: // con
      case 291: // sysdcon_no_list
      case 292: // sysdcon
      case 293: // conop
      case 294: // qconop
      case 295: // gtycon
      case 296: // ntgtycon
      case 297: // oqtycon
      case 298: // oqtycon_no_varcon
      case 299: // qtyconop
      case 300: // qtycondoc
      case 301: // qtycon
      case 302: // tycon
      case 303: // qtyconsym
      case 304: // tyconsym
      case 305: // op
      case 306: // varop
      case 307: // qop
      case 308: // qopm
      case 309: // hole_op
      case 310: // qvarop
      case 311: // qvaropm
      case 312: // tyvar
      case 313: // tyvarop
      case 314: // tyvarid
      case 315: // var
      case 316: // qvar
      case 317: // qvarid
      case 318: // varid
      case 319: // qvarsym
      case 320: // qvarsym_no_minus
      case 321: // qvarsym1
      case 322: // varsym
      case 323: // varsym_no_minus
      case 324: // special_id
      case 325: // special_sym
      case 326: // qconid
      case 327: // conid
      case 328: // qconsym
      case 329: // consym
      case 332: // modid
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
      case 194: // decls
      case 205: // sigtypes1
      case 216: // btype_no_ops
      case 217: // tyapps
      case 223: // comma_types0
      case 224: // comma_types1
      case 226: // tv_bndrs
      case 229: // constrs
      case 230: // constrs1
      case 234: // fielddecls
      case 235: // fielddecls1
      case 244: // gdrhs
      case 250: // infixexp
      case 251: // infixexp_top
      case 256: // fexp
      case 261: // tup_exprs
      case 263: // lexps
      case 264: // squals
      case 266: // guardquals
      case 267: // guardquals1
      case 268: // altslist
      case 269: // alts
      case 270: // alts1
      case 273: // gdpats
      case 279: // apats1
      case 280: // stmtlist
      case 281: // stmts
        value.move< std::vector<expression_ref> > (std::move (that.value));
        break;

      case 174: // ops
      case 204: // sig_vars
        value.move< std::vector<std::string> > (std::move (that.value));
        break;

      default:
        break;
    }

      }
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
        symbol_kind_type yykind = this->kind ();
        basic_symbol<Base>& yysym = *this;
        (void) yysym;
        switch (yykind)
        {
       default:
          break;
        }

        // Value type destructor.
switch (yykind)
    {
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        value.template destroy< bool > ();
        break;

      case 128: // "CHAR"
      case 132: // "PRIMCHAR"
        value.template destroy< char > ();
        break;

      case 131: // "RATIONAL"
      case 137: // "PRIMDOUBLE"
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
      case 178: // cl_decl
      case 179: // ty_decl
      case 180: // inst_decl
      case 186: // tycl_hdr
      case 195: // decllist
      case 196: // binds
      case 197: // wherebinds
      case 200: // opt_sig
      case 201: // opt_tyconsig
      case 202: // sigtype
      case 203: // sigtypedoc
      case 209: // ctype
      case 210: // ctypedoc
      case 211: // context
      case 212: // context_no_ops
      case 213: // type
      case 214: // typedoc
      case 215: // btype
      case 218: // tyapp
      case 219: // atype_docs
      case 220: // atype
      case 221: // inst_type
      case 227: // tv_bndr
      case 228: // kind
      case 231: // constr
      case 232: // forall
      case 233: // constr_stuff
      case 236: // fielddecl
      case 241: // decl_no_th
      case 242: // decl
      case 243: // rhs
      case 245: // gdrh
      case 246: // sigdecl
      case 249: // exp
      case 252: // exp10_top
      case 253: // exp10
      case 257: // aexp
      case 258: // aexp1
      case 259: // aexp2
      case 260: // texp
      case 262: // list
      case 265: // transformqual
      case 271: // alt
      case 272: // alt_rhs
      case 274: // ifgdpats
      case 275: // gdpat
      case 276: // pat
      case 277: // bindpat
      case 278: // apat
      case 282: // stmt
      case 283: // qual
      case 330: // literal
        value.template destroy< expression_ref > ();
        break;

      case 136: // "PRIMFLOAT"
        value.template destroy< float > ();
        break;

      case 130: // "INTEGER"
      case 134: // "PRIMINTEGER"
      case 135: // "PRIMWORD"
      case 333: // commas
        value.template destroy< int > ();
        break;

      case 172: // prec
        value.template destroy< std::optional<int> > ();
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.template destroy< std::optional<std::string> > ();
        break;

      case 118: // "VARID"
      case 119: // "CONID"
      case 120: // "VARSYM"
      case 121: // "CONSYM"
      case 122: // "QVARID"
      case 123: // "QCONID"
      case 124: // "QVARSYM"
      case 125: // "QCONSYM"
      case 126: // "IPDUPVARID"
      case 127: // "LABELVARID"
      case 129: // "STRING"
      case 133: // "PRIMSTRING"
      case 173: // infix
      case 184: // data_or_newtype
      case 206: // strict_mark
      case 207: // strictness
      case 287: // qcon
      case 288: // gen_qcon
      case 289: // con
      case 291: // sysdcon_no_list
      case 292: // sysdcon
      case 293: // conop
      case 294: // qconop
      case 295: // gtycon
      case 296: // ntgtycon
      case 297: // oqtycon
      case 298: // oqtycon_no_varcon
      case 299: // qtyconop
      case 300: // qtycondoc
      case 301: // qtycon
      case 302: // tycon
      case 303: // qtyconsym
      case 304: // tyconsym
      case 305: // op
      case 306: // varop
      case 307: // qop
      case 308: // qopm
      case 309: // hole_op
      case 310: // qvarop
      case 311: // qvaropm
      case 312: // tyvar
      case 313: // tyvarop
      case 314: // tyvarid
      case 315: // var
      case 316: // qvar
      case 317: // qvarid
      case 318: // varid
      case 319: // qvarsym
      case 320: // qvarsym_no_minus
      case 321: // qvarsym1
      case 322: // varsym
      case 323: // varsym_no_minus
      case 324: // special_id
      case 325: // special_sym
      case 326: // qconid
      case 327: // conid
      case 328: // qconsym
      case 329: // consym
      case 332: // modid
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
      case 194: // decls
      case 205: // sigtypes1
      case 216: // btype_no_ops
      case 217: // tyapps
      case 223: // comma_types0
      case 224: // comma_types1
      case 226: // tv_bndrs
      case 229: // constrs
      case 230: // constrs1
      case 234: // fielddecls
      case 235: // fielddecls1
      case 244: // gdrhs
      case 250: // infixexp
      case 251: // infixexp_top
      case 256: // fexp
      case 261: // tup_exprs
      case 263: // lexps
      case 264: // squals
      case 266: // guardquals
      case 267: // guardquals1
      case 268: // altslist
      case 269: // alts
      case 270: // alts1
      case 273: // gdpats
      case 279: // apats1
      case 280: // stmtlist
      case 281: // stmts
        value.template destroy< std::vector<expression_ref> > ();
        break;

      case 174: // ops
      case 204: // sig_vars
        value.template destroy< std::vector<std::string> > ();
        break;

      default:
        break;
    }

        Base::clear ();
      }

      /// The user-facing name of this symbol.
      std::string name () const YY_NOEXCEPT
      {
        return parser::symbol_name (this->kind ());
      }

      /// Backward compatibility (Bison 3.6).
      symbol_kind_type type_get () const YY_NOEXCEPT;

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
    struct by_kind
    {
      /// Default constructor.
      by_kind ();

#if 201103L <= YY_CPLUSPLUS
      /// Move constructor.
      by_kind (by_kind&& that);
#endif

      /// Copy constructor.
      by_kind (const by_kind& that);

      /// The symbol kind as needed by the constructor.
      typedef token_kind_type kind_type;

      /// Constructor from (external) token numbers.
      by_kind (kind_type t);

      /// Record that this symbol is empty.
      void clear ();

      /// Steal the symbol kind from \a that.
      void move (by_kind& that);

      /// The (internal) type number (corresponding to \a type).
      /// \a empty when empty.
      symbol_kind_type kind () const YY_NOEXCEPT;

      /// Backward compatibility (Bison 3.6).
      symbol_kind_type type_get () const YY_NOEXCEPT;

      /// The symbol kind.
      /// \a S_YYEMPTY when empty.
      symbol_kind_type kind_;
    };

    /// Backward compatibility for a private implementation detail (Bison 3.6).
    typedef by_kind by_type;

    /// "External" symbols: returned by the scanner.
    struct symbol_type : basic_symbol<by_kind>
    {
      /// Superclass.
      typedef basic_symbol<by_kind> super_type;

      /// Empty symbol.
      symbol_type () {}

      /// Constructor for valueless symbols, and symbols from each type.
#if 201103L <= YY_CPLUSPLUS
      symbol_type (int tok, location_type l)
        : super_type(token_type (tok), std::move (l))
      {
        YY_ASSERT (tok == token::TOK_END || tok == token::TOK_YYerror || tok == token::TOK_YYUNDEF || tok == token::TOK_UNDERSCORE || tok == token::TOK_AS || tok == token::TOK_CASE || tok == token::TOK_CLASS || tok == token::TOK_DATA || tok == token::TOK_DEFAULT || tok == token::TOK_DERIVING || tok == token::TOK_DO || tok == token::TOK_ELSE || tok == token::TOK_HIDING || tok == token::TOK_IF || tok == token::TOK_IMPORT || tok == token::TOK_IN || tok == token::TOK_INFIX || tok == token::TOK_INFIXL || tok == token::TOK_INFIXR || tok == token::TOK_INSTANCE || tok == token::TOK_LET || tok == token::TOK_MODULE || tok == token::TOK_NEWTYPE || tok == token::TOK_OF || tok == token::TOK_QUALIFIED || tok == token::TOK_THEN || tok == token::TOK_TYPE || tok == token::TOK_WHERE || tok == token::TOK_BUILTIN || tok == token::TOK_FORALL || tok == token::TOK_FOREIGN || tok == token::TOK_EXPORT || tok == token::TOK_LABEL || tok == token::TOK_DYNAMIC || tok == token::TOK_SAFE || tok == token::TOK_INTERRUPTIBLE || tok == token::TOK_UNSAFE || tok == token::TOK_MDO || tok == token::TOK_FAMILY || tok == token::TOK_ROLE || tok == token::TOK_STDCALL || tok == token::TOK_CCALL || tok == token::TOK_CAPI || tok == token::TOK_PRIM || tok == token::TOK_JAVASCRIPT || tok == token::TOK_PROC || tok == token::TOK_REC || tok == token::TOK_GROUP || tok == token::TOK_BY || tok == token::TOK_USING || tok == token::TOK_PATTERN || tok == token::TOK_STATIC || tok == token::TOK_STOCK || tok == token::TOK_ANYCLASS || tok == token::TOK_VIA || tok == token::TOK_UNIT || tok == token::TOK_SIGNATURE || tok == token::TOK_DEPENDENCY || tok == token::TOK_INLINE_PRAG || tok == token::TOK_SPECIALIZE_PRAG || tok == token::TOK_SPECIALIZE_INLINE_PRAG || tok == token::TOK_SOURCE_PRAG || tok == token::TOK_RULES_PRAG || tok == token::TOK_CORE_PRAG || tok == token::TOK_SCC_PRAG || tok == token::TOK_GENERATED_PRAG || tok == token::TOK_DEPRECATED_PRAG || tok == token::TOK_WARNING_PRAG || tok == token::TOK_UNPACK_PRAG || tok == token::TOK_NOUNPACK_PRAG || tok == token::TOK_ANN_PRAG || tok == token::TOK_MINIMAL_PRAG || tok == token::TOK_CTYPE_PRAG || tok == token::TOK_OVERLAPPING_PRAG || tok == token::TOK_OVERLAPPABLE_PRAG || tok == token::TOK_OVERLAPS_PRAG || tok == token::TOK_INCOHERENT_PRAG || tok == token::TOK_COMPLETE_PRAG || tok == token::TOK_CLOSE_PRAG || tok == token::TOK_DOTDOT || tok == token::TOK_COLON || tok == token::TOK_DCOLON || tok == token::TOK_EQUAL || tok == token::TOK_LAM || tok == token::TOK_LCASE || tok == token::TOK_VBAR || tok == token::TOK_LARROW || tok == token::TOK_RARROW || tok == token::TOK_AT || tok == token::TOK_TILDE || tok == token::TOK_DARROW || tok == token::TOK_MINUS || tok == token::TOK_BANG || tok == token::TOK_STAR || tok == token::TOK_lARROWTAIL || tok == token::TOK_rARROWTAIL || tok == token::TOK_LARROWTAIL || tok == token::TOK_RARROWTAIL || tok == token::TOK_DOT || tok == token::TOK_TYPEAPP || tok == token::TOK_OCURLY || tok == token::TOK_CCURLY || tok == token::TOK_VOCURLY || tok == token::TOK_VCCURLY || tok == token::TOK_OBRACK || tok == token::TOK_CBRACK || tok == token::TOK_OPABRACK || tok == token::TOK_CPABRACK || tok == token::TOK_OPAREN || tok == token::TOK_CPAREN || tok == token::TOK_OUBXPAREN || tok == token::TOK_CUBXPAREN || tok == token::TOK_OPARENBAR || tok == token::TOK_CPARENBAR || tok == token::TOK_SEMI || tok == token::TOK_COMMA || tok == token::TOK_BACKQUOTE || tok == token::TOK_SIMPLEQUOTE || tok == 393 || tok == 394 || tok == 395);
      }
#else
      symbol_type (int tok, const location_type& l)
        : super_type(token_type (tok), l)
      {
        YY_ASSERT (tok == token::TOK_END || tok == token::TOK_YYerror || tok == token::TOK_YYUNDEF || tok == token::TOK_UNDERSCORE || tok == token::TOK_AS || tok == token::TOK_CASE || tok == token::TOK_CLASS || tok == token::TOK_DATA || tok == token::TOK_DEFAULT || tok == token::TOK_DERIVING || tok == token::TOK_DO || tok == token::TOK_ELSE || tok == token::TOK_HIDING || tok == token::TOK_IF || tok == token::TOK_IMPORT || tok == token::TOK_IN || tok == token::TOK_INFIX || tok == token::TOK_INFIXL || tok == token::TOK_INFIXR || tok == token::TOK_INSTANCE || tok == token::TOK_LET || tok == token::TOK_MODULE || tok == token::TOK_NEWTYPE || tok == token::TOK_OF || tok == token::TOK_QUALIFIED || tok == token::TOK_THEN || tok == token::TOK_TYPE || tok == token::TOK_WHERE || tok == token::TOK_BUILTIN || tok == token::TOK_FORALL || tok == token::TOK_FOREIGN || tok == token::TOK_EXPORT || tok == token::TOK_LABEL || tok == token::TOK_DYNAMIC || tok == token::TOK_SAFE || tok == token::TOK_INTERRUPTIBLE || tok == token::TOK_UNSAFE || tok == token::TOK_MDO || tok == token::TOK_FAMILY || tok == token::TOK_ROLE || tok == token::TOK_STDCALL || tok == token::TOK_CCALL || tok == token::TOK_CAPI || tok == token::TOK_PRIM || tok == token::TOK_JAVASCRIPT || tok == token::TOK_PROC || tok == token::TOK_REC || tok == token::TOK_GROUP || tok == token::TOK_BY || tok == token::TOK_USING || tok == token::TOK_PATTERN || tok == token::TOK_STATIC || tok == token::TOK_STOCK || tok == token::TOK_ANYCLASS || tok == token::TOK_VIA || tok == token::TOK_UNIT || tok == token::TOK_SIGNATURE || tok == token::TOK_DEPENDENCY || tok == token::TOK_INLINE_PRAG || tok == token::TOK_SPECIALIZE_PRAG || tok == token::TOK_SPECIALIZE_INLINE_PRAG || tok == token::TOK_SOURCE_PRAG || tok == token::TOK_RULES_PRAG || tok == token::TOK_CORE_PRAG || tok == token::TOK_SCC_PRAG || tok == token::TOK_GENERATED_PRAG || tok == token::TOK_DEPRECATED_PRAG || tok == token::TOK_WARNING_PRAG || tok == token::TOK_UNPACK_PRAG || tok == token::TOK_NOUNPACK_PRAG || tok == token::TOK_ANN_PRAG || tok == token::TOK_MINIMAL_PRAG || tok == token::TOK_CTYPE_PRAG || tok == token::TOK_OVERLAPPING_PRAG || tok == token::TOK_OVERLAPPABLE_PRAG || tok == token::TOK_OVERLAPS_PRAG || tok == token::TOK_INCOHERENT_PRAG || tok == token::TOK_COMPLETE_PRAG || tok == token::TOK_CLOSE_PRAG || tok == token::TOK_DOTDOT || tok == token::TOK_COLON || tok == token::TOK_DCOLON || tok == token::TOK_EQUAL || tok == token::TOK_LAM || tok == token::TOK_LCASE || tok == token::TOK_VBAR || tok == token::TOK_LARROW || tok == token::TOK_RARROW || tok == token::TOK_AT || tok == token::TOK_TILDE || tok == token::TOK_DARROW || tok == token::TOK_MINUS || tok == token::TOK_BANG || tok == token::TOK_STAR || tok == token::TOK_lARROWTAIL || tok == token::TOK_rARROWTAIL || tok == token::TOK_LARROWTAIL || tok == token::TOK_RARROWTAIL || tok == token::TOK_DOT || tok == token::TOK_TYPEAPP || tok == token::TOK_OCURLY || tok == token::TOK_CCURLY || tok == token::TOK_VOCURLY || tok == token::TOK_VCCURLY || tok == token::TOK_OBRACK || tok == token::TOK_CBRACK || tok == token::TOK_OPABRACK || tok == token::TOK_CPABRACK || tok == token::TOK_OPAREN || tok == token::TOK_CPAREN || tok == token::TOK_OUBXPAREN || tok == token::TOK_CUBXPAREN || tok == token::TOK_OPARENBAR || tok == token::TOK_CPARENBAR || tok == token::TOK_SEMI || tok == token::TOK_COMMA || tok == token::TOK_BACKQUOTE || tok == token::TOK_SIMPLEQUOTE || tok == 393 || tok == 394 || tok == 395);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      symbol_type (int tok, char v, location_type l)
        : super_type(token_type (tok), std::move (v), std::move (l))
      {
        YY_ASSERT (tok == token::TOK_CHAR || tok == token::TOK_PRIMCHAR);
      }
#else
      symbol_type (int tok, const char& v, const location_type& l)
        : super_type(token_type (tok), v, l)
      {
        YY_ASSERT (tok == token::TOK_CHAR || tok == token::TOK_PRIMCHAR);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      symbol_type (int tok, double v, location_type l)
        : super_type(token_type (tok), std::move (v), std::move (l))
      {
        YY_ASSERT (tok == token::TOK_RATIONAL || tok == token::TOK_PRIMDOUBLE);
      }
#else
      symbol_type (int tok, const double& v, const location_type& l)
        : super_type(token_type (tok), v, l)
      {
        YY_ASSERT (tok == token::TOK_RATIONAL || tok == token::TOK_PRIMDOUBLE);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      symbol_type (int tok, float v, location_type l)
        : super_type(token_type (tok), std::move (v), std::move (l))
      {
        YY_ASSERT (tok == token::TOK_PRIMFLOAT);
      }
#else
      symbol_type (int tok, const float& v, const location_type& l)
        : super_type(token_type (tok), v, l)
      {
        YY_ASSERT (tok == token::TOK_PRIMFLOAT);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      symbol_type (int tok, int v, location_type l)
        : super_type(token_type (tok), std::move (v), std::move (l))
      {
        YY_ASSERT (tok == token::TOK_INTEGER || tok == token::TOK_PRIMINTEGER || tok == token::TOK_PRINTWORD);
      }
#else
      symbol_type (int tok, const int& v, const location_type& l)
        : super_type(token_type (tok), v, l)
      {
        YY_ASSERT (tok == token::TOK_INTEGER || tok == token::TOK_PRIMINTEGER || tok == token::TOK_PRINTWORD);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      symbol_type (int tok, std::string v, location_type l)
        : super_type(token_type (tok), std::move (v), std::move (l))
      {
        YY_ASSERT (tok == token::TOK_VARID || tok == token::TOK_CONID || tok == token::TOK_VARSYM || tok == token::TOK_CONSYM || tok == token::TOK_QVARID || tok == token::TOK_QCONID || tok == token::TOK_QVARSYM || tok == token::TOK_QCONSYM || tok == token::TOK_IPDUPVARID || tok == token::TOK_LABELVARID || tok == token::TOK_STRING || tok == token::TOK_PRIMSTRING);
      }
#else
      symbol_type (int tok, const std::string& v, const location_type& l)
        : super_type(token_type (tok), v, l)
      {
        YY_ASSERT (tok == token::TOK_VARID || tok == token::TOK_CONID || tok == token::TOK_VARSYM || tok == token::TOK_CONSYM || tok == token::TOK_QVARID || tok == token::TOK_QCONID || tok == token::TOK_QVARSYM || tok == token::TOK_QCONSYM || tok == token::TOK_IPDUPVARID || tok == token::TOK_LABELVARID || tok == token::TOK_STRING || tok == token::TOK_PRIMSTRING);
      }
#endif
    };

    /// Build a parser object.
    parser (driver& drv_yyarg);
    virtual ~parser ();

#if 201103L <= YY_CPLUSPLUS
    /// Non copyable.
    parser (const parser&) = delete;
    /// Non copyable.
    parser& operator= (const parser&) = delete;
#endif

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

    /// The user-facing name of the symbol whose (internal) number is
    /// YYSYMBOL.  No bounds checking.
    static std::string symbol_name (symbol_kind_type yysymbol);

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
      make_YYerror (location_type l)
      {
        return symbol_type (token::TOK_YYerror, std::move (l));
      }
#else
      static
      symbol_type
      make_YYerror (const location_type& l)
      {
        return symbol_type (token::TOK_YYerror, l);
      }
#endif
#if 201103L <= YY_CPLUSPLUS
      static
      symbol_type
      make_YYUNDEF (location_type l)
      {
        return symbol_type (token::TOK_YYUNDEF, std::move (l));
      }
#else
      static
      symbol_type
      make_YYUNDEF (const location_type& l)
      {
        return symbol_type (token::TOK_YYUNDEF, l);
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
      make_CLASS (location_type l)
      {
        return symbol_type (token::TOK_CLASS, std::move (l));
      }
#else
      static
      symbol_type
      make_CLASS (const location_type& l)
      {
        return symbol_type (token::TOK_CLASS, l);
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


    class context
    {
    public:
      context (const parser& yyparser, const symbol_type& yyla);
      const symbol_type& lookahead () const { return yyla_; }
      symbol_kind_type token () const { return yyla_.kind (); }
      const location_type& location () const { return yyla_.location; }

      /// Put in YYARG at most YYARGN of the expected tokens, and return the
      /// number of tokens stored in YYARG.  If YYARG is null, return the
      /// number of expected tokens (guaranteed to be less than YYNTOKENS).
      int expected_tokens (symbol_kind_type yyarg[], int yyargn) const;

    private:
      const parser& yyparser_;
      const symbol_type& yyla_;
    };

  private:
#if YY_CPLUSPLUS < 201103L
    /// Non copyable.
    parser (const parser&);
    /// Non copyable.
    parser& operator= (const parser&);
#endif


    /// Stored state numbers (used for stacks).
    typedef short state_type;

    /// The arguments of the error message.
    int yy_syntax_error_arguments_ (const context& yyctx,
                                    symbol_kind_type yyarg[], int yyargn) const;

    /// Generate an error message.
    /// \param yyctx     the context in which the error occurred.
    virtual std::string yysyntax_error_ (const context& yyctx) const;
    /// Compute post-reduction state.
    /// \param yystate   the current state
    /// \param yysym     the nonterminal to push on the stack
    static state_type yy_lr_goto_state_ (state_type yystate, int yysym);

    /// Whether the given \c yypact_ value indicates a defaulted state.
    /// \param yyvalue   the value to check
    static bool yy_pact_value_is_default_ (int yyvalue);

    /// Whether the given \c yytable_ value indicates a syntax error.
    /// \param yyvalue   the value to check
    static bool yy_table_value_is_error_ (int yyvalue);

    static const short yypact_ninf_;
    static const short yytable_ninf_;

    /// Convert a scanner token kind \a t to a symbol kind.
    /// In theory \a t should be a token_kind_type, but character literals
    /// are valid, yet not members of the token_type enum.
    static symbol_kind_type yytranslate_ (int t);

    /// Convert the symbol name \a n to a form suitable for a diagnostic.
    static std::string yytnamerr_ (const char *yystr);

    /// For a symbol, its name in clear.
    static const char* const yytname_[];


    // Tables.
    // YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
    // STATE-NUM.
    static const short yypact_[];

    // YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
    // Performed when YYTABLE does not specify something else to do.  Zero
    // means the default is an error.
    static const short yydefact_[];

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
    static const short yystos_[];

    // YYR1[YYN] -- Symbol number of symbol that rule YYN derives.
    static const short yyr1_[];

    // YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.
    static const signed char yyr2_[];


#if YYDEBUG
    // YYRLINE[YYN] -- Source line where rule number YYN was defined.
    static const short yyrline_[];
    /// Report on the debug stream that the rule \a r is going to be reduced.
    virtual void yy_reduce_print_ (int r) const;
    /// Print the state stack on the debug stream.
    virtual void yy_stack_print_ () const;

    /// Debugging level.
    int yydebug_;
    /// Debug stream.
    std::ostream* yycdebug_;

    /// \brief Display a symbol kind, value and location.
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

      /// The symbol kind as needed by the constructor.
      typedef state_type kind_type;

      /// Constructor.
      by_state (kind_type s) YY_NOEXCEPT;

      /// Copy constructor.
      by_state (const by_state& that) YY_NOEXCEPT;

      /// Record that this symbol is empty.
      void clear () YY_NOEXCEPT;

      /// Steal the symbol kind from \a that.
      void move (by_state& that);

      /// The symbol kind (corresponding to \a state).
      /// \a S_YYEMPTY when empty.
      symbol_kind_type kind () const YY_NOEXCEPT;

      /// The state number used to denote an empty symbol.
      /// We use the initial state, as it does not have a value.
      enum { empty_state = 0 };

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

      /// Assignment, needed by push_back by other implementations.
      /// Needed by some other old implementations.
      stack_symbol_type& operator= (const stack_symbol_type& that);
#endif
    };

    /// A stack with random access from its top.
    template <typename T, typename S = std::vector<T> >
    class stack
    {
    public:
      // Hide our reversed order.
      typedef typename S::iterator iterator;
      typedef typename S::const_iterator const_iterator;
      typedef typename S::size_type size_type;
      typedef typename std::ptrdiff_t index_type;

      stack (size_type n = 200)
        : seq_ (n)
      {}

#if 201103L <= YY_CPLUSPLUS
      /// Non copyable.
      stack (const stack&) = delete;
      /// Non copyable.
      stack& operator= (const stack&) = delete;
#endif

      /// Random access.
      ///
      /// Index 0 returns the topmost element.
      const T&
      operator[] (index_type i) const
      {
        return seq_[size_type (size () - 1 - i)];
      }

      /// Random access.
      ///
      /// Index 0 returns the topmost element.
      T&
      operator[] (index_type i)
      {
        return seq_[size_type (size () - 1 - i)];
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
      pop (std::ptrdiff_t n = 1) YY_NOEXCEPT
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
      index_type
      size () const YY_NOEXCEPT
      {
        return index_type (seq_.size ());
      }

      /// Iterator on top of the stack (going downwards).
      const_iterator
      begin () const YY_NOEXCEPT
      {
        return seq_.begin ();
      }

      /// Bottom of the stack.
      const_iterator
      end () const YY_NOEXCEPT
      {
        return seq_.end ();
      }

      /// Present a slice of the top of a stack.
      class slice
      {
      public:
        slice (const stack& stack, index_type range)
          : stack_ (stack)
          , range_ (range)
        {}

        const T&
        operator[] (index_type i) const
        {
          return stack_[range_ - i];
        }

      private:
        const stack& stack_;
        index_type range_;
      };

    private:
#if YY_CPLUSPLUS < 201103L
      /// Non copyable.
      stack (const stack&);
      /// Non copyable.
      stack& operator= (const stack&);
#endif
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
      yylast_ = 5275,     ///< Last index in yytable_.
      yynnts_ = 193,  ///< Number of nonterminal symbols.
      yyfinal_ = 12 ///< Termination state number.
    };


    // User arguments.
    driver& drv;

  };

  inline
  parser::symbol_kind_type
  parser::yytranslate_ (int t)
  {
    // YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to
    // TOKEN-NUM as returned by yylex.
    static
    const unsigned char
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
    const int user_token_number_max_ = 395;

    if (t <= 0)
      return symbol_kind::S_YYEOF;
    else if (t <= user_token_number_max_)
      return YY_CAST (symbol_kind_type, translate_table[t]);
    else
      return symbol_kind::S_YYUNDEF;
  }

  // basic_symbol.
  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (const basic_symbol& that)
    : Base (that)
    , value ()
    , location (that.location)
  {
    switch (this->kind ())
    {
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        value.copy< bool > (YY_MOVE (that.value));
        break;

      case 128: // "CHAR"
      case 132: // "PRIMCHAR"
        value.copy< char > (YY_MOVE (that.value));
        break;

      case 131: // "RATIONAL"
      case 137: // "PRIMDOUBLE"
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
      case 178: // cl_decl
      case 179: // ty_decl
      case 180: // inst_decl
      case 186: // tycl_hdr
      case 195: // decllist
      case 196: // binds
      case 197: // wherebinds
      case 200: // opt_sig
      case 201: // opt_tyconsig
      case 202: // sigtype
      case 203: // sigtypedoc
      case 209: // ctype
      case 210: // ctypedoc
      case 211: // context
      case 212: // context_no_ops
      case 213: // type
      case 214: // typedoc
      case 215: // btype
      case 218: // tyapp
      case 219: // atype_docs
      case 220: // atype
      case 221: // inst_type
      case 227: // tv_bndr
      case 228: // kind
      case 231: // constr
      case 232: // forall
      case 233: // constr_stuff
      case 236: // fielddecl
      case 241: // decl_no_th
      case 242: // decl
      case 243: // rhs
      case 245: // gdrh
      case 246: // sigdecl
      case 249: // exp
      case 252: // exp10_top
      case 253: // exp10
      case 257: // aexp
      case 258: // aexp1
      case 259: // aexp2
      case 260: // texp
      case 262: // list
      case 265: // transformqual
      case 271: // alt
      case 272: // alt_rhs
      case 274: // ifgdpats
      case 275: // gdpat
      case 276: // pat
      case 277: // bindpat
      case 278: // apat
      case 282: // stmt
      case 283: // qual
      case 330: // literal
        value.copy< expression_ref > (YY_MOVE (that.value));
        break;

      case 136: // "PRIMFLOAT"
        value.copy< float > (YY_MOVE (that.value));
        break;

      case 130: // "INTEGER"
      case 134: // "PRIMINTEGER"
      case 135: // "PRIMWORD"
      case 333: // commas
        value.copy< int > (YY_MOVE (that.value));
        break;

      case 172: // prec
        value.copy< std::optional<int> > (YY_MOVE (that.value));
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.copy< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case 118: // "VARID"
      case 119: // "CONID"
      case 120: // "VARSYM"
      case 121: // "CONSYM"
      case 122: // "QVARID"
      case 123: // "QCONID"
      case 124: // "QVARSYM"
      case 125: // "QCONSYM"
      case 126: // "IPDUPVARID"
      case 127: // "LABELVARID"
      case 129: // "STRING"
      case 133: // "PRIMSTRING"
      case 173: // infix
      case 184: // data_or_newtype
      case 206: // strict_mark
      case 207: // strictness
      case 287: // qcon
      case 288: // gen_qcon
      case 289: // con
      case 291: // sysdcon_no_list
      case 292: // sysdcon
      case 293: // conop
      case 294: // qconop
      case 295: // gtycon
      case 296: // ntgtycon
      case 297: // oqtycon
      case 298: // oqtycon_no_varcon
      case 299: // qtyconop
      case 300: // qtycondoc
      case 301: // qtycon
      case 302: // tycon
      case 303: // qtyconsym
      case 304: // tyconsym
      case 305: // op
      case 306: // varop
      case 307: // qop
      case 308: // qopm
      case 309: // hole_op
      case 310: // qvarop
      case 311: // qvaropm
      case 312: // tyvar
      case 313: // tyvarop
      case 314: // tyvarid
      case 315: // var
      case 316: // qvar
      case 317: // qvarid
      case 318: // varid
      case 319: // qvarsym
      case 320: // qvarsym_no_minus
      case 321: // qvarsym1
      case 322: // varsym
      case 323: // varsym_no_minus
      case 324: // special_id
      case 325: // special_sym
      case 326: // qconid
      case 327: // conid
      case 328: // qconsym
      case 329: // consym
      case 332: // modid
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
      case 194: // decls
      case 205: // sigtypes1
      case 216: // btype_no_ops
      case 217: // tyapps
      case 223: // comma_types0
      case 224: // comma_types1
      case 226: // tv_bndrs
      case 229: // constrs
      case 230: // constrs1
      case 234: // fielddecls
      case 235: // fielddecls1
      case 244: // gdrhs
      case 250: // infixexp
      case 251: // infixexp_top
      case 256: // fexp
      case 261: // tup_exprs
      case 263: // lexps
      case 264: // squals
      case 266: // guardquals
      case 267: // guardquals1
      case 268: // altslist
      case 269: // alts
      case 270: // alts1
      case 273: // gdpats
      case 279: // apats1
      case 280: // stmtlist
      case 281: // stmts
        value.copy< std::vector<expression_ref> > (YY_MOVE (that.value));
        break;

      case 174: // ops
      case 204: // sig_vars
        value.copy< std::vector<std::string> > (YY_MOVE (that.value));
        break;

      default:
        break;
    }

  }



  template <typename Base>
  parser::symbol_kind_type
  parser::basic_symbol<Base>::type_get () const YY_NOEXCEPT
  {
    return this->kind ();
  }

  template <typename Base>
  bool
  parser::basic_symbol<Base>::empty () const YY_NOEXCEPT
  {
    return this->kind () == symbol_kind::S_YYEMPTY;
  }

  template <typename Base>
  void
  parser::basic_symbol<Base>::move (basic_symbol& s)
  {
    super_type::move (s);
    switch (this->kind ())
    {
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        value.move< bool > (YY_MOVE (s.value));
        break;

      case 128: // "CHAR"
      case 132: // "PRIMCHAR"
        value.move< char > (YY_MOVE (s.value));
        break;

      case 131: // "RATIONAL"
      case 137: // "PRIMDOUBLE"
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
      case 178: // cl_decl
      case 179: // ty_decl
      case 180: // inst_decl
      case 186: // tycl_hdr
      case 195: // decllist
      case 196: // binds
      case 197: // wherebinds
      case 200: // opt_sig
      case 201: // opt_tyconsig
      case 202: // sigtype
      case 203: // sigtypedoc
      case 209: // ctype
      case 210: // ctypedoc
      case 211: // context
      case 212: // context_no_ops
      case 213: // type
      case 214: // typedoc
      case 215: // btype
      case 218: // tyapp
      case 219: // atype_docs
      case 220: // atype
      case 221: // inst_type
      case 227: // tv_bndr
      case 228: // kind
      case 231: // constr
      case 232: // forall
      case 233: // constr_stuff
      case 236: // fielddecl
      case 241: // decl_no_th
      case 242: // decl
      case 243: // rhs
      case 245: // gdrh
      case 246: // sigdecl
      case 249: // exp
      case 252: // exp10_top
      case 253: // exp10
      case 257: // aexp
      case 258: // aexp1
      case 259: // aexp2
      case 260: // texp
      case 262: // list
      case 265: // transformqual
      case 271: // alt
      case 272: // alt_rhs
      case 274: // ifgdpats
      case 275: // gdpat
      case 276: // pat
      case 277: // bindpat
      case 278: // apat
      case 282: // stmt
      case 283: // qual
      case 330: // literal
        value.move< expression_ref > (YY_MOVE (s.value));
        break;

      case 136: // "PRIMFLOAT"
        value.move< float > (YY_MOVE (s.value));
        break;

      case 130: // "INTEGER"
      case 134: // "PRIMINTEGER"
      case 135: // "PRIMWORD"
      case 333: // commas
        value.move< int > (YY_MOVE (s.value));
        break;

      case 172: // prec
        value.move< std::optional<int> > (YY_MOVE (s.value));
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.move< std::optional<std::string> > (YY_MOVE (s.value));
        break;

      case 118: // "VARID"
      case 119: // "CONID"
      case 120: // "VARSYM"
      case 121: // "CONSYM"
      case 122: // "QVARID"
      case 123: // "QCONID"
      case 124: // "QVARSYM"
      case 125: // "QCONSYM"
      case 126: // "IPDUPVARID"
      case 127: // "LABELVARID"
      case 129: // "STRING"
      case 133: // "PRIMSTRING"
      case 173: // infix
      case 184: // data_or_newtype
      case 206: // strict_mark
      case 207: // strictness
      case 287: // qcon
      case 288: // gen_qcon
      case 289: // con
      case 291: // sysdcon_no_list
      case 292: // sysdcon
      case 293: // conop
      case 294: // qconop
      case 295: // gtycon
      case 296: // ntgtycon
      case 297: // oqtycon
      case 298: // oqtycon_no_varcon
      case 299: // qtyconop
      case 300: // qtycondoc
      case 301: // qtycon
      case 302: // tycon
      case 303: // qtyconsym
      case 304: // tyconsym
      case 305: // op
      case 306: // varop
      case 307: // qop
      case 308: // qopm
      case 309: // hole_op
      case 310: // qvarop
      case 311: // qvaropm
      case 312: // tyvar
      case 313: // tyvarop
      case 314: // tyvarid
      case 315: // var
      case 316: // qvar
      case 317: // qvarid
      case 318: // varid
      case 319: // qvarsym
      case 320: // qvarsym_no_minus
      case 321: // qvarsym1
      case 322: // varsym
      case 323: // varsym_no_minus
      case 324: // special_id
      case 325: // special_sym
      case 326: // qconid
      case 327: // conid
      case 328: // qconsym
      case 329: // consym
      case 332: // modid
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
      case 194: // decls
      case 205: // sigtypes1
      case 216: // btype_no_ops
      case 217: // tyapps
      case 223: // comma_types0
      case 224: // comma_types1
      case 226: // tv_bndrs
      case 229: // constrs
      case 230: // constrs1
      case 234: // fielddecls
      case 235: // fielddecls1
      case 244: // gdrhs
      case 250: // infixexp
      case 251: // infixexp_top
      case 256: // fexp
      case 261: // tup_exprs
      case 263: // lexps
      case 264: // squals
      case 266: // guardquals
      case 267: // guardquals1
      case 268: // altslist
      case 269: // alts
      case 270: // alts1
      case 273: // gdpats
      case 279: // apats1
      case 280: // stmtlist
      case 281: // stmts
        value.move< std::vector<expression_ref> > (YY_MOVE (s.value));
        break;

      case 174: // ops
      case 204: // sig_vars
        value.move< std::vector<std::string> > (YY_MOVE (s.value));
        break;

      default:
        break;
    }

    location = YY_MOVE (s.location);
  }

  // by_kind.
  inline
  parser::by_kind::by_kind ()
    : kind_ (symbol_kind::S_YYEMPTY)
  {}

#if 201103L <= YY_CPLUSPLUS
  inline
  parser::by_kind::by_kind (by_kind&& that)
    : kind_ (that.kind_)
  {
    that.clear ();
  }
#endif

  inline
  parser::by_kind::by_kind (const by_kind& that)
    : kind_ (that.kind_)
  {}

  inline
  parser::by_kind::by_kind (token_kind_type t)
    : kind_ (yytranslate_ (t))
  {}

  inline
  void
  parser::by_kind::clear ()
  {
    kind_ = symbol_kind::S_YYEMPTY;
  }

  inline
  void
  parser::by_kind::move (by_kind& that)
  {
    kind_ = that.kind_;
    that.clear ();
  }

  inline
  parser::symbol_kind_type
  parser::by_kind::kind () const YY_NOEXCEPT
  {
    return kind_;
  }

  inline
  parser::symbol_kind_type
  parser::by_kind::type_get () const YY_NOEXCEPT
  {
    return this->kind ();
  }

} // yy
#line 5040 "parser.hh"





#endif // !YY_YY_PARSER_HH_INCLUDED
