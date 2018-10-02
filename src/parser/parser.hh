// A Bison parser, made by GNU Bison 3.1.

// Skeleton interface for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015, 2018 Free Software Foundation, Inc.

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

#ifndef YY_YY_PARSER_HH_INCLUDED
# define YY_YY_PARSER_HH_INCLUDED
// //                    "%code requires" blocks.
#line 9 "parser.y" // lalr1.cc:380

  # include <string>
  # include <iostream>
  # include <boost/optional.hpp>
  # include <boost/optional/optional_io.hpp>
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
  expression_ref make_infix(const std::string& infix, boost::optional<int>& prec, std::vector<std::string>& ops);
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
  expression_ref make_gdrhs(const std::vector<expression_ref>& gdrhs);

  expression_ref make_typed_exp(const expression_ref& exp, const expression_ref& type);
  expression_ref make_infixexp(const std::vector<expression_ref>& args);
  expression_ref make_minus(const expression_ref& exp);
  expression_ref make_fexp(const std::vector<expression_ref>& args);
  expression_ref make_as_pattern(const std::string& var, const expression_ref& body);
  expression_ref make_lazy_pattern(const expression_ref& pat);
  expression_ref make_lambda(const std::vector<expression_ref>& pats, const expression_ref& body);
  expression_ref make_let(const expression_ref& binds, const expression_ref& body);
  expression_ref make_if(const expression_ref& cond, const expression_ref& alt_true, const expression_ref& alt_false);
  expression_ref make_case(const expression_ref& obj, const expression_ref& alts);
  expression_ref make_do(const std::vector<expression_ref>& stmts);
  expression_ref yy_make_tuple(const std::vector<expression_ref>& tup_exprs);


  expression_ref make_list(const std::vector<expression_ref>& items);
  expression_ref make_flattenedpquals(const std::vector<expression_ref>& pquals);
  expression_ref make_squals(const std::vector<expression_ref>& squals);
  expression_ref make_alts(const std::vector<expression_ref>& alts);
  expression_ref yy_make_alt(const expression_ref& pat, const expression_ref& alt_rhs);
  expression_ref make_alt_rhs(const expression_ref& ralt, const expression_ref& wherebinds);
  expression_ref make_gdpats(const std::vector<expression_ref>& gdpats);
  expression_ref make_gdpat(const expression_ref& guardquals, const expression_ref& exp);

  expression_ref make_stmts(const std::vector<expression_ref>& stmts);

#line 104 "parser.hh" // lalr1.cc:380

# include <cassert>
# include <cstdlib> // std::abort
# include <iostream>
# include <stdexcept>
# include <string>
# include <vector>
# include "stack.hh"
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

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
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
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif


namespace yy {
#line 188 "parser.hh" // lalr1.cc:380



  /// A char[S] buffer to store and retrieve objects.
  ///
  /// Sort of a variant, but does not keep track of the nature
  /// of the stored data, since that knowledge is available
  /// via the current state.
  template <size_t S>
  struct variant
  {
    /// Type of *this.
    typedef variant<S> self_type;

    /// Empty construction.
    variant ()
      : yybuffer_ ()
      , yytypeid_ (YY_NULLPTR)
    {}

    /// Construct and fill.
    template <typename T>
    variant (const T& t)
      : yytypeid_ (&typeid (T))
    {
      YYASSERT (sizeof (T) <= S);
      new (yyas_<T> ()) T (t);
    }

    /// Destruction, allowed only if empty.
    ~variant ()
    {
      YYASSERT (!yytypeid_);
    }

    /// Instantiate an empty \a T in here.
    template <typename T>
    T&
    build ()
    {
      YYASSERT (!yytypeid_);
      YYASSERT (sizeof (T) <= S);
      yytypeid_ = & typeid (T);
      return *new (yyas_<T> ()) T ();
    }

    /// Instantiate a \a T in here from \a t.
    template <typename T>
    T&
    build (const T& t)
    {
      YYASSERT (!yytypeid_);
      YYASSERT (sizeof (T) <= S);
      yytypeid_ = & typeid (T);
      return *new (yyas_<T> ()) T (t);
    }

    /// Accessor to a built \a T.
    template <typename T>
    T&
    as ()
    {
      YYASSERT (yytypeid_);
      YYASSERT (*yytypeid_ == typeid (T));
      YYASSERT (sizeof (T) <= S);
      return *yyas_<T> ();
    }

    /// Const accessor to a built \a T (for %printer).
    template <typename T>
    const T&
    as () const
    {
      YYASSERT (yytypeid_);
      YYASSERT (*yytypeid_ == typeid (T));
      YYASSERT (sizeof (T) <= S);
      return *yyas_<T> ();
    }

    /// Swap the content with \a other, of same type.
    ///
    /// Both variants must be built beforehand, because swapping the actual
    /// data requires reading it (with as()), and this is not possible on
    /// unconstructed variants: it would require some dynamic testing, which
    /// should not be the variant's responsability.
    /// Swapping between built and (possibly) non-built is done with
    /// variant::move ().
    template <typename T>
    void
    swap (self_type& other)
    {
      YYASSERT (yytypeid_);
      YYASSERT (*yytypeid_ == *other.yytypeid_);
      std::swap (as<T> (), other.as<T> ());
    }

    /// Move the content of \a other to this.
    ///
    /// Destroys \a other.
    template <typename T>
    void
    move (self_type& other)
    {
      build<T> ();
      swap<T> (other);
      other.destroy<T> ();
    }

    /// Copy the content of \a other to this.
    template <typename T>
    void
    copy (const self_type& other)
    {
      build<T> (other.as<T> ());
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
    self_type& operator=(const self_type&);
    variant (const self_type&);

    /// Accessor to raw memory as \a T.
    template <typename T>
    T*
    yyas_ ()
    {
      void *yyp = yybuffer_.yyraw;
      return static_cast<T*> (yyp);
     }

    /// Const accessor to raw memory as \a T.
    template <typename T>
    const T*
    yyas_ () const
    {
      const void *yyp = yybuffer_.yyraw;
      return static_cast<const T*> (yyp);
     }

    union
    {
      /// Strongest alignment constraints.
      long double yyalign_me;
      /// A buffer large enough to store any of the semantic values.
      char yyraw[S];
    } yybuffer_;

    /// Whether the content is built: if defined, the name of the stored type.
    const std::type_info *yytypeid_;
  };


  /// A Bison parser.
  class parser
  {
  public:
#ifndef YYSTYPE
    /// An auxiliary type to compute the largest semantic type.
    union union_type
    {
      // maybe_src
      // maybe_safe
      // optqualified
      char dummy1[sizeof(bool)];

      // prec
      char dummy2[sizeof(boost::optional<int>)];

      // maybe_pkg
      // maybeas
      char dummy3[sizeof(boost::optional<std::string>)];

      // "CHAR"
      // "PRIMCHAR"
      char dummy4[sizeof(char)];

      // "RATIONAL"
      // "PRIMDOUBLE"
      char dummy5[sizeof(double)];

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
      // flattenedpquals
      // transformqual
      // alt
      // alt_rhs
      // ralt
      // ifgdpats
      // gdpat
      // pat
      // bindpat
      // apat
      // stmt
      // qual
      // literal
      char dummy6[sizeof(expression_ref)];

      // "PRIMFLOAT"
      char dummy7[sizeof(float)];

      // "INTEGER"
      // "PRIMINTEGER"
      // "PRIMWORD"
      // commas
      char dummy8[sizeof(int)];

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
      char dummy9[sizeof(std::string)];

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
      // pquals
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
      char dummy10[sizeof(std::vector<expression_ref>)];

      // ops
      char dummy11[sizeof(std::vector<std::string>)];
};

    /// Symbol semantic values.
    typedef variant<sizeof(union_type)> semantic_type;
#else
    typedef YYSTYPE semantic_type;
#endif
    /// Symbol locations.
    typedef location location_type;

    /// Syntax errors thrown from user actions.
    struct syntax_error : std::runtime_error
    {
      syntax_error (const location_type& l, const std::string& m);
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
    /// via type_get().
    ///
    /// Provide access to semantic value and location.
    template <typename Base>
    struct basic_symbol : Base
    {
      /// Alias to Base.
      typedef Base super_type;

      /// Default constructor.
      basic_symbol ();

      /// Copy constructor.
      basic_symbol (const basic_symbol& other);

      /// Constructor for valueless symbols, and symbols from each type.

  basic_symbol (typename Base::kind_type t, const location_type& l);

  basic_symbol (typename Base::kind_type t, const bool& v, const location_type& l);

  basic_symbol (typename Base::kind_type t, const boost::optional<int>& v, const location_type& l);

  basic_symbol (typename Base::kind_type t, const boost::optional<std::string>& v, const location_type& l);

  basic_symbol (typename Base::kind_type t, const char& v, const location_type& l);

  basic_symbol (typename Base::kind_type t, const double& v, const location_type& l);

  basic_symbol (typename Base::kind_type t, const expression_ref& v, const location_type& l);

  basic_symbol (typename Base::kind_type t, const float& v, const location_type& l);

  basic_symbol (typename Base::kind_type t, const int& v, const location_type& l);

  basic_symbol (typename Base::kind_type t, const std::string& v, const location_type& l);

  basic_symbol (typename Base::kind_type t, const std::vector<expression_ref>& v, const location_type& l);

  basic_symbol (typename Base::kind_type t, const std::vector<std::string>& v, const location_type& l);


      /// Constructor for symbols with semantic value.
      basic_symbol (typename Base::kind_type t,
                    const semantic_type& v,
                    const location_type& l);

      /// Destroy the symbol.
      ~basic_symbol ();

      /// Destroy contents, and record that is empty.
      void clear ();

      /// Whether empty.
      bool empty () const;

      /// Destructive move, \a s is emptied into this.
      void move (basic_symbol& s);

      /// The semantic value.
      semantic_type value;

      /// The location.
      location_type location;

    private:
      /// Assignment operator.
      basic_symbol& operator= (const basic_symbol& other);
    };

    /// Type access provider for token (enum) based symbols.
    struct by_type
    {
      /// Default constructor.
      by_type ();

      /// Copy constructor.
      by_type (const by_type& other);

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
      symbol_number_type type_get () const;

      /// The token.
      token_type token () const;

      /// The symbol type.
      /// \a empty_symbol when empty.
      /// An int, not token_number_type, to be able to store empty_symbol.
      int type;
    };

    /// "External" symbols: returned by the scanner.
    typedef basic_symbol<by_type> symbol_type;

    // Symbol constructors declarations.
    static inline
    symbol_type
    make_END (const location_type& l);

    static inline
    symbol_type
    make_UNDERSCORE (const location_type& l);

    static inline
    symbol_type
    make_AS (const location_type& l);

    static inline
    symbol_type
    make_CASE (const location_type& l);

    static inline
    symbol_type
    make_DATA (const location_type& l);

    static inline
    symbol_type
    make_DEFAULT (const location_type& l);

    static inline
    symbol_type
    make_DERIVING (const location_type& l);

    static inline
    symbol_type
    make_DO (const location_type& l);

    static inline
    symbol_type
    make_ELSE (const location_type& l);

    static inline
    symbol_type
    make_HIDING (const location_type& l);

    static inline
    symbol_type
    make_IF (const location_type& l);

    static inline
    symbol_type
    make_IMPORT (const location_type& l);

    static inline
    symbol_type
    make_IN (const location_type& l);

    static inline
    symbol_type
    make_INFIX (const location_type& l);

    static inline
    symbol_type
    make_INFIXL (const location_type& l);

    static inline
    symbol_type
    make_INFIXR (const location_type& l);

    static inline
    symbol_type
    make_INSTANCE (const location_type& l);

    static inline
    symbol_type
    make_LET (const location_type& l);

    static inline
    symbol_type
    make_MODULE (const location_type& l);

    static inline
    symbol_type
    make_NEWTYPE (const location_type& l);

    static inline
    symbol_type
    make_OF (const location_type& l);

    static inline
    symbol_type
    make_QUALIFIED (const location_type& l);

    static inline
    symbol_type
    make_THEN (const location_type& l);

    static inline
    symbol_type
    make_TYPE (const location_type& l);

    static inline
    symbol_type
    make_WHERE (const location_type& l);

    static inline
    symbol_type
    make_BUILTIN (const location_type& l);

    static inline
    symbol_type
    make_FORALL (const location_type& l);

    static inline
    symbol_type
    make_FOREIGN (const location_type& l);

    static inline
    symbol_type
    make_EXPORT (const location_type& l);

    static inline
    symbol_type
    make_LABEL (const location_type& l);

    static inline
    symbol_type
    make_DYNAMIC (const location_type& l);

    static inline
    symbol_type
    make_SAFE (const location_type& l);

    static inline
    symbol_type
    make_INTERRUPTIBLE (const location_type& l);

    static inline
    symbol_type
    make_UNSAFE (const location_type& l);

    static inline
    symbol_type
    make_MDO (const location_type& l);

    static inline
    symbol_type
    make_FAMILY (const location_type& l);

    static inline
    symbol_type
    make_ROLE (const location_type& l);

    static inline
    symbol_type
    make_STDCALL (const location_type& l);

    static inline
    symbol_type
    make_CCALL (const location_type& l);

    static inline
    symbol_type
    make_CAPI (const location_type& l);

    static inline
    symbol_type
    make_PRIM (const location_type& l);

    static inline
    symbol_type
    make_JAVASCRIPT (const location_type& l);

    static inline
    symbol_type
    make_PROC (const location_type& l);

    static inline
    symbol_type
    make_REC (const location_type& l);

    static inline
    symbol_type
    make_GROUP (const location_type& l);

    static inline
    symbol_type
    make_BY (const location_type& l);

    static inline
    symbol_type
    make_USING (const location_type& l);

    static inline
    symbol_type
    make_PATTERN (const location_type& l);

    static inline
    symbol_type
    make_STATIC (const location_type& l);

    static inline
    symbol_type
    make_STOCK (const location_type& l);

    static inline
    symbol_type
    make_ANYCLASS (const location_type& l);

    static inline
    symbol_type
    make_VIA (const location_type& l);

    static inline
    symbol_type
    make_UNIT (const location_type& l);

    static inline
    symbol_type
    make_SIGNATURE (const location_type& l);

    static inline
    symbol_type
    make_DEPENDENCY (const location_type& l);

    static inline
    symbol_type
    make_INLINE_PRAG (const location_type& l);

    static inline
    symbol_type
    make_SPECIALIZE_PRAG (const location_type& l);

    static inline
    symbol_type
    make_SPECIALIZE_INLINE_PRAG (const location_type& l);

    static inline
    symbol_type
    make_SOURCE_PRAG (const location_type& l);

    static inline
    symbol_type
    make_RULES_PRAG (const location_type& l);

    static inline
    symbol_type
    make_CORE_PRAG (const location_type& l);

    static inline
    symbol_type
    make_SCC_PRAG (const location_type& l);

    static inline
    symbol_type
    make_GENERATED_PRAG (const location_type& l);

    static inline
    symbol_type
    make_DEPRECATED_PRAG (const location_type& l);

    static inline
    symbol_type
    make_WARNING_PRAG (const location_type& l);

    static inline
    symbol_type
    make_UNPACK_PRAG (const location_type& l);

    static inline
    symbol_type
    make_NOUNPACK_PRAG (const location_type& l);

    static inline
    symbol_type
    make_ANN_PRAG (const location_type& l);

    static inline
    symbol_type
    make_MINIMAL_PRAG (const location_type& l);

    static inline
    symbol_type
    make_CTYPE_PRAG (const location_type& l);

    static inline
    symbol_type
    make_OVERLAPPING_PRAG (const location_type& l);

    static inline
    symbol_type
    make_OVERLAPPABLE_PRAG (const location_type& l);

    static inline
    symbol_type
    make_OVERLAPS_PRAG (const location_type& l);

    static inline
    symbol_type
    make_INCOHERENT_PRAG (const location_type& l);

    static inline
    symbol_type
    make_COMPLETE_PRAG (const location_type& l);

    static inline
    symbol_type
    make_CLOSE_PRAG (const location_type& l);

    static inline
    symbol_type
    make_DOTDOT (const location_type& l);

    static inline
    symbol_type
    make_COLON (const location_type& l);

    static inline
    symbol_type
    make_DCOLON (const location_type& l);

    static inline
    symbol_type
    make_EQUAL (const location_type& l);

    static inline
    symbol_type
    make_LAM (const location_type& l);

    static inline
    symbol_type
    make_LCASE (const location_type& l);

    static inline
    symbol_type
    make_VBAR (const location_type& l);

    static inline
    symbol_type
    make_LARROW (const location_type& l);

    static inline
    symbol_type
    make_RARROW (const location_type& l);

    static inline
    symbol_type
    make_AT (const location_type& l);

    static inline
    symbol_type
    make_TILDE (const location_type& l);

    static inline
    symbol_type
    make_DARROW (const location_type& l);

    static inline
    symbol_type
    make_MINUS (const location_type& l);

    static inline
    symbol_type
    make_BANG (const location_type& l);

    static inline
    symbol_type
    make_STAR (const location_type& l);

    static inline
    symbol_type
    make_lARROWTAIL (const location_type& l);

    static inline
    symbol_type
    make_rARROWTAIL (const location_type& l);

    static inline
    symbol_type
    make_LARROWTAIL (const location_type& l);

    static inline
    symbol_type
    make_RARROWTAIL (const location_type& l);

    static inline
    symbol_type
    make_DOT (const location_type& l);

    static inline
    symbol_type
    make_TYPEAPP (const location_type& l);

    static inline
    symbol_type
    make_OCURLY (const location_type& l);

    static inline
    symbol_type
    make_CCURLY (const location_type& l);

    static inline
    symbol_type
    make_VOCURLY (const location_type& l);

    static inline
    symbol_type
    make_VCCURLY (const location_type& l);

    static inline
    symbol_type
    make_OBRACK (const location_type& l);

    static inline
    symbol_type
    make_CBRACK (const location_type& l);

    static inline
    symbol_type
    make_OPABRACK (const location_type& l);

    static inline
    symbol_type
    make_CPABRACK (const location_type& l);

    static inline
    symbol_type
    make_OPAREN (const location_type& l);

    static inline
    symbol_type
    make_CPAREN (const location_type& l);

    static inline
    symbol_type
    make_OUBXPAREN (const location_type& l);

    static inline
    symbol_type
    make_CUBXPAREN (const location_type& l);

    static inline
    symbol_type
    make_OPARENBAR (const location_type& l);

    static inline
    symbol_type
    make_CPARENBAR (const location_type& l);

    static inline
    symbol_type
    make_SEMI (const location_type& l);

    static inline
    symbol_type
    make_COMMA (const location_type& l);

    static inline
    symbol_type
    make_BACKQUOTE (const location_type& l);

    static inline
    symbol_type
    make_SIMPLEQUOTE (const location_type& l);

    static inline
    symbol_type
    make_VARID (const std::string& v, const location_type& l);

    static inline
    symbol_type
    make_CONID (const std::string& v, const location_type& l);

    static inline
    symbol_type
    make_VARSYM (const std::string& v, const location_type& l);

    static inline
    symbol_type
    make_CONSYM (const std::string& v, const location_type& l);

    static inline
    symbol_type
    make_QVARID (const std::string& v, const location_type& l);

    static inline
    symbol_type
    make_QCONID (const std::string& v, const location_type& l);

    static inline
    symbol_type
    make_QVARSYM (const std::string& v, const location_type& l);

    static inline
    symbol_type
    make_QCONSYM (const std::string& v, const location_type& l);

    static inline
    symbol_type
    make_IPDUPVARID (const std::string& v, const location_type& l);

    static inline
    symbol_type
    make_LABELVARID (const std::string& v, const location_type& l);

    static inline
    symbol_type
    make_CHAR (const char& v, const location_type& l);

    static inline
    symbol_type
    make_STRING (const std::string& v, const location_type& l);

    static inline
    symbol_type
    make_INTEGER (const int& v, const location_type& l);

    static inline
    symbol_type
    make_RATIONAL (const double& v, const location_type& l);

    static inline
    symbol_type
    make_PRIMCHAR (const char& v, const location_type& l);

    static inline
    symbol_type
    make_PRIMSTRING (const std::string& v, const location_type& l);

    static inline
    symbol_type
    make_PRIMINTEGER (const int& v, const location_type& l);

    static inline
    symbol_type
    make_PRINTWORD (const int& v, const location_type& l);

    static inline
    symbol_type
    make_PRIMFLOAT (const float& v, const location_type& l);

    static inline
    symbol_type
    make_PRIMDOUBLE (const double& v, const location_type& l);


    /// Build a parser object.
    parser (driver& drv_yyarg);
    virtual ~parser ();

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

    // Debugging.
    int yydebug_;
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
      by_state ();

      /// The symbol type as needed by the constructor.
      typedef state_type kind_type;

      /// Constructor.
      by_state (kind_type s);

      /// Copy constructor.
      by_state (const by_state& other);

      /// Record that this symbol is empty.
      void clear ();

      /// Steal the symbol type from \a that.
      void move (by_state& that);

      /// The (internal) type number (corresponding to \a state).
      /// \a empty_symbol when empty.
      symbol_number_type type_get () const;

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
      /// Copy construct (for efficiency).
      stack_symbol_type (const stack_symbol_type& that);
      /// Steal the contents from \a sym to build this.
      stack_symbol_type (state_type s, symbol_type& sym);
      /// Assignment, needed by push_back.
      stack_symbol_type& operator= (const stack_symbol_type& that);
    };

    /// Stack type.
    typedef stack<stack_symbol_type> stack_type;

    /// The stack.
    stack_type yystack_;

    /// Push a new state on the stack.
    /// \param m    a debug message to display
    ///             if null, no trace is output.
    /// \param s    the symbol
    /// \warning the contents of \a s.value is stolen.
    void yypush_ (const char* m, stack_symbol_type& s);

    /// Push a new look ahead token on the state on the stack.
    /// \param m    a debug message to display
    ///             if null, no trace is output.
    /// \param s    the state
    /// \param sym  the symbol (for its value and location).
    /// \warning the contents of \a s.value is stolen.
    void yypush_ (const char* m, state_type s, symbol_type& sym);

    /// Pop \a n symbols the three stacks.
    void yypop_ (unsigned n = 1);

    /// Constants.
    enum
    {
      yyeof_ = 0,
      yylast_ = 5829,     ///< Last index in yytable_.
      yynnts_ = 208,  ///< Number of nonterminal symbols.
      yyfinal_ = 12, ///< Termination state number.
      yyterror_ = 1,
      yyerrcode_ = 256,
      yyntokens_ = 141  ///< Number of tokens.
    };


    // User arguments.
    driver& drv;
  };

  // Symbol number corresponding to token number t.
  inline
  parser::token_number_type
  parser::yytranslate_ (token_type t)
  {
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

  inline
  parser::syntax_error::syntax_error (const location_type& l, const std::string& m)
    : std::runtime_error (m)
    , location (l)
  {}

  // basic_symbol.
  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol ()
    : value ()
    , location ()
  {}

  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (const basic_symbol& other)
    : Base (other)
    , value ()
    , location (other.location)
  {
    switch (other.type_get ())
    {
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        value.copy< bool > (other.value);
        break;

      case 172: // prec
        value.copy< boost::optional<int> > (other.value);
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.copy< boost::optional<std::string> > (other.value);
        break;

      case 127: // "CHAR"
      case 131: // "PRIMCHAR"
        value.copy< char > (other.value);
        break;

      case 130: // "RATIONAL"
      case 136: // "PRIMDOUBLE"
        value.copy< double > (other.value);
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
      case 276: // flattenedpquals
      case 279: // transformqual
      case 285: // alt
      case 286: // alt_rhs
      case 287: // ralt
      case 289: // ifgdpats
      case 290: // gdpat
      case 291: // pat
      case 292: // bindpat
      case 293: // apat
      case 297: // stmt
      case 298: // qual
      case 345: // literal
        value.copy< expression_ref > (other.value);
        break;

      case 135: // "PRIMFLOAT"
        value.copy< float > (other.value);
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 348: // commas
        value.copy< int > (other.value);
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
      case 302: // qcon
      case 303: // gen_qcon
      case 304: // con
      case 306: // sysdcon_no_list
      case 307: // sysdcon
      case 308: // conop
      case 309: // qconop
      case 310: // gtycon
      case 311: // ntgtycon
      case 312: // oqtycon
      case 313: // oqtycon_no_varcon
      case 314: // qtyconop
      case 315: // qtycondoc
      case 316: // qtycon
      case 317: // tycon
      case 318: // qtyconsym
      case 319: // tyconsym
      case 320: // op
      case 321: // varop
      case 322: // qop
      case 323: // qopm
      case 324: // hole_op
      case 325: // qvarop
      case 326: // qvaropm
      case 327: // tyvar
      case 328: // tyvarop
      case 329: // tyvarid
      case 330: // var
      case 331: // qvar
      case 332: // qvarid
      case 333: // varid
      case 334: // qvarsym
      case 335: // qvarsym_no_minus
      case 336: // qvarsym1
      case 337: // varsym
      case 338: // varsym_no_minus
      case 339: // special_id
      case 340: // special_sym
      case 341: // qconid
      case 342: // conid
      case 343: // qconsym
      case 344: // consym
      case 347: // modid
        value.copy< std::string > (other.value);
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
      case 277: // pquals
      case 278: // squals
      case 280: // guardquals
      case 281: // guardquals1
      case 282: // altslist
      case 283: // alts
      case 284: // alts1
      case 288: // gdpats
      case 294: // apats1
      case 295: // stmtlist
      case 296: // stmts
        value.copy< std::vector<expression_ref> > (other.value);
        break;

      case 174: // ops
        value.copy< std::vector<std::string> > (other.value);
        break;

      default:
        break;
    }

  }

  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const semantic_type& v, const location_type& l)
    : Base (t)
    , value ()
    , location (l)
  {
    (void) v;
    switch (this->type_get ())
    {
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        value.copy< bool > (v);
        break;

      case 172: // prec
        value.copy< boost::optional<int> > (v);
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.copy< boost::optional<std::string> > (v);
        break;

      case 127: // "CHAR"
      case 131: // "PRIMCHAR"
        value.copy< char > (v);
        break;

      case 130: // "RATIONAL"
      case 136: // "PRIMDOUBLE"
        value.copy< double > (v);
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
      case 276: // flattenedpquals
      case 279: // transformqual
      case 285: // alt
      case 286: // alt_rhs
      case 287: // ralt
      case 289: // ifgdpats
      case 290: // gdpat
      case 291: // pat
      case 292: // bindpat
      case 293: // apat
      case 297: // stmt
      case 298: // qual
      case 345: // literal
        value.copy< expression_ref > (v);
        break;

      case 135: // "PRIMFLOAT"
        value.copy< float > (v);
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 348: // commas
        value.copy< int > (v);
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
      case 302: // qcon
      case 303: // gen_qcon
      case 304: // con
      case 306: // sysdcon_no_list
      case 307: // sysdcon
      case 308: // conop
      case 309: // qconop
      case 310: // gtycon
      case 311: // ntgtycon
      case 312: // oqtycon
      case 313: // oqtycon_no_varcon
      case 314: // qtyconop
      case 315: // qtycondoc
      case 316: // qtycon
      case 317: // tycon
      case 318: // qtyconsym
      case 319: // tyconsym
      case 320: // op
      case 321: // varop
      case 322: // qop
      case 323: // qopm
      case 324: // hole_op
      case 325: // qvarop
      case 326: // qvaropm
      case 327: // tyvar
      case 328: // tyvarop
      case 329: // tyvarid
      case 330: // var
      case 331: // qvar
      case 332: // qvarid
      case 333: // varid
      case 334: // qvarsym
      case 335: // qvarsym_no_minus
      case 336: // qvarsym1
      case 337: // varsym
      case 338: // varsym_no_minus
      case 339: // special_id
      case 340: // special_sym
      case 341: // qconid
      case 342: // conid
      case 343: // qconsym
      case 344: // consym
      case 347: // modid
        value.copy< std::string > (v);
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
      case 277: // pquals
      case 278: // squals
      case 280: // guardquals
      case 281: // guardquals1
      case 282: // altslist
      case 283: // alts
      case 284: // alts1
      case 288: // gdpats
      case 294: // apats1
      case 295: // stmtlist
      case 296: // stmts
        value.copy< std::vector<expression_ref> > (v);
        break;

      case 174: // ops
        value.copy< std::vector<std::string> > (v);
        break;

      default:
        break;
    }
}


  // Implementation of basic_symbol constructor for each type.

  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const location_type& l)
    : Base (t)
    , location (l)
  {}

  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const bool& v, const location_type& l)
    : Base (t)
    , value (v)
    , location (l)
  {}

  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const boost::optional<int>& v, const location_type& l)
    : Base (t)
    , value (v)
    , location (l)
  {}

  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const boost::optional<std::string>& v, const location_type& l)
    : Base (t)
    , value (v)
    , location (l)
  {}

  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const char& v, const location_type& l)
    : Base (t)
    , value (v)
    , location (l)
  {}

  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const double& v, const location_type& l)
    : Base (t)
    , value (v)
    , location (l)
  {}

  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const expression_ref& v, const location_type& l)
    : Base (t)
    , value (v)
    , location (l)
  {}

  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const float& v, const location_type& l)
    : Base (t)
    , value (v)
    , location (l)
  {}

  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const int& v, const location_type& l)
    : Base (t)
    , value (v)
    , location (l)
  {}

  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const std::string& v, const location_type& l)
    : Base (t)
    , value (v)
    , location (l)
  {}

  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const std::vector<expression_ref>& v, const location_type& l)
    : Base (t)
    , value (v)
    , location (l)
  {}

  template <typename Base>
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const std::vector<std::string>& v, const location_type& l)
    : Base (t)
    , value (v)
    , location (l)
  {}


  template <typename Base>
  parser::basic_symbol<Base>::~basic_symbol ()
  {
    clear ();
  }

  template <typename Base>
  void
  parser::basic_symbol<Base>::clear ()
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

      case 172: // prec
        value.template destroy< boost::optional<int> > ();
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.template destroy< boost::optional<std::string> > ();
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
      case 276: // flattenedpquals
      case 279: // transformqual
      case 285: // alt
      case 286: // alt_rhs
      case 287: // ralt
      case 289: // ifgdpats
      case 290: // gdpat
      case 291: // pat
      case 292: // bindpat
      case 293: // apat
      case 297: // stmt
      case 298: // qual
      case 345: // literal
        value.template destroy< expression_ref > ();
        break;

      case 135: // "PRIMFLOAT"
        value.template destroy< float > ();
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 348: // commas
        value.template destroy< int > ();
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
      case 302: // qcon
      case 303: // gen_qcon
      case 304: // con
      case 306: // sysdcon_no_list
      case 307: // sysdcon
      case 308: // conop
      case 309: // qconop
      case 310: // gtycon
      case 311: // ntgtycon
      case 312: // oqtycon
      case 313: // oqtycon_no_varcon
      case 314: // qtyconop
      case 315: // qtycondoc
      case 316: // qtycon
      case 317: // tycon
      case 318: // qtyconsym
      case 319: // tyconsym
      case 320: // op
      case 321: // varop
      case 322: // qop
      case 323: // qopm
      case 324: // hole_op
      case 325: // qvarop
      case 326: // qvaropm
      case 327: // tyvar
      case 328: // tyvarop
      case 329: // tyvarid
      case 330: // var
      case 331: // qvar
      case 332: // qvarid
      case 333: // varid
      case 334: // qvarsym
      case 335: // qvarsym_no_minus
      case 336: // qvarsym1
      case 337: // varsym
      case 338: // varsym_no_minus
      case 339: // special_id
      case 340: // special_sym
      case 341: // qconid
      case 342: // conid
      case 343: // qconsym
      case 344: // consym
      case 347: // modid
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
      case 277: // pquals
      case 278: // squals
      case 280: // guardquals
      case 281: // guardquals1
      case 282: // altslist
      case 283: // alts
      case 284: // alts1
      case 288: // gdpats
      case 294: // apats1
      case 295: // stmtlist
      case 296: // stmts
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

  template <typename Base>
  bool
  parser::basic_symbol<Base>::empty () const
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
        value.move< bool > (s.value);
        break;

      case 172: // prec
        value.move< boost::optional<int> > (s.value);
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.move< boost::optional<std::string> > (s.value);
        break;

      case 127: // "CHAR"
      case 131: // "PRIMCHAR"
        value.move< char > (s.value);
        break;

      case 130: // "RATIONAL"
      case 136: // "PRIMDOUBLE"
        value.move< double > (s.value);
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
      case 276: // flattenedpquals
      case 279: // transformqual
      case 285: // alt
      case 286: // alt_rhs
      case 287: // ralt
      case 289: // ifgdpats
      case 290: // gdpat
      case 291: // pat
      case 292: // bindpat
      case 293: // apat
      case 297: // stmt
      case 298: // qual
      case 345: // literal
        value.move< expression_ref > (s.value);
        break;

      case 135: // "PRIMFLOAT"
        value.move< float > (s.value);
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 348: // commas
        value.move< int > (s.value);
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
      case 302: // qcon
      case 303: // gen_qcon
      case 304: // con
      case 306: // sysdcon_no_list
      case 307: // sysdcon
      case 308: // conop
      case 309: // qconop
      case 310: // gtycon
      case 311: // ntgtycon
      case 312: // oqtycon
      case 313: // oqtycon_no_varcon
      case 314: // qtyconop
      case 315: // qtycondoc
      case 316: // qtycon
      case 317: // tycon
      case 318: // qtyconsym
      case 319: // tyconsym
      case 320: // op
      case 321: // varop
      case 322: // qop
      case 323: // qopm
      case 324: // hole_op
      case 325: // qvarop
      case 326: // qvaropm
      case 327: // tyvar
      case 328: // tyvarop
      case 329: // tyvarid
      case 330: // var
      case 331: // qvar
      case 332: // qvarid
      case 333: // varid
      case 334: // qvarsym
      case 335: // qvarsym_no_minus
      case 336: // qvarsym1
      case 337: // varsym
      case 338: // varsym_no_minus
      case 339: // special_id
      case 340: // special_sym
      case 341: // qconid
      case 342: // conid
      case 343: // qconsym
      case 344: // consym
      case 347: // modid
        value.move< std::string > (s.value);
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
      case 277: // pquals
      case 278: // squals
      case 280: // guardquals
      case 281: // guardquals1
      case 282: // altslist
      case 283: // alts
      case 284: // alts1
      case 288: // gdpats
      case 294: // apats1
      case 295: // stmtlist
      case 296: // stmts
        value.move< std::vector<expression_ref> > (s.value);
        break;

      case 174: // ops
        value.move< std::vector<std::string> > (s.value);
        break;

      default:
        break;
    }

    location = s.location;
  }

  // by_type.
  inline
  parser::by_type::by_type ()
    : type (empty_symbol)
  {}

  inline
  parser::by_type::by_type (const by_type& other)
    : type (other.type)
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
  parser::by_type::type_get () const
  {
    return type;
  }

  inline
  parser::token_type
  parser::by_type::token () const
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
    return static_cast<token_type> (yytoken_number_[type]);
  }
  // Implementation of make_symbol for each symbol type.
  parser::symbol_type
  parser::make_END (const location_type& l)
  {
    return symbol_type (token::TOK_END, l);
  }

  parser::symbol_type
  parser::make_UNDERSCORE (const location_type& l)
  {
    return symbol_type (token::TOK_UNDERSCORE, l);
  }

  parser::symbol_type
  parser::make_AS (const location_type& l)
  {
    return symbol_type (token::TOK_AS, l);
  }

  parser::symbol_type
  parser::make_CASE (const location_type& l)
  {
    return symbol_type (token::TOK_CASE, l);
  }

  parser::symbol_type
  parser::make_DATA (const location_type& l)
  {
    return symbol_type (token::TOK_DATA, l);
  }

  parser::symbol_type
  parser::make_DEFAULT (const location_type& l)
  {
    return symbol_type (token::TOK_DEFAULT, l);
  }

  parser::symbol_type
  parser::make_DERIVING (const location_type& l)
  {
    return symbol_type (token::TOK_DERIVING, l);
  }

  parser::symbol_type
  parser::make_DO (const location_type& l)
  {
    return symbol_type (token::TOK_DO, l);
  }

  parser::symbol_type
  parser::make_ELSE (const location_type& l)
  {
    return symbol_type (token::TOK_ELSE, l);
  }

  parser::symbol_type
  parser::make_HIDING (const location_type& l)
  {
    return symbol_type (token::TOK_HIDING, l);
  }

  parser::symbol_type
  parser::make_IF (const location_type& l)
  {
    return symbol_type (token::TOK_IF, l);
  }

  parser::symbol_type
  parser::make_IMPORT (const location_type& l)
  {
    return symbol_type (token::TOK_IMPORT, l);
  }

  parser::symbol_type
  parser::make_IN (const location_type& l)
  {
    return symbol_type (token::TOK_IN, l);
  }

  parser::symbol_type
  parser::make_INFIX (const location_type& l)
  {
    return symbol_type (token::TOK_INFIX, l);
  }

  parser::symbol_type
  parser::make_INFIXL (const location_type& l)
  {
    return symbol_type (token::TOK_INFIXL, l);
  }

  parser::symbol_type
  parser::make_INFIXR (const location_type& l)
  {
    return symbol_type (token::TOK_INFIXR, l);
  }

  parser::symbol_type
  parser::make_INSTANCE (const location_type& l)
  {
    return symbol_type (token::TOK_INSTANCE, l);
  }

  parser::symbol_type
  parser::make_LET (const location_type& l)
  {
    return symbol_type (token::TOK_LET, l);
  }

  parser::symbol_type
  parser::make_MODULE (const location_type& l)
  {
    return symbol_type (token::TOK_MODULE, l);
  }

  parser::symbol_type
  parser::make_NEWTYPE (const location_type& l)
  {
    return symbol_type (token::TOK_NEWTYPE, l);
  }

  parser::symbol_type
  parser::make_OF (const location_type& l)
  {
    return symbol_type (token::TOK_OF, l);
  }

  parser::symbol_type
  parser::make_QUALIFIED (const location_type& l)
  {
    return symbol_type (token::TOK_QUALIFIED, l);
  }

  parser::symbol_type
  parser::make_THEN (const location_type& l)
  {
    return symbol_type (token::TOK_THEN, l);
  }

  parser::symbol_type
  parser::make_TYPE (const location_type& l)
  {
    return symbol_type (token::TOK_TYPE, l);
  }

  parser::symbol_type
  parser::make_WHERE (const location_type& l)
  {
    return symbol_type (token::TOK_WHERE, l);
  }

  parser::symbol_type
  parser::make_BUILTIN (const location_type& l)
  {
    return symbol_type (token::TOK_BUILTIN, l);
  }

  parser::symbol_type
  parser::make_FORALL (const location_type& l)
  {
    return symbol_type (token::TOK_FORALL, l);
  }

  parser::symbol_type
  parser::make_FOREIGN (const location_type& l)
  {
    return symbol_type (token::TOK_FOREIGN, l);
  }

  parser::symbol_type
  parser::make_EXPORT (const location_type& l)
  {
    return symbol_type (token::TOK_EXPORT, l);
  }

  parser::symbol_type
  parser::make_LABEL (const location_type& l)
  {
    return symbol_type (token::TOK_LABEL, l);
  }

  parser::symbol_type
  parser::make_DYNAMIC (const location_type& l)
  {
    return symbol_type (token::TOK_DYNAMIC, l);
  }

  parser::symbol_type
  parser::make_SAFE (const location_type& l)
  {
    return symbol_type (token::TOK_SAFE, l);
  }

  parser::symbol_type
  parser::make_INTERRUPTIBLE (const location_type& l)
  {
    return symbol_type (token::TOK_INTERRUPTIBLE, l);
  }

  parser::symbol_type
  parser::make_UNSAFE (const location_type& l)
  {
    return symbol_type (token::TOK_UNSAFE, l);
  }

  parser::symbol_type
  parser::make_MDO (const location_type& l)
  {
    return symbol_type (token::TOK_MDO, l);
  }

  parser::symbol_type
  parser::make_FAMILY (const location_type& l)
  {
    return symbol_type (token::TOK_FAMILY, l);
  }

  parser::symbol_type
  parser::make_ROLE (const location_type& l)
  {
    return symbol_type (token::TOK_ROLE, l);
  }

  parser::symbol_type
  parser::make_STDCALL (const location_type& l)
  {
    return symbol_type (token::TOK_STDCALL, l);
  }

  parser::symbol_type
  parser::make_CCALL (const location_type& l)
  {
    return symbol_type (token::TOK_CCALL, l);
  }

  parser::symbol_type
  parser::make_CAPI (const location_type& l)
  {
    return symbol_type (token::TOK_CAPI, l);
  }

  parser::symbol_type
  parser::make_PRIM (const location_type& l)
  {
    return symbol_type (token::TOK_PRIM, l);
  }

  parser::symbol_type
  parser::make_JAVASCRIPT (const location_type& l)
  {
    return symbol_type (token::TOK_JAVASCRIPT, l);
  }

  parser::symbol_type
  parser::make_PROC (const location_type& l)
  {
    return symbol_type (token::TOK_PROC, l);
  }

  parser::symbol_type
  parser::make_REC (const location_type& l)
  {
    return symbol_type (token::TOK_REC, l);
  }

  parser::symbol_type
  parser::make_GROUP (const location_type& l)
  {
    return symbol_type (token::TOK_GROUP, l);
  }

  parser::symbol_type
  parser::make_BY (const location_type& l)
  {
    return symbol_type (token::TOK_BY, l);
  }

  parser::symbol_type
  parser::make_USING (const location_type& l)
  {
    return symbol_type (token::TOK_USING, l);
  }

  parser::symbol_type
  parser::make_PATTERN (const location_type& l)
  {
    return symbol_type (token::TOK_PATTERN, l);
  }

  parser::symbol_type
  parser::make_STATIC (const location_type& l)
  {
    return symbol_type (token::TOK_STATIC, l);
  }

  parser::symbol_type
  parser::make_STOCK (const location_type& l)
  {
    return symbol_type (token::TOK_STOCK, l);
  }

  parser::symbol_type
  parser::make_ANYCLASS (const location_type& l)
  {
    return symbol_type (token::TOK_ANYCLASS, l);
  }

  parser::symbol_type
  parser::make_VIA (const location_type& l)
  {
    return symbol_type (token::TOK_VIA, l);
  }

  parser::symbol_type
  parser::make_UNIT (const location_type& l)
  {
    return symbol_type (token::TOK_UNIT, l);
  }

  parser::symbol_type
  parser::make_SIGNATURE (const location_type& l)
  {
    return symbol_type (token::TOK_SIGNATURE, l);
  }

  parser::symbol_type
  parser::make_DEPENDENCY (const location_type& l)
  {
    return symbol_type (token::TOK_DEPENDENCY, l);
  }

  parser::symbol_type
  parser::make_INLINE_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_INLINE_PRAG, l);
  }

  parser::symbol_type
  parser::make_SPECIALIZE_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_SPECIALIZE_PRAG, l);
  }

  parser::symbol_type
  parser::make_SPECIALIZE_INLINE_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_SPECIALIZE_INLINE_PRAG, l);
  }

  parser::symbol_type
  parser::make_SOURCE_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_SOURCE_PRAG, l);
  }

  parser::symbol_type
  parser::make_RULES_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_RULES_PRAG, l);
  }

  parser::symbol_type
  parser::make_CORE_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_CORE_PRAG, l);
  }

  parser::symbol_type
  parser::make_SCC_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_SCC_PRAG, l);
  }

  parser::symbol_type
  parser::make_GENERATED_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_GENERATED_PRAG, l);
  }

  parser::symbol_type
  parser::make_DEPRECATED_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_DEPRECATED_PRAG, l);
  }

  parser::symbol_type
  parser::make_WARNING_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_WARNING_PRAG, l);
  }

  parser::symbol_type
  parser::make_UNPACK_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_UNPACK_PRAG, l);
  }

  parser::symbol_type
  parser::make_NOUNPACK_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_NOUNPACK_PRAG, l);
  }

  parser::symbol_type
  parser::make_ANN_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_ANN_PRAG, l);
  }

  parser::symbol_type
  parser::make_MINIMAL_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_MINIMAL_PRAG, l);
  }

  parser::symbol_type
  parser::make_CTYPE_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_CTYPE_PRAG, l);
  }

  parser::symbol_type
  parser::make_OVERLAPPING_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_OVERLAPPING_PRAG, l);
  }

  parser::symbol_type
  parser::make_OVERLAPPABLE_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_OVERLAPPABLE_PRAG, l);
  }

  parser::symbol_type
  parser::make_OVERLAPS_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_OVERLAPS_PRAG, l);
  }

  parser::symbol_type
  parser::make_INCOHERENT_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_INCOHERENT_PRAG, l);
  }

  parser::symbol_type
  parser::make_COMPLETE_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_COMPLETE_PRAG, l);
  }

  parser::symbol_type
  parser::make_CLOSE_PRAG (const location_type& l)
  {
    return symbol_type (token::TOK_CLOSE_PRAG, l);
  }

  parser::symbol_type
  parser::make_DOTDOT (const location_type& l)
  {
    return symbol_type (token::TOK_DOTDOT, l);
  }

  parser::symbol_type
  parser::make_COLON (const location_type& l)
  {
    return symbol_type (token::TOK_COLON, l);
  }

  parser::symbol_type
  parser::make_DCOLON (const location_type& l)
  {
    return symbol_type (token::TOK_DCOLON, l);
  }

  parser::symbol_type
  parser::make_EQUAL (const location_type& l)
  {
    return symbol_type (token::TOK_EQUAL, l);
  }

  parser::symbol_type
  parser::make_LAM (const location_type& l)
  {
    return symbol_type (token::TOK_LAM, l);
  }

  parser::symbol_type
  parser::make_LCASE (const location_type& l)
  {
    return symbol_type (token::TOK_LCASE, l);
  }

  parser::symbol_type
  parser::make_VBAR (const location_type& l)
  {
    return symbol_type (token::TOK_VBAR, l);
  }

  parser::symbol_type
  parser::make_LARROW (const location_type& l)
  {
    return symbol_type (token::TOK_LARROW, l);
  }

  parser::symbol_type
  parser::make_RARROW (const location_type& l)
  {
    return symbol_type (token::TOK_RARROW, l);
  }

  parser::symbol_type
  parser::make_AT (const location_type& l)
  {
    return symbol_type (token::TOK_AT, l);
  }

  parser::symbol_type
  parser::make_TILDE (const location_type& l)
  {
    return symbol_type (token::TOK_TILDE, l);
  }

  parser::symbol_type
  parser::make_DARROW (const location_type& l)
  {
    return symbol_type (token::TOK_DARROW, l);
  }

  parser::symbol_type
  parser::make_MINUS (const location_type& l)
  {
    return symbol_type (token::TOK_MINUS, l);
  }

  parser::symbol_type
  parser::make_BANG (const location_type& l)
  {
    return symbol_type (token::TOK_BANG, l);
  }

  parser::symbol_type
  parser::make_STAR (const location_type& l)
  {
    return symbol_type (token::TOK_STAR, l);
  }

  parser::symbol_type
  parser::make_lARROWTAIL (const location_type& l)
  {
    return symbol_type (token::TOK_lARROWTAIL, l);
  }

  parser::symbol_type
  parser::make_rARROWTAIL (const location_type& l)
  {
    return symbol_type (token::TOK_rARROWTAIL, l);
  }

  parser::symbol_type
  parser::make_LARROWTAIL (const location_type& l)
  {
    return symbol_type (token::TOK_LARROWTAIL, l);
  }

  parser::symbol_type
  parser::make_RARROWTAIL (const location_type& l)
  {
    return symbol_type (token::TOK_RARROWTAIL, l);
  }

  parser::symbol_type
  parser::make_DOT (const location_type& l)
  {
    return symbol_type (token::TOK_DOT, l);
  }

  parser::symbol_type
  parser::make_TYPEAPP (const location_type& l)
  {
    return symbol_type (token::TOK_TYPEAPP, l);
  }

  parser::symbol_type
  parser::make_OCURLY (const location_type& l)
  {
    return symbol_type (token::TOK_OCURLY, l);
  }

  parser::symbol_type
  parser::make_CCURLY (const location_type& l)
  {
    return symbol_type (token::TOK_CCURLY, l);
  }

  parser::symbol_type
  parser::make_VOCURLY (const location_type& l)
  {
    return symbol_type (token::TOK_VOCURLY, l);
  }

  parser::symbol_type
  parser::make_VCCURLY (const location_type& l)
  {
    return symbol_type (token::TOK_VCCURLY, l);
  }

  parser::symbol_type
  parser::make_OBRACK (const location_type& l)
  {
    return symbol_type (token::TOK_OBRACK, l);
  }

  parser::symbol_type
  parser::make_CBRACK (const location_type& l)
  {
    return symbol_type (token::TOK_CBRACK, l);
  }

  parser::symbol_type
  parser::make_OPABRACK (const location_type& l)
  {
    return symbol_type (token::TOK_OPABRACK, l);
  }

  parser::symbol_type
  parser::make_CPABRACK (const location_type& l)
  {
    return symbol_type (token::TOK_CPABRACK, l);
  }

  parser::symbol_type
  parser::make_OPAREN (const location_type& l)
  {
    return symbol_type (token::TOK_OPAREN, l);
  }

  parser::symbol_type
  parser::make_CPAREN (const location_type& l)
  {
    return symbol_type (token::TOK_CPAREN, l);
  }

  parser::symbol_type
  parser::make_OUBXPAREN (const location_type& l)
  {
    return symbol_type (token::TOK_OUBXPAREN, l);
  }

  parser::symbol_type
  parser::make_CUBXPAREN (const location_type& l)
  {
    return symbol_type (token::TOK_CUBXPAREN, l);
  }

  parser::symbol_type
  parser::make_OPARENBAR (const location_type& l)
  {
    return symbol_type (token::TOK_OPARENBAR, l);
  }

  parser::symbol_type
  parser::make_CPARENBAR (const location_type& l)
  {
    return symbol_type (token::TOK_CPARENBAR, l);
  }

  parser::symbol_type
  parser::make_SEMI (const location_type& l)
  {
    return symbol_type (token::TOK_SEMI, l);
  }

  parser::symbol_type
  parser::make_COMMA (const location_type& l)
  {
    return symbol_type (token::TOK_COMMA, l);
  }

  parser::symbol_type
  parser::make_BACKQUOTE (const location_type& l)
  {
    return symbol_type (token::TOK_BACKQUOTE, l);
  }

  parser::symbol_type
  parser::make_SIMPLEQUOTE (const location_type& l)
  {
    return symbol_type (token::TOK_SIMPLEQUOTE, l);
  }

  parser::symbol_type
  parser::make_VARID (const std::string& v, const location_type& l)
  {
    return symbol_type (token::TOK_VARID, v, l);
  }

  parser::symbol_type
  parser::make_CONID (const std::string& v, const location_type& l)
  {
    return symbol_type (token::TOK_CONID, v, l);
  }

  parser::symbol_type
  parser::make_VARSYM (const std::string& v, const location_type& l)
  {
    return symbol_type (token::TOK_VARSYM, v, l);
  }

  parser::symbol_type
  parser::make_CONSYM (const std::string& v, const location_type& l)
  {
    return symbol_type (token::TOK_CONSYM, v, l);
  }

  parser::symbol_type
  parser::make_QVARID (const std::string& v, const location_type& l)
  {
    return symbol_type (token::TOK_QVARID, v, l);
  }

  parser::symbol_type
  parser::make_QCONID (const std::string& v, const location_type& l)
  {
    return symbol_type (token::TOK_QCONID, v, l);
  }

  parser::symbol_type
  parser::make_QVARSYM (const std::string& v, const location_type& l)
  {
    return symbol_type (token::TOK_QVARSYM, v, l);
  }

  parser::symbol_type
  parser::make_QCONSYM (const std::string& v, const location_type& l)
  {
    return symbol_type (token::TOK_QCONSYM, v, l);
  }

  parser::symbol_type
  parser::make_IPDUPVARID (const std::string& v, const location_type& l)
  {
    return symbol_type (token::TOK_IPDUPVARID, v, l);
  }

  parser::symbol_type
  parser::make_LABELVARID (const std::string& v, const location_type& l)
  {
    return symbol_type (token::TOK_LABELVARID, v, l);
  }

  parser::symbol_type
  parser::make_CHAR (const char& v, const location_type& l)
  {
    return symbol_type (token::TOK_CHAR, v, l);
  }

  parser::symbol_type
  parser::make_STRING (const std::string& v, const location_type& l)
  {
    return symbol_type (token::TOK_STRING, v, l);
  }

  parser::symbol_type
  parser::make_INTEGER (const int& v, const location_type& l)
  {
    return symbol_type (token::TOK_INTEGER, v, l);
  }

  parser::symbol_type
  parser::make_RATIONAL (const double& v, const location_type& l)
  {
    return symbol_type (token::TOK_RATIONAL, v, l);
  }

  parser::symbol_type
  parser::make_PRIMCHAR (const char& v, const location_type& l)
  {
    return symbol_type (token::TOK_PRIMCHAR, v, l);
  }

  parser::symbol_type
  parser::make_PRIMSTRING (const std::string& v, const location_type& l)
  {
    return symbol_type (token::TOK_PRIMSTRING, v, l);
  }

  parser::symbol_type
  parser::make_PRIMINTEGER (const int& v, const location_type& l)
  {
    return symbol_type (token::TOK_PRIMINTEGER, v, l);
  }

  parser::symbol_type
  parser::make_PRINTWORD (const int& v, const location_type& l)
  {
    return symbol_type (token::TOK_PRINTWORD, v, l);
  }

  parser::symbol_type
  parser::make_PRIMFLOAT (const float& v, const location_type& l)
  {
    return symbol_type (token::TOK_PRIMFLOAT, v, l);
  }

  parser::symbol_type
  parser::make_PRIMDOUBLE (const double& v, const location_type& l)
  {
    return symbol_type (token::TOK_PRIMDOUBLE, v, l);
  }



} // yy
#line 3549 "parser.hh" // lalr1.cc:380




#endif // !YY_YY_PARSER_HH_INCLUDED
