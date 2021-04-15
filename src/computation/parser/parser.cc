// A Bison parser, made by GNU Bison 3.7.5.

// Skeleton implementation for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015, 2018-2021 Free Software Foundation, Inc.

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

// DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
// especially those whose name start with YY_ or yy_.  They are
// private implementation details that can be changed or removed.





#include "parser.hh"


// Unqualified %code blocks.
#line 97 "parser.y"

# include "driver.hh"

#line 50 "parser.cc"


#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> // FIXME: INFRINGES ON USER NAME SPACE.
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif


// Whether we are compiled with exception support.
#ifndef YY_EXCEPTIONS
# if defined __GNUC__ && !defined __EXCEPTIONS
#  define YY_EXCEPTIONS 0
# else
#  define YY_EXCEPTIONS 1
# endif
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K].location)
/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

# ifndef YYLLOC_DEFAULT
#  define YYLLOC_DEFAULT(Current, Rhs, N)                               \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).begin  = YYRHSLOC (Rhs, 1).begin;                   \
          (Current).end    = YYRHSLOC (Rhs, N).end;                     \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).begin = (Current).end = YYRHSLOC (Rhs, 0).end;      \
        }                                                               \
    while (false)
# endif


// Enable debugging if requested.
#if YYDEBUG

// A pseudo ostream that takes yydebug_ into account.
# define YYCDEBUG if (yydebug_) (*yycdebug_)

# define YY_SYMBOL_PRINT(Title, Symbol)         \
  do {                                          \
    if (yydebug_)                               \
    {                                           \
      *yycdebug_ << Title << ' ';               \
      yy_print_ (*yycdebug_, Symbol);           \
      *yycdebug_ << '\n';                       \
    }                                           \
  } while (false)

# define YY_REDUCE_PRINT(Rule)          \
  do {                                  \
    if (yydebug_)                       \
      yy_reduce_print_ (Rule);          \
  } while (false)

# define YY_STACK_PRINT()               \
  do {                                  \
    if (yydebug_)                       \
      yy_stack_print_ ();                \
  } while (false)

#else // !YYDEBUG

# define YYCDEBUG if (false) std::cerr
# define YY_SYMBOL_PRINT(Title, Symbol)  YY_USE (Symbol)
# define YY_REDUCE_PRINT(Rule)           static_cast<void> (0)
# define YY_STACK_PRINT()                static_cast<void> (0)

#endif // !YYDEBUG

#define yyerrok         (yyerrstatus_ = 0)
#define yyclearin       (yyla.clear ())

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYRECOVERING()  (!!yyerrstatus_)

namespace yy {
#line 142 "parser.cc"

  /// Build a parser object.
  parser::parser (driver& drv_yyarg)
#if YYDEBUG
    : yydebug_ (false),
      yycdebug_ (&std::cerr),
#else
    :
#endif
      drv (drv_yyarg)
  {}

  parser::~parser ()
  {}

  parser::syntax_error::~syntax_error () YY_NOEXCEPT YY_NOTHROW
  {}

  /*---------------.
  | symbol kinds.  |
  `---------------*/



  // by_state.
  parser::by_state::by_state () YY_NOEXCEPT
    : state (empty_state)
  {}

  parser::by_state::by_state (const by_state& that) YY_NOEXCEPT
    : state (that.state)
  {}

  void
  parser::by_state::clear () YY_NOEXCEPT
  {
    state = empty_state;
  }

  void
  parser::by_state::move (by_state& that)
  {
    state = that.state;
    that.clear ();
  }

  parser::by_state::by_state (state_type s) YY_NOEXCEPT
    : state (s)
  {}

  parser::symbol_kind_type
  parser::by_state::kind () const YY_NOEXCEPT
  {
    if (state == empty_state)
      return symbol_kind::S_YYEMPTY;
    else
      return YY_CAST (symbol_kind_type, yystos_[+state]);
  }

  parser::stack_symbol_type::stack_symbol_type ()
  {}

  parser::stack_symbol_type::stack_symbol_type (YY_RVREF (stack_symbol_type) that)
    : super_type (YY_MOVE (that.state), YY_MOVE (that.location))
  {
    switch (that.kind ())
    {
      case symbol_kind::S_altslist: // altslist
        value.YY_MOVE_OR_COPY< Haskell::Alts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_constr: // constr
        value.YY_MOVE_OR_COPY< Haskell::Constructor > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        value.YY_MOVE_OR_COPY< Haskell::Context > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        value.YY_MOVE_OR_COPY< Haskell::DataOrNewtype > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.YY_MOVE_OR_COPY< Haskell::FieldDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infix: // infix
        value.YY_MOVE_OR_COPY< Haskell::Fixity > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.YY_MOVE_OR_COPY< Haskell::GuardedRHS > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.YY_MOVE_OR_COPY< Haskell::Stmts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_strict_mark: // strict_mark
      case symbol_kind::S_strictness: // strictness
        value.YY_MOVE_OR_COPY< Haskell::StrictLazy > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alt: // alt
        value.YY_MOVE_OR_COPY< Located<Haskell::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
        value.YY_MOVE_OR_COPY< Located<Haskell::Decls> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybe_src: // maybe_src
      case symbol_kind::S_maybe_safe: // maybe_safe
      case symbol_kind::S_optqualified: // optqualified
        value.YY_MOVE_OR_COPY< bool > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_CHAR: // "CHAR"
      case symbol_kind::S_PRIMCHAR: // "PRIMCHAR"
        value.YY_MOVE_OR_COPY< char > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_RATIONAL: // "RATIONAL"
      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.YY_MOVE_OR_COPY< double > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_module: // module
      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
      case symbol_kind::S_maybeexports: // maybeexports
      case symbol_kind::S_export: // export
      case symbol_kind::S_qcname_ext_w_wildcard: // qcname_ext_w_wildcard
      case symbol_kind::S_qcname_ext: // qcname_ext
      case symbol_kind::S_qcname: // qcname
      case symbol_kind::S_importdecl: // importdecl
      case symbol_kind::S_maybeimpspec: // maybeimpspec
      case symbol_kind::S_impspec: // impspec
      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ctype: // ctype
      case symbol_kind::S_ctypedoc: // ctypedoc
      case symbol_kind::S_type: // type
      case symbol_kind::S_typedoc: // typedoc
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_tyapp: // tyapp
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_forall: // forall
      case symbol_kind::S_constr_stuff: // constr_stuff
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_alt_rhs: // alt_rhs
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
      case symbol_kind::S_literal: // literal
        value.YY_MOVE_OR_COPY< expression_ref > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        value.YY_MOVE_OR_COPY< float > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        value.YY_MOVE_OR_COPY< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.YY_MOVE_OR_COPY< std::optional<Located<Haskell::Decls>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_prec: // prec
        value.YY_MOVE_OR_COPY< std::optional<int> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybe_pkg: // maybe_pkg
      case symbol_kind::S_maybeas: // maybeas
        value.YY_MOVE_OR_COPY< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.YY_MOVE_OR_COPY< std::pair<Haskell::Context,expression_ref> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_CONID: // "CONID"
      case symbol_kind::S_VARSYM: // "VARSYM"
      case symbol_kind::S_CONSYM: // "CONSYM"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_QCONID: // "QCONID"
      case symbol_kind::S_QVARSYM: // "QVARSYM"
      case symbol_kind::S_QCONSYM: // "QCONSYM"
      case symbol_kind::S_IPDUPVARID: // "IPDUPVARID"
      case symbol_kind::S_LABELVARID: // "LABELVARID"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_PRIMSTRING: // "PRIMSTRING"
      case symbol_kind::S_qcon: // qcon
      case symbol_kind::S_gen_qcon: // gen_qcon
      case symbol_kind::S_con: // con
      case symbol_kind::S_sysdcon_no_list: // sysdcon_no_list
      case symbol_kind::S_sysdcon: // sysdcon
      case symbol_kind::S_conop: // conop
      case symbol_kind::S_qconop: // qconop
      case symbol_kind::S_gtycon: // gtycon
      case symbol_kind::S_ntgtycon: // ntgtycon
      case symbol_kind::S_oqtycon: // oqtycon
      case symbol_kind::S_oqtycon_no_varcon: // oqtycon_no_varcon
      case symbol_kind::S_qtyconop: // qtyconop
      case symbol_kind::S_qtycondoc: // qtycondoc
      case symbol_kind::S_qtycon: // qtycon
      case symbol_kind::S_tycon: // tycon
      case symbol_kind::S_qtyconsym: // qtyconsym
      case symbol_kind::S_tyconsym: // tyconsym
      case symbol_kind::S_op: // op
      case symbol_kind::S_varop: // varop
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_hole_op: // hole_op
      case symbol_kind::S_qvarop: // qvarop
      case symbol_kind::S_qvaropm: // qvaropm
      case symbol_kind::S_tyvar: // tyvar
      case symbol_kind::S_tyvarop: // tyvarop
      case symbol_kind::S_tyvarid: // tyvarid
      case symbol_kind::S_var: // var
      case symbol_kind::S_qvar: // qvar
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
      case symbol_kind::S_qvarsym: // qvarsym
      case symbol_kind::S_qvarsym_no_minus: // qvarsym_no_minus
      case symbol_kind::S_qvarsym1: // qvarsym1
      case symbol_kind::S_varsym: // varsym
      case symbol_kind::S_varsym_no_minus: // varsym_no_minus
      case symbol_kind::S_special_id: // special_id
      case symbol_kind::S_special_sym: // special_sym
      case symbol_kind::S_qconid: // qconid
      case symbol_kind::S_conid: // conid
      case symbol_kind::S_qconsym: // qconsym
      case symbol_kind::S_consym: // consym
      case symbol_kind::S_modid: // modid
        value.YY_MOVE_OR_COPY< std::string > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        value.YY_MOVE_OR_COPY< std::vector<Haskell::Constructor> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.YY_MOVE_OR_COPY< std::vector<Haskell::FieldDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.YY_MOVE_OR_COPY< std::vector<Haskell::GuardedRHS> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.YY_MOVE_OR_COPY< std::vector<Located<Haskell::Alt>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
      case symbol_kind::S_decls: // decls
      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        value.YY_MOVE_OR_COPY< std::vector<expression_ref> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ops: // ops
      case symbol_kind::S_sig_vars: // sig_vars
        value.YY_MOVE_OR_COPY< std::vector<std::string> > (YY_MOVE (that.value));
        break;

      default:
        break;
    }

#if 201103L <= YY_CPLUSPLUS
    // that is emptied.
    that.state = empty_state;
#endif
  }

  parser::stack_symbol_type::stack_symbol_type (state_type s, YY_MOVE_REF (symbol_type) that)
    : super_type (s, YY_MOVE (that.location))
  {
    switch (that.kind ())
    {
      case symbol_kind::S_altslist: // altslist
        value.move< Haskell::Alts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_constr: // constr
        value.move< Haskell::Constructor > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        value.move< Haskell::Context > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        value.move< Haskell::DataOrNewtype > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.move< Haskell::FieldDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infix: // infix
        value.move< Haskell::Fixity > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.move< Haskell::GuardedRHS > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.move< Haskell::Stmts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_strict_mark: // strict_mark
      case symbol_kind::S_strictness: // strictness
        value.move< Haskell::StrictLazy > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alt: // alt
        value.move< Located<Haskell::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
        value.move< Located<Haskell::Decls> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybe_src: // maybe_src
      case symbol_kind::S_maybe_safe: // maybe_safe
      case symbol_kind::S_optqualified: // optqualified
        value.move< bool > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_CHAR: // "CHAR"
      case symbol_kind::S_PRIMCHAR: // "PRIMCHAR"
        value.move< char > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_RATIONAL: // "RATIONAL"
      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.move< double > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_module: // module
      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
      case symbol_kind::S_maybeexports: // maybeexports
      case symbol_kind::S_export: // export
      case symbol_kind::S_qcname_ext_w_wildcard: // qcname_ext_w_wildcard
      case symbol_kind::S_qcname_ext: // qcname_ext
      case symbol_kind::S_qcname: // qcname
      case symbol_kind::S_importdecl: // importdecl
      case symbol_kind::S_maybeimpspec: // maybeimpspec
      case symbol_kind::S_impspec: // impspec
      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ctype: // ctype
      case symbol_kind::S_ctypedoc: // ctypedoc
      case symbol_kind::S_type: // type
      case symbol_kind::S_typedoc: // typedoc
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_tyapp: // tyapp
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_forall: // forall
      case symbol_kind::S_constr_stuff: // constr_stuff
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_alt_rhs: // alt_rhs
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
      case symbol_kind::S_literal: // literal
        value.move< expression_ref > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        value.move< float > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        value.move< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.move< std::optional<Located<Haskell::Decls>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_prec: // prec
        value.move< std::optional<int> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybe_pkg: // maybe_pkg
      case symbol_kind::S_maybeas: // maybeas
        value.move< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.move< std::pair<Haskell::Context,expression_ref> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_CONID: // "CONID"
      case symbol_kind::S_VARSYM: // "VARSYM"
      case symbol_kind::S_CONSYM: // "CONSYM"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_QCONID: // "QCONID"
      case symbol_kind::S_QVARSYM: // "QVARSYM"
      case symbol_kind::S_QCONSYM: // "QCONSYM"
      case symbol_kind::S_IPDUPVARID: // "IPDUPVARID"
      case symbol_kind::S_LABELVARID: // "LABELVARID"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_PRIMSTRING: // "PRIMSTRING"
      case symbol_kind::S_qcon: // qcon
      case symbol_kind::S_gen_qcon: // gen_qcon
      case symbol_kind::S_con: // con
      case symbol_kind::S_sysdcon_no_list: // sysdcon_no_list
      case symbol_kind::S_sysdcon: // sysdcon
      case symbol_kind::S_conop: // conop
      case symbol_kind::S_qconop: // qconop
      case symbol_kind::S_gtycon: // gtycon
      case symbol_kind::S_ntgtycon: // ntgtycon
      case symbol_kind::S_oqtycon: // oqtycon
      case symbol_kind::S_oqtycon_no_varcon: // oqtycon_no_varcon
      case symbol_kind::S_qtyconop: // qtyconop
      case symbol_kind::S_qtycondoc: // qtycondoc
      case symbol_kind::S_qtycon: // qtycon
      case symbol_kind::S_tycon: // tycon
      case symbol_kind::S_qtyconsym: // qtyconsym
      case symbol_kind::S_tyconsym: // tyconsym
      case symbol_kind::S_op: // op
      case symbol_kind::S_varop: // varop
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_hole_op: // hole_op
      case symbol_kind::S_qvarop: // qvarop
      case symbol_kind::S_qvaropm: // qvaropm
      case symbol_kind::S_tyvar: // tyvar
      case symbol_kind::S_tyvarop: // tyvarop
      case symbol_kind::S_tyvarid: // tyvarid
      case symbol_kind::S_var: // var
      case symbol_kind::S_qvar: // qvar
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
      case symbol_kind::S_qvarsym: // qvarsym
      case symbol_kind::S_qvarsym_no_minus: // qvarsym_no_minus
      case symbol_kind::S_qvarsym1: // qvarsym1
      case symbol_kind::S_varsym: // varsym
      case symbol_kind::S_varsym_no_minus: // varsym_no_minus
      case symbol_kind::S_special_id: // special_id
      case symbol_kind::S_special_sym: // special_sym
      case symbol_kind::S_qconid: // qconid
      case symbol_kind::S_conid: // conid
      case symbol_kind::S_qconsym: // qconsym
      case symbol_kind::S_consym: // consym
      case symbol_kind::S_modid: // modid
        value.move< std::string > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        value.move< std::vector<Haskell::Constructor> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.move< std::vector<Haskell::FieldDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.move< std::vector<Haskell::GuardedRHS> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Located<Haskell::Alt>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
      case symbol_kind::S_decls: // decls
      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        value.move< std::vector<expression_ref> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ops: // ops
      case symbol_kind::S_sig_vars: // sig_vars
        value.move< std::vector<std::string> > (YY_MOVE (that.value));
        break;

      default:
        break;
    }

    // that is emptied.
    that.kind_ = symbol_kind::S_YYEMPTY;
  }

#if YY_CPLUSPLUS < 201103L
  parser::stack_symbol_type&
  parser::stack_symbol_type::operator= (const stack_symbol_type& that)
  {
    state = that.state;
    switch (that.kind ())
    {
      case symbol_kind::S_altslist: // altslist
        value.copy< Haskell::Alts > (that.value);
        break;

      case symbol_kind::S_constr: // constr
        value.copy< Haskell::Constructor > (that.value);
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        value.copy< Haskell::Context > (that.value);
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        value.copy< Haskell::DataOrNewtype > (that.value);
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.copy< Haskell::FieldDecl > (that.value);
        break;

      case symbol_kind::S_infix: // infix
        value.copy< Haskell::Fixity > (that.value);
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.copy< Haskell::GuardedRHS > (that.value);
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.copy< Haskell::Stmts > (that.value);
        break;

      case symbol_kind::S_strict_mark: // strict_mark
      case symbol_kind::S_strictness: // strictness
        value.copy< Haskell::StrictLazy > (that.value);
        break;

      case symbol_kind::S_alt: // alt
        value.copy< Located<Haskell::Alt> > (that.value);
        break;

      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
        value.copy< Located<Haskell::Decls> > (that.value);
        break;

      case symbol_kind::S_maybe_src: // maybe_src
      case symbol_kind::S_maybe_safe: // maybe_safe
      case symbol_kind::S_optqualified: // optqualified
        value.copy< bool > (that.value);
        break;

      case symbol_kind::S_CHAR: // "CHAR"
      case symbol_kind::S_PRIMCHAR: // "PRIMCHAR"
        value.copy< char > (that.value);
        break;

      case symbol_kind::S_RATIONAL: // "RATIONAL"
      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.copy< double > (that.value);
        break;

      case symbol_kind::S_module: // module
      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
      case symbol_kind::S_maybeexports: // maybeexports
      case symbol_kind::S_export: // export
      case symbol_kind::S_qcname_ext_w_wildcard: // qcname_ext_w_wildcard
      case symbol_kind::S_qcname_ext: // qcname_ext
      case symbol_kind::S_qcname: // qcname
      case symbol_kind::S_importdecl: // importdecl
      case symbol_kind::S_maybeimpspec: // maybeimpspec
      case symbol_kind::S_impspec: // impspec
      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ctype: // ctype
      case symbol_kind::S_ctypedoc: // ctypedoc
      case symbol_kind::S_type: // type
      case symbol_kind::S_typedoc: // typedoc
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_tyapp: // tyapp
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_forall: // forall
      case symbol_kind::S_constr_stuff: // constr_stuff
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_alt_rhs: // alt_rhs
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
      case symbol_kind::S_literal: // literal
        value.copy< expression_ref > (that.value);
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        value.copy< float > (that.value);
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        value.copy< int > (that.value);
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.copy< std::optional<Located<Haskell::Decls>> > (that.value);
        break;

      case symbol_kind::S_prec: // prec
        value.copy< std::optional<int> > (that.value);
        break;

      case symbol_kind::S_maybe_pkg: // maybe_pkg
      case symbol_kind::S_maybeas: // maybeas
        value.copy< std::optional<std::string> > (that.value);
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.copy< std::pair<Haskell::Context,expression_ref> > (that.value);
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_CONID: // "CONID"
      case symbol_kind::S_VARSYM: // "VARSYM"
      case symbol_kind::S_CONSYM: // "CONSYM"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_QCONID: // "QCONID"
      case symbol_kind::S_QVARSYM: // "QVARSYM"
      case symbol_kind::S_QCONSYM: // "QCONSYM"
      case symbol_kind::S_IPDUPVARID: // "IPDUPVARID"
      case symbol_kind::S_LABELVARID: // "LABELVARID"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_PRIMSTRING: // "PRIMSTRING"
      case symbol_kind::S_qcon: // qcon
      case symbol_kind::S_gen_qcon: // gen_qcon
      case symbol_kind::S_con: // con
      case symbol_kind::S_sysdcon_no_list: // sysdcon_no_list
      case symbol_kind::S_sysdcon: // sysdcon
      case symbol_kind::S_conop: // conop
      case symbol_kind::S_qconop: // qconop
      case symbol_kind::S_gtycon: // gtycon
      case symbol_kind::S_ntgtycon: // ntgtycon
      case symbol_kind::S_oqtycon: // oqtycon
      case symbol_kind::S_oqtycon_no_varcon: // oqtycon_no_varcon
      case symbol_kind::S_qtyconop: // qtyconop
      case symbol_kind::S_qtycondoc: // qtycondoc
      case symbol_kind::S_qtycon: // qtycon
      case symbol_kind::S_tycon: // tycon
      case symbol_kind::S_qtyconsym: // qtyconsym
      case symbol_kind::S_tyconsym: // tyconsym
      case symbol_kind::S_op: // op
      case symbol_kind::S_varop: // varop
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_hole_op: // hole_op
      case symbol_kind::S_qvarop: // qvarop
      case symbol_kind::S_qvaropm: // qvaropm
      case symbol_kind::S_tyvar: // tyvar
      case symbol_kind::S_tyvarop: // tyvarop
      case symbol_kind::S_tyvarid: // tyvarid
      case symbol_kind::S_var: // var
      case symbol_kind::S_qvar: // qvar
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
      case symbol_kind::S_qvarsym: // qvarsym
      case symbol_kind::S_qvarsym_no_minus: // qvarsym_no_minus
      case symbol_kind::S_qvarsym1: // qvarsym1
      case symbol_kind::S_varsym: // varsym
      case symbol_kind::S_varsym_no_minus: // varsym_no_minus
      case symbol_kind::S_special_id: // special_id
      case symbol_kind::S_special_sym: // special_sym
      case symbol_kind::S_qconid: // qconid
      case symbol_kind::S_conid: // conid
      case symbol_kind::S_qconsym: // qconsym
      case symbol_kind::S_consym: // consym
      case symbol_kind::S_modid: // modid
        value.copy< std::string > (that.value);
        break;

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        value.copy< std::vector<Haskell::Constructor> > (that.value);
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.copy< std::vector<Haskell::FieldDecl> > (that.value);
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.copy< std::vector<Haskell::GuardedRHS> > (that.value);
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.copy< std::vector<Located<Haskell::Alt>> > (that.value);
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
      case symbol_kind::S_decls: // decls
      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        value.copy< std::vector<expression_ref> > (that.value);
        break;

      case symbol_kind::S_ops: // ops
      case symbol_kind::S_sig_vars: // sig_vars
        value.copy< std::vector<std::string> > (that.value);
        break;

      default:
        break;
    }

    location = that.location;
    return *this;
  }

  parser::stack_symbol_type&
  parser::stack_symbol_type::operator= (stack_symbol_type& that)
  {
    state = that.state;
    switch (that.kind ())
    {
      case symbol_kind::S_altslist: // altslist
        value.move< Haskell::Alts > (that.value);
        break;

      case symbol_kind::S_constr: // constr
        value.move< Haskell::Constructor > (that.value);
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        value.move< Haskell::Context > (that.value);
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        value.move< Haskell::DataOrNewtype > (that.value);
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.move< Haskell::FieldDecl > (that.value);
        break;

      case symbol_kind::S_infix: // infix
        value.move< Haskell::Fixity > (that.value);
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.move< Haskell::GuardedRHS > (that.value);
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.move< Haskell::Stmts > (that.value);
        break;

      case symbol_kind::S_strict_mark: // strict_mark
      case symbol_kind::S_strictness: // strictness
        value.move< Haskell::StrictLazy > (that.value);
        break;

      case symbol_kind::S_alt: // alt
        value.move< Located<Haskell::Alt> > (that.value);
        break;

      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
        value.move< Located<Haskell::Decls> > (that.value);
        break;

      case symbol_kind::S_maybe_src: // maybe_src
      case symbol_kind::S_maybe_safe: // maybe_safe
      case symbol_kind::S_optqualified: // optqualified
        value.move< bool > (that.value);
        break;

      case symbol_kind::S_CHAR: // "CHAR"
      case symbol_kind::S_PRIMCHAR: // "PRIMCHAR"
        value.move< char > (that.value);
        break;

      case symbol_kind::S_RATIONAL: // "RATIONAL"
      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.move< double > (that.value);
        break;

      case symbol_kind::S_module: // module
      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
      case symbol_kind::S_maybeexports: // maybeexports
      case symbol_kind::S_export: // export
      case symbol_kind::S_qcname_ext_w_wildcard: // qcname_ext_w_wildcard
      case symbol_kind::S_qcname_ext: // qcname_ext
      case symbol_kind::S_qcname: // qcname
      case symbol_kind::S_importdecl: // importdecl
      case symbol_kind::S_maybeimpspec: // maybeimpspec
      case symbol_kind::S_impspec: // impspec
      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ctype: // ctype
      case symbol_kind::S_ctypedoc: // ctypedoc
      case symbol_kind::S_type: // type
      case symbol_kind::S_typedoc: // typedoc
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_tyapp: // tyapp
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_forall: // forall
      case symbol_kind::S_constr_stuff: // constr_stuff
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_alt_rhs: // alt_rhs
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
      case symbol_kind::S_literal: // literal
        value.move< expression_ref > (that.value);
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        value.move< float > (that.value);
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        value.move< int > (that.value);
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.move< std::optional<Located<Haskell::Decls>> > (that.value);
        break;

      case symbol_kind::S_prec: // prec
        value.move< std::optional<int> > (that.value);
        break;

      case symbol_kind::S_maybe_pkg: // maybe_pkg
      case symbol_kind::S_maybeas: // maybeas
        value.move< std::optional<std::string> > (that.value);
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.move< std::pair<Haskell::Context,expression_ref> > (that.value);
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_CONID: // "CONID"
      case symbol_kind::S_VARSYM: // "VARSYM"
      case symbol_kind::S_CONSYM: // "CONSYM"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_QCONID: // "QCONID"
      case symbol_kind::S_QVARSYM: // "QVARSYM"
      case symbol_kind::S_QCONSYM: // "QCONSYM"
      case symbol_kind::S_IPDUPVARID: // "IPDUPVARID"
      case symbol_kind::S_LABELVARID: // "LABELVARID"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_PRIMSTRING: // "PRIMSTRING"
      case symbol_kind::S_qcon: // qcon
      case symbol_kind::S_gen_qcon: // gen_qcon
      case symbol_kind::S_con: // con
      case symbol_kind::S_sysdcon_no_list: // sysdcon_no_list
      case symbol_kind::S_sysdcon: // sysdcon
      case symbol_kind::S_conop: // conop
      case symbol_kind::S_qconop: // qconop
      case symbol_kind::S_gtycon: // gtycon
      case symbol_kind::S_ntgtycon: // ntgtycon
      case symbol_kind::S_oqtycon: // oqtycon
      case symbol_kind::S_oqtycon_no_varcon: // oqtycon_no_varcon
      case symbol_kind::S_qtyconop: // qtyconop
      case symbol_kind::S_qtycondoc: // qtycondoc
      case symbol_kind::S_qtycon: // qtycon
      case symbol_kind::S_tycon: // tycon
      case symbol_kind::S_qtyconsym: // qtyconsym
      case symbol_kind::S_tyconsym: // tyconsym
      case symbol_kind::S_op: // op
      case symbol_kind::S_varop: // varop
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_hole_op: // hole_op
      case symbol_kind::S_qvarop: // qvarop
      case symbol_kind::S_qvaropm: // qvaropm
      case symbol_kind::S_tyvar: // tyvar
      case symbol_kind::S_tyvarop: // tyvarop
      case symbol_kind::S_tyvarid: // tyvarid
      case symbol_kind::S_var: // var
      case symbol_kind::S_qvar: // qvar
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
      case symbol_kind::S_qvarsym: // qvarsym
      case symbol_kind::S_qvarsym_no_minus: // qvarsym_no_minus
      case symbol_kind::S_qvarsym1: // qvarsym1
      case symbol_kind::S_varsym: // varsym
      case symbol_kind::S_varsym_no_minus: // varsym_no_minus
      case symbol_kind::S_special_id: // special_id
      case symbol_kind::S_special_sym: // special_sym
      case symbol_kind::S_qconid: // qconid
      case symbol_kind::S_conid: // conid
      case symbol_kind::S_qconsym: // qconsym
      case symbol_kind::S_consym: // consym
      case symbol_kind::S_modid: // modid
        value.move< std::string > (that.value);
        break;

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        value.move< std::vector<Haskell::Constructor> > (that.value);
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.move< std::vector<Haskell::FieldDecl> > (that.value);
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.move< std::vector<Haskell::GuardedRHS> > (that.value);
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Located<Haskell::Alt>> > (that.value);
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
      case symbol_kind::S_decls: // decls
      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        value.move< std::vector<expression_ref> > (that.value);
        break;

      case symbol_kind::S_ops: // ops
      case symbol_kind::S_sig_vars: // sig_vars
        value.move< std::vector<std::string> > (that.value);
        break;

      default:
        break;
    }

    location = that.location;
    // that is emptied.
    that.state = empty_state;
    return *this;
  }
#endif

  template <typename Base>
  void
  parser::yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const
  {
    if (yymsg)
      YY_SYMBOL_PRINT (yymsg, yysym);
  }

#if YYDEBUG
  template <typename Base>
  void
  parser::yy_print_ (std::ostream& yyo, const basic_symbol<Base>& yysym) const
  {
    std::ostream& yyoutput = yyo;
    YY_USE (yyoutput);
    if (yysym.empty ())
      yyo << "empty symbol";
    else
      {
        symbol_kind_type yykind = yysym.kind ();
        yyo << (yykind < YYNTOKENS ? "token" : "nterm")
            << ' ' << yysym.name () << " ("
            << yysym.location << ": ";
        YY_USE (yykind);
        yyo << ')';
      }
  }
#endif

  void
  parser::yypush_ (const char* m, YY_MOVE_REF (stack_symbol_type) sym)
  {
    if (m)
      YY_SYMBOL_PRINT (m, sym);
    yystack_.push (YY_MOVE (sym));
  }

  void
  parser::yypush_ (const char* m, state_type s, YY_MOVE_REF (symbol_type) sym)
  {
#if 201103L <= YY_CPLUSPLUS
    yypush_ (m, stack_symbol_type (s, std::move (sym)));
#else
    stack_symbol_type ss (s, sym);
    yypush_ (m, ss);
#endif
  }

  void
  parser::yypop_ (int n)
  {
    yystack_.pop (n);
  }

#if YYDEBUG
  std::ostream&
  parser::debug_stream () const
  {
    return *yycdebug_;
  }

  void
  parser::set_debug_stream (std::ostream& o)
  {
    yycdebug_ = &o;
  }


  parser::debug_level_type
  parser::debug_level () const
  {
    return yydebug_;
  }

  void
  parser::set_debug_level (debug_level_type l)
  {
    yydebug_ = l;
  }
#endif // YYDEBUG

  parser::state_type
  parser::yy_lr_goto_state_ (state_type yystate, int yysym)
  {
    int yyr = yypgoto_[yysym - YYNTOKENS] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - YYNTOKENS];
  }

  bool
  parser::yy_pact_value_is_default_ (int yyvalue)
  {
    return yyvalue == yypact_ninf_;
  }

  bool
  parser::yy_table_value_is_error_ (int yyvalue)
  {
    return yyvalue == yytable_ninf_;
  }

  int
  parser::operator() ()
  {
    return parse ();
  }

  int
  parser::parse ()
  {
    int yyn;
    /// Length of the RHS of the rule being reduced.
    int yylen = 0;

    // Error handling.
    int yynerrs_ = 0;
    int yyerrstatus_ = 0;

    /// The lookahead symbol.
    symbol_type yyla;

    /// The locations where the error started and ended.
    stack_symbol_type yyerror_range[3];

    /// The return value of parse ().
    int yyresult;

#if YY_EXCEPTIONS
    try
#endif // YY_EXCEPTIONS
      {
    YYCDEBUG << "Starting parse\n";


    /* Initialize the stack.  The initial state will be set in
       yynewstate, since the latter expects the semantical and the
       location values to have been already stored, initialize these
       stacks with a primary value.  */
    yystack_.clear ();
    yypush_ (YY_NULLPTR, 0, YY_MOVE (yyla));

  /*-----------------------------------------------.
  | yynewstate -- push a new symbol on the stack.  |
  `-----------------------------------------------*/
  yynewstate:
    YYCDEBUG << "Entering state " << int (yystack_[0].state) << '\n';
    YY_STACK_PRINT ();

    // Accept?
    if (yystack_[0].state == yyfinal_)
      YYACCEPT;

    goto yybackup;


  /*-----------.
  | yybackup.  |
  `-----------*/
  yybackup:
    // Try to take a decision without lookahead.
    yyn = yypact_[+yystack_[0].state];
    if (yy_pact_value_is_default_ (yyn))
      goto yydefault;

    // Read a lookahead token.
    if (yyla.empty ())
      {
        YYCDEBUG << "Reading a token\n";
#if YY_EXCEPTIONS
        try
#endif // YY_EXCEPTIONS
          {
            symbol_type yylookahead (yylex (drv));
            yyla.move (yylookahead);
          }
#if YY_EXCEPTIONS
        catch (const syntax_error& yyexc)
          {
            YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
            error (yyexc);
            goto yyerrlab1;
          }
#endif // YY_EXCEPTIONS
      }
    YY_SYMBOL_PRINT ("Next token is", yyla);

    if (yyla.kind () == symbol_kind::S_YYerror)
    {
      // The scanner already issued an error message, process directly
      // to error recovery.  But do not keep the error token as
      // lookahead, it is too special and may lead us to an endless
      // loop in error recovery. */
      yyla.kind_ = symbol_kind::S_YYUNDEF;
      goto yyerrlab1;
    }

    /* If the proper action on seeing token YYLA.TYPE is to reduce or
       to detect an error, take that action.  */
    yyn += yyla.kind ();
    if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.kind ())
      {
        goto yydefault;
      }

    // Reduce or error.
    yyn = yytable_[yyn];
    if (yyn <= 0)
      {
        if (yy_table_value_is_error_ (yyn))
          goto yyerrlab;
        yyn = -yyn;
        goto yyreduce;
      }

    // Count tokens shifted since error; after three, turn off error status.
    if (yyerrstatus_)
      --yyerrstatus_;

    // Shift the lookahead token.
    yypush_ ("Shifting", state_type (yyn), YY_MOVE (yyla));
    goto yynewstate;


  /*-----------------------------------------------------------.
  | yydefault -- do the default action for the current state.  |
  `-----------------------------------------------------------*/
  yydefault:
    yyn = yydefact_[+yystack_[0].state];
    if (yyn == 0)
      goto yyerrlab;
    goto yyreduce;


  /*-----------------------------.
  | yyreduce -- do a reduction.  |
  `-----------------------------*/
  yyreduce:
    yylen = yyr2_[yyn];
    {
      stack_symbol_type yylhs;
      yylhs.state = yy_lr_goto_state_ (yystack_[yylen].state, yyr1_[yyn]);
      /* Variants are always initialized to an empty instance of the
         correct type. The default '$$ = $1' action is NOT applied
         when using variants.  */
      switch (yyr1_[yyn])
    {
      case symbol_kind::S_altslist: // altslist
        yylhs.value.emplace< Haskell::Alts > ();
        break;

      case symbol_kind::S_constr: // constr
        yylhs.value.emplace< Haskell::Constructor > ();
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        yylhs.value.emplace< Haskell::Context > ();
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        yylhs.value.emplace< Haskell::DataOrNewtype > ();
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        yylhs.value.emplace< Haskell::FieldDecl > ();
        break;

      case symbol_kind::S_infix: // infix
        yylhs.value.emplace< Haskell::Fixity > ();
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        yylhs.value.emplace< Haskell::GuardedRHS > ();
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        yylhs.value.emplace< Haskell::Stmts > ();
        break;

      case symbol_kind::S_strict_mark: // strict_mark
      case symbol_kind::S_strictness: // strictness
        yylhs.value.emplace< Haskell::StrictLazy > ();
        break;

      case symbol_kind::S_alt: // alt
        yylhs.value.emplace< Located<Haskell::Alt> > ();
        break;

      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
        yylhs.value.emplace< Located<Haskell::Decls> > ();
        break;

      case symbol_kind::S_maybe_src: // maybe_src
      case symbol_kind::S_maybe_safe: // maybe_safe
      case symbol_kind::S_optqualified: // optqualified
        yylhs.value.emplace< bool > ();
        break;

      case symbol_kind::S_CHAR: // "CHAR"
      case symbol_kind::S_PRIMCHAR: // "PRIMCHAR"
        yylhs.value.emplace< char > ();
        break;

      case symbol_kind::S_RATIONAL: // "RATIONAL"
      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        yylhs.value.emplace< double > ();
        break;

      case symbol_kind::S_module: // module
      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
      case symbol_kind::S_maybeexports: // maybeexports
      case symbol_kind::S_export: // export
      case symbol_kind::S_qcname_ext_w_wildcard: // qcname_ext_w_wildcard
      case symbol_kind::S_qcname_ext: // qcname_ext
      case symbol_kind::S_qcname: // qcname
      case symbol_kind::S_importdecl: // importdecl
      case symbol_kind::S_maybeimpspec: // maybeimpspec
      case symbol_kind::S_impspec: // impspec
      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ctype: // ctype
      case symbol_kind::S_ctypedoc: // ctypedoc
      case symbol_kind::S_type: // type
      case symbol_kind::S_typedoc: // typedoc
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_tyapp: // tyapp
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_forall: // forall
      case symbol_kind::S_constr_stuff: // constr_stuff
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_alt_rhs: // alt_rhs
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
      case symbol_kind::S_literal: // literal
        yylhs.value.emplace< expression_ref > ();
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        yylhs.value.emplace< float > ();
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        yylhs.value.emplace< int > ();
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        yylhs.value.emplace< std::optional<Located<Haskell::Decls>> > ();
        break;

      case symbol_kind::S_prec: // prec
        yylhs.value.emplace< std::optional<int> > ();
        break;

      case symbol_kind::S_maybe_pkg: // maybe_pkg
      case symbol_kind::S_maybeas: // maybeas
        yylhs.value.emplace< std::optional<std::string> > ();
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        yylhs.value.emplace< std::pair<Haskell::Context,expression_ref> > ();
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_CONID: // "CONID"
      case symbol_kind::S_VARSYM: // "VARSYM"
      case symbol_kind::S_CONSYM: // "CONSYM"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_QCONID: // "QCONID"
      case symbol_kind::S_QVARSYM: // "QVARSYM"
      case symbol_kind::S_QCONSYM: // "QCONSYM"
      case symbol_kind::S_IPDUPVARID: // "IPDUPVARID"
      case symbol_kind::S_LABELVARID: // "LABELVARID"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_PRIMSTRING: // "PRIMSTRING"
      case symbol_kind::S_qcon: // qcon
      case symbol_kind::S_gen_qcon: // gen_qcon
      case symbol_kind::S_con: // con
      case symbol_kind::S_sysdcon_no_list: // sysdcon_no_list
      case symbol_kind::S_sysdcon: // sysdcon
      case symbol_kind::S_conop: // conop
      case symbol_kind::S_qconop: // qconop
      case symbol_kind::S_gtycon: // gtycon
      case symbol_kind::S_ntgtycon: // ntgtycon
      case symbol_kind::S_oqtycon: // oqtycon
      case symbol_kind::S_oqtycon_no_varcon: // oqtycon_no_varcon
      case symbol_kind::S_qtyconop: // qtyconop
      case symbol_kind::S_qtycondoc: // qtycondoc
      case symbol_kind::S_qtycon: // qtycon
      case symbol_kind::S_tycon: // tycon
      case symbol_kind::S_qtyconsym: // qtyconsym
      case symbol_kind::S_tyconsym: // tyconsym
      case symbol_kind::S_op: // op
      case symbol_kind::S_varop: // varop
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_hole_op: // hole_op
      case symbol_kind::S_qvarop: // qvarop
      case symbol_kind::S_qvaropm: // qvaropm
      case symbol_kind::S_tyvar: // tyvar
      case symbol_kind::S_tyvarop: // tyvarop
      case symbol_kind::S_tyvarid: // tyvarid
      case symbol_kind::S_var: // var
      case symbol_kind::S_qvar: // qvar
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
      case symbol_kind::S_qvarsym: // qvarsym
      case symbol_kind::S_qvarsym_no_minus: // qvarsym_no_minus
      case symbol_kind::S_qvarsym1: // qvarsym1
      case symbol_kind::S_varsym: // varsym
      case symbol_kind::S_varsym_no_minus: // varsym_no_minus
      case symbol_kind::S_special_id: // special_id
      case symbol_kind::S_special_sym: // special_sym
      case symbol_kind::S_qconid: // qconid
      case symbol_kind::S_conid: // conid
      case symbol_kind::S_qconsym: // qconsym
      case symbol_kind::S_consym: // consym
      case symbol_kind::S_modid: // modid
        yylhs.value.emplace< std::string > ();
        break;

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        yylhs.value.emplace< std::vector<Haskell::Constructor> > ();
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        yylhs.value.emplace< std::vector<Haskell::FieldDecl> > ();
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        yylhs.value.emplace< std::vector<Haskell::GuardedRHS> > ();
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        yylhs.value.emplace< std::vector<Located<Haskell::Alt>> > ();
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
      case symbol_kind::S_decls: // decls
      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        yylhs.value.emplace< std::vector<expression_ref> > ();
        break;

      case symbol_kind::S_ops: // ops
      case symbol_kind::S_sig_vars: // sig_vars
        yylhs.value.emplace< std::vector<std::string> > ();
        break;

      default:
        break;
    }


      // Default location.
      {
        stack_type::slice range (yystack_, yylen);
        YYLLOC_DEFAULT (yylhs.location, range, yylen);
        yyerror_range[1].location = yylhs.location;
      }

      // Perform the reduction.
      YY_REDUCE_PRINT (yyn);
#if YY_EXCEPTIONS
      try
#endif // YY_EXCEPTIONS
        {
          switch (yyn)
            {
  case 2: // unit: module
#line 522 "parser.y"
             {drv.result = yystack_[0].value.as < expression_ref > ();}
#line 1826 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 539 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = make_module(yystack_[4].value.as < std::string > (),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 1832 "parser.cc"
    break;

  case 4: // module: body2
#line 540 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = make_module("Main",{},yystack_[0].value.as < expression_ref > ());}
#line 1838 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 542 "parser.y"
                                                                 {drv.push_module_context();}
#line 1844 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 550 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1850 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 551 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1856 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 553 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1862 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 554 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1868 "parser.cc"
    break;

  case 13: // top: semis top1
#line 557 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1874 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 559 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 1880 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 560 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 1886 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 561 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[0].value.as < std::vector<expression_ref> > (),{});}
#line 1892 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 569 "parser.y"
                                      {yylhs.value.as < expression_ref > () = make_exports(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 1898 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 570 "parser.y"
                                      {}
#line 1904 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 572 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 1910 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 574 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1916 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 575 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1922 "parser.cc"
    break;

  case 22: // export: qcname_ext export_subspec
#line 577 "parser.y"
                                      {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1928 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 578 "parser.y"
                                      {yylhs.value.as < expression_ref > () = AST_node("module",yystack_[0].value.as < std::string > ());}
#line 1934 "parser.cc"
    break;

  case 26: // qcnames: %empty
#line 584 "parser.y"
                   {}
#line 1940 "parser.cc"
    break;

  case 27: // qcnames: qcnames1
#line 585 "parser.y"
                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 1946 "parser.cc"
    break;

  case 28: // qcnames1: qcnames1 "," qcname_ext_w_wildcard ","
#line 587 "parser.y"
                                                  {yylhs.value.as < std::vector<expression_ref> > () = yystack_[3].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ());}
#line 1952 "parser.cc"
    break;

  case 29: // qcnames1: qcname_ext_w_wildcard
#line 588 "parser.y"
                                              {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1958 "parser.cc"
    break;

  case 30: // qcname_ext_w_wildcard: qcname_ext
#line 590 "parser.y"
                                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1964 "parser.cc"
    break;

  case 31: // qcname_ext_w_wildcard: ".."
#line 591 "parser.y"
                                     {}
#line 1970 "parser.cc"
    break;

  case 32: // qcname_ext: qcname
#line 593 "parser.y"
                                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1976 "parser.cc"
    break;

  case 33: // qcname_ext: "type" oqtycon
#line 594 "parser.y"
                                     {}
#line 1982 "parser.cc"
    break;

  case 34: // qcname: qvar
#line 596 "parser.y"
                                     {yylhs.value.as < expression_ref > () = AST_node("qvar",yystack_[0].value.as < std::string > ()); }
#line 1988 "parser.cc"
    break;

  case 35: // qcname: oqtycon_no_varcon
#line 597 "parser.y"
                                     {yylhs.value.as < expression_ref > () = AST_node("qvar",yystack_[0].value.as < std::string > ()); }
#line 1994 "parser.cc"
    break;

  case 40: // importdecls: importdecls_semi importdecl
#line 607 "parser.y"
                                         { yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (), yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 2000 "parser.cc"
    break;

  case 41: // importdecls_semi: importdecls_semi importdecl semis1
#line 609 "parser.y"
                                                     { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2006 "parser.cc"
    break;

  case 42: // importdecls_semi: %empty
#line 610 "parser.y"
                         { }
#line 2012 "parser.cc"
    break;

  case 43: // importdecl: "import" maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec
#line 612 "parser.y"
                                                                                            {
    std::vector<expression_ref> e;
    if (yystack_[4].value.as < bool > ()) e.push_back(AST_node("qualified"));
    e.push_back(String(yystack_[2].value.as < std::string > ()));
    if (yystack_[1].value.as < std::optional<std::string> > ()) e.push_back(AST_node("as", *yystack_[1].value.as < std::optional<std::string> > ()));
    if (yystack_[0].value.as < expression_ref > ()) e.push_back(yystack_[0].value.as < expression_ref > ());
    yylhs.value.as < expression_ref > () = expression_ref(new expression(AST_node("ImpDecl"),std::move(e)));
}
#line 2025 "parser.cc"
    break;

  case 44: // maybe_src: "{-# SOURCE" "#-}"
#line 621 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2031 "parser.cc"
    break;

  case 45: // maybe_src: %empty
#line 622 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2037 "parser.cc"
    break;

  case 46: // maybe_safe: "safe"
#line 624 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2043 "parser.cc"
    break;

  case 47: // maybe_safe: %empty
#line 625 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2049 "parser.cc"
    break;

  case 48: // maybe_pkg: "STRING"
#line 627 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 2055 "parser.cc"
    break;

  case 49: // maybe_pkg: %empty
#line 628 "parser.y"
                               { }
#line 2061 "parser.cc"
    break;

  case 50: // optqualified: "qualified"
#line 630 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2067 "parser.cc"
    break;

  case 51: // optqualified: %empty
#line 631 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2073 "parser.cc"
    break;

  case 52: // maybeas: "as" modid
#line 633 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 2079 "parser.cc"
    break;

  case 53: // maybeas: %empty
#line 634 "parser.y"
                               { }
#line 2085 "parser.cc"
    break;

  case 54: // maybeimpspec: impspec
#line 636 "parser.y"
                               { yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > (); }
#line 2091 "parser.cc"
    break;

  case 55: // maybeimpspec: %empty
#line 637 "parser.y"
                               { }
#line 2097 "parser.cc"
    break;

  case 56: // impspec: "(" exportlist ")"
#line 639 "parser.y"
                                      { yylhs.value.as < expression_ref > () = expression_ref{AST_node("only"),yystack_[1].value.as < std::vector<expression_ref> > ()}; }
#line 2103 "parser.cc"
    break;

  case 57: // impspec: "hiding" "(" exportlist ")"
#line 640 "parser.y"
                                      { yylhs.value.as < expression_ref > () = expression_ref{AST_node("hiding"),yystack_[1].value.as < std::vector<expression_ref> > ()}; }
#line 2109 "parser.cc"
    break;

  case 58: // prec: %empty
#line 645 "parser.y"
                   { }
#line 2115 "parser.cc"
    break;

  case 59: // prec: "INTEGER"
#line 646 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < int > (); }
#line 2121 "parser.cc"
    break;

  case 60: // infix: "infix"
#line 648 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infix; }
#line 2127 "parser.cc"
    break;

  case 61: // infix: "infixl"
#line 649 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infixl; }
#line 2133 "parser.cc"
    break;

  case 62: // infix: "infixr"
#line 650 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infixr; }
#line 2139 "parser.cc"
    break;

  case 63: // ops: ops "," op
#line 652 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 2145 "parser.cc"
    break;

  case 64: // ops: op
#line 653 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 2151 "parser.cc"
    break;

  case 65: // topdecls: topdecls_semi topdecl
#line 657 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 2157 "parser.cc"
    break;

  case 66: // topdecls_semi: topdecls_semi topdecl semis1
#line 659 "parser.y"
                                            { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2163 "parser.cc"
    break;

  case 67: // topdecls_semi: %empty
#line 660 "parser.y"
                                            { }
#line 2169 "parser.cc"
    break;

  case 68: // topdecl: cl_decl
#line 662 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2175 "parser.cc"
    break;

  case 69: // topdecl: ty_decl
#line 663 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2181 "parser.cc"
    break;

  case 70: // topdecl: inst_decl
#line 664 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2187 "parser.cc"
    break;

  case 71: // topdecl: "default" "(" comma_types0 ")"
#line 667 "parser.y"
                                               {}
#line 2193 "parser.cc"
    break;

  case 72: // topdecl: decl_no_th
#line 674 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2199 "parser.cc"
    break;

  case 73: // topdecl: infixexp_top
#line 676 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2205 "parser.cc"
    break;

  case 74: // topdecl: "builtin" var "INTEGER" "STRING" "STRING"
#line 677 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2211 "parser.cc"
    break;

  case 75: // topdecl: "builtin" var "INTEGER" "STRING"
#line 678 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2217 "parser.cc"
    break;

  case 76: // topdecl: "builtin" varop "INTEGER" "STRING" "STRING"
#line 679 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2223 "parser.cc"
    break;

  case 77: // topdecl: "builtin" varop "INTEGER" "STRING"
#line 680 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2229 "parser.cc"
    break;

  case 78: // cl_decl: "class" tycl_hdr wherebinds
#line 682 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_class_decl(yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().second,yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 2235 "parser.cc"
    break;

  case 79: // ty_decl: "type" type "=" ctypedoc
#line 684 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_type_synonym({yystack_[2].location,yystack_[2].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2241 "parser.cc"
    break;

  case 80: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 685 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < Haskell::DataOrNewtype > (),yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().second,yystack_[1].value.as < std::vector<Haskell::Constructor> > ());}
#line 2247 "parser.cc"
    break;

  case 81: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig
#line 686 "parser.y"
                                                                           {}
#line 2253 "parser.cc"
    break;

  case 82: // inst_decl: "instance" overlap_pragma inst_type wherebinds
#line 691 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_instance_decl({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 2259 "parser.cc"
    break;

  case 92: // data_or_newtype: "data"
#line 746 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::data;}
#line 2265 "parser.cc"
    break;

  case 93: // data_or_newtype: "newtype"
#line 747 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::newtype;}
#line 2271 "parser.cc"
    break;

  case 96: // tycl_hdr: context "=>" type
#line 759 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ()};}
#line 2277 "parser.cc"
    break;

  case 97: // tycl_hdr: type
#line 760 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {{},yystack_[0].value.as < expression_ref > ()};}
#line 2283 "parser.cc"
    break;

  case 101: // decls: decls ";" decl
#line 808 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2289 "parser.cc"
    break;

  case 102: // decls: decls ";"
#line 809 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2295 "parser.cc"
    break;

  case 103: // decls: decl
#line 810 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2301 "parser.cc"
    break;

  case 104: // decls: %empty
#line 811 "parser.y"
                        {}
#line 2307 "parser.cc"
    break;

  case 105: // decllist: "{" decls "}"
#line 813 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Decls> > () = make_decls(yystack_[1].location,yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2313 "parser.cc"
    break;

  case 106: // decllist: "vocurly" decls close
#line 814 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Decls> > () = make_decls(yystack_[1].location,yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2319 "parser.cc"
    break;

  case 107: // binds: decllist
#line 816 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Decls> > () = yystack_[0].value.as < Located<Haskell::Decls> > ();}
#line 2325 "parser.cc"
    break;

  case 108: // wherebinds: "where" binds
#line 818 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Haskell::Decls>> > () = yystack_[0].value.as < Located<Haskell::Decls> > ();}
#line 2331 "parser.cc"
    break;

  case 109: // wherebinds: %empty
#line 819 "parser.y"
                                 {}
#line 2337 "parser.cc"
    break;

  case 115: // opt_tyconsig: %empty
#line 845 "parser.y"
                     {}
#line 2343 "parser.cc"
    break;

  case 116: // opt_tyconsig: "::" gtycon
#line 846 "parser.y"
                     {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2349 "parser.cc"
    break;

  case 117: // sigtype: ctype
#line 848 "parser.y"
                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2355 "parser.cc"
    break;

  case 118: // sigtypedoc: ctypedoc
#line 850 "parser.y"
                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2361 "parser.cc"
    break;

  case 119: // sig_vars: sig_vars "," var
#line 852 "parser.y"
                           {yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 2367 "parser.cc"
    break;

  case 120: // sig_vars: var
#line 853 "parser.y"
                           {yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 2373 "parser.cc"
    break;

  case 121: // sigtypes1: sigtype
#line 855 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2379 "parser.cc"
    break;

  case 122: // sigtypes1: sigtypes1 "," sigtype
#line 856 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2385 "parser.cc"
    break;

  case 123: // strict_mark: strictness
#line 860 "parser.y"
                                            {yylhs.value.as < Haskell::StrictLazy > () = yystack_[0].value.as < Haskell::StrictLazy > ();}
#line 2391 "parser.cc"
    break;

  case 124: // strictness: "!"
#line 866 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::strict;}
#line 2397 "parser.cc"
    break;

  case 125: // strictness: "~"
#line 867 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::lazy;}
#line 2403 "parser.cc"
    break;

  case 126: // ctype: "forall" tv_bndrs "." ctype
#line 874 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_forall_type(yystack_[2].value.as < std::vector<expression_ref> > (), yystack_[0].value.as < expression_ref > ());}
#line 2409 "parser.cc"
    break;

  case 127: // ctype: context "=>" ctype
#line 875 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_constrained_type(yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ());}
#line 2415 "parser.cc"
    break;

  case 128: // ctype: type
#line 877 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2421 "parser.cc"
    break;

  case 129: // ctypedoc: ctype
#line 879 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2427 "parser.cc"
    break;

  case 130: // context: btype
#line 888 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(yystack_[0].value.as < expression_ref > ());}
#line 2433 "parser.cc"
    break;

  case 131: // context_no_ops: btype_no_ops
#line 890 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2439 "parser.cc"
    break;

  case 132: // type: btype
#line 892 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2445 "parser.cc"
    break;

  case 133: // type: btype "->" ctype
#line 893 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps({make_type_var("->"),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()});}
#line 2451 "parser.cc"
    break;

  case 134: // typedoc: type
#line 895 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2457 "parser.cc"
    break;

  case 135: // btype: tyapps
#line 898 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2463 "parser.cc"
    break;

  case 136: // btype_no_ops: atype_docs
#line 900 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2469 "parser.cc"
    break;

  case 137: // btype_no_ops: btype_no_ops atype_docs
#line 901 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2475 "parser.cc"
    break;

  case 138: // tyapps: tyapp
#line 903 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2481 "parser.cc"
    break;

  case 139: // tyapps: tyapps tyapp
#line 904 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2487 "parser.cc"
    break;

  case 140: // tyapp: atype
#line 906 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2493 "parser.cc"
    break;

  case 141: // tyapp: qtyconop
#line 907 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2499 "parser.cc"
    break;

  case 142: // tyapp: tyvarop
#line 908 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2505 "parser.cc"
    break;

  case 143: // atype_docs: atype
#line 914 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2511 "parser.cc"
    break;

  case 144: // atype: ntgtycon
#line 921 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2517 "parser.cc"
    break;

  case 145: // atype: tyvar
#line 922 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2523 "parser.cc"
    break;

  case 146: // atype: "*"
#line 923 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var("*");}
#line 2529 "parser.cc"
    break;

  case 147: // atype: strict_mark atype
#line 924 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_strict_lazy_type(yystack_[1].value.as < Haskell::StrictLazy > (),yystack_[0].value.as < expression_ref > ());}
#line 2535 "parser.cc"
    break;

  case 148: // atype: "{" fielddecls "}"
#line 925 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_field_decls(yystack_[1].value.as < std::vector<Haskell::FieldDecl> > ());}
#line 2541 "parser.cc"
    break;

  case 149: // atype: "(" ")"
#line 926 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var("()");}
#line 2547 "parser.cc"
    break;

  case 150: // atype: "(" comma_types1 "," ctype ")"
#line 927 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<expression_ref> > ();ts.push_back(yystack_[1].value.as < expression_ref > ());yylhs.value.as < expression_ref > () = make_tuple_type(ts);}
#line 2553 "parser.cc"
    break;

  case 151: // atype: "[" ctype "]"
#line 933 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_list_type(yystack_[1].value.as < expression_ref > ());}
#line 2559 "parser.cc"
    break;

  case 152: // atype: "(" ctype ")"
#line 934 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2565 "parser.cc"
    break;

  case 153: // atype: "(" ctype "::" kind ")"
#line 935 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_of_kind(yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ());}
#line 2571 "parser.cc"
    break;

  case 154: // inst_type: sigtype
#line 938 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2577 "parser.cc"
    break;

  case 157: // comma_types0: comma_types1
#line 943 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2583 "parser.cc"
    break;

  case 158: // comma_types0: %empty
#line 944 "parser.y"
                                       { /* default construction OK */ }
#line 2589 "parser.cc"
    break;

  case 159: // comma_types1: ctype
#line 946 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2595 "parser.cc"
    break;

  case 160: // comma_types1: comma_types1 "," ctype
#line 947 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2601 "parser.cc"
    break;

  case 161: // tv_bndrs: tv_bndrs tv_bndr
#line 954 "parser.y"
                               {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2607 "parser.cc"
    break;

  case 162: // tv_bndrs: %empty
#line 955 "parser.y"
                               { /* default construction OK */}
#line 2613 "parser.cc"
    break;

  case 163: // tv_bndr: tyvar
#line 957 "parser.y"
                                    {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2619 "parser.cc"
    break;

  case 164: // tv_bndr: "(" tyvar "::" kind ")"
#line 958 "parser.y"
                                    {yylhs.value.as < expression_ref > () = make_type_var_of_kind(yystack_[3].value.as < std::string > (),yystack_[1].value.as < expression_ref > ());}
#line 2625 "parser.cc"
    break;

  case 165: // kind: ctype
#line 976 "parser.y"
             {yylhs.value.as < expression_ref > () = make_kind(yystack_[0].value.as < expression_ref > ());}
#line 2631 "parser.cc"
    break;

  case 166: // constrs: "=" constrs1
#line 982 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::Constructor> > () = yystack_[0].value.as < std::vector<Haskell::Constructor> > ();}
#line 2637 "parser.cc"
    break;

  case 167: // constrs1: constrs1 "|" constr
#line 984 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::Constructor> > () = yystack_[2].value.as < std::vector<Haskell::Constructor> > (); yylhs.value.as < std::vector<Haskell::Constructor> > ().push_back(yystack_[0].value.as < Haskell::Constructor > ());}
#line 2643 "parser.cc"
    break;

  case 168: // constrs1: constr
#line 985 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::Constructor> > ().push_back(yystack_[0].value.as < Haskell::Constructor > ());}
#line 2649 "parser.cc"
    break;

  case 169: // constr: forall context_no_ops "=>" constr_stuff
#line 987 "parser.y"
                                                {yylhs.value.as < Haskell::Constructor > () = make_constructor(yystack_[3].value.as < expression_ref > (),yystack_[2].value.as < Haskell::Context > (), yystack_[0].value.as < expression_ref > ());}
#line 2655 "parser.cc"
    break;

  case 170: // constr: forall constr_stuff
#line 988 "parser.y"
                                                {yylhs.value.as < Haskell::Constructor > () = make_constructor(yystack_[1].value.as < expression_ref > (),{}, yystack_[0].value.as < expression_ref > ());}
#line 2661 "parser.cc"
    break;

  case 171: // forall: "forall" tv_bndrs "."
#line 990 "parser.y"
                                {if (yystack_[1].value.as < std::vector<expression_ref> > ().size()>1) yylhs.value.as < expression_ref > () = make_tv_bndrs(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2667 "parser.cc"
    break;

  case 172: // forall: %empty
#line 991 "parser.y"
                                {}
#line 2673 "parser.cc"
    break;

  case 173: // constr_stuff: btype_no_ops
#line 993 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2679 "parser.cc"
    break;

  case 174: // constr_stuff: btype_no_ops conop btype_no_ops
#line 994 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps({make_type_var(yystack_[1].value.as < std::string > ()),make_tyapps(yystack_[2].value.as < std::vector<expression_ref> > ()),make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 2685 "parser.cc"
    break;

  case 175: // fielddecls: %empty
#line 996 "parser.y"
                                {}
#line 2691 "parser.cc"
    break;

  case 176: // fielddecls: fielddecls1
#line 997 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::FieldDecl> > () = yystack_[0].value.as < std::vector<Haskell::FieldDecl> > ();}
#line 2697 "parser.cc"
    break;

  case 177: // fielddecls1: fielddecls1 "," fielddecl
#line 999 "parser.y"
                                        {yylhs.value.as < std::vector<Haskell::FieldDecl> > () = yystack_[2].value.as < std::vector<Haskell::FieldDecl> > (); yylhs.value.as < std::vector<Haskell::FieldDecl> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2703 "parser.cc"
    break;

  case 178: // fielddecls1: fielddecl
#line 1000 "parser.y"
                                        {yylhs.value.as < std::vector<Haskell::FieldDecl> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2709 "parser.cc"
    break;

  case 179: // fielddecl: sig_vars "::" ctype
#line 1002 "parser.y"
                                        {yylhs.value.as < Haskell::FieldDecl > () = make_field_decl(yystack_[2].value.as < std::vector<std::string> > (),yystack_[0].value.as < expression_ref > ());}
#line 2715 "parser.cc"
    break;

  case 190: // decl_no_th: sigdecl
#line 1021 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2721 "parser.cc"
    break;

  case 191: // decl_no_th: "!" aexp rhs
#line 1023 "parser.y"
                              {yylhs.value.as < expression_ref > () = new expression(AST_node("Decl:Strict"),{(yystack_[1].value.as < expression_ref > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2727 "parser.cc"
    break;

  case 192: // decl_no_th: infixexp_top rhs
#line 1029 "parser.y"
                                  {yylhs.value.as < expression_ref > () = make_value_decl(make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ());}
#line 2733 "parser.cc"
    break;

  case 193: // decl: decl_no_th
#line 1034 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2739 "parser.cc"
    break;

  case 194: // rhs: "=" exp wherebinds
#line 1038 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_rhs({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 2745 "parser.cc"
    break;

  case 195: // rhs: gdrhs wherebinds
#line 1039 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 2751 "parser.cc"
    break;

  case 196: // gdrhs: gdrhs gdrh
#line 1041 "parser.y"
                              {yylhs.value.as < std::vector<Haskell::GuardedRHS> > () = yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (); yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 2757 "parser.cc"
    break;

  case 197: // gdrhs: gdrh
#line 1042 "parser.y"
                              {yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 2763 "parser.cc"
    break;

  case 198: // gdrh: "|" guardquals "=" exp
#line 1046 "parser.y"
                              {yylhs.value.as < Haskell::GuardedRHS > () = make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2769 "parser.cc"
    break;

  case 199: // sigdecl: infixexp_top "::" sigtypedoc
#line 1048 "parser.y"
                                             { yylhs.value.as < expression_ref > () = expression_ref(AST_node("Decl:sigtype"),{make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2775 "parser.cc"
    break;

  case 200: // sigdecl: var "," sig_vars "::" sigtypedoc
#line 1049 "parser.y"
                                          {}
#line 2781 "parser.cc"
    break;

  case 201: // sigdecl: infix prec ops
#line 1050 "parser.y"
                         { yylhs.value.as < expression_ref > () = make_fixity_decl(yystack_[2].value.as < Haskell::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()); }
#line 2787 "parser.cc"
    break;

  case 202: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1052 "parser.y"
                                                    {}
#line 2793 "parser.cc"
    break;

  case 203: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1053 "parser.y"
                                            {}
#line 2799 "parser.cc"
    break;

  case 204: // sigdecl: "{-# SCC" qvar "#-}"
#line 1054 "parser.y"
                              {}
#line 2805 "parser.cc"
    break;

  case 205: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1055 "parser.y"
                                     {}
#line 2811 "parser.cc"
    break;

  case 206: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1056 "parser.y"
                                                               {}
#line 2817 "parser.cc"
    break;

  case 207: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1057 "parser.y"
                                                                      {}
#line 2823 "parser.cc"
    break;

  case 208: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1058 "parser.y"
                                                     {}
#line 2829 "parser.cc"
    break;

  case 213: // exp: infixexp "::" sigtype
#line 1069 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_typed_exp(make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()); }
#line 2835 "parser.cc"
    break;

  case 214: // exp: infixexp
#line 1070 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 2841 "parser.cc"
    break;

  case 215: // infixexp: exp10
#line 1072 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2847 "parser.cc"
    break;

  case 216: // infixexp: infixexp qop exp10
#line 1073 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_var({yystack_[1].location,yystack_[1].value.as < std::string > ()})); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2853 "parser.cc"
    break;

  case 217: // infixexp_top: exp10_top
#line 1075 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2859 "parser.cc"
    break;

  case 218: // infixexp_top: infixexp_top qop exp10_top
#line 1076 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_var({yystack_[1].location,yystack_[1].value.as < std::string > ()})); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2865 "parser.cc"
    break;

  case 219: // exp10_top: "-" fexp
#line 1078 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_minus(make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2871 "parser.cc"
    break;

  case 220: // exp10_top: "{-# CORE" "STRING" "#-}"
#line 1079 "parser.y"
                                   {}
#line 2877 "parser.cc"
    break;

  case 221: // exp10_top: fexp
#line 1080 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2883 "parser.cc"
    break;

  case 222: // exp10: exp10_top
#line 1082 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2889 "parser.cc"
    break;

  case 223: // exp10: scc_annot exp
#line 1083 "parser.y"
                                 {}
#line 2895 "parser.cc"
    break;

  case 228: // fexp: fexp aexp
#line 1094 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2901 "parser.cc"
    break;

  case 229: // fexp: fexp "TYPEAPP" atype
#line 1095 "parser.y"
                                 {}
#line 2907 "parser.cc"
    break;

  case 230: // fexp: "static" aexp
#line 1096 "parser.y"
                                 {}
#line 2913 "parser.cc"
    break;

  case 231: // fexp: aexp
#line 1097 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2919 "parser.cc"
    break;

  case 232: // aexp: qvar "@" aexp
#line 1099 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_as_pattern(make_var({yystack_[2].location,yystack_[2].value.as < std::string > ()}),yystack_[0].value.as < expression_ref > ());}
#line 2925 "parser.cc"
    break;

  case 233: // aexp: "~" aexp
#line 1100 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lazy_pattern(yystack_[0].value.as < expression_ref > ());}
#line 2931 "parser.cc"
    break;

  case 234: // aexp: "\\" apats1 "->" exp
#line 1101 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lambdaexp(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2937 "parser.cc"
    break;

  case 235: // aexp: "let" binds "in" exp
#line 1102 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_let(yystack_[2].value.as < Located<Haskell::Decls> > (),{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2943 "parser.cc"
    break;

  case 236: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1104 "parser.y"
                                                       {yylhs.value.as < expression_ref > () = make_if({yystack_[6].location,yystack_[6].value.as < expression_ref > ()},{yystack_[3].location,yystack_[3].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2949 "parser.cc"
    break;

  case 237: // aexp: "case" exp "of" altslist
#line 1106 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_case(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < Haskell::Alts > ());}
#line 2955 "parser.cc"
    break;

  case 238: // aexp: "do" stmtlist
#line 1107 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_do(yystack_[0].value.as < Haskell::Stmts > ());}
#line 2961 "parser.cc"
    break;

  case 239: // aexp: "mdo" stmtlist
#line 1108 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_mdo(yystack_[0].value.as < Haskell::Stmts > ());}
#line 2967 "parser.cc"
    break;

  case 240: // aexp: aexp1
#line 1110 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2973 "parser.cc"
    break;

  case 241: // aexp1: aexp1 "{" fbinds "}"
#line 1112 "parser.y"
                              {}
#line 2979 "parser.cc"
    break;

  case 242: // aexp1: aexp2
#line 1113 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2985 "parser.cc"
    break;

  case 243: // aexp2: qvar
#line 1115 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2991 "parser.cc"
    break;

  case 244: // aexp2: qcon
#line 1116 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2997 "parser.cc"
    break;

  case 245: // aexp2: literal
#line 1117 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3003 "parser.cc"
    break;

  case 246: // aexp2: "(" texp ")"
#line 1118 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3009 "parser.cc"
    break;

  case 247: // aexp2: "(" tup_exprs ")"
#line 1119 "parser.y"
                              {yylhs.value.as < expression_ref > () = yy_make_tuple(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3015 "parser.cc"
    break;

  case 248: // aexp2: "[" list "]"
#line 1124 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3021 "parser.cc"
    break;

  case 249: // aexp2: "_"
#line 1125 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::WildcardPattern();}
#line 3027 "parser.cc"
    break;

  case 250: // texp: exp
#line 1130 "parser.y"
                      {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3033 "parser.cc"
    break;

  case 251: // texp: infixexp qop
#line 1131 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("LeftSection"),{make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ()),make_var({yystack_[0].location,yystack_[0].value.as < std::string > ()})});}
#line 3039 "parser.cc"
    break;

  case 252: // texp: qopm infixexp
#line 1132 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("RightSection"),{make_var({yystack_[1].location,yystack_[1].value.as < std::string > ()}),make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 3045 "parser.cc"
    break;

  case 253: // tup_exprs: tup_exprs "," texp
#line 1137 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3051 "parser.cc"
    break;

  case 254: // tup_exprs: texp "," texp
#line 1138 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3057 "parser.cc"
    break;

  case 255: // list: texp
#line 1156 "parser.y"
                                 { yylhs.value.as < expression_ref > () = make_list({yystack_[0].value.as < expression_ref > ()}); }
#line 3063 "parser.cc"
    break;

  case 256: // list: lexps
#line 1157 "parser.y"
                                 { yylhs.value.as < expression_ref > () = make_list(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 3069 "parser.cc"
    break;

  case 257: // list: texp ".."
#line 1158 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFrom"),{yystack_[1].value.as < expression_ref > ()}); }
#line 3075 "parser.cc"
    break;

  case 258: // list: texp "," exp ".."
#line 1159 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThen"),{yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()}); }
#line 3081 "parser.cc"
    break;

  case 259: // list: texp ".." exp
#line 1160 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromTo"),{yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 3087 "parser.cc"
    break;

  case 260: // list: texp "," exp ".." exp
#line 1161 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThenTo"),{yystack_[4].value.as < expression_ref > (),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 3093 "parser.cc"
    break;

  case 261: // list: texp "|" squals
#line 1162 "parser.y"
                                 { auto quals = yystack_[0].value.as < std::vector<expression_ref> > (); quals.push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < expression_ref > () = expression_ref(AST_node("ListComprehension"),quals); }
#line 3099 "parser.cc"
    break;

  case 262: // lexps: lexps "," texp
#line 1164 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3105 "parser.cc"
    break;

  case 263: // lexps: texp "," texp
#line 1165 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3111 "parser.cc"
    break;

  case 264: // squals: squals "," qual
#line 1178 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3117 "parser.cc"
    break;

  case 265: // squals: qual
#line 1180 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3123 "parser.cc"
    break;

  case 266: // guardquals: guardquals1
#line 1190 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3129 "parser.cc"
    break;

  case 267: // guardquals1: guardquals1 "," qual
#line 1192 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > ();yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3135 "parser.cc"
    break;

  case 268: // guardquals1: qual
#line 1193 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3141 "parser.cc"
    break;

  case 269: // altslist: "{" alts "}"
#line 1196 "parser.y"
                                 {yylhs.value.as < Haskell::Alts > () = make_alts(yystack_[1].value.as < std::vector<Located<Haskell::Alt>> > ());}
#line 3147 "parser.cc"
    break;

  case 270: // altslist: "vocurly" alts close
#line 1197 "parser.y"
                                 {yylhs.value.as < Haskell::Alts > () = make_alts(yystack_[1].value.as < std::vector<Located<Haskell::Alt>> > ());}
#line 3153 "parser.cc"
    break;

  case 271: // altslist: "{" "}"
#line 1198 "parser.y"
                                 {}
#line 3159 "parser.cc"
    break;

  case 272: // altslist: "vocurly" close
#line 1199 "parser.y"
                                 {}
#line 3165 "parser.cc"
    break;

  case 273: // alts: alts1
#line 1201 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[0].value.as < std::vector<Located<Haskell::Alt>> > ();}
#line 3171 "parser.cc"
    break;

  case 274: // alts: ";" alts
#line 1202 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[0].value.as < std::vector<Located<Haskell::Alt>> > ();}
#line 3177 "parser.cc"
    break;

  case 275: // alts1: alts1 ";" alt
#line 1204 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[2].value.as < std::vector<Located<Haskell::Alt>> > (); yylhs.value.as < std::vector<Located<Haskell::Alt>> > ().push_back(yystack_[0].value.as < Located<Haskell::Alt> > ());}
#line 3183 "parser.cc"
    break;

  case 276: // alts1: alts1 ";"
#line 1205 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[1].value.as < std::vector<Located<Haskell::Alt>> > ();}
#line 3189 "parser.cc"
    break;

  case 277: // alts1: alt
#line 1206 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > ().push_back(yystack_[0].value.as < Located<Haskell::Alt> > ());}
#line 3195 "parser.cc"
    break;

  case 278: // alt: pat alt_rhs
#line 1208 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Alt> > () = yy_make_alt(yystack_[1].location+yystack_[0].location,yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3201 "parser.cc"
    break;

  case 279: // alt_rhs: "->" exp wherebinds
#line 1210 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_rhs({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 3207 "parser.cc"
    break;

  case 280: // alt_rhs: gdpats wherebinds
#line 1211 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 3213 "parser.cc"
    break;

  case 281: // gdpats: gdpats gdpat
#line 1213 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::GuardedRHS> > () = yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (); yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 3219 "parser.cc"
    break;

  case 282: // gdpats: gdpat
#line 1214 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 3225 "parser.cc"
    break;

  case 283: // gdpat: "|" guardquals "->" exp
#line 1223 "parser.y"
                                 {yylhs.value.as < Haskell::GuardedRHS > ()=make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3231 "parser.cc"
    break;

  case 284: // pat: exp
#line 1225 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3237 "parser.cc"
    break;

  case 285: // pat: "!" aexp
#line 1226 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3243 "parser.cc"
    break;

  case 286: // bindpat: exp
#line 1228 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3249 "parser.cc"
    break;

  case 287: // bindpat: "!" aexp
#line 1229 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3255 "parser.cc"
    break;

  case 288: // apat: aexp
#line 1231 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3261 "parser.cc"
    break;

  case 289: // apat: "!" aexp
#line 1232 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3267 "parser.cc"
    break;

  case 290: // apats1: apats1 apat
#line 1234 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3273 "parser.cc"
    break;

  case 291: // apats1: apat
#line 1235 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3279 "parser.cc"
    break;

  case 292: // stmtlist: "{" stmts "}"
#line 1238 "parser.y"
                               {yylhs.value.as < Haskell::Stmts > () = make_stmts(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3285 "parser.cc"
    break;

  case 293: // stmtlist: "vocurly" stmts close
#line 1239 "parser.y"
                               {yylhs.value.as < Haskell::Stmts > () = make_stmts(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3291 "parser.cc"
    break;

  case 294: // stmts: stmts ";" stmt
#line 1241 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3297 "parser.cc"
    break;

  case 295: // stmts: stmts ";"
#line 1242 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3303 "parser.cc"
    break;

  case 296: // stmts: stmt
#line 1243 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3309 "parser.cc"
    break;

  case 297: // stmts: %empty
#line 1244 "parser.y"
                       {}
#line 3315 "parser.cc"
    break;

  case 298: // stmt: qual
#line 1249 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3321 "parser.cc"
    break;

  case 299: // stmt: "rec" stmtlist
#line 1250 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::RecStmt(yystack_[0].value.as < Haskell::Stmts > ());}
#line 3327 "parser.cc"
    break;

  case 300: // qual: bindpat "<-" exp
#line 1252 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::PatQual(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3333 "parser.cc"
    break;

  case 301: // qual: exp
#line 1253 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::SimpleQual(yystack_[0].value.as < expression_ref > ());}
#line 3339 "parser.cc"
    break;

  case 302: // qual: "let" binds
#line 1254 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::LetQual(yystack_[0].value.as < Located<Haskell::Decls> > ());}
#line 3345 "parser.cc"
    break;

  case 310: // qcon: gen_qcon
#line 1299 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3351 "parser.cc"
    break;

  case 311: // qcon: sysdcon
#line 1300 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3357 "parser.cc"
    break;

  case 312: // gen_qcon: qconid
#line 1302 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3363 "parser.cc"
    break;

  case 313: // gen_qcon: "(" qconsym ")"
#line 1303 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3369 "parser.cc"
    break;

  case 314: // con: conid
#line 1305 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3375 "parser.cc"
    break;

  case 315: // con: "(" consym ")"
#line 1306 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3381 "parser.cc"
    break;

  case 316: // con: sysdcon
#line 1307 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3387 "parser.cc"
    break;

  case 319: // sysdcon_no_list: "(" ")"
#line 1312 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 3393 "parser.cc"
    break;

  case 320: // sysdcon_no_list: "(" commas ")"
#line 1313 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3399 "parser.cc"
    break;

  case 321: // sysdcon_no_list: "(#" "#)"
#line 1314 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 3405 "parser.cc"
    break;

  case 322: // sysdcon_no_list: "(#" commas "#)"
#line 1315 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3411 "parser.cc"
    break;

  case 323: // sysdcon: sysdcon_no_list
#line 1317 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3417 "parser.cc"
    break;

  case 324: // sysdcon: "[" "]"
#line 1318 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 3423 "parser.cc"
    break;

  case 325: // conop: consym
#line 1320 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3429 "parser.cc"
    break;

  case 326: // conop: "`" conid "`"
#line 1321 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3435 "parser.cc"
    break;

  case 327: // qconop: qconsym
#line 1323 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3441 "parser.cc"
    break;

  case 328: // qconop: "`" qconid "`"
#line 1324 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3447 "parser.cc"
    break;

  case 329: // gtycon: ntgtycon
#line 1327 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3453 "parser.cc"
    break;

  case 330: // gtycon: "(" ")"
#line 1328 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 3459 "parser.cc"
    break;

  case 331: // gtycon: "(#" "#)"
#line 1329 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 3465 "parser.cc"
    break;

  case 332: // ntgtycon: oqtycon
#line 1331 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3471 "parser.cc"
    break;

  case 333: // ntgtycon: "(" commas ")"
#line 1332 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3477 "parser.cc"
    break;

  case 334: // ntgtycon: "(#" commas "#)"
#line 1333 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3483 "parser.cc"
    break;

  case 335: // ntgtycon: "(" "->" ")"
#line 1334 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 3489 "parser.cc"
    break;

  case 336: // ntgtycon: "[" "]"
#line 1335 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 3495 "parser.cc"
    break;

  case 337: // oqtycon: qtycon
#line 1337 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3501 "parser.cc"
    break;

  case 338: // oqtycon: "(" qtyconsym ")"
#line 1338 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3507 "parser.cc"
    break;

  case 339: // oqtycon: "(" "~" ")"
#line 1339 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3513 "parser.cc"
    break;

  case 340: // oqtycon_no_varcon: qtycon
#line 1341 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3519 "parser.cc"
    break;

  case 341: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1342 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3525 "parser.cc"
    break;

  case 342: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1343 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3531 "parser.cc"
    break;

  case 343: // oqtycon_no_varcon: "(" ":" ")"
#line 1344 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 3537 "parser.cc"
    break;

  case 344: // oqtycon_no_varcon: "(" "~" ")"
#line 1345 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3543 "parser.cc"
    break;

  case 345: // qtyconop: qtyconsym
#line 1348 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3549 "parser.cc"
    break;

  case 346: // qtyconop: "`" qtycon "`"
#line 1349 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3555 "parser.cc"
    break;

  case 347: // qtycondoc: qtycon
#line 1351 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3561 "parser.cc"
    break;

  case 348: // qtycon: "QCONID"
#line 1353 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3567 "parser.cc"
    break;

  case 349: // qtycon: tycon
#line 1354 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3573 "parser.cc"
    break;

  case 350: // tycon: "CONID"
#line 1358 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3579 "parser.cc"
    break;

  case 351: // qtyconsym: "QCONSYM"
#line 1360 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3585 "parser.cc"
    break;

  case 352: // qtyconsym: "QVARSYM"
#line 1361 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3591 "parser.cc"
    break;

  case 353: // qtyconsym: tyconsym
#line 1362 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3597 "parser.cc"
    break;

  case 354: // tyconsym: "CONSYM"
#line 1364 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3603 "parser.cc"
    break;

  case 355: // tyconsym: "VARSYM"
#line 1365 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3609 "parser.cc"
    break;

  case 356: // tyconsym: ":"
#line 1366 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 3615 "parser.cc"
    break;

  case 357: // tyconsym: "-"
#line 1367 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 3621 "parser.cc"
    break;

  case 358: // op: varop
#line 1372 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3627 "parser.cc"
    break;

  case 359: // op: conop
#line 1373 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3633 "parser.cc"
    break;

  case 360: // varop: varsym
#line 1375 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3639 "parser.cc"
    break;

  case 361: // varop: "`" varid "`"
#line 1376 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3645 "parser.cc"
    break;

  case 362: // qop: qvarop
#line 1378 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3651 "parser.cc"
    break;

  case 363: // qop: qconop
#line 1379 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3657 "parser.cc"
    break;

  case 364: // qop: hole_op
#line 1380 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3663 "parser.cc"
    break;

  case 365: // qopm: qvaropm
#line 1382 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3669 "parser.cc"
    break;

  case 366: // qopm: qconop
#line 1383 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3675 "parser.cc"
    break;

  case 367: // qopm: hole_op
#line 1384 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3681 "parser.cc"
    break;

  case 368: // hole_op: "`" "_" "`"
#line 1386 "parser.y"
                      { yylhs.value.as < std::string > () = "_"; }
#line 3687 "parser.cc"
    break;

  case 369: // qvarop: qvarsym
#line 1388 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3693 "parser.cc"
    break;

  case 370: // qvarop: "`" qvarid "`"
#line 1389 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3699 "parser.cc"
    break;

  case 371: // qvaropm: qvarsym_no_minus
#line 1391 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 3705 "parser.cc"
    break;

  case 372: // qvaropm: "`" qvarid "`"
#line 1392 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3711 "parser.cc"
    break;

  case 373: // tyvar: tyvarid
#line 1396 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3717 "parser.cc"
    break;

  case 374: // tyvarop: "`" tyvarid "`"
#line 1398 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3723 "parser.cc"
    break;

  case 375: // tyvarid: "VARID"
#line 1400 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3729 "parser.cc"
    break;

  case 376: // tyvarid: special_id
#line 1401 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3735 "parser.cc"
    break;

  case 377: // tyvarid: "unsafe"
#line 1402 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 3741 "parser.cc"
    break;

  case 378: // tyvarid: "safe"
#line 1403 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 3747 "parser.cc"
    break;

  case 379: // tyvarid: "interruptible"
#line 1404 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 3753 "parser.cc"
    break;

  case 380: // var: varid
#line 1407 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3759 "parser.cc"
    break;

  case 381: // var: "(" varsym ")"
#line 1408 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3765 "parser.cc"
    break;

  case 382: // qvar: qvarid
#line 1410 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3771 "parser.cc"
    break;

  case 383: // qvar: "(" varsym ")"
#line 1411 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3777 "parser.cc"
    break;

  case 384: // qvar: "(" qvarsym1 ")"
#line 1412 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3783 "parser.cc"
    break;

  case 385: // qvarid: varid
#line 1414 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3789 "parser.cc"
    break;

  case 386: // qvarid: "QVARID"
#line 1415 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3795 "parser.cc"
    break;

  case 387: // varid: "VARID"
#line 1417 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3801 "parser.cc"
    break;

  case 388: // varid: special_id
#line 1418 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3807 "parser.cc"
    break;

  case 389: // varid: "unsafe"
#line 1419 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 3813 "parser.cc"
    break;

  case 390: // varid: "safe"
#line 1420 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 3819 "parser.cc"
    break;

  case 391: // varid: "interruptible"
#line 1421 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 3825 "parser.cc"
    break;

  case 392: // varid: "forall"
#line 1422 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 3831 "parser.cc"
    break;

  case 393: // varid: "family"
#line 1423 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 3837 "parser.cc"
    break;

  case 394: // varid: "role"
#line 1424 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 3843 "parser.cc"
    break;

  case 395: // qvarsym: varsym
#line 1426 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3849 "parser.cc"
    break;

  case 396: // qvarsym: qvarsym1
#line 1427 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3855 "parser.cc"
    break;

  case 397: // qvarsym_no_minus: varsym_no_minus
#line 1429 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3861 "parser.cc"
    break;

  case 398: // qvarsym_no_minus: qvarsym1
#line 1430 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3867 "parser.cc"
    break;

  case 399: // qvarsym1: "QVARSYM"
#line 1432 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3873 "parser.cc"
    break;

  case 400: // varsym: varsym_no_minus
#line 1434 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3879 "parser.cc"
    break;

  case 401: // varsym: "-"
#line 1435 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 3885 "parser.cc"
    break;

  case 402: // varsym_no_minus: "VARSYM"
#line 1437 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3891 "parser.cc"
    break;

  case 403: // varsym_no_minus: special_sym
#line 1438 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3897 "parser.cc"
    break;

  case 404: // special_id: "as"
#line 1440 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 3903 "parser.cc"
    break;

  case 405: // special_id: "qualified"
#line 1441 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 3909 "parser.cc"
    break;

  case 406: // special_id: "hiding"
#line 1442 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 3915 "parser.cc"
    break;

  case 407: // special_id: "export"
#line 1443 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 3921 "parser.cc"
    break;

  case 408: // special_id: "label"
#line 1444 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 3927 "parser.cc"
    break;

  case 409: // special_id: "dynamic"
#line 1445 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 3933 "parser.cc"
    break;

  case 410: // special_id: "stdcall"
#line 1446 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 3939 "parser.cc"
    break;

  case 411: // special_id: "ccall"
#line 1447 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 3945 "parser.cc"
    break;

  case 412: // special_id: "capi"
#line 1448 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 3951 "parser.cc"
    break;

  case 413: // special_id: "prim"
#line 1449 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 3957 "parser.cc"
    break;

  case 414: // special_id: "javascript"
#line 1450 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 3963 "parser.cc"
    break;

  case 415: // special_id: "group"
#line 1451 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 3969 "parser.cc"
    break;

  case 416: // special_id: "stock"
#line 1452 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 3975 "parser.cc"
    break;

  case 417: // special_id: "anyclass"
#line 1453 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 3981 "parser.cc"
    break;

  case 418: // special_id: "via"
#line 1454 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 3987 "parser.cc"
    break;

  case 419: // special_id: "unit"
#line 1455 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 3993 "parser.cc"
    break;

  case 420: // special_id: "dependency"
#line 1456 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 3999 "parser.cc"
    break;

  case 421: // special_id: "signature"
#line 1457 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 4005 "parser.cc"
    break;

  case 422: // special_sym: "!"
#line 1459 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 4011 "parser.cc"
    break;

  case 423: // special_sym: "."
#line 1460 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 4017 "parser.cc"
    break;

  case 424: // special_sym: "*"
#line 1461 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 4023 "parser.cc"
    break;

  case 425: // qconid: conid
#line 1465 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4029 "parser.cc"
    break;

  case 426: // qconid: "QCONID"
#line 1466 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4035 "parser.cc"
    break;

  case 427: // conid: "CONID"
#line 1468 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4041 "parser.cc"
    break;

  case 428: // qconsym: consym
#line 1470 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4047 "parser.cc"
    break;

  case 429: // qconsym: "QCONSYM"
#line 1471 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4053 "parser.cc"
    break;

  case 430: // consym: "CONSYM"
#line 1473 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4059 "parser.cc"
    break;

  case 431: // consym: ":"
#line 1474 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4065 "parser.cc"
    break;

  case 432: // literal: "CHAR"
#line 1478 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < char > ();}
#line 4071 "parser.cc"
    break;

  case 433: // literal: "STRING"
#line 1479 "parser.y"
                  {yylhs.value.as < expression_ref > () = yy_make_string(yystack_[0].value.as < std::string > ());}
#line 4077 "parser.cc"
    break;

  case 434: // literal: "INTEGER"
#line 1480 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < int > ();}
#line 4083 "parser.cc"
    break;

  case 435: // literal: "RATIONAL"
#line 1481 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < double > ();}
#line 4089 "parser.cc"
    break;

  case 437: // close: error
#line 1489 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 4095 "parser.cc"
    break;

  case 438: // modid: "CONID"
#line 1493 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4101 "parser.cc"
    break;

  case 439: // modid: "QCONID"
#line 1494 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4107 "parser.cc"
    break;

  case 440: // commas: commas ","
#line 1496 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 4113 "parser.cc"
    break;

  case 441: // commas: ","
#line 1497 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 4119 "parser.cc"
    break;


#line 4123 "parser.cc"

            default:
              break;
            }
        }
#if YY_EXCEPTIONS
      catch (const syntax_error& yyexc)
        {
          YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
          error (yyexc);
          YYERROR;
        }
#endif // YY_EXCEPTIONS
      YY_SYMBOL_PRINT ("-> $$ =", yylhs);
      yypop_ (yylen);
      yylen = 0;

      // Shift the result of the reduction.
      yypush_ (YY_NULLPTR, YY_MOVE (yylhs));
    }
    goto yynewstate;


  /*--------------------------------------.
  | yyerrlab -- here on detecting error.  |
  `--------------------------------------*/
  yyerrlab:
    // If not already recovering from an error, report this error.
    if (!yyerrstatus_)
      {
        ++yynerrs_;
        context yyctx (*this, yyla);
        std::string msg = yysyntax_error_ (yyctx);
        error (yyla.location, YY_MOVE (msg));
      }


    yyerror_range[1].location = yyla.location;
    if (yyerrstatus_ == 3)
      {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */

        // Return failure if at end of input.
        if (yyla.kind () == symbol_kind::S_YYEOF)
          YYABORT;
        else if (!yyla.empty ())
          {
            yy_destroy_ ("Error: discarding", yyla);
            yyla.clear ();
          }
      }

    // Else will try to reuse lookahead token after shifting the error token.
    goto yyerrlab1;


  /*---------------------------------------------------.
  | yyerrorlab -- error raised explicitly by YYERROR.  |
  `---------------------------------------------------*/
  yyerrorlab:
    /* Pacify compilers when the user code never invokes YYERROR and
       the label yyerrorlab therefore never appears in user code.  */
    if (false)
      YYERROR;

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYERROR.  */
    yypop_ (yylen);
    yylen = 0;
    YY_STACK_PRINT ();
    goto yyerrlab1;


  /*-------------------------------------------------------------.
  | yyerrlab1 -- common code for both syntax error and YYERROR.  |
  `-------------------------------------------------------------*/
  yyerrlab1:
    yyerrstatus_ = 3;   // Each real token shifted decrements this.
    // Pop stack until we find a state that shifts the error token.
    for (;;)
      {
        yyn = yypact_[+yystack_[0].state];
        if (!yy_pact_value_is_default_ (yyn))
          {
            yyn += symbol_kind::S_YYerror;
            if (0 <= yyn && yyn <= yylast_
                && yycheck_[yyn] == symbol_kind::S_YYerror)
              {
                yyn = yytable_[yyn];
                if (0 < yyn)
                  break;
              }
          }

        // Pop the current state because it cannot handle the error token.
        if (yystack_.size () == 1)
          YYABORT;

        yyerror_range[1].location = yystack_[0].location;
        yy_destroy_ ("Error: popping", yystack_[0]);
        yypop_ ();
        YY_STACK_PRINT ();
      }
    {
      stack_symbol_type error_token;

      yyerror_range[2].location = yyla.location;
      YYLLOC_DEFAULT (error_token.location, yyerror_range, 2);

      // Shift the error token.
      error_token.state = state_type (yyn);
      yypush_ ("Shifting", YY_MOVE (error_token));
    }
    goto yynewstate;


  /*-------------------------------------.
  | yyacceptlab -- YYACCEPT comes here.  |
  `-------------------------------------*/
  yyacceptlab:
    yyresult = 0;
    goto yyreturn;


  /*-----------------------------------.
  | yyabortlab -- YYABORT comes here.  |
  `-----------------------------------*/
  yyabortlab:
    yyresult = 1;
    goto yyreturn;


  /*-----------------------------------------------------.
  | yyreturn -- parsing is finished, return the result.  |
  `-----------------------------------------------------*/
  yyreturn:
    if (!yyla.empty ())
      yy_destroy_ ("Cleanup: discarding lookahead", yyla);

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYABORT or YYACCEPT.  */
    yypop_ (yylen);
    YY_STACK_PRINT ();
    while (1 < yystack_.size ())
      {
        yy_destroy_ ("Cleanup: popping", yystack_[0]);
        yypop_ ();
      }

    return yyresult;
  }
#if YY_EXCEPTIONS
    catch (...)
      {
        YYCDEBUG << "Exception caught: cleaning lookahead and stack\n";
        // Do not try to display the values of the reclaimed symbols,
        // as their printers might throw an exception.
        if (!yyla.empty ())
          yy_destroy_ (YY_NULLPTR, yyla);

        while (1 < yystack_.size ())
          {
            yy_destroy_ (YY_NULLPTR, yystack_[0]);
            yypop_ ();
          }
        throw;
      }
#endif // YY_EXCEPTIONS
  }

  void
  parser::error (const syntax_error& yyexc)
  {
    error (yyexc.location, yyexc.what ());
  }

  /* Return YYSTR after stripping away unnecessary quotes and
     backslashes, so that it's suitable for yyerror.  The heuristic is
     that double-quoting is unnecessary unless the string contains an
     apostrophe, a comma, or backslash (other than backslash-backslash).
     YYSTR is taken from yytname.  */
  std::string
  parser::yytnamerr_ (const char *yystr)
  {
    if (*yystr == '"')
      {
        std::string yyr;
        char const *yyp = yystr;

        for (;;)
          switch (*++yyp)
            {
            case '\'':
            case ',':
              goto do_not_strip_quotes;

            case '\\':
              if (*++yyp != '\\')
                goto do_not_strip_quotes;
              else
                goto append;

            append:
            default:
              yyr += *yyp;
              break;

            case '"':
              return yyr;
            }
      do_not_strip_quotes: ;
      }

    return yystr;
  }

  std::string
  parser::symbol_name (symbol_kind_type yysymbol)
  {
    return yytnamerr_ (yytname_[yysymbol]);
  }



  // parser::context.
  parser::context::context (const parser& yyparser, const symbol_type& yyla)
    : yyparser_ (yyparser)
    , yyla_ (yyla)
  {}

  int
  parser::context::expected_tokens (symbol_kind_type yyarg[], int yyargn) const
  {
    // Actual number of expected tokens
    int yycount = 0;

    int yyn = yypact_[+yyparser_.yystack_[0].state];
    if (!yy_pact_value_is_default_ (yyn))
      {
        /* Start YYX at -YYN if negative to avoid negative indexes in
           YYCHECK.  In other words, skip the first -YYN actions for
           this state because they are default actions.  */
        int yyxbegin = yyn < 0 ? -yyn : 0;
        // Stay within bounds of both yycheck and yytname.
        int yychecklim = yylast_ - yyn + 1;
        int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
        for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
          if (yycheck_[yyx + yyn] == yyx && yyx != symbol_kind::S_YYerror
              && !yy_table_value_is_error_ (yytable_[yyx + yyn]))
            {
              if (!yyarg)
                ++yycount;
              else if (yycount == yyargn)
                return 0;
              else
                yyarg[yycount++] = YY_CAST (symbol_kind_type, yyx);
            }
      }

    if (yyarg && yycount == 0 && 0 < yyargn)
      yyarg[0] = symbol_kind::S_YYEMPTY;
    return yycount;
  }



  int
  parser::yy_syntax_error_arguments_ (const context& yyctx,
                                                 symbol_kind_type yyarg[], int yyargn) const
  {
    /* There are many possibilities here to consider:
       - If this state is a consistent state with a default action, then
         the only way this function was invoked is if the default action
         is an error action.  In that case, don't check for expected
         tokens because there are none.
       - The only way there can be no lookahead present (in yyla) is
         if this state is a consistent state with a default action.
         Thus, detecting the absence of a lookahead is sufficient to
         determine that there is no unexpected or expected token to
         report.  In that case, just report a simple "syntax error".
       - Don't assume there isn't a lookahead just because this state is
         a consistent state with a default action.  There might have
         been a previous inconsistent state, consistent state with a
         non-default action, or user semantic action that manipulated
         yyla.  (However, yyla is currently not documented for users.)
       - Of course, the expected token list depends on states to have
         correct lookahead information, and it depends on the parser not
         to perform extra reductions after fetching a lookahead from the
         scanner and before detecting a syntax error.  Thus, state merging
         (from LALR or IELR) and default reductions corrupt the expected
         token list.  However, the list is correct for canonical LR with
         one exception: it will still contain any token that will not be
         accepted due to an error action in a later state.
    */

    if (!yyctx.lookahead ().empty ())
      {
        if (yyarg)
          yyarg[0] = yyctx.token ();
        int yyn = yyctx.expected_tokens (yyarg ? yyarg + 1 : yyarg, yyargn - 1);
        return yyn + 1;
      }
    return 0;
  }

  // Generate an error message.
  std::string
  parser::yysyntax_error_ (const context& yyctx) const
  {
    // Its maximum.
    enum { YYARGS_MAX = 5 };
    // Arguments of yyformat.
    symbol_kind_type yyarg[YYARGS_MAX];
    int yycount = yy_syntax_error_arguments_ (yyctx, yyarg, YYARGS_MAX);

    char const* yyformat = YY_NULLPTR;
    switch (yycount)
      {
#define YYCASE_(N, S)                         \
        case N:                               \
          yyformat = S;                       \
        break
      default: // Avoid compiler warnings.
        YYCASE_ (0, YY_("syntax error"));
        YYCASE_ (1, YY_("syntax error, unexpected %s"));
        YYCASE_ (2, YY_("syntax error, unexpected %s, expecting %s"));
        YYCASE_ (3, YY_("syntax error, unexpected %s, expecting %s or %s"));
        YYCASE_ (4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
        YYCASE_ (5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
      }

    std::string yyres;
    // Argument number.
    std::ptrdiff_t yyi = 0;
    for (char const* yyp = yyformat; *yyp; ++yyp)
      if (yyp[0] == '%' && yyp[1] == 's' && yyi < yycount)
        {
          yyres += symbol_name (yyarg[yyi++]);
          ++yyp;
        }
      else
        yyres += *yyp;
    return yyres;
  }


  const short parser::yypact_ninf_ = -595;

  const short parser::yytable_ninf_ = -401;

  const short
  parser::yypact_[] =
  {
      18,    73,  -595,    89,  -595,  -595,  -595,  -595,  -595,   109,
     -31,   -39,  -595,    48,   -38,   -38,    25,  -595,  -595,  -595,
    -595,    86,  -595,  -595,  -595,    38,  -595,   128,   150,  3571,
     219,   181,   146,  -595,   632,  -595,   -13,  -595,  -595,  -595,
    -595,    73,  -595,   116,  -595,  -595,  -595,  -595,  -595,  -595,
    -595,  -595,  -595,  -595,  -595,  -595,  -595,  -595,  -595,  -595,
    -595,  -595,  -595,  -595,  -595,   429,  -595,  -595,  -595,  -595,
     166,   169,  -595,   192,  -595,  -595,  -595,  -595,  -595,  -595,
    -595,  -595,   205,   228,   274,  -595,   199,  -595,  2001,  3233,
    -595,   211,   214,  2001,  -595,  -595,  -595,   312,   230,  -595,
    3233,  3892,   214,  2727,   220,   212,  3847,   218,  2364,  2727,
    2485,  2727,  1140,  1012,   118,  -595,  -595,  -595,  -595,  -595,
    -595,    26,   220,   217,   146,  -595,  -595,  -595,   281,  -595,
    -595,   173,  -595,  2606,  -595,   259,  -595,  -595,  -595,  -595,
    -595,   249,   279,   257,  -595,  -595,  -595,  -595,   247,  -595,
     120,  -595,  -595,   268,   272,  -595,  -595,  -595,  -595,  -595,
     273,  -595,   283,   284,   285,  -595,  -595,  -595,  3571,  3604,
    -595,  -595,  -595,  -595,  -595,  -595,   353,  -595,   -36,  1012,
     366,   414,  -595,  -595,  2001,  -595,  -595,  -595,  -595,  -595,
    -595,  -595,  -595,  4014,  2930,  2829,   276,  3755,  -595,  -595,
    -595,  -595,  -595,   371,  3663,  -595,   311,  -595,   180,  3233,
    -595,  -595,  -595,  -595,  -595,  -595,  -595,  -595,  -595,  -595,
    -595,  3031,  1517,  1517,  -595,   289,   327,   331,   334,   336,
    3031,   761,   761,  -595,   400,   338,   335,   282,  4071,   293,
     294,  -595,  -595,  -595,  -595,   -10,  3847,  -595,   340,   188,
     -19,   321,   -15,   313,   346,  -595,  -595,  2727,  -595,  -595,
    2243,  -595,  2606,   210,  -595,  -595,  3334,  -595,  -595,  -595,
     414,    10,   325,   316,  -595,  2001,  -595,  -595,  -595,  -595,
    -595,  -595,  -595,  2485,  -595,  -595,   101,   121,   284,   323,
     324,   326,   122,  -595,   148,  3031,  3847,  3847,  -595,    81,
     199,   305,  3233,  3031,  2001,  1759,  3334,  -595,    52,  -595,
    -595,  2122,  -595,  -595,  -595,  -595,  -595,  3663,  -595,  3788,
    4014,  2727,  -595,   330,   332,  -595,  -595,  -595,  -595,  -595,
    -595,  -595,  -595,   337,   329,  -595,  -595,   339,    48,  -595,
     307,   369,   370,   248,  3031,  2001,  -595,    74,   348,   341,
    -595,  -595,  -595,  -595,   345,   361,  -595,   344,   330,  -595,
      79,   343,   332,   141,   161,   349,   350,   230,  -595,  -595,
    3233,  3031,  -595,  -595,   352,   355,   230,   214,  2727,   368,
     373,    64,  -595,  -595,    45,  -595,   441,  -595,  -595,  -595,
    -595,  -595,  -595,   371,    69,  -595,  -595,   173,    49,  2001,
    3031,   362,   356,   354,   357,   347,   374,   377,  -595,  -595,
     395,   365,   218,    76,   397,  -595,  2001,  -595,  -595,   383,
     385,   392,  2001,  2001,  1759,  1268,  -595,  1268,   613,  -595,
    1268,  -595,  1268,   363,  -595,  -595,  -595,  -595,   404,   403,
     409,  3981,   396,  -595,  -595,  -595,  -595,   -14,   288,  -595,
    -595,  -595,   371,   428,   398,  -595,   399,  -595,  -595,  -595,
    -595,  -595,   413,  -595,   401,   437,    83,  -595,  -595,  -595,
    -595,  3604,  -595,  -595,  -595,    73,  -595,  -595,  1396,   891,
    -595,  -595,  -595,  3031,  4014,  -595,  4014,  4104,  -595,  3031,
    -595,  3031,  -595,  3031,  -595,  -595,  -595,  -595,  -595,  -595,
    -595,  -595,  3031,   400,  -595,  -595,  2001,  -595,  1517,  -595,
    2001,  -595,  -595,   761,  -595,  -595,  -595,  -595,  -595,   394,
     402,   419,  -595,  -595,  -595,  -595,  -595,   420,   742,   167,
    -595,  -595,  -595,  -595,  -595,  -595,  -595,  -595,   417,  -595,
     449,  -595,  -595,  -595,  -595,  -595,  3031,  3031,   421,    81,
    -595,   451,  3031,   506,  -595,   530,  -595,  2001,  1759,  -595,
    -595,  3788,  1268,  3031,   426,   537,  2727,  -595,  1638,  -595,
     442,   431,  -595,   269,    48,  -595,  -595,  -595,  -595,  3031,
    4163,  -595,  -595,  -595,  -595,   438,   439,  -595,  -595,  -595,
     289,  -595,  -595,  -595,  -595,  -595,  -595,  1759,  2001,  -595,
      -7,    53,  -595,  -595,  -595,  -595,  -595,   466,  -595,  3663,
      31,  -595,   530,  -595,  -595,  -595,  -595,  -595,  -595,  -595,
      73,    50,  -595,  -595,  -595,  1880,  1759,  2001,  -595,    60,
    -595,  -595,  -595,   471,  -595,  -595,   543,  -595,  -595,  -595,
    3031,  -595,  4130,   506,   468,  3379,  -595,  -595,  -595,  -595,
    -595,  -595,  3132,   154,   507,  -595,  -595,  -595,  -595,   452,
    3571,  -595,  -595,  -595,   476,   371,  -595,  -595,  3031,  2001,
    -595,  -595,  -595,  3663,   445,  -595,  3663,  -595,  -595,   453,
     457,  -595,  3233,  -595,  3571,   458,  2001,  -595,   461,  -595,
    3471,  -595,  3663,  3233,  -595,  -595,   462,  -595,  -595,  -595,
    -595,  -595
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    39,     0,     2,    39,     4,   438,   439,     8,
       0,    42,     1,     0,     0,     0,    18,    11,    38,    13,
      16,    67,   437,   436,    12,   114,   110,     0,     0,     0,
       0,    45,    40,    15,    14,   113,     0,     6,     7,   404,
     406,     0,   405,     0,   392,   407,   408,   409,   390,   391,
     389,   393,   394,   410,   411,   412,   413,   414,   415,   416,
     417,   418,   419,   421,   420,     0,   387,   350,   386,   348,
       0,    19,    21,    24,    32,    35,   340,   349,    34,   382,
     385,   388,     0,     0,    47,    37,    41,   249,     0,     0,
      92,     0,     0,     0,    60,    61,    62,    87,     0,    93,
       0,     0,     0,     0,   209,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   427,   426,   432,   433,   434,
     435,   209,   209,    58,    65,    68,    69,    70,   100,    72,
     190,    73,   217,   221,   231,   240,   242,   244,   310,   323,
     311,     0,   243,   385,   312,   425,   245,   111,     0,    23,
       0,    33,   337,     0,     0,   401,   422,   424,   423,   402,
       0,   399,     0,     0,     0,   400,   403,    17,     0,    26,
      22,    39,    39,     3,    44,    46,    51,    36,     0,     0,
       0,   214,   222,   215,     0,   378,   379,   377,   356,   125,
     357,   124,   146,   175,     0,     0,     0,     0,   375,   355,
     354,   352,   351,   109,     0,   123,     0,    97,   132,   135,
     138,   140,   144,   332,   141,   345,   353,   145,   142,   373,
     376,   158,   297,   297,   238,   225,     0,     0,     0,     0,
       0,   104,   104,   107,     0,     0,   132,     0,     0,     0,
       0,   380,   360,   239,   230,     0,     0,   210,     0,     0,
       0,     0,     0,   317,   115,   316,   314,     0,   288,   291,
       0,   233,   219,     0,   431,   324,     0,   430,   429,   250,
     214,   255,     0,   256,   366,     0,   367,   365,   371,   398,
     397,   327,   428,   401,   319,   441,     0,     0,   398,     0,
     397,   327,     0,   321,     0,     0,     0,     0,    59,     0,
      66,     0,     0,     0,     0,     0,     0,   192,   109,   197,
     363,     0,   364,   362,   369,   396,   395,     0,   228,   304,
       0,     0,   112,     0,     0,   343,   344,   342,   341,   384,
     383,    20,    31,     0,    27,    29,    30,     0,     0,    50,
      49,     0,     0,     0,     0,     0,   223,     0,     0,   176,
     178,   120,   162,   336,     0,     0,   128,     0,   125,   149,
     159,     0,   345,     0,     0,     0,     0,     0,    78,   147,
       0,     0,   139,   159,     0,   157,     0,     0,     0,   301,
       0,     0,   296,   298,     0,   224,     0,    84,    83,    85,
      86,   154,   117,   109,     0,   193,   103,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   220,   204,
       0,     0,     0,     0,     0,   289,     0,   290,   191,     0,
       0,     0,   251,   257,     0,     0,   248,     0,   252,   246,
       0,   247,     0,   383,   313,   320,   440,   322,     0,     0,
       0,     0,   201,   359,    64,   358,   325,     0,    94,   199,
     129,   118,   109,     0,   266,   268,     0,   195,   196,   218,
     229,   307,     0,   303,   306,   309,     0,   232,   339,   338,
      25,     0,     9,    10,    48,     0,   227,   226,     0,     0,
     237,   213,   216,     0,     0,   148,     0,     0,   151,     0,
     335,     0,   152,     0,   333,   334,   346,   374,   108,    96,
     133,    71,     0,   302,   299,   287,     0,   292,   295,   293,
       0,    82,   105,   102,   106,   235,    79,   381,   361,    77,
      75,     0,   211,   203,   205,   315,   318,     0,     0,     0,
     116,   329,   202,   234,   368,   372,   328,   259,   261,   265,
     250,   263,   262,   254,   253,   208,     0,     0,     0,     0,
      99,     0,     0,   172,    81,   180,   194,     0,     0,   370,
     241,     0,     0,     0,     0,    53,     0,   271,     0,   284,
       0,   273,   277,     0,     0,   272,   179,   119,   177,     0,
       0,   161,   163,   127,   165,     0,   160,   160,   300,   294,
     225,   101,    76,    74,   212,   330,   331,     0,   258,   121,
       0,     0,   326,    63,    98,    95,   162,   166,   168,     0,
       0,    80,   181,   183,   198,   267,   305,   308,   200,    28,
       0,    55,   285,   274,   269,   276,     0,     0,   278,   109,
     282,   270,   126,     0,   153,   150,     0,   264,   260,   206,
       0,   207,     0,   172,     0,   173,   136,   143,   170,    90,
      88,    89,     0,     0,   184,   187,   347,   182,    52,     0,
       0,    43,    54,   275,     0,   109,   280,   281,     0,     0,
     122,   171,   167,     0,     0,   137,     0,   188,   134,   155,
       0,   185,     0,   186,     0,     0,     0,   279,     0,   236,
     173,   169,   174,     0,   189,    91,     0,    56,   283,   164,
     156,    57
  };

  const short
  parser::yypgoto_[] =
  {
    -595,  -595,  -595,  -595,  -595,  -595,  -595,    54,  -595,  -595,
    -508,  -595,   405,  -595,  -595,  -595,   103,  -145,  -595,   448,
    -595,  -595,  -595,  -595,  -595,  -595,  -595,  -595,  -595,  -595,
    -595,  -595,  -595,  -595,  -595,  -595,  -595,  -595,  -595,  -595,
    -595,  -595,  -595,  -595,  -595,   275,  -595,   358,  -595,  -281,
    -287,   560,  -595,  -595,  -296,    13,   258,    32,  -595,  -595,
    -169,   182,   -55,  -595,   -88,  -595,   -97,  -375,  -595,   372,
    -574,  -189,   296,  -113,  -595,   367,    -9,  -595,  -512,  -595,
    -595,   -50,  -595,   -78,  -595,  -595,   113,  -595,  -595,   -12,
     -51,   567,    90,   351,  -595,   299,  -595,    68,  -595,   -82,
     -71,   576,   -11,  -277,    21,  -595,   -67,   -76,  -595,  -595,
     -65,  -595,  -595,  -595,  -595,    -8,  -595,  -595,  -425,  -595,
      -6,  -595,  -595,   -17,  -595,  -595,   360,  -595,   -74,   390,
     107,  -269,  -595,    55,  -595,  -595,  -595,  -595,   209,  -595,
     -93,  -594,   -58,  -595,   204,   579,  -595,  -595,  -595,   -27,
    -595,  -140,  -595,    75,   522,  -144,  -595,   -33,  -595,  -595,
    -449,  -595,   430,   -72,   -29,  -194,   -25,  -595,  -595,   -46,
     -57,   -95,   -84,  -595,  -191,  -103,   -52,  -239,  -595,  -318,
     -34,   -92
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   173,     6,    10,    19,    30,
      70,    71,    72,   170,   333,   334,   335,    73,    74,    86,
      11,    20,    21,    32,    84,   176,   475,   340,   621,   661,
     662,   299,   123,   442,    33,    34,   124,   125,   126,   127,
     230,   653,   683,   128,   554,   203,   302,   394,   233,   234,
     368,    27,    36,   414,   391,   449,   347,   600,   204,   205,
     392,   451,   355,   644,   356,   679,   208,   645,   209,   210,
     646,   211,   393,   680,   374,   361,   487,   581,   585,   555,
     607,   608,   609,   648,   348,   349,   350,   611,   612,   613,
     654,   395,   396,   307,   308,   309,   130,   246,   247,   379,
     181,   397,   182,   183,   386,   184,   133,   134,   135,   136,
     286,   287,   272,   273,   538,   453,   454,   480,   570,   571,
     572,   628,   629,   630,   573,   380,   259,   260,   224,   381,
     382,   383,   462,   463,   464,   137,   138,   253,   254,   139,
     140,   443,   274,   530,   212,   213,    75,   214,   655,   152,
      77,   215,   216,   444,   445,   311,   275,   276,   313,   277,
     217,   218,   219,   141,   142,    79,    80,   314,   278,   279,
     316,   165,    81,   166,   144,   145,   281,   282,   146,    24,
       9,   292
  };

  const short
  parser::yytable_[] =
  {
      78,   207,    76,   236,   256,   220,   180,   149,   164,   143,
     324,   225,   235,   411,   255,   369,   220,   280,   290,   163,
     473,   457,   294,   132,   336,   354,   360,   244,   243,   240,
     269,   269,   258,   261,   206,   263,   455,   345,   582,     1,
     605,   270,   270,   262,   242,   295,    22,   271,   481,    22,
      22,   676,   373,   649,   574,   362,   289,   318,   409,    13,
     446,   291,   659,   550,   264,    25,   509,   288,   482,    17,
     639,   675,   420,   310,    18,   421,   241,   250,   405,   367,
     514,   341,   650,   651,   290,   315,   498,   367,   423,    12,
      26,   147,   342,   284,   424,   503,   676,   269,   312,   285,
      31,   148,   346,   363,   364,   267,   511,   640,   270,   410,
     220,   220,   456,   220,   551,   421,   675,     2,   675,   406,
     220,   351,   164,   310,   425,   220,   422,   291,   460,   245,
     641,   633,    29,   288,   450,   315,   305,   220,   652,    78,
      78,    76,    76,   623,   626,   482,   220,    23,   312,    67,
      23,    23,   685,    69,   483,   539,   688,   660,   508,   491,
     264,   575,   513,   563,   507,   556,    35,   640,   241,   512,
     365,   155,   156,   157,    14,    15,   696,   508,   158,   527,
     401,   415,   513,   528,   258,   529,   318,   492,   484,   296,
     297,     7,   164,   582,    67,     8,   441,   484,    69,   188,
     159,   267,   500,   163,   428,    37,   143,   143,   323,   429,
     190,   220,   310,   402,   207,   430,   262,   407,   220,   220,
     132,   132,   452,   150,   315,   337,   338,    38,   293,   431,
     435,   450,   285,   220,    67,   432,   436,   312,    69,   199,
     200,    83,   242,   201,   202,   467,    82,   206,   351,   494,
     599,   599,   264,   303,   304,   436,   631,   305,   437,    85,
     220,   652,   436,   155,   156,   157,   371,   439,   440,  -130,
     158,   495,    67,   236,   167,   436,    69,   596,   155,   156,
     157,   285,   499,   168,   345,   158,   220,   220,   306,   615,
     465,   304,   159,   267,   305,   241,   161,   268,   690,   169,
     459,   692,   505,   504,   171,   174,   172,   159,   175,   256,
     446,   161,   177,   222,   576,   223,   220,   515,   221,   255,
     583,   251,   584,   245,   586,   252,   336,   114,   637,   231,
     280,   232,   280,   587,   533,   280,   115,   280,   548,   310,
     248,   537,   666,   540,   670,   269,   298,   478,   269,   479,
     269,   315,   301,   626,   270,   627,   270,   455,   319,   270,
     541,   270,   542,   320,   312,   543,   321,   544,   552,   553,
     310,  -380,   155,   156,   157,   322,   325,   339,   687,   158,
     326,   327,   315,   584,   226,   227,   228,   229,   324,   343,
     285,   328,   329,   330,   450,   312,   569,   569,   367,   220,
     370,   159,   385,   220,   387,   220,   446,   220,   388,   220,
     632,   389,   577,   390,   351,   399,   402,   408,   220,   400,
     647,   371,   403,   404,   588,   265,   413,   412,   590,   426,
     427,   433,  -400,   447,   434,   474,   363,   364,   468,   472,
     469,   565,    78,   471,    76,   470,   476,   477,   485,   488,
     489,   446,   490,  -286,   523,   486,   647,   493,   506,   241,
     501,   241,   220,   220,   496,   497,   510,   280,   220,   502,
     517,   518,   524,   525,   532,   614,   521,  -381,   522,   220,
     269,   545,   519,   546,   647,   520,   569,   647,   143,   547,
     622,   270,   242,   264,   344,   220,   220,   617,   534,   584,
     535,   647,   132,   647,   155,   156,   157,   536,   153,   557,
     549,   158,   558,   560,   559,   561,   638,   154,   562,   155,
     156,   157,   592,   594,   353,   220,   158,   598,   604,   306,
     593,   597,   465,   159,   267,   606,   602,   161,   268,   610,
     619,   620,   624,   569,   625,   665,   634,   635,   159,   160,
     643,   668,   161,   162,   669,   236,   220,   673,   220,   684,
     682,   220,   686,   115,   678,   694,   697,   693,   220,   699,
     701,   548,   300,   331,   564,    28,   618,   448,   466,   601,
     700,   372,   516,   656,   220,   236,   658,   689,   375,   220,
     398,   438,   220,   672,   695,   691,   236,   642,   220,   578,
     657,   129,   681,   591,   698,   678,   220,   458,   220,   220,
     131,   636,   667,   384,   418,   589,   616,   531,   664,   663,
     417,   526,   151,   239,   603,     0,   656,   366,     0,     0,
       0,    78,     0,    76,     0,    87,    39,    88,    89,    90,
      91,     0,    92,     0,    40,    93,     0,     0,    94,    95,
      96,    97,    98,     0,    99,    78,    42,    76,   100,     0,
     101,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,   103,    59,    60,    61,    62,    63,    64,   104,
       0,     0,   264,     0,   105,   106,     0,     0,     0,     0,
       0,     0,     0,   155,   156,   157,     0,     0,   107,     0,
     158,     0,     0,     0,   108,     0,     0,     0,     0,     0,
     109,     0,   110,   111,     0,     0,     0,     0,   306,     0,
       0,     0,   159,   267,     0,   112,   161,   268,     0,   113,
       0,   114,     0,     0,     0,     0,     0,     0,     0,    66,
     115,     0,     0,    68,   116,     0,     0,     0,     0,   117,
     118,   119,   120,     0,    87,    39,    88,     0,     0,   121,
     122,    92,     0,    40,    93,     0,     0,    94,    95,    96,
       0,    98,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,   103,    59,    60,    61,    62,    63,    64,   104,     0,
       0,   188,     0,   105,   106,     0,     0,     0,   357,     0,
     323,     0,   190,     0,     0,     0,     0,   107,     0,     0,
       0,     0,     0,   108,     0,     0,     0,     0,     0,   109,
     595,   110,   111,     0,     0,     0,   285,     0,     0,     0,
       0,   199,   200,     0,   112,   201,   202,     0,   113,     0,
     114,     0,     0,     0,     0,     0,     0,     0,    66,   115,
       0,     0,    68,   116,     0,     0,     0,     0,   117,   118,
     119,   120,    22,     0,    87,    39,    88,     0,   121,   122,
       0,    92,     0,    40,    93,     0,     0,     0,     0,     0,
       0,    98,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,   103,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,   105,   178,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   108,     0,     0,     0,     0,     0,   109,
       0,   110,   566,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    23,   112,     0,     0,     0,   179,     0,
     114,     0,     0,     0,   568,     0,     0,     0,    66,   115,
       0,     0,    68,   116,     0,    87,    39,    88,   117,   118,
     119,   120,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,   103,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,   105,   178,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   264,     0,     0,   108,     0,     0,     0,     0,     0,
     109,     0,   283,   156,   157,     0,     0,     0,     0,   158,
       0,     0,     0,     0,     0,   112,     0,     0,     0,   179,
     284,   114,     0,     0,     0,     0,   285,   266,     0,    66,
     115,   159,   267,    68,   116,   161,   268,     0,     0,   117,
     118,   119,   120,    87,    39,    88,     0,     0,     0,     0,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
     103,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,   105,   178,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   264,
       0,     0,   108,     0,     0,     0,     0,     0,   109,     0,
     110,   156,   157,     0,     0,     0,     0,   158,     0,     0,
       0,     0,     0,   112,   265,     0,     0,   179,     0,   114,
       0,     0,     0,     0,     0,   266,     0,    66,   115,   159,
     267,    68,   116,   161,   268,     0,     0,   117,   118,   119,
     120,    87,    39,    88,     0,     0,     0,     0,    92,     0,
      40,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,   103,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
     105,   178,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   264,     0,     0,
     108,     0,     0,     0,     0,     0,   109,     0,   110,   156,
     157,     0,     0,     0,     0,   158,     0,     0,     0,     0,
       0,   112,     0,     0,     0,   179,     0,   114,     0,     0,
       0,     0,     0,   266,     0,    66,   115,   159,   267,    68,
     116,   161,   268,     0,     0,   117,   118,   119,   120,    87,
      39,    88,     0,     0,     0,     0,    92,     0,    40,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,   103,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,   105,   178,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   108,     0,
       0,     0,     0,     0,   109,     0,   110,   566,     0,     0,
       0,     0,     0,     0,     0,     0,   567,     0,     0,   112,
       0,     0,     0,   179,     0,   114,     0,     0,     0,   568,
       0,     0,     0,    66,   115,     0,     0,    68,   116,     0,
      87,    39,    88,   117,   118,   119,   120,    92,     0,    40,
      93,     0,     0,     0,     0,     0,     0,   376,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,    55,
      56,    57,     0,   377,    58,     0,     0,   103,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,   105,
     178,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   108,
       0,     0,     0,     0,     0,   109,     0,   110,   378,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     112,     0,     0,     0,   179,     0,   114,     0,     0,     0,
       0,     0,     0,     0,    66,   115,     0,     0,    68,   116,
       0,    87,    39,    88,   117,   118,   119,   120,    92,     0,
      40,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,   103,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
     105,   178,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     108,     0,     0,     0,     0,     0,   109,     0,   110,   566,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   112,     0,     0,     0,   179,     0,   114,     0,     0,
       0,   568,     0,     0,     0,    66,   115,     0,     0,    68,
     116,     0,    87,    39,    88,   117,   118,   119,   120,    92,
       0,    40,    93,     0,     0,     0,     0,     0,     0,   376,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,   103,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,   105,   178,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   108,     0,     0,     0,     0,     0,   109,     0,   110,
     378,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   112,     0,     0,     0,   179,     0,   114,     0,
       0,     0,     0,     0,     0,     0,    66,   115,     0,     0,
      68,   116,     0,    87,    39,    88,   117,   118,   119,   120,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
     103,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,   105,   178,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   108,     0,     0,     0,     0,     0,   109,     0,
     110,   566,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   112,     0,     0,     0,   179,     0,   114,
       0,     0,     0,     0,     0,     0,     0,    66,   115,     0,
       0,    68,   116,     0,    87,    39,    88,   117,   118,   119,
     120,    92,     0,    40,    93,     0,     0,     0,     0,     0,
       0,    98,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,   103,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,   105,   178,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   108,     0,     0,     0,     0,     0,   109,
       0,   110,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   112,     0,     0,     0,   179,     0,
     114,     0,     0,     0,     0,     0,     0,     0,    66,   115,
       0,     0,    68,   116,     0,    87,    39,    88,   117,   118,
     119,   120,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,   103,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,   105,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   108,     0,     0,     0,     0,     0,
     109,     0,   110,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   112,     0,     0,     0,   179,
       0,   114,     0,     0,     0,     0,     0,     0,     0,    66,
     115,     0,     0,    68,   116,     0,    87,    39,    88,   117,
     118,   119,   120,    92,     0,    40,    93,     0,     0,     0,
       0,     0,     0,    98,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   108,     0,     0,     0,   416,
       0,   109,     0,     0,   257,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   112,     0,     0,     0,
     179,     0,   114,     0,     0,     0,     0,     0,     0,     0,
      66,   115,     0,     0,    68,   116,     0,    87,    39,    88,
     117,   118,   119,   120,    92,     0,    40,    93,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   108,     0,     0,     0,
       0,     0,   109,     0,     0,   257,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   112,     0,     0,
       0,   179,     0,   114,     0,     0,     0,     0,     0,     0,
       0,    66,   115,     0,     0,    68,   116,     0,    87,    39,
      88,   117,   118,   119,   120,    92,     0,    40,    93,     0,
       0,     0,     0,     0,     0,    98,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,   103,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   108,     0,     0,
       0,     0,     0,   109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   112,     0,
       0,     0,   179,     0,   114,     0,     0,     0,     0,     0,
       0,     0,    66,   115,     0,     0,    68,   116,     0,    87,
      39,    88,   117,   118,   119,   120,    92,     0,    40,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   108,     0,
       0,     0,     0,     0,   109,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   317,     0,     0,     0,     0,   112,
       0,     0,     0,   179,     0,   114,     0,     0,     0,     0,
       0,     0,     0,    66,   115,     0,     0,    68,   116,     0,
      87,    39,    88,   117,   118,   119,   120,    92,     0,    40,
      93,     0,     0,     0,     0,     0,     0,    98,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   108,
       0,     0,     0,     0,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     112,     0,     0,    39,   179,     0,   114,     0,     0,     0,
       0,    40,     0,     0,    66,   115,     0,     0,    68,   116,
       0,     0,     0,    42,   117,   118,   119,   120,   352,     0,
      45,    46,    47,   185,   186,   187,     0,     0,     0,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,     0,
       0,     0,     0,     0,     0,   357,     0,   358,     0,   190,
     191,   192,     0,     0,     0,     0,     0,     0,   193,     0,
       0,     0,   194,     0,    39,     0,   195,   359,   196,     0,
       0,     0,    40,   285,   197,     0,   198,    67,   199,   200,
       0,    69,   201,   202,    42,     0,     0,     0,     0,   352,
       0,    45,    46,    47,   185,   186,   187,     0,     0,     0,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
       0,     0,     0,     0,     0,     0,     0,     0,   189,     0,
     190,   191,   192,     0,     0,     0,     0,     0,     0,   193,
       0,     0,     0,   194,   353,    39,     0,   195,     0,   196,
       0,     0,     0,    40,     0,   197,     0,   198,    67,   199,
     200,     0,    69,   201,   202,    42,     0,     0,     0,     0,
     352,     0,    45,    46,    47,   185,   186,   187,     0,     0,
       0,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,     0,     0,     0,     0,     0,     0,     0,     0,   189,
       0,   190,   191,   192,     0,     0,     0,     0,     0,     0,
     193,     0,     0,     0,   194,     0,    39,     0,   195,     0,
     196,     0,     0,     0,    40,     0,   197,     0,   198,    67,
     199,   200,     0,    69,   201,   202,    42,     0,     0,     0,
       0,     0,     0,    45,    46,    47,   185,   186,   187,     0,
       0,     0,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,     0,     0,     0,     0,     0,     0,     0,     0,
     189,     0,   190,   191,   192,     0,     0,     0,     0,     0,
       0,   193,     0,     0,     0,   194,     0,    39,     0,   195,
     677,   196,     0,     0,     0,    40,     0,   197,     0,   198,
      67,   199,   200,     0,    69,   201,   202,    42,     0,     0,
       0,     0,     0,     0,    45,    46,    47,   185,   186,   187,
       0,     0,     0,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,     0,     0,     0,     0,     0,     0,     0,
       0,   189,     0,   190,   191,   192,     0,     0,     0,     0,
       0,     0,   193,     0,     0,     0,   194,   419,    39,     0,
     195,     0,   196,     0,     0,     0,    40,     0,   197,     0,
     198,    67,   199,   200,     0,    69,   201,   202,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,    39,     0,    59,    60,    61,    62,    63,
      64,    40,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    42,     0,     0,     0,     0,     0,     0,
      45,    46,    47,   185,   186,   187,     0,     0,     0,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    66,   115,     0,     0,    68,   116,     0,   264,     0,
       0,     0,     0,     0,     0,     0,     0,   189,  -131,     0,
     191,   192,     0,     0,     0,    39,     0,     0,   193,     0,
       0,     0,   194,    40,     0,     0,   195,     0,   196,     0,
       0,     0,     0,     0,   674,    42,   198,    67,     0,   267,
       0,    69,    45,    46,    47,   185,   186,   187,     0,     0,
       0,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     264,     0,     0,     0,     0,     0,     0,     0,     0,   189,
       0,     0,   191,   192,     0,     0,     0,     0,     0,     0,
     193,     0,     0,     0,   194,    39,     0,     0,   195,     0,
     196,     0,     0,    40,     0,     0,   674,     0,   198,    67,
       0,   267,    41,    69,     0,    42,     0,    43,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,    39,    51,
      52,    53,    54,    55,    56,    57,    40,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,    42,     0,
      43,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,    39,     0,     0,
       0,     0,     0,     0,     0,    40,     0,     0,    65,     0,
       0,     0,   332,     0,     0,     0,     0,    42,    66,    67,
       0,     0,    68,    69,    45,    46,    47,   185,   186,   187,
       0,     0,     0,    53,    54,    55,    56,    57,     0,     0,
      58,    65,     0,     0,    59,    60,    61,    62,    63,    64,
       0,    66,    67,     0,     0,    68,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   189,     0,     0,   191,   192,     0,     0,     0,    39,
       0,     0,   193,     0,     0,     0,   194,    40,     0,     0,
     195,     0,   196,     0,     0,     0,     0,     0,     0,    42,
     198,    67,     0,     0,     0,    69,    45,    46,    47,   185,
     186,   187,    39,     0,     0,    53,    54,    55,    56,    57,
      40,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,    39,     0,     0,     0,     0,     0,     0,     0,    40,
       0,     0,     0,     0,     0,     0,   461,     0,     0,     0,
       0,    42,   198,    67,     0,     0,    44,    69,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    55,
      56,    57,     0,     0,    58,   249,    39,     0,    59,    60,
      61,    62,    63,    64,    40,    66,     0,     0,     0,    68,
       0,     0,     0,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,   249,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    66,     0,     0,     0,    68,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   155,   156,   157,    39,     0,     0,     0,   158,
       0,     0,     0,    40,     0,     0,     0,     0,     0,   237,
       0,     0,     0,     0,     0,    42,     0,   238,     0,    66,
      44,   159,    45,    46,    47,    48,    49,    50,    39,    51,
      52,    53,    54,    55,    56,    57,    40,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,    39,     0,     0,     0,     0,
       0,     0,     0,    40,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    42,     0,     0,    66,   115,
      44,     0,    45,    46,    47,    48,    49,    50,    39,    51,
      52,    53,    54,    55,    56,    57,    40,     0,    58,     0,
       0,   237,    59,    60,    61,    62,    63,    64,    42,     0,
       0,    66,     0,     0,    39,    45,    46,    47,   185,   186,
     187,     0,    40,     0,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,    42,    59,    60,    61,    62,    63,
      64,    45,    46,    47,   185,   186,   187,    39,     0,     0,
      53,    54,    55,    56,    57,    40,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,    42,    66,     0,
       0,     0,     0,     0,    45,    46,    47,   185,   186,   187,
       0,   579,     0,    53,    54,    55,    56,    57,     0,     0,
      58,   580,     0,     0,    59,    60,    61,    62,    63,    64,
       0,   198,     0,     0,     0,     0,     0,   671,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   580,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   198,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     198
  };

  const short
  parser::yycheck_[] =
  {
      29,    89,    29,   100,   107,    89,    88,    41,    65,    34,
     150,    93,   100,   252,   107,   204,   100,   112,   113,    65,
     338,   308,   114,    34,   169,   194,   195,   103,   102,   101,
     112,   113,   108,   109,    89,   111,   305,   181,   487,    21,
     552,   112,   113,   110,   101,    19,     1,   112,   344,     1,
       1,   645,   221,    22,   479,   195,   113,   133,    77,     5,
     299,   113,    12,    77,    79,   103,   384,   113,   345,   100,
      77,   645,   266,   131,   113,   266,   101,   106,    88,    27,
     398,   117,    51,    52,   179,   131,   367,    27,    78,     0,
     128,   104,   128,   108,    84,   376,   690,   179,   131,   114,
      14,   114,   184,   195,   196,   120,   393,   114,   179,   128,
     194,   195,   306,   197,   128,   306,   690,    99,   692,   129,
     204,   193,   179,   181,   114,   209,   270,   179,   317,   103,
      77,   580,   107,   179,   303,   181,    84,   221,   107,   168,
     169,   168,   169,   568,    84,   422,   230,   102,   181,   118,
     102,   102,   660,   122,    80,   424,   668,   107,   113,    80,
      79,   479,   113,    80,   100,   452,   128,   114,   193,   100,
     197,    90,    91,    92,    65,    66,   684,   113,    97,   103,
     237,   257,   113,   107,   260,   109,   262,   108,   114,   121,
     122,   118,   249,   642,   118,   122,   115,   114,   122,    79,
     119,   120,   371,   249,   275,    77,   231,   232,    88,   108,
      90,   295,   270,   238,   302,   114,   283,   246,   302,   303,
     231,   232,   304,   107,   270,   171,   172,    77,   110,   108,
     108,   400,   114,   317,   118,   114,   114,   270,   122,   119,
     120,    60,   299,   123,   124,   321,    27,   302,   320,   108,
     546,   547,    79,    80,    81,   114,   574,    84,   110,   113,
     344,   107,   114,    90,    91,    92,    86,   296,   297,    89,
      97,   110,   118,   370,   108,   114,   122,   110,    90,    91,
      92,   114,   370,   114,   428,    97,   370,   371,   115,   558,
     319,    81,   119,   120,    84,   320,   123,   124,   673,   107,
     311,   676,   378,   377,    99,    77,   101,   119,    34,   412,
     549,   123,   113,    99,   483,   101,   400,   399,   107,   412,
     489,   103,   491,   103,   493,   107,   471,   109,   597,    99,
     425,   101,   427,   502,   416,   430,   118,   432,   441,   397,
     128,   423,   629,   425,   640,   427,   129,    99,   430,   101,
     432,   397,    71,    84,   425,    86,   427,   626,    99,   430,
     425,   432,   427,   114,   397,   430,    87,   432,    80,    81,
     428,   114,    90,    91,    92,   128,   108,    24,   665,    97,
     108,   108,   428,   552,    72,    73,    74,    75,   528,    23,
     114,   108,   108,   108,   563,   428,   478,   479,    27,   483,
      89,   119,   113,   487,    77,   489,   645,   491,    77,   493,
     579,    77,   484,    77,   486,    15,   441,    77,   502,    81,
     609,    86,   129,   129,   506,   104,    80,   114,   510,   104,
     114,   108,   108,   128,   108,   128,   528,   529,   108,   100,
     108,   475,   471,   114,   471,   108,    77,    77,   100,   104,
      89,   690,   108,    85,    77,   114,   645,   114,    85,   484,
     108,   486,   546,   547,   115,   115,    25,   562,   552,   114,
     108,   115,    77,   108,    77,   557,   129,   114,   104,   563,
     562,    77,   128,    80,   673,   128,   568,   676,   513,    80,
     566,   562,   549,    79,    80,   579,   580,   562,   115,   668,
     115,   690,   513,   692,    90,    91,    92,   115,    79,    81,
     114,    97,   114,   100,   115,   114,   598,    88,    81,    90,
      91,    92,   128,   104,   104,   609,    97,    78,    77,   115,
     128,   114,   561,   119,   120,    29,   115,   123,   124,     9,
     114,     4,   100,   625,   113,   627,   108,   108,   119,   120,
      84,    80,   123,   124,    11,   652,   640,    89,   642,   107,
      53,   645,    86,   118,   652,   108,   108,   114,   652,   108,
     108,   674,   124,   168,   471,    15,   563,   302,   320,   547,
     693,   209,   400,   610,   668,   682,   620,   669,   221,   673,
     232,   295,   676,   643,   682,   673,   693,   606,   682,   486,
     612,    34,   653,   513,   686,   693,   690,   308,   692,   693,
      34,   590,   629,   223,   263,   508,   561,   413,   626,   625,
     260,   412,    43,   101,   549,    -1,   653,   197,    -1,    -1,
      -1,   660,    -1,   660,    -1,     3,     4,     5,     6,     7,
       8,    -1,    10,    -1,    12,    13,    -1,    -1,    16,    17,
      18,    19,    20,    -1,    22,   684,    24,   684,    26,    -1,
      28,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    -1,    79,    -1,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    91,    92,    -1,    -1,    76,    -1,
      97,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    91,    -1,    -1,    -1,    -1,   115,    -1,
      -1,    -1,   119,   120,    -1,   103,   123,   124,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,
     128,   129,   130,    -1,     3,     4,     5,    -1,    -1,   137,
     138,    10,    -1,    12,    13,    -1,    -1,    16,    17,    18,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      -1,    79,    -1,    62,    63,    -1,    -1,    -1,    86,    -1,
      88,    -1,    90,    -1,    -1,    -1,    -1,    76,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
     108,    90,    91,    -1,    -1,    -1,   114,    -1,    -1,    -1,
      -1,   119,   120,    -1,   103,   123,   124,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,
     129,   130,     1,    -1,     3,     4,     5,    -1,   137,   138,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   102,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,   113,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,     3,     4,     5,   127,   128,
     129,   130,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,    97,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
     108,   109,    -1,    -1,    -1,    -1,   114,   115,    -1,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,   127,
     128,   129,   130,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    92,    -1,    -1,    -1,    -1,    97,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,    -1,   107,    -1,   109,
      -1,    -1,    -1,    -1,    -1,   115,    -1,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,   127,   128,   129,
     130,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,
      92,    -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,    -1,    -1,   115,    -1,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,   127,   128,   129,   130,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,   103,
      -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,   113,
      -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,
       3,     4,     5,   127,   128,   129,   130,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    46,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,     3,     4,     5,   127,   128,   129,   130,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,   113,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,
     122,    -1,     3,     4,     5,   127,   128,   129,   130,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,     3,     4,     5,   127,   128,   129,   130,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,
      -1,   121,   122,    -1,     3,     4,     5,   127,   128,   129,
     130,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,     3,     4,     5,   127,   128,
     129,   130,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,     3,     4,     5,   127,
     128,   129,   130,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    86,
      -1,    88,    -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,     3,     4,     5,
     127,   128,   129,   130,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    -1,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,     3,     4,
       5,   127,   128,   129,   130,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,     3,
       4,     5,   127,   128,   129,   130,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,
       3,     4,     5,   127,   128,   129,   130,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,     4,   107,    -1,   109,    -1,    -1,    -1,
      -1,    12,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,    -1,    -1,    24,   127,   128,   129,   130,    29,    -1,
      31,    32,    33,    34,    35,    36,    -1,    -1,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    86,    -1,    88,    -1,    90,
      91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,
      -1,    -1,   103,    -1,     4,    -1,   107,   108,   109,    -1,
      -1,    -1,    12,   114,   115,    -1,   117,   118,   119,   120,
      -1,   122,   123,   124,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,    -1,   103,   104,     4,    -1,   107,    -1,   109,
      -1,    -1,    -1,    12,    -1,   115,    -1,   117,   118,   119,
     120,    -1,   122,   123,   124,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    -1,    -1,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,    -1,   103,    -1,     4,    -1,   107,    -1,
     109,    -1,    -1,    -1,    12,    -1,   115,    -1,   117,   118,
     119,   120,    -1,   122,   123,   124,    24,    -1,    -1,    -1,
      -1,    -1,    -1,    31,    32,    33,    34,    35,    36,    -1,
      -1,    -1,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,    -1,   103,    -1,     4,    -1,   107,
     108,   109,    -1,    -1,    -1,    12,    -1,   115,    -1,   117,
     118,   119,   120,    -1,   122,   123,   124,    24,    -1,    -1,
      -1,    -1,    -1,    -1,    31,    32,    33,    34,    35,    36,
      -1,    -1,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,    -1,   103,     3,     4,    -1,
     107,    -1,   109,    -1,    -1,    -1,    12,    -1,   115,    -1,
     117,   118,   119,   120,    -1,   122,   123,   124,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    -1,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,     4,    -1,    51,    52,    53,    54,    55,
      56,    12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    -1,
      31,    32,    33,    34,    35,    36,    -1,    -1,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    89,    -1,
      91,    92,    -1,    -1,    -1,     4,    -1,    -1,    99,    -1,
      -1,    -1,   103,    12,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,   115,    24,   117,   118,    -1,   120,
      -1,   122,    31,    32,    33,    34,    35,    36,    -1,    -1,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,    -1,   103,     4,    -1,    -1,   107,    -1,
     109,    -1,    -1,    12,    -1,    -1,   115,    -1,   117,   118,
      -1,   120,    21,   122,    -1,    24,    -1,    26,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,     4,    38,
      39,    40,    41,    42,    43,    44,    12,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    24,    -1,
      26,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    -1,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,   107,    -1,
      -1,    -1,    78,    -1,    -1,    -1,    -1,    24,   117,   118,
      -1,    -1,   121,   122,    31,    32,    33,    34,    35,    36,
      -1,    -1,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      47,   107,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    -1,    91,    92,    -1,    -1,    -1,     4,
      -1,    -1,    99,    -1,    -1,    -1,   103,    12,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    24,
     117,   118,    -1,    -1,    -1,   122,    31,    32,    33,    34,
      35,    36,     4,    -1,    -1,    40,    41,    42,    43,    44,
      12,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    -1,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,
      -1,    24,   117,   118,    -1,    -1,    29,   122,    31,    32,
      33,    34,    35,    36,    -1,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    47,   107,     4,    -1,    51,    52,
      53,    54,    55,    56,    12,   117,    -1,    -1,    -1,   121,
      -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    -1,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,    -1,    -1,    -1,   121,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    91,    92,     4,    -1,    -1,    -1,    97,
      -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,    -1,   107,
      -1,    -1,    -1,    -1,    -1,    24,    -1,   115,    -1,   117,
      29,   119,    31,    32,    33,    34,    35,    36,     4,    38,
      39,    40,    41,    42,    43,    44,    12,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    -1,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,   117,   118,
      29,    -1,    31,    32,    33,    34,    35,    36,     4,    38,
      39,    40,    41,    42,    43,    44,    12,    -1,    47,    -1,
      -1,   107,    51,    52,    53,    54,    55,    56,    24,    -1,
      -1,   117,    -1,    -1,     4,    31,    32,    33,    34,    35,
      36,    -1,    12,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    24,    51,    52,    53,    54,    55,
      56,    31,    32,    33,    34,    35,    36,     4,    -1,    -1,
      40,    41,    42,    43,    44,    12,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    24,   117,    -1,
      -1,    -1,    -1,    -1,    31,    32,    33,    34,    35,    36,
      -1,    97,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      47,   107,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,   117,    -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117
  };

  const short
  parser::yystos_[] =
  {
       0,    21,    99,   140,   141,   142,   145,   118,   122,   319,
     146,   159,     0,   146,    65,    66,   143,   100,   113,   147,
     160,   161,     1,   102,   318,   103,   128,   190,   190,   107,
     148,    14,   162,   173,   174,   128,   191,    77,    77,     4,
      12,    21,    24,    26,    29,    31,    32,    33,    34,    35,
      36,    38,    39,    40,    41,    42,    43,    44,    47,    51,
      52,    53,    54,    55,    56,   107,   117,   118,   121,   122,
     149,   150,   151,   156,   157,   285,   288,   289,   303,   304,
     305,   311,    27,    60,   163,   113,   158,     3,     5,     6,
       7,     8,    10,    13,    16,    17,    18,    19,    20,    22,
      26,    28,    37,    50,    57,    62,    63,    76,    82,    88,
      90,    91,   103,   107,   109,   118,   122,   127,   128,   129,
     130,   137,   138,   171,   175,   176,   177,   178,   182,   230,
     235,   240,   241,   245,   246,   247,   248,   274,   275,   278,
     279,   302,   303,   305,   313,   314,   317,   104,   114,   319,
     107,   284,   288,    79,    88,    90,    91,    92,    97,   119,
     120,   123,   124,   308,   309,   310,   312,   108,   114,   107,
     152,    99,   101,   144,    77,    34,   164,   113,    63,   107,
     238,   239,   241,   242,   244,    34,    35,    36,    79,    88,
      90,    91,    92,    99,   103,   107,   109,   115,   117,   119,
     120,   123,   124,   184,   197,   198,   201,   203,   205,   207,
     208,   210,   283,   284,   286,   290,   291,   299,   300,   301,
     311,   107,    99,   101,   267,   238,    72,    73,    74,    75,
     179,    99,   101,   187,   188,   203,   205,   107,   115,   293,
     302,   305,   309,   267,   246,   103,   236,   237,   128,   107,
     303,   103,   107,   276,   277,   279,   314,    91,   246,   265,
     266,   246,   245,   246,    79,   104,   115,   120,   124,   238,
     239,   249,   251,   252,   281,   295,   296,   298,   307,   308,
     310,   315,   316,    90,   108,   114,   249,   250,   308,   309,
     310,   315,   320,   110,   320,    19,   236,   236,   129,   170,
     158,    71,   185,    80,    81,    84,   115,   232,   233,   234,
     281,   294,   296,   297,   306,   308,   309,    98,   246,    99,
     114,    87,   128,    88,   290,   108,   108,   108,   108,   108,
     108,   151,    78,   153,   154,   155,   156,   146,   146,    24,
     166,   117,   128,    23,    80,   294,   238,   195,   223,   224,
     225,   302,    29,   104,   199,   201,   203,    86,    88,   108,
     199,   214,   290,   320,   320,   288,   301,    27,   189,   210,
      89,    86,   208,   199,   213,   214,    20,    46,    91,   238,
     264,   268,   269,   270,   268,   113,   243,    77,    77,    77,
      77,   193,   199,   211,   186,   230,   231,   240,   186,    15,
      81,   309,   305,   129,   129,    88,   129,   303,    77,    77,
     128,   316,   114,    80,   192,   246,    86,   265,   232,     3,
     304,   313,   294,    78,    84,   114,   104,   114,   239,   108,
     114,   108,   114,   108,   108,   108,   114,   110,   211,   303,
     303,   115,   172,   280,   292,   293,   316,   128,   184,   194,
     199,   200,   238,   254,   255,   270,   304,   189,   234,   241,
     210,    78,   271,   272,   273,   303,   195,   246,   108,   108,
     108,   114,   100,   318,   128,   165,    77,    77,    99,   101,
     256,   193,   242,    80,   114,   100,   114,   215,   104,    89,
     108,    80,   108,   114,   108,   110,   115,   115,   188,   203,
     199,   108,   114,   188,   267,   246,    85,   100,   113,   318,
      25,   189,   100,   113,   318,   238,   200,   108,   115,   128,
     128,   129,   104,    77,    77,   108,   277,   103,   107,   109,
     282,   283,    77,   238,   115,   115,   115,   238,   253,   270,
     238,   249,   249,   249,   249,    77,    80,    80,   314,   114,
      77,   128,    80,    81,   183,   218,   189,    81,   114,   115,
     100,   114,    81,    80,   155,   319,    91,   100,   113,   238,
     257,   258,   259,   263,   257,   318,   199,   302,   225,    97,
     107,   216,   299,   199,   199,   217,   199,   199,   238,   269,
     238,   231,   128,   128,   104,   108,   110,   114,    78,   193,
     196,   196,   115,   292,    77,   217,    29,   219,   220,   221,
       9,   226,   227,   228,   238,   270,   272,   249,   194,   114,
       4,   167,   246,   257,   100,   113,    84,    86,   260,   261,
     262,   318,   199,   299,   108,   108,   243,   270,   238,    77,
     114,    77,   215,    84,   202,   206,   209,   210,   222,    22,
      51,    52,   107,   180,   229,   287,   288,   228,   319,    12,
     107,   168,   169,   259,   254,   238,   189,   262,    80,    11,
     193,    97,   220,    89,   115,   209,   280,   108,   203,   204,
     212,   229,    53,   181,   107,   149,    86,   189,   217,   238,
     206,   222,   206,   114,   108,   203,   149,   108,   238,   108,
     212,   108
  };

  const short
  parser::yyr1_[] =
  {
       0,   139,   140,   141,   141,   142,   143,   143,   143,   144,
     144,   145,   145,   146,   147,   147,   147,   148,   148,   149,
     150,   150,   151,   151,   152,   152,   153,   153,   154,   154,
     155,   155,   156,   156,   157,   157,   158,   158,   159,   159,
     160,   161,   161,   162,   163,   163,   164,   164,   165,   165,
     166,   166,   167,   167,   168,   168,   169,   169,   170,   170,
     171,   171,   171,   172,   172,   173,   174,   174,   175,   175,
     175,   175,   175,   175,   175,   175,   175,   175,   176,   177,
     177,   177,   178,   179,   179,   179,   179,   179,   180,   180,
     180,   181,   182,   182,   183,   183,   184,   184,   185,   185,
     185,   186,   186,   186,   186,   187,   187,   188,   189,   189,
     190,   190,   191,   191,   191,   192,   192,   193,   194,   195,
     195,   196,   196,   197,   198,   198,   199,   199,   199,   200,
     201,   202,   203,   203,   204,   205,   206,   206,   207,   207,
     208,   208,   208,   209,   210,   210,   210,   210,   210,   210,
     210,   210,   210,   210,   211,   212,   212,   213,   213,   214,
     214,   215,   215,   216,   216,   217,   218,   219,   219,   220,
     220,   221,   221,   222,   222,   223,   223,   224,   224,   225,
     226,   226,   227,   227,   228,   228,   228,   229,   229,   229,
     230,   230,   230,   231,   232,   232,   233,   233,   234,   235,
     235,   235,   235,   235,   235,   235,   235,   235,   235,   236,
     236,   237,   237,   238,   238,   239,   239,   240,   240,   241,
     241,   241,   242,   242,   243,   243,   244,   244,   245,   245,
     245,   245,   246,   246,   246,   246,   246,   246,   246,   246,
     246,   247,   247,   248,   248,   248,   248,   248,   248,   248,
     249,   249,   249,   250,   250,   251,   251,   251,   251,   251,
     251,   251,   252,   252,   253,   253,   254,   255,   255,   256,
     256,   256,   256,   257,   257,   258,   258,   258,   259,   260,
     260,   261,   261,   262,   263,   263,   264,   264,   265,   265,
     266,   266,   267,   267,   268,   268,   268,   268,   269,   269,
     270,   270,   270,   271,   271,   272,   272,   272,   273,   273,
     274,   274,   275,   275,   276,   276,   276,   277,   277,   278,
     278,   278,   278,   279,   279,   280,   280,   281,   281,   282,
     282,   282,   283,   283,   283,   283,   283,   284,   284,   284,
     285,   285,   285,   285,   285,   286,   286,   287,   288,   288,
     289,   290,   290,   290,   291,   291,   291,   291,   292,   292,
     293,   293,   294,   294,   294,   295,   295,   295,   296,   297,
     297,   298,   298,   299,   300,   301,   301,   301,   301,   301,
     302,   302,   303,   303,   303,   304,   304,   305,   305,   305,
     305,   305,   305,   305,   305,   306,   306,   307,   307,   308,
     309,   309,   310,   310,   311,   311,   311,   311,   311,   311,
     311,   311,   311,   311,   311,   311,   311,   311,   311,   311,
     311,   311,   312,   312,   312,   313,   313,   314,   315,   315,
     316,   316,   317,   317,   317,   317,   318,   318,   319,   319,
     320,   320
  };

  const signed char
  parser::yyr2_[] =
  {
       0,     2,     1,     6,     1,     0,     3,     3,     0,     3,
       3,     3,     3,     2,     2,     2,     1,     3,     0,     1,
       3,     1,     2,     2,     0,     3,     0,     1,     4,     1,
       1,     1,     1,     2,     1,     1,     2,     1,     2,     0,
       2,     3,     0,     8,     2,     0,     1,     0,     1,     0,
       1,     0,     2,     0,     1,     0,     3,     4,     0,     1,
       1,     1,     1,     3,     1,     2,     3,     0,     1,     1,
       1,     4,     1,     1,     5,     4,     5,     4,     3,     4,
       5,     4,     4,     2,     2,     2,     2,     0,     1,     1,
       1,     2,     1,     1,     0,     2,     3,     1,     4,     3,
       0,     3,     2,     1,     0,     3,     3,     1,     2,     0,
       1,     3,     3,     1,     0,     0,     2,     1,     1,     3,
       1,     1,     3,     1,     1,     1,     4,     3,     1,     1,
       1,     1,     1,     3,     1,     1,     1,     2,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     2,     3,     2,
       5,     3,     3,     5,     1,     1,     3,     1,     0,     1,
       3,     2,     0,     1,     5,     1,     2,     3,     1,     4,
       2,     3,     0,     1,     3,     0,     1,     3,     1,     3,
       0,     1,     2,     1,     2,     3,     3,     1,     2,     3,
       1,     3,     2,     1,     3,     2,     2,     1,     4,     3,
       5,     3,     4,     4,     3,     4,     6,     6,     4,     0,
       1,     3,     4,     3,     1,     1,     3,     1,     3,     2,
       3,     1,     1,     2,     1,     0,     3,     3,     2,     3,
       2,     1,     3,     2,     4,     4,     8,     4,     2,     2,
       1,     4,     1,     1,     1,     1,     3,     3,     3,     1,
       1,     2,     2,     3,     3,     1,     1,     2,     4,     3,
       5,     3,     3,     3,     3,     1,     1,     3,     1,     3,
       3,     2,     2,     1,     2,     3,     2,     1,     2,     3,
       2,     2,     1,     4,     1,     2,     1,     2,     1,     2,
       2,     1,     3,     3,     3,     2,     1,     0,     1,     2,
       3,     1,     2,     1,     0,     3,     1,     1,     3,     1,
       1,     1,     1,     3,     1,     3,     1,     1,     3,     2,
       3,     2,     3,     1,     2,     1,     3,     1,     3,     1,
       2,     2,     1,     3,     3,     3,     2,     1,     3,     3,
       1,     3,     3,     3,     3,     1,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     1,     1,     1,     1,     1,     1,     3,     1,
       3,     1,     3,     1,     3,     1,     1,     1,     1,     1,
       1,     3,     1,     3,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     1
  };


#if YYDEBUG || 1
  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a YYNTOKENS, nonterminals.
  const char*
  const parser::yytname_[] =
  {
  "\"end of file\"", "error", "\"invalid token\"", "\"_\"", "\"as\"",
  "\"case\"", "\"class\"", "\"data\"", "\"default\"", "\"deriving\"",
  "\"do\"", "\"else\"", "\"hiding\"", "\"if\"", "\"import\"", "\"in\"",
  "\"infix\"", "\"infixl\"", "\"infixr\"", "\"instance\"", "\"let\"",
  "\"module\"", "\"newtype\"", "\"of\"", "\"qualified\"", "\"then\"",
  "\"type\"", "\"where\"", "\"builtin\"", "\"forall\"", "\"foreign\"",
  "\"export\"", "\"label\"", "\"dynamic\"", "\"safe\"",
  "\"interruptible\"", "\"unsafe\"", "\"mdo\"", "\"family\"", "\"role\"",
  "\"stdcall\"", "\"ccall\"", "\"capi\"", "\"prim\"", "\"javascript\"",
  "\"proc\"", "\"rec\"", "\"group\"", "\"by\"", "\"using\"", "\"static\"",
  "\"stock\"", "\"anyclass\"", "\"via\"", "\"unit\"", "\"signature\"",
  "\"dependency\"", "\"{-# INLINE\"", "\"{-# SPECIALIZE\"",
  "\"{-# SPECIALIZE_INLINE\"", "\"{-# SOURCE\"", "\"{-# RULES\"",
  "\"{-# CORE\"", "\"{-# SCC\"", "\"{-# GENERATED\"", "\"{-# DEPRECATED\"",
  "\"{-# WARNING\"", "\"{-# UNPACK\"", "\"{-# NOUNPACK\"", "\"{-# ANN\"",
  "\"{-# MINIMAL\"", "\"{-# CTYPE\"", "\"{-# OVERLAPPING\"",
  "\"{-# OVERLAPPABLE\"", "\"{-# OVERLAPS\"", "\"{-# INCOHERENT\"",
  "\"{-# COMPLETE\"", "\"#-}\"", "\"..\"", "\":\"", "\"::\"", "\"=\"",
  "\"\\\\\"", "\"lcase\"", "\"|\"", "\"<-\"", "\"->\"", "\"@\"", "\"~\"",
  "\"=>\"", "\"-\"", "\"!\"", "\"*\"", "\"-<\"", "\">-\"", "\"-<<\"",
  "\">>-\"", "\".\"", "\"TYPEAPP\"", "\"{\"", "\"}\"", "\"vocurly\"",
  "\"vccurly\"", "\"[\"", "\"]\"", "\"[:\"", "\":]\"", "\"(\"", "\")\"",
  "\"(#\"", "\"#)\"", "\"(|\"", "\"|)\"", "\";\"", "\",\"", "\"`\"",
  "\"'\"", "\"VARID\"", "\"CONID\"", "\"VARSYM\"", "\"CONSYM\"",
  "\"QVARID\"", "\"QCONID\"", "\"QVARSYM\"", "\"QCONSYM\"",
  "\"IPDUPVARID\"", "\"LABELVARID\"", "\"CHAR\"", "\"STRING\"",
  "\"INTEGER\"", "\"RATIONAL\"", "\"PRIMCHAR\"", "\"PRIMSTRING\"",
  "\"PRIMINTEGER\"", "\"PRIMWORD\"", "\"PRIMFLOAT\"", "\"PRIMDOUBLE\"",
  "\"{-# SPECIALISE\"", "\"{-# SPECIALISE_INLINE\"", "$accept", "unit",
  "module", "missing_module_keyword", "maybemodwarning", "body", "body2",
  "top", "top1", "maybeexports", "exportlist", "exportlist1", "export",
  "export_subspec", "qcnames", "qcnames1", "qcname_ext_w_wildcard",
  "qcname_ext", "qcname", "semis1", "semis", "importdecls",
  "importdecls_semi", "importdecl", "maybe_src", "maybe_safe", "maybe_pkg",
  "optqualified", "maybeas", "maybeimpspec", "impspec", "prec", "infix",
  "ops", "topdecls", "topdecls_semi", "topdecl", "cl_decl", "ty_decl",
  "inst_decl", "overlap_pragma", "deriv_strategy_no_via",
  "deriv_strategy_via", "data_or_newtype", "opt_kind_sig", "tycl_hdr",
  "capi_ctype", "decls", "decllist", "binds", "wherebinds", "strings",
  "stringlist", "opt_tyconsig", "sigtype", "sigtypedoc", "sig_vars",
  "sigtypes1", "strict_mark", "strictness", "ctype", "ctypedoc", "context",
  "context_no_ops", "type", "typedoc", "btype", "btype_no_ops", "tyapps",
  "tyapp", "atype_docs", "atype", "inst_type", "deriv_types",
  "comma_types0", "comma_types1", "tv_bndrs", "tv_bndr", "kind", "constrs",
  "constrs1", "constr", "forall", "constr_stuff", "fielddecls",
  "fielddecls1", "fielddecl", "maybe_derivings", "derivings", "deriving",
  "deriv_clause_types", "decl_no_th", "decl", "rhs", "gdrhs", "gdrh",
  "sigdecl", "activation", "explicit_activation", "exp", "infixexp",
  "infixexp_top", "exp10_top", "exp10", "optSemi", "scc_annot", "fexp",
  "aexp", "aexp1", "aexp2", "texp", "tup_exprs", "list", "lexps", "squals",
  "guardquals", "guardquals1", "altslist", "alts", "alts1", "alt",
  "alt_rhs", "gdpats", "gdpat", "pat", "bindpat", "apat", "apats1",
  "stmtlist", "stmts", "stmt", "qual", "fbinds", "fbinds1", "fbind",
  "qcon", "gen_qcon", "con", "con_list", "sysdcon_no_list", "sysdcon",
  "conop", "qconop", "gtycon", "ntgtycon", "oqtycon", "oqtycon_no_varcon",
  "qtyconop", "qtycondoc", "qtycon", "tycon", "qtyconsym", "tyconsym",
  "op", "varop", "qop", "qopm", "hole_op", "qvarop", "qvaropm", "tyvar",
  "tyvarop", "tyvarid", "var", "qvar", "qvarid", "varid", "qvarsym",
  "qvarsym_no_minus", "qvarsym1", "varsym", "varsym_no_minus",
  "special_id", "special_sym", "qconid", "conid", "qconsym", "consym",
  "literal", "close", "modid", "commas", YY_NULLPTR
  };
#endif


#if YYDEBUG
  const short
  parser::yyrline_[] =
  {
       0,   522,   522,   539,   540,   542,   546,   547,   548,   550,
     551,   553,   554,   557,   559,   560,   561,   569,   570,   572,
     574,   575,   577,   578,   581,   582,   584,   585,   587,   588,
     590,   591,   593,   594,   596,   597,   601,   602,   604,   605,
     607,   609,   610,   612,   621,   622,   624,   625,   627,   628,
     630,   631,   633,   634,   636,   637,   639,   640,   645,   646,
     648,   649,   650,   652,   653,   657,   659,   660,   662,   663,
     664,   667,   674,   676,   677,   678,   679,   680,   682,   684,
     685,   686,   691,   696,   697,   698,   699,   700,   702,   703,
     704,   706,   746,   747,   749,   750,   759,   760,   762,   763,
     764,   808,   809,   810,   811,   813,   814,   816,   818,   819,
     827,   828,   830,   831,   832,   845,   846,   848,   850,   852,
     853,   855,   856,   860,   866,   867,   874,   875,   877,   879,
     888,   890,   892,   893,   895,   898,   900,   901,   903,   904,
     906,   907,   908,   914,   921,   922,   923,   924,   925,   926,
     927,   933,   934,   935,   938,   940,   941,   943,   944,   946,
     947,   954,   955,   957,   958,   976,   982,   984,   985,   987,
     988,   990,   991,   993,   994,   996,   997,   999,  1000,  1002,
    1004,  1005,  1007,  1008,  1010,  1011,  1012,  1014,  1015,  1016,
    1021,  1023,  1029,  1034,  1038,  1039,  1041,  1042,  1046,  1048,
    1049,  1050,  1052,  1053,  1054,  1055,  1056,  1057,  1058,  1061,
    1062,  1064,  1065,  1069,  1070,  1072,  1073,  1075,  1076,  1078,
    1079,  1080,  1082,  1083,  1086,  1087,  1089,  1090,  1094,  1095,
    1096,  1097,  1099,  1100,  1101,  1102,  1104,  1106,  1107,  1108,
    1110,  1112,  1113,  1115,  1116,  1117,  1118,  1119,  1124,  1125,
    1130,  1131,  1132,  1137,  1138,  1156,  1157,  1158,  1159,  1160,
    1161,  1162,  1164,  1165,  1178,  1180,  1190,  1192,  1193,  1196,
    1197,  1198,  1199,  1201,  1202,  1204,  1205,  1206,  1208,  1210,
    1211,  1213,  1214,  1223,  1225,  1226,  1228,  1229,  1231,  1232,
    1234,  1235,  1238,  1239,  1241,  1242,  1243,  1244,  1249,  1250,
    1252,  1253,  1254,  1259,  1260,  1262,  1263,  1264,  1266,  1267,
    1299,  1300,  1302,  1303,  1305,  1306,  1307,  1309,  1310,  1312,
    1313,  1314,  1315,  1317,  1318,  1320,  1321,  1323,  1324,  1327,
    1328,  1329,  1331,  1332,  1333,  1334,  1335,  1337,  1338,  1339,
    1341,  1342,  1343,  1344,  1345,  1348,  1349,  1351,  1353,  1354,
    1358,  1360,  1361,  1362,  1364,  1365,  1366,  1367,  1372,  1373,
    1375,  1376,  1378,  1379,  1380,  1382,  1383,  1384,  1386,  1388,
    1389,  1391,  1392,  1396,  1398,  1400,  1401,  1402,  1403,  1404,
    1407,  1408,  1410,  1411,  1412,  1414,  1415,  1417,  1418,  1419,
    1420,  1421,  1422,  1423,  1424,  1426,  1427,  1429,  1430,  1432,
    1434,  1435,  1437,  1438,  1440,  1441,  1442,  1443,  1444,  1445,
    1446,  1447,  1448,  1449,  1450,  1451,  1452,  1453,  1454,  1455,
    1456,  1457,  1459,  1460,  1461,  1465,  1466,  1468,  1470,  1471,
    1473,  1474,  1478,  1479,  1480,  1481,  1486,  1489,  1493,  1494,
    1496,  1497
  };

  void
  parser::yy_stack_print_ () const
  {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator
           i = yystack_.begin (),
           i_end = yystack_.end ();
         i != i_end; ++i)
      *yycdebug_ << ' ' << int (i->state);
    *yycdebug_ << '\n';
  }

  void
  parser::yy_reduce_print_ (int yyrule) const
  {
    int yylno = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    // Print the symbols being reduced, and their result.
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1
               << " (line " << yylno << "):\n";
    // The symbols being reduced.
    for (int yyi = 0; yyi < yynrhs; yyi++)
      YY_SYMBOL_PRINT ("   $" << yyi + 1 << " =",
                       yystack_[(yynrhs) - (yyi + 1)]);
  }
#endif // YYDEBUG


} // yy
#line 5872 "parser.cc"

#line 1506 "parser.y"


using std::optional;
using std::string;
using std::vector;
using std::pair;

void
yy::parser::error (const location_type& l, const std::string& m)
{
    drv.push_error_message({l,m});
}

expression_ref make_module(const string& name, const expression_ref& exports, const expression_ref& body)
{
    vector<expression_ref> e = {String(name)};
    if (exports)
	e.push_back(exports);
    e.push_back(body);
    return new expression(AST_node("Module"),e);
}

expression_ref make_body(const std::vector<expression_ref>& imports, const std::vector<expression_ref>& topdecls)
{
    expression_ref i = new expression(AST_node("impdecls"),imports);
    expression_ref t = new expression(AST_node("TopDecls"),topdecls);
    return new expression(AST_node("Body"),{i,t});
}

expression_ref make_exports(const vector<expression_ref>& exports)
{
    return new expression(AST_node("Exports"),exports);
}

expression_ref make_builtin_expr(const string& name, int args, const string& s1, const string& s2)
{
    return new expression(AST_node("Builtin"),{String(name), args, String(s1), String(s2)});
}

expression_ref make_builtin_expr(const string& name, int args, const string& s1)
{
    return new expression(AST_node("Builtin"),{String(name), args, String(s1)});
}

vector<expression_ref> make_String_vec(const vector<string>& strings)
{
    vector<expression_ref> Strings;
    for(auto& string: strings)
        Strings.push_back(String(string));
    return Strings;
}

// See PostProcess.hs:checkTyClHdr
std::tuple<string, vector<expression_ref>>
check_type_or_class_header(expression_ref type)
{
    auto [type_head, type_args] = Haskell::decompose_type_apps(type);

    // FIXME -- add location!
    if (not type_head.is_a<Haskell::TypeVar>())
        throw myexception()<<"Malformed type or class header '"<<type<<"'";
    auto name = type_head.as_<Haskell::TypeVar>().name;

    return {name, type_args};
}

Haskell::FieldDecl make_field_decl(const std::vector<std::string>& field_names, const Haskell::Type& type)
{
    return {field_names, type};
}

Haskell::TypeSynonymDecl make_type_synonym(const Located<expression_ref>& lhs_type, const Located<expression_ref>& rhs_type)
{
    auto [name, type_args] = check_type_or_class_header(unloc(lhs_type));
    return {name, type_args, rhs_type};
}

Haskell::DataOrNewtypeDecl make_data_or_newtype(const Haskell::DataOrNewtype& d_or_n, const Haskell::Context&  context,
                                                const expression_ref& header, const vector<Haskell::Constructor>& constrs)
{
    auto [name, type_args] = check_type_or_class_header(header);
    if (d_or_n == Haskell::DataOrNewtype::newtype and constrs.size() != 1)
        throw myexception()<<"newtype '"<<name<<"' may only have 1 constructors with 1 field";
    return {d_or_n, name, type_args, context, constrs};
}

Haskell::InstanceDecl make_instance_decl(const Located<expression_ref>& type, const optional<Located<Haskell::Decls>>& decls)
{
    return {type, decls};
}

Haskell::ClassDecl make_class_decl(const Haskell::Context& context, const expression_ref& header, const optional<Located<Haskell::Decls>>& decls)
{
    auto [name, type_args] = check_type_or_class_header(header);
    return {name,type_args,context,decls};
}

// Can we change the context parsing rule to expect:
// nothing
// | ctype => header
// | ( ctypes2 ) => header
Haskell::Context make_context(const expression_ref& context)
{
    vector<Haskell::Type> constraints;
    if (context.is_a<Haskell::TupleType>())
    {
        constraints = context.as_<Haskell::TupleType>().element_types;
    }
    else
        constraints.push_back(context);

    return {constraints};
}

expression_ref make_tv_bndrs(const vector<expression_ref>& tv_bndrs)
{
    return new expression(AST_node("tv_bndrs"),tv_bndrs);
}

expression_ref make_tyapps(const std::vector<expression_ref>& tyapps)
{
    assert(not tyapps.empty());
    expression_ref E = tyapps[0];
    for(int i=1;i<tyapps.size();i++)
	E = make_type_app(E,tyapps[i]);
    return E;
}

Haskell::Var make_var(const Located<string>& v)
{
    return {v};
}

bool check_kind(const Haskell::Type& kind)
{
    auto [kind_head, kind_args] = Haskell::decompose_type_apps(kind);

    if (not kind_head.is_a<Haskell::TypeVar>()) return false;

    auto V = kind_head.as_<Haskell::TypeVar>();
    if (kind_args.empty())
    {
        return (V.name == "*");
    }
    else if (kind_args.size() == 2)
    {
        return (V.name == "->") and check_kind(kind_args[0]) and check_kind(kind_args[1]);
    }
    else
        return false;
}

Haskell::Type make_kind(const Haskell::Type& kind)
{
    if (not check_kind(kind))
        throw myexception()<<"Kind '"<<kind<<"' is malformed";

    return kind;
}

Haskell::TypeVar make_type_var(const string& id)
{
    return Haskell::TypeVar(id);
}

Haskell::TypeVarOfKind make_type_var_of_kind(const string& id, const Haskell::Type& kind)
{
    return {id, kind};
}

Haskell::TypeOfKind make_type_of_kind(const Haskell::Type& type, const Haskell::Type& kind)
{
    return {type, kind};
}

Haskell::TupleType make_tuple_type(const std::vector<Haskell::Type>& types)
{
    return {types};
}

Haskell::ListType make_list_type(const Haskell::Type& type)
{
    return {type};
}

Haskell::TypeApp make_type_app(const Haskell::Type& head, const Haskell::Type& arg)
{
    return {head, arg};
}

optional<pair<string, Haskell::FieldDecls>> is_record_con(const expression_ref& typeish)
{
    auto [head,args] = Haskell::decompose_type_apps(typeish);

    if (args.size() != 1) return {};

    if (not head.is_a<Haskell::TypeVar>()) return {};

    if (not args[0].is_a<Haskell::FieldDecls>()) return {};

    return {{head.as_<Haskell::TypeVar>().name, args[0].as_<Haskell::FieldDecls>()}};
}

optional<pair<string, std::vector<expression_ref>>> is_normal_con(const expression_ref& typeish)
{
    if (is_record_con(typeish)) return {};

    auto [head,args] = Haskell::decompose_type_apps(typeish);

    if (not head.is_a<Haskell::TypeVar>())
        return {};

    return {{head.as_<Haskell::TypeVar>().name, args}};
}

Haskell::Constructor make_constructor(const expression_ref& forall, const std::optional<Haskell::Context>& c, const expression_ref& typeish)
{
    if (auto constr = is_record_con(typeish))
    {
        auto [name, fields] = *constr;
        return {forall, c, name, fields};
    }
    else if (auto constr = is_normal_con(typeish))
    {
        auto [name, fields] = *constr;
        return {forall, c, name, fields};
    }
    else
        throw myexception()<<"constructor '"<<typeish<<"' does not make sense";
}

Haskell::FieldDecls make_field_decls(const std::vector<Haskell::FieldDecl>& ds)
{
    return {ds};
}

Haskell::StrictLazyType make_strict_lazy_type(const Haskell::StrictLazy& sl, const Haskell::Type& t)
{
    return {sl, t};
}

expression_ref make_forall_type(const std::vector<expression_ref>& tv_bndrs, const Haskell::Type& t)
{
    if (tv_bndrs.empty())
        return t;
    else
        return Haskell::ForallType(tv_bndrs, t);
}

expression_ref make_constrained_type(const Haskell::Context& context, const Haskell::Type& t)
{
    if (context.constraints.empty())
        return t;
    else
        return Haskell::ConstrainedType(context, t);
}

expression_ref make_typed_exp(const expression_ref& exp, const expression_ref& type)
{
    return new expression(AST_node("typed_exp"),{exp,type});
}

Haskell::SimpleRHS make_rhs(const Located<expression_ref>& exp, const optional<Located<Haskell::Decls>>& wherebinds)
{
    return {exp, wherebinds};
}

expression_ref make_infixexp(const vector<expression_ref>& args)
{
    if (args.size() == 1)
	return args[0];
    else
	return new expression(AST_node("infixexp"),args);
}


expression_ref make_minus(const expression_ref& exp)
{
    return new expression(AST_node("infixexp"),{AST_node("neg"),exp});
}

expression_ref make_fexp(const vector<expression_ref>& args)
{
    if (args.size() == 1)
	return args[0];
    else {
	expression_ref f = args[0];
	for(int i=1;i<args.size();i++)
	    f = {f,args[i]};
	return f;
    }
}

Haskell::AsPattern make_as_pattern(const Haskell::Var& x, const expression_ref& pat)
{
    return Haskell::AsPattern(x,pat);
}

Haskell::LazyPattern make_lazy_pattern(const expression_ref& pat)
{
    return { pat };
}

Haskell::StrictPattern make_strict_pattern(const expression_ref& pat)
{
    return { pat };
}

Located<Haskell::Decls> make_decls(const yy::location& loc, std::vector<expression_ref>& decls)
{
    return {loc, Haskell::Decls(decls)};
}

Haskell::ValueDecl make_value_decl(const expression_ref& lhs,  const expression_ref& rhs)
{
    return {lhs, rhs};
}

Haskell::LambdaExp make_lambdaexp(const vector<expression_ref>& pats, const expression_ref& body)
{
    return { pats, body };
}

Haskell::LetExp make_let(const Located<Haskell::Decls>& binds, const Located<expression_ref>& body)
{
    return { binds, body };
}

Haskell::IfExp make_if(const Located<expression_ref>& cond, const Located<expression_ref>& alt_true, const Located<expression_ref>& alt_false)
{
    return {cond, alt_true, alt_false};
}

Haskell::CaseExp make_case(const expression_ref& obj, const Haskell::Alts& alts)
{
    return {obj, alts};
}

Haskell::Do make_do(const Haskell::Stmts& stmts)
{
    return {stmts};
}

Haskell::MDo make_mdo(const Haskell::Stmts& stmts)
{
    return {stmts};
}

Haskell::Tuple yy_make_tuple(const vector<expression_ref>& elements)
{
    return Haskell::Tuple(elements);
}

Haskell::List make_list(const vector<expression_ref>& elements)
{
    return Haskell::List(elements);
}

Haskell::Alts make_alts(const vector<Located<Haskell::Alt>>& alts)
{
    return {alts};
}

Located<Haskell::Alt> yy_make_alt(const yy::location& loc, const expression_ref& pat, const expression_ref& alt_rhs)
{
    return {loc, {pat, alt_rhs}};
}

Haskell::MultiGuardedRHS make_gdrhs(const vector<Haskell::GuardedRHS>& guards, const optional<Located<Haskell::Decls>>& wherebinds)
{
    return {guards, wherebinds};
}

Haskell::GuardedRHS make_gdrh(const vector<expression_ref>& guardquals, const expression_ref& exp)
{
    return {guardquals, exp};
}

Haskell::Stmts make_stmts(const vector<expression_ref>& stmts)
{
    return {stmts};
}

Haskell::FixityDecl make_fixity_decl(const Haskell::Fixity& fixity, optional<int>& prec, vector<string>& op_names)
{
    return {fixity, prec, op_names};
}

expression_ref yy_make_string(const std::string& s)
{
    vector<expression_ref> chars;
    for(char c: s)
	chars.push_back(c);
    return make_list(chars);
}

