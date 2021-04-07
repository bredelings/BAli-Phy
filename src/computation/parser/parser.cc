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
#line 94 "parser.y"

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
      case symbol_kind::S_alt: // alt
        value.YY_MOVE_OR_COPY< Haskell::Alt > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_altslist: // altslist
        value.YY_MOVE_OR_COPY< Haskell::Alts > (YY_MOVE (that.value));
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
      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
      case symbol_kind::S_wherebinds: // wherebinds
      case symbol_kind::S_opt_sig: // opt_sig
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
      case symbol_kind::S_constr: // constr
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

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.YY_MOVE_OR_COPY< std::vector<Haskell::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.YY_MOVE_OR_COPY< std::vector<Haskell::FieldDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.YY_MOVE_OR_COPY< std::vector<Haskell::GuardedRHS> > (YY_MOVE (that.value));
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
      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
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
      case symbol_kind::S_alt: // alt
        value.move< Haskell::Alt > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_altslist: // altslist
        value.move< Haskell::Alts > (YY_MOVE (that.value));
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
      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
      case symbol_kind::S_wherebinds: // wherebinds
      case symbol_kind::S_opt_sig: // opt_sig
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
      case symbol_kind::S_constr: // constr
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

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Haskell::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.move< std::vector<Haskell::FieldDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.move< std::vector<Haskell::GuardedRHS> > (YY_MOVE (that.value));
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
      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
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
      case symbol_kind::S_alt: // alt
        value.copy< Haskell::Alt > (that.value);
        break;

      case symbol_kind::S_altslist: // altslist
        value.copy< Haskell::Alts > (that.value);
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
      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
      case symbol_kind::S_wherebinds: // wherebinds
      case symbol_kind::S_opt_sig: // opt_sig
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
      case symbol_kind::S_constr: // constr
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

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.copy< std::vector<Haskell::Alt> > (that.value);
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.copy< std::vector<Haskell::FieldDecl> > (that.value);
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.copy< std::vector<Haskell::GuardedRHS> > (that.value);
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
      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
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
      case symbol_kind::S_alt: // alt
        value.move< Haskell::Alt > (that.value);
        break;

      case symbol_kind::S_altslist: // altslist
        value.move< Haskell::Alts > (that.value);
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
      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
      case symbol_kind::S_wherebinds: // wherebinds
      case symbol_kind::S_opt_sig: // opt_sig
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
      case symbol_kind::S_constr: // constr
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

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Haskell::Alt> > (that.value);
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.move< std::vector<Haskell::FieldDecl> > (that.value);
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.move< std::vector<Haskell::GuardedRHS> > (that.value);
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
      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
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
      case symbol_kind::S_alt: // alt
        yylhs.value.emplace< Haskell::Alt > ();
        break;

      case symbol_kind::S_altslist: // altslist
        yylhs.value.emplace< Haskell::Alts > ();
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
      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
      case symbol_kind::S_wherebinds: // wherebinds
      case symbol_kind::S_opt_sig: // opt_sig
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
      case symbol_kind::S_constr: // constr
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

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        yylhs.value.emplace< std::vector<Haskell::Alt> > ();
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        yylhs.value.emplace< std::vector<Haskell::FieldDecl> > ();
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        yylhs.value.emplace< std::vector<Haskell::GuardedRHS> > ();
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
      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
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
#line 519 "parser.y"
             {drv.result = yystack_[0].value.as < expression_ref > ();}
#line 1771 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 536 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = make_module(yystack_[4].value.as < std::string > (),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 1777 "parser.cc"
    break;

  case 4: // module: body2
#line 537 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = make_module("Main",{},yystack_[0].value.as < expression_ref > ());}
#line 1783 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 539 "parser.y"
                                                                 {drv.push_module_context();}
#line 1789 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 547 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1795 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 548 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1801 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 550 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1807 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 551 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1813 "parser.cc"
    break;

  case 13: // top: semis top1
#line 554 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1819 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 556 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 1825 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 557 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 1831 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 558 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[0].value.as < std::vector<expression_ref> > (),{});}
#line 1837 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 566 "parser.y"
                                      {yylhs.value.as < expression_ref > () = make_exports(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 1843 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 567 "parser.y"
                                      {}
#line 1849 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 569 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 1855 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 571 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1861 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 572 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1867 "parser.cc"
    break;

  case 22: // export: qcname_ext export_subspec
#line 574 "parser.y"
                                      {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1873 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 575 "parser.y"
                                      {yylhs.value.as < expression_ref > () = AST_node("module",yystack_[0].value.as < std::string > ());}
#line 1879 "parser.cc"
    break;

  case 26: // qcnames: %empty
#line 581 "parser.y"
                   {}
#line 1885 "parser.cc"
    break;

  case 27: // qcnames: qcnames1
#line 582 "parser.y"
                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 1891 "parser.cc"
    break;

  case 28: // qcnames1: qcnames1 "," qcname_ext_w_wildcard ","
#line 584 "parser.y"
                                                  {yylhs.value.as < std::vector<expression_ref> > () = yystack_[3].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ());}
#line 1897 "parser.cc"
    break;

  case 29: // qcnames1: qcname_ext_w_wildcard
#line 585 "parser.y"
                                              {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1903 "parser.cc"
    break;

  case 30: // qcname_ext_w_wildcard: qcname_ext
#line 587 "parser.y"
                                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1909 "parser.cc"
    break;

  case 31: // qcname_ext_w_wildcard: ".."
#line 588 "parser.y"
                                     {}
#line 1915 "parser.cc"
    break;

  case 32: // qcname_ext: qcname
#line 590 "parser.y"
                                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1921 "parser.cc"
    break;

  case 33: // qcname_ext: "type" oqtycon
#line 591 "parser.y"
                                     {}
#line 1927 "parser.cc"
    break;

  case 34: // qcname: qvar
#line 593 "parser.y"
                                     {yylhs.value.as < expression_ref > () = AST_node("qvar",yystack_[0].value.as < std::string > ()); }
#line 1933 "parser.cc"
    break;

  case 35: // qcname: oqtycon_no_varcon
#line 594 "parser.y"
                                     {yylhs.value.as < expression_ref > () = AST_node("qvar",yystack_[0].value.as < std::string > ()); }
#line 1939 "parser.cc"
    break;

  case 40: // importdecls: importdecls_semi importdecl
#line 604 "parser.y"
                                         { yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (), yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 1945 "parser.cc"
    break;

  case 41: // importdecls_semi: importdecls_semi importdecl semis1
#line 606 "parser.y"
                                                     { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 1951 "parser.cc"
    break;

  case 42: // importdecls_semi: %empty
#line 607 "parser.y"
                         { }
#line 1957 "parser.cc"
    break;

  case 43: // importdecl: "import" maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec
#line 609 "parser.y"
                                                                                            {
    std::vector<expression_ref> e;
    if (yystack_[4].value.as < bool > ()) e.push_back(AST_node("qualified"));
    e.push_back(String(yystack_[2].value.as < std::string > ()));
    if (yystack_[1].value.as < std::optional<std::string> > ()) e.push_back(AST_node("as", *yystack_[1].value.as < std::optional<std::string> > ()));
    if (yystack_[0].value.as < expression_ref > ()) e.push_back(yystack_[0].value.as < expression_ref > ());
    yylhs.value.as < expression_ref > () = expression_ref(new expression(AST_node("ImpDecl"),std::move(e)));
}
#line 1970 "parser.cc"
    break;

  case 44: // maybe_src: "{-# SOURCE" "#-}"
#line 618 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 1976 "parser.cc"
    break;

  case 45: // maybe_src: %empty
#line 619 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 1982 "parser.cc"
    break;

  case 46: // maybe_safe: "safe"
#line 621 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 1988 "parser.cc"
    break;

  case 47: // maybe_safe: %empty
#line 622 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 1994 "parser.cc"
    break;

  case 48: // maybe_pkg: "STRING"
#line 624 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 2000 "parser.cc"
    break;

  case 49: // maybe_pkg: %empty
#line 625 "parser.y"
                               { }
#line 2006 "parser.cc"
    break;

  case 50: // optqualified: "qualified"
#line 627 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2012 "parser.cc"
    break;

  case 51: // optqualified: %empty
#line 628 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2018 "parser.cc"
    break;

  case 52: // maybeas: "as" modid
#line 630 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 2024 "parser.cc"
    break;

  case 53: // maybeas: %empty
#line 631 "parser.y"
                               { }
#line 2030 "parser.cc"
    break;

  case 54: // maybeimpspec: impspec
#line 633 "parser.y"
                               { yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > (); }
#line 2036 "parser.cc"
    break;

  case 55: // maybeimpspec: %empty
#line 634 "parser.y"
                               { }
#line 2042 "parser.cc"
    break;

  case 56: // impspec: "(" exportlist ")"
#line 636 "parser.y"
                                      { yylhs.value.as < expression_ref > () = expression_ref{AST_node("only"),yystack_[1].value.as < std::vector<expression_ref> > ()}; }
#line 2048 "parser.cc"
    break;

  case 57: // impspec: "hiding" "(" exportlist ")"
#line 637 "parser.y"
                                      { yylhs.value.as < expression_ref > () = expression_ref{AST_node("hiding"),yystack_[1].value.as < std::vector<expression_ref> > ()}; }
#line 2054 "parser.cc"
    break;

  case 58: // prec: %empty
#line 642 "parser.y"
                   { }
#line 2060 "parser.cc"
    break;

  case 59: // prec: "INTEGER"
#line 643 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < int > (); }
#line 2066 "parser.cc"
    break;

  case 60: // infix: "infix"
#line 645 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infix; }
#line 2072 "parser.cc"
    break;

  case 61: // infix: "infixl"
#line 646 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infixl; }
#line 2078 "parser.cc"
    break;

  case 62: // infix: "infixr"
#line 647 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infixr; }
#line 2084 "parser.cc"
    break;

  case 63: // ops: ops "," op
#line 649 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 2090 "parser.cc"
    break;

  case 64: // ops: op
#line 650 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 2096 "parser.cc"
    break;

  case 65: // topdecls: topdecls_semi topdecl
#line 654 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 2102 "parser.cc"
    break;

  case 66: // topdecls_semi: topdecls_semi topdecl semis1
#line 656 "parser.y"
                                            { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2108 "parser.cc"
    break;

  case 67: // topdecls_semi: %empty
#line 657 "parser.y"
                                            { }
#line 2114 "parser.cc"
    break;

  case 68: // topdecl: cl_decl
#line 659 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2120 "parser.cc"
    break;

  case 69: // topdecl: ty_decl
#line 660 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2126 "parser.cc"
    break;

  case 70: // topdecl: inst_decl
#line 661 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2132 "parser.cc"
    break;

  case 71: // topdecl: "default" "(" comma_types0 ")"
#line 664 "parser.y"
                                               {}
#line 2138 "parser.cc"
    break;

  case 72: // topdecl: decl_no_th
#line 671 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2144 "parser.cc"
    break;

  case 73: // topdecl: infixexp_top
#line 673 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2150 "parser.cc"
    break;

  case 74: // topdecl: "builtin" var "INTEGER" "STRING" "STRING"
#line 674 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2156 "parser.cc"
    break;

  case 75: // topdecl: "builtin" var "INTEGER" "STRING"
#line 675 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2162 "parser.cc"
    break;

  case 76: // topdecl: "builtin" varop "INTEGER" "STRING" "STRING"
#line 676 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2168 "parser.cc"
    break;

  case 77: // topdecl: "builtin" varop "INTEGER" "STRING"
#line 677 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2174 "parser.cc"
    break;

  case 78: // cl_decl: "class" tycl_hdr wherebinds
#line 679 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_class_decl(yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().second,{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2180 "parser.cc"
    break;

  case 79: // ty_decl: "type" type "=" ctypedoc
#line 681 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_type_synonym({yystack_[2].location,yystack_[2].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2186 "parser.cc"
    break;

  case 80: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 682 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < Haskell::DataOrNewtype > (),yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().second,yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2192 "parser.cc"
    break;

  case 81: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig
#line 683 "parser.y"
                                                                           {}
#line 2198 "parser.cc"
    break;

  case 82: // inst_decl: "instance" overlap_pragma inst_type wherebinds
#line 688 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_instance_decl({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2204 "parser.cc"
    break;

  case 92: // data_or_newtype: "data"
#line 743 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::data;}
#line 2210 "parser.cc"
    break;

  case 93: // data_or_newtype: "newtype"
#line 744 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::newtype;}
#line 2216 "parser.cc"
    break;

  case 96: // tycl_hdr: context "=>" type
#line 756 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ()};}
#line 2222 "parser.cc"
    break;

  case 97: // tycl_hdr: type
#line 757 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {{},yystack_[0].value.as < expression_ref > ()};}
#line 2228 "parser.cc"
    break;

  case 101: // decls: decls ";" decl
#line 805 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2234 "parser.cc"
    break;

  case 102: // decls: decls ";"
#line 806 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2240 "parser.cc"
    break;

  case 103: // decls: decl
#line 807 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2246 "parser.cc"
    break;

  case 104: // decls: %empty
#line 808 "parser.y"
                        {}
#line 2252 "parser.cc"
    break;

  case 105: // decllist: "{" decls "}"
#line 810 "parser.y"
                                 {yylhs.value.as < expression_ref > () = expression_ref{AST_node("Decls"),yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 2258 "parser.cc"
    break;

  case 106: // decllist: "vocurly" decls close
#line 811 "parser.y"
                                 {yylhs.value.as < expression_ref > () = expression_ref{AST_node("Decls"),yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 2264 "parser.cc"
    break;

  case 107: // binds: decllist
#line 813 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2270 "parser.cc"
    break;

  case 108: // wherebinds: "where" binds
#line 815 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2276 "parser.cc"
    break;

  case 109: // wherebinds: %empty
#line 816 "parser.y"
                                 {}
#line 2282 "parser.cc"
    break;

  case 115: // opt_sig: %empty
#line 837 "parser.y"
                 {}
#line 2288 "parser.cc"
    break;

  case 116: // opt_sig: "::" sigtype
#line 838 "parser.y"
                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2294 "parser.cc"
    break;

  case 117: // opt_tyconsig: %empty
#line 840 "parser.y"
                     {}
#line 2300 "parser.cc"
    break;

  case 118: // opt_tyconsig: "::" gtycon
#line 841 "parser.y"
                     {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2306 "parser.cc"
    break;

  case 119: // sigtype: ctype
#line 843 "parser.y"
                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2312 "parser.cc"
    break;

  case 120: // sigtypedoc: ctypedoc
#line 845 "parser.y"
                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2318 "parser.cc"
    break;

  case 121: // sig_vars: sig_vars "," var
#line 847 "parser.y"
                           {yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 2324 "parser.cc"
    break;

  case 122: // sig_vars: var
#line 848 "parser.y"
                           {yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 2330 "parser.cc"
    break;

  case 123: // sigtypes1: sigtype
#line 850 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2336 "parser.cc"
    break;

  case 124: // sigtypes1: sigtypes1 "," sigtype
#line 851 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2342 "parser.cc"
    break;

  case 125: // strict_mark: strictness
#line 855 "parser.y"
                                            {yylhs.value.as < Haskell::StrictLazy > () = yystack_[0].value.as < Haskell::StrictLazy > ();}
#line 2348 "parser.cc"
    break;

  case 126: // strictness: "!"
#line 861 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::strict;}
#line 2354 "parser.cc"
    break;

  case 127: // strictness: "~"
#line 862 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::lazy;}
#line 2360 "parser.cc"
    break;

  case 128: // ctype: "forall" tv_bndrs "." ctype
#line 869 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_forall_type(yystack_[2].value.as < std::vector<expression_ref> > (), yystack_[0].value.as < expression_ref > ());}
#line 2366 "parser.cc"
    break;

  case 129: // ctype: context "=>" ctype
#line 870 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_constrained_type(yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ());}
#line 2372 "parser.cc"
    break;

  case 130: // ctype: type
#line 872 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2378 "parser.cc"
    break;

  case 131: // ctypedoc: ctype
#line 874 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2384 "parser.cc"
    break;

  case 132: // context: btype
#line 883 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(yystack_[0].value.as < expression_ref > ());}
#line 2390 "parser.cc"
    break;

  case 133: // context_no_ops: btype_no_ops
#line 885 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2396 "parser.cc"
    break;

  case 134: // type: btype
#line 887 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2402 "parser.cc"
    break;

  case 135: // type: btype "->" ctype
#line 888 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps({make_type_var("->"),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()});}
#line 2408 "parser.cc"
    break;

  case 136: // typedoc: type
#line 890 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2414 "parser.cc"
    break;

  case 137: // btype: tyapps
#line 893 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2420 "parser.cc"
    break;

  case 138: // btype_no_ops: atype_docs
#line 895 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2426 "parser.cc"
    break;

  case 139: // btype_no_ops: btype_no_ops atype_docs
#line 896 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2432 "parser.cc"
    break;

  case 140: // tyapps: tyapp
#line 898 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2438 "parser.cc"
    break;

  case 141: // tyapps: tyapps tyapp
#line 899 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2444 "parser.cc"
    break;

  case 142: // tyapp: atype
#line 901 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2450 "parser.cc"
    break;

  case 143: // tyapp: qtyconop
#line 902 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2456 "parser.cc"
    break;

  case 144: // tyapp: tyvarop
#line 903 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2462 "parser.cc"
    break;

  case 145: // atype_docs: atype
#line 909 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2468 "parser.cc"
    break;

  case 146: // atype: ntgtycon
#line 916 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2474 "parser.cc"
    break;

  case 147: // atype: tyvar
#line 917 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2480 "parser.cc"
    break;

  case 148: // atype: "*"
#line 918 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var("*");}
#line 2486 "parser.cc"
    break;

  case 149: // atype: strict_mark atype
#line 919 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_strict_lazy_type(yystack_[1].value.as < Haskell::StrictLazy > (),yystack_[0].value.as < expression_ref > ());}
#line 2492 "parser.cc"
    break;

  case 150: // atype: "{" fielddecls "}"
#line 920 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_field_decls(yystack_[1].value.as < std::vector<Haskell::FieldDecl> > ());}
#line 2498 "parser.cc"
    break;

  case 151: // atype: "(" ")"
#line 921 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var("()");}
#line 2504 "parser.cc"
    break;

  case 152: // atype: "(" comma_types1 "," ctype ")"
#line 922 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<expression_ref> > ();ts.push_back(yystack_[1].value.as < expression_ref > ());yylhs.value.as < expression_ref > () = make_tuple_type(ts);}
#line 2510 "parser.cc"
    break;

  case 153: // atype: "[" ctype "]"
#line 928 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_list_type(yystack_[1].value.as < expression_ref > ());}
#line 2516 "parser.cc"
    break;

  case 154: // atype: "(" ctype ")"
#line 929 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2522 "parser.cc"
    break;

  case 155: // atype: "(" ctype "::" kind ")"
#line 930 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_of_kind(yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ());}
#line 2528 "parser.cc"
    break;

  case 156: // inst_type: sigtype
#line 933 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2534 "parser.cc"
    break;

  case 159: // comma_types0: comma_types1
#line 938 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2540 "parser.cc"
    break;

  case 160: // comma_types0: %empty
#line 939 "parser.y"
                                       { /* default construction OK */ }
#line 2546 "parser.cc"
    break;

  case 161: // comma_types1: ctype
#line 941 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2552 "parser.cc"
    break;

  case 162: // comma_types1: comma_types1 "," ctype
#line 942 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2558 "parser.cc"
    break;

  case 163: // tv_bndrs: tv_bndrs tv_bndr
#line 949 "parser.y"
                               {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2564 "parser.cc"
    break;

  case 164: // tv_bndrs: %empty
#line 950 "parser.y"
                               { /* default construction OK */}
#line 2570 "parser.cc"
    break;

  case 165: // tv_bndr: tyvar
#line 952 "parser.y"
                                    {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2576 "parser.cc"
    break;

  case 166: // tv_bndr: "(" tyvar "::" kind ")"
#line 953 "parser.y"
                                    {yylhs.value.as < expression_ref > () = make_type_var_of_kind(yystack_[3].value.as < std::string > (),yystack_[1].value.as < expression_ref > ());}
#line 2582 "parser.cc"
    break;

  case 167: // kind: ctype
#line 971 "parser.y"
             {yylhs.value.as < expression_ref > () = make_kind(yystack_[0].value.as < expression_ref > ());}
#line 2588 "parser.cc"
    break;

  case 168: // constrs: "=" constrs1
#line 977 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2594 "parser.cc"
    break;

  case 169: // constrs1: constrs1 "|" constr
#line 979 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2600 "parser.cc"
    break;

  case 170: // constrs1: constr
#line 980 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2606 "parser.cc"
    break;

  case 171: // constr: forall context_no_ops "=>" constr_stuff
#line 982 "parser.y"
                                                {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2612 "parser.cc"
    break;

  case 172: // constr: forall constr_stuff
#line 983 "parser.y"
                                                {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2618 "parser.cc"
    break;

  case 173: // forall: "forall" tv_bndrs "."
#line 985 "parser.y"
                                {if (yystack_[1].value.as < std::vector<expression_ref> > ().size()>1) yylhs.value.as < expression_ref > () = make_tv_bndrs(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2624 "parser.cc"
    break;

  case 174: // forall: %empty
#line 986 "parser.y"
                                {}
#line 2630 "parser.cc"
    break;

  case 175: // constr_stuff: btype_no_ops
#line 988 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2636 "parser.cc"
    break;

  case 176: // constr_stuff: btype_no_ops conop btype_no_ops
#line 989 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps({make_type_var(yystack_[1].value.as < std::string > ()),make_tyapps(yystack_[2].value.as < std::vector<expression_ref> > ()),make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 2642 "parser.cc"
    break;

  case 177: // fielddecls: %empty
#line 991 "parser.y"
                                {}
#line 2648 "parser.cc"
    break;

  case 178: // fielddecls: fielddecls1
#line 992 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::FieldDecl> > () = yystack_[0].value.as < std::vector<Haskell::FieldDecl> > ();}
#line 2654 "parser.cc"
    break;

  case 179: // fielddecls1: fielddecls1 "," fielddecl
#line 994 "parser.y"
                                        {yylhs.value.as < std::vector<Haskell::FieldDecl> > () = yystack_[2].value.as < std::vector<Haskell::FieldDecl> > (); yylhs.value.as < std::vector<Haskell::FieldDecl> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2660 "parser.cc"
    break;

  case 180: // fielddecls1: fielddecl
#line 995 "parser.y"
                                        {yylhs.value.as < std::vector<Haskell::FieldDecl> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2666 "parser.cc"
    break;

  case 181: // fielddecl: sig_vars "::" ctype
#line 997 "parser.y"
                                        {yylhs.value.as < Haskell::FieldDecl > () = make_field_decl(yystack_[2].value.as < std::vector<std::string> > (),yystack_[0].value.as < expression_ref > ());}
#line 2672 "parser.cc"
    break;

  case 192: // decl_no_th: sigdecl
#line 1016 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2678 "parser.cc"
    break;

  case 193: // decl_no_th: "!" aexp rhs
#line 1018 "parser.y"
                              {yylhs.value.as < expression_ref > () = new expression(AST_node("Decl:Strict"),{(yystack_[1].value.as < expression_ref > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2684 "parser.cc"
    break;

  case 194: // decl_no_th: infixexp_top opt_sig rhs
#line 1020 "parser.y"
                              {yylhs.value.as < expression_ref > () = new expression(AST_node("Decl"),{make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2690 "parser.cc"
    break;

  case 195: // decl: decl_no_th
#line 1024 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2696 "parser.cc"
    break;

  case 196: // rhs: "=" exp wherebinds
#line 1028 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_rhs(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2702 "parser.cc"
    break;

  case 197: // rhs: gdrhs wherebinds
#line 1029 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (),yystack_[0].value.as < expression_ref > ());}
#line 2708 "parser.cc"
    break;

  case 198: // gdrhs: gdrhs gdrh
#line 1031 "parser.y"
                              {yylhs.value.as < std::vector<Haskell::GuardedRHS> > () = yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (); yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 2714 "parser.cc"
    break;

  case 199: // gdrhs: gdrh
#line 1032 "parser.y"
                              {yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 2720 "parser.cc"
    break;

  case 200: // gdrh: "|" guardquals "=" exp
#line 1036 "parser.y"
                              {yylhs.value.as < Haskell::GuardedRHS > () = make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2726 "parser.cc"
    break;

  case 201: // sigdecl: infixexp_top "::" sigtypedoc
#line 1038 "parser.y"
                                             { yylhs.value.as < expression_ref > () = expression_ref(AST_node("Decl:sigtype"),{make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2732 "parser.cc"
    break;

  case 202: // sigdecl: var "," sig_vars "::" sigtypedoc
#line 1039 "parser.y"
                                          {}
#line 2738 "parser.cc"
    break;

  case 203: // sigdecl: infix prec ops
#line 1040 "parser.y"
                         { yylhs.value.as < expression_ref > () = make_fixity_decl(yystack_[2].value.as < Haskell::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()); }
#line 2744 "parser.cc"
    break;

  case 204: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1042 "parser.y"
                                                    {}
#line 2750 "parser.cc"
    break;

  case 205: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1043 "parser.y"
                                            {}
#line 2756 "parser.cc"
    break;

  case 206: // sigdecl: "{-# SCC" qvar "#-}"
#line 1044 "parser.y"
                              {}
#line 2762 "parser.cc"
    break;

  case 207: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1045 "parser.y"
                                     {}
#line 2768 "parser.cc"
    break;

  case 208: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1046 "parser.y"
                                                               {}
#line 2774 "parser.cc"
    break;

  case 209: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1047 "parser.y"
                                                                      {}
#line 2780 "parser.cc"
    break;

  case 210: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1048 "parser.y"
                                                     {}
#line 2786 "parser.cc"
    break;

  case 215: // exp: infixexp "::" sigtype
#line 1059 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_typed_exp(make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()); }
#line 2792 "parser.cc"
    break;

  case 216: // exp: infixexp
#line 1060 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 2798 "parser.cc"
    break;

  case 217: // infixexp: exp10
#line 1062 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2804 "parser.cc"
    break;

  case 218: // infixexp: infixexp qop exp10
#line 1063 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_id(yystack_[1].location,yystack_[1].value.as < std::string > ())); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2810 "parser.cc"
    break;

  case 219: // infixexp_top: exp10_top
#line 1065 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2816 "parser.cc"
    break;

  case 220: // infixexp_top: infixexp_top qop exp10_top
#line 1066 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_id(yystack_[1].location,yystack_[1].value.as < std::string > ())); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2822 "parser.cc"
    break;

  case 221: // exp10_top: "-" fexp
#line 1068 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_minus(make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2828 "parser.cc"
    break;

  case 222: // exp10_top: "{-# CORE" "STRING" "#-}"
#line 1069 "parser.y"
                                   {}
#line 2834 "parser.cc"
    break;

  case 223: // exp10_top: fexp
#line 1070 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2840 "parser.cc"
    break;

  case 224: // exp10: exp10_top
#line 1072 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2846 "parser.cc"
    break;

  case 225: // exp10: scc_annot exp
#line 1073 "parser.y"
                                 {}
#line 2852 "parser.cc"
    break;

  case 230: // fexp: fexp aexp
#line 1084 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2858 "parser.cc"
    break;

  case 231: // fexp: fexp "TYPEAPP" atype
#line 1085 "parser.y"
                                 {}
#line 2864 "parser.cc"
    break;

  case 232: // fexp: "static" aexp
#line 1086 "parser.y"
                                 {}
#line 2870 "parser.cc"
    break;

  case 233: // fexp: aexp
#line 1087 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2876 "parser.cc"
    break;

  case 234: // aexp: qvar "@" aexp
#line 1089 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_as_pattern(make_id(yystack_[2].location,yystack_[2].value.as < std::string > ()),yystack_[0].value.as < expression_ref > ());}
#line 2882 "parser.cc"
    break;

  case 235: // aexp: "~" aexp
#line 1090 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lazy_pattern(yystack_[0].value.as < expression_ref > ());}
#line 2888 "parser.cc"
    break;

  case 236: // aexp: "\\" apats1 "->" exp
#line 1091 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lambdaexp(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2894 "parser.cc"
    break;

  case 237: // aexp: "let" binds "in" exp
#line 1092 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_let(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2900 "parser.cc"
    break;

  case 238: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1094 "parser.y"
                                                       {yylhs.value.as < expression_ref > () = make_if(yystack_[6].value.as < expression_ref > (),yystack_[3].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2906 "parser.cc"
    break;

  case 239: // aexp: "case" exp "of" altslist
#line 1096 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_case(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < Haskell::Alts > ());}
#line 2912 "parser.cc"
    break;

  case 240: // aexp: "do" stmtlist
#line 1097 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_do(yystack_[0].value.as < Haskell::Stmts > ());}
#line 2918 "parser.cc"
    break;

  case 241: // aexp: "mdo" stmtlist
#line 1098 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_mdo(yystack_[0].value.as < Haskell::Stmts > ());}
#line 2924 "parser.cc"
    break;

  case 242: // aexp: aexp1
#line 1100 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2930 "parser.cc"
    break;

  case 243: // aexp1: aexp1 "{" fbinds "}"
#line 1102 "parser.y"
                              {}
#line 2936 "parser.cc"
    break;

  case 244: // aexp1: aexp2
#line 1103 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2942 "parser.cc"
    break;

  case 245: // aexp2: qvar
#line 1105 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_id(yystack_[0].location,yystack_[0].value.as < std::string > ());}
#line 2948 "parser.cc"
    break;

  case 246: // aexp2: qcon
#line 1106 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_id(yystack_[0].location,yystack_[0].value.as < std::string > ());}
#line 2954 "parser.cc"
    break;

  case 247: // aexp2: literal
#line 1107 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2960 "parser.cc"
    break;

  case 248: // aexp2: "(" texp ")"
#line 1108 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2966 "parser.cc"
    break;

  case 249: // aexp2: "(" tup_exprs ")"
#line 1109 "parser.y"
                              {yylhs.value.as < expression_ref > () = yy_make_tuple(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2972 "parser.cc"
    break;

  case 250: // aexp2: "[" list "]"
#line 1114 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2978 "parser.cc"
    break;

  case 251: // aexp2: "_"
#line 1115 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::WildcardPattern();}
#line 2984 "parser.cc"
    break;

  case 252: // texp: exp
#line 1120 "parser.y"
                      {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2990 "parser.cc"
    break;

  case 253: // texp: infixexp qop
#line 1121 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("LeftSection"),{make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ()),make_id(yystack_[0].location,yystack_[0].value.as < std::string > ())});}
#line 2996 "parser.cc"
    break;

  case 254: // texp: qopm infixexp
#line 1122 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("RightSection"),{make_id(yystack_[1].location,yystack_[1].value.as < std::string > ()),make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 3002 "parser.cc"
    break;

  case 255: // tup_exprs: tup_exprs "," texp
#line 1127 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3008 "parser.cc"
    break;

  case 256: // tup_exprs: texp "," texp
#line 1128 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3014 "parser.cc"
    break;

  case 257: // list: texp
#line 1146 "parser.y"
                                 { yylhs.value.as < expression_ref > () = make_list({yystack_[0].value.as < expression_ref > ()}); }
#line 3020 "parser.cc"
    break;

  case 258: // list: lexps
#line 1147 "parser.y"
                                 { yylhs.value.as < expression_ref > () = make_list(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 3026 "parser.cc"
    break;

  case 259: // list: texp ".."
#line 1148 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFrom"),{yystack_[1].value.as < expression_ref > ()}); }
#line 3032 "parser.cc"
    break;

  case 260: // list: texp "," exp ".."
#line 1149 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThen"),{yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()}); }
#line 3038 "parser.cc"
    break;

  case 261: // list: texp ".." exp
#line 1150 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromTo"),{yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 3044 "parser.cc"
    break;

  case 262: // list: texp "," exp ".." exp
#line 1151 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThenTo"),{yystack_[4].value.as < expression_ref > (),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 3050 "parser.cc"
    break;

  case 263: // list: texp "|" squals
#line 1152 "parser.y"
                                 { auto quals = yystack_[0].value.as < std::vector<expression_ref> > (); quals.push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < expression_ref > () = expression_ref(AST_node("ListComprehension"),quals); }
#line 3056 "parser.cc"
    break;

  case 264: // lexps: lexps "," texp
#line 1154 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3062 "parser.cc"
    break;

  case 265: // lexps: texp "," texp
#line 1155 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3068 "parser.cc"
    break;

  case 266: // squals: squals "," qual
#line 1168 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3074 "parser.cc"
    break;

  case 267: // squals: qual
#line 1170 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3080 "parser.cc"
    break;

  case 268: // guardquals: guardquals1
#line 1180 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3086 "parser.cc"
    break;

  case 269: // guardquals1: guardquals1 "," qual
#line 1182 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > ();yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3092 "parser.cc"
    break;

  case 270: // guardquals1: qual
#line 1183 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3098 "parser.cc"
    break;

  case 271: // altslist: "{" alts "}"
#line 1186 "parser.y"
                                 {yylhs.value.as < Haskell::Alts > () = make_alts(yystack_[1].value.as < std::vector<Haskell::Alt> > ());}
#line 3104 "parser.cc"
    break;

  case 272: // altslist: "vocurly" alts close
#line 1187 "parser.y"
                                 {yylhs.value.as < Haskell::Alts > () = make_alts(yystack_[1].value.as < std::vector<Haskell::Alt> > ());}
#line 3110 "parser.cc"
    break;

  case 273: // altslist: "{" "}"
#line 1188 "parser.y"
                                 {}
#line 3116 "parser.cc"
    break;

  case 274: // altslist: "vocurly" close
#line 1189 "parser.y"
                                 {}
#line 3122 "parser.cc"
    break;

  case 275: // alts: alts1
#line 1191 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::Alt> > () = yystack_[0].value.as < std::vector<Haskell::Alt> > ();}
#line 3128 "parser.cc"
    break;

  case 276: // alts: ";" alts
#line 1192 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::Alt> > () = yystack_[0].value.as < std::vector<Haskell::Alt> > ();}
#line 3134 "parser.cc"
    break;

  case 277: // alts1: alts1 ";" alt
#line 1194 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::Alt> > () = yystack_[2].value.as < std::vector<Haskell::Alt> > (); yylhs.value.as < std::vector<Haskell::Alt> > ().push_back(yystack_[0].value.as < Haskell::Alt > ());}
#line 3140 "parser.cc"
    break;

  case 278: // alts1: alts1 ";"
#line 1195 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::Alt> > () = yystack_[1].value.as < std::vector<Haskell::Alt> > ();}
#line 3146 "parser.cc"
    break;

  case 279: // alts1: alt
#line 1196 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::Alt> > ().push_back(yystack_[0].value.as < Haskell::Alt > ());}
#line 3152 "parser.cc"
    break;

  case 280: // alt: pat alt_rhs
#line 1198 "parser.y"
                                 {yylhs.value.as < Haskell::Alt > () = yy_make_alt(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3158 "parser.cc"
    break;

  case 281: // alt_rhs: "->" exp wherebinds
#line 1200 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_rhs(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3164 "parser.cc"
    break;

  case 282: // alt_rhs: gdpats wherebinds
#line 1201 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (),yystack_[0].value.as < expression_ref > ());}
#line 3170 "parser.cc"
    break;

  case 283: // gdpats: gdpats gdpat
#line 1203 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::GuardedRHS> > () = yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (); yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 3176 "parser.cc"
    break;

  case 284: // gdpats: gdpat
#line 1204 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 3182 "parser.cc"
    break;

  case 285: // gdpat: "|" guardquals "->" exp
#line 1213 "parser.y"
                                 {yylhs.value.as < Haskell::GuardedRHS > ()=make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3188 "parser.cc"
    break;

  case 286: // pat: exp
#line 1215 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3194 "parser.cc"
    break;

  case 287: // pat: "!" aexp
#line 1216 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3200 "parser.cc"
    break;

  case 288: // bindpat: exp
#line 1218 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3206 "parser.cc"
    break;

  case 289: // bindpat: "!" aexp
#line 1219 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3212 "parser.cc"
    break;

  case 290: // apat: aexp
#line 1221 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3218 "parser.cc"
    break;

  case 291: // apat: "!" aexp
#line 1222 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3224 "parser.cc"
    break;

  case 292: // apats1: apats1 apat
#line 1224 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3230 "parser.cc"
    break;

  case 293: // apats1: apat
#line 1225 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3236 "parser.cc"
    break;

  case 294: // stmtlist: "{" stmts "}"
#line 1228 "parser.y"
                               {yylhs.value.as < Haskell::Stmts > () = make_stmts(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3242 "parser.cc"
    break;

  case 295: // stmtlist: "vocurly" stmts close
#line 1229 "parser.y"
                               {yylhs.value.as < Haskell::Stmts > () = make_stmts(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3248 "parser.cc"
    break;

  case 296: // stmts: stmts ";" stmt
#line 1231 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3254 "parser.cc"
    break;

  case 297: // stmts: stmts ";"
#line 1232 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3260 "parser.cc"
    break;

  case 298: // stmts: stmt
#line 1233 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3266 "parser.cc"
    break;

  case 299: // stmts: %empty
#line 1234 "parser.y"
                       {}
#line 3272 "parser.cc"
    break;

  case 300: // stmt: qual
#line 1239 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3278 "parser.cc"
    break;

  case 301: // stmt: "rec" stmtlist
#line 1240 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::RecStmt(yystack_[0].value.as < Haskell::Stmts > ());}
#line 3284 "parser.cc"
    break;

  case 302: // qual: bindpat "<-" exp
#line 1242 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::PatQual(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3290 "parser.cc"
    break;

  case 303: // qual: exp
#line 1243 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::SimpleQual(yystack_[0].value.as < expression_ref > ());}
#line 3296 "parser.cc"
    break;

  case 304: // qual: "let" binds
#line 1244 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::LetQual(yystack_[0].value.as < expression_ref > ());}
#line 3302 "parser.cc"
    break;

  case 312: // qcon: gen_qcon
#line 1289 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3308 "parser.cc"
    break;

  case 313: // qcon: sysdcon
#line 1290 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3314 "parser.cc"
    break;

  case 314: // gen_qcon: qconid
#line 1292 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3320 "parser.cc"
    break;

  case 315: // gen_qcon: "(" qconsym ")"
#line 1293 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3326 "parser.cc"
    break;

  case 316: // con: conid
#line 1295 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3332 "parser.cc"
    break;

  case 317: // con: "(" consym ")"
#line 1296 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3338 "parser.cc"
    break;

  case 318: // con: sysdcon
#line 1297 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3344 "parser.cc"
    break;

  case 321: // sysdcon_no_list: "(" ")"
#line 1302 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 3350 "parser.cc"
    break;

  case 322: // sysdcon_no_list: "(" commas ")"
#line 1303 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3356 "parser.cc"
    break;

  case 323: // sysdcon_no_list: "(#" "#)"
#line 1304 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 3362 "parser.cc"
    break;

  case 324: // sysdcon_no_list: "(#" commas "#)"
#line 1305 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3368 "parser.cc"
    break;

  case 325: // sysdcon: sysdcon_no_list
#line 1307 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3374 "parser.cc"
    break;

  case 326: // sysdcon: "[" "]"
#line 1308 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 3380 "parser.cc"
    break;

  case 327: // conop: consym
#line 1310 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3386 "parser.cc"
    break;

  case 328: // conop: "`" conid "`"
#line 1311 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3392 "parser.cc"
    break;

  case 329: // qconop: qconsym
#line 1313 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3398 "parser.cc"
    break;

  case 330: // qconop: "`" qconid "`"
#line 1314 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3404 "parser.cc"
    break;

  case 331: // gtycon: ntgtycon
#line 1317 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3410 "parser.cc"
    break;

  case 332: // gtycon: "(" ")"
#line 1318 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 3416 "parser.cc"
    break;

  case 333: // gtycon: "(#" "#)"
#line 1319 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 3422 "parser.cc"
    break;

  case 334: // ntgtycon: oqtycon
#line 1321 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3428 "parser.cc"
    break;

  case 335: // ntgtycon: "(" commas ")"
#line 1322 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3434 "parser.cc"
    break;

  case 336: // ntgtycon: "(#" commas "#)"
#line 1323 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3440 "parser.cc"
    break;

  case 337: // ntgtycon: "(" "->" ")"
#line 1324 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 3446 "parser.cc"
    break;

  case 338: // ntgtycon: "[" "]"
#line 1325 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 3452 "parser.cc"
    break;

  case 339: // oqtycon: qtycon
#line 1327 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3458 "parser.cc"
    break;

  case 340: // oqtycon: "(" qtyconsym ")"
#line 1328 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3464 "parser.cc"
    break;

  case 341: // oqtycon: "(" "~" ")"
#line 1329 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3470 "parser.cc"
    break;

  case 342: // oqtycon_no_varcon: qtycon
#line 1331 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3476 "parser.cc"
    break;

  case 343: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1332 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3482 "parser.cc"
    break;

  case 344: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1333 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3488 "parser.cc"
    break;

  case 345: // oqtycon_no_varcon: "(" ":" ")"
#line 1334 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 3494 "parser.cc"
    break;

  case 346: // oqtycon_no_varcon: "(" "~" ")"
#line 1335 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3500 "parser.cc"
    break;

  case 347: // qtyconop: qtyconsym
#line 1338 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3506 "parser.cc"
    break;

  case 348: // qtyconop: "`" qtycon "`"
#line 1339 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3512 "parser.cc"
    break;

  case 349: // qtycondoc: qtycon
#line 1341 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3518 "parser.cc"
    break;

  case 350: // qtycon: "QCONID"
#line 1343 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3524 "parser.cc"
    break;

  case 351: // qtycon: tycon
#line 1344 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3530 "parser.cc"
    break;

  case 352: // tycon: "CONID"
#line 1348 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3536 "parser.cc"
    break;

  case 353: // qtyconsym: "QCONSYM"
#line 1350 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3542 "parser.cc"
    break;

  case 354: // qtyconsym: "QVARSYM"
#line 1351 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3548 "parser.cc"
    break;

  case 355: // qtyconsym: tyconsym
#line 1352 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3554 "parser.cc"
    break;

  case 356: // tyconsym: "CONSYM"
#line 1354 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3560 "parser.cc"
    break;

  case 357: // tyconsym: "VARSYM"
#line 1355 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3566 "parser.cc"
    break;

  case 358: // tyconsym: ":"
#line 1356 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 3572 "parser.cc"
    break;

  case 359: // tyconsym: "-"
#line 1357 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 3578 "parser.cc"
    break;

  case 360: // op: varop
#line 1362 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3584 "parser.cc"
    break;

  case 361: // op: conop
#line 1363 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3590 "parser.cc"
    break;

  case 362: // varop: varsym
#line 1365 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3596 "parser.cc"
    break;

  case 363: // varop: "`" varid "`"
#line 1366 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3602 "parser.cc"
    break;

  case 364: // qop: qvarop
#line 1368 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3608 "parser.cc"
    break;

  case 365: // qop: qconop
#line 1369 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3614 "parser.cc"
    break;

  case 366: // qop: hole_op
#line 1370 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3620 "parser.cc"
    break;

  case 367: // qopm: qvaropm
#line 1372 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3626 "parser.cc"
    break;

  case 368: // qopm: qconop
#line 1373 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3632 "parser.cc"
    break;

  case 369: // qopm: hole_op
#line 1374 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3638 "parser.cc"
    break;

  case 370: // hole_op: "`" "_" "`"
#line 1376 "parser.y"
                      { yylhs.value.as < std::string > () = "_"; }
#line 3644 "parser.cc"
    break;

  case 371: // qvarop: qvarsym
#line 1378 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3650 "parser.cc"
    break;

  case 372: // qvarop: "`" qvarid "`"
#line 1379 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3656 "parser.cc"
    break;

  case 373: // qvaropm: qvarsym_no_minus
#line 1381 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 3662 "parser.cc"
    break;

  case 374: // qvaropm: "`" qvarid "`"
#line 1382 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3668 "parser.cc"
    break;

  case 375: // tyvar: tyvarid
#line 1386 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3674 "parser.cc"
    break;

  case 376: // tyvarop: "`" tyvarid "`"
#line 1388 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3680 "parser.cc"
    break;

  case 377: // tyvarid: "VARID"
#line 1390 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3686 "parser.cc"
    break;

  case 378: // tyvarid: special_id
#line 1391 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3692 "parser.cc"
    break;

  case 379: // tyvarid: "unsafe"
#line 1392 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 3698 "parser.cc"
    break;

  case 380: // tyvarid: "safe"
#line 1393 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 3704 "parser.cc"
    break;

  case 381: // tyvarid: "interruptible"
#line 1394 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 3710 "parser.cc"
    break;

  case 382: // var: varid
#line 1397 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3716 "parser.cc"
    break;

  case 383: // var: "(" varsym ")"
#line 1398 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3722 "parser.cc"
    break;

  case 384: // qvar: qvarid
#line 1400 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3728 "parser.cc"
    break;

  case 385: // qvar: "(" varsym ")"
#line 1401 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3734 "parser.cc"
    break;

  case 386: // qvar: "(" qvarsym1 ")"
#line 1402 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3740 "parser.cc"
    break;

  case 387: // qvarid: varid
#line 1404 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3746 "parser.cc"
    break;

  case 388: // qvarid: "QVARID"
#line 1405 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3752 "parser.cc"
    break;

  case 389: // varid: "VARID"
#line 1407 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3758 "parser.cc"
    break;

  case 390: // varid: special_id
#line 1408 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3764 "parser.cc"
    break;

  case 391: // varid: "unsafe"
#line 1409 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 3770 "parser.cc"
    break;

  case 392: // varid: "safe"
#line 1410 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 3776 "parser.cc"
    break;

  case 393: // varid: "interruptible"
#line 1411 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 3782 "parser.cc"
    break;

  case 394: // varid: "forall"
#line 1412 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 3788 "parser.cc"
    break;

  case 395: // varid: "family"
#line 1413 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 3794 "parser.cc"
    break;

  case 396: // varid: "role"
#line 1414 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 3800 "parser.cc"
    break;

  case 397: // qvarsym: varsym
#line 1416 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3806 "parser.cc"
    break;

  case 398: // qvarsym: qvarsym1
#line 1417 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3812 "parser.cc"
    break;

  case 399: // qvarsym_no_minus: varsym_no_minus
#line 1419 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3818 "parser.cc"
    break;

  case 400: // qvarsym_no_minus: qvarsym1
#line 1420 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3824 "parser.cc"
    break;

  case 401: // qvarsym1: "QVARSYM"
#line 1422 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3830 "parser.cc"
    break;

  case 402: // varsym: varsym_no_minus
#line 1424 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3836 "parser.cc"
    break;

  case 403: // varsym: "-"
#line 1425 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 3842 "parser.cc"
    break;

  case 404: // varsym_no_minus: "VARSYM"
#line 1427 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3848 "parser.cc"
    break;

  case 405: // varsym_no_minus: special_sym
#line 1428 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3854 "parser.cc"
    break;

  case 406: // special_id: "as"
#line 1430 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 3860 "parser.cc"
    break;

  case 407: // special_id: "qualified"
#line 1431 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 3866 "parser.cc"
    break;

  case 408: // special_id: "hiding"
#line 1432 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 3872 "parser.cc"
    break;

  case 409: // special_id: "export"
#line 1433 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 3878 "parser.cc"
    break;

  case 410: // special_id: "label"
#line 1434 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 3884 "parser.cc"
    break;

  case 411: // special_id: "dynamic"
#line 1435 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 3890 "parser.cc"
    break;

  case 412: // special_id: "stdcall"
#line 1436 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 3896 "parser.cc"
    break;

  case 413: // special_id: "ccall"
#line 1437 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 3902 "parser.cc"
    break;

  case 414: // special_id: "capi"
#line 1438 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 3908 "parser.cc"
    break;

  case 415: // special_id: "prim"
#line 1439 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 3914 "parser.cc"
    break;

  case 416: // special_id: "javascript"
#line 1440 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 3920 "parser.cc"
    break;

  case 417: // special_id: "group"
#line 1441 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 3926 "parser.cc"
    break;

  case 418: // special_id: "stock"
#line 1442 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 3932 "parser.cc"
    break;

  case 419: // special_id: "anyclass"
#line 1443 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 3938 "parser.cc"
    break;

  case 420: // special_id: "via"
#line 1444 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 3944 "parser.cc"
    break;

  case 421: // special_id: "unit"
#line 1445 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 3950 "parser.cc"
    break;

  case 422: // special_id: "dependency"
#line 1446 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 3956 "parser.cc"
    break;

  case 423: // special_id: "signature"
#line 1447 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 3962 "parser.cc"
    break;

  case 424: // special_sym: "!"
#line 1449 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 3968 "parser.cc"
    break;

  case 425: // special_sym: "."
#line 1450 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 3974 "parser.cc"
    break;

  case 426: // special_sym: "*"
#line 1451 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 3980 "parser.cc"
    break;

  case 427: // qconid: conid
#line 1455 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3986 "parser.cc"
    break;

  case 428: // qconid: "QCONID"
#line 1456 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3992 "parser.cc"
    break;

  case 429: // conid: "CONID"
#line 1458 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3998 "parser.cc"
    break;

  case 430: // qconsym: consym
#line 1460 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4004 "parser.cc"
    break;

  case 431: // qconsym: "QCONSYM"
#line 1461 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4010 "parser.cc"
    break;

  case 432: // consym: "CONSYM"
#line 1463 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4016 "parser.cc"
    break;

  case 433: // consym: ":"
#line 1464 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4022 "parser.cc"
    break;

  case 434: // literal: "CHAR"
#line 1468 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < char > ();}
#line 4028 "parser.cc"
    break;

  case 435: // literal: "STRING"
#line 1469 "parser.y"
                  {yylhs.value.as < expression_ref > () = yy_make_string(yystack_[0].value.as < std::string > ());}
#line 4034 "parser.cc"
    break;

  case 436: // literal: "INTEGER"
#line 1470 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < int > ();}
#line 4040 "parser.cc"
    break;

  case 437: // literal: "RATIONAL"
#line 1471 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < double > ();}
#line 4046 "parser.cc"
    break;

  case 439: // close: error
#line 1479 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 4052 "parser.cc"
    break;

  case 440: // modid: "CONID"
#line 1483 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4058 "parser.cc"
    break;

  case 441: // modid: "QCONID"
#line 1484 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4064 "parser.cc"
    break;

  case 442: // commas: commas ","
#line 1486 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 4070 "parser.cc"
    break;

  case 443: // commas: ","
#line 1487 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 4076 "parser.cc"
    break;


#line 4080 "parser.cc"

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


  const short parser::yypact_ninf_ = -593;

  const short parser::yytable_ninf_ = -403;

  const short
  parser::yypact_[] =
  {
      32,    77,  -593,    83,  -593,  -593,  -593,  -593,  -593,   197,
     -24,    44,  -593,    47,   -30,   -30,    95,  -593,  -593,  -593,
    -593,   158,  -593,  -593,  -593,    55,  -593,   146,   159,  3574,
     185,   217,   172,  -593,   635,  -593,   -26,  -593,  -593,  -593,
    -593,    77,  -593,   -38,  -593,  -593,  -593,  -593,  -593,  -593,
    -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,
    -593,  -593,  -593,  -593,  -593,   414,  -593,  -593,  -593,  -593,
     191,   207,  -593,   222,  -593,  -593,  -593,  -593,  -593,  -593,
    -593,  -593,   154,   234,   300,  -593,   224,  -593,  2004,  3236,
    -593,   228,   206,  2004,  -593,  -593,  -593,   353,   215,  -593,
    3236,  3895,   206,  2730,   236,   212,  3850,   124,  2367,  2730,
    2488,  2730,  1143,  1015,    61,  -593,  -593,  -593,  -593,  -593,
    -593,    40,   236,   218,   172,  -593,  -593,  -593,   273,  -593,
    -593,   289,  -593,  2609,  -593,   247,  -593,  -593,  -593,  -593,
    -593,   235,   265,   240,  -593,  -593,  -593,  -593,   231,  -593,
      73,  -593,  -593,   253,   256,  -593,  -593,  -593,  -593,  -593,
     257,  -593,   259,   263,   267,  -593,  -593,  -593,  3574,  3607,
    -593,  -593,  -593,  -593,  -593,  -593,   359,  -593,    50,  1015,
     354,   339,  -593,  -593,  2004,  -593,  -593,  -593,  -593,  -593,
    -593,  -593,  -593,  4017,  2933,  2832,   270,  3758,  -593,  -593,
    -593,  -593,  -593,   358,  3666,  -593,   301,  -593,   149,  3236,
    -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,
    -593,  3034,  1520,  1520,  -593,   276,   320,   325,   329,   330,
    3034,   764,   764,  -593,   395,   334,   346,   204,  4074,   282,
     304,  -593,  -593,  -593,  -593,   -20,  3850,  -593,   360,   169,
     -16,   335,   -14,   327,   362,  -593,  -593,  2730,  -593,  -593,
    2246,  -593,  2609,   104,  -593,  -593,  3337,  -593,  -593,  -593,
     339,    76,   340,   331,  -593,  2004,  -593,  -593,  -593,  -593,
    -593,  -593,  -593,  2488,  -593,  -593,   102,   120,   263,   338,
     341,   342,   129,  -593,    99,  3034,  3850,  3850,  -593,   160,
     224,   315,  3236,  3034,  3337,   104,  -593,  2125,  -593,  -593,
    -593,  -593,  -593,  3666,  -593,  3791,  4017,  2730,  -593,   343,
     344,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,   347,
     333,  -593,  -593,   348,    47,  -593,   328,   380,   383,   223,
    3034,  2004,  -593,    48,   364,   355,  -593,  -593,  -593,  -593,
     363,   381,  -593,   365,   343,  -593,    65,   361,   344,   140,
     110,   357,   367,   215,  -593,  -593,  3236,  3034,  -593,  -593,
     369,   371,   215,   206,  2730,   393,   398,    53,  -593,  -593,
      45,  -593,   462,  -593,  -593,  -593,  -593,  -593,  -593,   358,
      69,  -593,  -593,   430,    46,  2004,  3034,   389,   374,   373,
     384,   379,   387,   436,  -593,  -593,   437,   407,   124,   269,
     440,  -593,  2004,  -593,  2004,  1762,  -593,    30,  -593,   403,
     408,   409,  2004,  2004,  1762,  1271,  -593,  1271,   450,  -593,
    1271,  -593,  1271,   411,  -593,  -593,  -593,  -593,   449,   451,
     455,  3984,   416,  -593,  -593,  -593,  -593,   -13,   190,  -593,
    -593,   203,  -593,   421,  -593,  -593,  -593,  -593,   439,  -593,
     429,   467,    70,  -593,  -593,  -593,  -593,  3607,  -593,  -593,
    -593,    77,  -593,  -593,  1399,   894,  -593,  -593,  -593,  3034,
    4017,  -593,  4017,  4107,  -593,  3034,  -593,  3034,  -593,  3034,
    -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  3034,   395,
    -593,  -593,  2004,  -593,  1520,  -593,  2004,  -593,  -593,   764,
    -593,  -593,  -593,  -593,  -593,  -593,   423,   424,   452,  -593,
    -593,  -593,  -593,  -593,   453,   616,   135,  -593,  -593,  -593,
    -593,   358,   474,   446,  -593,  -593,  -593,  -593,  -593,  -593,
    -593,   448,  -593,   485,  -593,  -593,  -593,  -593,  -593,  3034,
    3034,   457,   160,  -593,   489,  3034,   539,  -593,   566,  -593,
    -593,  3791,  1271,  3034,   463,   572,  2730,  -593,  1641,  -593,
     479,   468,  -593,   241,    47,  -593,  -593,  -593,  -593,  3034,
    4166,  -593,  -593,  -593,  -593,   472,   475,  -593,  -593,  -593,
     276,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  2004,  1762,
    1762,  2004,  -593,    16,    22,  -593,  -593,  -593,  -593,  -593,
     498,  -593,  3666,    82,  -593,   566,  -593,  -593,  -593,  -593,
    -593,    77,    25,  -593,  -593,  -593,  1883,  1762,  2004,  -593,
      60,  -593,  -593,  -593,   505,  -593,  -593,   578,  -593,  -593,
    -593,  -593,  -593,  3034,  -593,  4133,   539,   502,  3382,  -593,
    -593,  -593,  -593,  -593,  -593,  3135,   -32,   540,  -593,  -593,
    -593,  -593,   487,  3574,  -593,  -593,  -593,   510,   358,  -593,
    -593,  3034,  2004,  -593,  -593,  -593,  3666,   480,  -593,  3666,
    -593,  -593,   486,   494,  -593,  3236,  -593,  3574,   495,  2004,
    -593,   496,  -593,  3474,  -593,  3666,  3236,  -593,  -593,   497,
    -593,  -593,  -593,  -593,  -593
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    39,     0,     2,    39,     4,   440,   441,     8,
       0,    42,     1,     0,     0,     0,    18,    11,    38,    13,
      16,    67,   439,   438,    12,   114,   110,     0,     0,     0,
       0,    45,    40,    15,    14,   113,     0,     6,     7,   406,
     408,     0,   407,     0,   394,   409,   410,   411,   392,   393,
     391,   395,   396,   412,   413,   414,   415,   416,   417,   418,
     419,   420,   421,   423,   422,     0,   389,   352,   388,   350,
       0,    19,    21,    24,    32,    35,   342,   351,    34,   384,
     387,   390,     0,     0,    47,    37,    41,   251,     0,     0,
      92,     0,     0,     0,    60,    61,    62,    87,     0,    93,
       0,     0,     0,     0,   211,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   429,   428,   434,   435,   436,
     437,   211,   211,    58,    65,    68,    69,    70,   100,    72,
     192,    73,   219,   223,   233,   242,   244,   246,   312,   325,
     313,     0,   245,   387,   314,   427,   247,   111,     0,    23,
       0,    33,   339,     0,     0,   403,   424,   426,   425,   404,
       0,   401,     0,     0,     0,   402,   405,    17,     0,    26,
      22,    39,    39,     3,    44,    46,    51,    36,     0,     0,
       0,   216,   224,   217,     0,   380,   381,   379,   358,   127,
     359,   126,   148,   177,     0,     0,     0,     0,   377,   357,
     356,   354,   353,   109,     0,   125,     0,    97,   134,   137,
     140,   142,   146,   334,   143,   347,   355,   147,   144,   375,
     378,   160,   299,   299,   240,   227,     0,     0,     0,     0,
       0,   104,   104,   107,     0,     0,   134,     0,     0,     0,
       0,   382,   362,   241,   232,     0,     0,   212,     0,     0,
       0,     0,     0,   319,   117,   318,   316,     0,   290,   293,
       0,   235,   221,     0,   433,   326,     0,   432,   431,   252,
     216,   257,     0,   258,   368,     0,   369,   367,   373,   400,
     399,   329,   430,   403,   321,   443,     0,     0,   400,     0,
     399,   329,     0,   323,     0,     0,     0,     0,    59,     0,
      66,     0,     0,     0,     0,     0,   365,     0,   366,   364,
     371,   398,   397,     0,   230,   306,     0,     0,   112,     0,
       0,   345,   346,   344,   343,   386,   385,    20,    31,     0,
      27,    29,    30,     0,     0,    50,    49,     0,     0,     0,
       0,     0,   225,     0,     0,   178,   180,   122,   164,   338,
       0,     0,   130,     0,   127,   151,   161,     0,   347,     0,
       0,     0,     0,     0,    78,   149,     0,     0,   141,   161,
       0,   159,     0,     0,     0,   303,     0,     0,   298,   300,
       0,   226,     0,    84,    83,    85,    86,   156,   119,   109,
       0,   195,   103,   115,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   222,   206,     0,     0,     0,     0,
       0,   291,     0,   292,     0,     0,   193,   109,   199,     0,
       0,     0,   253,   259,     0,     0,   250,     0,   254,   248,
       0,   249,     0,   385,   315,   322,   442,   324,     0,     0,
       0,     0,   203,   361,    64,   360,   327,     0,    94,   116,
     201,   131,   120,     0,   194,   220,   231,   309,     0,   305,
     308,   311,     0,   234,   341,   340,    25,     0,     9,    10,
      48,     0,   229,   228,     0,     0,   239,   215,   218,     0,
       0,   150,     0,     0,   153,     0,   337,     0,   154,     0,
     335,   336,   348,   376,   108,    96,   135,    71,     0,   304,
     301,   289,     0,   294,   297,   295,     0,    82,   105,   102,
     106,   237,   131,    79,   383,   363,    77,    75,     0,   213,
     205,   207,   317,   320,     0,     0,     0,   118,   331,   204,
     236,   109,     0,   268,   270,   197,   198,   370,   374,   330,
     261,   263,   267,   252,   265,   264,   256,   255,   210,     0,
       0,     0,     0,    99,     0,     0,   174,    81,   182,   372,
     243,     0,     0,     0,     0,    53,     0,   273,     0,   286,
       0,   275,   279,     0,     0,   274,   181,   121,   179,     0,
       0,   163,   165,   129,   167,     0,   162,   162,   302,   296,
     227,   101,    76,    74,   214,   332,   333,   196,     0,     0,
       0,   260,   123,     0,     0,   328,    63,    98,    95,   164,
     168,   170,     0,     0,    80,   183,   185,   307,   310,   202,
      28,     0,    55,   287,   276,   271,   278,     0,     0,   280,
     109,   284,   272,   128,     0,   155,   152,     0,   200,   269,
     266,   262,   208,     0,   209,     0,   174,     0,   175,   138,
     145,   172,    90,    88,    89,     0,     0,   186,   189,   349,
     184,    52,     0,     0,    43,    54,   277,     0,   109,   282,
     283,     0,     0,   124,   173,   169,     0,     0,   139,     0,
     190,   136,   157,     0,   187,     0,   188,     0,     0,     0,
     281,     0,   238,   175,   171,   176,     0,   191,    91,     0,
      56,   285,   166,   158,    57
  };

  const short
  parser::yypgoto_[] =
  {
    -593,  -593,  -593,  -593,  -593,  -593,  -593,    36,  -593,  -593,
    -507,  -593,   438,  -593,  -593,  -593,   143,  -148,  -593,   490,
    -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,
    -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,
    -593,  -593,  -593,  -593,  -593,   311,  -593,   385,  -593,  -208,
    -366,   600,  -593,  -593,  -593,  -277,    56,   302,    66,  -593,
    -593,  -181,   225,   -62,  -593,   -88,  -593,   -97,  -376,  -593,
     413,  -592,  -195,   332,   -76,  -593,   402,    15,  -593,  -520,
    -593,  -593,   -21,  -593,   -50,  -593,  -593,   148,  -593,  -593,
      13,   -25,   598,   126,   345,  -593,   216,  -593,   168,  -593,
     -82,   -94,   603,   -10,  -287,    54,  -593,   -77,   -59,  -593,
    -593,   -70,  -593,  -593,  -593,  -593,    19,  -593,  -593,  -430,
    -593,    23,  -593,  -593,    26,  -593,  -593,   405,  -593,   -80,
     458,   176,  -353,  -593,   101,  -593,  -593,  -593,  -593,   275,
    -593,   -90,  -576,  -102,  -593,   284,   641,  -593,  -593,  -593,
     -27,  -593,  -125,  -593,   142,   595,  -147,  -593,   -65,  -593,
    -593,  -451,  -593,   503,   -86,   -29,  -185,    -6,  -593,  -593,
      -5,   -58,   -74,   -84,  -593,  -177,   -99,   -55,  -232,  -593,
    -298,   -37,  -104
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   173,     6,    10,    19,    30,
      70,    71,    72,   170,   329,   330,   331,    73,    74,    86,
      11,    20,    21,    32,    84,   176,   471,   336,   622,   664,
     665,   299,   123,   442,    33,    34,   124,   125,   126,   127,
     230,   656,   686,   128,   557,   203,   302,   390,   233,   234,
     364,    27,    36,   305,   410,   387,   450,   343,   603,   204,
     205,   388,   452,   351,   647,   352,   682,   208,   648,   209,
     210,   649,   211,   389,   683,   370,   357,   483,   581,   585,
     558,   610,   611,   612,   651,   344,   345,   346,   614,   615,
     616,   657,   391,   392,   416,   417,   418,   130,   246,   247,
     375,   181,   393,   182,   183,   382,   184,   133,   134,   135,
     136,   286,   287,   272,   273,   541,   532,   533,   476,   570,
     571,   572,   629,   630,   631,   573,   376,   259,   260,   224,
     377,   378,   379,   458,   459,   460,   137,   138,   253,   254,
     139,   140,   443,   274,   527,   212,   213,    75,   214,   658,
     152,    77,   215,   216,   444,   445,   307,   275,   276,   309,
     277,   217,   218,   219,   141,   142,    79,    80,   310,   278,
     279,   312,   165,    81,   166,   144,   145,   281,   282,   146,
      24,     9,   292
  };

  const short
  parser::yytable_[] =
  {
      78,   207,    76,   236,   149,   220,   180,   164,   256,   365,
     294,   225,   235,   350,   356,   240,   220,   255,   270,   270,
     407,   332,   243,   507,   132,   320,   449,   206,   143,   306,
     269,   269,   582,   262,   341,   608,   469,   662,   280,   290,
     369,    13,   271,   242,   244,   574,    22,    22,    22,   258,
     261,   535,   263,     1,   478,   289,   678,   363,   291,   295,
     163,   405,   534,   477,   553,   264,   308,   446,   401,   150,
     358,   542,   679,    25,   314,   655,    17,   250,   147,   306,
      67,   420,   505,    12,    69,   270,    67,   363,   148,   421,
      69,   359,   360,   642,   284,   241,   510,   269,    26,   644,
     285,   678,   342,   678,   652,   290,   267,   347,   288,   402,
     220,   220,   406,   220,   415,   554,   308,   679,   456,   453,
     220,   164,   451,   422,   291,   220,   311,   421,   479,   634,
     643,     2,   663,   653,   654,   478,   643,   220,   624,    78,
      78,    76,    76,   245,   627,   487,   220,    23,    23,    23,
     563,   691,   188,   503,   423,   494,   688,    18,   504,   509,
     424,   319,   480,   190,   499,   597,   504,   337,   306,   508,
     361,   293,    31,   488,   288,   285,   311,   575,   338,   397,
     699,   428,   509,    35,   480,   414,   496,   241,   415,   655,
     425,   164,   199,   200,   582,     7,   201,   202,   411,     8,
      67,   258,    29,   314,    69,   308,   262,   333,   334,   437,
     429,   220,    82,   436,   207,   512,   430,   403,   220,   220,
     491,   132,   132,    37,   436,   143,   143,   251,   431,   220,
     347,   252,   398,   114,   432,   367,    38,   435,  -132,   264,
     206,   242,   115,   436,   163,   596,   639,   640,   490,   285,
     155,   156,   157,   171,   436,   172,   220,   158,   463,   155,
     156,   157,    14,    15,   669,   311,   158,   439,   440,   236,
     555,   556,   602,   602,   534,   441,   632,    83,   495,   159,
     267,   341,   220,   220,  -119,    85,   461,  -119,   159,   296,
     297,   306,   161,   500,   155,   156,   157,   455,   576,   167,
     693,   158,   690,   695,   583,   222,   584,   223,   586,   256,
     241,   174,   220,   511,   231,   501,   232,   587,   255,   332,
     446,   168,   474,   159,   475,   627,   306,   628,   308,   169,
     530,   270,   531,   270,   175,   221,   270,   177,   270,   245,
     248,   540,   551,   543,   301,   269,   315,   298,   269,   316,
     269,   280,   317,   280,  -382,   544,   280,   545,   280,   318,
     546,   321,   547,   308,   322,   323,   673,   324,   264,   303,
    -115,   325,   524,  -115,   584,   326,   525,   339,   526,   155,
     156,   157,   512,   335,   285,   363,   158,    67,   311,   381,
     366,    69,   569,   569,   577,   220,   347,   383,   633,   220,
     320,   220,   384,   220,   304,   220,   385,   386,   159,   267,
     395,   399,   161,   268,   220,   396,   446,   650,   264,   340,
     588,   359,   360,   311,   590,   226,   227,   228,   229,   155,
     156,   157,   367,   400,   565,   398,   158,   404,    78,   265,
      76,   408,   409,   447,   426,   427,   433,   467,   468,  -402,
     434,   464,   465,   650,   304,   466,   470,   472,   159,   267,
     473,   446,   161,   268,   481,   220,   220,   484,   270,   482,
     485,   220,   492,   486,   241,   489,   241,   497,  -288,   220,
     269,   650,   493,   502,   650,   498,   569,   506,   280,   515,
     584,   519,   618,   153,   242,   220,   220,   514,   650,   132,
     650,   516,   154,   143,   155,   156,   157,   623,   518,   264,
     303,   158,   517,   520,   521,   522,   638,   529,   537,   641,
     155,   156,   157,   538,   539,  -383,   548,   158,   220,   264,
     552,   549,   461,   159,   160,   550,   559,   161,   162,   560,
     155,   156,   157,   561,   569,   304,   668,   158,   562,   159,
     267,   592,   593,   161,   268,   598,   594,   349,   236,   220,
     599,   220,   600,   601,   220,   304,   607,   681,   609,   159,
     267,   220,   605,   161,   268,   613,   621,   620,   551,   625,
     635,   626,   646,   636,   661,   671,   659,   220,   236,   672,
     692,   676,   220,   685,   687,   220,   689,   698,   115,   236,
     696,   220,   697,   700,   702,   704,   327,   701,   681,   220,
     564,   220,   220,   448,   300,    28,   604,   394,   462,   619,
     703,   513,   368,   371,   645,   675,   694,   438,   660,   659,
     578,   684,   129,   536,    78,   591,    76,   131,    87,    39,
      88,    89,    90,    91,   637,    92,   667,    40,    93,   666,
     454,    94,    95,    96,    97,    98,   670,    99,    78,    42,
      76,   100,   617,   101,    44,   413,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,    55,    56,    57,
     589,   380,    58,   523,   151,   103,    59,    60,    61,    62,
      63,    64,   104,   528,   606,   188,   239,   105,   106,     0,
     362,     0,   353,     0,   319,     0,   190,     0,     0,     0,
       0,   107,     0,     0,     0,     0,     0,   108,     0,     0,
       0,     0,     0,   109,   595,   110,   111,     0,     0,     0,
     285,     0,     0,     0,     0,   199,   200,     0,   112,   201,
     202,     0,   113,     0,   114,     0,     0,     0,     0,     0,
       0,     0,    66,   115,     0,     0,    68,   116,     0,     0,
       0,     0,   117,   118,   119,   120,     0,    87,    39,    88,
       0,     0,   121,   122,    92,     0,    40,    93,     0,     0,
      94,    95,    96,     0,    98,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,   103,    59,    60,    61,    62,    63,
      64,   104,     0,     0,     0,     0,   105,   106,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,   108,     0,     0,     0,
       0,     0,   109,     0,   110,   111,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   112,     0,     0,
       0,   113,     0,   114,     0,     0,     0,     0,     0,     0,
       0,    66,   115,     0,     0,    68,   116,     0,     0,     0,
       0,   117,   118,   119,   120,    22,     0,    87,    39,    88,
       0,   121,   122,     0,    92,     0,    40,    93,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,   103,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,   105,   178,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   108,     0,     0,     0,
       0,     0,   109,     0,   110,   566,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    23,   112,     0,     0,
       0,   179,     0,   114,     0,     0,     0,   568,     0,     0,
       0,    66,   115,     0,     0,    68,   116,     0,    87,    39,
      88,   117,   118,   119,   120,    92,     0,    40,    93,     0,
       0,     0,     0,     0,     0,    98,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,   103,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,   105,   178,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   264,     0,     0,   108,     0,     0,
       0,     0,     0,   109,     0,   283,   156,   157,     0,     0,
       0,     0,   158,     0,     0,     0,     0,     0,   112,     0,
       0,     0,   179,   284,   114,     0,     0,     0,     0,   285,
     266,     0,    66,   115,   159,   267,    68,   116,   161,   268,
       0,     0,   117,   118,   119,   120,    87,    39,    88,     0,
       0,     0,     0,    92,     0,    40,    93,     0,     0,     0,
       0,     0,     0,    98,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,   103,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,   105,   178,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   264,     0,     0,   108,     0,     0,     0,     0,
       0,   109,     0,   110,   156,   157,     0,     0,     0,     0,
     158,     0,     0,     0,     0,     0,   112,   265,     0,     0,
     179,     0,   114,     0,     0,     0,     0,     0,   266,     0,
      66,   115,   159,   267,    68,   116,   161,   268,     0,     0,
     117,   118,   119,   120,    87,    39,    88,     0,     0,     0,
       0,    92,     0,    40,    93,     0,     0,     0,     0,     0,
       0,    98,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,   103,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,   105,   178,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     264,     0,     0,   108,     0,     0,     0,     0,     0,   109,
       0,   110,   156,   157,     0,     0,     0,     0,   158,     0,
       0,     0,     0,     0,   112,     0,     0,     0,   179,     0,
     114,     0,     0,     0,     0,     0,   266,     0,    66,   115,
     159,   267,    68,   116,   161,   268,     0,     0,   117,   118,
     119,   120,    87,    39,    88,     0,     0,     0,     0,    92,
       0,    40,    93,     0,     0,     0,     0,     0,     0,    98,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,   103,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,   105,   178,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   108,     0,     0,     0,     0,     0,   109,     0,   110,
     566,     0,     0,     0,     0,     0,     0,     0,     0,   567,
       0,     0,   112,     0,     0,     0,   179,     0,   114,     0,
       0,     0,   568,     0,     0,     0,    66,   115,     0,     0,
      68,   116,     0,    87,    39,    88,   117,   118,   119,   120,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
     372,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,     0,   373,    58,     0,     0,
     103,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,   105,   178,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   108,     0,     0,     0,     0,     0,   109,     0,
     110,   374,     0,     0,     0,     0,     0,     0,     0,     0,
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
       0,   110,   566,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   112,     0,     0,     0,   179,     0,
     114,     0,     0,     0,   568,     0,     0,     0,    66,   115,
       0,     0,    68,   116,     0,    87,    39,    88,   117,   118,
     119,   120,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,   372,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,   103,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,   105,   178,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   108,     0,     0,     0,     0,     0,
     109,     0,   110,   374,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   112,     0,     0,     0,   179,
       0,   114,     0,     0,     0,     0,     0,     0,     0,    66,
     115,     0,     0,    68,   116,     0,    87,    39,    88,   117,
     118,   119,   120,    92,     0,    40,    93,     0,     0,     0,
       0,     0,     0,    98,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,   103,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,   105,   178,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   108,     0,     0,     0,     0,
       0,   109,     0,   110,   566,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   112,     0,     0,     0,
     179,     0,   114,     0,     0,     0,     0,     0,     0,     0,
      66,   115,     0,     0,    68,   116,     0,    87,    39,    88,
     117,   118,   119,   120,    92,     0,    40,    93,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,   103,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,   105,   178,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   108,     0,     0,     0,
       0,     0,   109,     0,   110,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   112,     0,     0,
       0,   179,     0,   114,     0,     0,     0,     0,     0,     0,
       0,    66,   115,     0,     0,    68,   116,     0,    87,    39,
      88,   117,   118,   119,   120,    92,     0,    40,    93,     0,
       0,     0,     0,     0,     0,    98,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,   103,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,   105,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   108,     0,     0,
       0,     0,     0,   109,     0,   110,     0,     0,     0,     0,
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
       0,     0,   412,     0,   109,     0,     0,   257,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   112,
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
       0,     0,     0,     0,     0,   109,     0,     0,   257,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     112,     0,     0,     0,   179,     0,   114,     0,     0,     0,
       0,     0,     0,     0,    66,   115,     0,     0,    68,   116,
       0,    87,    39,    88,   117,   118,   119,   120,    92,     0,
      40,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,   103,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     108,     0,     0,     0,     0,     0,   109,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   112,     0,     0,     0,   179,     0,   114,     0,     0,
       0,     0,     0,     0,     0,    66,   115,     0,     0,    68,
     116,     0,    87,    39,    88,   117,   118,   119,   120,    92,
       0,    40,    93,     0,     0,     0,     0,     0,     0,    98,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   108,     0,     0,     0,     0,     0,   109,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   313,     0,     0,
       0,     0,   112,     0,     0,     0,   179,     0,   114,     0,
       0,     0,     0,     0,     0,     0,    66,   115,     0,     0,
      68,   116,     0,    87,    39,    88,   117,   118,   119,   120,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   108,     0,     0,     0,     0,     0,   109,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   112,     0,     0,    39,   179,     0,   114,
       0,     0,     0,     0,    40,     0,     0,    66,   115,     0,
       0,    68,   116,     0,     0,     0,    42,   117,   118,   119,
     120,   348,     0,    45,    46,    47,   185,   186,   187,     0,
       0,     0,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,     0,     0,     0,     0,     0,     0,   353,     0,
     354,     0,   190,   191,   192,     0,     0,     0,     0,     0,
       0,   193,     0,     0,     0,   194,     0,    39,     0,   195,
     355,   196,     0,     0,     0,    40,   285,   197,     0,   198,
      67,   199,   200,     0,    69,   201,   202,    42,     0,     0,
       0,     0,   348,     0,    45,    46,    47,   185,   186,   187,
       0,     0,     0,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,     0,     0,     0,     0,     0,     0,     0,
       0,   189,     0,   190,   191,   192,     0,     0,     0,     0,
       0,     0,   193,     0,     0,     0,   194,   349,    39,     0,
     195,     0,   196,     0,     0,     0,    40,     0,   197,     0,
     198,    67,   199,   200,     0,    69,   201,   202,    42,     0,
       0,     0,     0,   348,     0,    45,    46,    47,   185,   186,
     187,     0,     0,     0,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,     0,     0,     0,     0,     0,     0,
       0,     0,   189,     0,   190,   191,   192,     0,     0,     0,
       0,     0,     0,   193,     0,     0,     0,   194,     0,    39,
       0,   195,     0,   196,     0,     0,     0,    40,     0,   197,
       0,   198,    67,   199,   200,     0,    69,   201,   202,    42,
       0,     0,     0,     0,     0,     0,    45,    46,    47,   185,
     186,   187,     0,     0,     0,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,     0,     0,     0,     0,     0,
       0,     0,     0,   189,     0,   190,   191,   192,     0,     0,
       0,     0,     0,     0,   193,     0,     0,     0,   194,     0,
      39,     0,   195,   680,   196,     0,     0,     0,    40,     0,
     197,     0,   198,    67,   199,   200,     0,    69,   201,   202,
      42,     0,     0,     0,     0,     0,     0,    45,    46,    47,
     185,   186,   187,     0,     0,     0,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,     0,     0,     0,     0,
       0,     0,     0,     0,   189,     0,   190,   191,   192,     0,
       0,     0,     0,     0,     0,   193,     0,     0,     0,   194,
     419,    39,     0,   195,     0,   196,     0,     0,     0,    40,
       0,   197,     0,   198,    67,   199,   200,     0,    69,   201,
     202,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    55,
      56,    57,     0,     0,    58,     0,    39,     0,    59,    60,
      61,    62,    63,    64,    40,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    42,     0,     0,     0,
       0,     0,     0,    45,    46,    47,   185,   186,   187,     0,
       0,     0,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    66,   115,     0,     0,    68,   116,
       0,   264,     0,     0,     0,     0,     0,     0,     0,     0,
     189,  -133,     0,   191,   192,     0,     0,     0,    39,     0,
       0,   193,     0,     0,     0,   194,    40,     0,     0,   195,
       0,   196,     0,     0,     0,     0,     0,   677,    42,   198,
      67,     0,   267,     0,    69,    45,    46,    47,   185,   186,
     187,     0,     0,     0,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   264,     0,     0,     0,     0,     0,     0,
       0,     0,   189,     0,     0,   191,   192,     0,     0,     0,
       0,     0,     0,   193,     0,     0,     0,   194,    39,     0,
       0,   195,     0,   196,     0,     0,    40,     0,     0,   677,
       0,   198,    67,     0,   267,    41,    69,     0,    42,     0,
      43,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,    39,    51,    52,    53,    54,    55,    56,    57,    40,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,    42,     0,    43,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
      39,     0,     0,     0,     0,     0,     0,     0,    40,     0,
       0,    65,     0,     0,     0,   328,     0,     0,     0,     0,
      42,    66,    67,     0,     0,    68,    69,    45,    46,    47,
     185,   186,   187,     0,     0,     0,    53,    54,    55,    56,
      57,     0,     0,    58,    65,     0,     0,    59,    60,    61,
      62,    63,    64,     0,    66,    67,     0,     0,    68,    69,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   189,     0,     0,   191,   192,     0,
       0,     0,    39,     0,     0,   193,     0,     0,     0,   194,
      40,     0,     0,   195,     0,   196,     0,     0,     0,     0,
       0,     0,    42,   198,    67,     0,     0,     0,    69,    45,
      46,    47,   185,   186,   187,    39,     0,     0,    53,    54,
      55,    56,    57,    40,     0,    58,     0,     0,     0,    59,
      60,    61,    62,    63,    64,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,    39,     0,     0,     0,     0,     0,
       0,     0,    40,     0,     0,     0,     0,     0,     0,   457,
       0,     0,     0,     0,    42,   198,    67,     0,     0,    44,
      69,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,   249,    39,
       0,    59,    60,    61,    62,    63,    64,    40,    66,     0,
       0,     0,    68,     0,     0,     0,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,     0,    51,    52,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,   249,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    66,     0,     0,
       0,    68,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   155,   156,   157,    39,     0,
       0,     0,   158,     0,     0,     0,    40,     0,     0,     0,
       0,     0,   237,     0,     0,     0,     0,     0,    42,     0,
     238,     0,    66,    44,   159,    45,    46,    47,    48,    49,
      50,    39,    51,    52,    53,    54,    55,    56,    57,    40,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,    39,     0,
       0,     0,     0,     0,     0,     0,    40,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    42,     0,
       0,    66,   115,    44,     0,    45,    46,    47,    48,    49,
      50,    39,    51,    52,    53,    54,    55,    56,    57,    40,
       0,    58,     0,     0,   237,    59,    60,    61,    62,    63,
      64,    42,     0,     0,    66,     0,     0,    39,    45,    46,
      47,   185,   186,   187,     0,    40,     0,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,    42,    59,    60,
      61,    62,    63,    64,    45,    46,    47,   185,   186,   187,
      39,     0,     0,    53,    54,    55,    56,    57,    40,     0,
      58,     0,     0,     0,    59,    60,    61,    62,    63,    64,
      42,    66,     0,     0,     0,     0,     0,    45,    46,    47,
     185,   186,   187,     0,   579,     0,    53,    54,    55,    56,
      57,     0,     0,    58,   580,     0,     0,    59,    60,    61,
      62,    63,    64,     0,   198,     0,     0,     0,     0,     0,
     674,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     580,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     198,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   198
  };

  const short
  parser::yycheck_[] =
  {
      29,    89,    29,   100,    41,    89,    88,    65,   107,   204,
     114,    93,   100,   194,   195,   101,   100,   107,   112,   113,
     252,   169,   102,   389,    34,   150,   303,    89,    34,   131,
     112,   113,   483,   110,   181,   555,   334,    12,   112,   113,
     221,     5,   112,   101,   103,   475,     1,     1,     1,   108,
     109,   417,   111,    21,   341,   113,   648,    27,   113,    19,
      65,    77,   415,   340,    77,    79,   131,   299,    88,   107,
     195,   424,   648,   103,   133,   107,   100,   106,   104,   181,
     118,   266,   380,     0,   122,   179,   118,    27,   114,   266,
     122,   195,   196,    77,   108,   101,   394,   179,   128,    77,
     114,   693,   184,   695,    22,   179,   120,   193,   113,   129,
     194,   195,   128,   197,    84,   128,   181,   693,   313,   304,
     204,   179,   303,   270,   179,   209,   131,   304,    80,   580,
     114,    99,   107,    51,    52,   422,   114,   221,   568,   168,
     169,   168,   169,   103,    84,    80,   230,   102,   102,   102,
      80,   671,    79,   100,    78,   363,   663,   113,   113,   113,
      84,    88,   114,    90,   372,   531,   113,   117,   270,   100,
     197,   110,    14,   108,   179,   114,   181,   475,   128,   237,
     687,   275,   113,   128,   114,    81,   367,   193,    84,   107,
     114,   249,   119,   120,   645,   118,   123,   124,   257,   122,
     118,   260,   107,   262,   122,   270,   283,   171,   172,   110,
     108,   295,    27,   114,   302,   396,   114,   246,   302,   303,
     110,   231,   232,    77,   114,   231,   232,   103,   108,   313,
     316,   107,   238,   109,   114,    86,    77,   108,    89,    79,
     302,   299,   118,   114,   249,   110,   599,   600,   108,   114,
      90,    91,    92,    99,   114,   101,   340,    97,   317,    90,
      91,    92,    65,    66,   630,   270,    97,   296,   297,   366,
      80,    81,   549,   550,   627,   115,   574,    60,   366,   119,
     120,   428,   366,   367,    81,   113,   315,    84,   119,   121,
     122,   393,   123,   373,    90,    91,    92,   307,   479,   108,
     676,    97,   668,   679,   485,    99,   487,   101,   489,   408,
     316,    77,   396,   395,    99,   374,   101,   498,   408,   467,
     552,   114,    99,   119,   101,    84,   428,    86,   393,   107,
     412,   425,   414,   427,    34,   107,   430,   113,   432,   103,
     128,   423,   441,   425,    71,   427,    99,   129,   430,   114,
     432,   425,    87,   427,   114,   425,   430,   427,   432,   128,
     430,   108,   432,   428,   108,   108,   643,   108,    79,    80,
      81,   108,   103,    84,   555,   108,   107,    23,   109,    90,
      91,    92,   563,    24,   114,    27,    97,   118,   393,   113,
      89,   122,   474,   475,   480,   479,   482,    77,   579,   483,
     525,   485,    77,   487,   115,   489,    77,    77,   119,   120,
      15,   129,   123,   124,   498,    81,   648,   612,    79,    80,
     502,   525,   526,   428,   506,    72,    73,    74,    75,    90,
      91,    92,    86,   129,   471,   441,    97,    77,   467,   104,
     467,   114,    80,   128,   104,   114,   108,   114,   100,   108,
     108,   108,   108,   648,   115,   108,   128,    77,   119,   120,
      77,   693,   123,   124,   100,   549,   550,   104,   562,   114,
      89,   555,   115,   108,   480,   114,   482,   108,    85,   563,
     562,   676,   115,    85,   679,   114,   568,    25,   562,   115,
     671,   104,   562,    79,   552,   579,   580,   108,   693,   509,
     695,   128,    88,   509,    90,    91,    92,   566,   129,    79,
      80,    97,   128,    77,    77,   108,   598,    77,   115,   601,
      90,    91,    92,   115,   115,   114,    77,    97,   612,    79,
     114,    80,   561,   119,   120,    80,   115,   123,   124,   100,
      90,    91,    92,   114,   626,   115,   628,    97,    81,   119,
     120,   128,   128,   123,   124,    81,   104,   104,   655,   643,
     114,   645,   114,    78,   648,   115,    77,   655,    29,   119,
     120,   655,   115,   123,   124,     9,     4,   114,   677,   100,
     108,   113,    84,   108,   621,    80,   613,   671,   685,    11,
     672,    89,   676,    53,   107,   679,    86,   685,   118,   696,
     114,   685,   108,   108,   108,   108,   168,   689,   696,   693,
     467,   695,   696,   302,   124,    15,   550,   232,   316,   563,
     696,   396,   209,   221,   609,   646,   676,   295,   615,   656,
     482,   656,    34,   417,   663,   509,   663,    34,     3,     4,
       5,     6,     7,     8,   590,    10,   627,    12,    13,   626,
     305,    16,    17,    18,    19,    20,   630,    22,   687,    24,
     687,    26,   561,    28,    29,   260,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
     504,   223,    47,   408,    43,    50,    51,    52,    53,    54,
      55,    56,    57,   409,   552,    79,   101,    62,    63,    -1,
     197,    -1,    86,    -1,    88,    -1,    90,    -1,    -1,    -1,
      -1,    76,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,   108,    90,    91,    -1,    -1,    -1,
     114,    -1,    -1,    -1,    -1,   119,   120,    -1,   103,   123,
     124,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,
      -1,    -1,   127,   128,   129,   130,    -1,     3,     4,     5,
      -1,    -1,   137,   138,    10,    -1,    12,    13,    -1,    -1,
      16,    17,    18,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,
      -1,   127,   128,   129,   130,     1,    -1,     3,     4,     5,
      -1,   137,   138,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   102,   103,    -1,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,   113,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,     3,     4,
       5,   127,   128,   129,   130,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,    -1,
      -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,   107,   108,   109,    -1,    -1,    -1,    -1,   114,
     115,    -1,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,   127,   128,   129,   130,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,
      97,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,    -1,    -1,   115,    -1,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    -1,
     127,   128,   129,   130,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    92,    -1,    -1,    -1,    -1,    97,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,   115,    -1,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,   127,   128,
     129,   130,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,   113,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,     3,     4,     5,   127,   128,   129,   130,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    46,    47,    -1,    -1,
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
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,   113,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,     3,     4,     5,   127,   128,
     129,   130,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,     3,     4,     5,   127,
     128,   129,   130,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,     3,     4,     5,
     127,   128,   129,   130,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,     3,     4,
       5,   127,   128,   129,   130,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    -1,    -1,    -1,    -1,
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
      -1,    -1,    86,    -1,    88,    -1,    -1,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
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
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,     3,     4,     5,   127,   128,   129,   130,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,
     122,    -1,     3,     4,     5,   127,   128,   129,   130,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,     3,     4,     5,   127,   128,   129,   130,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,     4,   107,    -1,   109,
      -1,    -1,    -1,    -1,    12,    -1,    -1,   117,   118,    -1,
      -1,   121,   122,    -1,    -1,    -1,    24,   127,   128,   129,
     130,    29,    -1,    31,    32,    33,    34,    35,    36,    -1,
      -1,    -1,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    86,    -1,
      88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,    -1,   103,    -1,     4,    -1,   107,
     108,   109,    -1,    -1,    -1,    12,   114,   115,    -1,   117,
     118,   119,   120,    -1,   122,   123,   124,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      -1,    -1,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,    -1,   103,   104,     4,    -1,
     107,    -1,   109,    -1,    -1,    -1,    12,    -1,   115,    -1,
     117,   118,   119,   120,    -1,   122,   123,   124,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    -1,    -1,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,   103,    -1,     4,
      -1,   107,    -1,   109,    -1,    -1,    -1,    12,    -1,   115,
      -1,   117,   118,   119,   120,    -1,   122,   123,   124,    24,
      -1,    -1,    -1,    -1,    -1,    -1,    31,    32,    33,    34,
      35,    36,    -1,    -1,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,    -1,
       4,    -1,   107,   108,   109,    -1,    -1,    -1,    12,    -1,
     115,    -1,   117,   118,   119,   120,    -1,   122,   123,   124,
      24,    -1,    -1,    -1,    -1,    -1,    -1,    31,    32,    33,
      34,    35,    36,    -1,    -1,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,
       3,     4,    -1,   107,    -1,   109,    -1,    -1,    -1,    12,
      -1,   115,    -1,   117,   118,   119,   120,    -1,   122,   123,
     124,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    -1,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,     4,    -1,    51,    52,
      53,    54,    55,    56,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    -1,    -1,    31,    32,    33,    34,    35,    36,    -1,
      -1,    -1,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      88,    89,    -1,    91,    92,    -1,    -1,    -1,     4,    -1,
      -1,    99,    -1,    -1,    -1,   103,    12,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,   115,    24,   117,
     118,    -1,   120,    -1,   122,    31,    32,    33,    34,    35,
      36,    -1,    -1,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    -1,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,   103,     4,    -1,
      -1,   107,    -1,   109,    -1,    -1,    12,    -1,    -1,   115,
      -1,   117,   118,    -1,   120,    21,   122,    -1,    24,    -1,
      26,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,     4,    38,    39,    40,    41,    42,    43,    44,    12,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    24,    -1,    26,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    -1,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
       4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,
      -1,   107,    -1,    -1,    -1,    78,    -1,    -1,    -1,    -1,
      24,   117,   118,    -1,    -1,   121,   122,    31,    32,    33,
      34,    35,    36,    -1,    -1,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    47,   107,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    92,    -1,
      -1,    -1,     4,    -1,    -1,    99,    -1,    -1,    -1,   103,
      12,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    24,   117,   118,    -1,    -1,    -1,   122,    31,
      32,    33,    34,    35,    36,     4,    -1,    -1,    40,    41,
      42,    43,    44,    12,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    -1,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    12,    -1,    -1,    -1,    -1,    -1,    -1,    78,
      -1,    -1,    -1,    -1,    24,   117,   118,    -1,    -1,    29,
     122,    31,    32,    33,    34,    35,    36,    -1,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,   107,     4,
      -1,    51,    52,    53,    54,    55,    56,    12,   117,    -1,
      -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    -1,    38,    39,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,    -1,    -1,
      -1,   121,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    91,    92,     4,    -1,
      -1,    -1,    97,    -1,    -1,    -1,    12,    -1,    -1,    -1,
      -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,    24,    -1,
     115,    -1,   117,    29,   119,    31,    32,    33,    34,    35,
      36,     4,    38,    39,    40,    41,    42,    43,    44,    12,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    -1,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,
      -1,   117,   118,    29,    -1,    31,    32,    33,    34,    35,
      36,     4,    38,    39,    40,    41,    42,    43,    44,    12,
      -1,    47,    -1,    -1,   107,    51,    52,    53,    54,    55,
      56,    24,    -1,    -1,   117,    -1,    -1,     4,    31,    32,
      33,    34,    35,    36,    -1,    12,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    24,    51,    52,
      53,    54,    55,    56,    31,    32,    33,    34,    35,    36,
       4,    -1,    -1,    40,    41,    42,    43,    44,    12,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      24,   117,    -1,    -1,    -1,    -1,    -1,    31,    32,    33,
      34,    35,    36,    -1,    97,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    47,   107,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,   117,    -1,    -1,    -1,    -1,    -1,
      97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   117
  };

  const short
  parser::yystos_[] =
  {
       0,    21,    99,   140,   141,   142,   145,   118,   122,   320,
     146,   159,     0,   146,    65,    66,   143,   100,   113,   147,
     160,   161,     1,   102,   319,   103,   128,   190,   190,   107,
     148,    14,   162,   173,   174,   128,   191,    77,    77,     4,
      12,    21,    24,    26,    29,    31,    32,    33,    34,    35,
      36,    38,    39,    40,    41,    42,    43,    44,    47,    51,
      52,    53,    54,    55,    56,   107,   117,   118,   121,   122,
     149,   150,   151,   156,   157,   286,   289,   290,   304,   305,
     306,   312,    27,    60,   163,   113,   158,     3,     5,     6,
       7,     8,    10,    13,    16,    17,    18,    19,    20,    22,
      26,    28,    37,    50,    57,    62,    63,    76,    82,    88,
      90,    91,   103,   107,   109,   118,   122,   127,   128,   129,
     130,   137,   138,   171,   175,   176,   177,   178,   182,   231,
     236,   241,   242,   246,   247,   248,   249,   275,   276,   279,
     280,   303,   304,   306,   314,   315,   318,   104,   114,   320,
     107,   285,   289,    79,    88,    90,    91,    92,    97,   119,
     120,   123,   124,   309,   310,   311,   313,   108,   114,   107,
     152,    99,   101,   144,    77,    34,   164,   113,    63,   107,
     239,   240,   242,   243,   245,    34,    35,    36,    79,    88,
      90,    91,    92,    99,   103,   107,   109,   115,   117,   119,
     120,   123,   124,   184,   198,   199,   202,   204,   206,   208,
     209,   211,   284,   285,   287,   291,   292,   300,   301,   302,
     312,   107,    99,   101,   268,   239,    72,    73,    74,    75,
     179,    99,   101,   187,   188,   204,   206,   107,   115,   294,
     303,   306,   310,   268,   247,   103,   237,   238,   128,   107,
     304,   103,   107,   277,   278,   280,   315,    91,   247,   266,
     267,   247,   246,   247,    79,   104,   115,   120,   124,   239,
     240,   250,   252,   253,   282,   296,   297,   299,   308,   309,
     311,   316,   317,    90,   108,   114,   250,   251,   309,   310,
     311,   316,   321,   110,   321,    19,   237,   237,   129,   170,
     158,    71,   185,    80,   115,   192,   282,   295,   297,   298,
     307,   309,   310,    98,   247,    99,   114,    87,   128,    88,
     291,   108,   108,   108,   108,   108,   108,   151,    78,   153,
     154,   155,   156,   146,   146,    24,   166,   117,   128,    23,
      80,   295,   239,   196,   224,   225,   226,   303,    29,   104,
     200,   202,   204,    86,    88,   108,   200,   215,   291,   321,
     321,   289,   302,    27,   189,   211,    89,    86,   209,   200,
     214,   215,    20,    46,    91,   239,   265,   269,   270,   271,
     269,   113,   244,    77,    77,    77,    77,   194,   200,   212,
     186,   231,   232,   241,   186,    15,    81,   310,   306,   129,
     129,    88,   129,   304,    77,    77,   128,   317,   114,    80,
     193,   247,    86,   266,    81,    84,   233,   234,   235,     3,
     305,   314,   295,    78,    84,   114,   104,   114,   240,   108,
     114,   108,   114,   108,   108,   108,   114,   110,   212,   304,
     304,   115,   172,   281,   293,   294,   317,   128,   184,   194,
     195,   200,   201,   305,   233,   242,   211,    78,   272,   273,
     274,   304,   196,   247,   108,   108,   108,   114,   100,   319,
     128,   165,    77,    77,    99,   101,   257,   194,   243,    80,
     114,   100,   114,   216,   104,    89,   108,    80,   108,   114,
     108,   110,   115,   115,   188,   204,   200,   108,   114,   188,
     268,   247,    85,   100,   113,   319,    25,   189,   100,   113,
     319,   239,   200,   201,   108,   115,   128,   128,   129,   104,
      77,    77,   108,   278,   103,   107,   109,   283,   284,    77,
     239,   239,   255,   256,   271,   189,   235,   115,   115,   115,
     239,   254,   271,   239,   250,   250,   250,   250,    77,    80,
      80,   315,   114,    77,   128,    80,    81,   183,   219,   115,
     100,   114,    81,    80,   155,   320,    91,   100,   113,   239,
     258,   259,   260,   264,   258,   319,   200,   303,   226,    97,
     107,   217,   300,   200,   200,   218,   200,   200,   239,   270,
     239,   232,   128,   128,   104,   108,   110,   189,    81,   114,
     114,    78,   194,   197,   197,   115,   293,    77,   218,    29,
     220,   221,   222,     9,   227,   228,   229,   273,   250,   195,
     114,     4,   167,   247,   258,   100,   113,    84,    86,   261,
     262,   263,   319,   200,   300,   108,   108,   244,   239,   271,
     271,   239,    77,   114,    77,   216,    84,   203,   207,   210,
     211,   223,    22,    51,    52,   107,   180,   230,   288,   289,
     229,   320,    12,   107,   168,   169,   260,   255,   239,   189,
     263,    80,    11,   194,    97,   221,    89,   115,   210,   281,
     108,   204,   205,   213,   230,    53,   181,   107,   149,    86,
     189,   218,   239,   207,   223,   207,   114,   108,   204,   149,
     108,   239,   108,   213,   108
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
     190,   190,   191,   191,   191,   192,   192,   193,   193,   194,
     195,   196,   196,   197,   197,   198,   199,   199,   200,   200,
     200,   201,   202,   203,   204,   204,   205,   206,   207,   207,
     208,   208,   209,   209,   209,   210,   211,   211,   211,   211,
     211,   211,   211,   211,   211,   211,   212,   213,   213,   214,
     214,   215,   215,   216,   216,   217,   217,   218,   219,   220,
     220,   221,   221,   222,   222,   223,   223,   224,   224,   225,
     225,   226,   227,   227,   228,   228,   229,   229,   229,   230,
     230,   230,   231,   231,   231,   232,   233,   233,   234,   234,
     235,   236,   236,   236,   236,   236,   236,   236,   236,   236,
     236,   237,   237,   238,   238,   239,   239,   240,   240,   241,
     241,   242,   242,   242,   243,   243,   244,   244,   245,   245,
     246,   246,   246,   246,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   248,   248,   249,   249,   249,   249,   249,
     249,   249,   250,   250,   250,   251,   251,   252,   252,   252,
     252,   252,   252,   252,   253,   253,   254,   254,   255,   256,
     256,   257,   257,   257,   257,   258,   258,   259,   259,   259,
     260,   261,   261,   262,   262,   263,   264,   264,   265,   265,
     266,   266,   267,   267,   268,   268,   269,   269,   269,   269,
     270,   270,   271,   271,   271,   272,   272,   273,   273,   273,
     274,   274,   275,   275,   276,   276,   277,   277,   277,   278,
     278,   279,   279,   279,   279,   280,   280,   281,   281,   282,
     282,   283,   283,   283,   284,   284,   284,   284,   284,   285,
     285,   285,   286,   286,   286,   286,   286,   287,   287,   288,
     289,   289,   290,   291,   291,   291,   292,   292,   292,   292,
     293,   293,   294,   294,   295,   295,   295,   296,   296,   296,
     297,   298,   298,   299,   299,   300,   301,   302,   302,   302,
     302,   302,   303,   303,   304,   304,   304,   305,   305,   306,
     306,   306,   306,   306,   306,   306,   306,   307,   307,   308,
     308,   309,   310,   310,   311,   311,   312,   312,   312,   312,
     312,   312,   312,   312,   312,   312,   312,   312,   312,   312,
     312,   312,   312,   312,   313,   313,   313,   314,   314,   315,
     316,   316,   317,   317,   318,   318,   318,   318,   319,   319,
     320,   320,   321,   321
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
       1,     3,     3,     1,     0,     0,     2,     0,     2,     1,
       1,     3,     1,     1,     3,     1,     1,     1,     4,     3,
       1,     1,     1,     1,     1,     3,     1,     1,     1,     2,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     2,
       3,     2,     5,     3,     3,     5,     1,     1,     3,     1,
       0,     1,     3,     2,     0,     1,     5,     1,     2,     3,
       1,     4,     2,     3,     0,     1,     3,     0,     1,     3,
       1,     3,     0,     1,     2,     1,     2,     3,     3,     1,
       2,     3,     1,     3,     3,     1,     3,     2,     2,     1,
       4,     3,     5,     3,     4,     4,     3,     4,     6,     6,
       4,     0,     1,     3,     4,     3,     1,     1,     3,     1,
       3,     2,     3,     1,     1,     2,     1,     0,     3,     3,
       2,     3,     2,     1,     3,     2,     4,     4,     8,     4,
       2,     2,     1,     4,     1,     1,     1,     1,     3,     3,
       3,     1,     1,     2,     2,     3,     3,     1,     1,     2,
       4,     3,     5,     3,     3,     3,     3,     1,     1,     3,
       1,     3,     3,     2,     2,     1,     2,     3,     2,     1,
       2,     3,     2,     2,     1,     4,     1,     2,     1,     2,
       1,     2,     2,     1,     3,     3,     3,     2,     1,     0,
       1,     2,     3,     1,     2,     1,     0,     3,     1,     1,
       3,     1,     1,     1,     1,     3,     1,     3,     1,     1,
       3,     2,     3,     2,     3,     1,     2,     1,     3,     1,
       3,     1,     2,     2,     1,     3,     3,     3,     2,     1,
       3,     3,     1,     3,     3,     3,     3,     1,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     1,     1,     1,     1,     1,     1,
       3,     1,     3,     1,     3,     1,     3,     1,     1,     1,
       1,     1,     1,     3,     1,     3,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1
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
  "stringlist", "opt_sig", "opt_tyconsig", "sigtype", "sigtypedoc",
  "sig_vars", "sigtypes1", "strict_mark", "strictness", "ctype",
  "ctypedoc", "context", "context_no_ops", "type", "typedoc", "btype",
  "btype_no_ops", "tyapps", "tyapp", "atype_docs", "atype", "inst_type",
  "deriv_types", "comma_types0", "comma_types1", "tv_bndrs", "tv_bndr",
  "kind", "constrs", "constrs1", "constr", "forall", "constr_stuff",
  "fielddecls", "fielddecls1", "fielddecl", "maybe_derivings", "derivings",
  "deriving", "deriv_clause_types", "decl_no_th", "decl", "rhs", "gdrhs",
  "gdrh", "sigdecl", "activation", "explicit_activation", "exp",
  "infixexp", "infixexp_top", "exp10_top", "exp10", "optSemi", "scc_annot",
  "fexp", "aexp", "aexp1", "aexp2", "texp", "tup_exprs", "list", "lexps",
  "squals", "guardquals", "guardquals1", "altslist", "alts", "alts1",
  "alt", "alt_rhs", "gdpats", "gdpat", "pat", "bindpat", "apat", "apats1",
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
       0,   519,   519,   536,   537,   539,   543,   544,   545,   547,
     548,   550,   551,   554,   556,   557,   558,   566,   567,   569,
     571,   572,   574,   575,   578,   579,   581,   582,   584,   585,
     587,   588,   590,   591,   593,   594,   598,   599,   601,   602,
     604,   606,   607,   609,   618,   619,   621,   622,   624,   625,
     627,   628,   630,   631,   633,   634,   636,   637,   642,   643,
     645,   646,   647,   649,   650,   654,   656,   657,   659,   660,
     661,   664,   671,   673,   674,   675,   676,   677,   679,   681,
     682,   683,   688,   693,   694,   695,   696,   697,   699,   700,
     701,   703,   743,   744,   746,   747,   756,   757,   759,   760,
     761,   805,   806,   807,   808,   810,   811,   813,   815,   816,
     824,   825,   827,   828,   829,   837,   838,   840,   841,   843,
     845,   847,   848,   850,   851,   855,   861,   862,   869,   870,
     872,   874,   883,   885,   887,   888,   890,   893,   895,   896,
     898,   899,   901,   902,   903,   909,   916,   917,   918,   919,
     920,   921,   922,   928,   929,   930,   933,   935,   936,   938,
     939,   941,   942,   949,   950,   952,   953,   971,   977,   979,
     980,   982,   983,   985,   986,   988,   989,   991,   992,   994,
     995,   997,   999,  1000,  1002,  1003,  1005,  1006,  1007,  1009,
    1010,  1011,  1016,  1018,  1020,  1024,  1028,  1029,  1031,  1032,
    1036,  1038,  1039,  1040,  1042,  1043,  1044,  1045,  1046,  1047,
    1048,  1051,  1052,  1054,  1055,  1059,  1060,  1062,  1063,  1065,
    1066,  1068,  1069,  1070,  1072,  1073,  1076,  1077,  1079,  1080,
    1084,  1085,  1086,  1087,  1089,  1090,  1091,  1092,  1094,  1096,
    1097,  1098,  1100,  1102,  1103,  1105,  1106,  1107,  1108,  1109,
    1114,  1115,  1120,  1121,  1122,  1127,  1128,  1146,  1147,  1148,
    1149,  1150,  1151,  1152,  1154,  1155,  1168,  1170,  1180,  1182,
    1183,  1186,  1187,  1188,  1189,  1191,  1192,  1194,  1195,  1196,
    1198,  1200,  1201,  1203,  1204,  1213,  1215,  1216,  1218,  1219,
    1221,  1222,  1224,  1225,  1228,  1229,  1231,  1232,  1233,  1234,
    1239,  1240,  1242,  1243,  1244,  1249,  1250,  1252,  1253,  1254,
    1256,  1257,  1289,  1290,  1292,  1293,  1295,  1296,  1297,  1299,
    1300,  1302,  1303,  1304,  1305,  1307,  1308,  1310,  1311,  1313,
    1314,  1317,  1318,  1319,  1321,  1322,  1323,  1324,  1325,  1327,
    1328,  1329,  1331,  1332,  1333,  1334,  1335,  1338,  1339,  1341,
    1343,  1344,  1348,  1350,  1351,  1352,  1354,  1355,  1356,  1357,
    1362,  1363,  1365,  1366,  1368,  1369,  1370,  1372,  1373,  1374,
    1376,  1378,  1379,  1381,  1382,  1386,  1388,  1390,  1391,  1392,
    1393,  1394,  1397,  1398,  1400,  1401,  1402,  1404,  1405,  1407,
    1408,  1409,  1410,  1411,  1412,  1413,  1414,  1416,  1417,  1419,
    1420,  1422,  1424,  1425,  1427,  1428,  1430,  1431,  1432,  1433,
    1434,  1435,  1436,  1437,  1438,  1439,  1440,  1441,  1442,  1443,
    1444,  1445,  1446,  1447,  1449,  1450,  1451,  1455,  1456,  1458,
    1460,  1461,  1463,  1464,  1468,  1469,  1470,  1471,  1476,  1479,
    1483,  1484,  1486,  1487
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
#line 5829 "parser.cc"

#line 1496 "parser.y"


using std::optional;
using std::string;
using std::vector;

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

Haskell::InstanceDecl make_instance_decl(const Located<expression_ref>& type, const Located<expression_ref>& decls)
{
    return {type, decls};
}

Haskell::TypeSynonymDecl make_type_synonym(const Located<expression_ref>& lhs_type, const Located<expression_ref>& rhs_type)
{
    auto [name, type_args] = check_type_or_class_header(lhs_type);
    return {name, type_args, rhs_type};
}

Haskell::DataOrNewtypeDecl make_data_or_newtype(const Haskell::DataOrNewtype& d_or_n, const Haskell::Context&  context,
                                                const expression_ref& header, const vector<expression_ref>& constrs)
{
    auto [name, type_args] = check_type_or_class_header(header);
    if (d_or_n == Haskell::DataOrNewtype::newtype and constrs.size() != 1)
        throw myexception()<<"newtype '"<<name<<"' may only have 1 constructors with 1 field";
    return {d_or_n, name, type_args, context, constrs};
}

Haskell::ClassDecl make_class_decl(const Haskell::Context& context, const expression_ref& header, const Located<expression_ref>& decls)
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

Located<Haskell::ID> make_id(const yy::location& loc, const string& id)
{
    return Located<Haskell::ID>(loc, {id});
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

Haskell::SimpleRHS make_rhs(const expression_ref& exp, const expression_ref& wherebinds)
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

Haskell::AsPattern make_as_pattern(const Located<Haskell::ID>& x, const expression_ref& pat)
{
    return Haskell::AsPattern(x,pat);
}

Haskell::LazyPattern make_lazy_pattern(const expression_ref& pat)
{
    return Haskell::LazyPattern(pat);
}

expression_ref make_strict_pattern(const expression_ref& pat)
{
    return new expression(AST_node("StrictPattern"), {pat});
}

Haskell::LambdaExp make_lambdaexp(const vector<expression_ref>& pats, const expression_ref& body)
{
    return { pats, body };
}

Haskell::LetExp make_let(const expression_ref& binds, const expression_ref& body)
{
    return { binds, body };
}

expression_ref make_if(const expression_ref& cond, const expression_ref& alt_true, const expression_ref& alt_false)
{
    return new expression(AST_node("If"), {cond, alt_true, alt_false});
}

expression_ref make_case(const expression_ref& obj, const expression_ref& alts)
{
    return new expression(AST_node("Case"), {obj, alts});
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

Haskell::Alts make_alts(const vector<Haskell::Alt>& alts)
{
    return {alts};
}

Haskell::Alt yy_make_alt(const expression_ref& pat, const expression_ref& alt_rhs)
{
    return {pat, alt_rhs};
}

Haskell::MultiGuardedRHS make_gdrhs(const vector<Haskell::GuardedRHS>& guards, const expression_ref& wherebinds)
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

