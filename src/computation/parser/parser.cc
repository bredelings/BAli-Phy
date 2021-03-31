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
      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_transformqual: // transformqual
      case symbol_kind::S_alt: // alt
      case symbol_kind::S_alt_rhs: // alt_rhs
      case symbol_kind::S_ifgdpats: // ifgdpats
      case symbol_kind::S_gdpat: // gdpat
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

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.YY_MOVE_OR_COPY< std::vector<Haskell::FieldDecl> > (YY_MOVE (that.value));
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
      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_altslist: // altslist
      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
      case symbol_kind::S_gdpats: // gdpats
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
      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_transformqual: // transformqual
      case symbol_kind::S_alt: // alt
      case symbol_kind::S_alt_rhs: // alt_rhs
      case symbol_kind::S_ifgdpats: // ifgdpats
      case symbol_kind::S_gdpat: // gdpat
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

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.move< std::vector<Haskell::FieldDecl> > (YY_MOVE (that.value));
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
      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_altslist: // altslist
      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
      case symbol_kind::S_gdpats: // gdpats
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
      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_transformqual: // transformqual
      case symbol_kind::S_alt: // alt
      case symbol_kind::S_alt_rhs: // alt_rhs
      case symbol_kind::S_ifgdpats: // ifgdpats
      case symbol_kind::S_gdpat: // gdpat
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

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.copy< std::vector<Haskell::FieldDecl> > (that.value);
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
      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_altslist: // altslist
      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
      case symbol_kind::S_gdpats: // gdpats
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
      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_transformqual: // transformqual
      case symbol_kind::S_alt: // alt
      case symbol_kind::S_alt_rhs: // alt_rhs
      case symbol_kind::S_ifgdpats: // ifgdpats
      case symbol_kind::S_gdpat: // gdpat
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

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.move< std::vector<Haskell::FieldDecl> > (that.value);
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
      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_altslist: // altslist
      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
      case symbol_kind::S_gdpats: // gdpats
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
      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_transformqual: // transformqual
      case symbol_kind::S_alt: // alt
      case symbol_kind::S_alt_rhs: // alt_rhs
      case symbol_kind::S_ifgdpats: // ifgdpats
      case symbol_kind::S_gdpat: // gdpat
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

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        yylhs.value.emplace< std::vector<Haskell::FieldDecl> > ();
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
      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_altslist: // altslist
      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
      case symbol_kind::S_gdpats: // gdpats
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
#line 1706 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 536 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = make_module(yystack_[4].value.as < std::string > (),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 1712 "parser.cc"
    break;

  case 4: // module: body2
#line 537 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = make_module("Main",{},yystack_[0].value.as < expression_ref > ());}
#line 1718 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 539 "parser.y"
                                                                 {drv.push_module_context();}
#line 1724 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 547 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1730 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 548 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1736 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 550 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1742 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 551 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1748 "parser.cc"
    break;

  case 13: // top: semis top1
#line 554 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1754 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 556 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 1760 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 557 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 1766 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 558 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[0].value.as < std::vector<expression_ref> > (),{});}
#line 1772 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 566 "parser.y"
                                      {yylhs.value.as < expression_ref > () = make_exports(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 1778 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 567 "parser.y"
                                      {}
#line 1784 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 569 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 1790 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 571 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1796 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 572 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1802 "parser.cc"
    break;

  case 22: // export: qcname_ext export_subspec
#line 574 "parser.y"
                                      {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1808 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 575 "parser.y"
                                      {yylhs.value.as < expression_ref > () = AST_node("module",yystack_[0].value.as < std::string > ());}
#line 1814 "parser.cc"
    break;

  case 26: // qcnames: %empty
#line 581 "parser.y"
                   {}
#line 1820 "parser.cc"
    break;

  case 27: // qcnames: qcnames1
#line 582 "parser.y"
                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 1826 "parser.cc"
    break;

  case 28: // qcnames1: qcnames1 "," qcname_ext_w_wildcard ","
#line 584 "parser.y"
                                                  {yylhs.value.as < std::vector<expression_ref> > () = yystack_[3].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ());}
#line 1832 "parser.cc"
    break;

  case 29: // qcnames1: qcname_ext_w_wildcard
#line 585 "parser.y"
                                              {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1838 "parser.cc"
    break;

  case 30: // qcname_ext_w_wildcard: qcname_ext
#line 587 "parser.y"
                                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1844 "parser.cc"
    break;

  case 31: // qcname_ext_w_wildcard: ".."
#line 588 "parser.y"
                                     {}
#line 1850 "parser.cc"
    break;

  case 32: // qcname_ext: qcname
#line 590 "parser.y"
                                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1856 "parser.cc"
    break;

  case 33: // qcname_ext: "type" oqtycon
#line 591 "parser.y"
                                     {}
#line 1862 "parser.cc"
    break;

  case 34: // qcname: qvar
#line 593 "parser.y"
                                     {yylhs.value.as < expression_ref > () = AST_node("qvar",yystack_[0].value.as < std::string > ()); }
#line 1868 "parser.cc"
    break;

  case 35: // qcname: oqtycon_no_varcon
#line 594 "parser.y"
                                     {yylhs.value.as < expression_ref > () = AST_node("qvar",yystack_[0].value.as < std::string > ()); }
#line 1874 "parser.cc"
    break;

  case 40: // importdecls: importdecls_semi importdecl
#line 604 "parser.y"
                                         { yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (), yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 1880 "parser.cc"
    break;

  case 41: // importdecls_semi: importdecls_semi importdecl semis1
#line 606 "parser.y"
                                                     { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 1886 "parser.cc"
    break;

  case 42: // importdecls_semi: %empty
#line 607 "parser.y"
                         { }
#line 1892 "parser.cc"
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
#line 1905 "parser.cc"
    break;

  case 44: // maybe_src: "{-# SOURCE" "#-}"
#line 618 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 1911 "parser.cc"
    break;

  case 45: // maybe_src: %empty
#line 619 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 1917 "parser.cc"
    break;

  case 46: // maybe_safe: "safe"
#line 621 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 1923 "parser.cc"
    break;

  case 47: // maybe_safe: %empty
#line 622 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 1929 "parser.cc"
    break;

  case 48: // maybe_pkg: "STRING"
#line 624 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 1935 "parser.cc"
    break;

  case 49: // maybe_pkg: %empty
#line 625 "parser.y"
                               { }
#line 1941 "parser.cc"
    break;

  case 50: // optqualified: "qualified"
#line 627 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 1947 "parser.cc"
    break;

  case 51: // optqualified: %empty
#line 628 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 1953 "parser.cc"
    break;

  case 52: // maybeas: "as" modid
#line 630 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 1959 "parser.cc"
    break;

  case 53: // maybeas: %empty
#line 631 "parser.y"
                               { }
#line 1965 "parser.cc"
    break;

  case 54: // maybeimpspec: impspec
#line 633 "parser.y"
                               { yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > (); }
#line 1971 "parser.cc"
    break;

  case 55: // maybeimpspec: %empty
#line 634 "parser.y"
                               { }
#line 1977 "parser.cc"
    break;

  case 56: // impspec: "(" exportlist ")"
#line 636 "parser.y"
                                      { yylhs.value.as < expression_ref > () = expression_ref{AST_node("only"),yystack_[1].value.as < std::vector<expression_ref> > ()}; }
#line 1983 "parser.cc"
    break;

  case 57: // impspec: "hiding" "(" exportlist ")"
#line 637 "parser.y"
                                      { yylhs.value.as < expression_ref > () = expression_ref{AST_node("hiding"),yystack_[1].value.as < std::vector<expression_ref> > ()}; }
#line 1989 "parser.cc"
    break;

  case 58: // prec: %empty
#line 642 "parser.y"
                   { }
#line 1995 "parser.cc"
    break;

  case 59: // prec: "INTEGER"
#line 643 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < int > (); }
#line 2001 "parser.cc"
    break;

  case 60: // infix: "infix"
#line 645 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infix; }
#line 2007 "parser.cc"
    break;

  case 61: // infix: "infixl"
#line 646 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infixl; }
#line 2013 "parser.cc"
    break;

  case 62: // infix: "infixr"
#line 647 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infixr; }
#line 2019 "parser.cc"
    break;

  case 63: // ops: ops "," op
#line 649 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 2025 "parser.cc"
    break;

  case 64: // ops: op
#line 650 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 2031 "parser.cc"
    break;

  case 65: // topdecls: topdecls_semi topdecl
#line 654 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 2037 "parser.cc"
    break;

  case 66: // topdecls_semi: topdecls_semi topdecl semis1
#line 656 "parser.y"
                                            { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2043 "parser.cc"
    break;

  case 67: // topdecls_semi: %empty
#line 657 "parser.y"
                                            { }
#line 2049 "parser.cc"
    break;

  case 68: // topdecl: cl_decl
#line 659 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2055 "parser.cc"
    break;

  case 69: // topdecl: ty_decl
#line 660 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2061 "parser.cc"
    break;

  case 70: // topdecl: inst_decl
#line 661 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2067 "parser.cc"
    break;

  case 71: // topdecl: "default" "(" comma_types0 ")"
#line 664 "parser.y"
                                               {}
#line 2073 "parser.cc"
    break;

  case 72: // topdecl: decl_no_th
#line 671 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2079 "parser.cc"
    break;

  case 73: // topdecl: infixexp_top
#line 673 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2085 "parser.cc"
    break;

  case 74: // topdecl: "builtin" var "INTEGER" "STRING" "STRING"
#line 674 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2091 "parser.cc"
    break;

  case 75: // topdecl: "builtin" var "INTEGER" "STRING"
#line 675 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2097 "parser.cc"
    break;

  case 76: // topdecl: "builtin" varop "INTEGER" "STRING" "STRING"
#line 676 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2103 "parser.cc"
    break;

  case 77: // topdecl: "builtin" varop "INTEGER" "STRING"
#line 677 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2109 "parser.cc"
    break;

  case 78: // cl_decl: "class" tycl_hdr wherebinds
#line 679 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_class_decl(yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().second,{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2115 "parser.cc"
    break;

  case 79: // ty_decl: "type" type "=" ctypedoc
#line 681 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_type_synonym({yystack_[2].location,yystack_[2].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2121 "parser.cc"
    break;

  case 80: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 682 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < Haskell::DataOrNewtype > (),yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().second,yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2127 "parser.cc"
    break;

  case 81: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig
#line 683 "parser.y"
                                                                           {}
#line 2133 "parser.cc"
    break;

  case 82: // inst_decl: "instance" overlap_pragma inst_type wherebinds
#line 688 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_instance_decl({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2139 "parser.cc"
    break;

  case 92: // data_or_newtype: "data"
#line 743 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::data;}
#line 2145 "parser.cc"
    break;

  case 93: // data_or_newtype: "newtype"
#line 744 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::newtype;}
#line 2151 "parser.cc"
    break;

  case 96: // tycl_hdr: context "=>" type
#line 756 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ()};}
#line 2157 "parser.cc"
    break;

  case 97: // tycl_hdr: type
#line 757 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {{},yystack_[0].value.as < expression_ref > ()};}
#line 2163 "parser.cc"
    break;

  case 101: // decls: decls ";" decl
#line 805 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2169 "parser.cc"
    break;

  case 102: // decls: decls ";"
#line 806 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2175 "parser.cc"
    break;

  case 103: // decls: decl
#line 807 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2181 "parser.cc"
    break;

  case 104: // decls: %empty
#line 808 "parser.y"
                        {}
#line 2187 "parser.cc"
    break;

  case 105: // decllist: "{" decls "}"
#line 810 "parser.y"
                                 {yylhs.value.as < expression_ref > () = expression_ref{AST_node("Decls"),yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 2193 "parser.cc"
    break;

  case 106: // decllist: "vocurly" decls close
#line 811 "parser.y"
                                 {yylhs.value.as < expression_ref > () = expression_ref{AST_node("Decls"),yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 2199 "parser.cc"
    break;

  case 107: // binds: decllist
#line 813 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2205 "parser.cc"
    break;

  case 108: // wherebinds: "where" binds
#line 815 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2211 "parser.cc"
    break;

  case 109: // wherebinds: %empty
#line 816 "parser.y"
                                 {}
#line 2217 "parser.cc"
    break;

  case 115: // opt_sig: %empty
#line 837 "parser.y"
                 {}
#line 2223 "parser.cc"
    break;

  case 116: // opt_sig: "::" sigtype
#line 838 "parser.y"
                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2229 "parser.cc"
    break;

  case 117: // opt_tyconsig: %empty
#line 840 "parser.y"
                     {}
#line 2235 "parser.cc"
    break;

  case 118: // opt_tyconsig: "::" gtycon
#line 841 "parser.y"
                     {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2241 "parser.cc"
    break;

  case 119: // sigtype: ctype
#line 843 "parser.y"
                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2247 "parser.cc"
    break;

  case 120: // sigtypedoc: ctypedoc
#line 845 "parser.y"
                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2253 "parser.cc"
    break;

  case 121: // sig_vars: sig_vars "," var
#line 847 "parser.y"
                           {yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 2259 "parser.cc"
    break;

  case 122: // sig_vars: var
#line 848 "parser.y"
                           {yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 2265 "parser.cc"
    break;

  case 123: // sigtypes1: sigtype
#line 850 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2271 "parser.cc"
    break;

  case 124: // sigtypes1: sigtypes1 "," sigtype
#line 851 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2277 "parser.cc"
    break;

  case 125: // strict_mark: strictness
#line 855 "parser.y"
                                            {yylhs.value.as < Haskell::StrictLazy > () = yystack_[0].value.as < Haskell::StrictLazy > ();}
#line 2283 "parser.cc"
    break;

  case 126: // strictness: "!"
#line 861 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::strict;}
#line 2289 "parser.cc"
    break;

  case 127: // strictness: "~"
#line 862 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::lazy;}
#line 2295 "parser.cc"
    break;

  case 128: // ctype: "forall" tv_bndrs "." ctype
#line 869 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_forall_type(yystack_[2].value.as < std::vector<expression_ref> > (), yystack_[0].value.as < expression_ref > ());}
#line 2301 "parser.cc"
    break;

  case 129: // ctype: context "=>" ctype
#line 870 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_constrained_type(yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ());}
#line 2307 "parser.cc"
    break;

  case 130: // ctype: type
#line 872 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2313 "parser.cc"
    break;

  case 131: // ctypedoc: ctype
#line 874 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2319 "parser.cc"
    break;

  case 132: // context: btype
#line 883 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(yystack_[0].value.as < expression_ref > ());}
#line 2325 "parser.cc"
    break;

  case 133: // context_no_ops: btype_no_ops
#line 885 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2331 "parser.cc"
    break;

  case 134: // type: btype
#line 887 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2337 "parser.cc"
    break;

  case 135: // type: btype "->" ctype
#line 888 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps({make_type_var("->"),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()});}
#line 2343 "parser.cc"
    break;

  case 136: // typedoc: type
#line 890 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2349 "parser.cc"
    break;

  case 137: // btype: tyapps
#line 893 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2355 "parser.cc"
    break;

  case 138: // btype_no_ops: atype_docs
#line 895 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2361 "parser.cc"
    break;

  case 139: // btype_no_ops: btype_no_ops atype_docs
#line 896 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2367 "parser.cc"
    break;

  case 140: // tyapps: tyapp
#line 898 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2373 "parser.cc"
    break;

  case 141: // tyapps: tyapps tyapp
#line 899 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2379 "parser.cc"
    break;

  case 142: // tyapp: atype
#line 901 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2385 "parser.cc"
    break;

  case 143: // tyapp: qtyconop
#line 902 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2391 "parser.cc"
    break;

  case 144: // tyapp: tyvarop
#line 903 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2397 "parser.cc"
    break;

  case 145: // atype_docs: atype
#line 909 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2403 "parser.cc"
    break;

  case 146: // atype: ntgtycon
#line 916 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2409 "parser.cc"
    break;

  case 147: // atype: tyvar
#line 917 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2415 "parser.cc"
    break;

  case 148: // atype: "*"
#line 918 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var("*");}
#line 2421 "parser.cc"
    break;

  case 149: // atype: strict_mark atype
#line 919 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_strict_lazy_type(yystack_[1].value.as < Haskell::StrictLazy > (),yystack_[0].value.as < expression_ref > ());}
#line 2427 "parser.cc"
    break;

  case 150: // atype: "{" fielddecls "}"
#line 920 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_field_decls(yystack_[1].value.as < std::vector<Haskell::FieldDecl> > ());}
#line 2433 "parser.cc"
    break;

  case 151: // atype: "(" ")"
#line 921 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var("()");}
#line 2439 "parser.cc"
    break;

  case 152: // atype: "(" comma_types1 "," ctype ")"
#line 922 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<expression_ref> > ();ts.push_back(yystack_[1].value.as < expression_ref > ());yylhs.value.as < expression_ref > () = make_tuple_type(ts);}
#line 2445 "parser.cc"
    break;

  case 153: // atype: "[" ctype "]"
#line 928 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_list_type(yystack_[1].value.as < expression_ref > ());}
#line 2451 "parser.cc"
    break;

  case 154: // atype: "(" ctype ")"
#line 929 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2457 "parser.cc"
    break;

  case 155: // atype: "(" ctype "::" kind ")"
#line 930 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_of_kind(yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ());}
#line 2463 "parser.cc"
    break;

  case 156: // inst_type: sigtype
#line 933 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2469 "parser.cc"
    break;

  case 159: // comma_types0: comma_types1
#line 938 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2475 "parser.cc"
    break;

  case 160: // comma_types0: %empty
#line 939 "parser.y"
                                       { /* default construction OK */ }
#line 2481 "parser.cc"
    break;

  case 161: // comma_types1: ctype
#line 941 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2487 "parser.cc"
    break;

  case 162: // comma_types1: comma_types1 "," ctype
#line 942 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2493 "parser.cc"
    break;

  case 163: // tv_bndrs: tv_bndrs tv_bndr
#line 949 "parser.y"
                               {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2499 "parser.cc"
    break;

  case 164: // tv_bndrs: %empty
#line 950 "parser.y"
                               { /* default construction OK */}
#line 2505 "parser.cc"
    break;

  case 165: // tv_bndr: tyvar
#line 952 "parser.y"
                                    {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2511 "parser.cc"
    break;

  case 166: // tv_bndr: "(" tyvar "::" kind ")"
#line 953 "parser.y"
                                    {yylhs.value.as < expression_ref > () = make_type_var_of_kind(yystack_[3].value.as < std::string > (),yystack_[1].value.as < expression_ref > ());}
#line 2517 "parser.cc"
    break;

  case 167: // kind: ctype
#line 971 "parser.y"
             {yylhs.value.as < expression_ref > () = make_kind(yystack_[0].value.as < expression_ref > ());}
#line 2523 "parser.cc"
    break;

  case 168: // constrs: "=" constrs1
#line 977 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2529 "parser.cc"
    break;

  case 169: // constrs1: constrs1 "|" constr
#line 979 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2535 "parser.cc"
    break;

  case 170: // constrs1: constr
#line 980 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2541 "parser.cc"
    break;

  case 171: // constr: forall context_no_ops "=>" constr_stuff
#line 982 "parser.y"
                                                {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2547 "parser.cc"
    break;

  case 172: // constr: forall constr_stuff
#line 983 "parser.y"
                                                {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2553 "parser.cc"
    break;

  case 173: // forall: "forall" tv_bndrs "."
#line 985 "parser.y"
                                {if (yystack_[1].value.as < std::vector<expression_ref> > ().size()>1) yylhs.value.as < expression_ref > () = make_tv_bndrs(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2559 "parser.cc"
    break;

  case 174: // forall: %empty
#line 986 "parser.y"
                                {}
#line 2565 "parser.cc"
    break;

  case 175: // constr_stuff: btype_no_ops
#line 988 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2571 "parser.cc"
    break;

  case 176: // constr_stuff: btype_no_ops conop btype_no_ops
#line 989 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps({make_type_var(yystack_[1].value.as < std::string > ()),make_tyapps(yystack_[2].value.as < std::vector<expression_ref> > ()),make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 2577 "parser.cc"
    break;

  case 177: // fielddecls: %empty
#line 991 "parser.y"
                                {}
#line 2583 "parser.cc"
    break;

  case 178: // fielddecls: fielddecls1
#line 992 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::FieldDecl> > () = yystack_[0].value.as < std::vector<Haskell::FieldDecl> > ();}
#line 2589 "parser.cc"
    break;

  case 179: // fielddecls1: fielddecls1 "," fielddecl
#line 994 "parser.y"
                                        {yylhs.value.as < std::vector<Haskell::FieldDecl> > () = yystack_[2].value.as < std::vector<Haskell::FieldDecl> > (); yylhs.value.as < std::vector<Haskell::FieldDecl> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2595 "parser.cc"
    break;

  case 180: // fielddecls1: fielddecl
#line 995 "parser.y"
                                        {yylhs.value.as < std::vector<Haskell::FieldDecl> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2601 "parser.cc"
    break;

  case 181: // fielddecl: sig_vars "::" ctype
#line 997 "parser.y"
                                        {yylhs.value.as < Haskell::FieldDecl > () = make_field_decl(yystack_[2].value.as < std::vector<std::string> > (),yystack_[0].value.as < expression_ref > ());}
#line 2607 "parser.cc"
    break;

  case 192: // decl_no_th: sigdecl
#line 1016 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2613 "parser.cc"
    break;

  case 193: // decl_no_th: "!" aexp rhs
#line 1018 "parser.y"
                              {yylhs.value.as < expression_ref > () = new expression(AST_node("Decl:Strict"),{(yystack_[1].value.as < expression_ref > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2619 "parser.cc"
    break;

  case 194: // decl_no_th: infixexp_top opt_sig rhs
#line 1020 "parser.y"
                              {yylhs.value.as < expression_ref > () = new expression(AST_node("Decl"),{make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2625 "parser.cc"
    break;

  case 195: // decl: decl_no_th
#line 1024 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2631 "parser.cc"
    break;

  case 196: // rhs: "=" exp wherebinds
#line 1028 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_rhs(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2637 "parser.cc"
    break;

  case 197: // rhs: gdrhs wherebinds
#line 1029 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2643 "parser.cc"
    break;

  case 198: // gdrhs: gdrhs gdrh
#line 1031 "parser.y"
                              {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2649 "parser.cc"
    break;

  case 199: // gdrhs: gdrh
#line 1032 "parser.y"
                              {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2655 "parser.cc"
    break;

  case 200: // gdrh: "|" guardquals "=" exp
#line 1036 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2661 "parser.cc"
    break;

  case 201: // sigdecl: infixexp_top "::" sigtypedoc
#line 1038 "parser.y"
                                             { yylhs.value.as < expression_ref > () = expression_ref(AST_node("Decl:sigtype"),{make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2667 "parser.cc"
    break;

  case 202: // sigdecl: var "," sig_vars "::" sigtypedoc
#line 1039 "parser.y"
                                          {}
#line 2673 "parser.cc"
    break;

  case 203: // sigdecl: infix prec ops
#line 1040 "parser.y"
                         { yylhs.value.as < expression_ref > () = make_fixity_decl(yystack_[2].value.as < Haskell::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()); }
#line 2679 "parser.cc"
    break;

  case 204: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1042 "parser.y"
                                                    {}
#line 2685 "parser.cc"
    break;

  case 205: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1043 "parser.y"
                                            {}
#line 2691 "parser.cc"
    break;

  case 206: // sigdecl: "{-# SCC" qvar "#-}"
#line 1044 "parser.y"
                              {}
#line 2697 "parser.cc"
    break;

  case 207: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1045 "parser.y"
                                     {}
#line 2703 "parser.cc"
    break;

  case 208: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1046 "parser.y"
                                                               {}
#line 2709 "parser.cc"
    break;

  case 209: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1047 "parser.y"
                                                                      {}
#line 2715 "parser.cc"
    break;

  case 210: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1048 "parser.y"
                                                     {}
#line 2721 "parser.cc"
    break;

  case 215: // exp: infixexp "::" sigtype
#line 1059 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_typed_exp(make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()); }
#line 2727 "parser.cc"
    break;

  case 216: // exp: infixexp
#line 1060 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 2733 "parser.cc"
    break;

  case 217: // infixexp: exp10
#line 1062 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2739 "parser.cc"
    break;

  case 218: // infixexp: infixexp qop exp10
#line 1063 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_id(yystack_[1].location,yystack_[1].value.as < std::string > ())); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2745 "parser.cc"
    break;

  case 219: // infixexp_top: exp10_top
#line 1065 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2751 "parser.cc"
    break;

  case 220: // infixexp_top: infixexp_top qop exp10_top
#line 1066 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_id(yystack_[1].location,yystack_[1].value.as < std::string > ())); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2757 "parser.cc"
    break;

  case 221: // exp10_top: "-" fexp
#line 1068 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_minus(make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2763 "parser.cc"
    break;

  case 222: // exp10_top: "{-# CORE" "STRING" "#-}"
#line 1069 "parser.y"
                                   {}
#line 2769 "parser.cc"
    break;

  case 223: // exp10_top: fexp
#line 1070 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2775 "parser.cc"
    break;

  case 224: // exp10: exp10_top
#line 1072 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2781 "parser.cc"
    break;

  case 225: // exp10: scc_annot exp
#line 1073 "parser.y"
                                 {}
#line 2787 "parser.cc"
    break;

  case 230: // fexp: fexp aexp
#line 1084 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2793 "parser.cc"
    break;

  case 231: // fexp: fexp "TYPEAPP" atype
#line 1085 "parser.y"
                                 {}
#line 2799 "parser.cc"
    break;

  case 232: // fexp: "static" aexp
#line 1086 "parser.y"
                                 {}
#line 2805 "parser.cc"
    break;

  case 233: // fexp: aexp
#line 1087 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2811 "parser.cc"
    break;

  case 234: // aexp: qvar "@" aexp
#line 1089 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_as_pattern(make_id(yystack_[2].location,yystack_[2].value.as < std::string > ()),yystack_[0].value.as < expression_ref > ());}
#line 2817 "parser.cc"
    break;

  case 235: // aexp: "~" aexp
#line 1090 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lazy_pattern(yystack_[0].value.as < expression_ref > ());}
#line 2823 "parser.cc"
    break;

  case 236: // aexp: "\\" apats1 "->" exp
#line 1091 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lambda(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2829 "parser.cc"
    break;

  case 237: // aexp: "let" binds "in" exp
#line 1092 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_let(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2835 "parser.cc"
    break;

  case 238: // aexp: "\\" "case" altslist
#line 1093 "parser.y"
                                 {}
#line 2841 "parser.cc"
    break;

  case 239: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1094 "parser.y"
                                                       {yylhs.value.as < expression_ref > () = make_if(yystack_[6].value.as < expression_ref > (),yystack_[3].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2847 "parser.cc"
    break;

  case 240: // aexp: "if" ifgdpats
#line 1095 "parser.y"
                                 {}
#line 2853 "parser.cc"
    break;

  case 241: // aexp: "case" exp "of" altslist
#line 1096 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_case(yystack_[2].value.as < expression_ref > (),make_alts(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2859 "parser.cc"
    break;

  case 242: // aexp: "do" stmtlist
#line 1097 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_do(yystack_[0].value.as < Haskell::Stmts > ());}
#line 2865 "parser.cc"
    break;

  case 243: // aexp: "mdo" stmtlist
#line 1098 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_mdo(yystack_[0].value.as < Haskell::Stmts > ());}
#line 2871 "parser.cc"
    break;

  case 244: // aexp: "proc" aexp "->" exp
#line 1099 "parser.y"
                                 {}
#line 2877 "parser.cc"
    break;

  case 245: // aexp: aexp1
#line 1100 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2883 "parser.cc"
    break;

  case 246: // aexp1: aexp1 "{" fbinds "}"
#line 1102 "parser.y"
                              {}
#line 2889 "parser.cc"
    break;

  case 247: // aexp1: aexp2
#line 1103 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2895 "parser.cc"
    break;

  case 248: // aexp2: qvar
#line 1105 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_id(yystack_[0].location,yystack_[0].value.as < std::string > ());}
#line 2901 "parser.cc"
    break;

  case 249: // aexp2: qcon
#line 1106 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_id(yystack_[0].location,yystack_[0].value.as < std::string > ());}
#line 2907 "parser.cc"
    break;

  case 250: // aexp2: literal
#line 1107 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2913 "parser.cc"
    break;

  case 251: // aexp2: "(" texp ")"
#line 1108 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2919 "parser.cc"
    break;

  case 252: // aexp2: "(" tup_exprs ")"
#line 1109 "parser.y"
                              {yylhs.value.as < expression_ref > () = yy_make_tuple(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2925 "parser.cc"
    break;

  case 253: // aexp2: "(#" texp "#)"
#line 1110 "parser.y"
                              {}
#line 2931 "parser.cc"
    break;

  case 254: // aexp2: "(#" tup_exprs "#)"
#line 1111 "parser.y"
                              {}
#line 2937 "parser.cc"
    break;

  case 255: // aexp2: "[" list "]"
#line 1112 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2943 "parser.cc"
    break;

  case 256: // aexp2: "_"
#line 1113 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::WildcardPattern();}
#line 2949 "parser.cc"
    break;

  case 257: // texp: exp
#line 1118 "parser.y"
                      {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2955 "parser.cc"
    break;

  case 258: // texp: infixexp qop
#line 1119 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("LeftSection"),{make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ()),make_id(yystack_[0].location,yystack_[0].value.as < std::string > ())});}
#line 2961 "parser.cc"
    break;

  case 259: // texp: qopm infixexp
#line 1120 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("RightSection"),{make_id(yystack_[1].location,yystack_[1].value.as < std::string > ()),make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 2967 "parser.cc"
    break;

  case 260: // tup_exprs: tup_exprs "," texp
#line 1125 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2973 "parser.cc"
    break;

  case 261: // tup_exprs: texp "," texp
#line 1126 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2979 "parser.cc"
    break;

  case 262: // list: texp
#line 1144 "parser.y"
                                 { yylhs.value.as < expression_ref > () = make_list({yystack_[0].value.as < expression_ref > ()}); }
#line 2985 "parser.cc"
    break;

  case 263: // list: lexps
#line 1145 "parser.y"
                                 { yylhs.value.as < expression_ref > () = make_list(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 2991 "parser.cc"
    break;

  case 264: // list: texp ".."
#line 1146 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFrom"),{yystack_[1].value.as < expression_ref > ()}); }
#line 2997 "parser.cc"
    break;

  case 265: // list: texp "," exp ".."
#line 1147 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThen"),{yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()}); }
#line 3003 "parser.cc"
    break;

  case 266: // list: texp ".." exp
#line 1148 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromTo"),{yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 3009 "parser.cc"
    break;

  case 267: // list: texp "," exp ".." exp
#line 1149 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThenTo"),{yystack_[4].value.as < expression_ref > (),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 3015 "parser.cc"
    break;

  case 268: // list: texp "|" squals
#line 1150 "parser.y"
                                 { auto quals = yystack_[0].value.as < std::vector<expression_ref> > (); quals.push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < expression_ref > () = expression_ref(AST_node("ListComprehension"),quals); }
#line 3021 "parser.cc"
    break;

  case 269: // lexps: lexps "," texp
#line 1152 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3027 "parser.cc"
    break;

  case 270: // lexps: texp "," texp
#line 1153 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3033 "parser.cc"
    break;

  case 271: // squals: squals "," transformqual
#line 1165 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3039 "parser.cc"
    break;

  case 272: // squals: squals "," qual
#line 1166 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3045 "parser.cc"
    break;

  case 273: // squals: transformqual
#line 1167 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3051 "parser.cc"
    break;

  case 274: // squals: qual
#line 1168 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3057 "parser.cc"
    break;

  case 275: // transformqual: "then" exp
#line 1170 "parser.y"
                                                    {}
#line 3063 "parser.cc"
    break;

  case 276: // transformqual: "then" exp "by" exp
#line 1171 "parser.y"
                                                    {}
#line 3069 "parser.cc"
    break;

  case 277: // transformqual: "then" "group" "using" exp
#line 1172 "parser.y"
                                                    {}
#line 3075 "parser.cc"
    break;

  case 278: // transformqual: "then" "group" "by" exp "using" exp
#line 1173 "parser.y"
                                                    {}
#line 3081 "parser.cc"
    break;

  case 279: // guardquals: guardquals1
#line 1176 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3087 "parser.cc"
    break;

  case 280: // guardquals1: guardquals1 "," qual
#line 1178 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > ();yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3093 "parser.cc"
    break;

  case 281: // guardquals1: qual
#line 1179 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3099 "parser.cc"
    break;

  case 282: // altslist: "{" alts "}"
#line 1182 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3105 "parser.cc"
    break;

  case 283: // altslist: "vocurly" alts close
#line 1183 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3111 "parser.cc"
    break;

  case 284: // altslist: "{" "}"
#line 1184 "parser.y"
                                 {}
#line 3117 "parser.cc"
    break;

  case 285: // altslist: "vocurly" close
#line 1185 "parser.y"
                                 {}
#line 3123 "parser.cc"
    break;

  case 286: // alts: alts1
#line 1187 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3129 "parser.cc"
    break;

  case 287: // alts: ";" alts
#line 1188 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3135 "parser.cc"
    break;

  case 288: // alts1: alts1 ";" alt
#line 1190 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3141 "parser.cc"
    break;

  case 289: // alts1: alts1 ";"
#line 1191 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3147 "parser.cc"
    break;

  case 290: // alts1: alt
#line 1192 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3153 "parser.cc"
    break;

  case 291: // alt: pat alt_rhs
#line 1194 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yy_make_alt(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3159 "parser.cc"
    break;

  case 292: // alt_rhs: "->" exp wherebinds
#line 1196 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_rhs(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3165 "parser.cc"
    break;

  case 293: // alt_rhs: gdpats wherebinds
#line 1197 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3171 "parser.cc"
    break;

  case 294: // gdpats: gdpats gdpat
#line 1199 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3177 "parser.cc"
    break;

  case 295: // gdpats: gdpat
#line 1200 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3183 "parser.cc"
    break;

  case 296: // ifgdpats: "{" gdpats "}"
#line 1202 "parser.y"
                                 {}
#line 3189 "parser.cc"
    break;

  case 297: // ifgdpats: gdpats close
#line 1203 "parser.y"
                                 {}
#line 3195 "parser.cc"
    break;

  case 298: // gdpat: "|" guardquals "->" exp
#line 1205 "parser.y"
                                 {yylhs.value.as < expression_ref > ()=make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3201 "parser.cc"
    break;

  case 299: // pat: exp
#line 1207 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3207 "parser.cc"
    break;

  case 300: // pat: "!" aexp
#line 1208 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3213 "parser.cc"
    break;

  case 301: // bindpat: exp
#line 1210 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3219 "parser.cc"
    break;

  case 302: // bindpat: "!" aexp
#line 1211 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3225 "parser.cc"
    break;

  case 303: // apat: aexp
#line 1213 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3231 "parser.cc"
    break;

  case 304: // apat: "!" aexp
#line 1214 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3237 "parser.cc"
    break;

  case 305: // apats1: apats1 apat
#line 1216 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3243 "parser.cc"
    break;

  case 306: // apats1: apat
#line 1217 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3249 "parser.cc"
    break;

  case 307: // stmtlist: "{" stmts "}"
#line 1220 "parser.y"
                               {yylhs.value.as < Haskell::Stmts > () = make_stmts(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3255 "parser.cc"
    break;

  case 308: // stmtlist: "vocurly" stmts close
#line 1221 "parser.y"
                               {yylhs.value.as < Haskell::Stmts > () = make_stmts(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3261 "parser.cc"
    break;

  case 309: // stmts: stmts ";" stmt
#line 1223 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3267 "parser.cc"
    break;

  case 310: // stmts: stmts ";"
#line 1224 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3273 "parser.cc"
    break;

  case 311: // stmts: stmt
#line 1225 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3279 "parser.cc"
    break;

  case 312: // stmts: %empty
#line 1226 "parser.y"
                       {}
#line 3285 "parser.cc"
    break;

  case 313: // stmt: qual
#line 1231 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3291 "parser.cc"
    break;

  case 314: // stmt: "rec" stmtlist
#line 1232 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::RecStmt(yystack_[0].value.as < Haskell::Stmts > ());}
#line 3297 "parser.cc"
    break;

  case 315: // qual: bindpat "<-" exp
#line 1234 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::PatQual(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3303 "parser.cc"
    break;

  case 316: // qual: exp
#line 1235 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::SimpleQual(yystack_[0].value.as < expression_ref > ());}
#line 3309 "parser.cc"
    break;

  case 317: // qual: "let" binds
#line 1236 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::LetQual(yystack_[0].value.as < expression_ref > ());}
#line 3315 "parser.cc"
    break;

  case 325: // qcon: gen_qcon
#line 1281 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3321 "parser.cc"
    break;

  case 326: // qcon: sysdcon
#line 1282 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3327 "parser.cc"
    break;

  case 327: // gen_qcon: qconid
#line 1284 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3333 "parser.cc"
    break;

  case 328: // gen_qcon: "(" qconsym ")"
#line 1285 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3339 "parser.cc"
    break;

  case 329: // con: conid
#line 1287 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3345 "parser.cc"
    break;

  case 330: // con: "(" consym ")"
#line 1288 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3351 "parser.cc"
    break;

  case 331: // con: sysdcon
#line 1289 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3357 "parser.cc"
    break;

  case 334: // sysdcon_no_list: "(" ")"
#line 1294 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 3363 "parser.cc"
    break;

  case 335: // sysdcon_no_list: "(" commas ")"
#line 1295 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3369 "parser.cc"
    break;

  case 336: // sysdcon_no_list: "(#" "#)"
#line 1296 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 3375 "parser.cc"
    break;

  case 337: // sysdcon_no_list: "(#" commas "#)"
#line 1297 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3381 "parser.cc"
    break;

  case 338: // sysdcon: sysdcon_no_list
#line 1299 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3387 "parser.cc"
    break;

  case 339: // sysdcon: "[" "]"
#line 1300 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 3393 "parser.cc"
    break;

  case 340: // conop: consym
#line 1302 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3399 "parser.cc"
    break;

  case 341: // conop: "`" conid "`"
#line 1303 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3405 "parser.cc"
    break;

  case 342: // qconop: qconsym
#line 1305 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3411 "parser.cc"
    break;

  case 343: // qconop: "`" qconid "`"
#line 1306 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3417 "parser.cc"
    break;

  case 344: // gtycon: ntgtycon
#line 1309 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3423 "parser.cc"
    break;

  case 345: // gtycon: "(" ")"
#line 1310 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 3429 "parser.cc"
    break;

  case 346: // gtycon: "(#" "#)"
#line 1311 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 3435 "parser.cc"
    break;

  case 347: // ntgtycon: oqtycon
#line 1313 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3441 "parser.cc"
    break;

  case 348: // ntgtycon: "(" commas ")"
#line 1314 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3447 "parser.cc"
    break;

  case 349: // ntgtycon: "(#" commas "#)"
#line 1315 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3453 "parser.cc"
    break;

  case 350: // ntgtycon: "(" "->" ")"
#line 1316 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 3459 "parser.cc"
    break;

  case 351: // ntgtycon: "[" "]"
#line 1317 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 3465 "parser.cc"
    break;

  case 352: // oqtycon: qtycon
#line 1319 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3471 "parser.cc"
    break;

  case 353: // oqtycon: "(" qtyconsym ")"
#line 1320 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3477 "parser.cc"
    break;

  case 354: // oqtycon: "(" "~" ")"
#line 1321 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3483 "parser.cc"
    break;

  case 355: // oqtycon_no_varcon: qtycon
#line 1323 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3489 "parser.cc"
    break;

  case 356: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1324 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3495 "parser.cc"
    break;

  case 357: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1325 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3501 "parser.cc"
    break;

  case 358: // oqtycon_no_varcon: "(" ":" ")"
#line 1326 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 3507 "parser.cc"
    break;

  case 359: // oqtycon_no_varcon: "(" "~" ")"
#line 1327 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3513 "parser.cc"
    break;

  case 360: // qtyconop: qtyconsym
#line 1330 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3519 "parser.cc"
    break;

  case 361: // qtyconop: "`" qtycon "`"
#line 1331 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3525 "parser.cc"
    break;

  case 362: // qtycondoc: qtycon
#line 1333 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3531 "parser.cc"
    break;

  case 363: // qtycon: "QCONID"
#line 1335 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3537 "parser.cc"
    break;

  case 364: // qtycon: tycon
#line 1336 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3543 "parser.cc"
    break;

  case 365: // tycon: "CONID"
#line 1340 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3549 "parser.cc"
    break;

  case 366: // qtyconsym: "QCONSYM"
#line 1342 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3555 "parser.cc"
    break;

  case 367: // qtyconsym: "QVARSYM"
#line 1343 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3561 "parser.cc"
    break;

  case 368: // qtyconsym: tyconsym
#line 1344 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3567 "parser.cc"
    break;

  case 369: // tyconsym: "CONSYM"
#line 1346 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3573 "parser.cc"
    break;

  case 370: // tyconsym: "VARSYM"
#line 1347 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3579 "parser.cc"
    break;

  case 371: // tyconsym: ":"
#line 1348 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 3585 "parser.cc"
    break;

  case 372: // tyconsym: "-"
#line 1349 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 3591 "parser.cc"
    break;

  case 373: // op: varop
#line 1354 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3597 "parser.cc"
    break;

  case 374: // op: conop
#line 1355 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3603 "parser.cc"
    break;

  case 375: // varop: varsym
#line 1357 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3609 "parser.cc"
    break;

  case 376: // varop: "`" varid "`"
#line 1358 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3615 "parser.cc"
    break;

  case 377: // qop: qvarop
#line 1360 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3621 "parser.cc"
    break;

  case 378: // qop: qconop
#line 1361 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3627 "parser.cc"
    break;

  case 379: // qop: hole_op
#line 1362 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3633 "parser.cc"
    break;

  case 380: // qopm: qvaropm
#line 1364 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3639 "parser.cc"
    break;

  case 381: // qopm: qconop
#line 1365 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3645 "parser.cc"
    break;

  case 382: // qopm: hole_op
#line 1366 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3651 "parser.cc"
    break;

  case 383: // hole_op: "`" "_" "`"
#line 1368 "parser.y"
                      { yylhs.value.as < std::string > () = "_"; }
#line 3657 "parser.cc"
    break;

  case 384: // qvarop: qvarsym
#line 1370 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3663 "parser.cc"
    break;

  case 385: // qvarop: "`" qvarid "`"
#line 1371 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3669 "parser.cc"
    break;

  case 386: // qvaropm: qvarsym_no_minus
#line 1373 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 3675 "parser.cc"
    break;

  case 387: // qvaropm: "`" qvarid "`"
#line 1374 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3681 "parser.cc"
    break;

  case 388: // tyvar: tyvarid
#line 1378 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3687 "parser.cc"
    break;

  case 389: // tyvarop: "`" tyvarid "`"
#line 1380 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3693 "parser.cc"
    break;

  case 390: // tyvarid: "VARID"
#line 1382 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3699 "parser.cc"
    break;

  case 391: // tyvarid: special_id
#line 1383 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3705 "parser.cc"
    break;

  case 392: // tyvarid: "unsafe"
#line 1384 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 3711 "parser.cc"
    break;

  case 393: // tyvarid: "safe"
#line 1385 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 3717 "parser.cc"
    break;

  case 394: // tyvarid: "interruptible"
#line 1386 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 3723 "parser.cc"
    break;

  case 395: // var: varid
#line 1389 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3729 "parser.cc"
    break;

  case 396: // var: "(" varsym ")"
#line 1390 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3735 "parser.cc"
    break;

  case 397: // qvar: qvarid
#line 1392 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3741 "parser.cc"
    break;

  case 398: // qvar: "(" varsym ")"
#line 1393 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3747 "parser.cc"
    break;

  case 399: // qvar: "(" qvarsym1 ")"
#line 1394 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3753 "parser.cc"
    break;

  case 400: // qvarid: varid
#line 1396 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3759 "parser.cc"
    break;

  case 401: // qvarid: "QVARID"
#line 1397 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3765 "parser.cc"
    break;

  case 402: // varid: "VARID"
#line 1399 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3771 "parser.cc"
    break;

  case 403: // varid: special_id
#line 1400 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3777 "parser.cc"
    break;

  case 404: // varid: "unsafe"
#line 1401 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 3783 "parser.cc"
    break;

  case 405: // varid: "safe"
#line 1402 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 3789 "parser.cc"
    break;

  case 406: // varid: "interruptible"
#line 1403 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 3795 "parser.cc"
    break;

  case 407: // varid: "forall"
#line 1404 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 3801 "parser.cc"
    break;

  case 408: // varid: "family"
#line 1405 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 3807 "parser.cc"
    break;

  case 409: // varid: "role"
#line 1406 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 3813 "parser.cc"
    break;

  case 410: // qvarsym: varsym
#line 1408 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3819 "parser.cc"
    break;

  case 411: // qvarsym: qvarsym1
#line 1409 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3825 "parser.cc"
    break;

  case 412: // qvarsym_no_minus: varsym_no_minus
#line 1411 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3831 "parser.cc"
    break;

  case 413: // qvarsym_no_minus: qvarsym1
#line 1412 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3837 "parser.cc"
    break;

  case 414: // qvarsym1: "QVARSYM"
#line 1414 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3843 "parser.cc"
    break;

  case 415: // varsym: varsym_no_minus
#line 1416 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3849 "parser.cc"
    break;

  case 416: // varsym: "-"
#line 1417 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 3855 "parser.cc"
    break;

  case 417: // varsym_no_minus: "VARSYM"
#line 1419 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3861 "parser.cc"
    break;

  case 418: // varsym_no_minus: special_sym
#line 1420 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3867 "parser.cc"
    break;

  case 419: // special_id: "as"
#line 1422 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 3873 "parser.cc"
    break;

  case 420: // special_id: "qualified"
#line 1423 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 3879 "parser.cc"
    break;

  case 421: // special_id: "hiding"
#line 1424 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 3885 "parser.cc"
    break;

  case 422: // special_id: "export"
#line 1425 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 3891 "parser.cc"
    break;

  case 423: // special_id: "label"
#line 1426 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 3897 "parser.cc"
    break;

  case 424: // special_id: "dynamic"
#line 1427 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 3903 "parser.cc"
    break;

  case 425: // special_id: "stdcall"
#line 1428 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 3909 "parser.cc"
    break;

  case 426: // special_id: "ccall"
#line 1429 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 3915 "parser.cc"
    break;

  case 427: // special_id: "capi"
#line 1430 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 3921 "parser.cc"
    break;

  case 428: // special_id: "prim"
#line 1431 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 3927 "parser.cc"
    break;

  case 429: // special_id: "javascript"
#line 1432 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 3933 "parser.cc"
    break;

  case 430: // special_id: "group"
#line 1433 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 3939 "parser.cc"
    break;

  case 431: // special_id: "stock"
#line 1434 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 3945 "parser.cc"
    break;

  case 432: // special_id: "anyclass"
#line 1435 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 3951 "parser.cc"
    break;

  case 433: // special_id: "via"
#line 1436 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 3957 "parser.cc"
    break;

  case 434: // special_id: "unit"
#line 1437 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 3963 "parser.cc"
    break;

  case 435: // special_id: "dependency"
#line 1438 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 3969 "parser.cc"
    break;

  case 436: // special_id: "signature"
#line 1439 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 3975 "parser.cc"
    break;

  case 437: // special_sym: "!"
#line 1441 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 3981 "parser.cc"
    break;

  case 438: // special_sym: "."
#line 1442 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 3987 "parser.cc"
    break;

  case 439: // special_sym: "*"
#line 1443 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 3993 "parser.cc"
    break;

  case 440: // qconid: conid
#line 1447 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3999 "parser.cc"
    break;

  case 441: // qconid: "QCONID"
#line 1448 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4005 "parser.cc"
    break;

  case 442: // conid: "CONID"
#line 1450 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4011 "parser.cc"
    break;

  case 443: // qconsym: consym
#line 1452 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4017 "parser.cc"
    break;

  case 444: // qconsym: "QCONSYM"
#line 1453 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4023 "parser.cc"
    break;

  case 445: // consym: "CONSYM"
#line 1455 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4029 "parser.cc"
    break;

  case 446: // consym: ":"
#line 1456 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4035 "parser.cc"
    break;

  case 447: // literal: "CHAR"
#line 1460 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < char > ();}
#line 4041 "parser.cc"
    break;

  case 448: // literal: "STRING"
#line 1461 "parser.y"
                  {yylhs.value.as < expression_ref > () = yy_make_string(yystack_[0].value.as < std::string > ());}
#line 4047 "parser.cc"
    break;

  case 449: // literal: "INTEGER"
#line 1462 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < int > ();}
#line 4053 "parser.cc"
    break;

  case 450: // literal: "RATIONAL"
#line 1463 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < double > ();}
#line 4059 "parser.cc"
    break;

  case 452: // close: error
#line 1471 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 4065 "parser.cc"
    break;

  case 453: // modid: "CONID"
#line 1475 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4071 "parser.cc"
    break;

  case 454: // modid: "QCONID"
#line 1476 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4077 "parser.cc"
    break;

  case 455: // commas: commas ","
#line 1478 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 4083 "parser.cc"
    break;

  case 456: // commas: ","
#line 1479 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 4089 "parser.cc"
    break;


#line 4093 "parser.cc"

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


  const short parser::yypact_ninf_ = -620;

  const short parser::yytable_ninf_ = -416;

  const short
  parser::yypact_[] =
  {
      30,   117,  -620,    93,  -620,  -620,  -620,  -620,  -620,   180,
       3,    68,  -620,    56,   -22,   -22,    66,  -620,  -620,  -620,
    -620,   181,  -620,  -620,  -620,    76,  -620,   145,   187,  4217,
     209,   197,   157,  -620,   666,  -620,    63,  -620,  -620,  -620,
    -620,   117,  -620,   137,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,   346,  -620,  -620,  -620,  -620,
     169,   174,  -620,   195,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,   113,   221,   269,  -620,   199,  -620,  2526,  3879,
    -620,   213,   240,  1679,  -620,  -620,  -620,   367,   254,  -620,
    3879,  4538,   240,  3373,  3373,   231,   217,  4493,    98,  3010,
    3373,  3131,  3373,  1302,  1046,  1174,  -620,  -620,  -620,  -620,
    -620,  -620,    60,   231,   251,   157,  -620,  -620,  -620,   279,
    -620,  -620,   454,  -620,  3252,  -620,   276,  -620,  -620,  -620,
    -620,  -620,   278,   303,   283,  -620,  -620,  -620,  -620,   265,
    -620,   247,  -620,  -620,   290,   294,  -620,  -620,  -620,  -620,
    -620,   297,  -620,   300,   302,   304,  -620,  -620,  -620,  4217,
    4250,  -620,  -620,  -620,  -620,  -620,  -620,   389,  -620,    52,
    1046,   392,   647,  -620,  -620,  2526,  -620,  -620,  -620,  -620,
    -620,  -620,  -620,  -620,  4660,  3576,  3475,   313,  4401,  -620,
    -620,  -620,  -620,  -620,   402,  4309,  -620,   327,  -620,   222,
    3879,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,  3677,  1800,  1800,  -620,  2284,   351,   317,    62,
    -620,  -620,   368,   369,   370,   371,  3677,   795,   795,  -620,
     429,   378,   363,   171,  4717,   321,   323,  -620,  -620,  -620,
     376,  -620,   -15,  4493,  -620,   387,   309,   -11,   373,    74,
     103,   353,   395,  -620,  -620,  1921,  3373,  -620,  -620,  2889,
    -620,  3252,   191,  -620,  -620,  3980,  -620,  -620,  -620,   647,
      61,   375,   366,  -620,  2526,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,  3131,  -620,  -620,   -20,    92,   302,   374,   382,
     383,    95,  -620,   133,   159,   172,  3677,  4493,  4493,  -620,
     381,   199,   355,  3879,  3677,  3980,   191,  -620,  2768,  -620,
    -620,  -620,  -620,  -620,  4309,  -620,  4434,  4660,  3373,  -620,
     384,   385,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,
     386,   394,  -620,  -620,   397,    56,  -620,   356,   404,   412,
     277,  3677,  2526,  -620,    18,   403,   396,  -620,  -620,  -620,
    -620,   391,   417,  -620,   401,   384,  -620,    10,   406,   385,
     123,   173,   400,   407,   254,  -620,  -620,  3879,  3677,  -620,
    -620,   409,   410,   254,   240,  3373,   436,   438,   -17,  -620,
    -620,    42,   439,   416,  -620,    78,  -620,   501,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,  -620,   402,   -16,  -620,  -620,
     776,    46,  2526,  3677,   428,   424,   413,   414,  2526,   418,
     444,   466,  -620,  -620,   472,   442,    98,    67,   475,  1558,
     925,  -620,  -620,  2526,  -620,  2526,  2284,  -620,    32,  -620,
     443,   445,   448,  2526,  2526,  2042,  1430,  -620,  1430,   161,
    -620,  1430,  -620,  1430,   440,  -620,  -620,  -620,  -620,  -620,
    -620,   476,   477,   479,  4627,   452,  -620,  -620,  -620,  -620,
       0,   248,  -620,  -620,   241,  -620,   453,  -620,  -620,  -620,
    -620,   470,  -620,   457,   491,    85,  -620,  -620,  -620,  -620,
    4250,  -620,  -620,  -620,   117,  -620,  -620,  -620,  -620,  -620,
    3677,  4660,  -620,  4660,  4750,  -620,  3677,  -620,  3677,  -620,
    3677,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  3677,
     429,  -620,  -620,  2526,  -620,  1800,  -620,  2526,  2284,  -620,
    2526,  -620,  -620,   795,  -620,  -620,  -620,  -620,  -620,  -620,
     447,   451,  -620,   478,  -620,  -620,  -620,  -620,  -620,   480,
     228,   186,  -620,  -620,  -620,  3373,  -620,  2163,  -620,   481,
     463,  -620,   301,    56,  -620,  -620,   402,   499,  -620,  -620,
    -620,  -620,  -620,  -620,  2647,   469,  -620,  -620,   507,  -620,
    -620,  -620,  -620,  -620,  3677,  3677,   471,   381,  -620,   510,
    3677,   559,  -620,   581,  -620,  -620,  4434,  1430,  3677,   486,
     588,  -620,  -620,  -620,  3677,  4809,  -620,  -620,  -620,  -620,
     485,   487,  -620,  -620,  -620,  -620,  -620,   317,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,  -620,  -620,  2405,  2526,  -620,
      37,  -620,  -620,  2526,   311,   554,  2042,  2526,  -620,    -7,
       8,  -620,  -620,  -620,  -620,  -620,   519,  -620,  4309,    50,
    -620,   581,  -620,  -620,  -620,  -620,  -620,   117,    53,  -620,
     525,  -620,  -620,   596,  -620,   402,  -620,  -620,  2526,  2526,
    2526,  -620,  -620,  -620,  -620,  3677,  -620,  4776,   559,   520,
    4025,  -620,  -620,  -620,  -620,  -620,  -620,  3778,   149,   555,
    -620,  -620,  -620,  -620,   503,  4217,  -620,  -620,  3677,  2526,
    -620,   562,  -620,  -620,  -620,  -620,  -620,  4309,   495,  -620,
    4309,  -620,  -620,   500,   508,  -620,  3879,  -620,  4217,   509,
     511,  -620,  2526,  4117,  -620,  4309,  3879,  -620,  -620,   515,
    -620,  -620,  -620,  -620,  -620
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    39,     0,     2,    39,     4,   453,   454,     8,
       0,    42,     1,     0,     0,     0,    18,    11,    38,    13,
      16,    67,   452,   451,    12,   114,   110,     0,     0,     0,
       0,    45,    40,    15,    14,   113,     0,     6,     7,   419,
     421,     0,   420,     0,   407,   422,   423,   424,   405,   406,
     404,   408,   409,   425,   426,   427,   428,   429,   430,   431,
     432,   433,   434,   436,   435,     0,   402,   365,   401,   363,
       0,    19,    21,    24,    32,    35,   355,   364,    34,   397,
     400,   403,     0,     0,    47,    37,    41,   256,     0,     0,
      92,     0,     0,     0,    60,    61,    62,    87,     0,    93,
       0,     0,     0,     0,     0,   211,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   442,   441,   447,   448,
     449,   450,   211,   211,    58,    65,    68,    69,    70,   100,
      72,   192,    73,   219,   223,   233,   245,   247,   249,   325,
     338,   326,     0,   248,   400,   327,   440,   250,   111,     0,
      23,     0,    33,   352,     0,     0,   416,   437,   439,   438,
     417,     0,   414,     0,     0,     0,   415,   418,    17,     0,
      26,    22,    39,    39,     3,    44,    46,    51,    36,     0,
       0,     0,   216,   224,   217,     0,   393,   394,   392,   371,
     127,   372,   126,   148,   177,     0,     0,     0,     0,   390,
     370,   369,   367,   366,   109,     0,   125,     0,    97,   134,
     137,   140,   142,   146,   347,   143,   360,   368,   147,   144,
     388,   391,   160,   312,   312,   242,     0,     0,   227,     0,
     240,   295,     0,     0,     0,     0,     0,   104,   104,   107,
       0,     0,   134,     0,     0,     0,     0,   395,   375,   243,
       0,   232,     0,     0,   212,     0,     0,     0,     0,     0,
       0,   332,   117,   331,   329,     0,     0,   303,   306,     0,
     235,   221,     0,   446,   339,     0,   445,   444,   257,   216,
     262,     0,   263,   381,     0,   382,   380,   386,   413,   412,
     342,   443,   416,   334,   456,     0,     0,   413,     0,   412,
     342,     0,   336,     0,     0,     0,     0,     0,     0,    59,
       0,    66,     0,     0,     0,     0,     0,   378,     0,   379,
     377,   384,   411,   410,     0,   230,   319,     0,     0,   112,
       0,     0,   358,   359,   357,   356,   399,   398,    20,    31,
       0,    27,    29,    30,     0,     0,    50,    49,     0,     0,
       0,     0,     0,   225,     0,     0,   178,   180,   122,   164,
     351,     0,     0,   130,     0,   127,   151,   161,     0,   360,
       0,     0,     0,     0,     0,    78,   149,     0,     0,   141,
     161,     0,   159,     0,     0,     0,   316,     0,     0,   311,
     313,     0,     0,   279,   281,     0,   226,     0,   294,   297,
      84,    83,    85,    86,   156,   119,   109,     0,   195,   103,
     115,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   222,   206,     0,     0,     0,     0,     0,     0,
       0,   238,   304,     0,   305,     0,     0,   193,   109,   199,
       0,     0,     0,   258,   264,     0,     0,   255,     0,   259,
     251,     0,   252,     0,   398,   328,   335,   455,   253,   254,
     337,     0,     0,     0,     0,   203,   374,    64,   373,   340,
       0,    94,   116,   201,   131,   120,     0,   194,   220,   231,
     322,     0,   318,   321,   324,     0,   234,   354,   353,    25,
       0,     9,    10,    48,     0,   229,   228,   241,   215,   218,
       0,     0,   150,     0,     0,   153,     0,   350,     0,   154,
       0,   348,   349,   361,   389,   108,    96,   135,    71,     0,
     317,   314,   302,     0,   307,   310,   308,     0,     0,   296,
       0,    82,   105,   102,   106,   237,   131,    79,   396,   376,
      77,    75,   244,     0,   213,   205,   207,   330,   333,     0,
       0,     0,   118,   344,   204,     0,   284,     0,   299,     0,
     286,   290,     0,     0,   285,   236,   109,     0,   197,   198,
     383,   387,   343,   266,     0,   268,   273,   274,   257,   270,
     269,   261,   260,   210,     0,     0,     0,     0,    99,     0,
       0,   174,    81,   182,   385,   246,     0,     0,     0,     0,
      53,   181,   121,   179,     0,     0,   163,   165,   129,   167,
       0,   162,   162,   315,   309,   298,   280,   227,   101,    76,
      74,   214,   345,   346,   300,   287,   282,   289,     0,   291,
     109,   283,   196,     0,   430,   275,     0,   265,   123,     0,
       0,   341,    63,    98,    95,   164,   168,   170,     0,     0,
      80,   183,   185,   320,   323,   202,    28,     0,    55,   128,
       0,   155,   152,     0,   288,   109,   293,   200,     0,     0,
       0,   271,   272,   267,   208,     0,   209,     0,   174,     0,
     175,   138,   145,   172,    90,    88,    89,     0,     0,   186,
     189,   362,   184,    52,     0,     0,    43,    54,     0,     0,
     292,     0,   277,   276,   124,   173,   169,     0,     0,   139,
       0,   190,   136,   157,     0,   187,     0,   188,     0,     0,
       0,   239,     0,   175,   171,   176,     0,   191,    91,     0,
      56,   166,   278,   158,    57
  };

  const short
  parser::yypgoto_[] =
  {
    -620,  -620,  -620,  -620,  -620,  -620,  -620,    57,  -620,  -620,
    -619,  -620,   446,  -620,  -620,  -620,   128,  -147,  -620,   504,
    -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,   312,  -620,   388,  -620,  -288,
    -364,   616,  -620,  -620,  -620,  -280,    34,   307,    51,  -620,
    -620,  -187,   229,   -53,  -620,   -88,  -620,   -83,  -377,  -620,
     425,  -576,  -194,   335,   -81,  -620,   422,     1,  -620,  -537,
    -620,  -620,   -31,  -620,   -59,  -620,  -620,   146,  -620,  -620,
       2,   -36,   617,   121,   339,  -620,   218,  -620,   246,  -620,
     -72,   -69,   623,   -19,  -277,    41,  -620,   -71,   -79,  -620,
    -620,   -57,   544,  -620,  -620,  -620,    24,   226,  -620,   314,
    -401,  -620,    36,  -620,  -208,  -620,  -211,  -620,  -620,   398,
    -620,   -75,   441,   150,  -213,  -620,    81,  -620,  -620,  -620,
    -620,   255,  -620,   -82,  -600,  -100,  -620,   253,   644,  -620,
    -620,  -620,   -27,  -620,  -129,  -620,   106,   595,  -154,  -620,
     -93,  -620,  -620,  -467,  -620,   514,   -94,   -29,  -207,     4,
    -620,  -620,   -45,   -60,   -65,   -86,  -620,  -184,  -102,   -54,
    -249,  -620,  -137,   -37,   -63
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   174,     6,    10,    19,    30,
      70,    71,    72,   171,   340,   341,   342,    73,    74,    86,
      11,    20,    21,    32,    84,   177,   494,   347,   658,   696,
     697,   310,   124,   465,    33,    34,   125,   126,   127,   128,
     236,   688,   717,   129,   592,   204,   313,   407,   239,   240,
     375,    27,    36,   316,   428,   404,   473,   354,   639,   205,
     206,   405,   475,   362,   679,   363,   713,   209,   680,   210,
     211,   681,   212,   406,   714,   381,   368,   504,   606,   610,
     593,   646,   647,   648,   683,   355,   356,   357,   650,   651,
     652,   689,   408,   409,   437,   438,   439,   131,   253,   254,
     278,   182,   410,   183,   184,   397,   185,   134,   135,   136,
     137,   295,   296,   281,   282,   575,   576,   392,   393,   431,
     559,   560,   561,   629,   229,   230,   231,   562,   387,   268,
     269,   225,   388,   389,   390,   481,   482,   483,   138,   139,
     261,   262,   140,   141,   466,   283,   552,   213,   214,    75,
     215,   690,   153,    77,   216,   217,   467,   468,   318,   284,
     285,   320,   286,   218,   219,   220,   142,   143,    79,    80,
     321,   287,   288,   323,   166,    81,   167,   145,   146,   290,
     291,   147,    24,     9,   301
  };

  const short
  parser::yytable_[] =
  {
      78,   208,    76,   221,   150,   165,   264,   246,   361,   367,
     425,   376,   241,   394,   221,   133,   181,   242,   398,   395,
     164,   228,   331,   343,   250,   251,   263,   249,   352,   563,
     267,   270,   317,   272,   472,   380,   207,   607,   144,   319,
     271,   248,   531,    22,   279,   279,   279,    22,   289,   299,
     289,     1,   305,   644,   298,   325,   280,    22,   303,   374,
     300,   469,    13,    22,   374,   694,   423,   369,   441,   297,
     674,   498,   684,   419,   568,   499,   719,   588,   257,   306,
     710,    25,   317,   524,   532,   676,   515,   322,   450,   319,
     508,   442,   399,    12,   451,   520,   525,   533,   500,   729,
     358,   685,   686,    17,   709,   247,    26,   675,   476,   221,
     221,   279,   221,   353,   420,   299,   436,   424,   509,   221,
     165,   226,   675,   710,   221,   443,   300,   474,   589,     2,
     479,   442,   501,   370,   371,   297,   221,   322,   660,   444,
      78,    78,    76,    76,    23,   445,   226,   709,    23,   709,
     221,   386,   386,   273,   386,   525,   625,   687,    23,   533,
     695,   720,   226,   252,    23,   598,   499,   148,    67,   348,
     549,   372,    69,    29,   550,   446,   551,   149,   529,   317,
     349,    18,   293,   414,   398,    67,   319,   432,   294,    69,
     267,   517,   325,   181,   276,    31,   165,   305,   247,   501,
     452,   258,   632,   456,    35,   259,   453,   260,   492,   457,
     607,   164,   172,   302,   173,   449,   116,   294,   133,   133,
     221,   271,    37,   394,   421,   208,   536,   221,   221,   344,
     345,   511,   577,   358,   322,     7,    82,   457,   221,     8,
     273,   144,   144,   458,   151,    14,    15,   451,   415,   486,
     248,   156,   157,   158,   526,    67,   687,    83,   159,    69,
     207,   156,   157,   158,    38,   221,   666,    67,   159,   459,
      85,    69,   435,   453,   534,   436,   315,   168,   462,   463,
     160,   276,   460,   512,   162,   277,   457,   457,   169,   516,
     160,   221,   221,   564,   242,   352,   623,   484,   175,   478,
     294,   700,   170,   176,   638,   638,   522,   189,   378,   521,
     317,  -132,   178,   601,   364,   616,   330,   319,   191,   608,
     222,   609,  -119,   611,   264,  -119,   189,   221,   590,   591,
     723,   247,   612,   725,   252,   330,   622,   191,   469,   223,
     535,   224,   294,   343,   263,   255,   542,   200,   201,   317,
     312,   202,   203,   237,   630,   238,   319,   558,   558,   668,
     669,   565,   586,   566,   386,   322,   200,   201,   307,   308,
     202,   203,   573,   386,   578,   326,   429,   279,   430,   279,
     309,   289,   279,   289,   279,   226,   289,   628,   289,   579,
     328,   580,   327,   329,   581,   704,   582,  -395,   332,   156,
     157,   158,   333,   609,   322,   334,   159,   602,   335,   358,
     336,   536,   337,   346,   221,   350,   377,   659,   221,   398,
     221,   331,   221,   672,   221,   154,   631,   294,   160,   374,
     396,   469,   162,   221,   155,   226,   156,   157,   158,   232,
     233,   234,   235,   159,   412,   400,   401,   402,   403,   378,
     416,   613,   417,   386,   682,   615,   386,   600,   617,   413,
     273,    78,   418,    76,   422,   160,   161,   426,   415,   162,
     163,   156,   157,   158,   469,   427,   624,   274,   159,   447,
     448,   495,   454,   470,   493,   558,   682,   370,   371,   496,
    -415,   455,   487,   488,   489,   505,   464,   491,   221,   221,
     160,   276,   635,   502,   221,   247,   506,   247,   490,   507,
     503,   609,   221,   682,   133,   513,   682,   518,   221,   221,
     510,  -301,   514,   523,   519,   527,   530,   248,   279,   682,
     528,   682,   289,   273,   314,  -115,   538,   144,  -115,   539,
     654,   540,   541,   545,   156,   157,   158,   543,   544,   546,
     547,   159,   554,   583,  -396,   558,   665,   584,   570,   585,
     571,   667,   221,   572,   386,   673,   587,   484,   594,   315,
     595,   596,   597,   160,   276,   619,   627,   162,   277,   620,
     633,   626,   621,   636,   360,   637,   641,   643,   645,   221,
     649,   221,   657,   661,   221,   662,   701,   702,   703,   712,
     656,   221,   670,   678,   242,   698,   586,   699,   716,   707,
     718,   722,   221,   116,   726,   338,   727,   730,   599,   731,
     693,   221,   691,   734,   221,   471,   411,   721,   728,   311,
     221,    28,   655,   242,   485,   379,   640,   221,   712,   221,
     221,   461,   537,   242,   382,   733,   677,   706,   724,   603,
     732,   130,   715,   692,   618,   477,   569,   132,   663,   304,
     671,   691,   567,   664,   497,   391,    78,   434,    76,    87,
      39,    88,    89,    90,    91,   614,    92,   653,    40,    93,
     553,   548,    94,    95,    96,    97,    98,   152,    99,    78,
      42,    76,   100,   642,   101,    44,   245,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,   103,   373,    58,     0,     0,   104,    59,    60,    61,
      62,    63,    64,   105,     0,     0,   273,   351,   106,   107,
       0,     0,     0,     0,     0,     0,     0,   156,   157,   158,
       0,     0,   108,     0,   159,     0,     0,     0,   109,     0,
       0,     0,     0,     0,   110,     0,   111,   112,     0,     0,
       0,     0,   315,     0,     0,     0,   160,   276,     0,   113,
     162,   277,     0,   114,     0,   115,     0,     0,     0,     0,
       0,     0,     0,    66,   116,     0,     0,    68,   117,     0,
       0,     0,     0,   118,   119,   120,   121,     0,    87,    39,
      88,     0,     0,   122,   123,    92,     0,    40,    93,     0,
       0,    94,    95,    96,     0,    98,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,    55,    56,    57,
     103,     0,    58,     0,     0,   104,    59,    60,    61,    62,
      63,    64,   105,     0,     0,   273,   314,   106,   107,     0,
       0,     0,     0,     0,     0,     0,   156,   157,   158,     0,
       0,   108,     0,   159,     0,     0,     0,   109,     0,     0,
       0,     0,     0,   110,     0,   111,   112,     0,     0,     0,
       0,   315,     0,     0,     0,   160,   276,     0,   113,   162,
     277,     0,   114,     0,   115,     0,     0,     0,     0,     0,
       0,     0,    66,   116,     0,     0,    68,   117,     0,     0,
       0,     0,   118,   119,   120,   121,    22,     0,    87,    39,
      88,     0,   122,   123,     0,    92,     0,    40,    93,     0,
       0,     0,     0,     0,     0,    98,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,    55,    56,    57,
     103,     0,    58,     0,     0,   104,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,   106,   179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   109,     0,     0,
       0,     0,     0,   110,     0,   111,   555,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    23,   113,     0,
       0,     0,   180,     0,   115,     0,     0,     0,   557,     0,
       0,     0,    66,   116,     0,     0,    68,   117,     0,    87,
      39,    88,   118,   119,   120,   121,    92,     0,    40,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,   103,     0,    58,     0,     0,   104,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,   106,   179,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   273,     0,     0,   109,     0,
       0,     0,     0,     0,   110,     0,   292,   157,   158,     0,
       0,     0,     0,   159,     0,     0,     0,     0,     0,   113,
       0,     0,     0,   180,   293,   115,     0,     0,     0,     0,
     294,   275,     0,    66,   116,   160,   276,    68,   117,   162,
     277,     0,     0,   118,   119,   120,   121,    87,    39,    88,
       0,     0,     0,     0,    92,     0,    40,    93,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,   103,
       0,    58,     0,     0,   104,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,   106,   179,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   273,     0,     0,   109,     0,     0,     0,
       0,     0,   110,     0,   111,   157,   158,     0,     0,     0,
       0,   159,     0,     0,     0,     0,     0,   113,     0,     0,
       0,   180,     0,   115,   302,     0,     0,     0,   294,   275,
       0,    66,   116,   160,   276,    68,   117,   162,   277,     0,
       0,   118,   119,   120,   121,    87,    39,    88,     0,     0,
       0,     0,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,   103,     0,    58,
       0,     0,   104,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,   106,   179,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   273,     0,     0,   109,     0,     0,     0,     0,     0,
     110,     0,   111,   157,   158,     0,     0,     0,     0,   159,
       0,     0,     0,     0,     0,   113,   274,     0,     0,   180,
       0,   115,     0,     0,     0,     0,     0,   275,     0,    66,
     116,   160,   276,    68,   117,   162,   277,     0,     0,   118,
     119,   120,   121,    87,    39,    88,     0,     0,     0,     0,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,   103,     0,    58,     0,     0,
     104,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,   106,   179,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   273,
       0,     0,   109,     0,     0,     0,     0,     0,   110,     0,
     111,   157,   158,     0,     0,     0,     0,   159,     0,     0,
       0,     0,     0,   113,     0,     0,     0,   180,     0,   115,
       0,     0,     0,     0,     0,   275,     0,    66,   116,   160,
     276,    68,   117,   162,   277,     0,     0,   118,   119,   120,
     121,    87,    39,    88,     0,     0,     0,     0,    92,     0,
      40,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,   103,     0,    58,     0,     0,   104,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
     106,   179,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,     0,     0,     0,     0,     0,   110,     0,   111,   555,
       0,     0,     0,     0,     0,     0,     0,     0,   556,     0,
       0,   113,     0,     0,     0,   180,     0,   115,     0,     0,
       0,   557,     0,     0,     0,    66,   116,     0,     0,    68,
     117,     0,    87,    39,    88,   118,   119,   120,   121,    92,
       0,    40,    93,     0,     0,     0,     0,     0,     0,    98,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,    55,    56,    57,   103,     0,    58,     0,     0,   104,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,   106,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,     0,   226,     0,     0,     0,   110,     0,   111,
       0,     0,     0,     0,     0,     0,     0,     0,   227,     0,
       0,     0,   113,     0,     0,     0,   180,     0,   115,     0,
       0,     0,     0,     0,     0,     0,    66,   116,     0,     0,
      68,   117,     0,    87,    39,    88,   118,   119,   120,   121,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
     383,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,   103,   384,    58,     0,     0,
     104,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,   106,   179,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   109,     0,     0,     0,     0,     0,   110,     0,
     111,   385,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   113,     0,     0,     0,   180,     0,   115,
       0,     0,     0,     0,     0,     0,     0,    66,   116,     0,
       0,    68,   117,     0,    87,    39,    88,   118,   119,   120,
     121,    92,     0,    40,    93,     0,     0,     0,     0,     0,
       0,    98,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,    55,    56,    57,   103,     0,    58,     0,
       0,   104,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,   106,   179,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,     0,     0,     0,     0,     0,   110,
       0,   111,     0,     0,     0,     0,     0,     0,     0,     0,
     429,     0,   430,     0,   113,     0,     0,     0,   180,     0,
     115,     0,     0,     0,     0,     0,     0,     0,    66,   116,
       0,     0,    68,   117,     0,    87,    39,    88,   118,   119,
     120,   121,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,   383,     0,     0,     0,    42,   574,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,   103,     0,    58,
       0,     0,   104,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,   106,   179,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,     0,     0,     0,     0,     0,
     110,     0,   111,   385,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   113,     0,     0,     0,   180,
       0,   115,     0,     0,     0,     0,     0,     0,     0,    66,
     116,     0,     0,    68,   117,     0,    87,    39,    88,   118,
     119,   120,   121,    92,     0,    40,    93,     0,     0,     0,
       0,     0,     0,    98,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,    55,    56,    57,   103,     0,
      58,     0,     0,   104,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,   106,   179,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,     0,     0,     0,     0,
       0,   110,     0,   111,   555,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   113,     0,     0,     0,
     180,     0,   115,     0,     0,     0,   557,     0,     0,     0,
      66,   116,     0,     0,    68,   117,     0,    87,    39,    88,
     118,   119,   120,   121,    92,     0,    40,    93,     0,     0,
       0,     0,     0,     0,   383,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,   103,
       0,    58,     0,     0,   104,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,   106,   179,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   109,     0,     0,     0,
       0,     0,   110,     0,   111,   385,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   113,     0,     0,
       0,   180,     0,   115,     0,     0,     0,     0,     0,     0,
       0,    66,   116,     0,     0,    68,   117,     0,    87,    39,
      88,   118,   119,   120,   121,    92,     0,    40,    93,     0,
       0,     0,     0,     0,     0,    98,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,    55,    56,    57,
     103,     0,    58,     0,     0,   104,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,   106,   179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   109,     0,     0,
       0,     0,     0,   110,     0,   111,   555,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   113,     0,
       0,     0,   180,     0,   115,     0,     0,     0,     0,     0,
       0,     0,    66,   116,     0,     0,    68,   117,     0,    87,
      39,    88,   118,   119,   120,   121,    92,     0,    40,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,   103,     0,    58,     0,     0,   104,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,   106,   179,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   109,     0,
       0,     0,     0,     0,   110,     0,   111,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   113,
       0,     0,     0,   180,     0,   115,     0,     0,     0,     0,
       0,     0,     0,    66,   116,     0,     0,    68,   117,     0,
      87,    39,    88,   118,   119,   120,   121,    92,     0,    40,
      93,     0,     0,     0,     0,     0,     0,    98,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,    55,
      56,    57,   103,     0,   634,     0,     0,   104,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,   106,
     179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   109,
       0,     0,     0,     0,     0,   110,     0,   111,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     113,     0,     0,     0,   180,     0,   115,     0,     0,     0,
       0,     0,     0,     0,    66,   116,     0,     0,    68,   117,
       0,    87,    39,    88,   118,   119,   120,   121,    92,     0,
      40,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,   103,     0,    58,     0,     0,   104,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,     0,     0,     0,     0,     0,   110,     0,   111,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   113,     0,     0,     0,   180,     0,   115,     0,     0,
       0,     0,     0,     0,     0,    66,   116,     0,     0,    68,
     117,     0,    87,    39,    88,   118,   119,   120,   121,    92,
       0,    40,    93,     0,     0,     0,     0,     0,     0,    98,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,    55,    56,    57,   103,     0,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,     0,     0,     0,   433,     0,   110,     0,     0,
     266,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   113,     0,     0,     0,   180,     0,   115,     0,
       0,     0,     0,     0,     0,     0,    66,   116,     0,     0,
      68,   117,     0,    87,    39,   265,   118,   119,   120,   121,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,   103,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   109,     0,     0,     0,     0,     0,   110,     0,
       0,   266,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   113,     0,     0,     0,   180,     0,   115,
       0,     0,     0,     0,     0,     0,     0,    66,   116,     0,
       0,    68,   117,     0,    87,    39,    88,   118,   119,   120,
     121,    92,     0,    40,    93,     0,     0,     0,     0,     0,
       0,    98,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,    55,    56,    57,   103,     0,    58,     0,
       0,   104,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,     0,     0,     0,     0,     0,   110,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   113,     0,     0,     0,   180,     0,
     115,     0,     0,     0,     0,     0,     0,     0,    66,   116,
       0,     0,    68,   117,     0,    87,    39,    88,   118,   119,
     120,   121,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,   103,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,     0,     0,     0,     0,     0,
     110,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     324,     0,     0,     0,     0,   113,     0,     0,     0,   180,
       0,   115,     0,     0,     0,     0,     0,     0,     0,    66,
     116,     0,     0,    68,   117,     0,    87,    39,    88,   118,
     119,   120,   121,    92,     0,    40,    93,     0,     0,     0,
       0,     0,     0,    98,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,    55,    56,    57,   103,     0,
      58,     0,     0,     0,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,     0,     0,     0,     0,
       0,   110,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   113,     0,     0,    39,
     180,     0,   115,     0,     0,     0,     0,    40,     0,     0,
      66,   116,     0,     0,    68,   117,     0,     0,     0,    42,
     118,   119,   120,   121,   359,     0,    45,    46,    47,   186,
     187,   188,     0,     0,     0,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   189,     0,     0,     0,     0,     0,
       0,   364,     0,   365,     0,   191,   192,   193,     0,     0,
       0,     0,     0,     0,   194,     0,     0,     0,   195,     0,
      39,     0,   196,   366,   197,     0,     0,     0,    40,   294,
     198,     0,   199,    67,   200,   201,     0,    69,   202,   203,
      42,     0,     0,     0,     0,   359,     0,    45,    46,    47,
     186,   187,   188,     0,     0,     0,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   189,     0,     0,     0,     0,
       0,     0,     0,     0,   190,     0,   191,   192,   193,     0,
       0,     0,     0,     0,     0,   194,     0,     0,     0,   195,
     360,    39,     0,   196,     0,   197,     0,     0,     0,    40,
       0,   198,     0,   199,    67,   200,   201,     0,    69,   202,
     203,    42,     0,     0,     0,     0,   359,     0,    45,    46,
      47,   186,   187,   188,     0,     0,     0,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   189,     0,     0,     0,
       0,     0,     0,     0,     0,   190,     0,   191,   192,   193,
       0,     0,     0,     0,     0,     0,   194,     0,     0,     0,
     195,     0,    39,     0,   196,     0,   197,     0,     0,     0,
      40,     0,   198,     0,   199,    67,   200,   201,     0,    69,
     202,   203,    42,     0,     0,     0,     0,     0,     0,    45,
      46,    47,   186,   187,   188,     0,     0,     0,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   189,     0,     0,
       0,     0,     0,     0,     0,     0,   190,     0,   191,   192,
     193,     0,     0,     0,     0,     0,     0,   194,     0,     0,
       0,   195,     0,    39,     0,   196,   711,   197,     0,     0,
       0,    40,     0,   198,     0,   199,    67,   200,   201,     0,
      69,   202,   203,    42,     0,     0,     0,     0,     0,     0,
      45,    46,    47,   186,   187,   188,     0,     0,     0,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   189,     0,
       0,     0,     0,     0,     0,     0,     0,   190,     0,   191,
     192,   193,     0,     0,     0,     0,     0,     0,   194,     0,
       0,     0,   195,   440,    39,     0,   196,     0,   197,     0,
       0,     0,    40,     0,   198,     0,   199,    67,   200,   201,
       0,    69,   202,   203,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,    39,
       0,    59,    60,    61,    62,    63,    64,    40,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    42,
       0,     0,     0,     0,     0,     0,    45,    46,    47,   186,
     187,   188,     0,     0,     0,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    66,   116,     0,
       0,    68,   117,     0,   273,     0,     0,     0,     0,     0,
       0,     0,     0,   190,  -133,     0,   192,   193,     0,     0,
       0,    39,     0,     0,   194,     0,     0,     0,   195,    40,
       0,     0,   196,     0,   197,     0,     0,     0,     0,     0,
     708,    42,   199,    67,     0,   276,     0,    69,    45,    46,
      47,   186,   187,   188,     0,     0,     0,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   273,     0,     0,     0,
       0,     0,     0,     0,     0,   190,     0,     0,   192,   193,
       0,     0,     0,     0,     0,     0,   194,     0,     0,     0,
     195,    39,     0,     0,   196,     0,   197,     0,     0,    40,
       0,     0,   708,     0,   199,    67,     0,   276,    41,    69,
       0,    42,     0,    43,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,    39,    51,    52,    53,    54,    55,
      56,    57,    40,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,    42,     0,    43,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,    39,     0,     0,     0,     0,     0,     0,
       0,    40,     0,     0,    65,     0,     0,     0,   339,     0,
       0,     0,     0,    42,    66,    67,     0,     0,    68,    69,
      45,    46,    47,   186,   187,   188,     0,     0,     0,    53,
      54,    55,    56,    57,     0,     0,    58,    65,     0,     0,
      59,    60,    61,    62,    63,    64,     0,    66,    67,     0,
       0,    68,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,     0,     0,
     192,   193,     0,     0,     0,    39,     0,     0,   194,     0,
       0,     0,   195,    40,     0,     0,   196,     0,   197,     0,
       0,     0,     0,     0,     0,    42,   199,    67,     0,     0,
       0,    69,    45,    46,    47,   186,   187,   188,    39,     0,
       0,    53,    54,    55,    56,    57,    40,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,    39,     0,     0,
       0,     0,     0,     0,     0,    40,     0,     0,     0,     0,
       0,     0,   480,     0,     0,     0,     0,    42,   199,    67,
       0,     0,    44,    69,    45,    46,    47,    48,    49,    50,
       0,    51,    52,    53,    54,    55,    56,    57,     0,     0,
      58,   256,    39,     0,    59,    60,    61,    62,    63,    64,
      40,    66,     0,     0,     0,    68,     0,     0,     0,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
     256,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      66,     0,     0,     0,    68,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   156,   157,
     158,    39,     0,     0,     0,   159,     0,     0,     0,    40,
       0,     0,     0,     0,     0,   243,     0,     0,     0,     0,
       0,    42,     0,   244,     0,    66,    44,   160,    45,    46,
      47,    48,    49,    50,    39,    51,    52,    53,    54,    55,
      56,    57,    40,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,    39,     0,     0,     0,     0,     0,     0,     0,    40,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    42,     0,     0,    66,   116,    44,     0,    45,    46,
      47,    48,    49,    50,    39,    51,    52,    53,    54,    55,
      56,    57,    40,     0,    58,     0,     0,   243,    59,    60,
      61,    62,    63,    64,    42,     0,     0,    66,     0,     0,
      39,    45,    46,    47,   186,   187,   188,     0,    40,     0,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
      42,    59,    60,    61,    62,    63,    64,    45,    46,    47,
     186,   187,   188,    39,     0,     0,    53,    54,    55,    56,
      57,    40,     0,    58,     0,     0,     0,    59,    60,    61,
      62,    63,    64,    42,    66,     0,     0,     0,     0,     0,
      45,    46,    47,   186,   187,   188,     0,   604,     0,    53,
      54,    55,    56,    57,     0,     0,    58,   605,     0,     0,
      59,    60,    61,    62,    63,    64,     0,   199,     0,     0,
       0,     0,     0,   705,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   605,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   199,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   199
  };

  const short
  parser::yycheck_[] =
  {
      29,    89,    29,    89,    41,    65,   108,   101,   195,   196,
     259,   205,   100,   226,   100,    34,    88,   100,   229,   227,
      65,    93,   151,   170,   103,   104,   108,   102,   182,   430,
     109,   110,   132,   112,   314,   222,    89,   504,    34,   132,
     111,   101,   406,     1,   113,   114,   115,     1,   113,   114,
     115,    21,   115,   590,   114,   134,   113,     1,   115,    27,
     114,   310,     5,     1,    27,    12,    77,   196,   275,   114,
      77,   351,    22,    88,   438,   352,   695,    77,   107,    19,
     680,   103,   182,   100,   100,    77,   374,   132,   108,   182,
      80,   275,   229,     0,   114,   383,   113,   113,    80,   718,
     194,    51,    52,   100,   680,   101,   128,   114,   315,   195,
     196,   180,   198,   185,   129,   180,    84,   128,   108,   205,
     180,    84,   114,   723,   210,   279,   180,   314,   128,    99,
     324,   315,   114,   196,   197,   180,   222,   182,   605,    78,
     169,   170,   169,   170,   102,    84,    84,   723,   102,   725,
     236,   223,   224,    79,   226,   113,   557,   107,   102,   113,
     107,   698,    84,   103,   102,    80,   443,   104,   118,   117,
     103,   198,   122,   107,   107,   114,   109,   114,   100,   279,
     128,   113,   108,   243,   395,   118,   279,   266,   114,   122,
     269,   378,   271,   265,   120,    14,   256,   260,   194,   114,
     108,   103,   566,   108,   128,   107,   114,   109,   345,   114,
     677,   256,    99,   110,   101,   284,   118,   114,   237,   238,
     306,   292,    77,   436,   253,   313,   413,   313,   314,   172,
     173,   108,   445,   327,   279,   118,    27,   114,   324,   122,
      79,   237,   238,   110,   107,    65,    66,   114,   244,   328,
     310,    90,    91,    92,   391,   118,   107,    60,    97,   122,
     313,    90,    91,    92,    77,   351,   630,   118,    97,   110,
     113,   122,    81,   114,   411,    84,   115,   108,   307,   308,
     119,   120,   110,   110,   123,   124,   114,   114,   114,   377,
     119,   377,   378,   430,   377,   449,   110,   326,    77,   318,
     114,   665,   107,    34,   584,   585,   385,    79,    86,   384,
     410,    89,   113,   500,    86,   528,    88,   410,    90,   506,
     107,   508,    81,   510,   426,    84,    79,   413,    80,    81,
     707,   327,   519,   710,   103,    88,   108,    90,   587,    99,
     412,   101,   114,   490,   426,   128,   418,   119,   120,   449,
      71,   123,   124,    99,   562,   101,   449,   429,   430,    48,
      49,   433,   464,   435,   436,   410,   119,   120,   122,   123,
     123,   124,   444,   445,   446,    99,    99,   446,   101,   448,
     129,   446,   451,   448,   453,    84,   451,    86,   453,   446,
      87,   448,   114,   128,   451,   675,   453,   114,   108,    90,
      91,    92,   108,   590,   449,   108,    97,   501,   108,   503,
     108,   598,   108,    24,   500,    23,    89,   604,   504,   630,
     506,   550,   508,   636,   510,    79,   563,   114,   119,    27,
     113,   680,   123,   519,    88,    84,    90,    91,    92,    72,
      73,    74,    75,    97,    15,    77,    77,    77,    77,    86,
     129,   523,   129,   525,   648,   527,   528,   494,   530,    81,
      79,   490,    86,   490,    77,   119,   120,   114,   464,   123,
     124,    90,    91,    92,   723,    80,   555,   104,    97,   104,
     114,    77,   108,   128,   128,   557,   680,   550,   551,    77,
     108,   108,   108,   108,   108,   104,   115,   100,   584,   585,
     119,   120,   574,   100,   590,   501,    89,   503,   114,   108,
     114,   698,   598,   707,   533,   115,   710,   108,   604,   605,
     114,    85,   115,    85,   114,    86,    25,   587,   597,   723,
     114,   725,   597,    79,    80,    81,   108,   533,    84,   115,
     597,   128,   128,    77,    90,    91,    92,   129,   104,    77,
     108,    97,    77,    77,   114,   627,   628,    80,   115,    80,
     115,   633,   648,   115,   636,   637,   114,   596,   115,   115,
     100,   114,    81,   119,   120,   128,   113,   123,   124,   128,
      81,   100,   104,   114,   104,    78,   115,    77,    29,   675,
       9,   677,     4,   108,   680,   108,   668,   669,   670,   687,
     114,   687,    48,    84,   687,    80,   708,    11,    53,    89,
     107,    49,   698,   118,   114,   169,   108,   108,   490,   108,
     657,   707,   649,   108,   710,   313,   238,   699,   716,   125,
     716,    15,   598,   716,   327,   210,   585,   723,   726,   725,
     726,   306,   413,   726,   222,   726,   645,   678,   707,   503,
     722,    34,   688,   651,   533,   316,   438,    34,   617,   115,
     636,   688,   436,   627,   350,   224,   695,   269,   695,     3,
       4,     5,     6,     7,     8,   525,    10,   596,    12,    13,
     427,   426,    16,    17,    18,    19,    20,    43,    22,   718,
      24,   718,    26,   587,    28,    29,   101,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,   198,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    -1,    79,    80,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    92,
      -1,    -1,    76,    -1,    97,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    91,    -1,    -1,
      -1,    -1,   115,    -1,    -1,    -1,   119,   120,    -1,   103,
     123,   124,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,
      -1,    -1,    -1,   127,   128,   129,   130,    -1,     3,     4,
       5,    -1,    -1,   137,   138,    10,    -1,    12,    13,    -1,
      -1,    16,    17,    18,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    -1,    79,    80,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    92,    -1,
      -1,    76,    -1,    97,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    91,    -1,    -1,    -1,
      -1,   115,    -1,    -1,    -1,   119,   120,    -1,   103,   123,
     124,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,
      -1,    -1,   127,   128,   129,   130,     1,    -1,     3,     4,
       5,    -1,   137,   138,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,   103,    -1,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,   113,    -1,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,     3,
       4,     5,   127,   128,   129,   130,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,
      -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,   107,   108,   109,    -1,    -1,    -1,    -1,
     114,   115,    -1,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,   127,   128,   129,   130,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,
      -1,    97,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
      -1,   107,    -1,   109,   110,    -1,    -1,    -1,   114,   115,
      -1,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
      -1,   127,   128,   129,   130,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,    97,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,   115,    -1,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,   127,
     128,   129,   130,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    92,    -1,    -1,    -1,    -1,    97,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,
      -1,    -1,    -1,    -1,    -1,   115,    -1,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,   127,   128,   129,
     130,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,
      -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,   113,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,
     122,    -1,     3,     4,     5,   127,   128,   129,   130,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    84,    -1,    -1,    -1,    88,    -1,    90,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,     3,     4,     5,   127,   128,   129,   130,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    -1,    -1,
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
      39,    40,    41,    42,    43,    44,    45,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,   101,    -1,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,     3,     4,     5,   127,   128,
     129,   130,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    25,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    47,
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
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      47,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,   113,    -1,    -1,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,     3,     4,     5,
     127,   128,   129,   130,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,     3,     4,
       5,   127,   128,   129,   130,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,     3,
       4,     5,   127,   128,   129,   130,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,
       3,     4,     5,   127,   128,   129,   130,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,     3,     4,     5,   127,   128,   129,   130,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,
     122,    -1,     3,     4,     5,   127,   128,   129,   130,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    86,    -1,    88,    -1,    -1,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,     3,     4,     5,   127,   128,   129,   130,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,
      -1,   121,   122,    -1,     3,     4,     5,   127,   128,   129,
     130,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,     3,     4,     5,   127,   128,
     129,   130,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      98,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,     3,     4,     5,   127,
     128,   129,   130,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,     4,
     107,    -1,   109,    -1,    -1,    -1,    -1,    12,    -1,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,    24,
     127,   128,   129,   130,    29,    -1,    31,    32,    33,    34,
      35,    36,    -1,    -1,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    86,    -1,    88,    -1,    90,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,    -1,
       4,    -1,   107,   108,   109,    -1,    -1,    -1,    12,   114,
     115,    -1,   117,   118,   119,   120,    -1,   122,   123,   124,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    -1,    -1,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,
     104,     4,    -1,   107,    -1,   109,    -1,    -1,    -1,    12,
      -1,   115,    -1,   117,   118,   119,   120,    -1,   122,   123,
     124,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    -1,    -1,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,
     103,    -1,     4,    -1,   107,    -1,   109,    -1,    -1,    -1,
      12,    -1,   115,    -1,   117,   118,   119,   120,    -1,   122,
     123,   124,    24,    -1,    -1,    -1,    -1,    -1,    -1,    31,
      32,    33,    34,    35,    36,    -1,    -1,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
      -1,   103,    -1,     4,    -1,   107,   108,   109,    -1,    -1,
      -1,    12,    -1,   115,    -1,   117,   118,   119,   120,    -1,
     122,   123,   124,    24,    -1,    -1,    -1,    -1,    -1,    -1,
      31,    32,    33,    34,    35,    36,    -1,    -1,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,
      -1,    -1,   103,     3,     4,    -1,   107,    -1,   109,    -1,
      -1,    -1,    12,    -1,   115,    -1,   117,   118,   119,   120,
      -1,   122,   123,   124,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    -1,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,     4,
      -1,    51,    52,    53,    54,    55,    56,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    -1,    -1,    31,    32,    33,    34,
      35,    36,    -1,    -1,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,
      -1,   121,   122,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    89,    -1,    91,    92,    -1,    -1,
      -1,     4,    -1,    -1,    99,    -1,    -1,    -1,   103,    12,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
     115,    24,   117,   118,    -1,   120,    -1,   122,    31,    32,
      33,    34,    35,    36,    -1,    -1,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,
     103,     4,    -1,    -1,   107,    -1,   109,    -1,    -1,    12,
      -1,    -1,   115,    -1,   117,   118,    -1,   120,    21,   122,
      -1,    24,    -1,    26,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,     4,    38,    39,    40,    41,    42,
      43,    44,    12,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    24,    -1,    26,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    -1,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    -1,    -1,   107,    -1,    -1,    -1,    78,    -1,
      -1,    -1,    -1,    24,   117,   118,    -1,    -1,   121,   122,
      31,    32,    33,    34,    35,    36,    -1,    -1,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    47,   107,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,   117,   118,    -1,
      -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,
      91,    92,    -1,    -1,    -1,     4,    -1,    -1,    99,    -1,
      -1,    -1,   103,    12,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    24,   117,   118,    -1,    -1,
      -1,   122,    31,    32,    33,    34,    35,    36,     4,    -1,
      -1,    40,    41,    42,    43,    44,    12,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    -1,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    78,    -1,    -1,    -1,    -1,    24,   117,   118,
      -1,    -1,    29,   122,    31,    32,    33,    34,    35,    36,
      -1,    38,    39,    40,    41,    42,    43,    44,    -1,    -1,
      47,   107,     4,    -1,    51,    52,    53,    54,    55,    56,
      12,   117,    -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    -1,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117,    -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,
      92,     4,    -1,    -1,    -1,    97,    -1,    -1,    -1,    12,
      -1,    -1,    -1,    -1,    -1,   107,    -1,    -1,    -1,    -1,
      -1,    24,    -1,   115,    -1,   117,    29,   119,    31,    32,
      33,    34,    35,    36,     4,    38,    39,    40,    41,    42,
      43,    44,    12,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    -1,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    24,    -1,    -1,   117,   118,    29,    -1,    31,    32,
      33,    34,    35,    36,     4,    38,    39,    40,    41,    42,
      43,    44,    12,    -1,    47,    -1,    -1,   107,    51,    52,
      53,    54,    55,    56,    24,    -1,    -1,   117,    -1,    -1,
       4,    31,    32,    33,    34,    35,    36,    -1,    12,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      24,    51,    52,    53,    54,    55,    56,    31,    32,    33,
      34,    35,    36,     4,    -1,    -1,    40,    41,    42,    43,
      44,    12,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    24,   117,    -1,    -1,    -1,    -1,    -1,
      31,    32,    33,    34,    35,    36,    -1,    97,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    47,   107,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,   117,    -1,    -1,
      -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   117,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117
  };

  const short
  parser::yystos_[] =
  {
       0,    21,    99,   140,   141,   142,   145,   118,   122,   322,
     146,   159,     0,   146,    65,    66,   143,   100,   113,   147,
     160,   161,     1,   102,   321,   103,   128,   190,   190,   107,
     148,    14,   162,   173,   174,   128,   191,    77,    77,     4,
      12,    21,    24,    26,    29,    31,    32,    33,    34,    35,
      36,    38,    39,    40,    41,    42,    43,    44,    47,    51,
      52,    53,    54,    55,    56,   107,   117,   118,   121,   122,
     149,   150,   151,   156,   157,   288,   291,   292,   306,   307,
     308,   314,    27,    60,   163,   113,   158,     3,     5,     6,
       7,     8,    10,    13,    16,    17,    18,    19,    20,    22,
      26,    28,    37,    45,    50,    57,    62,    63,    76,    82,
      88,    90,    91,   103,   107,   109,   118,   122,   127,   128,
     129,   130,   137,   138,   171,   175,   176,   177,   178,   182,
     231,   236,   241,   242,   246,   247,   248,   249,   277,   278,
     281,   282,   305,   306,   308,   316,   317,   320,   104,   114,
     322,   107,   287,   291,    79,    88,    90,    91,    92,    97,
     119,   120,   123,   124,   311,   312,   313,   315,   108,   114,
     107,   152,    99,   101,   144,    77,    34,   164,   113,    63,
     107,   239,   240,   242,   243,   245,    34,    35,    36,    79,
      88,    90,    91,    92,    99,   103,   107,   109,   115,   117,
     119,   120,   123,   124,   184,   198,   199,   202,   204,   206,
     208,   209,   211,   286,   287,   289,   293,   294,   302,   303,
     304,   314,   107,    99,   101,   270,    84,    99,   239,   263,
     264,   265,    72,    73,    74,    75,   179,    99,   101,   187,
     188,   204,   206,   107,   115,   296,   305,   308,   312,   270,
     247,   247,   103,   237,   238,   128,   107,   306,   103,   107,
     109,   279,   280,   282,   317,     5,    91,   247,   268,   269,
     247,   246,   247,    79,   104,   115,   120,   124,   239,   240,
     250,   252,   253,   284,   298,   299,   301,   310,   311,   313,
     318,   319,    90,   108,   114,   250,   251,   311,   312,   313,
     318,   323,   110,   250,   251,   323,    19,   237,   237,   129,
     170,   158,    71,   185,    80,   115,   192,   284,   297,   299,
     300,   309,   311,   312,    98,   247,    99,   114,    87,   128,
      88,   293,   108,   108,   108,   108,   108,   108,   151,    78,
     153,   154,   155,   156,   146,   146,    24,   166,   117,   128,
      23,    80,   297,   239,   196,   224,   225,   226,   305,    29,
     104,   200,   202,   204,    86,    88,   108,   200,   215,   293,
     323,   323,   291,   304,    27,   189,   211,    89,    86,   209,
     200,   214,   215,    20,    46,    91,   239,   267,   271,   272,
     273,   271,   256,   257,   273,   263,   113,   244,   265,   321,
      77,    77,    77,    77,   194,   200,   212,   186,   231,   232,
     241,   186,    15,    81,   312,   308,   129,   129,    86,    88,
     129,   306,    77,    77,   128,   319,   114,    80,   193,    99,
     101,   258,   247,    86,   268,    81,    84,   233,   234,   235,
       3,   307,   316,   297,    78,    84,   114,   104,   114,   240,
     108,   114,   108,   114,   108,   108,   108,   114,   110,   110,
     110,   212,   306,   306,   115,   172,   283,   295,   296,   319,
     128,   184,   194,   195,   200,   201,   307,   233,   242,   211,
      78,   274,   275,   276,   306,   196,   247,   108,   108,   108,
     114,   100,   321,   128,   165,    77,    77,   258,   194,   243,
      80,   114,   100,   114,   216,   104,    89,   108,    80,   108,
     114,   108,   110,   115,   115,   188,   204,   200,   108,   114,
     188,   270,   247,    85,   100,   113,   321,    86,   114,   100,
      25,   189,   100,   113,   321,   239,   200,   201,   108,   115,
     128,   128,   239,   129,   104,    77,    77,   108,   280,   103,
     107,   109,   285,   286,    77,    91,   100,   113,   239,   259,
     260,   261,   266,   259,   321,   239,   239,   256,   189,   235,
     115,   115,   115,   239,    25,   254,   255,   273,   239,   250,
     250,   250,   250,    77,    80,    80,   317,   114,    77,   128,
      80,    81,   183,   219,   115,   100,   114,    81,    80,   155,
     322,   200,   305,   226,    97,   107,   217,   302,   200,   200,
     218,   200,   200,   239,   272,   239,   273,   239,   232,   128,
     128,   104,   108,   110,   247,   259,   100,   113,    86,   262,
     263,   321,   189,    81,    47,   239,   114,    78,   194,   197,
     197,   115,   295,    77,   218,    29,   220,   221,   222,     9,
     227,   228,   229,   275,   250,   195,   114,     4,   167,   200,
     302,   108,   108,   244,   261,   239,   189,   239,    48,    49,
      48,   255,   273,   239,    77,   114,    77,   216,    84,   203,
     207,   210,   211,   223,    22,    51,    52,   107,   180,   230,
     290,   291,   229,   322,    12,   107,   168,   169,    80,    11,
     189,   239,   239,   239,   194,    97,   221,    89,   115,   210,
     283,   108,   204,   205,   213,   230,    53,   181,   107,   149,
     218,   239,    49,   207,   223,   207,   114,   108,   204,   149,
     108,   108,   239,   213,   108
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
     247,   247,   247,   247,   247,   247,   248,   248,   249,   249,
     249,   249,   249,   249,   249,   249,   249,   250,   250,   250,
     251,   251,   252,   252,   252,   252,   252,   252,   252,   253,
     253,   254,   254,   254,   254,   255,   255,   255,   255,   256,
     257,   257,   258,   258,   258,   258,   259,   259,   260,   260,
     260,   261,   262,   262,   263,   263,   264,   264,   265,   266,
     266,   267,   267,   268,   268,   269,   269,   270,   270,   271,
     271,   271,   271,   272,   272,   273,   273,   273,   274,   274,
     275,   275,   275,   276,   276,   277,   277,   278,   278,   279,
     279,   279,   280,   280,   281,   281,   281,   281,   282,   282,
     283,   283,   284,   284,   285,   285,   285,   286,   286,   286,
     286,   286,   287,   287,   287,   288,   288,   288,   288,   288,
     289,   289,   290,   291,   291,   292,   293,   293,   293,   294,
     294,   294,   294,   295,   295,   296,   296,   297,   297,   297,
     298,   298,   298,   299,   300,   300,   301,   301,   302,   303,
     304,   304,   304,   304,   304,   305,   305,   306,   306,   306,
     307,   307,   308,   308,   308,   308,   308,   308,   308,   308,
     309,   309,   310,   310,   311,   312,   312,   313,   313,   314,
     314,   314,   314,   314,   314,   314,   314,   314,   314,   314,
     314,   314,   314,   314,   314,   314,   314,   315,   315,   315,
     316,   316,   317,   318,   318,   319,   319,   320,   320,   320,
     320,   321,   321,   322,   322,   323,   323
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
       2,     3,     2,     1,     3,     2,     4,     4,     3,     8,
       2,     4,     2,     2,     4,     1,     4,     1,     1,     1,
       1,     3,     3,     3,     3,     3,     1,     1,     2,     2,
       3,     3,     1,     1,     2,     4,     3,     5,     3,     3,
       3,     3,     3,     1,     1,     2,     4,     4,     6,     1,
       3,     1,     3,     3,     2,     2,     1,     2,     3,     2,
       1,     2,     3,     2,     2,     1,     3,     2,     4,     1,
       2,     1,     2,     1,     2,     2,     1,     3,     3,     3,
       2,     1,     0,     1,     2,     3,     1,     2,     1,     0,
       3,     1,     1,     3,     1,     1,     1,     1,     3,     1,
       3,     1,     1,     3,     2,     3,     2,     3,     1,     2,
       1,     3,     1,     3,     1,     2,     2,     1,     3,     3,
       3,     2,     1,     3,     3,     1,     3,     3,     3,     3,
       1,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     1,     1,     1,
       1,     1,     1,     3,     1,     3,     1,     3,     1,     3,
       1,     1,     1,     1,     1,     1,     3,     1,     3,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     1
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
  "squals", "transformqual", "guardquals", "guardquals1", "altslist",
  "alts", "alts1", "alt", "alt_rhs", "gdpats", "ifgdpats", "gdpat", "pat",
  "bindpat", "apat", "apats1", "stmtlist", "stmts", "stmt", "qual",
  "fbinds", "fbinds1", "fbind", "qcon", "gen_qcon", "con", "con_list",
  "sysdcon_no_list", "sysdcon", "conop", "qconop", "gtycon", "ntgtycon",
  "oqtycon", "oqtycon_no_varcon", "qtyconop", "qtycondoc", "qtycon",
  "tycon", "qtyconsym", "tyconsym", "op", "varop", "qop", "qopm",
  "hole_op", "qvarop", "qvaropm", "tyvar", "tyvarop", "tyvarid", "var",
  "qvar", "qvarid", "varid", "qvarsym", "qvarsym_no_minus", "qvarsym1",
  "varsym", "varsym_no_minus", "special_id", "special_sym", "qconid",
  "conid", "qconsym", "consym", "literal", "close", "modid", "commas", YY_NULLPTR
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
    1084,  1085,  1086,  1087,  1089,  1090,  1091,  1092,  1093,  1094,
    1095,  1096,  1097,  1098,  1099,  1100,  1102,  1103,  1105,  1106,
    1107,  1108,  1109,  1110,  1111,  1112,  1113,  1118,  1119,  1120,
    1125,  1126,  1144,  1145,  1146,  1147,  1148,  1149,  1150,  1152,
    1153,  1165,  1166,  1167,  1168,  1170,  1171,  1172,  1173,  1176,
    1178,  1179,  1182,  1183,  1184,  1185,  1187,  1188,  1190,  1191,
    1192,  1194,  1196,  1197,  1199,  1200,  1202,  1203,  1205,  1207,
    1208,  1210,  1211,  1213,  1214,  1216,  1217,  1220,  1221,  1223,
    1224,  1225,  1226,  1231,  1232,  1234,  1235,  1236,  1241,  1242,
    1244,  1245,  1246,  1248,  1249,  1281,  1282,  1284,  1285,  1287,
    1288,  1289,  1291,  1292,  1294,  1295,  1296,  1297,  1299,  1300,
    1302,  1303,  1305,  1306,  1309,  1310,  1311,  1313,  1314,  1315,
    1316,  1317,  1319,  1320,  1321,  1323,  1324,  1325,  1326,  1327,
    1330,  1331,  1333,  1335,  1336,  1340,  1342,  1343,  1344,  1346,
    1347,  1348,  1349,  1354,  1355,  1357,  1358,  1360,  1361,  1362,
    1364,  1365,  1366,  1368,  1370,  1371,  1373,  1374,  1378,  1380,
    1382,  1383,  1384,  1385,  1386,  1389,  1390,  1392,  1393,  1394,
    1396,  1397,  1399,  1400,  1401,  1402,  1403,  1404,  1405,  1406,
    1408,  1409,  1411,  1412,  1414,  1416,  1417,  1419,  1420,  1422,
    1423,  1424,  1425,  1426,  1427,  1428,  1429,  1430,  1431,  1432,
    1433,  1434,  1435,  1436,  1437,  1438,  1439,  1441,  1442,  1443,
    1447,  1448,  1450,  1452,  1453,  1455,  1456,  1460,  1461,  1462,
    1463,  1468,  1471,  1475,  1476,  1478,  1479
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
#line 5982 "parser.cc"

#line 1488 "parser.y"


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

expression_ref make_rhs(const expression_ref& exp, const expression_ref& wherebinds)
{
    vector<expression_ref> e = {exp};
    if (wherebinds and wherebinds.size())
	e.push_back(wherebinds);
    return expression_ref{AST_node("rhs"), std::move(e)};
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

expression_ref make_as_pattern(const Located<Haskell::ID>& x, const expression_ref& pat)
{
    return Haskell::AsPattern(x,pat);
}

expression_ref make_lazy_pattern(const expression_ref& pat)
{
    return Haskell::LazyPattern(pat);
}

expression_ref make_strict_pattern(const expression_ref& pat)
{
    return new expression(AST_node("StrictPattern"), {pat});
}

expression_ref make_lambda(const vector<expression_ref>& pats, const expression_ref& body)
{
    auto e = pats;
    e.push_back(body);
    return new expression(AST_node("Lambda"), e);
}

expression_ref make_let(const expression_ref& binds, const expression_ref& body)
{
    return new expression(AST_node("Let"), {binds, body});
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

expression_ref yy_make_tuple(const vector<expression_ref>& elements)
{
    return Haskell::Tuple(elements);
}

expression_ref make_list(const vector<expression_ref>& elements)
{
    return Haskell::List(elements);
}

Haskell::Alts make_alts(const vector<expression_ref>& alts)
{
    return {alts};
}

expression_ref yy_make_alt(const expression_ref& pat, const expression_ref& alt_rhs)
{
    return AST_node("alt") + pat + alt_rhs;
}

expression_ref make_gdrhs(const vector<expression_ref>& guards, const expression_ref& wherebinds)
{
    vector<expression_ref> e = {expression_ref{AST_node("guards"),guards}};
    if (wherebinds and wherebinds.size())
	e.push_back(wherebinds);
    return expression_ref{AST_node("gdrhs"),std::move(e)};
}

expression_ref make_gdrh(const vector<expression_ref>& guardquals, const expression_ref& exp)
{
    return expression_ref(AST_node("gdrh"), {expression_ref(AST_node("guards"),guardquals),exp});
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

