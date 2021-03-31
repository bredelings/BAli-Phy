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
#line 90 "parser.y"

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
      case symbol_kind::S_infix: // infix
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
      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
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
      case symbol_kind::S_stmtlist: // stmtlist
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
      case symbol_kind::S_infix: // infix
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
      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
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
      case symbol_kind::S_stmtlist: // stmtlist
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
      case symbol_kind::S_infix: // infix
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
      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
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
      case symbol_kind::S_stmtlist: // stmtlist
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
      case symbol_kind::S_infix: // infix
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
      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
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
      case symbol_kind::S_stmtlist: // stmtlist
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
      case symbol_kind::S_infix: // infix
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
      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
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
      case symbol_kind::S_stmtlist: // stmtlist
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
#line 515 "parser.y"
             {drv.result = yystack_[0].value.as < expression_ref > ();}
#line 1661 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 532 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = make_module(yystack_[4].value.as < std::string > (),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 1667 "parser.cc"
    break;

  case 4: // module: body2
#line 533 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = make_module("Main",{},yystack_[0].value.as < expression_ref > ());}
#line 1673 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 535 "parser.y"
                                                                 {drv.push_module_context();}
#line 1679 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 543 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1685 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 544 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1691 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 546 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1697 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 547 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1703 "parser.cc"
    break;

  case 13: // top: semis top1
#line 550 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1709 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 552 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 1715 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 553 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 1721 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 554 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[0].value.as < std::vector<expression_ref> > (),{});}
#line 1727 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 562 "parser.y"
                                      {yylhs.value.as < expression_ref > () = make_exports(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 1733 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 563 "parser.y"
                                      {}
#line 1739 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 565 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 1745 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 567 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1751 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 568 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1757 "parser.cc"
    break;

  case 22: // export: qcname_ext export_subspec
#line 570 "parser.y"
                                      {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1763 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 571 "parser.y"
                                      {yylhs.value.as < expression_ref > () = AST_node("module",yystack_[0].value.as < std::string > ());}
#line 1769 "parser.cc"
    break;

  case 24: // export: "pattern" qcon
#line 572 "parser.y"
                                      {}
#line 1775 "parser.cc"
    break;

  case 27: // qcnames: %empty
#line 577 "parser.y"
                   {}
#line 1781 "parser.cc"
    break;

  case 28: // qcnames: qcnames1
#line 578 "parser.y"
                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 1787 "parser.cc"
    break;

  case 29: // qcnames1: qcnames1 "," qcname_ext_w_wildcard ","
#line 580 "parser.y"
                                                  {yylhs.value.as < std::vector<expression_ref> > () = yystack_[3].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ());}
#line 1793 "parser.cc"
    break;

  case 30: // qcnames1: qcname_ext_w_wildcard
#line 581 "parser.y"
                                              {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1799 "parser.cc"
    break;

  case 31: // qcname_ext_w_wildcard: qcname_ext
#line 583 "parser.y"
                                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1805 "parser.cc"
    break;

  case 32: // qcname_ext_w_wildcard: ".."
#line 584 "parser.y"
                                     {}
#line 1811 "parser.cc"
    break;

  case 33: // qcname_ext: qcname
#line 586 "parser.y"
                                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1817 "parser.cc"
    break;

  case 34: // qcname_ext: "type" oqtycon
#line 587 "parser.y"
                                     {}
#line 1823 "parser.cc"
    break;

  case 35: // qcname: qvar
#line 589 "parser.y"
                                     {yylhs.value.as < expression_ref > () = AST_node("qvar",yystack_[0].value.as < std::string > ()); }
#line 1829 "parser.cc"
    break;

  case 36: // qcname: oqtycon_no_varcon
#line 590 "parser.y"
                                     {yylhs.value.as < expression_ref > () = AST_node("qvar",yystack_[0].value.as < std::string > ()); }
#line 1835 "parser.cc"
    break;

  case 41: // importdecls: importdecls_semi importdecl
#line 600 "parser.y"
                                         { yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (), yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 1841 "parser.cc"
    break;

  case 42: // importdecls_semi: importdecls_semi importdecl semis1
#line 602 "parser.y"
                                                     { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 1847 "parser.cc"
    break;

  case 43: // importdecls_semi: %empty
#line 603 "parser.y"
                         { }
#line 1853 "parser.cc"
    break;

  case 44: // importdecl: "import" maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec
#line 605 "parser.y"
                                                                                            {
    std::vector<expression_ref> e;
    if (yystack_[4].value.as < bool > ()) e.push_back(AST_node("qualified"));
    e.push_back(String(yystack_[2].value.as < std::string > ()));
    if (yystack_[1].value.as < std::optional<std::string> > ()) e.push_back(AST_node("as", *yystack_[1].value.as < std::optional<std::string> > ()));
    if (yystack_[0].value.as < expression_ref > ()) e.push_back(yystack_[0].value.as < expression_ref > ());
    yylhs.value.as < expression_ref > () = expression_ref(new expression(AST_node("ImpDecl"),std::move(e)));
}
#line 1866 "parser.cc"
    break;

  case 45: // maybe_src: "{-# SOURCE" "#-}"
#line 614 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 1872 "parser.cc"
    break;

  case 46: // maybe_src: %empty
#line 615 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 1878 "parser.cc"
    break;

  case 47: // maybe_safe: "safe"
#line 617 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 1884 "parser.cc"
    break;

  case 48: // maybe_safe: %empty
#line 618 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 1890 "parser.cc"
    break;

  case 49: // maybe_pkg: "STRING"
#line 620 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 1896 "parser.cc"
    break;

  case 50: // maybe_pkg: %empty
#line 621 "parser.y"
                               { }
#line 1902 "parser.cc"
    break;

  case 51: // optqualified: "qualified"
#line 623 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 1908 "parser.cc"
    break;

  case 52: // optqualified: %empty
#line 624 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 1914 "parser.cc"
    break;

  case 53: // maybeas: "as" modid
#line 626 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 1920 "parser.cc"
    break;

  case 54: // maybeas: %empty
#line 627 "parser.y"
                               { }
#line 1926 "parser.cc"
    break;

  case 55: // maybeimpspec: impspec
#line 629 "parser.y"
                               { yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > (); }
#line 1932 "parser.cc"
    break;

  case 56: // maybeimpspec: %empty
#line 630 "parser.y"
                               { }
#line 1938 "parser.cc"
    break;

  case 57: // impspec: "(" exportlist ")"
#line 632 "parser.y"
                                      { yylhs.value.as < expression_ref > () = expression_ref{AST_node("only"),yystack_[1].value.as < std::vector<expression_ref> > ()}; }
#line 1944 "parser.cc"
    break;

  case 58: // impspec: "hiding" "(" exportlist ")"
#line 633 "parser.y"
                                      { yylhs.value.as < expression_ref > () = expression_ref{AST_node("hiding"),yystack_[1].value.as < std::vector<expression_ref> > ()}; }
#line 1950 "parser.cc"
    break;

  case 59: // prec: %empty
#line 638 "parser.y"
                   { }
#line 1956 "parser.cc"
    break;

  case 60: // prec: "INTEGER"
#line 639 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < int > (); }
#line 1962 "parser.cc"
    break;

  case 61: // infix: "infix"
#line 641 "parser.y"
                   { yylhs.value.as < std::string > () = "infix";  }
#line 1968 "parser.cc"
    break;

  case 62: // infix: "infixl"
#line 642 "parser.y"
                   { yylhs.value.as < std::string > () = "infixl"; }
#line 1974 "parser.cc"
    break;

  case 63: // infix: "infixr"
#line 643 "parser.y"
                   { yylhs.value.as < std::string > () = "infixr"; }
#line 1980 "parser.cc"
    break;

  case 64: // ops: ops "," op
#line 645 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 1986 "parser.cc"
    break;

  case 65: // ops: op
#line 646 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 1992 "parser.cc"
    break;

  case 66: // topdecls: topdecls_semi topdecl
#line 650 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 1998 "parser.cc"
    break;

  case 67: // topdecls_semi: topdecls_semi topdecl semis1
#line 652 "parser.y"
                                            { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2004 "parser.cc"
    break;

  case 68: // topdecls_semi: %empty
#line 653 "parser.y"
                                            { }
#line 2010 "parser.cc"
    break;

  case 69: // topdecl: cl_decl
#line 655 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2016 "parser.cc"
    break;

  case 70: // topdecl: ty_decl
#line 656 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2022 "parser.cc"
    break;

  case 71: // topdecl: inst_decl
#line 657 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2028 "parser.cc"
    break;

  case 72: // topdecl: "default" "(" comma_types0 ")"
#line 660 "parser.y"
                                               {}
#line 2034 "parser.cc"
    break;

  case 73: // topdecl: decl_no_th
#line 667 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2040 "parser.cc"
    break;

  case 74: // topdecl: infixexp_top
#line 669 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2046 "parser.cc"
    break;

  case 75: // topdecl: "builtin" var "INTEGER" "STRING" "STRING"
#line 670 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2052 "parser.cc"
    break;

  case 76: // topdecl: "builtin" var "INTEGER" "STRING"
#line 671 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2058 "parser.cc"
    break;

  case 77: // topdecl: "builtin" varop "INTEGER" "STRING" "STRING"
#line 672 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2064 "parser.cc"
    break;

  case 78: // topdecl: "builtin" varop "INTEGER" "STRING"
#line 673 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2070 "parser.cc"
    break;

  case 79: // cl_decl: "class" tycl_hdr wherebinds
#line 675 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_class_decl(yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().second,{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2076 "parser.cc"
    break;

  case 80: // ty_decl: "type" type "=" ctypedoc
#line 677 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_type_synonym({yystack_[2].location,yystack_[2].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2082 "parser.cc"
    break;

  case 81: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 678 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < Haskell::DataOrNewtype > (),yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().second,yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2088 "parser.cc"
    break;

  case 82: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig
#line 679 "parser.y"
                                                                           {}
#line 2094 "parser.cc"
    break;

  case 83: // inst_decl: "instance" overlap_pragma inst_type wherebinds
#line 684 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_instance_decl({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2100 "parser.cc"
    break;

  case 93: // data_or_newtype: "data"
#line 739 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::data;}
#line 2106 "parser.cc"
    break;

  case 94: // data_or_newtype: "newtype"
#line 740 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::newtype;}
#line 2112 "parser.cc"
    break;

  case 97: // tycl_hdr: context "=>" type
#line 752 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ()};}
#line 2118 "parser.cc"
    break;

  case 98: // tycl_hdr: type
#line 753 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {{},yystack_[0].value.as < expression_ref > ()};}
#line 2124 "parser.cc"
    break;

  case 114: // decls: decls ";" decl
#line 797 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2130 "parser.cc"
    break;

  case 115: // decls: decls ";"
#line 798 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2136 "parser.cc"
    break;

  case 116: // decls: decl
#line 799 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2142 "parser.cc"
    break;

  case 117: // decls: %empty
#line 800 "parser.y"
                        {}
#line 2148 "parser.cc"
    break;

  case 118: // decllist: "{" decls "}"
#line 802 "parser.y"
                                 {yylhs.value.as < expression_ref > () = expression_ref{AST_node("Decls"),yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 2154 "parser.cc"
    break;

  case 119: // decllist: "vocurly" decls close
#line 803 "parser.y"
                                 {yylhs.value.as < expression_ref > () = expression_ref{AST_node("Decls"),yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 2160 "parser.cc"
    break;

  case 120: // binds: decllist
#line 805 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2166 "parser.cc"
    break;

  case 121: // wherebinds: "where" binds
#line 807 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2172 "parser.cc"
    break;

  case 122: // wherebinds: %empty
#line 808 "parser.y"
                                 {}
#line 2178 "parser.cc"
    break;

  case 128: // opt_sig: %empty
#line 829 "parser.y"
                 {}
#line 2184 "parser.cc"
    break;

  case 129: // opt_sig: "::" sigtype
#line 830 "parser.y"
                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2190 "parser.cc"
    break;

  case 130: // opt_tyconsig: %empty
#line 832 "parser.y"
                     {}
#line 2196 "parser.cc"
    break;

  case 131: // opt_tyconsig: "::" gtycon
#line 833 "parser.y"
                     {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2202 "parser.cc"
    break;

  case 132: // sigtype: ctype
#line 835 "parser.y"
                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2208 "parser.cc"
    break;

  case 133: // sigtypedoc: ctypedoc
#line 837 "parser.y"
                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2214 "parser.cc"
    break;

  case 134: // sig_vars: sig_vars "," var
#line 839 "parser.y"
                           {yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 2220 "parser.cc"
    break;

  case 135: // sig_vars: var
#line 840 "parser.y"
                           {yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 2226 "parser.cc"
    break;

  case 136: // sigtypes1: sigtype
#line 842 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2232 "parser.cc"
    break;

  case 137: // sigtypes1: sigtypes1 "," sigtype
#line 843 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2238 "parser.cc"
    break;

  case 138: // strict_mark: strictness
#line 847 "parser.y"
                                            {yylhs.value.as < Haskell::StrictLazy > () = yystack_[0].value.as < Haskell::StrictLazy > ();}
#line 2244 "parser.cc"
    break;

  case 139: // strictness: "!"
#line 853 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::strict;}
#line 2250 "parser.cc"
    break;

  case 140: // strictness: "~"
#line 854 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::lazy;}
#line 2256 "parser.cc"
    break;

  case 141: // ctype: "forall" tv_bndrs "." ctype
#line 861 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_forall_type(yystack_[2].value.as < std::vector<expression_ref> > (), yystack_[0].value.as < expression_ref > ());}
#line 2262 "parser.cc"
    break;

  case 142: // ctype: context "=>" ctype
#line 862 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_constrained_type(yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ());}
#line 2268 "parser.cc"
    break;

  case 143: // ctype: type
#line 864 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2274 "parser.cc"
    break;

  case 144: // ctypedoc: ctype
#line 866 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2280 "parser.cc"
    break;

  case 145: // context: btype
#line 875 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(yystack_[0].value.as < expression_ref > ());}
#line 2286 "parser.cc"
    break;

  case 146: // context_no_ops: btype_no_ops
#line 877 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2292 "parser.cc"
    break;

  case 147: // type: btype
#line 879 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2298 "parser.cc"
    break;

  case 148: // type: btype "->" ctype
#line 880 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps({make_type_var("->"),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()});}
#line 2304 "parser.cc"
    break;

  case 149: // typedoc: type
#line 882 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2310 "parser.cc"
    break;

  case 150: // btype: tyapps
#line 885 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2316 "parser.cc"
    break;

  case 151: // btype_no_ops: atype_docs
#line 887 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2322 "parser.cc"
    break;

  case 152: // btype_no_ops: btype_no_ops atype_docs
#line 888 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2328 "parser.cc"
    break;

  case 153: // tyapps: tyapp
#line 890 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2334 "parser.cc"
    break;

  case 154: // tyapps: tyapps tyapp
#line 891 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2340 "parser.cc"
    break;

  case 155: // tyapp: atype
#line 893 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2346 "parser.cc"
    break;

  case 156: // tyapp: qtyconop
#line 894 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2352 "parser.cc"
    break;

  case 157: // tyapp: tyvarop
#line 895 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2358 "parser.cc"
    break;

  case 158: // atype_docs: atype
#line 901 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2364 "parser.cc"
    break;

  case 159: // atype: ntgtycon
#line 908 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2370 "parser.cc"
    break;

  case 160: // atype: tyvar
#line 909 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2376 "parser.cc"
    break;

  case 161: // atype: "*"
#line 910 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var("*");}
#line 2382 "parser.cc"
    break;

  case 162: // atype: strict_mark atype
#line 911 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_strict_lazy_type(yystack_[1].value.as < Haskell::StrictLazy > (),yystack_[0].value.as < expression_ref > ());}
#line 2388 "parser.cc"
    break;

  case 163: // atype: "{" fielddecls "}"
#line 912 "parser.y"
                                       {yylhs.value.as < expression_ref > () = expression_ref{AST_node("FieldDecls"),yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 2394 "parser.cc"
    break;

  case 164: // atype: "(" ")"
#line 913 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var("()");}
#line 2400 "parser.cc"
    break;

  case 165: // atype: "(" comma_types1 "," ctype ")"
#line 914 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<expression_ref> > ();ts.push_back(yystack_[1].value.as < expression_ref > ());yylhs.value.as < expression_ref > () = make_tuple_type(ts);}
#line 2406 "parser.cc"
    break;

  case 166: // atype: "[" ctype "]"
#line 920 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_list_type(yystack_[1].value.as < expression_ref > ());}
#line 2412 "parser.cc"
    break;

  case 167: // atype: "(" ctype ")"
#line 921 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2418 "parser.cc"
    break;

  case 168: // atype: "(" ctype "::" kind ")"
#line 922 "parser.y"
                                       {yylhs.value.as < expression_ref > () = expression_ref(AST_node("TypeOfKind"),{yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()});}
#line 2424 "parser.cc"
    break;

  case 169: // inst_type: sigtype
#line 925 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2430 "parser.cc"
    break;

  case 172: // comma_types0: comma_types1
#line 930 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2436 "parser.cc"
    break;

  case 173: // comma_types0: %empty
#line 931 "parser.y"
                                       {}
#line 2442 "parser.cc"
    break;

  case 174: // comma_types1: ctype
#line 933 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2448 "parser.cc"
    break;

  case 175: // comma_types1: comma_types1 "," ctype
#line 934 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2454 "parser.cc"
    break;

  case 176: // tv_bndrs: tv_bndrs tv_bndr
#line 941 "parser.y"
                               {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2460 "parser.cc"
    break;

  case 177: // tv_bndrs: %empty
#line 942 "parser.y"
                               {}
#line 2466 "parser.cc"
    break;

  case 178: // tv_bndr: tyvar
#line 944 "parser.y"
                                    {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2472 "parser.cc"
    break;

  case 179: // tv_bndr: "(" tyvar "::" kind ")"
#line 945 "parser.y"
                                    {yylhs.value.as < expression_ref > () = new expression(AST_node("type_of_kind"),{make_type_var(yystack_[3].value.as < std::string > ()),yystack_[1].value.as < expression_ref > ()});}
#line 2478 "parser.cc"
    break;

  case 180: // kind: ctype
#line 963 "parser.y"
             {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2484 "parser.cc"
    break;

  case 181: // constrs: "=" constrs1
#line 969 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2490 "parser.cc"
    break;

  case 182: // constrs1: constrs1 "|" constr
#line 971 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2496 "parser.cc"
    break;

  case 183: // constrs1: constr
#line 972 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2502 "parser.cc"
    break;

  case 184: // constr: forall context_no_ops "=>" constr_stuff
#line 974 "parser.y"
                                                {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2508 "parser.cc"
    break;

  case 185: // constr: forall constr_stuff
#line 975 "parser.y"
                                                {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2514 "parser.cc"
    break;

  case 186: // forall: "forall" tv_bndrs "."
#line 977 "parser.y"
                                {if (yystack_[1].value.as < std::vector<expression_ref> > ().size()>1) yylhs.value.as < expression_ref > () = make_tv_bndrs(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2520 "parser.cc"
    break;

  case 187: // forall: %empty
#line 978 "parser.y"
                                {}
#line 2526 "parser.cc"
    break;

  case 188: // constr_stuff: btype_no_ops
#line 980 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2532 "parser.cc"
    break;

  case 189: // constr_stuff: btype_no_ops conop btype_no_ops
#line 981 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps({make_type_var(yystack_[1].value.as < std::string > ()),make_tyapps(yystack_[2].value.as < std::vector<expression_ref> > ()),make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 2538 "parser.cc"
    break;

  case 190: // fielddecls: %empty
#line 983 "parser.y"
                                {}
#line 2544 "parser.cc"
    break;

  case 191: // fielddecls: fielddecls1
#line 984 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2550 "parser.cc"
    break;

  case 192: // fielddecls1: fielddecls1 "," fielddecl
#line 986 "parser.y"
                                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2556 "parser.cc"
    break;

  case 193: // fielddecls1: fielddecl
#line 987 "parser.y"
                                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2562 "parser.cc"
    break;

  case 194: // fielddecl: sig_vars "::" ctype
#line 989 "parser.y"
                                        {yylhs.value.as < Haskell::FieldDecl > () = make_field_decl(yystack_[2].value.as < std::vector<std::string> > (),yystack_[0].value.as < expression_ref > ());}
#line 2568 "parser.cc"
    break;

  case 205: // decl_no_th: sigdecl
#line 1008 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2574 "parser.cc"
    break;

  case 206: // decl_no_th: "!" aexp rhs
#line 1010 "parser.y"
                              {yylhs.value.as < expression_ref > () = new expression(AST_node("Decl:Strict"),{(yystack_[1].value.as < expression_ref > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2580 "parser.cc"
    break;

  case 207: // decl_no_th: infixexp_top opt_sig rhs
#line 1012 "parser.y"
                              {yylhs.value.as < expression_ref > () = new expression(AST_node("Decl"),{make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2586 "parser.cc"
    break;

  case 208: // decl_no_th: pattern_synonym_decl
#line 1013 "parser.y"
                              {}
#line 2592 "parser.cc"
    break;

  case 209: // decl: decl_no_th
#line 1016 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2598 "parser.cc"
    break;

  case 210: // rhs: "=" exp wherebinds
#line 1020 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_rhs(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2604 "parser.cc"
    break;

  case 211: // rhs: gdrhs wherebinds
#line 1021 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2610 "parser.cc"
    break;

  case 212: // gdrhs: gdrhs gdrh
#line 1023 "parser.y"
                              {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2616 "parser.cc"
    break;

  case 213: // gdrhs: gdrh
#line 1024 "parser.y"
                              {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2622 "parser.cc"
    break;

  case 214: // gdrh: "|" guardquals "=" exp
#line 1028 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2628 "parser.cc"
    break;

  case 215: // sigdecl: infixexp_top "::" sigtypedoc
#line 1030 "parser.y"
                                             { yylhs.value.as < expression_ref > () = expression_ref(AST_node("Decl:sigtype"),{make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2634 "parser.cc"
    break;

  case 216: // sigdecl: var "," sig_vars "::" sigtypedoc
#line 1031 "parser.y"
                                          {}
#line 2640 "parser.cc"
    break;

  case 217: // sigdecl: infix prec ops
#line 1032 "parser.y"
                         { yylhs.value.as < expression_ref > () = make_infix(yystack_[2].value.as < std::string > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()); }
#line 2646 "parser.cc"
    break;

  case 218: // sigdecl: pattern_synonym_sig
#line 1033 "parser.y"
                             {}
#line 2652 "parser.cc"
    break;

  case 219: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1034 "parser.y"
                                                    {}
#line 2658 "parser.cc"
    break;

  case 220: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1035 "parser.y"
                                            {}
#line 2664 "parser.cc"
    break;

  case 221: // sigdecl: "{-# SCC" qvar "#-}"
#line 1036 "parser.y"
                              {}
#line 2670 "parser.cc"
    break;

  case 222: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1037 "parser.y"
                                     {}
#line 2676 "parser.cc"
    break;

  case 223: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1038 "parser.y"
                                                               {}
#line 2682 "parser.cc"
    break;

  case 224: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1039 "parser.y"
                                                                      {}
#line 2688 "parser.cc"
    break;

  case 225: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1040 "parser.y"
                                                     {}
#line 2694 "parser.cc"
    break;

  case 230: // exp: infixexp "::" sigtype
#line 1050 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_typed_exp(make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()); }
#line 2700 "parser.cc"
    break;

  case 231: // exp: infixexp
#line 1051 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 2706 "parser.cc"
    break;

  case 232: // infixexp: exp10
#line 1053 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2712 "parser.cc"
    break;

  case 233: // infixexp: infixexp qop exp10
#line 1054 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_id(yystack_[1].location,yystack_[1].value.as < std::string > ())); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2718 "parser.cc"
    break;

  case 234: // infixexp_top: exp10_top
#line 1056 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2724 "parser.cc"
    break;

  case 235: // infixexp_top: infixexp_top qop exp10_top
#line 1057 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_id(yystack_[1].location,yystack_[1].value.as < std::string > ())); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2730 "parser.cc"
    break;

  case 236: // exp10_top: "-" fexp
#line 1059 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_minus(make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2736 "parser.cc"
    break;

  case 237: // exp10_top: "{-# CORE" "STRING" "#-}"
#line 1060 "parser.y"
                                   {}
#line 2742 "parser.cc"
    break;

  case 238: // exp10_top: fexp
#line 1061 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2748 "parser.cc"
    break;

  case 239: // exp10: exp10_top
#line 1063 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2754 "parser.cc"
    break;

  case 240: // exp10: scc_annot exp
#line 1064 "parser.y"
                                 {}
#line 2760 "parser.cc"
    break;

  case 245: // fexp: fexp aexp
#line 1075 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2766 "parser.cc"
    break;

  case 246: // fexp: fexp "TYPEAPP" atype
#line 1076 "parser.y"
                                 {}
#line 2772 "parser.cc"
    break;

  case 247: // fexp: "static" aexp
#line 1077 "parser.y"
                                 {}
#line 2778 "parser.cc"
    break;

  case 248: // fexp: aexp
#line 1078 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2784 "parser.cc"
    break;

  case 249: // aexp: qvar "@" aexp
#line 1080 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_as_pattern(make_id(yystack_[2].location,yystack_[2].value.as < std::string > ()),yystack_[0].value.as < expression_ref > ());}
#line 2790 "parser.cc"
    break;

  case 250: // aexp: "~" aexp
#line 1081 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lazy_pattern(yystack_[0].value.as < expression_ref > ());}
#line 2796 "parser.cc"
    break;

  case 251: // aexp: "\\" apats1 "->" exp
#line 1082 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lambda(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2802 "parser.cc"
    break;

  case 252: // aexp: "let" binds "in" exp
#line 1083 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_let(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2808 "parser.cc"
    break;

  case 253: // aexp: "\\" "case" altslist
#line 1084 "parser.y"
                                 {}
#line 2814 "parser.cc"
    break;

  case 254: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1085 "parser.y"
                                                       {yylhs.value.as < expression_ref > () = make_if(yystack_[6].value.as < expression_ref > (),yystack_[3].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2820 "parser.cc"
    break;

  case 255: // aexp: "if" ifgdpats
#line 1086 "parser.y"
                                 {}
#line 2826 "parser.cc"
    break;

  case 256: // aexp: "case" exp "of" altslist
#line 1087 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_case(yystack_[2].value.as < expression_ref > (),make_alts(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2832 "parser.cc"
    break;

  case 257: // aexp: "do" stmtlist
#line 1088 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_do(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2838 "parser.cc"
    break;

  case 258: // aexp: "mdo" stmtlist
#line 1089 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_mdo(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2844 "parser.cc"
    break;

  case 259: // aexp: "proc" aexp "->" exp
#line 1090 "parser.y"
                                 {}
#line 2850 "parser.cc"
    break;

  case 260: // aexp: aexp1
#line 1091 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2856 "parser.cc"
    break;

  case 261: // aexp1: aexp1 "{" fbinds "}"
#line 1093 "parser.y"
                              {}
#line 2862 "parser.cc"
    break;

  case 262: // aexp1: aexp2
#line 1094 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2868 "parser.cc"
    break;

  case 263: // aexp2: qvar
#line 1096 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_id(yystack_[0].location,yystack_[0].value.as < std::string > ());}
#line 2874 "parser.cc"
    break;

  case 264: // aexp2: qcon
#line 1097 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_id(yystack_[0].location,yystack_[0].value.as < std::string > ());}
#line 2880 "parser.cc"
    break;

  case 265: // aexp2: literal
#line 1098 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2886 "parser.cc"
    break;

  case 266: // aexp2: "(" texp ")"
#line 1099 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2892 "parser.cc"
    break;

  case 267: // aexp2: "(" tup_exprs ")"
#line 1100 "parser.y"
                              {yylhs.value.as < expression_ref > () = yy_make_tuple(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2898 "parser.cc"
    break;

  case 268: // aexp2: "(#" texp "#)"
#line 1101 "parser.y"
                              {}
#line 2904 "parser.cc"
    break;

  case 269: // aexp2: "(#" tup_exprs "#)"
#line 1102 "parser.y"
                              {}
#line 2910 "parser.cc"
    break;

  case 270: // aexp2: "[" list "]"
#line 1103 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2916 "parser.cc"
    break;

  case 271: // aexp2: "_"
#line 1104 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::WildcardPattern();}
#line 2922 "parser.cc"
    break;

  case 272: // texp: exp
#line 1109 "parser.y"
                      {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2928 "parser.cc"
    break;

  case 273: // texp: infixexp qop
#line 1110 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("LeftSection"),{make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ()),make_id(yystack_[0].location,yystack_[0].value.as < std::string > ())});}
#line 2934 "parser.cc"
    break;

  case 274: // texp: qopm infixexp
#line 1111 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("RightSection"),{make_id(yystack_[1].location,yystack_[1].value.as < std::string > ()),make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 2940 "parser.cc"
    break;

  case 275: // tup_exprs: tup_exprs "," texp
#line 1116 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2946 "parser.cc"
    break;

  case 276: // tup_exprs: texp "," texp
#line 1117 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2952 "parser.cc"
    break;

  case 277: // list: texp
#line 1135 "parser.y"
                                 { yylhs.value.as < expression_ref > () = make_list({yystack_[0].value.as < expression_ref > ()}); }
#line 2958 "parser.cc"
    break;

  case 278: // list: lexps
#line 1136 "parser.y"
                                 { yylhs.value.as < expression_ref > () = make_list(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 2964 "parser.cc"
    break;

  case 279: // list: texp ".."
#line 1137 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFrom"),{yystack_[1].value.as < expression_ref > ()}); }
#line 2970 "parser.cc"
    break;

  case 280: // list: texp "," exp ".."
#line 1138 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThen"),{yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()}); }
#line 2976 "parser.cc"
    break;

  case 281: // list: texp ".." exp
#line 1139 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromTo"),{yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 2982 "parser.cc"
    break;

  case 282: // list: texp "," exp ".." exp
#line 1140 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThenTo"),{yystack_[4].value.as < expression_ref > (),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 2988 "parser.cc"
    break;

  case 283: // list: texp "|" squals
#line 1141 "parser.y"
                                 { auto quals = yystack_[0].value.as < std::vector<expression_ref> > (); quals.push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < expression_ref > () = expression_ref(AST_node("ListComprehension"),quals); }
#line 2994 "parser.cc"
    break;

  case 284: // lexps: lexps "," texp
#line 1143 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3000 "parser.cc"
    break;

  case 285: // lexps: texp "," texp
#line 1144 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3006 "parser.cc"
    break;

  case 286: // squals: squals "," transformqual
#line 1156 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3012 "parser.cc"
    break;

  case 287: // squals: squals "," qual
#line 1157 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3018 "parser.cc"
    break;

  case 288: // squals: transformqual
#line 1158 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3024 "parser.cc"
    break;

  case 289: // squals: qual
#line 1159 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3030 "parser.cc"
    break;

  case 290: // transformqual: "then" exp
#line 1161 "parser.y"
                                                    {}
#line 3036 "parser.cc"
    break;

  case 291: // transformqual: "then" exp "by" exp
#line 1162 "parser.y"
                                                    {}
#line 3042 "parser.cc"
    break;

  case 292: // transformqual: "then" "group" "using" exp
#line 1163 "parser.y"
                                                    {}
#line 3048 "parser.cc"
    break;

  case 293: // transformqual: "then" "group" "by" exp "using" exp
#line 1164 "parser.y"
                                                    {}
#line 3054 "parser.cc"
    break;

  case 294: // guardquals: guardquals1
#line 1167 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3060 "parser.cc"
    break;

  case 295: // guardquals1: guardquals1 "," qual
#line 1169 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > ();yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3066 "parser.cc"
    break;

  case 296: // guardquals1: qual
#line 1170 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3072 "parser.cc"
    break;

  case 297: // altslist: "{" alts "}"
#line 1173 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3078 "parser.cc"
    break;

  case 298: // altslist: "vocurly" alts close
#line 1174 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3084 "parser.cc"
    break;

  case 299: // altslist: "{" "}"
#line 1175 "parser.y"
                                 {}
#line 3090 "parser.cc"
    break;

  case 300: // altslist: "vocurly" close
#line 1176 "parser.y"
                                 {}
#line 3096 "parser.cc"
    break;

  case 301: // alts: alts1
#line 1178 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3102 "parser.cc"
    break;

  case 302: // alts: ";" alts
#line 1179 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3108 "parser.cc"
    break;

  case 303: // alts1: alts1 ";" alt
#line 1181 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3114 "parser.cc"
    break;

  case 304: // alts1: alts1 ";"
#line 1182 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3120 "parser.cc"
    break;

  case 305: // alts1: alt
#line 1183 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3126 "parser.cc"
    break;

  case 306: // alt: pat alt_rhs
#line 1185 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yy_make_alt(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3132 "parser.cc"
    break;

  case 307: // alt_rhs: "->" exp wherebinds
#line 1187 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_rhs(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3138 "parser.cc"
    break;

  case 308: // alt_rhs: gdpats wherebinds
#line 1188 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3144 "parser.cc"
    break;

  case 309: // gdpats: gdpats gdpat
#line 1190 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3150 "parser.cc"
    break;

  case 310: // gdpats: gdpat
#line 1191 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3156 "parser.cc"
    break;

  case 311: // ifgdpats: "{" gdpats "}"
#line 1193 "parser.y"
                                 {}
#line 3162 "parser.cc"
    break;

  case 312: // ifgdpats: gdpats close
#line 1194 "parser.y"
                                 {}
#line 3168 "parser.cc"
    break;

  case 313: // gdpat: "|" guardquals "->" exp
#line 1196 "parser.y"
                                 {yylhs.value.as < expression_ref > ()=make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3174 "parser.cc"
    break;

  case 314: // pat: exp
#line 1198 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3180 "parser.cc"
    break;

  case 315: // pat: "!" aexp
#line 1199 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3186 "parser.cc"
    break;

  case 316: // bindpat: exp
#line 1201 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3192 "parser.cc"
    break;

  case 317: // bindpat: "!" aexp
#line 1202 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3198 "parser.cc"
    break;

  case 318: // apat: aexp
#line 1204 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3204 "parser.cc"
    break;

  case 319: // apat: "!" aexp
#line 1205 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3210 "parser.cc"
    break;

  case 320: // apats1: apats1 apat
#line 1207 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3216 "parser.cc"
    break;

  case 321: // apats1: apat
#line 1208 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3222 "parser.cc"
    break;

  case 322: // stmtlist: "{" stmts "}"
#line 1211 "parser.y"
                               {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3228 "parser.cc"
    break;

  case 323: // stmtlist: "vocurly" stmts close
#line 1212 "parser.y"
                               {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3234 "parser.cc"
    break;

  case 324: // stmts: stmts ";" stmt
#line 1214 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3240 "parser.cc"
    break;

  case 325: // stmts: stmts ";"
#line 1215 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3246 "parser.cc"
    break;

  case 326: // stmts: stmt
#line 1216 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3252 "parser.cc"
    break;

  case 327: // stmts: %empty
#line 1217 "parser.y"
                       {}
#line 3258 "parser.cc"
    break;

  case 328: // stmt: qual
#line 1222 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3264 "parser.cc"
    break;

  case 329: // stmt: "rec" stmtlist
#line 1223 "parser.y"
                        {yylhs.value.as < expression_ref > () = new expression(AST_node("Rec"),{yystack_[0].value.as < std::vector<expression_ref> > ()});}
#line 3270 "parser.cc"
    break;

  case 330: // qual: bindpat "<-" exp
#line 1225 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::PatQual(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3276 "parser.cc"
    break;

  case 331: // qual: exp
#line 1226 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::SimpleQual(yystack_[0].value.as < expression_ref > ());}
#line 3282 "parser.cc"
    break;

  case 332: // qual: "let" binds
#line 1227 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::LetQual(yystack_[0].value.as < expression_ref > ());}
#line 3288 "parser.cc"
    break;

  case 340: // qcon: gen_qcon
#line 1272 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3294 "parser.cc"
    break;

  case 341: // qcon: sysdcon
#line 1273 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3300 "parser.cc"
    break;

  case 342: // gen_qcon: qconid
#line 1275 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3306 "parser.cc"
    break;

  case 343: // gen_qcon: "(" qconsym ")"
#line 1276 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3312 "parser.cc"
    break;

  case 344: // con: conid
#line 1278 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3318 "parser.cc"
    break;

  case 345: // con: "(" consym ")"
#line 1279 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3324 "parser.cc"
    break;

  case 346: // con: sysdcon
#line 1280 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3330 "parser.cc"
    break;

  case 349: // sysdcon_no_list: "(" ")"
#line 1285 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 3336 "parser.cc"
    break;

  case 350: // sysdcon_no_list: "(" commas ")"
#line 1286 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3342 "parser.cc"
    break;

  case 351: // sysdcon_no_list: "(#" "#)"
#line 1287 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 3348 "parser.cc"
    break;

  case 352: // sysdcon_no_list: "(#" commas "#)"
#line 1288 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3354 "parser.cc"
    break;

  case 353: // sysdcon: sysdcon_no_list
#line 1290 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3360 "parser.cc"
    break;

  case 354: // sysdcon: "[" "]"
#line 1291 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 3366 "parser.cc"
    break;

  case 355: // conop: consym
#line 1293 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3372 "parser.cc"
    break;

  case 356: // conop: "`" conid "`"
#line 1294 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3378 "parser.cc"
    break;

  case 357: // qconop: qconsym
#line 1296 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3384 "parser.cc"
    break;

  case 358: // qconop: "`" qconid "`"
#line 1297 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3390 "parser.cc"
    break;

  case 359: // gtycon: ntgtycon
#line 1300 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3396 "parser.cc"
    break;

  case 360: // gtycon: "(" ")"
#line 1301 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 3402 "parser.cc"
    break;

  case 361: // gtycon: "(#" "#)"
#line 1302 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 3408 "parser.cc"
    break;

  case 362: // ntgtycon: oqtycon
#line 1304 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3414 "parser.cc"
    break;

  case 363: // ntgtycon: "(" commas ")"
#line 1305 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3420 "parser.cc"
    break;

  case 364: // ntgtycon: "(#" commas "#)"
#line 1306 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3426 "parser.cc"
    break;

  case 365: // ntgtycon: "(" "->" ")"
#line 1307 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 3432 "parser.cc"
    break;

  case 366: // ntgtycon: "[" "]"
#line 1308 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 3438 "parser.cc"
    break;

  case 367: // oqtycon: qtycon
#line 1310 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3444 "parser.cc"
    break;

  case 368: // oqtycon: "(" qtyconsym ")"
#line 1311 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3450 "parser.cc"
    break;

  case 369: // oqtycon: "(" "~" ")"
#line 1312 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3456 "parser.cc"
    break;

  case 370: // oqtycon_no_varcon: qtycon
#line 1314 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3462 "parser.cc"
    break;

  case 371: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1315 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3468 "parser.cc"
    break;

  case 372: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1316 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3474 "parser.cc"
    break;

  case 373: // oqtycon_no_varcon: "(" ":" ")"
#line 1317 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 3480 "parser.cc"
    break;

  case 374: // oqtycon_no_varcon: "(" "~" ")"
#line 1318 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3486 "parser.cc"
    break;

  case 375: // qtyconop: qtyconsym
#line 1321 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3492 "parser.cc"
    break;

  case 376: // qtyconop: "`" qtycon "`"
#line 1322 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3498 "parser.cc"
    break;

  case 377: // qtycondoc: qtycon
#line 1324 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3504 "parser.cc"
    break;

  case 378: // qtycon: "QCONID"
#line 1326 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3510 "parser.cc"
    break;

  case 379: // qtycon: tycon
#line 1327 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3516 "parser.cc"
    break;

  case 380: // tycon: "CONID"
#line 1331 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3522 "parser.cc"
    break;

  case 381: // qtyconsym: "QCONSYM"
#line 1333 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3528 "parser.cc"
    break;

  case 382: // qtyconsym: "QVARSYM"
#line 1334 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3534 "parser.cc"
    break;

  case 383: // qtyconsym: tyconsym
#line 1335 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3540 "parser.cc"
    break;

  case 384: // tyconsym: "CONSYM"
#line 1337 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3546 "parser.cc"
    break;

  case 385: // tyconsym: "VARSYM"
#line 1338 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3552 "parser.cc"
    break;

  case 386: // tyconsym: ":"
#line 1339 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 3558 "parser.cc"
    break;

  case 387: // tyconsym: "-"
#line 1340 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 3564 "parser.cc"
    break;

  case 388: // op: varop
#line 1345 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3570 "parser.cc"
    break;

  case 389: // op: conop
#line 1346 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3576 "parser.cc"
    break;

  case 390: // varop: varsym
#line 1348 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3582 "parser.cc"
    break;

  case 391: // varop: "`" varid "`"
#line 1349 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3588 "parser.cc"
    break;

  case 392: // qop: qvarop
#line 1351 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3594 "parser.cc"
    break;

  case 393: // qop: qconop
#line 1352 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3600 "parser.cc"
    break;

  case 394: // qop: hole_op
#line 1353 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3606 "parser.cc"
    break;

  case 395: // qopm: qvaropm
#line 1355 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3612 "parser.cc"
    break;

  case 396: // qopm: qconop
#line 1356 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3618 "parser.cc"
    break;

  case 397: // qopm: hole_op
#line 1357 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3624 "parser.cc"
    break;

  case 398: // hole_op: "`" "_" "`"
#line 1359 "parser.y"
                      { yylhs.value.as < std::string > () = "_"; }
#line 3630 "parser.cc"
    break;

  case 399: // qvarop: qvarsym
#line 1361 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3636 "parser.cc"
    break;

  case 400: // qvarop: "`" qvarid "`"
#line 1362 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3642 "parser.cc"
    break;

  case 401: // qvaropm: qvarsym_no_minus
#line 1364 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 3648 "parser.cc"
    break;

  case 402: // qvaropm: "`" qvarid "`"
#line 1365 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3654 "parser.cc"
    break;

  case 403: // tyvar: tyvarid
#line 1369 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3660 "parser.cc"
    break;

  case 404: // tyvarop: "`" tyvarid "`"
#line 1371 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3666 "parser.cc"
    break;

  case 405: // tyvarid: "VARID"
#line 1373 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3672 "parser.cc"
    break;

  case 406: // tyvarid: special_id
#line 1374 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3678 "parser.cc"
    break;

  case 407: // tyvarid: "unsafe"
#line 1375 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 3684 "parser.cc"
    break;

  case 408: // tyvarid: "safe"
#line 1376 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 3690 "parser.cc"
    break;

  case 409: // tyvarid: "interruptible"
#line 1377 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 3696 "parser.cc"
    break;

  case 410: // var: varid
#line 1380 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3702 "parser.cc"
    break;

  case 411: // var: "(" varsym ")"
#line 1381 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3708 "parser.cc"
    break;

  case 412: // qvar: qvarid
#line 1383 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3714 "parser.cc"
    break;

  case 413: // qvar: "(" varsym ")"
#line 1384 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3720 "parser.cc"
    break;

  case 414: // qvar: "(" qvarsym1 ")"
#line 1385 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3726 "parser.cc"
    break;

  case 415: // qvarid: varid
#line 1387 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3732 "parser.cc"
    break;

  case 416: // qvarid: "QVARID"
#line 1388 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3738 "parser.cc"
    break;

  case 417: // varid: "VARID"
#line 1390 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3744 "parser.cc"
    break;

  case 418: // varid: special_id
#line 1391 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3750 "parser.cc"
    break;

  case 419: // varid: "unsafe"
#line 1392 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 3756 "parser.cc"
    break;

  case 420: // varid: "safe"
#line 1393 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 3762 "parser.cc"
    break;

  case 421: // varid: "interruptible"
#line 1394 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 3768 "parser.cc"
    break;

  case 422: // varid: "forall"
#line 1395 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 3774 "parser.cc"
    break;

  case 423: // varid: "family"
#line 1396 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 3780 "parser.cc"
    break;

  case 424: // varid: "role"
#line 1397 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 3786 "parser.cc"
    break;

  case 425: // qvarsym: varsym
#line 1399 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3792 "parser.cc"
    break;

  case 426: // qvarsym: qvarsym1
#line 1400 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3798 "parser.cc"
    break;

  case 427: // qvarsym_no_minus: varsym_no_minus
#line 1402 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3804 "parser.cc"
    break;

  case 428: // qvarsym_no_minus: qvarsym1
#line 1403 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3810 "parser.cc"
    break;

  case 429: // qvarsym1: "QVARSYM"
#line 1405 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3816 "parser.cc"
    break;

  case 430: // varsym: varsym_no_minus
#line 1407 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3822 "parser.cc"
    break;

  case 431: // varsym: "-"
#line 1408 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 3828 "parser.cc"
    break;

  case 432: // varsym_no_minus: "VARSYM"
#line 1410 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3834 "parser.cc"
    break;

  case 433: // varsym_no_minus: special_sym
#line 1411 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3840 "parser.cc"
    break;

  case 434: // special_id: "as"
#line 1413 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 3846 "parser.cc"
    break;

  case 435: // special_id: "qualified"
#line 1414 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 3852 "parser.cc"
    break;

  case 436: // special_id: "hiding"
#line 1415 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 3858 "parser.cc"
    break;

  case 437: // special_id: "export"
#line 1416 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 3864 "parser.cc"
    break;

  case 438: // special_id: "label"
#line 1417 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 3870 "parser.cc"
    break;

  case 439: // special_id: "dynamic"
#line 1418 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 3876 "parser.cc"
    break;

  case 440: // special_id: "stdcall"
#line 1419 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 3882 "parser.cc"
    break;

  case 441: // special_id: "ccall"
#line 1420 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 3888 "parser.cc"
    break;

  case 442: // special_id: "capi"
#line 1421 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 3894 "parser.cc"
    break;

  case 443: // special_id: "prim"
#line 1422 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 3900 "parser.cc"
    break;

  case 444: // special_id: "javascript"
#line 1423 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 3906 "parser.cc"
    break;

  case 445: // special_id: "group"
#line 1424 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 3912 "parser.cc"
    break;

  case 446: // special_id: "stock"
#line 1425 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 3918 "parser.cc"
    break;

  case 447: // special_id: "anyclass"
#line 1426 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 3924 "parser.cc"
    break;

  case 448: // special_id: "via"
#line 1427 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 3930 "parser.cc"
    break;

  case 449: // special_id: "unit"
#line 1428 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 3936 "parser.cc"
    break;

  case 450: // special_id: "dependency"
#line 1429 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 3942 "parser.cc"
    break;

  case 451: // special_id: "signature"
#line 1430 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 3948 "parser.cc"
    break;

  case 452: // special_sym: "!"
#line 1432 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 3954 "parser.cc"
    break;

  case 453: // special_sym: "."
#line 1433 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 3960 "parser.cc"
    break;

  case 454: // special_sym: "*"
#line 1434 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 3966 "parser.cc"
    break;

  case 455: // qconid: conid
#line 1438 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3972 "parser.cc"
    break;

  case 456: // qconid: "QCONID"
#line 1439 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3978 "parser.cc"
    break;

  case 457: // conid: "CONID"
#line 1441 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3984 "parser.cc"
    break;

  case 458: // qconsym: consym
#line 1443 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3990 "parser.cc"
    break;

  case 459: // qconsym: "QCONSYM"
#line 1444 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3996 "parser.cc"
    break;

  case 460: // consym: "CONSYM"
#line 1446 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4002 "parser.cc"
    break;

  case 461: // consym: ":"
#line 1447 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4008 "parser.cc"
    break;

  case 462: // literal: "CHAR"
#line 1451 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < char > ();}
#line 4014 "parser.cc"
    break;

  case 463: // literal: "STRING"
#line 1452 "parser.y"
                  {yylhs.value.as < expression_ref > () = yy_make_string(yystack_[0].value.as < std::string > ());}
#line 4020 "parser.cc"
    break;

  case 464: // literal: "INTEGER"
#line 1453 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < int > ();}
#line 4026 "parser.cc"
    break;

  case 465: // literal: "RATIONAL"
#line 1454 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < double > ();}
#line 4032 "parser.cc"
    break;

  case 467: // close: error
#line 1462 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 4038 "parser.cc"
    break;

  case 468: // modid: "CONID"
#line 1466 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4044 "parser.cc"
    break;

  case 469: // modid: "QCONID"
#line 1467 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4050 "parser.cc"
    break;

  case 470: // commas: commas ","
#line 1469 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 4056 "parser.cc"
    break;

  case 471: // commas: ","
#line 1470 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 4062 "parser.cc"
    break;


#line 4066 "parser.cc"

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


  const short parser::yypact_ninf_ = -624;

  const short parser::yytable_ninf_ = -431;

  const short
  parser::yypact_[] =
  {
      45,   147,  -624,    85,  -624,  -624,  -624,  -624,  -624,    86,
     -10,    -4,  -624,    58,   -23,   -23,    51,  -624,  -624,  -624,
    -624,   153,  -624,  -624,  -624,    65,  -624,   141,   165,  4281,
     231,   187,   177,  -624,   699,  -624,   -21,  -624,  -624,  -624,
    -624,   147,  -624,   205,  -624,  -624,  -624,  -624,  -624,  -624,
    -624,  -624,  -624,  -624,  -624,  -624,  -624,  -624,  -624,   311,
    -624,  -624,  -624,  -624,  -624,  -624,   125,  -624,  -624,  -624,
    -624,   242,   181,  -624,   232,  -624,  -624,  -624,  -624,  -624,
    -624,  -624,  -624,   157,   280,   325,  -624,   263,  -624,  2574,
    3939,  -624,   274,   167,  1720,  -624,  -624,  -624,   350,   298,
    -624,  3939,  4618,   167,  3428,  4708,  3428,   303,   279,  4572,
     152,  3062,  3428,  3184,  3428,  1340,  1082,  1211,  -624,  -624,
    -624,  -624,  -624,  -624,    42,   303,   305,   177,  -624,  -624,
    -624,   356,  -624,  -624,  -624,  -624,   478,  -624,  3306,  -624,
     344,  -624,  -624,  -624,  -624,  -624,   330,   366,   347,  -624,
    -624,  -624,  -624,   337,  -624,   206,  -624,  -624,   354,    75,
     230,  -624,   363,   368,  -624,  -624,  -624,  -624,  -624,   369,
    -624,   378,   379,   383,  -624,  -624,  -624,  4281,  4328,  -624,
    -624,  -624,  -624,  -624,  -624,   443,  -624,   -36,  1082,   467,
     492,  -624,  -624,  2574,  -624,  -624,  -624,  -624,  -624,  -624,
    -624,  -624,  4855,  3633,  3531,   380,  4479,  -624,  -624,  -624,
    -624,  -624,   469,  4386,  -624,   404,  -624,   221,  3939,  -624,
    -624,  -624,  -624,  -624,  -624,  -624,  -624,  -624,  -624,  -624,
    3735,  1842,  1842,  -624,  2330,   414,   388,    55,  -624,  -624,
     428,   430,   431,   433,  3735,   829,   829,  -624,   497,   432,
     429,   256,  4901,   385,   387,  -624,  -624,  -624,   434,    76,
     270,  4801,   437,  -624,    82,  -624,  -624,     8,  4572,  -624,
     441,   192,    -8,   405,   442,  1964,  3428,  -624,  -624,  2940,
    -624,  3306,   289,  -624,  -624,  4041,  -624,  -624,  -624,   492,
     107,   421,   413,  -624,  2574,  -624,  -624,  -624,  -624,  -624,
    -624,  -624,  3184,  -624,  -624,   -13,   178,   379,   422,   423,
     426,   189,  -624,   246,   268,   273,  3735,  4572,  4572,  -624,
     377,   263,   410,  3939,  3735,  4041,   289,  -624,  2818,  -624,
    -624,  -624,  -624,  -624,  4386,  -624,  4513,  4855,  3428,  -624,
     436,   439,   426,  -624,  -624,  -624,  -624,  -624,  -624,  -624,
    -624,   445,   425,  -624,  -624,   440,    58,  -624,   427,   464,
     472,   327,  3735,  2574,  -624,    92,   451,   447,  -624,  -624,
    -624,  -624,   450,   475,  -624,   457,   436,  -624,    20,   452,
     439,   220,   284,   458,   459,   298,  -624,  -624,  3939,  3735,
    -624,  -624,   468,   453,   298,   167,  3428,   493,   494,   -24,
    -624,  -624,    61,   491,   466,  -624,     2,  -624,   557,  -624,
    -624,  -624,  -624,  -624,  -624,  -624,  -624,   469,   137,  -624,
    -624,   566,    67,  2574,  3735,   477,   471,   460,   463,  2574,
     479,  2452,  2452,  4901,   152,  -624,  4901,  3735,   481,  4901,
    -624,   476,   502,   519,  -624,  -624,   531,   381,   532,  1598,
     960,  -624,  -624,  2574,  -624,  2574,  2330,  -624,    40,  -624,
     495,   498,   499,  2574,  2574,  2086,  1469,  -624,  1469,   680,
    -624,  1469,  -624,  1469,   503,  -624,  -624,  -624,  -624,  -624,
    -624,   541,   540,   543,  4754,   511,  -624,  -624,  -624,    -5,
     128,  -624,  -624,   331,  -624,   512,  -624,  -624,  -624,  -624,
     526,  -624,   514,   549,    96,  -624,  -624,  -624,  -624,  4328,
    -624,  -624,  -624,   147,  -624,  -624,  -624,  -624,  -624,  3735,
    4855,  -624,  4855,  4947,  -624,  3735,  -624,  3735,  -624,  3735,
    -624,  -624,  -624,  -624,  -624,  -624,  -624,  -624,  3735,   497,
    -624,  -624,  2574,  -624,  1842,  -624,  2574,  2330,  -624,  2574,
    -624,  -624,   829,  -624,  -624,  -624,  -624,  -624,  -624,   504,
     505,  -624,  -624,  3428,  -624,  -624,   608,   535,  4901,  -624,
    -624,  -624,   524,  -624,   536,  -624,  -624,  -624,   537,   185,
     307,  -624,  -624,  -624,  -624,  2208,   544,   530,  -624,   352,
      58,  -624,  -624,   469,   567,  -624,  -624,  -624,  -624,  -624,
    -624,  2696,   533,  -624,  -624,   572,  -624,  -624,  -624,  -624,
    -624,  3735,  3735,   377,  -624,   577,  3735,   632,  -624,   654,
    -624,  -624,  4513,  1469,  3735,   551,   663,  -624,  -624,  -624,
    3735,  5028,  -624,  -624,  -624,  -624,   568,   569,  -624,  -624,
    -624,  -624,  -624,   388,  -624,  -624,  -624,  -624,   358,  -624,
    -624,  -624,  -624,  -624,  -624,  -624,  -624,  -624,  2452,  2574,
    -624,    49,  -624,  -624,  2574,   272,   631,  2086,  2574,  -624,
      -7,    -6,  -624,  -624,  -624,  -624,   595,  -624,  4386,    70,
    -624,   654,  -624,  -624,  -624,  -624,  -624,   147,    52,  -624,
     600,  -624,  -624,   672,   829,   829,  -624,   469,  -624,  -624,
    2574,  2574,  2574,  -624,  -624,  -624,  -624,  3735,  -624,  4981,
     632,   594,  4087,  -624,  -624,  -624,  -624,  -624,  -624,  3837,
     215,   634,  -624,  -624,  -624,  -624,   581,  4281,  -624,  -624,
    3735,  2574,   139,    67,  -624,   636,  -624,  -624,  -624,  -624,
    -624,  4386,  -624,  4386,  -624,  -624,   580,   583,  -624,  3939,
    -624,  4281,   587,   588,  -624,  -624,  -624,  2574,  4180,  -624,
    4386,  3939,  -624,  -624,   590,  -624,  -624,  -624,  -624,  -624
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    40,     0,     2,    40,     4,   468,   469,     8,
       0,    43,     1,     0,     0,     0,    18,    11,    39,    13,
      16,    68,   467,   466,    12,   127,   123,     0,     0,     0,
       0,    46,    41,    15,    14,   126,     0,     6,     7,   434,
     436,     0,   435,     0,   422,   437,   438,   439,   420,   421,
     419,   423,   424,   440,   441,   442,   443,   444,   445,     0,
     446,   447,   448,   449,   451,   450,     0,   417,   380,   416,
     378,     0,    19,    21,    25,    33,    36,   370,   379,    35,
     412,   415,   418,     0,     0,    48,    38,    42,   271,     0,
       0,    93,     0,     0,     0,    61,    62,    63,    88,     0,
      94,     0,     0,     0,     0,     0,     0,   226,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   457,   456,
     462,   463,   464,   465,   226,   226,    59,    66,    69,    70,
      71,   101,   208,   218,    73,   205,    74,   234,   238,   248,
     260,   262,   264,   340,   353,   341,     0,   263,   415,   342,
     455,   265,   124,     0,    23,     0,    34,   367,     0,     0,
       0,    24,     0,     0,   431,   452,   454,   453,   432,     0,
     429,     0,     0,     0,   430,   433,    17,     0,    27,    22,
      40,    40,     3,    45,    47,    52,    37,     0,     0,     0,
     231,   239,   232,     0,   408,   409,   407,   386,   140,   387,
     139,   161,   190,     0,     0,     0,     0,   405,   385,   384,
     382,   381,   122,     0,   138,     0,    98,   147,   150,   153,
     155,   159,   362,   156,   375,   383,   160,   157,   403,   406,
     173,   327,   327,   257,     0,     0,   242,     0,   255,   310,
       0,     0,     0,     0,     0,   117,   117,   120,     0,     0,
     147,     0,     0,     0,     0,   410,   390,   258,     0,     0,
       0,   108,     0,   346,     0,   344,   247,     0,     0,   227,
       0,     0,     0,   347,   130,     0,     0,   318,   321,     0,
     250,   236,     0,   461,   354,     0,   460,   459,   272,   231,
     277,     0,   278,   396,     0,   397,   395,   401,   428,   427,
     357,   458,   431,   349,   471,     0,     0,   428,     0,   427,
     357,     0,   351,     0,     0,     0,     0,     0,     0,    60,
       0,    67,     0,     0,     0,     0,     0,   393,     0,   394,
     392,   399,   426,   425,     0,   245,   334,     0,     0,   125,
       0,     0,     0,   373,   374,   372,   371,   414,   413,    20,
      32,     0,    28,    30,    31,     0,     0,    51,    50,     0,
       0,     0,     0,     0,   240,     0,     0,   191,   193,   135,
     177,   366,     0,     0,   143,     0,   140,   164,   174,     0,
     375,     0,     0,     0,     0,     0,    79,   162,     0,     0,
     154,   174,     0,   172,     0,     0,     0,   331,     0,     0,
     326,   328,     0,     0,   294,   296,     0,   241,     0,   309,
     312,    85,    84,    86,    87,   169,   132,   122,     0,   209,
     116,   128,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   105,   108,     0,     0,     0,
     355,     0,     0,     0,   237,   221,     0,     0,     0,     0,
       0,   253,   319,     0,   320,     0,     0,   206,   122,   213,
       0,     0,     0,   273,   279,     0,     0,   270,     0,   274,
     266,     0,   267,     0,   413,   343,   350,   470,   268,   269,
     352,     0,     0,     0,     0,   217,   389,    65,   388,     0,
      95,   129,   215,   144,   133,     0,   207,   235,   246,   337,
       0,   333,   336,   339,     0,   249,   369,   368,    26,     0,
       9,    10,    49,     0,   244,   243,   256,   230,   233,     0,
       0,   163,     0,     0,   166,     0,   365,     0,   167,     0,
     363,   364,   376,   404,   121,    97,   148,    72,     0,   332,
     329,   317,     0,   322,   325,   323,     0,     0,   311,     0,
      83,   118,   115,   119,   252,   144,    80,   411,   391,    78,
      76,   259,   345,     0,   314,   102,   103,     0,   108,   348,
     109,   113,     0,   106,     0,   228,   220,   222,     0,     0,
       0,   131,   359,   219,   299,     0,     0,   301,   305,     0,
       0,   300,   251,   122,     0,   211,   212,   398,   402,   358,
     281,     0,   283,   288,   289,   272,   285,   284,   276,   275,
     225,     0,     0,     0,   100,     0,     0,   187,    82,   195,
     400,   261,     0,     0,     0,     0,    54,   194,   134,   192,
       0,     0,   176,   178,   142,   180,     0,   175,   175,   330,
     324,   313,   295,   242,   114,    77,    75,   315,     0,   104,
     107,   110,   356,   229,   360,   361,   302,   297,   304,     0,
     306,   122,   298,   210,     0,   445,   290,     0,   280,   136,
       0,     0,    64,    99,    96,   177,   181,   183,     0,     0,
      81,   196,   198,   335,   338,   216,    29,     0,    56,   141,
       0,   168,   165,     0,   117,   117,   303,   122,   308,   214,
       0,     0,     0,   286,   287,   282,   223,     0,   224,     0,
     187,     0,   188,   151,   158,   185,    91,    89,    90,     0,
       0,   199,   202,   377,   197,    53,     0,     0,    44,    55,
       0,     0,     0,     0,   307,     0,   292,   291,   137,   186,
     182,     0,   152,     0,   203,   149,   170,     0,   200,     0,
     201,     0,     0,     0,   254,   111,   112,     0,   188,   184,
     189,     0,   204,    92,     0,    57,   179,   293,   171,    58
  };

  const short
  parser::yypgoto_[] =
  {
    -624,  -624,  -624,  -624,  -624,  -624,  -624,    44,  -624,  -624,
    -523,  -624,   552,  -624,  -624,  -624,   199,  -146,  -624,   574,
    -624,  -624,  -624,  -624,  -624,  -624,  -624,  -624,  -624,  -624,
    -624,  -624,  -624,  -624,  -624,  -624,  -624,  -624,  -624,  -624,
    -624,  -624,  -624,  -624,  -624,   390,  -624,  -624,  -624,  -392,
    -624,  -624,  -624,  -239,  -624,  -186,  -380,   695,  -624,  -624,
    -624,  -297,  -389,   389,   102,  -624,  -624,  -183,   296,   -55,
    -624,   -89,  -624,   -96,  -291,  -624,   527,  -623,  -207,   448,
     -14,  -624,   518,    83,  -624,  -565,  -624,  -624,    56,  -624,
      18,  -624,  -624,   239,  -624,  -624,    84,    47,   734,   217,
     444,  -624,   316,  -624,   154,  -624,   -63,   -93,   741,   -25,
    -300,   134,  -624,   -71,     1,  -624,  -624,   -62,   662,  -624,
    -624,  -624,   113,   328,  -624,   420,  -405,  -624,   127,  -624,
    -217,  -624,  -218,   -99,  -624,   507,  -624,   -70,   555,   245,
    -204,  -624,   161,  -624,   733,  -624,   688,   -81,  -624,   -69,
    -248,  -102,  -624,   348,   751,  -624,  -624,  -624,   -27,  -624,
    -130,  -624,   184,   696,  -147,  -624,   -57,  -624,  -624,  -465,
    -624,   593,   -74,   -29,  -181,   -19,  -624,  -624,   -16,   -56,
     -77,   -87,  -624,  -171,   -53,   -41,  -251,  -624,  -220,   -37,
    -106
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   182,     6,    10,    19,    30,
      71,    72,    73,   179,   351,   352,   353,    74,    75,    87,
      11,    20,    21,    32,    85,   185,   513,   358,   688,   728,
     729,   320,   126,   485,    33,    34,   127,   128,   129,   130,
     244,   720,   750,   131,   618,   212,   323,   132,   260,   435,
     567,   649,   133,   418,   247,   248,   386,    27,    36,   326,
     448,   415,   492,   365,   670,   213,   214,   416,   494,   373,
     711,   374,   746,   217,   712,   218,   219,   713,   220,   417,
     747,   392,   379,   523,   632,   636,   619,   676,   677,   678,
     715,   366,   367,   368,   680,   681,   682,   721,   419,   420,
     457,   458,   459,   135,   268,   269,   288,   190,   421,   191,
     192,   408,   193,   138,   139,   140,   141,   305,   306,   291,
     292,   602,   603,   403,   404,   451,   586,   587,   588,   660,
     237,   238,   239,   589,   398,   278,   279,   233,   399,   400,
     401,   500,   501,   502,   142,   143,   273,   262,   144,   145,
     486,   293,   581,   221,   222,    76,   223,   722,   157,    78,
     224,   225,   487,   488,   328,   294,   295,   330,   296,   226,
     227,   228,   146,   147,    80,    81,   331,   297,   298,   333,
     174,    82,   175,   149,   150,   300,   301,   151,    24,     9,
     311
  };

  const short
  parser::yytable_[] =
  {
      79,   216,    77,   229,   154,   250,   387,   422,   430,   137,
     173,   315,   249,   440,   229,   148,   439,   410,   406,   409,
     372,   378,   289,   289,   289,   341,   189,   491,   254,   274,
     405,   236,   354,   257,   327,   215,   263,   550,   299,   309,
     299,   263,   281,   363,   570,   590,   256,   391,   571,    13,
     172,   674,   265,   290,   315,   313,    22,   265,   633,    22,
     308,   316,    22,   518,   726,   517,     1,   385,    22,   440,
     445,   706,   708,   614,   380,   310,   385,   543,   595,   329,
     272,    25,   359,   255,   152,    12,   264,   234,   327,   742,
     544,    17,   716,   360,   153,   289,   470,   441,   381,   382,
     307,   527,   471,   548,   461,   258,    26,   266,   707,   707,
      18,   309,   277,   280,   462,   282,   229,   229,   342,   229,
     332,   446,   717,   718,   615,   456,   229,   498,   369,   528,
     364,   229,   173,   329,   234,   742,   511,   742,   442,   335,
     234,   493,   463,   229,   495,     2,   267,   310,    79,    79,
      77,    77,    14,    15,   462,   283,   283,   229,    23,    29,
     727,    23,   283,   518,    23,   753,   690,    31,   397,   397,
      23,   397,   307,   519,   332,   544,   651,   624,   719,   383,
     656,   552,   545,   255,   303,   303,   464,   327,   409,    68,
     304,   304,   465,    70,    35,   425,   286,   286,   438,   534,
     287,   469,   553,   286,   752,   162,   536,   520,   539,   616,
     617,   520,   189,   663,   163,   173,   164,   165,   166,    37,
     137,   137,   466,   167,   355,   356,   148,   148,   764,   229,
     591,   281,   329,   426,   216,   685,   229,   229,   551,   443,
     755,   555,   436,    38,   633,   168,   169,   229,    84,   170,
     171,   552,   405,   552,   555,   172,   158,   180,    83,   181,
     259,   604,   160,   369,   256,   197,     7,   231,   215,   232,
       8,   118,   375,   332,   340,   229,   199,   452,   317,   318,
     277,   698,   335,   164,   165,   166,   197,   472,   482,   483,
     167,    86,   250,   473,   654,   340,   177,   199,   476,   535,
     304,   229,   229,   497,   477,   208,   209,   503,   389,   210,
     211,  -145,   168,   155,   669,   669,   170,   734,   255,   327,
     700,   701,   363,   719,    68,   540,   208,   209,    70,   530,
     210,   211,   565,   566,    68,   477,   627,   229,    70,   505,
     178,   312,   634,   642,   635,   304,   637,   164,   165,   166,
     229,   176,   431,   569,   167,   638,   432,   478,   183,   184,
     554,   471,   440,   354,   329,   263,   561,   327,   564,   564,
     662,   455,   661,   289,   456,   289,   168,   186,   289,   479,
     289,   265,   230,   473,   480,   572,   564,   564,   477,   299,
     592,   299,   593,   397,   299,   531,   299,   541,   245,   477,
     246,   600,   397,   605,   606,   332,   607,   267,   270,   608,
     738,   609,   329,  -132,   568,   158,  -132,   436,   655,   159,
     573,   160,   304,   240,   241,   242,   243,   449,   322,   450,
     118,   572,   229,   635,   119,   319,   229,   234,   229,   659,
     229,   555,   229,   409,   336,   337,   628,   689,   369,   341,
     758,   229,   760,   332,   338,   732,   733,   283,   694,   284,
     695,   440,  -410,   704,   743,   426,   339,   357,   164,   165,
     166,   714,   343,   381,   382,   167,   626,   344,   345,   639,
      79,   397,    77,   641,   397,   578,   643,   346,   347,   579,
     361,   580,   348,   484,   388,   304,   385,   168,   286,   234,
      68,   255,   407,   255,    70,   714,   411,   440,   412,   413,
     743,   414,   423,   756,   424,   427,   389,   428,   437,   444,
     434,   429,   564,   447,   229,   229,   467,   137,   468,   229,
     289,   474,  -430,   148,   714,   475,   714,   229,   666,   489,
     509,   510,   514,   229,   229,   506,   299,   635,   507,   436,
     515,   714,   521,   714,   508,   524,   512,   256,   283,   324,
    -128,   684,   522,  -128,   647,   525,   526,   529,   538,   164,
     165,   166,   283,   362,   532,   533,   167,   537,   546,  -316,
     542,   547,   549,   164,   165,   166,   557,   558,   562,   559,
     167,   229,   560,   503,   325,   564,   697,   576,   168,   286,
     118,   699,   170,   287,   397,   705,   574,   575,   325,   577,
     583,   597,   168,   286,   598,   599,   170,   287,  -411,   610,
     229,   611,   229,   250,   612,   229,   613,   621,   620,   622,
     745,   623,   229,   645,   646,   648,   650,   735,   736,   737,
     652,   653,   371,   229,   658,   657,   283,   324,   667,   664,
     725,   668,   723,   250,   229,   673,   229,   164,   165,   166,
     763,   675,   229,   679,   167,   250,   686,   687,   754,   137,
     137,   229,   745,   229,   229,   148,   148,   691,   692,   702,
     710,   730,   325,   731,   741,   757,   168,   286,   749,   751,
     170,   287,   762,   723,   767,   761,   765,   766,    79,   769,
      77,   321,    88,    39,    89,    90,    91,    92,   625,    93,
      28,    40,    94,   490,   671,    95,    96,    97,    98,    99,
     556,   100,    79,    42,    77,   101,   504,   102,    44,   349,
      45,    46,    47,    48,    49,    50,   103,    51,    52,    53,
      54,    55,    56,    57,   104,   390,    58,   768,   393,   105,
     106,    60,    61,    62,    63,    64,    65,   107,   709,   759,
     283,   629,   108,   109,   481,   724,   740,   748,   134,   644,
     496,   164,   165,   166,   596,   136,   110,   693,   167,   314,
     703,   516,   111,   683,   594,   696,   454,   402,   112,   640,
     113,   114,   161,   261,   156,   582,   325,   672,   253,   384,
     168,   286,     0,   115,   170,   287,     0,   116,     0,   117,
       0,     0,     0,     0,     0,     0,     0,    67,   118,     0,
       0,    69,   119,     0,     0,     0,     0,   120,   121,   122,
     123,     0,    88,    39,    89,     0,     0,   124,   125,    93,
       0,    40,    94,     0,     0,    95,    96,    97,     0,    99,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   103,    51,    52,    53,
      54,    55,    56,    57,   104,     0,    58,     0,     0,   105,
     106,    60,    61,    62,    63,    64,    65,   107,     0,     0,
       0,     0,   108,   109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
       0,     0,   111,     0,     0,     0,     0,     0,   112,     0,
     113,   114,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   115,     0,     0,     0,   116,     0,   117,
       0,     0,     0,     0,     0,     0,     0,    67,   118,     0,
       0,    69,   119,     0,     0,     0,     0,   120,   121,   122,
     123,    22,     0,    88,    39,    89,     0,   124,   125,     0,
      93,     0,    40,    94,     0,     0,     0,     0,     0,     0,
      99,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   103,    51,    52,
      53,    54,    55,    56,    57,   104,     0,    58,     0,     0,
       0,   106,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,   108,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   111,     0,     0,     0,     0,     0,   112,
       0,   113,   563,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    23,   115,     0,     0,     0,   188,     0,
     117,     0,     0,     0,   585,     0,     0,     0,    67,   118,
       0,     0,    69,   119,     0,    88,    39,    89,   120,   121,
     122,   123,    93,     0,    40,    94,     0,     0,     0,     0,
       0,     0,    99,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   103,
      51,    52,    53,    54,    55,    56,    57,   104,     0,    58,
       0,     0,     0,   106,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,   108,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   283,     0,     0,   111,     0,     0,     0,     0,
       0,   112,     0,   302,   165,   166,     0,     0,     0,     0,
     167,     0,     0,     0,     0,     0,   115,     0,     0,     0,
     188,   303,   117,     0,     0,     0,     0,   304,   285,     0,
      67,   118,   168,   286,    69,   119,   170,   287,     0,     0,
     120,   121,   122,   123,    88,    39,    89,     0,     0,     0,
       0,    93,     0,    40,    94,     0,     0,     0,     0,     0,
       0,    99,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   103,    51,
      52,    53,    54,    55,    56,    57,   104,     0,    58,     0,
       0,     0,   106,    60,    61,    62,    63,    64,    65,     0,
       0,     0,     0,     0,   108,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   283,     0,     0,   111,     0,     0,     0,     0,     0,
     112,     0,   113,   165,   166,     0,     0,     0,     0,   167,
       0,     0,     0,     0,     0,   115,     0,     0,     0,   188,
       0,   117,   312,     0,     0,     0,   304,   285,     0,    67,
     118,   168,   286,    69,   119,   170,   287,     0,     0,   120,
     121,   122,   123,    88,    39,    89,     0,     0,     0,     0,
      93,     0,    40,    94,     0,     0,     0,     0,     0,     0,
      99,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   103,    51,    52,
      53,    54,    55,    56,    57,   104,     0,    58,     0,     0,
       0,   106,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,   108,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     283,     0,     0,   111,     0,     0,     0,     0,     0,   112,
       0,   113,   165,   166,     0,     0,     0,     0,   167,     0,
       0,     0,     0,     0,   115,   284,     0,     0,   188,     0,
     117,     0,     0,     0,     0,     0,   285,     0,    67,   118,
     168,   286,    69,   119,   170,   287,     0,     0,   120,   121,
     122,   123,    88,    39,    89,     0,     0,     0,     0,    93,
       0,    40,    94,     0,     0,     0,     0,     0,     0,    99,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   103,    51,    52,    53,
      54,    55,    56,    57,   104,     0,    58,     0,     0,     0,
     106,    60,    61,    62,    63,    64,    65,     0,     0,     0,
       0,     0,   108,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   283,
       0,     0,   111,     0,     0,     0,     0,     0,   112,     0,
     113,   165,   166,     0,     0,     0,     0,   167,     0,     0,
       0,     0,     0,   115,     0,     0,     0,   188,     0,   117,
       0,     0,     0,     0,     0,   285,     0,    67,   118,   168,
     286,    69,   119,   170,   287,     0,     0,   120,   121,   122,
     123,    88,    39,    89,     0,     0,     0,     0,    93,     0,
      40,    94,     0,     0,     0,     0,     0,     0,    99,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   103,    51,    52,    53,    54,
      55,    56,    57,   104,     0,    58,     0,     0,     0,   106,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,   108,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   111,     0,     0,     0,     0,     0,   112,     0,   113,
     563,     0,     0,     0,     0,     0,     0,     0,     0,   584,
       0,     0,   115,     0,     0,     0,   188,     0,   117,     0,
       0,     0,   585,     0,     0,     0,    67,   118,     0,     0,
      69,   119,     0,    88,    39,    89,   120,   121,   122,   123,
      93,     0,    40,    94,     0,     0,     0,     0,     0,     0,
      99,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   103,    51,    52,
      53,    54,    55,    56,    57,   104,     0,    58,     0,     0,
       0,   106,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,   108,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   111,     0,   234,     0,     0,     0,   112,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
     235,     0,     0,     0,   115,     0,     0,     0,   188,     0,
     117,     0,     0,     0,     0,     0,     0,     0,    67,   118,
       0,     0,    69,   119,     0,    88,    39,    89,   120,   121,
     122,   123,    93,     0,    40,    94,     0,     0,     0,     0,
       0,     0,   394,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   103,
      51,    52,    53,    54,    55,    56,    57,   104,   395,    58,
       0,     0,     0,   106,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,   108,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   111,     0,     0,     0,     0,
       0,   112,     0,   113,   396,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   115,     0,     0,     0,
     188,     0,   117,     0,     0,     0,     0,     0,     0,     0,
      67,   118,     0,     0,    69,   119,     0,    88,    39,    89,
     120,   121,   122,   123,    93,     0,    40,    94,     0,     0,
       0,     0,     0,     0,    99,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   103,    51,    52,    53,    54,    55,    56,    57,   104,
       0,    58,     0,     0,     0,   106,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,   108,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   111,     0,     0,
       0,     0,     0,   112,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,   449,     0,   450,     0,   115,     0,
       0,     0,   188,     0,   117,     0,     0,     0,     0,     0,
       0,     0,    67,   118,     0,     0,    69,   119,     0,    88,
      39,    89,   120,   121,   122,   123,    93,     0,    40,    94,
       0,     0,     0,     0,     0,     0,   394,     0,     0,     0,
      42,   601,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   103,    51,    52,    53,    54,    55,    56,
      57,   104,     0,    58,     0,     0,     0,   106,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,   108,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   111,
       0,     0,     0,     0,     0,   112,     0,   113,   396,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     115,     0,     0,     0,   188,     0,   117,     0,     0,     0,
       0,     0,     0,     0,    67,   118,     0,     0,    69,   119,
       0,    88,    39,    89,   120,   121,   122,   123,    93,     0,
      40,    94,     0,     0,     0,     0,     0,     0,    99,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   103,    51,    52,    53,    54,
      55,    56,    57,   104,     0,    58,     0,     0,     0,   106,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,   108,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   111,     0,     0,     0,     0,     0,   112,     0,   113,
     563,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   115,     0,     0,     0,   188,     0,   117,     0,
       0,     0,   585,     0,     0,     0,    67,   118,     0,     0,
      69,   119,     0,    88,    39,    89,   120,   121,   122,   123,
      93,     0,    40,    94,     0,     0,     0,     0,     0,     0,
     394,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   103,    51,    52,
      53,    54,    55,    56,    57,   104,     0,    58,     0,     0,
       0,   106,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,   108,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   111,     0,     0,     0,     0,     0,   112,
       0,   113,   396,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   115,     0,     0,     0,   188,     0,
     117,     0,     0,     0,     0,     0,     0,     0,    67,   118,
       0,     0,    69,   119,     0,    88,    39,    89,   120,   121,
     122,   123,    93,     0,    40,    94,     0,     0,     0,     0,
       0,     0,    99,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   103,
      51,    52,    53,    54,    55,    56,    57,   104,     0,    58,
       0,     0,     0,   106,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,   108,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   111,     0,     0,     0,     0,
       0,   112,     0,   113,   563,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   115,     0,     0,     0,
     188,     0,   117,     0,     0,     0,     0,     0,     0,     0,
      67,   118,     0,     0,    69,   119,     0,    88,    39,    89,
     120,   121,   122,   123,    93,     0,    40,    94,     0,     0,
       0,     0,     0,     0,    99,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   103,    51,    52,    53,    54,    55,    56,    57,   104,
       0,    58,     0,     0,     0,   106,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,   108,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   111,     0,     0,
       0,     0,     0,   112,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   115,     0,
       0,     0,   188,     0,   117,     0,     0,     0,     0,     0,
       0,     0,    67,   118,     0,     0,    69,   119,     0,    88,
      39,    89,   120,   121,   122,   123,    93,     0,    40,    94,
       0,     0,     0,     0,     0,     0,    99,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   103,    51,    52,    53,    54,    55,    56,
      57,   104,     0,   665,     0,     0,     0,   106,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,   108,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   111,
       0,     0,     0,     0,     0,   112,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     115,     0,     0,     0,   188,     0,   117,     0,     0,     0,
       0,     0,     0,     0,    67,   118,     0,     0,    69,   119,
       0,    88,    39,    89,   120,   121,   122,   123,    93,     0,
      40,    94,     0,     0,     0,     0,     0,     0,    99,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   103,    51,    52,    53,    54,
      55,    56,    57,   104,     0,    58,     0,     0,     0,   106,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,   108,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   111,     0,     0,     0,     0,     0,   112,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   115,     0,     0,     0,   188,     0,   117,     0,
       0,     0,     0,     0,     0,     0,    67,   118,     0,     0,
      69,   119,     0,    88,    39,    89,   120,   121,   122,   123,
      93,     0,    40,    94,     0,     0,     0,     0,     0,     0,
      99,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   103,    51,    52,
      53,    54,    55,    56,    57,   104,     0,    58,     0,     0,
       0,     0,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   111,     0,     0,     0,   453,     0,   112,
       0,     0,   276,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   115,     0,     0,     0,   188,     0,
     117,     0,     0,     0,     0,     0,     0,     0,    67,   118,
       0,     0,    69,   119,     0,    88,    39,   275,   120,   121,
     122,   123,    93,     0,    40,    94,     0,     0,     0,     0,
       0,     0,    99,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   103,
      51,    52,    53,    54,    55,    56,    57,   104,     0,    58,
       0,     0,     0,     0,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   111,     0,     0,     0,     0,
       0,   112,     0,     0,   276,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   115,     0,     0,     0,
     188,     0,   117,     0,     0,     0,     0,     0,     0,     0,
      67,   118,     0,     0,    69,   119,     0,    88,    39,    89,
     120,   121,   122,   123,    93,     0,    40,    94,     0,     0,
       0,     0,     0,     0,    99,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   103,    51,    52,    53,    54,    55,    56,    57,   104,
       0,    58,     0,     0,     0,   106,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   111,     0,     0,
       0,     0,     0,   112,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   115,     0,
       0,     0,   188,     0,   117,     0,     0,     0,     0,     0,
       0,     0,    67,   118,     0,     0,    69,   119,     0,    88,
      39,    89,   120,   121,   122,   123,    93,     0,    40,    94,
       0,     0,     0,     0,     0,     0,    99,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   103,    51,    52,    53,    54,    55,    56,
      57,   104,     0,    58,     0,     0,     0,     0,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   111,
       0,     0,     0,     0,     0,   112,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   334,     0,     0,     0,     0,
     115,     0,     0,     0,   188,     0,   117,     0,     0,     0,
       0,     0,     0,     0,    67,   118,     0,     0,    69,   119,
       0,    88,    39,    89,   120,   121,   122,   123,    93,     0,
      40,    94,     0,     0,     0,     0,     0,     0,    99,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   103,    51,    52,    53,    54,
      55,    56,    57,   104,     0,    58,     0,     0,     0,     0,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   111,     0,     0,     0,     0,     0,   112,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   115,     0,     0,    39,   188,     0,   117,     0,
       0,     0,     0,    40,     0,     0,    67,   118,     0,     0,
      69,   119,     0,     0,     0,    42,   120,   121,   122,   123,
     370,     0,    45,    46,    47,   194,   195,   196,     0,     0,
       0,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,     0,    60,    61,    62,    63,    64,    65,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   197,     0,     0,     0,     0,     0,     0,   375,     0,
     376,     0,   199,   200,   201,     0,     0,     0,     0,     0,
       0,   202,     0,     0,     0,   203,     0,    39,     0,   204,
     377,   205,     0,     0,     0,    40,   304,   206,     0,   207,
      68,   208,   209,     0,    70,   210,   211,    42,     0,     0,
       0,     0,   370,     0,    45,    46,    47,   194,   195,   196,
       0,     0,     0,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,     0,     0,    60,    61,    62,    63,    64,
      65,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   197,     0,     0,     0,     0,     0,     0,
       0,     0,   198,     0,   199,   200,   201,     0,     0,     0,
       0,     0,     0,   202,     0,     0,     0,   203,   371,    39,
       0,   204,     0,   205,     0,     0,     0,    40,     0,   206,
       0,   207,    68,   208,   209,     0,    70,   210,   211,    42,
       0,     0,     0,     0,   370,     0,    45,    46,    47,   194,
     195,   196,     0,     0,     0,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,     0,     0,    60,    61,    62,
      63,    64,    65,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   197,     0,     0,     0,     0,
       0,     0,     0,     0,   198,     0,   199,   200,   201,     0,
       0,     0,     0,     0,     0,   202,     0,     0,     0,   203,
       0,    39,     0,   204,     0,   205,     0,     0,     0,    40,
       0,   206,     0,   207,    68,   208,   209,     0,    70,   210,
     211,    42,     0,     0,     0,     0,     0,     0,    45,    46,
      47,   194,   195,   196,     0,     0,     0,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,     0,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   197,     0,     0,
       0,     0,     0,     0,     0,     0,   198,     0,   199,   200,
     201,     0,     0,     0,     0,     0,     0,   202,     0,     0,
       0,   203,     0,    39,     0,   204,   744,   205,     0,     0,
       0,    40,     0,   206,     0,   207,    68,   208,   209,     0,
      70,   210,   211,    42,     0,     0,     0,     0,     0,     0,
      45,    46,    47,   194,   195,   196,     0,     0,     0,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,     0,
       0,    60,    61,    62,    63,    64,    65,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   197,
       0,     0,     0,     0,     0,     0,     0,     0,   198,     0,
     199,   200,   201,     0,     0,     0,     0,     0,     0,   202,
       0,     0,     0,   203,   460,    39,     0,   204,     0,   205,
       0,     0,     0,    40,     0,   206,     0,   207,    68,   208,
     209,     0,    70,   210,   211,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,    39,     0,    60,    61,    62,    63,    64,    65,    40,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    42,     0,     0,     0,     0,     0,     0,    45,    46,
      47,   194,   195,   196,     0,     0,     0,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,     0,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    67,
     118,     0,     0,    69,   119,     0,     0,   283,     0,     0,
       0,     0,     0,     0,     0,     0,   198,  -146,     0,   200,
     201,     0,     0,     0,    39,     0,     0,   202,     0,     0,
       0,   203,    40,     0,     0,   204,     0,   205,     0,     0,
       0,     0,     0,   438,    42,   207,    68,     0,   286,     0,
      70,    45,    46,    47,   194,   195,   196,     0,     0,     0,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,     0,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     283,     0,     0,     0,     0,     0,     0,     0,     0,   198,
       0,     0,   200,   201,     0,     0,     0,     0,     0,     0,
     202,     0,     0,     0,   203,    39,     0,     0,   204,     0,
     205,     0,     0,    40,     0,     0,   438,     0,   207,    68,
       0,   286,    41,    70,     0,    42,     0,    43,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,    59,    39,    60,    61,    62,    63,    64,    65,     0,
      40,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    42,     0,    43,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,     0,
      60,    61,    62,    63,    64,    65,     0,     0,     0,    66,
      39,     0,     0,     0,     0,     0,     0,     0,    40,    67,
      68,     0,     0,    69,    70,     0,     0,   350,     0,     0,
      42,     0,     0,     0,     0,     0,     0,    45,    46,    47,
     194,   195,   196,     0,     0,     0,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,    66,     0,    60,    61,
      62,    63,    64,    65,     0,     0,    67,    68,     0,     0,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   198,     0,     0,   200,   201,
       0,     0,     0,    39,     0,     0,   202,     0,     0,     0,
     203,    40,     0,     0,   204,     0,   205,     0,     0,     0,
       0,     0,     0,    42,   207,    68,     0,     0,     0,    70,
      45,    46,    47,   194,   195,   196,     0,    39,     0,    53,
      54,    55,    56,    57,     0,    40,    58,     0,     0,     0,
       0,    60,    61,    62,    63,    64,    65,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
       0,    51,    52,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,     0,     0,    60,    61,    62,    63,    64,
      65,     0,     0,     0,     0,     0,    39,     0,     0,     0,
       0,     0,     0,     0,    40,     0,     0,     0,     0,     0,
       0,     0,   499,     0,     0,     0,    42,   207,    68,     0,
       0,    44,    70,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,   271,    39,     0,    60,    61,    62,    63,    64,    65,
      40,    67,     0,     0,     0,    69,     0,     0,     0,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,     0,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
     271,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      67,     0,     0,     0,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   164,
     165,   166,    39,     0,     0,     0,   167,     0,     0,     0,
      40,     0,     0,     0,     0,     0,   251,     0,     0,     0,
       0,     0,    42,     0,   252,     0,    67,    44,   168,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,    39,     0,
      60,    61,    62,    63,    64,    65,    40,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    39,    60,    61,    62,    63,
      64,    65,   158,    40,     0,     0,   259,     0,   160,     0,
       0,     0,     0,     0,     0,    42,    67,   118,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,     0,    60,    61,    62,    63,    64,    65,    39,
       0,     0,     0,     0,     0,     0,     0,    40,     0,     0,
       0,     0,    67,   118,     0,     0,     0,     0,     0,    42,
       0,     0,  -347,     0,    44,     0,    45,    46,    47,    48,
      49,    50,     0,    51,    52,    53,    54,    55,    56,    57,
       0,   433,    58,     0,     0,    39,     0,    60,    61,    62,
      63,    64,    65,    40,     0,     0,   434,     0,     0,    67,
       0,     0,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,    39,     0,    60,    61,    62,    63,    64,    65,    40,
       0,     0,     0,   251,     0,     0,     0,     0,     0,     0,
       0,    42,     0,    67,     0,     0,     0,     0,    45,    46,
      47,   194,   195,   196,     0,    39,     0,    53,    54,    55,
      56,    57,     0,    40,    58,     0,     0,     0,     0,    60,
      61,    62,    63,    64,    65,    42,     0,     0,     0,     0,
       0,     0,    45,    46,    47,   194,   195,   196,     0,    67,
       0,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    39,    60,    61,    62,    63,    64,    65,     0,
      40,     0,     0,     0,     0,   630,     0,     0,     0,     0,
       0,     0,    42,     0,     0,   631,     0,     0,     0,    45,
      46,    47,   194,   195,   196,   207,     0,     0,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,   739,
      60,    61,    62,    63,    64,    65,     0,     0,     0,   631,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   207,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   207
  };

  const short
  parser::yycheck_[] =
  {
      29,    90,    29,    90,    41,   101,   213,   246,   259,    34,
      66,   117,   101,   264,   101,    34,   264,   237,   235,   237,
     203,   204,   115,   116,   117,   155,    89,   324,   102,   110,
     234,    94,   178,   103,   136,    90,   105,   417,   115,   116,
     117,   110,   113,   190,   436,   450,   102,   230,   437,     5,
      66,   616,   105,   115,   160,   117,     1,   110,   523,     1,
     116,    19,     1,   363,    12,   362,    21,    27,     1,   320,
      78,    78,    78,    78,   204,   116,    27,   101,   458,   136,
     109,   104,   118,   102,   105,     0,   105,    85,   190,   712,
     114,   101,    22,   129,   115,   188,   109,    89,   204,   205,
     116,    81,   115,   101,   285,   104,   129,   106,   115,   115,
     114,   188,   111,   112,   285,   114,   203,   204,   159,   206,
     136,   129,    52,    53,   129,    85,   213,   334,   202,   109,
     193,   218,   188,   190,    85,   758,   356,   760,   130,   138,
      85,   324,   289,   230,   325,   100,   104,   188,   177,   178,
     177,   178,    66,    67,   325,    80,    80,   244,   103,   108,
     108,   103,    80,   463,   103,   730,   631,    14,   231,   232,
     103,   234,   188,    81,   190,   114,   568,    81,   108,   206,
     585,   114,   402,   202,   109,   109,    79,   289,   406,   119,
     115,   115,    85,   123,   129,   251,   121,   121,   116,   385,
     125,   294,   422,   121,   727,    80,   389,   115,   394,    81,
      82,   115,   275,   593,    89,   271,    91,    92,    93,    78,
     245,   246,   115,    98,   180,   181,   245,   246,   751,   316,
     450,   302,   289,   252,   323,   624,   323,   324,   101,   268,
     101,   424,   261,    78,   709,   120,   121,   334,    61,   124,
     125,   114,   456,   114,   437,   271,   104,   100,    27,   102,
     108,   465,   110,   337,   320,    80,   119,   100,   323,   102,
     123,   119,    87,   289,    89,   362,    91,   276,   124,   125,
     279,   661,   281,    91,    92,    93,    80,   109,   317,   318,
      98,   114,   388,   115,   109,    89,   115,    91,   109,   388,
     115,   388,   389,   328,   115,   120,   121,   336,    87,   124,
     125,    90,   120,   108,   611,   612,   124,   697,   337,   421,
      48,    49,   469,   108,   119,   395,   120,   121,   123,   109,
     124,   125,   431,   432,   119,   115,   519,   424,   123,   338,
     108,   111,   525,   547,   527,   115,   529,    91,    92,    93,
     437,   109,    82,   434,    98,   538,    86,   111,    78,    34,
     423,   115,   613,   509,   421,   434,   429,   469,   431,   432,
     590,    82,   589,   466,    85,   468,   120,   114,   471,   111,
     473,   434,   108,   115,   111,   438,   449,   450,   115,   466,
     453,   468,   455,   456,   471,   111,   473,   396,   100,   115,
     102,   464,   465,   466,   466,   421,   468,   104,   129,   471,
     707,   473,   469,    82,   433,   104,    85,   436,   111,   108,
     439,   110,   115,    73,    74,    75,    76,   100,    72,   102,
     119,   484,   519,   616,   123,   130,   523,    85,   525,    87,
     527,   624,   529,   661,   100,   115,   520,   630,   522,   579,
     741,   538,   743,   469,    88,   694,   695,    80,   100,   105,
     102,   712,   115,   667,   712,   484,   129,    24,    91,    92,
      93,   678,   109,   579,   580,    98,   513,   109,   109,   542,
     509,   544,   509,   546,   547,   104,   549,   109,   109,   108,
      23,   110,   109,   116,    90,   115,    27,   120,   121,    85,
     119,   520,   114,   522,   123,   712,    78,   758,    78,    78,
     758,    78,    15,   733,    82,   130,    87,   130,    81,    78,
     115,    87,   585,    81,   611,   612,   105,   552,   115,   616,
     623,   109,   109,   552,   741,   109,   743,   624,   601,   129,
     115,   101,    78,   630,   631,   109,   623,   730,   109,   568,
      78,   758,   101,   760,   109,   105,   129,   613,    80,    81,
      82,   623,   115,    85,   563,    90,   109,   115,   115,    91,
      92,    93,    80,    81,   116,   116,    98,   109,    87,    86,
      86,   115,    25,    91,    92,    93,   109,   116,   109,   129,
      98,   678,   129,   622,   116,   658,   659,    78,   120,   121,
     119,   664,   124,   125,   667,   668,   130,   105,   116,    78,
      78,   116,   120,   121,   116,   116,   124,   125,   115,    78,
     707,    81,   709,   719,    81,   712,   115,   101,   116,   115,
     719,    82,   719,   129,   129,    27,   101,   700,   701,   702,
     116,   105,   105,   730,   114,   101,    80,    81,   115,    82,
     687,    79,   679,   749,   741,    78,   743,    91,    92,    93,
     749,    29,   749,     9,    98,   761,   115,     4,   731,   694,
     695,   758,   761,   760,   761,   694,   695,   109,   109,    48,
      85,    81,   116,    11,    90,    49,   120,   121,    54,   108,
     124,   125,   109,   720,   757,   115,   109,   109,   727,   109,
     727,   127,     3,     4,     5,     6,     7,     8,   509,    10,
      15,    12,    13,   323,   612,    16,    17,    18,    19,    20,
     424,    22,   751,    24,   751,    26,   337,    28,    29,   177,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,   218,    47,   761,   230,    50,
      51,    52,    53,    54,    55,    56,    57,    58,   675,   741,
      80,   522,    63,    64,   316,   681,   710,   720,    34,   552,
     326,    91,    92,    93,   458,    34,    77,   643,    98,   117,
     667,   361,    83,   622,   456,   658,   279,   232,    89,   544,
      91,    92,    59,   105,    43,   447,   116,   613,   102,   206,
     120,   121,    -1,   104,   124,   125,    -1,   108,    -1,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,   119,    -1,
      -1,   122,   123,    -1,    -1,    -1,    -1,   128,   129,   130,
     131,    -1,     3,     4,     5,    -1,    -1,   138,   139,    10,
      -1,    12,    13,    -1,    -1,    16,    17,    18,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    77,    -1,    -1,    -1,
      -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,    -1,    -1,   108,    -1,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,   119,    -1,
      -1,   122,   123,    -1,    -1,    -1,    -1,   128,   129,   130,
     131,     1,    -1,     3,     4,     5,    -1,   138,   139,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,    -1,    -1,   108,    -1,
     110,    -1,    -1,    -1,   114,    -1,    -1,    -1,   118,   119,
      -1,    -1,   122,   123,    -1,     3,     4,     5,   128,   129,
     130,   131,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    57,
      -1,    -1,    -1,    -1,    -1,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    83,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    91,    92,    93,    -1,    -1,    -1,    -1,
      98,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,
     108,   109,   110,    -1,    -1,    -1,    -1,   115,   116,    -1,
     118,   119,   120,   121,   122,   123,   124,   125,    -1,    -1,
     128,   129,   130,   131,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    -1,    -1,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    91,    92,    93,    -1,    -1,    -1,    -1,    98,
      -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,
      -1,   110,   111,    -1,    -1,    -1,   115,   116,    -1,   118,
     119,   120,   121,   122,   123,   124,   125,    -1,    -1,   128,
     129,   130,   131,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    91,    92,    93,    -1,    -1,    -1,    -1,    98,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,    -1,   108,    -1,
     110,    -1,    -1,    -1,    -1,    -1,   116,    -1,   118,   119,
     120,   121,   122,   123,   124,   125,    -1,    -1,   128,   129,
     130,   131,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,    -1,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      91,    92,    93,    -1,    -1,    -1,    -1,    98,    -1,    -1,
      -1,    -1,    -1,   104,    -1,    -1,    -1,   108,    -1,   110,
      -1,    -1,    -1,    -1,    -1,   116,    -1,   118,   119,   120,
     121,   122,   123,   124,   125,    -1,    -1,   128,   129,   130,
     131,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      -1,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    89,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,   104,    -1,    -1,    -1,   108,    -1,   110,    -1,
      -1,    -1,   114,    -1,    -1,    -1,   118,   119,    -1,    -1,
     122,   123,    -1,     3,     4,     5,   128,   129,   130,   131,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    83,    -1,    85,    -1,    -1,    -1,    89,
      -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     100,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,    -1,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,   119,
      -1,    -1,   122,   123,    -1,     3,     4,     5,   128,   129,
     130,   131,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    57,
      -1,    -1,    -1,    -1,    -1,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,
     108,    -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     118,   119,    -1,    -1,   122,   123,    -1,     3,     4,     5,
     128,   129,   130,   131,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    -1,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,    -1,   102,    -1,   104,    -1,
      -1,    -1,   108,    -1,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,   119,    -1,    -1,   122,   123,    -1,     3,
       4,     5,   128,   129,   130,   131,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    25,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,    -1,    -1,   108,    -1,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   118,   119,    -1,    -1,   122,   123,
      -1,     3,     4,     5,   128,   129,   130,   131,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      -1,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    89,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,    -1,   108,    -1,   110,    -1,
      -1,    -1,   114,    -1,    -1,    -1,   118,   119,    -1,    -1,
     122,   123,    -1,     3,     4,     5,   128,   129,   130,   131,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,    -1,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,   119,
      -1,    -1,   122,   123,    -1,     3,     4,     5,   128,   129,
     130,   131,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    57,
      -1,    -1,    -1,    -1,    -1,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,
     108,    -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     118,   119,    -1,    -1,   122,   123,    -1,     3,     4,     5,
     128,   129,   130,   131,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    -1,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
      -1,    -1,   108,    -1,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,   119,    -1,    -1,   122,   123,    -1,     3,
       4,     5,   128,   129,   130,   131,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,    -1,    -1,   108,    -1,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   118,   119,    -1,    -1,   122,   123,
      -1,     3,     4,     5,   128,   129,   130,   131,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    89,    -1,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,    -1,   108,    -1,   110,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   118,   119,    -1,    -1,
     122,   123,    -1,     3,     4,     5,   128,   129,   130,   131,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    47,    -1,    -1,
      -1,    -1,    52,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    83,    -1,    -1,    -1,    87,    -1,    89,
      -1,    -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,    -1,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,   119,
      -1,    -1,   122,   123,    -1,     3,     4,     5,   128,   129,
     130,   131,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    47,
      -1,    -1,    -1,    -1,    52,    53,    54,    55,    56,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,
     108,    -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     118,   119,    -1,    -1,   122,   123,    -1,     3,     4,     5,
     128,   129,   130,   131,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
      -1,    -1,   108,    -1,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,   119,    -1,    -1,   122,   123,    -1,     3,
       4,     5,   128,   129,   130,   131,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    47,    -1,    -1,    -1,    -1,    52,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,
     104,    -1,    -1,    -1,   108,    -1,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   118,   119,    -1,    -1,   122,   123,
      -1,     3,     4,     5,   128,   129,   130,   131,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    47,    -1,    -1,    -1,    -1,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,     4,   108,    -1,   110,    -1,
      -1,    -1,    -1,    12,    -1,    -1,   118,   119,    -1,    -1,
     122,   123,    -1,    -1,    -1,    24,   128,   129,   130,   131,
      29,    -1,    31,    32,    33,    34,    35,    36,    -1,    -1,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    -1,    52,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,
      89,    -1,    91,    92,    93,    -1,    -1,    -1,    -1,    -1,
      -1,   100,    -1,    -1,    -1,   104,    -1,     4,    -1,   108,
     109,   110,    -1,    -1,    -1,    12,   115,   116,    -1,   118,
     119,   120,   121,    -1,   123,   124,   125,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      -1,    -1,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    -1,    52,    53,    54,    55,    56,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    91,    92,    93,    -1,    -1,    -1,
      -1,    -1,    -1,   100,    -1,    -1,    -1,   104,   105,     4,
      -1,   108,    -1,   110,    -1,    -1,    -1,    12,    -1,   116,
      -1,   118,   119,   120,   121,    -1,   123,   124,   125,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    -1,    -1,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    -1,    52,    53,    54,
      55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    91,    92,    93,    -1,
      -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,   104,
      -1,     4,    -1,   108,    -1,   110,    -1,    -1,    -1,    12,
      -1,   116,    -1,   118,   119,   120,   121,    -1,   123,   124,
     125,    24,    -1,    -1,    -1,    -1,    -1,    -1,    31,    32,
      33,    34,    35,    36,    -1,    -1,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    -1,    52,
      53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    91,    92,
      93,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,
      -1,   104,    -1,     4,    -1,   108,   109,   110,    -1,    -1,
      -1,    12,    -1,   116,    -1,   118,   119,   120,   121,    -1,
     123,   124,   125,    24,    -1,    -1,    -1,    -1,    -1,    -1,
      31,    32,    33,    34,    35,    36,    -1,    -1,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      -1,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      91,    92,    93,    -1,    -1,    -1,    -1,    -1,    -1,   100,
      -1,    -1,    -1,   104,     3,     4,    -1,   108,    -1,   110,
      -1,    -1,    -1,    12,    -1,   116,    -1,   118,   119,   120,
     121,    -1,   123,   124,   125,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    -1,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,     4,    -1,    52,    53,    54,    55,    56,    57,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    -1,    -1,    31,    32,
      33,    34,    35,    36,    -1,    -1,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    -1,    52,
      53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,
     119,    -1,    -1,   122,   123,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    90,    -1,    92,
      93,    -1,    -1,    -1,     4,    -1,    -1,   100,    -1,    -1,
      -1,   104,    12,    -1,    -1,   108,    -1,   110,    -1,    -1,
      -1,    -1,    -1,   116,    24,   118,   119,    -1,   121,    -1,
     123,    31,    32,    33,    34,    35,    36,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    -1,    52,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    -1,    -1,    -1,    -1,    -1,
     100,    -1,    -1,    -1,   104,     4,    -1,    -1,   108,    -1,
     110,    -1,    -1,    12,    -1,    -1,   116,    -1,   118,   119,
      -1,   121,    21,   123,    -1,    24,    -1,    26,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    -1,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,     4,    52,    53,    54,    55,    56,    57,    -1,
      12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    24,    -1,    26,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    -1,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    -1,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,   108,
       4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,   118,
     119,    -1,    -1,   122,   123,    -1,    -1,    79,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    -1,    -1,    31,    32,    33,
      34,    35,    36,    -1,    -1,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,   108,    -1,    52,    53,
      54,    55,    56,    57,    -1,    -1,   118,   119,    -1,    -1,
     122,   123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    -1,    -1,     4,    -1,    -1,   100,    -1,    -1,    -1,
     104,    12,    -1,    -1,   108,    -1,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    24,   118,   119,    -1,    -1,    -1,   123,
      31,    32,    33,    34,    35,    36,    -1,     4,    -1,    40,
      41,    42,    43,    44,    -1,    12,    47,    -1,    -1,    -1,
      -1,    52,    53,    54,    55,    56,    57,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      -1,    38,    39,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    -1,    52,    53,    54,    55,    56,
      57,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    24,   118,   119,    -1,
      -1,    29,   123,    31,    32,    33,    34,    35,    36,    -1,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,   108,     4,    -1,    52,    53,    54,    55,    56,    57,
      12,   118,    -1,    -1,    -1,   122,    -1,    -1,    -1,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    -1,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    -1,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     118,    -1,    -1,    -1,   122,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,
      92,    93,     4,    -1,    -1,    -1,    98,    -1,    -1,    -1,
      12,    -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,
      -1,    -1,    24,    -1,   116,    -1,   118,    29,   120,    31,
      32,    33,    34,    35,    36,    -1,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,     4,    -1,
      52,    53,    54,    55,    56,    57,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    -1,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,     4,    52,    53,    54,    55,
      56,    57,   104,    12,    -1,    -1,   108,    -1,   110,    -1,
      -1,    -1,    -1,    -1,    -1,    24,   118,   119,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    -1,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    -1,    52,    53,    54,    55,    56,    57,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,
      -1,    -1,   118,   119,    -1,    -1,    -1,    -1,    -1,    24,
      -1,    -1,    81,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    -1,    38,    39,    40,    41,    42,    43,    44,
      -1,   100,    47,    -1,    -1,     4,    -1,    52,    53,    54,
      55,    56,    57,    12,    -1,    -1,   115,    -1,    -1,   118,
      -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    -1,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,     4,    -1,    52,    53,    54,    55,    56,    57,    12,
      -1,    -1,    -1,   108,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    24,    -1,   118,    -1,    -1,    -1,    -1,    31,    32,
      33,    34,    35,    36,    -1,     4,    -1,    40,    41,    42,
      43,    44,    -1,    12,    47,    -1,    -1,    -1,    -1,    52,
      53,    54,    55,    56,    57,    24,    -1,    -1,    -1,    -1,
      -1,    -1,    31,    32,    33,    34,    35,    36,    -1,   118,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,     4,    52,    53,    54,    55,    56,    57,    -1,
      12,    -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,
      -1,    -1,    24,    -1,    -1,   108,    -1,    -1,    -1,    31,
      32,    33,    34,    35,    36,   118,    -1,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    98,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,   108,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   118
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   100,   141,   142,   143,   146,   119,   123,   329,
     147,   160,     0,   147,    66,    67,   144,   101,   114,   148,
     161,   162,     1,   103,   328,   104,   129,   197,   197,   108,
     149,    14,   163,   174,   175,   129,   198,    78,    78,     4,
      12,    21,    24,    26,    29,    31,    32,    33,    34,    35,
      36,    38,    39,    40,    41,    42,    43,    44,    47,    50,
      52,    53,    54,    55,    56,    57,   108,   118,   119,   122,
     123,   150,   151,   152,   157,   158,   295,   298,   299,   313,
     314,   315,   321,    27,    61,   164,   114,   159,     3,     5,
       6,     7,     8,    10,    13,    16,    17,    18,    19,    20,
      22,    26,    28,    37,    45,    50,    51,    58,    63,    64,
      77,    83,    89,    91,    92,   104,   108,   110,   119,   123,
     128,   129,   130,   131,   138,   139,   172,   176,   177,   178,
     179,   183,   187,   192,   238,   243,   248,   249,   253,   254,
     255,   256,   284,   285,   288,   289,   312,   313,   315,   323,
     324,   327,   105,   115,   329,   108,   294,   298,   104,   108,
     110,   284,    80,    89,    91,    92,    93,    98,   120,   121,
     124,   125,   318,   319,   320,   322,   109,   115,   108,   153,
     100,   102,   145,    78,    34,   165,   114,    64,   108,   246,
     247,   249,   250,   252,    34,    35,    36,    80,    89,    91,
      92,    93,   100,   104,   108,   110,   116,   118,   120,   121,
     124,   125,   185,   205,   206,   209,   211,   213,   215,   216,
     218,   293,   294,   296,   300,   301,   309,   310,   311,   321,
     108,   100,   102,   277,    85,   100,   246,   270,   271,   272,
      73,    74,    75,    76,   180,   100,   102,   194,   195,   211,
     213,   108,   116,   303,   312,   315,   319,   277,   254,   108,
     188,   286,   287,   289,   315,   324,   254,   104,   244,   245,
     129,   108,   313,   286,   287,     5,    92,   254,   275,   276,
     254,   253,   254,    80,   105,   116,   121,   125,   246,   247,
     257,   259,   260,   291,   305,   306,   308,   317,   318,   320,
     325,   326,    91,   109,   115,   257,   258,   318,   319,   320,
     325,   330,   111,   257,   258,   330,    19,   244,   244,   130,
     171,   159,    72,   186,    81,   116,   199,   291,   304,   306,
     307,   316,   318,   319,    99,   254,   100,   115,    88,   129,
      89,   300,   325,   109,   109,   109,   109,   109,   109,   152,
      79,   154,   155,   156,   157,   147,   147,    24,   167,   118,
     129,    23,    81,   304,   246,   203,   231,   232,   233,   312,
      29,   105,   207,   209,   211,    87,    89,   109,   207,   222,
     300,   330,   330,   298,   311,    27,   196,   218,    90,    87,
     216,   207,   221,   222,    20,    46,    92,   246,   274,   278,
     279,   280,   278,   263,   264,   280,   270,   114,   251,   272,
     328,    78,    78,    78,    78,   201,   207,   219,   193,   238,
     239,   248,   193,    15,    82,   319,   315,   130,   130,    87,
     326,    82,    86,   100,   115,   189,   315,    81,   116,   290,
     326,    89,   130,   313,    78,    78,   129,    81,   200,   100,
     102,   265,   254,    87,   275,    82,    85,   240,   241,   242,
       3,   314,   323,   304,    79,    85,   115,   105,   115,   247,
     109,   115,   109,   115,   109,   109,   109,   115,   111,   111,
     111,   219,   313,   313,   116,   173,   290,   302,   303,   129,
     185,   201,   202,   207,   208,   314,   240,   249,   218,    79,
     281,   282,   283,   313,   203,   254,   109,   109,   109,   115,
     101,   328,   129,   166,    78,    78,   265,   201,   250,    81,
     115,   101,   115,   223,   105,    90,   109,    81,   109,   115,
     109,   111,   116,   116,   195,   211,   207,   109,   115,   195,
     277,   254,    86,   101,   114,   328,    87,   115,   101,    25,
     196,   101,   114,   328,   246,   207,   208,   109,   116,   129,
     129,   246,   109,    92,   246,   273,   273,   190,   315,   287,
     189,   202,   324,   315,   130,   105,    78,    78,   104,   108,
     110,   292,   293,    78,   101,   114,   266,   267,   268,   273,
     266,   328,   246,   246,   263,   196,   242,   116,   116,   116,
     246,    25,   261,   262,   280,   246,   257,   257,   257,   257,
      78,    81,    81,   115,    78,   129,    81,    82,   184,   226,
     116,   101,   115,    82,    81,   156,   329,   207,   312,   233,
      98,   108,   224,   309,   207,   207,   225,   207,   207,   246,
     279,   246,   280,   246,   239,   129,   129,   254,    27,   191,
     101,   189,   116,   105,   109,   111,   266,   101,   114,    87,
     269,   270,   328,   196,    82,    47,   246,   115,    79,   201,
     204,   204,   302,    78,   225,    29,   227,   228,   229,     9,
     234,   235,   236,   282,   257,   202,   115,     4,   168,   207,
     309,   109,   109,   251,   100,   102,   268,   246,   196,   246,
      48,    49,    48,   262,   280,   246,    78,   115,    78,   223,
      85,   210,   214,   217,   218,   230,    22,    52,    53,   108,
     181,   237,   297,   298,   236,   329,    12,   108,   169,   170,
      81,    11,   193,   193,   196,   246,   246,   246,   201,    98,
     228,    90,   217,   290,   109,   211,   212,   220,   237,    54,
     182,   108,   150,   225,   246,   101,   328,    49,   214,   230,
     214,   115,   109,   211,   150,   109,   109,   246,   220,   109
  };

  const short
  parser::yyr1_[] =
  {
       0,   140,   141,   142,   142,   143,   144,   144,   144,   145,
     145,   146,   146,   147,   148,   148,   148,   149,   149,   150,
     151,   151,   152,   152,   152,   153,   153,   154,   154,   155,
     155,   156,   156,   157,   157,   158,   158,   159,   159,   160,
     160,   161,   162,   162,   163,   164,   164,   165,   165,   166,
     166,   167,   167,   168,   168,   169,   169,   170,   170,   171,
     171,   172,   172,   172,   173,   173,   174,   175,   175,   176,
     176,   176,   176,   176,   176,   176,   176,   176,   176,   177,
     178,   178,   178,   179,   180,   180,   180,   180,   180,   181,
     181,   181,   182,   183,   183,   184,   184,   185,   185,   186,
     186,   186,   187,   187,   187,   188,   188,   188,   189,   189,
     190,   191,   191,   192,   193,   193,   193,   193,   194,   194,
     195,   196,   196,   197,   197,   198,   198,   198,   199,   199,
     200,   200,   201,   202,   203,   203,   204,   204,   205,   206,
     206,   207,   207,   207,   208,   209,   210,   211,   211,   212,
     213,   214,   214,   215,   215,   216,   216,   216,   217,   218,
     218,   218,   218,   218,   218,   218,   218,   218,   218,   219,
     220,   220,   221,   221,   222,   222,   223,   223,   224,   224,
     225,   226,   227,   227,   228,   228,   229,   229,   230,   230,
     231,   231,   232,   232,   233,   234,   234,   235,   235,   236,
     236,   236,   237,   237,   237,   238,   238,   238,   238,   239,
     240,   240,   241,   241,   242,   243,   243,   243,   243,   243,
     243,   243,   243,   243,   243,   243,   244,   244,   245,   245,
     246,   246,   247,   247,   248,   248,   249,   249,   249,   250,
     250,   251,   251,   252,   252,   253,   253,   253,   253,   254,
     254,   254,   254,   254,   254,   254,   254,   254,   254,   254,
     254,   255,   255,   256,   256,   256,   256,   256,   256,   256,
     256,   256,   257,   257,   257,   258,   258,   259,   259,   259,
     259,   259,   259,   259,   260,   260,   261,   261,   261,   261,
     262,   262,   262,   262,   263,   264,   264,   265,   265,   265,
     265,   266,   266,   267,   267,   267,   268,   269,   269,   270,
     270,   271,   271,   272,   273,   273,   274,   274,   275,   275,
     276,   276,   277,   277,   278,   278,   278,   278,   279,   279,
     280,   280,   280,   281,   281,   282,   282,   282,   283,   283,
     284,   284,   285,   285,   286,   286,   286,   287,   287,   288,
     288,   288,   288,   289,   289,   290,   290,   291,   291,   292,
     292,   292,   293,   293,   293,   293,   293,   294,   294,   294,
     295,   295,   295,   295,   295,   296,   296,   297,   298,   298,
     299,   300,   300,   300,   301,   301,   301,   301,   302,   302,
     303,   303,   304,   304,   304,   305,   305,   305,   306,   307,
     307,   308,   308,   309,   310,   311,   311,   311,   311,   311,
     312,   312,   313,   313,   313,   314,   314,   315,   315,   315,
     315,   315,   315,   315,   315,   316,   316,   317,   317,   318,
     319,   319,   320,   320,   321,   321,   321,   321,   321,   321,
     321,   321,   321,   321,   321,   321,   321,   321,   321,   321,
     321,   321,   322,   322,   322,   323,   323,   324,   325,   325,
     326,   326,   327,   327,   327,   327,   328,   328,   329,   329,
     330,   330
  };

  const signed char
  parser::yyr2_[] =
  {
       0,     2,     1,     6,     1,     0,     3,     3,     0,     3,
       3,     3,     3,     2,     2,     2,     1,     3,     0,     1,
       3,     1,     2,     2,     2,     0,     3,     0,     1,     4,
       1,     1,     1,     1,     2,     1,     1,     2,     1,     2,
       0,     2,     3,     0,     8,     2,     0,     1,     0,     1,
       0,     1,     0,     2,     0,     1,     0,     3,     4,     0,
       1,     1,     1,     1,     3,     1,     2,     3,     0,     1,
       1,     1,     4,     1,     1,     5,     4,     5,     4,     3,
       4,     5,     4,     4,     2,     2,     2,     2,     0,     1,
       1,     1,     2,     1,     1,     0,     2,     3,     1,     4,
       3,     0,     4,     4,     5,     2,     3,     4,     0,     2,
       2,     4,     4,     4,     3,     2,     1,     0,     3,     3,
       1,     2,     0,     1,     3,     3,     1,     0,     0,     2,
       0,     2,     1,     1,     3,     1,     1,     3,     1,     1,
       1,     4,     3,     1,     1,     1,     1,     1,     3,     1,
       1,     1,     2,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     2,     3,     2,     5,     3,     3,     5,     1,
       1,     3,     1,     0,     1,     3,     2,     0,     1,     5,
       1,     2,     3,     1,     4,     2,     3,     0,     1,     3,
       0,     1,     3,     1,     3,     0,     1,     2,     1,     2,
       3,     3,     1,     2,     3,     1,     3,     3,     1,     1,
       3,     2,     2,     1,     4,     3,     5,     3,     1,     4,
       4,     3,     4,     6,     6,     4,     0,     1,     3,     4,
       3,     1,     1,     3,     1,     3,     2,     3,     1,     1,
       2,     1,     0,     3,     3,     2,     3,     2,     1,     3,
       2,     4,     4,     3,     8,     2,     4,     2,     2,     4,
       1,     4,     1,     1,     1,     1,     3,     3,     3,     3,
       3,     1,     1,     2,     2,     3,     3,     1,     1,     2,
       4,     3,     5,     3,     3,     3,     3,     3,     1,     1,
       2,     4,     4,     6,     1,     3,     1,     3,     3,     2,
       2,     1,     2,     3,     2,     1,     2,     3,     2,     2,
       1,     3,     2,     4,     1,     2,     1,     2,     1,     2,
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
  "\"proc\"", "\"rec\"", "\"group\"", "\"by\"", "\"using\"", "\"pattern\"",
  "\"static\"", "\"stock\"", "\"anyclass\"", "\"via\"", "\"unit\"",
  "\"signature\"", "\"dependency\"", "\"{-# INLINE\"",
  "\"{-# SPECIALIZE\"", "\"{-# SPECIALIZE_INLINE\"", "\"{-# SOURCE\"",
  "\"{-# RULES\"", "\"{-# CORE\"", "\"{-# SCC\"", "\"{-# GENERATED\"",
  "\"{-# DEPRECATED\"", "\"{-# WARNING\"", "\"{-# UNPACK\"",
  "\"{-# NOUNPACK\"", "\"{-# ANN\"", "\"{-# MINIMAL\"", "\"{-# CTYPE\"",
  "\"{-# OVERLAPPING\"", "\"{-# OVERLAPPABLE\"", "\"{-# OVERLAPS\"",
  "\"{-# INCOHERENT\"", "\"{-# COMPLETE\"", "\"#-}\"", "\"..\"", "\":\"",
  "\"::\"", "\"=\"", "\"\\\\\"", "\"lcase\"", "\"|\"", "\"<-\"", "\"->\"",
  "\"@\"", "\"~\"", "\"=>\"", "\"-\"", "\"!\"", "\"*\"", "\"-<\"",
  "\">-\"", "\"-<<\"", "\">>-\"", "\".\"", "\"TYPEAPP\"", "\"{\"", "\"}\"",
  "\"vocurly\"", "\"vccurly\"", "\"[\"", "\"]\"", "\"[:\"", "\":]\"",
  "\"(\"", "\")\"", "\"(#\"", "\"#)\"", "\"(|\"", "\"|)\"", "\";\"",
  "\",\"", "\"`\"", "\"'\"", "\"VARID\"", "\"CONID\"", "\"VARSYM\"",
  "\"CONSYM\"", "\"QVARID\"", "\"QCONID\"", "\"QVARSYM\"", "\"QCONSYM\"",
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
  "capi_ctype", "pattern_synonym_decl", "pattern_synonym_lhs", "vars0",
  "cvars1", "where_decls", "pattern_synonym_sig", "decls", "decllist",
  "binds", "wherebinds", "strings", "stringlist", "opt_sig",
  "opt_tyconsig", "sigtype", "sigtypedoc", "sig_vars", "sigtypes1",
  "strict_mark", "strictness", "ctype", "ctypedoc", "context",
  "context_no_ops", "type", "typedoc", "btype", "btype_no_ops", "tyapps",
  "tyapp", "atype_docs", "atype", "inst_type", "deriv_types",
  "comma_types0", "comma_types1", "tv_bndrs", "tv_bndr", "kind", "constrs",
  "constrs1", "constr", "forall", "constr_stuff", "fielddecls",
  "fielddecls1", "fielddecl", "maybe_derivings", "derivings", "deriving",
  "deriv_clause_types", "decl_no_th", "decl", "rhs", "gdrhs", "gdrh",
  "sigdecl", "activation", "explicit_activation", "exp", "infixexp",
  "infixexp_top", "exp10_top", "exp10", "optSemi", "scc_annot", "fexp",
  "aexp", "aexp1", "aexp2", "texp", "tup_exprs", "list", "lexps", "squals",
  "transformqual", "guardquals", "guardquals1", "altslist", "alts",
  "alts1", "alt", "alt_rhs", "gdpats", "ifgdpats", "gdpat", "pat",
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
       0,   515,   515,   532,   533,   535,   539,   540,   541,   543,
     544,   546,   547,   550,   552,   553,   554,   562,   563,   565,
     567,   568,   570,   571,   572,   574,   575,   577,   578,   580,
     581,   583,   584,   586,   587,   589,   590,   594,   595,   597,
     598,   600,   602,   603,   605,   614,   615,   617,   618,   620,
     621,   623,   624,   626,   627,   629,   630,   632,   633,   638,
     639,   641,   642,   643,   645,   646,   650,   652,   653,   655,
     656,   657,   660,   667,   669,   670,   671,   672,   673,   675,
     677,   678,   679,   684,   689,   690,   691,   692,   693,   695,
     696,   697,   699,   739,   740,   742,   743,   752,   753,   755,
     756,   757,   774,   775,   776,   778,   779,   780,   782,   783,
     785,   787,   788,   791,   797,   798,   799,   800,   802,   803,
     805,   807,   808,   816,   817,   819,   820,   821,   829,   830,
     832,   833,   835,   837,   839,   840,   842,   843,   847,   853,
     854,   861,   862,   864,   866,   875,   877,   879,   880,   882,
     885,   887,   888,   890,   891,   893,   894,   895,   901,   908,
     909,   910,   911,   912,   913,   914,   920,   921,   922,   925,
     927,   928,   930,   931,   933,   934,   941,   942,   944,   945,
     963,   969,   971,   972,   974,   975,   977,   978,   980,   981,
     983,   984,   986,   987,   989,   991,   992,   994,   995,   997,
     998,   999,  1001,  1002,  1003,  1008,  1010,  1012,  1013,  1016,
    1020,  1021,  1023,  1024,  1028,  1030,  1031,  1032,  1033,  1034,
    1035,  1036,  1037,  1038,  1039,  1040,  1042,  1043,  1045,  1046,
    1050,  1051,  1053,  1054,  1056,  1057,  1059,  1060,  1061,  1063,
    1064,  1067,  1068,  1070,  1071,  1075,  1076,  1077,  1078,  1080,
    1081,  1082,  1083,  1084,  1085,  1086,  1087,  1088,  1089,  1090,
    1091,  1093,  1094,  1096,  1097,  1098,  1099,  1100,  1101,  1102,
    1103,  1104,  1109,  1110,  1111,  1116,  1117,  1135,  1136,  1137,
    1138,  1139,  1140,  1141,  1143,  1144,  1156,  1157,  1158,  1159,
    1161,  1162,  1163,  1164,  1167,  1169,  1170,  1173,  1174,  1175,
    1176,  1178,  1179,  1181,  1182,  1183,  1185,  1187,  1188,  1190,
    1191,  1193,  1194,  1196,  1198,  1199,  1201,  1202,  1204,  1205,
    1207,  1208,  1211,  1212,  1214,  1215,  1216,  1217,  1222,  1223,
    1225,  1226,  1227,  1232,  1233,  1235,  1236,  1237,  1239,  1240,
    1272,  1273,  1275,  1276,  1278,  1279,  1280,  1282,  1283,  1285,
    1286,  1287,  1288,  1290,  1291,  1293,  1294,  1296,  1297,  1300,
    1301,  1302,  1304,  1305,  1306,  1307,  1308,  1310,  1311,  1312,
    1314,  1315,  1316,  1317,  1318,  1321,  1322,  1324,  1326,  1327,
    1331,  1333,  1334,  1335,  1337,  1338,  1339,  1340,  1345,  1346,
    1348,  1349,  1351,  1352,  1353,  1355,  1356,  1357,  1359,  1361,
    1362,  1364,  1365,  1369,  1371,  1373,  1374,  1375,  1376,  1377,
    1380,  1381,  1383,  1384,  1385,  1387,  1388,  1390,  1391,  1392,
    1393,  1394,  1395,  1396,  1397,  1399,  1400,  1402,  1403,  1405,
    1407,  1408,  1410,  1411,  1413,  1414,  1415,  1416,  1417,  1418,
    1419,  1420,  1421,  1422,  1423,  1424,  1425,  1426,  1427,  1428,
    1429,  1430,  1432,  1433,  1434,  1438,  1439,  1441,  1443,  1444,
    1446,  1447,  1451,  1452,  1453,  1454,  1459,  1462,  1466,  1467,
    1469,  1470
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
#line 6018 "parser.cc"

#line 1479 "parser.y"


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

Haskell::TypeVar make_type_var(const string& id)
{
    return Haskell::TypeVar(id);
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

expression_ref make_do(const vector<expression_ref>& stmts)
{
    return new expression(AST_node("Do"), stmts);
}

expression_ref make_mdo(const vector<expression_ref>& stmts)
{
    return new expression(AST_node("MDo"), stmts);
}

expression_ref yy_make_tuple(const vector<expression_ref>& elements)
{
    return Haskell::Tuple(elements);
}

expression_ref make_list(const vector<expression_ref>& elements)
{
    return Haskell::List(elements);
}

expression_ref make_alts(const vector<expression_ref>& alts)
{
    return expression_ref(AST_node("alts"), alts);
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

expression_ref make_stmts(const vector<expression_ref>& stmts)
{
    return new expression(AST_node("Stmts"), stmts);
}

expression_ref make_infix(const string& infix, optional<int>& prec, vector<string>& op_names)
{
    expression_ref ops = new expression(AST_node("Ops"), make_String_vec(op_names));

    vector<expression_ref> e;
    e.push_back(String(infix));
    if (prec)
	e.push_back(*prec);
    e.push_back(ops);

    return new expression(AST_node("FixityDecl"),e);
}

expression_ref yy_make_string(const std::string& s)
{
    vector<expression_ref> chars;
    for(char c: s)
	chars.push_back(c);
    return make_list(chars);
}

