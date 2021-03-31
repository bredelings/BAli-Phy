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
#line 903 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2370 "parser.cc"
    break;

  case 160: // atype: tyvar
#line 904 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2376 "parser.cc"
    break;

  case 161: // atype: "*"
#line 905 "parser.y"
                                       {yylhs.value.as < expression_ref > () = AST_node("kind_star");}
#line 2382 "parser.cc"
    break;

  case 162: // atype: strict_mark atype
#line 906 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_strict_lazy_type(yystack_[1].value.as < Haskell::StrictLazy > (),yystack_[0].value.as < expression_ref > ());}
#line 2388 "parser.cc"
    break;

  case 163: // atype: "{" fielddecls "}"
#line 907 "parser.y"
                                       {yylhs.value.as < expression_ref > () = expression_ref{AST_node("FieldDecls"),yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 2394 "parser.cc"
    break;

  case 164: // atype: "(" ")"
#line 908 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var("()");}
#line 2400 "parser.cc"
    break;

  case 165: // atype: "(" comma_types1 "," ctype ")"
#line 909 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<expression_ref> > ();ts.push_back(yystack_[1].value.as < expression_ref > ());yylhs.value.as < expression_ref > () = make_tuple_type(ts);}
#line 2406 "parser.cc"
    break;

  case 166: // atype: "(#" "#)"
#line 910 "parser.y"
                                       {}
#line 2412 "parser.cc"
    break;

  case 167: // atype: "(#" comma_types1 "#)"
#line 911 "parser.y"
                                       {}
#line 2418 "parser.cc"
    break;

  case 168: // atype: "(#" bar_types2 "#)"
#line 912 "parser.y"
                                       {}
#line 2424 "parser.cc"
    break;

  case 169: // atype: "[" ctype "]"
#line 913 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_list_type(yystack_[1].value.as < expression_ref > ());}
#line 2430 "parser.cc"
    break;

  case 170: // atype: "(" ctype ")"
#line 914 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2436 "parser.cc"
    break;

  case 171: // atype: "(" ctype "::" kind ")"
#line 915 "parser.y"
                                       {yylhs.value.as < expression_ref > () = expression_ref(AST_node("TypeOfKind"),{yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()});}
#line 2442 "parser.cc"
    break;

  case 172: // inst_type: sigtype
#line 918 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2448 "parser.cc"
    break;

  case 175: // comma_types0: comma_types1
#line 923 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2454 "parser.cc"
    break;

  case 176: // comma_types0: %empty
#line 924 "parser.y"
                                       {}
#line 2460 "parser.cc"
    break;

  case 177: // comma_types1: ctype
#line 926 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2466 "parser.cc"
    break;

  case 178: // comma_types1: comma_types1 "," ctype
#line 927 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2472 "parser.cc"
    break;

  case 181: // tv_bndrs: tv_bndrs tv_bndr
#line 932 "parser.y"
                               {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2478 "parser.cc"
    break;

  case 182: // tv_bndrs: %empty
#line 933 "parser.y"
                               {}
#line 2484 "parser.cc"
    break;

  case 183: // tv_bndr: tyvar
#line 935 "parser.y"
                                    {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2490 "parser.cc"
    break;

  case 184: // tv_bndr: "(" tyvar "::" kind ")"
#line 936 "parser.y"
                                    {yylhs.value.as < expression_ref > () = new expression(AST_node("type_of_kind"),{make_type_var(yystack_[3].value.as < std::string > ()),yystack_[1].value.as < expression_ref > ()});}
#line 2496 "parser.cc"
    break;

  case 185: // kind: ctype
#line 954 "parser.y"
             {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2502 "parser.cc"
    break;

  case 186: // constrs: "=" constrs1
#line 960 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2508 "parser.cc"
    break;

  case 187: // constrs1: constrs1 "|" constr
#line 962 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2514 "parser.cc"
    break;

  case 188: // constrs1: constr
#line 963 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2520 "parser.cc"
    break;

  case 189: // constr: forall context_no_ops "=>" constr_stuff
#line 965 "parser.y"
                                                {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2526 "parser.cc"
    break;

  case 190: // constr: forall constr_stuff
#line 966 "parser.y"
                                                {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2532 "parser.cc"
    break;

  case 191: // forall: "forall" tv_bndrs "."
#line 968 "parser.y"
                                {if (yystack_[1].value.as < std::vector<expression_ref> > ().size()>1) yylhs.value.as < expression_ref > () = make_tv_bndrs(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2538 "parser.cc"
    break;

  case 192: // forall: %empty
#line 969 "parser.y"
                                {}
#line 2544 "parser.cc"
    break;

  case 193: // constr_stuff: btype_no_ops
#line 971 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2550 "parser.cc"
    break;

  case 194: // constr_stuff: btype_no_ops conop btype_no_ops
#line 972 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps({make_type_var(yystack_[1].value.as < std::string > ()),make_tyapps(yystack_[2].value.as < std::vector<expression_ref> > ()),make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 2556 "parser.cc"
    break;

  case 195: // fielddecls: %empty
#line 974 "parser.y"
                                {}
#line 2562 "parser.cc"
    break;

  case 196: // fielddecls: fielddecls1
#line 975 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2568 "parser.cc"
    break;

  case 197: // fielddecls1: fielddecls1 "," fielddecl
#line 977 "parser.y"
                                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2574 "parser.cc"
    break;

  case 198: // fielddecls1: fielddecl
#line 978 "parser.y"
                                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2580 "parser.cc"
    break;

  case 199: // fielddecl: sig_vars "::" ctype
#line 980 "parser.y"
                                        {yylhs.value.as < Haskell::FieldDecl > () = make_field_decl(yystack_[2].value.as < std::vector<std::string> > (),yystack_[0].value.as < expression_ref > ());}
#line 2586 "parser.cc"
    break;

  case 210: // decl_no_th: sigdecl
#line 999 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2592 "parser.cc"
    break;

  case 211: // decl_no_th: "!" aexp rhs
#line 1001 "parser.y"
                              {yylhs.value.as < expression_ref > () = new expression(AST_node("Decl:Strict"),{(yystack_[1].value.as < expression_ref > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2598 "parser.cc"
    break;

  case 212: // decl_no_th: infixexp_top opt_sig rhs
#line 1003 "parser.y"
                              {yylhs.value.as < expression_ref > () = new expression(AST_node("Decl"),{make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2604 "parser.cc"
    break;

  case 213: // decl_no_th: pattern_synonym_decl
#line 1004 "parser.y"
                              {}
#line 2610 "parser.cc"
    break;

  case 214: // decl: decl_no_th
#line 1007 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2616 "parser.cc"
    break;

  case 215: // rhs: "=" exp wherebinds
#line 1011 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_rhs(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2622 "parser.cc"
    break;

  case 216: // rhs: gdrhs wherebinds
#line 1012 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2628 "parser.cc"
    break;

  case 217: // gdrhs: gdrhs gdrh
#line 1014 "parser.y"
                              {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2634 "parser.cc"
    break;

  case 218: // gdrhs: gdrh
#line 1015 "parser.y"
                              {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2640 "parser.cc"
    break;

  case 219: // gdrh: "|" guardquals "=" exp
#line 1019 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2646 "parser.cc"
    break;

  case 220: // sigdecl: infixexp_top "::" sigtypedoc
#line 1021 "parser.y"
                                             { yylhs.value.as < expression_ref > () = expression_ref(AST_node("Decl:sigtype"),{make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2652 "parser.cc"
    break;

  case 221: // sigdecl: var "," sig_vars "::" sigtypedoc
#line 1022 "parser.y"
                                          {}
#line 2658 "parser.cc"
    break;

  case 222: // sigdecl: infix prec ops
#line 1023 "parser.y"
                         { yylhs.value.as < expression_ref > () = make_infix(yystack_[2].value.as < std::string > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()); }
#line 2664 "parser.cc"
    break;

  case 223: // sigdecl: pattern_synonym_sig
#line 1024 "parser.y"
                             {}
#line 2670 "parser.cc"
    break;

  case 224: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1025 "parser.y"
                                                    {}
#line 2676 "parser.cc"
    break;

  case 225: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1026 "parser.y"
                                            {}
#line 2682 "parser.cc"
    break;

  case 226: // sigdecl: "{-# SCC" qvar "#-}"
#line 1027 "parser.y"
                              {}
#line 2688 "parser.cc"
    break;

  case 227: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1028 "parser.y"
                                     {}
#line 2694 "parser.cc"
    break;

  case 228: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1029 "parser.y"
                                                               {}
#line 2700 "parser.cc"
    break;

  case 229: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1030 "parser.y"
                                                                      {}
#line 2706 "parser.cc"
    break;

  case 230: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1031 "parser.y"
                                                     {}
#line 2712 "parser.cc"
    break;

  case 235: // exp: infixexp "::" sigtype
#line 1041 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_typed_exp(make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()); }
#line 2718 "parser.cc"
    break;

  case 236: // exp: infixexp
#line 1042 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 2724 "parser.cc"
    break;

  case 237: // infixexp: exp10
#line 1044 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2730 "parser.cc"
    break;

  case 238: // infixexp: infixexp qop exp10
#line 1045 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_id(yystack_[1].location,yystack_[1].value.as < std::string > ())); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2736 "parser.cc"
    break;

  case 239: // infixexp_top: exp10_top
#line 1047 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2742 "parser.cc"
    break;

  case 240: // infixexp_top: infixexp_top qop exp10_top
#line 1048 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_id(yystack_[1].location,yystack_[1].value.as < std::string > ())); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2748 "parser.cc"
    break;

  case 241: // exp10_top: "-" fexp
#line 1050 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_minus(make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2754 "parser.cc"
    break;

  case 242: // exp10_top: "{-# CORE" "STRING" "#-}"
#line 1051 "parser.y"
                                   {}
#line 2760 "parser.cc"
    break;

  case 243: // exp10_top: fexp
#line 1052 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2766 "parser.cc"
    break;

  case 244: // exp10: exp10_top
#line 1054 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2772 "parser.cc"
    break;

  case 245: // exp10: scc_annot exp
#line 1055 "parser.y"
                                 {}
#line 2778 "parser.cc"
    break;

  case 250: // fexp: fexp aexp
#line 1066 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2784 "parser.cc"
    break;

  case 251: // fexp: fexp "TYPEAPP" atype
#line 1067 "parser.y"
                                 {}
#line 2790 "parser.cc"
    break;

  case 252: // fexp: "static" aexp
#line 1068 "parser.y"
                                 {}
#line 2796 "parser.cc"
    break;

  case 253: // fexp: aexp
#line 1069 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2802 "parser.cc"
    break;

  case 254: // aexp: qvar "@" aexp
#line 1071 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_as_pattern(make_id(yystack_[2].location,yystack_[2].value.as < std::string > ()),yystack_[0].value.as < expression_ref > ());}
#line 2808 "parser.cc"
    break;

  case 255: // aexp: "~" aexp
#line 1072 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lazy_pattern(yystack_[0].value.as < expression_ref > ());}
#line 2814 "parser.cc"
    break;

  case 256: // aexp: "\\" apats1 "->" exp
#line 1073 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lambda(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2820 "parser.cc"
    break;

  case 257: // aexp: "let" binds "in" exp
#line 1074 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_let(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2826 "parser.cc"
    break;

  case 258: // aexp: "\\" "case" altslist
#line 1075 "parser.y"
                                 {}
#line 2832 "parser.cc"
    break;

  case 259: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1076 "parser.y"
                                                       {yylhs.value.as < expression_ref > () = make_if(yystack_[6].value.as < expression_ref > (),yystack_[3].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2838 "parser.cc"
    break;

  case 260: // aexp: "if" ifgdpats
#line 1077 "parser.y"
                                 {}
#line 2844 "parser.cc"
    break;

  case 261: // aexp: "case" exp "of" altslist
#line 1078 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_case(yystack_[2].value.as < expression_ref > (),make_alts(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2850 "parser.cc"
    break;

  case 262: // aexp: "do" stmtlist
#line 1079 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_do(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2856 "parser.cc"
    break;

  case 263: // aexp: "mdo" stmtlist
#line 1080 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_mdo(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2862 "parser.cc"
    break;

  case 264: // aexp: "proc" aexp "->" exp
#line 1081 "parser.y"
                                 {}
#line 2868 "parser.cc"
    break;

  case 265: // aexp: aexp1
#line 1082 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2874 "parser.cc"
    break;

  case 266: // aexp1: aexp1 "{" fbinds "}"
#line 1084 "parser.y"
                              {}
#line 2880 "parser.cc"
    break;

  case 267: // aexp1: aexp2
#line 1085 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2886 "parser.cc"
    break;

  case 268: // aexp2: qvar
#line 1087 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_id(yystack_[0].location,yystack_[0].value.as < std::string > ());}
#line 2892 "parser.cc"
    break;

  case 269: // aexp2: qcon
#line 1088 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_id(yystack_[0].location,yystack_[0].value.as < std::string > ());}
#line 2898 "parser.cc"
    break;

  case 270: // aexp2: literal
#line 1089 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2904 "parser.cc"
    break;

  case 271: // aexp2: "(" texp ")"
#line 1090 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2910 "parser.cc"
    break;

  case 272: // aexp2: "(" tup_exprs ")"
#line 1091 "parser.y"
                              {yylhs.value.as < expression_ref > () = yy_make_tuple(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2916 "parser.cc"
    break;

  case 273: // aexp2: "(#" texp "#)"
#line 1092 "parser.y"
                              {}
#line 2922 "parser.cc"
    break;

  case 274: // aexp2: "(#" tup_exprs "#)"
#line 1093 "parser.y"
                              {}
#line 2928 "parser.cc"
    break;

  case 275: // aexp2: "[" list "]"
#line 1094 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2934 "parser.cc"
    break;

  case 276: // aexp2: "_"
#line 1095 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::WildcardPattern();}
#line 2940 "parser.cc"
    break;

  case 277: // texp: exp
#line 1100 "parser.y"
                      {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2946 "parser.cc"
    break;

  case 278: // texp: infixexp qop
#line 1101 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("LeftSection"),{make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ()),make_id(yystack_[0].location,yystack_[0].value.as < std::string > ())});}
#line 2952 "parser.cc"
    break;

  case 279: // texp: qopm infixexp
#line 1102 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("RightSection"),{make_id(yystack_[1].location,yystack_[1].value.as < std::string > ()),make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 2958 "parser.cc"
    break;

  case 280: // tup_exprs: tup_exprs "," texp
#line 1107 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2964 "parser.cc"
    break;

  case 281: // tup_exprs: texp "," texp
#line 1108 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2970 "parser.cc"
    break;

  case 282: // list: texp
#line 1126 "parser.y"
                                 { yylhs.value.as < expression_ref > () = make_list({yystack_[0].value.as < expression_ref > ()}); }
#line 2976 "parser.cc"
    break;

  case 283: // list: lexps
#line 1127 "parser.y"
                                 { yylhs.value.as < expression_ref > () = make_list(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 2982 "parser.cc"
    break;

  case 284: // list: texp ".."
#line 1128 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFrom"),{yystack_[1].value.as < expression_ref > ()}); }
#line 2988 "parser.cc"
    break;

  case 285: // list: texp "," exp ".."
#line 1129 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThen"),{yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()}); }
#line 2994 "parser.cc"
    break;

  case 286: // list: texp ".." exp
#line 1130 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromTo"),{yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 3000 "parser.cc"
    break;

  case 287: // list: texp "," exp ".." exp
#line 1131 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThenTo"),{yystack_[4].value.as < expression_ref > (),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 3006 "parser.cc"
    break;

  case 288: // list: texp "|" squals
#line 1132 "parser.y"
                                 { auto quals = yystack_[0].value.as < std::vector<expression_ref> > (); quals.push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < expression_ref > () = expression_ref(AST_node("ListComprehension"),quals); }
#line 3012 "parser.cc"
    break;

  case 289: // lexps: lexps "," texp
#line 1134 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3018 "parser.cc"
    break;

  case 290: // lexps: texp "," texp
#line 1135 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3024 "parser.cc"
    break;

  case 291: // squals: squals "," transformqual
#line 1147 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3030 "parser.cc"
    break;

  case 292: // squals: squals "," qual
#line 1148 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3036 "parser.cc"
    break;

  case 293: // squals: transformqual
#line 1149 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3042 "parser.cc"
    break;

  case 294: // squals: qual
#line 1150 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3048 "parser.cc"
    break;

  case 295: // transformqual: "then" exp
#line 1152 "parser.y"
                                                    {}
#line 3054 "parser.cc"
    break;

  case 296: // transformqual: "then" exp "by" exp
#line 1153 "parser.y"
                                                    {}
#line 3060 "parser.cc"
    break;

  case 297: // transformqual: "then" "group" "using" exp
#line 1154 "parser.y"
                                                    {}
#line 3066 "parser.cc"
    break;

  case 298: // transformqual: "then" "group" "by" exp "using" exp
#line 1155 "parser.y"
                                                    {}
#line 3072 "parser.cc"
    break;

  case 299: // guardquals: guardquals1
#line 1158 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3078 "parser.cc"
    break;

  case 300: // guardquals1: guardquals1 "," qual
#line 1160 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > ();yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3084 "parser.cc"
    break;

  case 301: // guardquals1: qual
#line 1161 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3090 "parser.cc"
    break;

  case 302: // altslist: "{" alts "}"
#line 1164 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3096 "parser.cc"
    break;

  case 303: // altslist: "vocurly" alts close
#line 1165 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3102 "parser.cc"
    break;

  case 304: // altslist: "{" "}"
#line 1166 "parser.y"
                                 {}
#line 3108 "parser.cc"
    break;

  case 305: // altslist: "vocurly" close
#line 1167 "parser.y"
                                 {}
#line 3114 "parser.cc"
    break;

  case 306: // alts: alts1
#line 1169 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3120 "parser.cc"
    break;

  case 307: // alts: ";" alts
#line 1170 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3126 "parser.cc"
    break;

  case 308: // alts1: alts1 ";" alt
#line 1172 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3132 "parser.cc"
    break;

  case 309: // alts1: alts1 ";"
#line 1173 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3138 "parser.cc"
    break;

  case 310: // alts1: alt
#line 1174 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3144 "parser.cc"
    break;

  case 311: // alt: pat alt_rhs
#line 1176 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yy_make_alt(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3150 "parser.cc"
    break;

  case 312: // alt_rhs: "->" exp wherebinds
#line 1178 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_rhs(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3156 "parser.cc"
    break;

  case 313: // alt_rhs: gdpats wherebinds
#line 1179 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3162 "parser.cc"
    break;

  case 314: // gdpats: gdpats gdpat
#line 1181 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3168 "parser.cc"
    break;

  case 315: // gdpats: gdpat
#line 1182 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3174 "parser.cc"
    break;

  case 316: // ifgdpats: "{" gdpats "}"
#line 1184 "parser.y"
                                 {}
#line 3180 "parser.cc"
    break;

  case 317: // ifgdpats: gdpats close
#line 1185 "parser.y"
                                 {}
#line 3186 "parser.cc"
    break;

  case 318: // gdpat: "|" guardquals "->" exp
#line 1187 "parser.y"
                                 {yylhs.value.as < expression_ref > ()=make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3192 "parser.cc"
    break;

  case 319: // pat: exp
#line 1189 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3198 "parser.cc"
    break;

  case 320: // pat: "!" aexp
#line 1190 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3204 "parser.cc"
    break;

  case 321: // bindpat: exp
#line 1192 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3210 "parser.cc"
    break;

  case 322: // bindpat: "!" aexp
#line 1193 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3216 "parser.cc"
    break;

  case 323: // apat: aexp
#line 1195 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3222 "parser.cc"
    break;

  case 324: // apat: "!" aexp
#line 1196 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3228 "parser.cc"
    break;

  case 325: // apats1: apats1 apat
#line 1198 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3234 "parser.cc"
    break;

  case 326: // apats1: apat
#line 1199 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3240 "parser.cc"
    break;

  case 327: // stmtlist: "{" stmts "}"
#line 1202 "parser.y"
                               {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3246 "parser.cc"
    break;

  case 328: // stmtlist: "vocurly" stmts close
#line 1203 "parser.y"
                               {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3252 "parser.cc"
    break;

  case 329: // stmts: stmts ";" stmt
#line 1205 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3258 "parser.cc"
    break;

  case 330: // stmts: stmts ";"
#line 1206 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3264 "parser.cc"
    break;

  case 331: // stmts: stmt
#line 1207 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3270 "parser.cc"
    break;

  case 332: // stmts: %empty
#line 1208 "parser.y"
                       {}
#line 3276 "parser.cc"
    break;

  case 333: // stmt: qual
#line 1213 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3282 "parser.cc"
    break;

  case 334: // stmt: "rec" stmtlist
#line 1214 "parser.y"
                        {yylhs.value.as < expression_ref > () = new expression(AST_node("Rec"),{yystack_[0].value.as < std::vector<expression_ref> > ()});}
#line 3288 "parser.cc"
    break;

  case 335: // qual: bindpat "<-" exp
#line 1216 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::PatQual(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3294 "parser.cc"
    break;

  case 336: // qual: exp
#line 1217 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::SimpleQual(yystack_[0].value.as < expression_ref > ());}
#line 3300 "parser.cc"
    break;

  case 337: // qual: "let" binds
#line 1218 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::LetQual(yystack_[0].value.as < expression_ref > ());}
#line 3306 "parser.cc"
    break;

  case 345: // qcon: gen_qcon
#line 1263 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3312 "parser.cc"
    break;

  case 346: // qcon: sysdcon
#line 1264 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3318 "parser.cc"
    break;

  case 347: // gen_qcon: qconid
#line 1266 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3324 "parser.cc"
    break;

  case 348: // gen_qcon: "(" qconsym ")"
#line 1267 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3330 "parser.cc"
    break;

  case 349: // con: conid
#line 1269 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3336 "parser.cc"
    break;

  case 350: // con: "(" consym ")"
#line 1270 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3342 "parser.cc"
    break;

  case 351: // con: sysdcon
#line 1271 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3348 "parser.cc"
    break;

  case 354: // sysdcon_no_list: "(" ")"
#line 1276 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 3354 "parser.cc"
    break;

  case 355: // sysdcon_no_list: "(" commas ")"
#line 1277 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3360 "parser.cc"
    break;

  case 356: // sysdcon_no_list: "(#" "#)"
#line 1278 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 3366 "parser.cc"
    break;

  case 357: // sysdcon_no_list: "(#" commas "#)"
#line 1279 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3372 "parser.cc"
    break;

  case 358: // sysdcon: sysdcon_no_list
#line 1281 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3378 "parser.cc"
    break;

  case 359: // sysdcon: "[" "]"
#line 1282 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 3384 "parser.cc"
    break;

  case 360: // conop: consym
#line 1284 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3390 "parser.cc"
    break;

  case 361: // conop: "`" conid "`"
#line 1285 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3396 "parser.cc"
    break;

  case 362: // qconop: qconsym
#line 1287 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3402 "parser.cc"
    break;

  case 363: // qconop: "`" qconid "`"
#line 1288 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3408 "parser.cc"
    break;

  case 364: // gtycon: ntgtycon
#line 1291 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3414 "parser.cc"
    break;

  case 365: // gtycon: "(" ")"
#line 1292 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 3420 "parser.cc"
    break;

  case 366: // gtycon: "(#" "#)"
#line 1293 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 3426 "parser.cc"
    break;

  case 367: // ntgtycon: oqtycon
#line 1295 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3432 "parser.cc"
    break;

  case 368: // ntgtycon: "(" commas ")"
#line 1296 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3438 "parser.cc"
    break;

  case 369: // ntgtycon: "(#" commas "#)"
#line 1297 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3444 "parser.cc"
    break;

  case 370: // ntgtycon: "(" "->" ")"
#line 1298 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 3450 "parser.cc"
    break;

  case 371: // ntgtycon: "[" "]"
#line 1299 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 3456 "parser.cc"
    break;

  case 372: // oqtycon: qtycon
#line 1301 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3462 "parser.cc"
    break;

  case 373: // oqtycon: "(" qtyconsym ")"
#line 1302 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3468 "parser.cc"
    break;

  case 374: // oqtycon: "(" "~" ")"
#line 1303 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3474 "parser.cc"
    break;

  case 375: // oqtycon_no_varcon: qtycon
#line 1305 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3480 "parser.cc"
    break;

  case 376: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1306 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3486 "parser.cc"
    break;

  case 377: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1307 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3492 "parser.cc"
    break;

  case 378: // oqtycon_no_varcon: "(" ":" ")"
#line 1308 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 3498 "parser.cc"
    break;

  case 379: // oqtycon_no_varcon: "(" "~" ")"
#line 1309 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3504 "parser.cc"
    break;

  case 380: // qtyconop: qtyconsym
#line 1312 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3510 "parser.cc"
    break;

  case 381: // qtyconop: "`" qtycon "`"
#line 1313 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3516 "parser.cc"
    break;

  case 382: // qtycondoc: qtycon
#line 1315 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3522 "parser.cc"
    break;

  case 383: // qtycon: "QCONID"
#line 1317 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3528 "parser.cc"
    break;

  case 384: // qtycon: tycon
#line 1318 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3534 "parser.cc"
    break;

  case 385: // tycon: "CONID"
#line 1322 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3540 "parser.cc"
    break;

  case 386: // qtyconsym: "QCONSYM"
#line 1324 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3546 "parser.cc"
    break;

  case 387: // qtyconsym: "QVARSYM"
#line 1325 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3552 "parser.cc"
    break;

  case 388: // qtyconsym: tyconsym
#line 1326 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3558 "parser.cc"
    break;

  case 389: // tyconsym: "CONSYM"
#line 1328 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3564 "parser.cc"
    break;

  case 390: // tyconsym: "VARSYM"
#line 1329 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3570 "parser.cc"
    break;

  case 391: // tyconsym: ":"
#line 1330 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 3576 "parser.cc"
    break;

  case 392: // tyconsym: "-"
#line 1331 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 3582 "parser.cc"
    break;

  case 393: // op: varop
#line 1336 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3588 "parser.cc"
    break;

  case 394: // op: conop
#line 1337 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3594 "parser.cc"
    break;

  case 395: // varop: varsym
#line 1339 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3600 "parser.cc"
    break;

  case 396: // varop: "`" varid "`"
#line 1340 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3606 "parser.cc"
    break;

  case 397: // qop: qvarop
#line 1342 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3612 "parser.cc"
    break;

  case 398: // qop: qconop
#line 1343 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3618 "parser.cc"
    break;

  case 399: // qop: hole_op
#line 1344 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3624 "parser.cc"
    break;

  case 400: // qopm: qvaropm
#line 1346 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3630 "parser.cc"
    break;

  case 401: // qopm: qconop
#line 1347 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3636 "parser.cc"
    break;

  case 402: // qopm: hole_op
#line 1348 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3642 "parser.cc"
    break;

  case 403: // hole_op: "`" "_" "`"
#line 1350 "parser.y"
                      { yylhs.value.as < std::string > () = "_"; }
#line 3648 "parser.cc"
    break;

  case 404: // qvarop: qvarsym
#line 1352 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3654 "parser.cc"
    break;

  case 405: // qvarop: "`" qvarid "`"
#line 1353 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3660 "parser.cc"
    break;

  case 406: // qvaropm: qvarsym_no_minus
#line 1355 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 3666 "parser.cc"
    break;

  case 407: // qvaropm: "`" qvarid "`"
#line 1356 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3672 "parser.cc"
    break;

  case 408: // tyvar: tyvarid
#line 1360 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3678 "parser.cc"
    break;

  case 409: // tyvarop: "`" tyvarid "`"
#line 1362 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3684 "parser.cc"
    break;

  case 410: // tyvarid: "VARID"
#line 1364 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3690 "parser.cc"
    break;

  case 411: // tyvarid: special_id
#line 1365 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3696 "parser.cc"
    break;

  case 412: // tyvarid: "unsafe"
#line 1366 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 3702 "parser.cc"
    break;

  case 413: // tyvarid: "safe"
#line 1367 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 3708 "parser.cc"
    break;

  case 414: // tyvarid: "interruptible"
#line 1368 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 3714 "parser.cc"
    break;

  case 415: // var: varid
#line 1371 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3720 "parser.cc"
    break;

  case 416: // var: "(" varsym ")"
#line 1372 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3726 "parser.cc"
    break;

  case 417: // qvar: qvarid
#line 1374 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3732 "parser.cc"
    break;

  case 418: // qvar: "(" varsym ")"
#line 1375 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3738 "parser.cc"
    break;

  case 419: // qvar: "(" qvarsym1 ")"
#line 1376 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3744 "parser.cc"
    break;

  case 420: // qvarid: varid
#line 1378 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3750 "parser.cc"
    break;

  case 421: // qvarid: "QVARID"
#line 1379 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3756 "parser.cc"
    break;

  case 422: // varid: "VARID"
#line 1381 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3762 "parser.cc"
    break;

  case 423: // varid: special_id
#line 1382 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3768 "parser.cc"
    break;

  case 424: // varid: "unsafe"
#line 1383 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 3774 "parser.cc"
    break;

  case 425: // varid: "safe"
#line 1384 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 3780 "parser.cc"
    break;

  case 426: // varid: "interruptible"
#line 1385 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 3786 "parser.cc"
    break;

  case 427: // varid: "forall"
#line 1386 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 3792 "parser.cc"
    break;

  case 428: // varid: "family"
#line 1387 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 3798 "parser.cc"
    break;

  case 429: // varid: "role"
#line 1388 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 3804 "parser.cc"
    break;

  case 430: // qvarsym: varsym
#line 1390 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3810 "parser.cc"
    break;

  case 431: // qvarsym: qvarsym1
#line 1391 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3816 "parser.cc"
    break;

  case 432: // qvarsym_no_minus: varsym_no_minus
#line 1393 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3822 "parser.cc"
    break;

  case 433: // qvarsym_no_minus: qvarsym1
#line 1394 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3828 "parser.cc"
    break;

  case 434: // qvarsym1: "QVARSYM"
#line 1396 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3834 "parser.cc"
    break;

  case 435: // varsym: varsym_no_minus
#line 1398 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3840 "parser.cc"
    break;

  case 436: // varsym: "-"
#line 1399 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 3846 "parser.cc"
    break;

  case 437: // varsym_no_minus: "VARSYM"
#line 1401 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3852 "parser.cc"
    break;

  case 438: // varsym_no_minus: special_sym
#line 1402 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3858 "parser.cc"
    break;

  case 439: // special_id: "as"
#line 1404 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 3864 "parser.cc"
    break;

  case 440: // special_id: "qualified"
#line 1405 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 3870 "parser.cc"
    break;

  case 441: // special_id: "hiding"
#line 1406 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 3876 "parser.cc"
    break;

  case 442: // special_id: "export"
#line 1407 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 3882 "parser.cc"
    break;

  case 443: // special_id: "label"
#line 1408 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 3888 "parser.cc"
    break;

  case 444: // special_id: "dynamic"
#line 1409 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 3894 "parser.cc"
    break;

  case 445: // special_id: "stdcall"
#line 1410 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 3900 "parser.cc"
    break;

  case 446: // special_id: "ccall"
#line 1411 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 3906 "parser.cc"
    break;

  case 447: // special_id: "capi"
#line 1412 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 3912 "parser.cc"
    break;

  case 448: // special_id: "prim"
#line 1413 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 3918 "parser.cc"
    break;

  case 449: // special_id: "javascript"
#line 1414 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 3924 "parser.cc"
    break;

  case 450: // special_id: "group"
#line 1415 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 3930 "parser.cc"
    break;

  case 451: // special_id: "stock"
#line 1416 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 3936 "parser.cc"
    break;

  case 452: // special_id: "anyclass"
#line 1417 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 3942 "parser.cc"
    break;

  case 453: // special_id: "via"
#line 1418 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 3948 "parser.cc"
    break;

  case 454: // special_id: "unit"
#line 1419 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 3954 "parser.cc"
    break;

  case 455: // special_id: "dependency"
#line 1420 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 3960 "parser.cc"
    break;

  case 456: // special_id: "signature"
#line 1421 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 3966 "parser.cc"
    break;

  case 457: // special_sym: "!"
#line 1423 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 3972 "parser.cc"
    break;

  case 458: // special_sym: "."
#line 1424 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 3978 "parser.cc"
    break;

  case 459: // special_sym: "*"
#line 1425 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 3984 "parser.cc"
    break;

  case 460: // qconid: conid
#line 1429 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3990 "parser.cc"
    break;

  case 461: // qconid: "QCONID"
#line 1430 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3996 "parser.cc"
    break;

  case 462: // conid: "CONID"
#line 1432 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4002 "parser.cc"
    break;

  case 463: // qconsym: consym
#line 1434 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4008 "parser.cc"
    break;

  case 464: // qconsym: "QCONSYM"
#line 1435 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4014 "parser.cc"
    break;

  case 465: // consym: "CONSYM"
#line 1437 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4020 "parser.cc"
    break;

  case 466: // consym: ":"
#line 1438 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4026 "parser.cc"
    break;

  case 467: // literal: "CHAR"
#line 1442 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < char > ();}
#line 4032 "parser.cc"
    break;

  case 468: // literal: "STRING"
#line 1443 "parser.y"
                  {yylhs.value.as < expression_ref > () = yy_make_string(yystack_[0].value.as < std::string > ());}
#line 4038 "parser.cc"
    break;

  case 469: // literal: "INTEGER"
#line 1444 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < int > ();}
#line 4044 "parser.cc"
    break;

  case 470: // literal: "RATIONAL"
#line 1445 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < double > ();}
#line 4050 "parser.cc"
    break;

  case 472: // close: error
#line 1453 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 4056 "parser.cc"
    break;

  case 473: // modid: "CONID"
#line 1457 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4062 "parser.cc"
    break;

  case 474: // modid: "QCONID"
#line 1458 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4068 "parser.cc"
    break;

  case 475: // commas: commas ","
#line 1460 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 4074 "parser.cc"
    break;

  case 476: // commas: ","
#line 1461 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 4080 "parser.cc"
    break;


#line 4084 "parser.cc"

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


  const short parser::yypact_ninf_ = -630;

  const short parser::yytable_ninf_ = -436;

  const short
  parser::yypact_[] =
  {
      75,   181,  -630,    71,  -630,  -630,  -630,  -630,  -630,   119,
     -22,   -20,  -630,    60,   -23,   -23,    -9,  -630,  -630,  -630,
    -630,   111,  -630,  -630,  -630,     4,  -630,   115,   140,  4392,
     201,   210,   129,  -630,   708,  -630,   137,  -630,  -630,  -630,
    -630,   181,  -630,    76,  -630,  -630,  -630,  -630,  -630,  -630,
    -630,  -630,  -630,  -630,  -630,  -630,  -630,  -630,  -630,   145,
    -630,  -630,  -630,  -630,  -630,  -630,   950,  -630,  -630,  -630,
    -630,   153,   178,  -630,   204,  -630,  -630,  -630,  -630,  -630,
    -630,  -630,  -630,   109,   259,   309,  -630,   255,  -630,  2583,
    4050,  -630,   252,   183,  1729,  -630,  -630,  -630,   329,   211,
    -630,  4050,  4729,   183,  3437,  4819,  3437,   292,   291,  4683,
     228,  3071,  3437,  3193,  3437,  1349,  1091,  1220,  -630,  -630,
    -630,  -630,  -630,  -630,    41,   292,   294,   129,  -630,  -630,
    -630,   355,  -630,  -630,  -630,  -630,   496,  -630,  3315,  -630,
     331,  -630,  -630,  -630,  -630,  -630,   321,   349,   323,  -630,
    -630,  -630,  -630,   312,  -630,   195,  -630,  -630,   337,   123,
     219,  -630,   334,   336,  -630,  -630,  -630,  -630,  -630,   338,
    -630,   344,   346,   350,  -630,  -630,  -630,  4392,  4439,  -630,
    -630,  -630,  -630,  -630,  -630,   434,  -630,   116,  1091,   437,
     689,  -630,  -630,  2583,  -630,  -630,  -630,  -630,  -630,  -630,
    -630,  -630,  4966,  3744,  3540,  3642,  4590,  -630,  -630,  -630,
    -630,  -630,   444,  4497,  -630,   382,  -630,   182,  4050,  -630,
    -630,  -630,  -630,  -630,  -630,  -630,  -630,  -630,  -630,  -630,
    3846,  1851,  1851,  -630,  2339,   391,   363,    57,  -630,  -630,
     400,   407,   410,   412,  3846,   838,   838,  -630,   478,   413,
     409,   203,  5012,   367,   368,  -630,  -630,  -630,   414,    -5,
     232,  4912,   418,  -630,    -7,  -630,  -630,    14,  4683,  -630,
     422,   167,   -10,   387,   423,  1973,  3437,  -630,  -630,  2949,
    -630,  3315,   172,  -630,  -630,  4152,  -630,  -630,  -630,   689,
      97,   398,   390,  -630,  2583,  -630,  -630,  -630,  -630,  -630,
    -630,  -630,  3193,  -630,  -630,   158,   161,   346,   397,   399,
     401,   188,  -630,   220,   229,   239,  3846,  4683,  4683,  -630,
     341,   255,   378,  4050,  3846,  4152,   172,  -630,  2827,  -630,
    -630,  -630,  -630,  -630,  4497,  -630,  4624,  4966,  3437,  -630,
     404,   406,   401,  -630,  -630,  -630,  -630,  -630,  -630,  -630,
    -630,   408,   403,  -630,  -630,   419,    60,  -630,   392,   441,
     445,   278,  3846,  2583,  -630,   -31,   421,   411,  -630,  -630,
    -630,  -630,   420,   438,  -630,   415,   404,  -630,    85,   416,
     406,   218,  -630,   442,   240,   431,   246,   417,   427,   211,
    -630,  -630,  4050,  3846,  -630,  -630,   425,   443,   211,   183,
    3437,   446,   463,    90,  -630,  -630,    50,   466,   448,  -630,
      89,  -630,   531,  -630,  -630,  -630,  -630,  -630,  -630,  -630,
    -630,   444,    91,  -630,  -630,   819,    51,  2583,  3846,   450,
     449,   432,   439,  2583,   458,  2461,  2461,  5012,   228,  -630,
    5012,  3846,   451,  5012,  -630,   452,   468,   507,  -630,  -630,
     508,   198,   512,  1607,   969,  -630,  -630,  2583,  -630,  2583,
    2339,  -630,    39,  -630,   475,   476,   477,  2583,  2583,  2095,
    1478,  -630,  1478,   459,  -630,  1478,  -630,  1478,   480,  -630,
    -630,  -630,  -630,  -630,  -630,   518,   493,   516,  4865,   483,
    -630,  -630,  -630,     7,   308,  -630,  -630,   267,  -630,   485,
    -630,  -630,  -630,  -630,   498,  -630,   487,   521,     2,  -630,
    -630,  -630,  -630,  4439,  -630,  -630,  -630,   181,  -630,  -630,
    -630,  -630,  -630,  3846,  4966,  -630,  4966,  5058,  -630,  3846,
    -630,  3846,  -630,  3846,  -630,  3846,  -630,  3846,  -630,  -630,
    -630,  -630,  -630,  -630,  -630,  -630,   478,  -630,  -630,  2583,
    -630,  1851,  -630,  2583,  2339,  -630,  2583,  -630,  -630,   838,
    -630,  -630,  -630,  -630,  -630,  -630,   479,   481,  -630,  -630,
    3437,  -630,  -630,   580,   517,  5012,  -630,  -630,  -630,   497,
    -630,   504,  -630,  -630,  -630,   514,  1072,   248,  -630,  -630,
    -630,  -630,  2217,   522,   510,  -630,   300,    60,  -630,  -630,
     444,   540,  -630,  -630,  -630,  -630,  -630,  -630,  2705,   511,
    -630,  -630,   546,  -630,  -630,  -630,  -630,  -630,  3846,  3846,
     341,  -630,   549,  3846,   600,  -630,   621,  -630,  -630,  4624,
    1478,  3846,   519,   627,  -630,  -630,  -630,  3846,  5139,  -630,
    -630,  -630,  -630,   523,   527,   442,  -630,  -630,  -630,  -630,
    -630,  -630,   363,  -630,  -630,  -630,  -630,   310,  -630,  -630,
    -630,  -630,  -630,  -630,  -630,  -630,  -630,  2461,  2583,  -630,
      47,  -630,  -630,  2583,   351,   589,  2095,  2583,  -630,    63,
      81,  -630,  -630,  -630,  -630,   554,  -630,  4497,    48,  -630,
     621,  -630,  -630,  -630,  -630,  -630,   181,    65,  -630,   559,
    -630,  -630,   631,   838,   838,  -630,   444,  -630,  -630,  2583,
    2583,  2583,  -630,  -630,  -630,  -630,  3846,  -630,  5092,   600,
     553,  4198,  -630,  -630,  -630,  -630,  -630,  -630,  3948,   100,
     590,  -630,  -630,  -630,  -630,   538,  4392,  -630,  -630,  3846,
    2583,   113,    51,  -630,   601,  -630,  -630,  -630,  -630,  -630,
    4497,  -630,  4497,  -630,  -630,   536,   543,  -630,  4050,  -630,
    4392,   544,   545,  -630,  -630,  -630,  2583,  4291,  -630,  4497,
    4050,  -630,  -630,   548,  -630,  -630,  -630,  -630,  -630
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    40,     0,     2,    40,     4,   473,   474,     8,
       0,    43,     1,     0,     0,     0,    18,    11,    39,    13,
      16,    68,   472,   471,    12,   127,   123,     0,     0,     0,
       0,    46,    41,    15,    14,   126,     0,     6,     7,   439,
     441,     0,   440,     0,   427,   442,   443,   444,   425,   426,
     424,   428,   429,   445,   446,   447,   448,   449,   450,     0,
     451,   452,   453,   454,   456,   455,     0,   422,   385,   421,
     383,     0,    19,    21,    25,    33,    36,   375,   384,    35,
     417,   420,   423,     0,     0,    48,    38,    42,   276,     0,
       0,    93,     0,     0,     0,    61,    62,    63,    88,     0,
      94,     0,     0,     0,     0,     0,     0,   231,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   462,   461,
     467,   468,   469,   470,   231,   231,    59,    66,    69,    70,
      71,   101,   213,   223,    73,   210,    74,   239,   243,   253,
     265,   267,   269,   345,   358,   346,     0,   268,   420,   347,
     460,   270,   124,     0,    23,     0,    34,   372,     0,     0,
       0,    24,     0,     0,   436,   457,   459,   458,   437,     0,
     434,     0,     0,     0,   435,   438,    17,     0,    27,    22,
      40,    40,     3,    45,    47,    52,    37,     0,     0,     0,
     236,   244,   237,     0,   413,   414,   412,   391,   140,   392,
     139,   161,   195,     0,     0,     0,     0,   410,   390,   389,
     387,   386,   122,     0,   138,     0,    98,   147,   150,   153,
     155,   159,   367,   156,   380,   388,   160,   157,   408,   411,
     176,   332,   332,   262,     0,     0,   247,     0,   260,   315,
       0,     0,     0,     0,     0,   117,   117,   120,     0,     0,
     147,     0,     0,     0,     0,   415,   395,   263,     0,     0,
       0,   108,     0,   351,     0,   349,   252,     0,     0,   232,
       0,     0,     0,   352,   130,     0,     0,   323,   326,     0,
     255,   241,     0,   466,   359,     0,   465,   464,   277,   236,
     282,     0,   283,   401,     0,   402,   400,   406,   433,   432,
     362,   463,   436,   354,   476,     0,     0,   433,     0,   432,
     362,     0,   356,     0,     0,     0,     0,     0,     0,    60,
       0,    67,     0,     0,     0,     0,     0,   398,     0,   399,
     397,   404,   431,   430,     0,   250,   339,     0,     0,   125,
       0,     0,     0,   378,   379,   377,   376,   419,   418,    20,
      32,     0,    28,    30,    31,     0,     0,    51,    50,     0,
       0,     0,     0,     0,   245,     0,     0,   196,   198,   135,
     182,   371,     0,     0,   143,     0,   140,   164,   177,     0,
     380,     0,   166,   177,     0,     0,     0,     0,     0,     0,
      79,   162,     0,     0,   154,   177,     0,   175,     0,     0,
       0,   336,     0,     0,   331,   333,     0,     0,   299,   301,
       0,   246,     0,   314,   317,    85,    84,    86,    87,   172,
     132,   122,     0,   214,   116,   128,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   105,
     108,     0,     0,     0,   360,     0,     0,     0,   242,   226,
       0,     0,     0,     0,     0,   258,   324,     0,   325,     0,
       0,   211,   122,   218,     0,     0,     0,   278,   284,     0,
       0,   275,     0,   279,   271,     0,   272,     0,   418,   348,
     355,   475,   273,   274,   357,     0,     0,     0,     0,   222,
     394,    65,   393,     0,    95,   129,   220,   144,   133,     0,
     212,   240,   251,   342,     0,   338,   341,   344,     0,   254,
     374,   373,    26,     0,     9,    10,    49,     0,   249,   248,
     261,   235,   238,     0,     0,   163,     0,     0,   169,     0,
     370,     0,   170,     0,   368,     0,   167,     0,   168,   369,
     381,   409,   121,    97,   148,    72,   337,   334,   322,     0,
     327,   330,   328,     0,     0,   316,     0,    83,   118,   115,
     119,   257,   144,    80,   416,   396,    78,    76,   264,   350,
       0,   319,   102,   103,     0,   108,   353,   109,   113,     0,
     106,     0,   233,   225,   227,     0,     0,     0,   131,   364,
     224,   304,     0,     0,   306,   310,     0,     0,   305,   256,
     122,     0,   216,   217,   403,   407,   363,   286,     0,   288,
     293,   294,   277,   290,   289,   281,   280,   230,     0,     0,
       0,   100,     0,     0,   192,    82,   200,   405,   266,     0,
       0,     0,     0,    54,   199,   134,   197,     0,     0,   181,
     183,   142,   185,     0,   178,   179,   180,   178,   335,   329,
     318,   300,   247,   114,    77,    75,   320,     0,   104,   107,
     110,   361,   234,   365,   366,   307,   302,   309,     0,   311,
     122,   303,   215,     0,   450,   295,     0,   285,   136,     0,
       0,    64,    99,    96,   182,   186,   188,     0,     0,    81,
     201,   203,   340,   343,   221,    29,     0,    56,   141,     0,
     171,   165,     0,   117,   117,   308,   122,   313,   219,     0,
       0,     0,   291,   292,   287,   228,     0,   229,     0,   192,
       0,   193,   151,   158,   190,    91,    89,    90,     0,     0,
     204,   207,   382,   202,    53,     0,     0,    44,    55,     0,
       0,     0,     0,   312,     0,   297,   296,   137,   191,   187,
       0,   152,     0,   208,   149,   173,     0,   205,     0,   206,
       0,     0,     0,   259,   111,   112,     0,   193,   189,   194,
       0,   209,    92,     0,    57,   184,   298,   174,    58
  };

  const short
  parser::yypgoto_[] =
  {
    -630,  -630,  -630,  -630,  -630,  -630,  -630,    44,  -630,  -630,
    -553,  -630,   482,  -630,  -630,  -630,   142,  -165,  -630,   535,
    -630,  -630,  -630,  -630,  -630,  -630,  -630,  -630,  -630,  -630,
    -630,  -630,  -630,  -630,  -630,  -630,  -630,  -630,  -630,  -630,
    -630,  -630,  -630,  -630,  -630,   340,  -630,  -630,  -630,  -395,
    -630,  -630,  -630,  -229,  -630,  -296,  -380,   649,  -630,  -630,
    -630,  -290,  -410,   328,    49,  -630,  -630,  -167,   238,   -60,
    -630,   -87,  -630,  -100,  -337,  -630,   454,  -629,  -205,   360,
     -91,  -630,   -24,   146,    -4,  -630,  -577,  -630,  -630,   -42,
    -630,   -68,  -630,  -630,   159,  -630,  -630,     0,   -38,   658,
     134,   369,  -630,   234,  -630,   293,  -630,   -62,   -93,   660,
     -30,  -299,    45,  -630,   -73,     1,  -630,  -630,   -89,   581,
    -630,  -630,  -630,    23,   241,  -630,   339,  -415,  -630,    36,
    -630,  -220,  -630,  -221,    -6,  -630,   426,  -630,   -74,   474,
     157,  -213,  -630,    88,  -630,   651,  -630,   614,   -85,  -630,
     -63,  -255,  -101,  -630,   271,   680,  -630,  -630,  -630,   -27,
    -630,  -135,  -630,   118,   633,  -131,  -630,   -79,  -630,  -630,
    -483,  -630,   550,   -59,   -29,  -207,   -15,  -630,  -630,    10,
     -54,   -61,   -83,  -630,  -197,   -19,   -51,  -253,  -630,  -204,
     -36,  -107
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   182,     6,    10,    19,    30,
      71,    72,    73,   179,   351,   352,   353,    74,    75,    87,
      11,    20,    21,    32,    85,   185,   517,   358,   697,   737,
     738,   320,   126,   489,    33,    34,   127,   128,   129,   130,
     244,   729,   759,   131,   625,   212,   323,   132,   260,   439,
     574,   658,   133,   422,   247,   248,   390,    27,    36,   326,
     452,   419,   496,   365,   679,   213,   214,   420,   498,   373,
     720,   374,   755,   217,   721,   218,   219,   722,   220,   421,
     756,   396,   379,   385,   527,   639,   643,   626,   685,   686,
     687,   724,   366,   367,   368,   689,   690,   691,   730,   423,
     424,   461,   462,   463,   135,   268,   269,   288,   190,   425,
     191,   192,   412,   193,   138,   139,   140,   141,   305,   306,
     291,   292,   609,   610,   407,   408,   455,   593,   594,   595,
     669,   237,   238,   239,   596,   402,   278,   279,   233,   403,
     404,   405,   504,   505,   506,   142,   143,   273,   262,   144,
     145,   490,   293,   588,   221,   222,    76,   223,   731,   157,
      78,   224,   225,   491,   492,   328,   294,   295,   330,   296,
     226,   227,   228,   146,   147,    80,    81,   331,   297,   298,
     333,   174,    82,   175,   149,   150,   300,   301,   151,    24,
       9,   311
  };

  const short
  parser::yytable_[] =
  {
      79,   250,    77,   216,   137,   154,   434,   229,   391,   443,
     315,   444,   173,   354,   249,   410,   413,   426,   229,   148,
     341,   409,   289,   289,   289,   274,   290,   189,   313,   257,
     215,   578,   236,   414,   495,   327,   372,   378,   383,   597,
     281,   557,   263,   254,   640,   577,   683,   263,   256,    13,
     523,    22,    22,   315,   299,   309,   299,   329,    22,   363,
     316,    22,   308,   395,   522,   310,   389,   444,   449,   380,
     725,    12,   521,   283,   389,   283,   172,   735,   465,    17,
     272,    25,   602,   631,   524,   621,   265,   255,   466,   327,
     264,   265,   751,   542,    18,   289,     1,   381,   386,    29,
     726,   727,   546,   445,   303,   258,    26,   266,   342,   442,
     304,   329,   277,   280,   286,   282,   286,   524,   499,   450,
     229,   229,   229,   229,   460,    31,   307,   309,   466,   502,
     229,   364,   234,    35,   173,   229,   622,   310,   751,   335,
     751,   715,   234,   369,   446,   267,   332,   229,    79,    79,
      77,    77,   515,    23,    23,   699,   728,   497,   467,   717,
      23,   229,   762,    23,   551,   559,   531,    68,   522,   401,
     401,    70,   401,   736,   234,     2,   468,   665,   716,   387,
     660,   384,   469,   761,   155,    14,    15,   255,   327,   413,
     555,   550,   558,    37,   532,    68,   716,   429,   307,    70,
     332,   473,   552,   283,   551,   559,   397,   773,   728,   180,
     329,   181,   470,   189,   764,   137,   137,   173,    38,    68,
     672,   694,   560,    70,   355,   356,   544,   559,    83,   281,
     148,   148,   303,   229,   359,   640,   216,   430,   304,   447,
     229,   229,   152,    86,   286,   360,   440,   409,   287,   158,
     598,   229,   153,   159,   459,   160,   611,   460,   164,   165,
     166,   562,   176,   215,   118,   167,   256,   474,   119,   393,
     476,    84,  -145,   475,   562,   197,   477,   456,   369,   229,
     277,   172,   335,   231,   340,   232,   199,   168,   486,   487,
     707,   170,   250,   177,   164,   165,   166,   480,   501,   332,
       7,   167,   585,   481,     8,   543,   586,   507,   587,   229,
     229,   245,   178,   246,   435,   208,   209,    68,   436,   210,
     211,    70,   255,   168,   327,   547,   743,   534,   678,   678,
     312,   482,   158,   481,   304,   475,   259,   183,   160,   509,
     483,   651,   363,   184,   477,   229,   329,   118,   354,  -132,
     484,   536,  -132,   576,   481,   537,   634,   539,   229,   664,
     230,   481,   641,   304,   642,   561,   644,   444,   645,   186,
     647,   568,   327,   571,   571,   263,   670,   289,   453,   289,
     454,   613,   289,   614,   289,   234,   615,   668,   616,   623,
     624,   571,   571,   671,   329,   599,   267,   600,   401,   709,
     710,   548,   240,   241,   242,   243,   607,   401,   612,   299,
     703,   299,   704,   767,   299,   769,   299,   317,   318,   265,
     270,   283,   575,   579,   319,   440,   747,   322,   580,   572,
     573,   336,   164,   165,   166,   332,   337,   338,  -415,   167,
     229,   339,   284,   343,   229,   344,   229,   345,   229,   413,
     229,   341,   229,   346,   229,   347,   642,   488,   357,   348,
     361,   168,   286,   713,   562,   635,   752,   369,   444,   579,
     698,   389,   392,   430,   741,   742,   234,   411,   415,   381,
     386,   633,   723,   332,    79,   416,    77,   648,   417,   401,
     418,   650,   401,   427,   652,   428,   393,   431,   432,   441,
     448,   433,   438,   471,   451,   472,   478,   493,  -435,   255,
     479,   255,   752,   510,   444,   511,   723,   512,   513,   518,
     514,   516,   525,   519,   530,   528,   526,   535,   529,   137,
     571,   533,  -321,   540,   545,   229,   229,   289,   765,   283,
     229,   693,   538,   541,   148,   723,   675,   723,   229,   549,
     164,   165,   166,   553,   229,   229,   556,   167,   537,   564,
     440,   566,   723,   554,   723,   565,   256,   569,   567,   299,
     118,   656,   642,   582,   618,   325,   283,   324,  -128,   168,
     286,  -128,   581,   170,   287,   583,   584,   164,   165,   166,
     590,   604,   605,   606,   167,  -416,   617,   619,   620,   628,
     507,   627,   629,   630,   229,   571,   706,   657,   654,   662,
     655,   708,   325,   661,   401,   714,   168,   286,   659,   371,
     170,   287,   673,   666,   667,   677,   676,   682,   250,   684,
     688,   696,   700,   229,   695,   229,   701,   711,   229,   719,
     739,   754,   740,   750,   758,   229,   760,   744,   745,   746,
     766,   770,   771,   774,   775,   632,   229,   778,   250,   349,
     734,   732,   321,   494,    28,   508,   563,   229,   680,   229,
     250,   772,   394,   137,   137,   229,   485,   749,   763,   777,
     718,   646,   768,   754,   229,   636,   229,   229,   148,   148,
     733,   757,   134,   653,   136,   500,   603,   702,   314,   712,
     520,   601,   732,   705,   776,   458,   406,    79,   649,    77,
     161,    88,    39,    89,    90,    91,    92,   692,    93,   261,
      40,    94,   589,   156,    95,    96,    97,    98,    99,     0,
     100,    79,    42,    77,   101,   253,   102,    44,   681,    45,
      46,    47,    48,    49,    50,   103,    51,    52,    53,    54,
      55,    56,    57,   104,     0,    58,   388,     0,   105,   106,
      60,    61,    62,    63,    64,    65,   107,     0,     0,   283,
     362,   108,   109,     0,     0,     0,     0,     0,     0,     0,
     164,   165,   166,     0,     0,   110,     0,   167,     0,     0,
       0,   111,     0,     0,     0,     0,     0,   112,     0,   113,
     114,     0,     0,     0,     0,   325,     0,     0,     0,   168,
     286,     0,   115,   170,   287,     0,   116,     0,   117,     0,
       0,     0,     0,     0,     0,     0,    67,   118,     0,     0,
      69,   119,     0,     0,     0,     0,   120,   121,   122,   123,
       0,    88,    39,    89,     0,     0,   124,   125,    93,     0,
      40,    94,     0,     0,    95,    96,    97,     0,    99,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   103,    51,    52,    53,    54,
      55,    56,    57,   104,     0,    58,     0,     0,   105,   106,
      60,    61,    62,    63,    64,    65,   107,     0,     0,   283,
     324,   108,   109,     0,     0,     0,     0,     0,     0,     0,
     164,   165,   166,     0,     0,   110,     0,   167,     0,     0,
       0,   111,     0,     0,     0,     0,     0,   112,     0,   113,
     114,     0,     0,     0,     0,   325,     0,     0,     0,   168,
     286,     0,   115,   170,   287,     0,   116,     0,   117,     0,
       0,     0,     0,     0,     0,     0,    67,   118,     0,     0,
      69,   119,     0,     0,     0,     0,   120,   121,   122,   123,
      22,     0,    88,    39,    89,     0,   124,   125,     0,    93,
       0,    40,    94,     0,     0,     0,     0,     0,     0,    99,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   103,    51,    52,    53,
      54,    55,    56,    57,   104,     0,    58,     0,     0,     0,
     106,    60,    61,    62,    63,    64,    65,     0,     0,     0,
     162,     0,   108,   187,     0,     0,     0,     0,     0,   163,
       0,   164,   165,   166,     0,     0,     0,     0,   167,     0,
       0,     0,   111,     0,     0,     0,     0,     0,   112,     0,
     113,   570,     0,     0,     0,     0,     0,     0,     0,     0,
     168,   169,    23,   115,   170,   171,     0,   188,     0,   117,
       0,     0,     0,   592,     0,     0,     0,    67,   118,     0,
       0,    69,   119,     0,    88,    39,    89,   120,   121,   122,
     123,    93,     0,    40,    94,     0,     0,     0,     0,     0,
       0,    99,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   103,    51,
      52,    53,    54,    55,    56,    57,   104,     0,    58,     0,
       0,     0,   106,    60,    61,    62,    63,    64,    65,     0,
       0,     0,   197,     0,   108,   187,     0,     0,     0,   375,
       0,   340,     0,   199,     0,     0,     0,     0,     0,     0,
       0,   283,     0,     0,   111,     0,     0,     0,     0,     0,
     112,   663,   302,   165,   166,     0,     0,   304,     0,   167,
       0,     0,   208,   209,     0,   115,   210,   211,     0,   188,
     303,   117,     0,     0,     0,     0,   304,   285,     0,    67,
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
       0,     0,     0,     0,   115,     0,     0,     0,   188,     0,
     117,   312,     0,     0,     0,   304,   285,     0,    67,   118,
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
       0,     0,     0,   115,   284,     0,     0,   188,     0,   117,
       0,     0,     0,     0,     0,   285,     0,    67,   118,   168,
     286,    69,   119,   170,   287,     0,     0,   120,   121,   122,
     123,    88,    39,    89,     0,     0,     0,     0,    93,     0,
      40,    94,     0,     0,     0,     0,     0,     0,    99,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   103,    51,    52,    53,    54,
      55,    56,    57,   104,     0,    58,     0,     0,     0,   106,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,   108,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   283,     0,
       0,   111,     0,     0,     0,     0,     0,   112,     0,   113,
     165,   166,     0,     0,     0,     0,   167,     0,     0,     0,
       0,     0,   115,     0,     0,     0,   188,     0,   117,     0,
       0,     0,     0,     0,   285,     0,    67,   118,   168,   286,
      69,   119,   170,   287,     0,     0,   120,   121,   122,   123,
      88,    39,    89,     0,     0,     0,     0,    93,     0,    40,
      94,     0,     0,     0,     0,     0,     0,    99,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   103,    51,    52,    53,    54,    55,
      56,    57,   104,     0,    58,     0,     0,     0,   106,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,     0,
     108,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     111,     0,     0,     0,     0,     0,   112,     0,   113,   570,
       0,     0,     0,     0,     0,     0,     0,     0,   591,     0,
       0,   115,     0,     0,     0,   188,     0,   117,     0,     0,
       0,   592,     0,     0,     0,    67,   118,     0,     0,    69,
     119,     0,    88,    39,    89,   120,   121,   122,   123,    93,
       0,    40,    94,     0,     0,     0,     0,     0,     0,    99,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   103,    51,    52,    53,
      54,    55,    56,    57,   104,     0,    58,     0,     0,     0,
     106,    60,    61,    62,    63,    64,    65,     0,     0,     0,
       0,     0,   108,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   111,     0,   234,     0,     0,     0,   112,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,   235,
       0,     0,     0,   115,     0,     0,     0,   188,     0,   117,
       0,     0,     0,     0,     0,     0,     0,    67,   118,     0,
       0,    69,   119,     0,    88,    39,    89,   120,   121,   122,
     123,    93,     0,    40,    94,     0,     0,     0,     0,     0,
       0,   398,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   103,    51,
      52,    53,    54,    55,    56,    57,   104,   399,    58,     0,
       0,     0,   106,    60,    61,    62,    63,    64,    65,     0,
       0,     0,     0,     0,   108,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   111,     0,     0,     0,     0,     0,
     112,     0,   113,   400,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   115,     0,     0,     0,   188,
       0,   117,     0,     0,     0,     0,     0,     0,     0,    67,
     118,     0,     0,    69,   119,     0,    88,    39,    89,   120,
     121,   122,   123,    93,     0,    40,    94,     0,     0,     0,
       0,     0,     0,    99,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     103,    51,    52,    53,    54,    55,    56,    57,   104,     0,
      58,     0,     0,     0,   106,    60,    61,    62,    63,    64,
      65,     0,     0,     0,     0,     0,   108,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,     0,     0,     0,
       0,     0,   112,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,   453,     0,   454,     0,   115,     0,     0,
       0,   188,     0,   117,     0,     0,     0,     0,     0,     0,
       0,    67,   118,     0,     0,    69,   119,     0,    88,    39,
      89,   120,   121,   122,   123,    93,     0,    40,    94,     0,
       0,     0,     0,     0,     0,   398,     0,     0,     0,    42,
     608,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   103,    51,    52,    53,    54,    55,    56,    57,
     104,     0,    58,     0,     0,     0,   106,    60,    61,    62,
      63,    64,    65,     0,     0,     0,     0,     0,   108,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   111,     0,
       0,     0,     0,     0,   112,     0,   113,   400,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   115,
       0,     0,     0,   188,     0,   117,     0,     0,     0,     0,
       0,     0,     0,    67,   118,     0,     0,    69,   119,     0,
      88,    39,    89,   120,   121,   122,   123,    93,     0,    40,
      94,     0,     0,     0,     0,     0,     0,    99,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   103,    51,    52,    53,    54,    55,
      56,    57,   104,     0,    58,     0,     0,     0,   106,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,     0,
     108,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     111,     0,     0,     0,     0,     0,   112,     0,   113,   570,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   115,     0,     0,     0,   188,     0,   117,     0,     0,
       0,   592,     0,     0,     0,    67,   118,     0,     0,    69,
     119,     0,    88,    39,    89,   120,   121,   122,   123,    93,
       0,    40,    94,     0,     0,     0,     0,     0,     0,   398,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   103,    51,    52,    53,
      54,    55,    56,    57,   104,     0,    58,     0,     0,     0,
     106,    60,    61,    62,    63,    64,    65,     0,     0,     0,
       0,     0,   108,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   111,     0,     0,     0,     0,     0,   112,     0,
     113,   400,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   115,     0,     0,     0,   188,     0,   117,
       0,     0,     0,     0,     0,     0,     0,    67,   118,     0,
       0,    69,   119,     0,    88,    39,    89,   120,   121,   122,
     123,    93,     0,    40,    94,     0,     0,     0,     0,     0,
       0,    99,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   103,    51,
      52,    53,    54,    55,    56,    57,   104,     0,    58,     0,
       0,     0,   106,    60,    61,    62,    63,    64,    65,     0,
       0,     0,     0,     0,   108,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   111,     0,     0,     0,     0,     0,
     112,     0,   113,   570,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   115,     0,     0,     0,   188,
       0,   117,     0,     0,     0,     0,     0,     0,     0,    67,
     118,     0,     0,    69,   119,     0,    88,    39,    89,   120,
     121,   122,   123,    93,     0,    40,    94,     0,     0,     0,
       0,     0,     0,    99,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     103,    51,    52,    53,    54,    55,    56,    57,   104,     0,
      58,     0,     0,     0,   106,    60,    61,    62,    63,    64,
      65,     0,     0,     0,     0,     0,   108,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,     0,     0,     0,
       0,     0,   112,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   115,     0,     0,
       0,   188,     0,   117,     0,     0,     0,     0,     0,     0,
       0,    67,   118,     0,     0,    69,   119,     0,    88,    39,
      89,   120,   121,   122,   123,    93,     0,    40,    94,     0,
       0,     0,     0,     0,     0,    99,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   103,    51,    52,    53,    54,    55,    56,    57,
     104,     0,   674,     0,     0,     0,   106,    60,    61,    62,
      63,    64,    65,     0,     0,     0,     0,     0,   108,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   111,     0,
       0,     0,     0,     0,   112,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   115,
       0,     0,     0,   188,     0,   117,     0,     0,     0,     0,
       0,     0,     0,    67,   118,     0,     0,    69,   119,     0,
      88,    39,    89,   120,   121,   122,   123,    93,     0,    40,
      94,     0,     0,     0,     0,     0,     0,    99,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   103,    51,    52,    53,    54,    55,
      56,    57,   104,     0,    58,     0,     0,     0,   106,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,     0,
     108,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     111,     0,     0,     0,     0,     0,   112,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   115,     0,     0,     0,   188,     0,   117,     0,     0,
       0,     0,     0,     0,     0,    67,   118,     0,     0,    69,
     119,     0,    88,    39,    89,   120,   121,   122,   123,    93,
       0,    40,    94,     0,     0,     0,     0,     0,     0,    99,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   103,    51,    52,    53,
      54,    55,    56,    57,   104,     0,    58,     0,     0,     0,
       0,    60,    61,    62,    63,    64,    65,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   111,     0,     0,     0,   457,     0,   112,     0,
       0,   276,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   115,     0,     0,     0,   188,     0,   117,
       0,     0,     0,     0,     0,     0,     0,    67,   118,     0,
       0,    69,   119,     0,    88,    39,   275,   120,   121,   122,
     123,    93,     0,    40,    94,     0,     0,     0,     0,     0,
       0,    99,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   103,    51,
      52,    53,    54,    55,    56,    57,   104,     0,    58,     0,
       0,     0,     0,    60,    61,    62,    63,    64,    65,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   111,     0,     0,     0,     0,     0,
     112,     0,     0,   276,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   115,     0,     0,     0,   188,
       0,   117,     0,     0,     0,     0,     0,     0,     0,    67,
     118,     0,     0,    69,   119,     0,    88,    39,    89,   120,
     121,   122,   123,    93,     0,    40,    94,     0,     0,     0,
       0,     0,     0,    99,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     103,    51,    52,    53,    54,    55,    56,    57,   104,     0,
      58,     0,     0,     0,   106,    60,    61,    62,    63,    64,
      65,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,     0,     0,     0,
       0,     0,   112,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   115,     0,     0,
       0,   188,     0,   117,     0,     0,     0,     0,     0,     0,
       0,    67,   118,     0,     0,    69,   119,     0,    88,    39,
      89,   120,   121,   122,   123,    93,     0,    40,    94,     0,
       0,     0,     0,     0,     0,    99,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   103,    51,    52,    53,    54,    55,    56,    57,
     104,     0,    58,     0,     0,     0,     0,    60,    61,    62,
      63,    64,    65,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   111,     0,
       0,     0,     0,     0,   112,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   334,     0,     0,     0,     0,   115,
       0,     0,     0,   188,     0,   117,     0,     0,     0,     0,
       0,     0,     0,    67,   118,     0,     0,    69,   119,     0,
      88,    39,    89,   120,   121,   122,   123,    93,     0,    40,
      94,     0,     0,     0,     0,     0,     0,    99,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   103,    51,    52,    53,    54,    55,
      56,    57,   104,     0,    58,     0,     0,     0,     0,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     111,     0,     0,     0,     0,     0,   112,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   115,     0,     0,    39,   188,     0,   117,     0,     0,
       0,     0,    40,     0,     0,    67,   118,     0,     0,    69,
     119,     0,     0,     0,    42,   120,   121,   122,   123,   370,
       0,    45,    46,    47,   194,   195,   196,     0,     0,     0,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,     0,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     197,     0,     0,     0,     0,     0,     0,   375,     0,   376,
       0,   199,   200,   201,     0,     0,     0,     0,     0,     0,
     202,     0,     0,     0,   203,     0,    39,     0,   204,   377,
     205,     0,     0,     0,    40,   304,   206,     0,   207,    68,
     208,   209,     0,    70,   210,   211,    42,     0,     0,     0,
       0,   370,     0,    45,    46,    47,   194,   195,   196,     0,
       0,     0,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,     0,     0,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   197,     0,     0,     0,     0,     0,     0,     0,
       0,   198,     0,   199,   200,   201,     0,     0,     0,     0,
       0,     0,   202,     0,     0,     0,   203,     0,    39,     0,
     204,     0,   205,   382,     0,     0,    40,   304,   206,     0,
     207,    68,   208,   209,     0,    70,   210,   211,    42,     0,
       0,     0,     0,   370,     0,    45,    46,    47,   194,   195,
     196,     0,     0,     0,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,     0,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   197,     0,     0,     0,     0,     0,
       0,     0,     0,   198,     0,   199,   200,   201,     0,     0,
       0,     0,     0,     0,   202,     0,     0,     0,   203,   371,
      39,     0,   204,     0,   205,     0,     0,     0,    40,     0,
     206,     0,   207,    68,   208,   209,     0,    70,   210,   211,
      42,     0,     0,     0,     0,   370,     0,    45,    46,    47,
     194,   195,   196,     0,     0,     0,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,     0,     0,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   197,     0,     0,     0,
       0,     0,     0,     0,     0,   198,     0,   199,   200,   201,
       0,     0,     0,     0,     0,     0,   202,     0,     0,     0,
     203,     0,    39,     0,   204,     0,   205,     0,     0,     0,
      40,     0,   206,     0,   207,    68,   208,   209,     0,    70,
     210,   211,    42,     0,     0,     0,     0,     0,     0,    45,
      46,    47,   194,   195,   196,     0,     0,     0,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,     0,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   197,     0,
       0,     0,     0,     0,     0,     0,     0,   198,     0,   199,
     200,   201,     0,     0,     0,     0,     0,     0,   202,     0,
       0,     0,   203,     0,    39,     0,   204,   753,   205,     0,
       0,     0,    40,     0,   206,     0,   207,    68,   208,   209,
       0,    70,   210,   211,    42,     0,     0,     0,     0,     0,
       0,    45,    46,    47,   194,   195,   196,     0,     0,     0,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,     0,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     197,     0,     0,     0,     0,     0,     0,     0,     0,   198,
       0,   199,   200,   201,     0,     0,     0,     0,     0,     0,
     202,     0,     0,     0,   203,   464,    39,     0,   204,     0,
     205,     0,     0,     0,    40,     0,   206,     0,   207,    68,
     208,   209,     0,    70,   210,   211,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,    39,     0,    60,    61,    62,    63,    64,    65,
      40,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    42,     0,     0,     0,     0,     0,     0,    45,
      46,    47,   194,   195,   196,     0,     0,     0,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,     0,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      67,   118,     0,     0,    69,   119,     0,     0,   283,     0,
       0,     0,     0,     0,     0,     0,     0,   198,  -146,     0,
     200,   201,     0,     0,     0,    39,     0,     0,   202,     0,
       0,     0,   203,    40,     0,     0,   204,     0,   205,     0,
       0,     0,     0,     0,   442,    42,   207,    68,     0,   286,
       0,    70,    45,    46,    47,   194,   195,   196,     0,     0,
       0,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,     0,    60,    61,    62,    63,    64,    65,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   283,     0,     0,     0,     0,     0,     0,     0,     0,
     198,     0,     0,   200,   201,     0,     0,     0,     0,     0,
       0,   202,     0,     0,     0,   203,    39,     0,     0,   204,
       0,   205,     0,     0,    40,     0,     0,   442,     0,   207,
      68,     0,   286,    41,    70,     0,    42,     0,    43,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,    59,    39,    60,    61,    62,    63,    64,    65,
       0,    40,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    42,     0,    43,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,     0,
       0,    60,    61,    62,    63,    64,    65,     0,     0,     0,
      66,    39,     0,     0,     0,     0,     0,     0,     0,    40,
      67,    68,     0,     0,    69,    70,     0,     0,   350,     0,
       0,    42,     0,     0,     0,     0,     0,     0,    45,    46,
      47,   194,   195,   196,     0,     0,     0,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,    66,     0,    60,
      61,    62,    63,    64,    65,     0,     0,    67,    68,     0,
       0,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   198,     0,     0,   200,
     201,     0,     0,     0,    39,     0,     0,   202,     0,     0,
       0,   203,    40,     0,     0,   204,     0,   205,     0,     0,
       0,     0,     0,     0,    42,   207,    68,     0,     0,     0,
      70,    45,    46,    47,   194,   195,   196,     0,    39,     0,
      53,    54,    55,    56,    57,     0,    40,    58,     0,     0,
       0,     0,    60,    61,    62,    63,    64,    65,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,     0,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,    39,     0,     0,
       0,     0,     0,     0,     0,    40,     0,     0,     0,     0,
       0,     0,     0,   503,     0,     0,     0,    42,   207,    68,
       0,     0,    44,    70,    45,    46,    47,    48,    49,    50,
       0,    51,    52,    53,    54,    55,    56,    57,     0,     0,
      58,     0,   271,    39,     0,    60,    61,    62,    63,    64,
      65,    40,    67,     0,     0,     0,    69,     0,     0,     0,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,     0,
       0,    60,    61,    62,    63,    64,    65,     0,     0,     0,
       0,   271,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    67,     0,     0,     0,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     164,   165,   166,    39,     0,     0,     0,   167,     0,     0,
       0,    40,     0,     0,     0,     0,     0,   251,     0,     0,
       0,     0,     0,    42,     0,   252,     0,    67,    44,   168,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,    39,
       0,    60,    61,    62,    63,    64,    65,    40,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,     0,    51,    52,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    39,    60,    61,    62,
      63,    64,    65,   158,    40,     0,     0,   259,     0,   160,
       0,     0,     0,     0,     0,     0,    42,    67,   118,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,     0,     0,    60,    61,    62,    63,    64,    65,
      39,     0,     0,     0,     0,     0,     0,     0,    40,     0,
       0,     0,     0,    67,   118,     0,     0,     0,     0,     0,
      42,     0,     0,  -352,     0,    44,     0,    45,    46,    47,
      48,    49,    50,     0,    51,    52,    53,    54,    55,    56,
      57,     0,   437,    58,     0,     0,    39,     0,    60,    61,
      62,    63,    64,    65,    40,     0,     0,   438,     0,     0,
      67,     0,     0,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,    39,     0,    60,    61,    62,    63,    64,    65,
      40,     0,     0,     0,   251,     0,     0,     0,     0,     0,
       0,     0,    42,     0,    67,     0,     0,     0,     0,    45,
      46,    47,   194,   195,   196,     0,    39,     0,    53,    54,
      55,    56,    57,     0,    40,    58,     0,     0,     0,     0,
      60,    61,    62,    63,    64,    65,    42,     0,     0,     0,
       0,     0,     0,    45,    46,    47,   194,   195,   196,     0,
      67,     0,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    39,    60,    61,    62,    63,    64,    65,
       0,    40,     0,     0,     0,     0,   637,     0,     0,     0,
       0,     0,     0,    42,     0,     0,   638,     0,     0,     0,
      45,    46,    47,   194,   195,   196,   207,     0,     0,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,     0,
     748,    60,    61,    62,    63,    64,    65,     0,     0,     0,
     638,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     207,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   207
  };

  const short
  parser::yycheck_[] =
  {
      29,   101,    29,    90,    34,    41,   259,    90,   213,   264,
     117,   264,    66,   178,   101,   235,   237,   246,   101,    34,
     155,   234,   115,   116,   117,   110,   115,    89,   117,   103,
      90,   441,    94,   237,   324,   136,   203,   204,   205,   454,
     113,   421,   105,   102,   527,   440,   623,   110,   102,     5,
      81,     1,     1,   160,   115,   116,   117,   136,     1,   190,
      19,     1,   116,   230,   363,   116,    27,   320,    78,   204,
      22,     0,   362,    80,    27,    80,    66,    12,   285,   101,
     109,   104,   462,    81,   115,    78,   105,   102,   285,   190,
     105,   110,   721,   389,   114,   188,    21,   204,   205,   108,
      52,    53,   398,    89,   109,   104,   129,   106,   159,   116,
     115,   190,   111,   112,   121,   114,   121,   115,   325,   129,
     203,   204,   205,   206,    85,    14,   116,   188,   325,   334,
     213,   193,    85,   129,   188,   218,   129,   188,   767,   138,
     769,    78,    85,   202,   130,   104,   136,   230,   177,   178,
     177,   178,   356,   103,   103,   638,   108,   324,   289,    78,
     103,   244,   739,   103,   114,   114,    81,   119,   467,   231,
     232,   123,   234,   108,    85,   100,    79,   592,   115,   206,
     575,   205,    85,   736,   108,    66,    67,   202,   289,   410,
     101,   101,   101,    78,   109,   119,   115,   251,   188,   123,
     190,   294,   406,    80,   114,   114,   230,   760,   108,   100,
     289,   102,   115,   275,   101,   245,   246,   271,    78,   119,
     600,   631,   426,   123,   180,   181,   393,   114,    27,   302,
     245,   246,   109,   316,   118,   718,   323,   252,   115,   268,
     323,   324,   105,   114,   121,   129,   261,   460,   125,   104,
     454,   334,   115,   108,    82,   110,   469,    85,    91,    92,
      93,   428,   109,   323,   119,    98,   320,   109,   123,    87,
     109,    61,    90,   115,   441,    80,   115,   276,   337,   362,
     279,   271,   281,   100,    89,   102,    91,   120,   317,   318,
     670,   124,   392,   115,    91,    92,    93,   109,   328,   289,
     119,    98,   104,   115,   123,   392,   108,   336,   110,   392,
     393,   100,   108,   102,    82,   120,   121,   119,    86,   124,
     125,   123,   337,   120,   425,   399,   706,   109,   618,   619,
     111,   111,   104,   115,   115,   115,   108,    78,   110,   338,
     111,   554,   473,    34,   115,   428,   425,   119,   513,    82,
     111,   111,    85,   438,   115,   115,   523,   111,   441,   111,
     108,   115,   529,   115,   531,   427,   533,   620,   535,   114,
     537,   433,   473,   435,   436,   438,   596,   470,   100,   472,
     102,   470,   475,   472,   477,    85,   475,    87,   477,    81,
      82,   453,   454,   597,   473,   457,   104,   459,   460,    48,
      49,   400,    73,    74,    75,    76,   468,   469,   470,   470,
     100,   472,   102,   750,   475,   752,   477,   124,   125,   438,
     129,    80,   437,   442,   130,   440,   716,    72,   443,   435,
     436,   100,    91,    92,    93,   425,   115,    88,   115,    98,
     523,   129,   105,   109,   527,   109,   529,   109,   531,   670,
     533,   586,   535,   109,   537,   109,   623,   116,    24,   109,
      23,   120,   121,   676,   631,   524,   721,   526,   721,   488,
     637,    27,    90,   488,   703,   704,    85,   114,    78,   586,
     587,   517,   687,   473,   513,    78,   513,   549,    78,   551,
      78,   553,   554,    15,   556,    82,    87,   130,   130,    81,
      78,    87,   115,   105,    81,   115,   109,   129,   109,   524,
     109,   526,   767,   109,   767,   109,   721,   109,   115,    78,
     101,   129,   101,    78,   109,   105,   115,    85,    90,   559,
     592,   115,    86,   116,   109,   618,   619,   630,   742,    80,
     623,   630,   111,   116,   559,   750,   608,   752,   631,    86,
      91,    92,    93,    87,   637,   638,    25,    98,   115,   109,
     575,   129,   767,   115,   769,   116,   620,   109,   129,   630,
     119,   570,   739,   105,    81,   116,    80,    81,    82,   120,
     121,    85,   130,   124,   125,    78,    78,    91,    92,    93,
      78,   116,   116,   116,    98,   115,    78,    81,   115,   101,
     629,   116,   115,    82,   687,   667,   668,    27,   129,   105,
     129,   673,   116,   116,   676,   677,   120,   121,   101,   105,
     124,   125,    82,   101,   114,    79,   115,    78,   728,    29,
       9,     4,   109,   716,   115,   718,   109,    48,   721,    85,
      81,   728,    11,    90,    54,   728,   108,   709,   710,   711,
      49,   115,   109,   109,   109,   513,   739,   109,   758,   177,
     696,   688,   127,   323,    15,   337,   428,   750,   619,   752,
     770,   758,   218,   703,   704,   758,   316,   719,   740,   770,
     684,   535,   750,   770,   767,   526,   769,   770,   703,   704,
     690,   729,    34,   559,    34,   326,   462,   652,   117,   676,
     361,   460,   729,   667,   766,   279,   232,   736,   551,   736,
      59,     3,     4,     5,     6,     7,     8,   629,    10,   105,
      12,    13,   451,    43,    16,    17,    18,    19,    20,    -1,
      22,   760,    24,   760,    26,   102,    28,    29,   620,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    47,   206,    -1,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    -1,    -1,    80,
      81,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      91,    92,    93,    -1,    -1,    77,    -1,    98,    -1,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    89,    -1,    91,
      92,    -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,   120,
     121,    -1,   104,   124,   125,    -1,   108,    -1,   110,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   118,   119,    -1,    -1,
     122,   123,    -1,    -1,    -1,    -1,   128,   129,   130,   131,
      -1,     3,     4,     5,    -1,    -1,   138,   139,    10,    -1,
      12,    13,    -1,    -1,    16,    17,    18,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    -1,    -1,    80,
      81,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      91,    92,    93,    -1,    -1,    77,    -1,    98,    -1,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    89,    -1,    91,
      92,    -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,   120,
     121,    -1,   104,   124,   125,    -1,   108,    -1,   110,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   118,   119,    -1,    -1,
     122,   123,    -1,    -1,    -1,    -1,   128,   129,   130,   131,
       1,    -1,     3,     4,     5,    -1,   138,   139,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      80,    -1,    63,    64,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    91,    92,    93,    -1,    -1,    -1,    -1,    98,    -1,
      -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     120,   121,   103,   104,   124,   125,    -1,   108,    -1,   110,
      -1,    -1,    -1,   114,    -1,    -1,    -1,   118,   119,    -1,
      -1,   122,   123,    -1,     3,     4,     5,   128,   129,   130,
     131,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    80,    -1,    63,    64,    -1,    -1,    -1,    87,
      -1,    89,    -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,
      89,   109,    91,    92,    93,    -1,    -1,   115,    -1,    98,
      -1,    -1,   120,   121,    -1,   104,   124,   125,    -1,   108,
     109,   110,    -1,    -1,    -1,    -1,   115,   116,    -1,   118,
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
      -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,    -1,
     110,   111,    -1,    -1,    -1,   115,   116,    -1,   118,   119,
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
      -1,    -1,    -1,   104,   105,    -1,    -1,   108,    -1,   110,
      -1,    -1,    -1,    -1,    -1,   116,    -1,   118,   119,   120,
     121,   122,   123,   124,   125,    -1,    -1,   128,   129,   130,
     131,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      -1,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    89,    -1,    91,
      92,    93,    -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,    -1,   108,    -1,   110,    -1,
      -1,    -1,    -1,    -1,   116,    -1,   118,   119,   120,   121,
     122,   123,   124,   125,    -1,    -1,   128,   129,   130,   131,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      83,    -1,    -1,    -1,    -1,    -1,    89,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,   104,    -1,    -1,    -1,   108,    -1,   110,    -1,    -1,
      -1,   114,    -1,    -1,    -1,   118,   119,    -1,    -1,   122,
     123,    -1,     3,     4,     5,   128,   129,   130,   131,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,    -1,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    83,    -1,    85,    -1,    -1,    -1,    89,    -1,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
      -1,    -1,    -1,   104,    -1,    -1,    -1,   108,    -1,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,   119,    -1,
      -1,   122,   123,    -1,     3,     4,     5,   128,   129,   130,
     131,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    -1,    -1,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,
      -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,
     119,    -1,    -1,   122,   123,    -1,     3,     4,     5,   128,
     129,   130,   131,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      57,    -1,    -1,    -1,    -1,    -1,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,    -1,   102,    -1,   104,    -1,    -1,
      -1,   108,    -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   118,   119,    -1,    -1,   122,   123,    -1,     3,     4,
       5,   128,   129,   130,   131,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      25,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,    -1,    -1,   108,    -1,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   118,   119,    -1,    -1,   122,   123,    -1,
       3,     4,     5,   128,   129,   130,   131,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      83,    -1,    -1,    -1,    -1,    -1,    89,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,    -1,    -1,    -1,   108,    -1,   110,    -1,    -1,
      -1,   114,    -1,    -1,    -1,   118,   119,    -1,    -1,   122,
     123,    -1,     3,     4,     5,   128,   129,   130,   131,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,    -1,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,    -1,    -1,   108,    -1,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,   119,    -1,
      -1,   122,   123,    -1,     3,     4,     5,   128,   129,   130,
     131,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    -1,    -1,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,
      -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,
     119,    -1,    -1,   122,   123,    -1,     3,     4,     5,   128,
     129,   130,   131,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      57,    -1,    -1,    -1,    -1,    -1,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,
      -1,   108,    -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   118,   119,    -1,    -1,   122,   123,    -1,     3,     4,
       5,   128,   129,   130,   131,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,    -1,    -1,   108,    -1,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   118,   119,    -1,    -1,   122,   123,    -1,
       3,     4,     5,   128,   129,   130,   131,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      83,    -1,    -1,    -1,    -1,    -1,    89,    -1,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,    -1,    -1,    -1,   108,    -1,   110,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   118,   119,    -1,    -1,   122,
     123,    -1,     3,     4,     5,   128,   129,   130,   131,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    47,    -1,    -1,    -1,
      -1,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    83,    -1,    -1,    -1,    87,    -1,    89,    -1,
      -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,    -1,    -1,   108,    -1,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,   119,    -1,
      -1,   122,   123,    -1,     3,     4,     5,   128,   129,   130,
     131,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    47,    -1,
      -1,    -1,    -1,    52,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,
      -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,
     119,    -1,    -1,   122,   123,    -1,     3,     4,     5,   128,
     129,   130,   131,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,
      -1,   108,    -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   118,   119,    -1,    -1,   122,   123,    -1,     3,     4,
       5,   128,   129,   130,   131,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    47,    -1,    -1,    -1,    -1,    52,    53,    54,
      55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,   104,
      -1,    -1,    -1,   108,    -1,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   118,   119,    -1,    -1,   122,   123,    -1,
       3,     4,     5,   128,   129,   130,   131,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    47,    -1,    -1,    -1,    -1,    52,
      53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      83,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,    -1,    -1,     4,   108,    -1,   110,    -1,    -1,
      -1,    -1,    12,    -1,    -1,   118,   119,    -1,    -1,   122,
     123,    -1,    -1,    -1,    24,   128,   129,   130,   131,    29,
      -1,    31,    32,    33,    34,    35,    36,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    -1,    52,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    89,
      -1,    91,    92,    93,    -1,    -1,    -1,    -1,    -1,    -1,
     100,    -1,    -1,    -1,   104,    -1,     4,    -1,   108,   109,
     110,    -1,    -1,    -1,    12,   115,   116,    -1,   118,   119,
     120,   121,    -1,   123,   124,   125,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    -1,
      -1,    -1,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    -1,    52,    53,    54,    55,    56,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    91,    92,    93,    -1,    -1,    -1,    -1,
      -1,    -1,   100,    -1,    -1,    -1,   104,    -1,     4,    -1,
     108,    -1,   110,   111,    -1,    -1,    12,   115,   116,    -1,
     118,   119,   120,   121,    -1,   123,   124,   125,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    -1,    -1,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    -1,    52,    53,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    91,    92,    93,    -1,    -1,
      -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,   104,   105,
       4,    -1,   108,    -1,   110,    -1,    -1,    -1,    12,    -1,
     116,    -1,   118,   119,   120,   121,    -1,   123,   124,   125,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    -1,    -1,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    -1,    52,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    91,    92,    93,
      -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,
     104,    -1,     4,    -1,   108,    -1,   110,    -1,    -1,    -1,
      12,    -1,   116,    -1,   118,   119,   120,   121,    -1,   123,
     124,   125,    24,    -1,    -1,    -1,    -1,    -1,    -1,    31,
      32,    33,    34,    35,    36,    -1,    -1,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    -1,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    91,
      92,    93,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,
      -1,    -1,   104,    -1,     4,    -1,   108,   109,   110,    -1,
      -1,    -1,    12,    -1,   116,    -1,   118,   119,   120,   121,
      -1,   123,   124,   125,    24,    -1,    -1,    -1,    -1,    -1,
      -1,    31,    32,    33,    34,    35,    36,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    -1,    52,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    91,    92,    93,    -1,    -1,    -1,    -1,    -1,    -1,
     100,    -1,    -1,    -1,   104,     3,     4,    -1,   108,    -1,
     110,    -1,    -1,    -1,    12,    -1,   116,    -1,   118,   119,
     120,   121,    -1,   123,   124,   125,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    -1,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,     4,    -1,    52,    53,    54,    55,    56,    57,
      12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    -1,    31,
      32,    33,    34,    35,    36,    -1,    -1,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    -1,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     118,   119,    -1,    -1,   122,   123,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    90,    -1,
      92,    93,    -1,    -1,    -1,     4,    -1,    -1,   100,    -1,
      -1,    -1,   104,    12,    -1,    -1,   108,    -1,   110,    -1,
      -1,    -1,    -1,    -1,   116,    24,   118,   119,    -1,   121,
      -1,   123,    31,    32,    33,    34,    35,    36,    -1,    -1,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    -1,    52,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    -1,    -1,    -1,    -1,
      -1,   100,    -1,    -1,    -1,   104,     4,    -1,    -1,   108,
      -1,   110,    -1,    -1,    12,    -1,    -1,   116,    -1,   118,
     119,    -1,   121,    21,   123,    -1,    24,    -1,    26,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    -1,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,     4,    52,    53,    54,    55,    56,    57,
      -1,    12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    24,    -1,    26,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    -1,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      -1,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
     108,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
     118,   119,    -1,    -1,   122,   123,    -1,    -1,    79,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    -1,    -1,    31,    32,
      33,    34,    35,    36,    -1,    -1,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,   108,    -1,    52,
      53,    54,    55,    56,    57,    -1,    -1,   118,   119,    -1,
      -1,   122,   123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    -1,    -1,     4,    -1,    -1,   100,    -1,    -1,
      -1,   104,    12,    -1,    -1,   108,    -1,   110,    -1,    -1,
      -1,    -1,    -1,    -1,    24,   118,   119,    -1,    -1,    -1,
     123,    31,    32,    33,    34,    35,    36,    -1,     4,    -1,
      40,    41,    42,    43,    44,    -1,    12,    47,    -1,    -1,
      -1,    -1,    52,    53,    54,    55,    56,    57,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    -1,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    -1,    52,    53,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    24,   118,   119,
      -1,    -1,    29,   123,    31,    32,    33,    34,    35,    36,
      -1,    38,    39,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,   108,     4,    -1,    52,    53,    54,    55,    56,
      57,    12,   118,    -1,    -1,    -1,   122,    -1,    -1,    -1,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    -1,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      -1,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   118,    -1,    -1,    -1,   122,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      91,    92,    93,     4,    -1,    -1,    -1,    98,    -1,    -1,
      -1,    12,    -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,
      -1,    -1,    -1,    24,    -1,   116,    -1,   118,    29,   120,
      31,    32,    33,    34,    35,    36,    -1,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,     4,
      -1,    52,    53,    54,    55,    56,    57,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    -1,    38,    39,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,     4,    52,    53,    54,
      55,    56,    57,   104,    12,    -1,    -1,   108,    -1,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    24,   118,   119,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    -1,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    -1,    52,    53,    54,    55,    56,    57,
       4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,
      -1,    -1,    -1,   118,   119,    -1,    -1,    -1,    -1,    -1,
      24,    -1,    -1,    81,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    -1,    38,    39,    40,    41,    42,    43,
      44,    -1,   100,    47,    -1,    -1,     4,    -1,    52,    53,
      54,    55,    56,    57,    12,    -1,    -1,   115,    -1,    -1,
     118,    -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    -1,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,     4,    -1,    52,    53,    54,    55,    56,    57,
      12,    -1,    -1,    -1,   108,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    24,    -1,   118,    -1,    -1,    -1,    -1,    31,
      32,    33,    34,    35,    36,    -1,     4,    -1,    40,    41,
      42,    43,    44,    -1,    12,    47,    -1,    -1,    -1,    -1,
      52,    53,    54,    55,    56,    57,    24,    -1,    -1,    -1,
      -1,    -1,    -1,    31,    32,    33,    34,    35,    36,    -1,
     118,    -1,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,     4,    52,    53,    54,    55,    56,    57,
      -1,    12,    -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,
      -1,    -1,    -1,    24,    -1,    -1,   108,    -1,    -1,    -1,
      31,    32,    33,    34,    35,    36,   118,    -1,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      98,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   118
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   100,   141,   142,   143,   146,   119,   123,   330,
     147,   160,     0,   147,    66,    67,   144,   101,   114,   148,
     161,   162,     1,   103,   329,   104,   129,   197,   197,   108,
     149,    14,   163,   174,   175,   129,   198,    78,    78,     4,
      12,    21,    24,    26,    29,    31,    32,    33,    34,    35,
      36,    38,    39,    40,    41,    42,    43,    44,    47,    50,
      52,    53,    54,    55,    56,    57,   108,   118,   119,   122,
     123,   150,   151,   152,   157,   158,   296,   299,   300,   314,
     315,   316,   322,    27,    61,   164,   114,   159,     3,     5,
       6,     7,     8,    10,    13,    16,    17,    18,    19,    20,
      22,    26,    28,    37,    45,    50,    51,    58,    63,    64,
      77,    83,    89,    91,    92,   104,   108,   110,   119,   123,
     128,   129,   130,   131,   138,   139,   172,   176,   177,   178,
     179,   183,   187,   192,   239,   244,   249,   250,   254,   255,
     256,   257,   285,   286,   289,   290,   313,   314,   316,   324,
     325,   328,   105,   115,   330,   108,   295,   299,   104,   108,
     110,   285,    80,    89,    91,    92,    93,    98,   120,   121,
     124,   125,   319,   320,   321,   323,   109,   115,   108,   153,
     100,   102,   145,    78,    34,   165,   114,    64,   108,   247,
     248,   250,   251,   253,    34,    35,    36,    80,    89,    91,
      92,    93,   100,   104,   108,   110,   116,   118,   120,   121,
     124,   125,   185,   205,   206,   209,   211,   213,   215,   216,
     218,   294,   295,   297,   301,   302,   310,   311,   312,   322,
     108,   100,   102,   278,    85,   100,   247,   271,   272,   273,
      73,    74,    75,    76,   180,   100,   102,   194,   195,   211,
     213,   108,   116,   304,   313,   316,   320,   278,   255,   108,
     188,   287,   288,   290,   316,   325,   255,   104,   245,   246,
     129,   108,   314,   287,   288,     5,    92,   255,   276,   277,
     255,   254,   255,    80,   105,   116,   121,   125,   247,   248,
     258,   260,   261,   292,   306,   307,   309,   318,   319,   321,
     326,   327,    91,   109,   115,   258,   259,   319,   320,   321,
     326,   331,   111,   258,   259,   331,    19,   245,   245,   130,
     171,   159,    72,   186,    81,   116,   199,   292,   305,   307,
     308,   317,   319,   320,    99,   255,   100,   115,    88,   129,
      89,   301,   326,   109,   109,   109,   109,   109,   109,   152,
      79,   154,   155,   156,   157,   147,   147,    24,   167,   118,
     129,    23,    81,   305,   247,   203,   232,   233,   234,   313,
      29,   105,   207,   209,   211,    87,    89,   109,   207,   222,
     301,   331,   111,   207,   222,   223,   331,   299,   312,    27,
     196,   218,    90,    87,   216,   207,   221,   222,    20,    46,
      92,   247,   275,   279,   280,   281,   279,   264,   265,   281,
     271,   114,   252,   273,   329,    78,    78,    78,    78,   201,
     207,   219,   193,   239,   240,   249,   193,    15,    82,   320,
     316,   130,   130,    87,   327,    82,    86,   100,   115,   189,
     316,    81,   116,   291,   327,    89,   130,   314,    78,    78,
     129,    81,   200,   100,   102,   266,   255,    87,   276,    82,
      85,   241,   242,   243,     3,   315,   324,   305,    79,    85,
     115,   105,   115,   248,   109,   115,   109,   115,   109,   109,
     109,   115,   111,   111,   111,   219,   314,   314,   116,   173,
     291,   303,   304,   129,   185,   201,   202,   207,   208,   315,
     241,   250,   218,    79,   282,   283,   284,   314,   203,   255,
     109,   109,   109,   115,   101,   329,   129,   166,    78,    78,
     266,   201,   251,    81,   115,   101,   115,   224,   105,    90,
     109,    81,   109,   115,   109,    85,   111,   115,   111,   111,
     116,   116,   195,   211,   207,   109,   195,   278,   255,    86,
     101,   114,   329,    87,   115,   101,    25,   196,   101,   114,
     329,   247,   207,   208,   109,   116,   129,   129,   247,   109,
      92,   247,   274,   274,   190,   316,   288,   189,   202,   325,
     316,   130,   105,    78,    78,   104,   108,   110,   293,   294,
      78,   101,   114,   267,   268,   269,   274,   267,   329,   247,
     247,   264,   196,   243,   116,   116,   116,   247,    25,   262,
     263,   281,   247,   258,   258,   258,   258,    78,    81,    81,
     115,    78,   129,    81,    82,   184,   227,   116,   101,   115,
      82,    81,   156,   330,   207,   313,   234,    98,   108,   225,
     310,   207,   207,   226,   207,   207,   223,   207,   247,   280,
     247,   281,   247,   240,   129,   129,   255,    27,   191,   101,
     189,   116,   105,   109,   111,   267,   101,   114,    87,   270,
     271,   329,   196,    82,    47,   247,   115,    79,   201,   204,
     204,   303,    78,   226,    29,   228,   229,   230,     9,   235,
     236,   237,   283,   258,   202,   115,     4,   168,   207,   310,
     109,   109,   252,   100,   102,   269,   247,   196,   247,    48,
      49,    48,   263,   281,   247,    78,   115,    78,   224,    85,
     210,   214,   217,   218,   231,    22,    52,    53,   108,   181,
     238,   298,   299,   237,   330,    12,   108,   169,   170,    81,
      11,   193,   193,   196,   247,   247,   247,   201,    98,   229,
      90,   217,   291,   109,   211,   212,   220,   238,    54,   182,
     108,   150,   226,   247,   101,   329,    49,   214,   231,   214,
     115,   109,   211,   150,   109,   109,   247,   220,   109
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
     218,   218,   218,   218,   218,   218,   218,   218,   218,   218,
     218,   218,   219,   220,   220,   221,   221,   222,   222,   223,
     223,   224,   224,   225,   225,   226,   227,   228,   228,   229,
     229,   230,   230,   231,   231,   232,   232,   233,   233,   234,
     235,   235,   236,   236,   237,   237,   237,   238,   238,   238,
     239,   239,   239,   239,   240,   241,   241,   242,   242,   243,
     244,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   245,   245,   246,   246,   247,   247,   248,   248,   249,
     249,   250,   250,   250,   251,   251,   252,   252,   253,   253,
     254,   254,   254,   254,   255,   255,   255,   255,   255,   255,
     255,   255,   255,   255,   255,   255,   256,   256,   257,   257,
     257,   257,   257,   257,   257,   257,   257,   258,   258,   258,
     259,   259,   260,   260,   260,   260,   260,   260,   260,   261,
     261,   262,   262,   262,   262,   263,   263,   263,   263,   264,
     265,   265,   266,   266,   266,   266,   267,   267,   268,   268,
     268,   269,   270,   270,   271,   271,   272,   272,   273,   274,
     274,   275,   275,   276,   276,   277,   277,   278,   278,   279,
     279,   279,   279,   280,   280,   281,   281,   281,   282,   282,
     283,   283,   283,   284,   284,   285,   285,   286,   286,   287,
     287,   287,   288,   288,   289,   289,   289,   289,   290,   290,
     291,   291,   292,   292,   293,   293,   293,   294,   294,   294,
     294,   294,   295,   295,   295,   296,   296,   296,   296,   296,
     297,   297,   298,   299,   299,   300,   301,   301,   301,   302,
     302,   302,   302,   303,   303,   304,   304,   305,   305,   305,
     306,   306,   306,   307,   308,   308,   309,   309,   310,   311,
     312,   312,   312,   312,   312,   313,   313,   314,   314,   314,
     315,   315,   316,   316,   316,   316,   316,   316,   316,   316,
     317,   317,   318,   318,   319,   320,   320,   321,   321,   322,
     322,   322,   322,   322,   322,   322,   322,   322,   322,   322,
     322,   322,   322,   322,   322,   322,   322,   323,   323,   323,
     324,   324,   325,   326,   326,   327,   327,   328,   328,   328,
     328,   329,   329,   330,   330,   331,   331
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
       1,     1,     2,     3,     2,     5,     2,     3,     3,     3,
       3,     5,     1,     1,     3,     1,     0,     1,     3,     3,
       3,     2,     0,     1,     5,     1,     2,     3,     1,     4,
       2,     3,     0,     1,     3,     0,     1,     3,     1,     3,
       0,     1,     2,     1,     2,     3,     3,     1,     2,     3,
       1,     3,     3,     1,     1,     3,     2,     2,     1,     4,
       3,     5,     3,     1,     4,     4,     3,     4,     6,     6,
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
  "comma_types0", "comma_types1", "bar_types2", "tv_bndrs", "tv_bndr",
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
     885,   887,   888,   890,   891,   893,   894,   895,   901,   903,
     904,   905,   906,   907,   908,   909,   910,   911,   912,   913,
     914,   915,   918,   920,   921,   923,   924,   926,   927,   929,
     930,   932,   933,   935,   936,   954,   960,   962,   963,   965,
     966,   968,   969,   971,   972,   974,   975,   977,   978,   980,
     982,   983,   985,   986,   988,   989,   990,   992,   993,   994,
     999,  1001,  1003,  1004,  1007,  1011,  1012,  1014,  1015,  1019,
    1021,  1022,  1023,  1024,  1025,  1026,  1027,  1028,  1029,  1030,
    1031,  1033,  1034,  1036,  1037,  1041,  1042,  1044,  1045,  1047,
    1048,  1050,  1051,  1052,  1054,  1055,  1058,  1059,  1061,  1062,
    1066,  1067,  1068,  1069,  1071,  1072,  1073,  1074,  1075,  1076,
    1077,  1078,  1079,  1080,  1081,  1082,  1084,  1085,  1087,  1088,
    1089,  1090,  1091,  1092,  1093,  1094,  1095,  1100,  1101,  1102,
    1107,  1108,  1126,  1127,  1128,  1129,  1130,  1131,  1132,  1134,
    1135,  1147,  1148,  1149,  1150,  1152,  1153,  1154,  1155,  1158,
    1160,  1161,  1164,  1165,  1166,  1167,  1169,  1170,  1172,  1173,
    1174,  1176,  1178,  1179,  1181,  1182,  1184,  1185,  1187,  1189,
    1190,  1192,  1193,  1195,  1196,  1198,  1199,  1202,  1203,  1205,
    1206,  1207,  1208,  1213,  1214,  1216,  1217,  1218,  1223,  1224,
    1226,  1227,  1228,  1230,  1231,  1263,  1264,  1266,  1267,  1269,
    1270,  1271,  1273,  1274,  1276,  1277,  1278,  1279,  1281,  1282,
    1284,  1285,  1287,  1288,  1291,  1292,  1293,  1295,  1296,  1297,
    1298,  1299,  1301,  1302,  1303,  1305,  1306,  1307,  1308,  1309,
    1312,  1313,  1315,  1317,  1318,  1322,  1324,  1325,  1326,  1328,
    1329,  1330,  1331,  1336,  1337,  1339,  1340,  1342,  1343,  1344,
    1346,  1347,  1348,  1350,  1352,  1353,  1355,  1356,  1360,  1362,
    1364,  1365,  1366,  1367,  1368,  1371,  1372,  1374,  1375,  1376,
    1378,  1379,  1381,  1382,  1383,  1384,  1385,  1386,  1387,  1388,
    1390,  1391,  1393,  1394,  1396,  1398,  1399,  1401,  1402,  1404,
    1405,  1406,  1407,  1408,  1409,  1410,  1411,  1412,  1413,  1414,
    1415,  1416,  1417,  1418,  1419,  1420,  1421,  1423,  1424,  1425,
    1429,  1430,  1432,  1434,  1435,  1437,  1438,  1442,  1443,  1444,
    1445,  1450,  1453,  1457,  1458,  1460,  1461
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
#line 6061 "parser.cc"

#line 1470 "parser.y"


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

