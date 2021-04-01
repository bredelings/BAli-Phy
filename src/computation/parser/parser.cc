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
      case symbol_kind::S_alt_rhs: // alt_rhs
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

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.YY_MOVE_OR_COPY< std::vector<Haskell::Alt> > (YY_MOVE (that.value));
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
      case symbol_kind::S_alt_rhs: // alt_rhs
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

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Haskell::Alt> > (YY_MOVE (that.value));
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
      case symbol_kind::S_alt_rhs: // alt_rhs
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

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.copy< std::vector<Haskell::Alt> > (that.value);
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
      case symbol_kind::S_alt_rhs: // alt_rhs
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

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Haskell::Alt> > (that.value);
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
      case symbol_kind::S_alt_rhs: // alt_rhs
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

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        yylhs.value.emplace< std::vector<Haskell::Alt> > ();
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
#line 1746 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 536 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = make_module(yystack_[4].value.as < std::string > (),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 1752 "parser.cc"
    break;

  case 4: // module: body2
#line 537 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = make_module("Main",{},yystack_[0].value.as < expression_ref > ());}
#line 1758 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 539 "parser.y"
                                                                 {drv.push_module_context();}
#line 1764 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 547 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1770 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 548 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1776 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 550 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1782 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 551 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1788 "parser.cc"
    break;

  case 13: // top: semis top1
#line 554 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1794 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 556 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 1800 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 557 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 1806 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 558 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[0].value.as < std::vector<expression_ref> > (),{});}
#line 1812 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 566 "parser.y"
                                      {yylhs.value.as < expression_ref > () = make_exports(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 1818 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 567 "parser.y"
                                      {}
#line 1824 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 569 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 1830 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 571 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1836 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 572 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1842 "parser.cc"
    break;

  case 22: // export: qcname_ext export_subspec
#line 574 "parser.y"
                                      {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1848 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 575 "parser.y"
                                      {yylhs.value.as < expression_ref > () = AST_node("module",yystack_[0].value.as < std::string > ());}
#line 1854 "parser.cc"
    break;

  case 26: // qcnames: %empty
#line 581 "parser.y"
                   {}
#line 1860 "parser.cc"
    break;

  case 27: // qcnames: qcnames1
#line 582 "parser.y"
                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 1866 "parser.cc"
    break;

  case 28: // qcnames1: qcnames1 "," qcname_ext_w_wildcard ","
#line 584 "parser.y"
                                                  {yylhs.value.as < std::vector<expression_ref> > () = yystack_[3].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ());}
#line 1872 "parser.cc"
    break;

  case 29: // qcnames1: qcname_ext_w_wildcard
#line 585 "parser.y"
                                              {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1878 "parser.cc"
    break;

  case 30: // qcname_ext_w_wildcard: qcname_ext
#line 587 "parser.y"
                                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1884 "parser.cc"
    break;

  case 31: // qcname_ext_w_wildcard: ".."
#line 588 "parser.y"
                                     {}
#line 1890 "parser.cc"
    break;

  case 32: // qcname_ext: qcname
#line 590 "parser.y"
                                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1896 "parser.cc"
    break;

  case 33: // qcname_ext: "type" oqtycon
#line 591 "parser.y"
                                     {}
#line 1902 "parser.cc"
    break;

  case 34: // qcname: qvar
#line 593 "parser.y"
                                     {yylhs.value.as < expression_ref > () = AST_node("qvar",yystack_[0].value.as < std::string > ()); }
#line 1908 "parser.cc"
    break;

  case 35: // qcname: oqtycon_no_varcon
#line 594 "parser.y"
                                     {yylhs.value.as < expression_ref > () = AST_node("qvar",yystack_[0].value.as < std::string > ()); }
#line 1914 "parser.cc"
    break;

  case 40: // importdecls: importdecls_semi importdecl
#line 604 "parser.y"
                                         { yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (), yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 1920 "parser.cc"
    break;

  case 41: // importdecls_semi: importdecls_semi importdecl semis1
#line 606 "parser.y"
                                                     { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 1926 "parser.cc"
    break;

  case 42: // importdecls_semi: %empty
#line 607 "parser.y"
                         { }
#line 1932 "parser.cc"
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
#line 1945 "parser.cc"
    break;

  case 44: // maybe_src: "{-# SOURCE" "#-}"
#line 618 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 1951 "parser.cc"
    break;

  case 45: // maybe_src: %empty
#line 619 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 1957 "parser.cc"
    break;

  case 46: // maybe_safe: "safe"
#line 621 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 1963 "parser.cc"
    break;

  case 47: // maybe_safe: %empty
#line 622 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 1969 "parser.cc"
    break;

  case 48: // maybe_pkg: "STRING"
#line 624 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 1975 "parser.cc"
    break;

  case 49: // maybe_pkg: %empty
#line 625 "parser.y"
                               { }
#line 1981 "parser.cc"
    break;

  case 50: // optqualified: "qualified"
#line 627 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 1987 "parser.cc"
    break;

  case 51: // optqualified: %empty
#line 628 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 1993 "parser.cc"
    break;

  case 52: // maybeas: "as" modid
#line 630 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 1999 "parser.cc"
    break;

  case 53: // maybeas: %empty
#line 631 "parser.y"
                               { }
#line 2005 "parser.cc"
    break;

  case 54: // maybeimpspec: impspec
#line 633 "parser.y"
                               { yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > (); }
#line 2011 "parser.cc"
    break;

  case 55: // maybeimpspec: %empty
#line 634 "parser.y"
                               { }
#line 2017 "parser.cc"
    break;

  case 56: // impspec: "(" exportlist ")"
#line 636 "parser.y"
                                      { yylhs.value.as < expression_ref > () = expression_ref{AST_node("only"),yystack_[1].value.as < std::vector<expression_ref> > ()}; }
#line 2023 "parser.cc"
    break;

  case 57: // impspec: "hiding" "(" exportlist ")"
#line 637 "parser.y"
                                      { yylhs.value.as < expression_ref > () = expression_ref{AST_node("hiding"),yystack_[1].value.as < std::vector<expression_ref> > ()}; }
#line 2029 "parser.cc"
    break;

  case 58: // prec: %empty
#line 642 "parser.y"
                   { }
#line 2035 "parser.cc"
    break;

  case 59: // prec: "INTEGER"
#line 643 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < int > (); }
#line 2041 "parser.cc"
    break;

  case 60: // infix: "infix"
#line 645 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infix; }
#line 2047 "parser.cc"
    break;

  case 61: // infix: "infixl"
#line 646 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infixl; }
#line 2053 "parser.cc"
    break;

  case 62: // infix: "infixr"
#line 647 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infixr; }
#line 2059 "parser.cc"
    break;

  case 63: // ops: ops "," op
#line 649 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 2065 "parser.cc"
    break;

  case 64: // ops: op
#line 650 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 2071 "parser.cc"
    break;

  case 65: // topdecls: topdecls_semi topdecl
#line 654 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 2077 "parser.cc"
    break;

  case 66: // topdecls_semi: topdecls_semi topdecl semis1
#line 656 "parser.y"
                                            { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2083 "parser.cc"
    break;

  case 67: // topdecls_semi: %empty
#line 657 "parser.y"
                                            { }
#line 2089 "parser.cc"
    break;

  case 68: // topdecl: cl_decl
#line 659 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2095 "parser.cc"
    break;

  case 69: // topdecl: ty_decl
#line 660 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2101 "parser.cc"
    break;

  case 70: // topdecl: inst_decl
#line 661 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2107 "parser.cc"
    break;

  case 71: // topdecl: "default" "(" comma_types0 ")"
#line 664 "parser.y"
                                               {}
#line 2113 "parser.cc"
    break;

  case 72: // topdecl: decl_no_th
#line 671 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2119 "parser.cc"
    break;

  case 73: // topdecl: infixexp_top
#line 673 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2125 "parser.cc"
    break;

  case 74: // topdecl: "builtin" var "INTEGER" "STRING" "STRING"
#line 674 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2131 "parser.cc"
    break;

  case 75: // topdecl: "builtin" var "INTEGER" "STRING"
#line 675 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2137 "parser.cc"
    break;

  case 76: // topdecl: "builtin" varop "INTEGER" "STRING" "STRING"
#line 676 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2143 "parser.cc"
    break;

  case 77: // topdecl: "builtin" varop "INTEGER" "STRING"
#line 677 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2149 "parser.cc"
    break;

  case 78: // cl_decl: "class" tycl_hdr wherebinds
#line 679 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_class_decl(yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().second,{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2155 "parser.cc"
    break;

  case 79: // ty_decl: "type" type "=" ctypedoc
#line 681 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_type_synonym({yystack_[2].location,yystack_[2].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2161 "parser.cc"
    break;

  case 80: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 682 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < Haskell::DataOrNewtype > (),yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().second,yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2167 "parser.cc"
    break;

  case 81: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig
#line 683 "parser.y"
                                                                           {}
#line 2173 "parser.cc"
    break;

  case 82: // inst_decl: "instance" overlap_pragma inst_type wherebinds
#line 688 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_instance_decl({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2179 "parser.cc"
    break;

  case 92: // data_or_newtype: "data"
#line 743 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::data;}
#line 2185 "parser.cc"
    break;

  case 93: // data_or_newtype: "newtype"
#line 744 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::newtype;}
#line 2191 "parser.cc"
    break;

  case 96: // tycl_hdr: context "=>" type
#line 756 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ()};}
#line 2197 "parser.cc"
    break;

  case 97: // tycl_hdr: type
#line 757 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {{},yystack_[0].value.as < expression_ref > ()};}
#line 2203 "parser.cc"
    break;

  case 101: // decls: decls ";" decl
#line 805 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2209 "parser.cc"
    break;

  case 102: // decls: decls ";"
#line 806 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2215 "parser.cc"
    break;

  case 103: // decls: decl
#line 807 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2221 "parser.cc"
    break;

  case 104: // decls: %empty
#line 808 "parser.y"
                        {}
#line 2227 "parser.cc"
    break;

  case 105: // decllist: "{" decls "}"
#line 810 "parser.y"
                                 {yylhs.value.as < expression_ref > () = expression_ref{AST_node("Decls"),yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 2233 "parser.cc"
    break;

  case 106: // decllist: "vocurly" decls close
#line 811 "parser.y"
                                 {yylhs.value.as < expression_ref > () = expression_ref{AST_node("Decls"),yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 2239 "parser.cc"
    break;

  case 107: // binds: decllist
#line 813 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2245 "parser.cc"
    break;

  case 108: // wherebinds: "where" binds
#line 815 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2251 "parser.cc"
    break;

  case 109: // wherebinds: %empty
#line 816 "parser.y"
                                 {}
#line 2257 "parser.cc"
    break;

  case 115: // opt_sig: %empty
#line 837 "parser.y"
                 {}
#line 2263 "parser.cc"
    break;

  case 116: // opt_sig: "::" sigtype
#line 838 "parser.y"
                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2269 "parser.cc"
    break;

  case 117: // opt_tyconsig: %empty
#line 840 "parser.y"
                     {}
#line 2275 "parser.cc"
    break;

  case 118: // opt_tyconsig: "::" gtycon
#line 841 "parser.y"
                     {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2281 "parser.cc"
    break;

  case 119: // sigtype: ctype
#line 843 "parser.y"
                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2287 "parser.cc"
    break;

  case 120: // sigtypedoc: ctypedoc
#line 845 "parser.y"
                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2293 "parser.cc"
    break;

  case 121: // sig_vars: sig_vars "," var
#line 847 "parser.y"
                           {yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 2299 "parser.cc"
    break;

  case 122: // sig_vars: var
#line 848 "parser.y"
                           {yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 2305 "parser.cc"
    break;

  case 123: // sigtypes1: sigtype
#line 850 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2311 "parser.cc"
    break;

  case 124: // sigtypes1: sigtypes1 "," sigtype
#line 851 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2317 "parser.cc"
    break;

  case 125: // strict_mark: strictness
#line 855 "parser.y"
                                            {yylhs.value.as < Haskell::StrictLazy > () = yystack_[0].value.as < Haskell::StrictLazy > ();}
#line 2323 "parser.cc"
    break;

  case 126: // strictness: "!"
#line 861 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::strict;}
#line 2329 "parser.cc"
    break;

  case 127: // strictness: "~"
#line 862 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::lazy;}
#line 2335 "parser.cc"
    break;

  case 128: // ctype: "forall" tv_bndrs "." ctype
#line 869 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_forall_type(yystack_[2].value.as < std::vector<expression_ref> > (), yystack_[0].value.as < expression_ref > ());}
#line 2341 "parser.cc"
    break;

  case 129: // ctype: context "=>" ctype
#line 870 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_constrained_type(yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ());}
#line 2347 "parser.cc"
    break;

  case 130: // ctype: type
#line 872 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2353 "parser.cc"
    break;

  case 131: // ctypedoc: ctype
#line 874 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2359 "parser.cc"
    break;

  case 132: // context: btype
#line 883 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(yystack_[0].value.as < expression_ref > ());}
#line 2365 "parser.cc"
    break;

  case 133: // context_no_ops: btype_no_ops
#line 885 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2371 "parser.cc"
    break;

  case 134: // type: btype
#line 887 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2377 "parser.cc"
    break;

  case 135: // type: btype "->" ctype
#line 888 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps({make_type_var("->"),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()});}
#line 2383 "parser.cc"
    break;

  case 136: // typedoc: type
#line 890 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2389 "parser.cc"
    break;

  case 137: // btype: tyapps
#line 893 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2395 "parser.cc"
    break;

  case 138: // btype_no_ops: atype_docs
#line 895 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2401 "parser.cc"
    break;

  case 139: // btype_no_ops: btype_no_ops atype_docs
#line 896 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2407 "parser.cc"
    break;

  case 140: // tyapps: tyapp
#line 898 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2413 "parser.cc"
    break;

  case 141: // tyapps: tyapps tyapp
#line 899 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2419 "parser.cc"
    break;

  case 142: // tyapp: atype
#line 901 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2425 "parser.cc"
    break;

  case 143: // tyapp: qtyconop
#line 902 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2431 "parser.cc"
    break;

  case 144: // tyapp: tyvarop
#line 903 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2437 "parser.cc"
    break;

  case 145: // atype_docs: atype
#line 909 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2443 "parser.cc"
    break;

  case 146: // atype: ntgtycon
#line 916 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2449 "parser.cc"
    break;

  case 147: // atype: tyvar
#line 917 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2455 "parser.cc"
    break;

  case 148: // atype: "*"
#line 918 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var("*");}
#line 2461 "parser.cc"
    break;

  case 149: // atype: strict_mark atype
#line 919 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_strict_lazy_type(yystack_[1].value.as < Haskell::StrictLazy > (),yystack_[0].value.as < expression_ref > ());}
#line 2467 "parser.cc"
    break;

  case 150: // atype: "{" fielddecls "}"
#line 920 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_field_decls(yystack_[1].value.as < std::vector<Haskell::FieldDecl> > ());}
#line 2473 "parser.cc"
    break;

  case 151: // atype: "(" ")"
#line 921 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var("()");}
#line 2479 "parser.cc"
    break;

  case 152: // atype: "(" comma_types1 "," ctype ")"
#line 922 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<expression_ref> > ();ts.push_back(yystack_[1].value.as < expression_ref > ());yylhs.value.as < expression_ref > () = make_tuple_type(ts);}
#line 2485 "parser.cc"
    break;

  case 153: // atype: "[" ctype "]"
#line 928 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_list_type(yystack_[1].value.as < expression_ref > ());}
#line 2491 "parser.cc"
    break;

  case 154: // atype: "(" ctype ")"
#line 929 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2497 "parser.cc"
    break;

  case 155: // atype: "(" ctype "::" kind ")"
#line 930 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_of_kind(yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ());}
#line 2503 "parser.cc"
    break;

  case 156: // inst_type: sigtype
#line 933 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2509 "parser.cc"
    break;

  case 159: // comma_types0: comma_types1
#line 938 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2515 "parser.cc"
    break;

  case 160: // comma_types0: %empty
#line 939 "parser.y"
                                       { /* default construction OK */ }
#line 2521 "parser.cc"
    break;

  case 161: // comma_types1: ctype
#line 941 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2527 "parser.cc"
    break;

  case 162: // comma_types1: comma_types1 "," ctype
#line 942 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2533 "parser.cc"
    break;

  case 163: // tv_bndrs: tv_bndrs tv_bndr
#line 949 "parser.y"
                               {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2539 "parser.cc"
    break;

  case 164: // tv_bndrs: %empty
#line 950 "parser.y"
                               { /* default construction OK */}
#line 2545 "parser.cc"
    break;

  case 165: // tv_bndr: tyvar
#line 952 "parser.y"
                                    {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2551 "parser.cc"
    break;

  case 166: // tv_bndr: "(" tyvar "::" kind ")"
#line 953 "parser.y"
                                    {yylhs.value.as < expression_ref > () = make_type_var_of_kind(yystack_[3].value.as < std::string > (),yystack_[1].value.as < expression_ref > ());}
#line 2557 "parser.cc"
    break;

  case 167: // kind: ctype
#line 971 "parser.y"
             {yylhs.value.as < expression_ref > () = make_kind(yystack_[0].value.as < expression_ref > ());}
#line 2563 "parser.cc"
    break;

  case 168: // constrs: "=" constrs1
#line 977 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2569 "parser.cc"
    break;

  case 169: // constrs1: constrs1 "|" constr
#line 979 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2575 "parser.cc"
    break;

  case 170: // constrs1: constr
#line 980 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2581 "parser.cc"
    break;

  case 171: // constr: forall context_no_ops "=>" constr_stuff
#line 982 "parser.y"
                                                {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2587 "parser.cc"
    break;

  case 172: // constr: forall constr_stuff
#line 983 "parser.y"
                                                {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2593 "parser.cc"
    break;

  case 173: // forall: "forall" tv_bndrs "."
#line 985 "parser.y"
                                {if (yystack_[1].value.as < std::vector<expression_ref> > ().size()>1) yylhs.value.as < expression_ref > () = make_tv_bndrs(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2599 "parser.cc"
    break;

  case 174: // forall: %empty
#line 986 "parser.y"
                                {}
#line 2605 "parser.cc"
    break;

  case 175: // constr_stuff: btype_no_ops
#line 988 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2611 "parser.cc"
    break;

  case 176: // constr_stuff: btype_no_ops conop btype_no_ops
#line 989 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps({make_type_var(yystack_[1].value.as < std::string > ()),make_tyapps(yystack_[2].value.as < std::vector<expression_ref> > ()),make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 2617 "parser.cc"
    break;

  case 177: // fielddecls: %empty
#line 991 "parser.y"
                                {}
#line 2623 "parser.cc"
    break;

  case 178: // fielddecls: fielddecls1
#line 992 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::FieldDecl> > () = yystack_[0].value.as < std::vector<Haskell::FieldDecl> > ();}
#line 2629 "parser.cc"
    break;

  case 179: // fielddecls1: fielddecls1 "," fielddecl
#line 994 "parser.y"
                                        {yylhs.value.as < std::vector<Haskell::FieldDecl> > () = yystack_[2].value.as < std::vector<Haskell::FieldDecl> > (); yylhs.value.as < std::vector<Haskell::FieldDecl> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2635 "parser.cc"
    break;

  case 180: // fielddecls1: fielddecl
#line 995 "parser.y"
                                        {yylhs.value.as < std::vector<Haskell::FieldDecl> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2641 "parser.cc"
    break;

  case 181: // fielddecl: sig_vars "::" ctype
#line 997 "parser.y"
                                        {yylhs.value.as < Haskell::FieldDecl > () = make_field_decl(yystack_[2].value.as < std::vector<std::string> > (),yystack_[0].value.as < expression_ref > ());}
#line 2647 "parser.cc"
    break;

  case 192: // decl_no_th: sigdecl
#line 1016 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2653 "parser.cc"
    break;

  case 193: // decl_no_th: "!" aexp rhs
#line 1018 "parser.y"
                              {yylhs.value.as < expression_ref > () = new expression(AST_node("Decl:Strict"),{(yystack_[1].value.as < expression_ref > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2659 "parser.cc"
    break;

  case 194: // decl_no_th: infixexp_top opt_sig rhs
#line 1020 "parser.y"
                              {yylhs.value.as < expression_ref > () = new expression(AST_node("Decl"),{make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2665 "parser.cc"
    break;

  case 195: // decl: decl_no_th
#line 1024 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2671 "parser.cc"
    break;

  case 196: // rhs: "=" exp wherebinds
#line 1028 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_rhs(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2677 "parser.cc"
    break;

  case 197: // rhs: gdrhs wherebinds
#line 1029 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2683 "parser.cc"
    break;

  case 198: // gdrhs: gdrhs gdrh
#line 1031 "parser.y"
                              {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2689 "parser.cc"
    break;

  case 199: // gdrhs: gdrh
#line 1032 "parser.y"
                              {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2695 "parser.cc"
    break;

  case 200: // gdrh: "|" guardquals "=" exp
#line 1036 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2701 "parser.cc"
    break;

  case 201: // sigdecl: infixexp_top "::" sigtypedoc
#line 1038 "parser.y"
                                             { yylhs.value.as < expression_ref > () = expression_ref(AST_node("Decl:sigtype"),{make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2707 "parser.cc"
    break;

  case 202: // sigdecl: var "," sig_vars "::" sigtypedoc
#line 1039 "parser.y"
                                          {}
#line 2713 "parser.cc"
    break;

  case 203: // sigdecl: infix prec ops
#line 1040 "parser.y"
                         { yylhs.value.as < expression_ref > () = make_fixity_decl(yystack_[2].value.as < Haskell::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()); }
#line 2719 "parser.cc"
    break;

  case 204: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1042 "parser.y"
                                                    {}
#line 2725 "parser.cc"
    break;

  case 205: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1043 "parser.y"
                                            {}
#line 2731 "parser.cc"
    break;

  case 206: // sigdecl: "{-# SCC" qvar "#-}"
#line 1044 "parser.y"
                              {}
#line 2737 "parser.cc"
    break;

  case 207: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1045 "parser.y"
                                     {}
#line 2743 "parser.cc"
    break;

  case 208: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1046 "parser.y"
                                                               {}
#line 2749 "parser.cc"
    break;

  case 209: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1047 "parser.y"
                                                                      {}
#line 2755 "parser.cc"
    break;

  case 210: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1048 "parser.y"
                                                     {}
#line 2761 "parser.cc"
    break;

  case 215: // exp: infixexp "::" sigtype
#line 1059 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_typed_exp(make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()); }
#line 2767 "parser.cc"
    break;

  case 216: // exp: infixexp
#line 1060 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 2773 "parser.cc"
    break;

  case 217: // infixexp: exp10
#line 1062 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2779 "parser.cc"
    break;

  case 218: // infixexp: infixexp qop exp10
#line 1063 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_id(yystack_[1].location,yystack_[1].value.as < std::string > ())); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2785 "parser.cc"
    break;

  case 219: // infixexp_top: exp10_top
#line 1065 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2791 "parser.cc"
    break;

  case 220: // infixexp_top: infixexp_top qop exp10_top
#line 1066 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_id(yystack_[1].location,yystack_[1].value.as < std::string > ())); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2797 "parser.cc"
    break;

  case 221: // exp10_top: "-" fexp
#line 1068 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_minus(make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2803 "parser.cc"
    break;

  case 222: // exp10_top: "{-# CORE" "STRING" "#-}"
#line 1069 "parser.y"
                                   {}
#line 2809 "parser.cc"
    break;

  case 223: // exp10_top: fexp
#line 1070 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2815 "parser.cc"
    break;

  case 224: // exp10: exp10_top
#line 1072 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2821 "parser.cc"
    break;

  case 225: // exp10: scc_annot exp
#line 1073 "parser.y"
                                 {}
#line 2827 "parser.cc"
    break;

  case 230: // fexp: fexp aexp
#line 1084 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2833 "parser.cc"
    break;

  case 231: // fexp: fexp "TYPEAPP" atype
#line 1085 "parser.y"
                                 {}
#line 2839 "parser.cc"
    break;

  case 232: // fexp: "static" aexp
#line 1086 "parser.y"
                                 {}
#line 2845 "parser.cc"
    break;

  case 233: // fexp: aexp
#line 1087 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2851 "parser.cc"
    break;

  case 234: // aexp: qvar "@" aexp
#line 1089 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_as_pattern(make_id(yystack_[2].location,yystack_[2].value.as < std::string > ()),yystack_[0].value.as < expression_ref > ());}
#line 2857 "parser.cc"
    break;

  case 235: // aexp: "~" aexp
#line 1090 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lazy_pattern(yystack_[0].value.as < expression_ref > ());}
#line 2863 "parser.cc"
    break;

  case 236: // aexp: "\\" apats1 "->" exp
#line 1091 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lambda(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2869 "parser.cc"
    break;

  case 237: // aexp: "let" binds "in" exp
#line 1092 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_let(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2875 "parser.cc"
    break;

  case 238: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1094 "parser.y"
                                                       {yylhs.value.as < expression_ref > () = make_if(yystack_[6].value.as < expression_ref > (),yystack_[3].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2881 "parser.cc"
    break;

  case 239: // aexp: "case" exp "of" altslist
#line 1096 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_case(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < Haskell::Alts > ());}
#line 2887 "parser.cc"
    break;

  case 240: // aexp: "do" stmtlist
#line 1097 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_do(yystack_[0].value.as < Haskell::Stmts > ());}
#line 2893 "parser.cc"
    break;

  case 241: // aexp: "mdo" stmtlist
#line 1098 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_mdo(yystack_[0].value.as < Haskell::Stmts > ());}
#line 2899 "parser.cc"
    break;

  case 242: // aexp: aexp1
#line 1100 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2905 "parser.cc"
    break;

  case 243: // aexp1: aexp1 "{" fbinds "}"
#line 1102 "parser.y"
                              {}
#line 2911 "parser.cc"
    break;

  case 244: // aexp1: aexp2
#line 1103 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2917 "parser.cc"
    break;

  case 245: // aexp2: qvar
#line 1105 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_id(yystack_[0].location,yystack_[0].value.as < std::string > ());}
#line 2923 "parser.cc"
    break;

  case 246: // aexp2: qcon
#line 1106 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_id(yystack_[0].location,yystack_[0].value.as < std::string > ());}
#line 2929 "parser.cc"
    break;

  case 247: // aexp2: literal
#line 1107 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2935 "parser.cc"
    break;

  case 248: // aexp2: "(" texp ")"
#line 1108 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2941 "parser.cc"
    break;

  case 249: // aexp2: "(" tup_exprs ")"
#line 1109 "parser.y"
                              {yylhs.value.as < expression_ref > () = yy_make_tuple(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2947 "parser.cc"
    break;

  case 250: // aexp2: "[" list "]"
#line 1114 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2953 "parser.cc"
    break;

  case 251: // aexp2: "_"
#line 1115 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::WildcardPattern();}
#line 2959 "parser.cc"
    break;

  case 252: // texp: exp
#line 1120 "parser.y"
                      {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2965 "parser.cc"
    break;

  case 253: // texp: infixexp qop
#line 1121 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("LeftSection"),{make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ()),make_id(yystack_[0].location,yystack_[0].value.as < std::string > ())});}
#line 2971 "parser.cc"
    break;

  case 254: // texp: qopm infixexp
#line 1122 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("RightSection"),{make_id(yystack_[1].location,yystack_[1].value.as < std::string > ()),make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 2977 "parser.cc"
    break;

  case 255: // tup_exprs: tup_exprs "," texp
#line 1127 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2983 "parser.cc"
    break;

  case 256: // tup_exprs: texp "," texp
#line 1128 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2989 "parser.cc"
    break;

  case 257: // list: texp
#line 1146 "parser.y"
                                 { yylhs.value.as < expression_ref > () = make_list({yystack_[0].value.as < expression_ref > ()}); }
#line 2995 "parser.cc"
    break;

  case 258: // list: lexps
#line 1147 "parser.y"
                                 { yylhs.value.as < expression_ref > () = make_list(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 3001 "parser.cc"
    break;

  case 259: // list: texp ".."
#line 1148 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFrom"),{yystack_[1].value.as < expression_ref > ()}); }
#line 3007 "parser.cc"
    break;

  case 260: // list: texp "," exp ".."
#line 1149 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThen"),{yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()}); }
#line 3013 "parser.cc"
    break;

  case 261: // list: texp ".." exp
#line 1150 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromTo"),{yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 3019 "parser.cc"
    break;

  case 262: // list: texp "," exp ".." exp
#line 1151 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThenTo"),{yystack_[4].value.as < expression_ref > (),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 3025 "parser.cc"
    break;

  case 263: // list: texp "|" squals
#line 1152 "parser.y"
                                 { auto quals = yystack_[0].value.as < std::vector<expression_ref> > (); quals.push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < expression_ref > () = expression_ref(AST_node("ListComprehension"),quals); }
#line 3031 "parser.cc"
    break;

  case 264: // lexps: lexps "," texp
#line 1154 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3037 "parser.cc"
    break;

  case 265: // lexps: texp "," texp
#line 1155 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3043 "parser.cc"
    break;

  case 266: // squals: squals "," transformqual
#line 1167 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3049 "parser.cc"
    break;

  case 267: // squals: squals "," qual
#line 1168 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3055 "parser.cc"
    break;

  case 268: // squals: transformqual
#line 1169 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3061 "parser.cc"
    break;

  case 269: // squals: qual
#line 1170 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3067 "parser.cc"
    break;

  case 270: // transformqual: "then" exp
#line 1172 "parser.y"
                                                    {}
#line 3073 "parser.cc"
    break;

  case 271: // transformqual: "then" exp "by" exp
#line 1173 "parser.y"
                                                    {}
#line 3079 "parser.cc"
    break;

  case 272: // transformqual: "then" "group" "using" exp
#line 1174 "parser.y"
                                                    {}
#line 3085 "parser.cc"
    break;

  case 273: // transformqual: "then" "group" "by" exp "using" exp
#line 1175 "parser.y"
                                                    {}
#line 3091 "parser.cc"
    break;

  case 274: // guardquals: guardquals1
#line 1178 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3097 "parser.cc"
    break;

  case 275: // guardquals1: guardquals1 "," qual
#line 1180 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > ();yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3103 "parser.cc"
    break;

  case 276: // guardquals1: qual
#line 1181 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3109 "parser.cc"
    break;

  case 277: // altslist: "{" alts "}"
#line 1184 "parser.y"
                                 {yylhs.value.as < Haskell::Alts > () = make_alts(yystack_[1].value.as < std::vector<Haskell::Alt> > ());}
#line 3115 "parser.cc"
    break;

  case 278: // altslist: "vocurly" alts close
#line 1185 "parser.y"
                                 {yylhs.value.as < Haskell::Alts > () = make_alts(yystack_[1].value.as < std::vector<Haskell::Alt> > ());}
#line 3121 "parser.cc"
    break;

  case 279: // altslist: "{" "}"
#line 1186 "parser.y"
                                 {}
#line 3127 "parser.cc"
    break;

  case 280: // altslist: "vocurly" close
#line 1187 "parser.y"
                                 {}
#line 3133 "parser.cc"
    break;

  case 281: // alts: alts1
#line 1189 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::Alt> > () = yystack_[0].value.as < std::vector<Haskell::Alt> > ();}
#line 3139 "parser.cc"
    break;

  case 282: // alts: ";" alts
#line 1190 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::Alt> > () = yystack_[0].value.as < std::vector<Haskell::Alt> > ();}
#line 3145 "parser.cc"
    break;

  case 283: // alts1: alts1 ";" alt
#line 1192 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::Alt> > () = yystack_[2].value.as < std::vector<Haskell::Alt> > (); yylhs.value.as < std::vector<Haskell::Alt> > ().push_back(yystack_[0].value.as < Haskell::Alt > ());}
#line 3151 "parser.cc"
    break;

  case 284: // alts1: alts1 ";"
#line 1193 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::Alt> > () = yystack_[1].value.as < std::vector<Haskell::Alt> > ();}
#line 3157 "parser.cc"
    break;

  case 285: // alts1: alt
#line 1194 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::Alt> > ().push_back(yystack_[0].value.as < Haskell::Alt > ());}
#line 3163 "parser.cc"
    break;

  case 286: // alt: pat alt_rhs
#line 1196 "parser.y"
                                 {yylhs.value.as < Haskell::Alt > () = yy_make_alt(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3169 "parser.cc"
    break;

  case 287: // alt_rhs: "->" exp wherebinds
#line 1198 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_rhs(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3175 "parser.cc"
    break;

  case 288: // alt_rhs: gdpats wherebinds
#line 1199 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3181 "parser.cc"
    break;

  case 289: // gdpats: gdpats gdpat
#line 1201 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3187 "parser.cc"
    break;

  case 290: // gdpats: gdpat
#line 1202 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3193 "parser.cc"
    break;

  case 291: // gdpat: "|" guardquals "->" exp
#line 1211 "parser.y"
                                 {yylhs.value.as < expression_ref > ()=make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3199 "parser.cc"
    break;

  case 292: // pat: exp
#line 1213 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3205 "parser.cc"
    break;

  case 293: // pat: "!" aexp
#line 1214 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3211 "parser.cc"
    break;

  case 294: // bindpat: exp
#line 1216 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3217 "parser.cc"
    break;

  case 295: // bindpat: "!" aexp
#line 1217 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3223 "parser.cc"
    break;

  case 296: // apat: aexp
#line 1219 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3229 "parser.cc"
    break;

  case 297: // apat: "!" aexp
#line 1220 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3235 "parser.cc"
    break;

  case 298: // apats1: apats1 apat
#line 1222 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3241 "parser.cc"
    break;

  case 299: // apats1: apat
#line 1223 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3247 "parser.cc"
    break;

  case 300: // stmtlist: "{" stmts "}"
#line 1226 "parser.y"
                               {yylhs.value.as < Haskell::Stmts > () = make_stmts(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3253 "parser.cc"
    break;

  case 301: // stmtlist: "vocurly" stmts close
#line 1227 "parser.y"
                               {yylhs.value.as < Haskell::Stmts > () = make_stmts(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3259 "parser.cc"
    break;

  case 302: // stmts: stmts ";" stmt
#line 1229 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3265 "parser.cc"
    break;

  case 303: // stmts: stmts ";"
#line 1230 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3271 "parser.cc"
    break;

  case 304: // stmts: stmt
#line 1231 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3277 "parser.cc"
    break;

  case 305: // stmts: %empty
#line 1232 "parser.y"
                       {}
#line 3283 "parser.cc"
    break;

  case 306: // stmt: qual
#line 1237 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3289 "parser.cc"
    break;

  case 307: // stmt: "rec" stmtlist
#line 1238 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::RecStmt(yystack_[0].value.as < Haskell::Stmts > ());}
#line 3295 "parser.cc"
    break;

  case 308: // qual: bindpat "<-" exp
#line 1240 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::PatQual(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3301 "parser.cc"
    break;

  case 309: // qual: exp
#line 1241 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::SimpleQual(yystack_[0].value.as < expression_ref > ());}
#line 3307 "parser.cc"
    break;

  case 310: // qual: "let" binds
#line 1242 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::LetQual(yystack_[0].value.as < expression_ref > ());}
#line 3313 "parser.cc"
    break;

  case 318: // qcon: gen_qcon
#line 1287 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3319 "parser.cc"
    break;

  case 319: // qcon: sysdcon
#line 1288 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3325 "parser.cc"
    break;

  case 320: // gen_qcon: qconid
#line 1290 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3331 "parser.cc"
    break;

  case 321: // gen_qcon: "(" qconsym ")"
#line 1291 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3337 "parser.cc"
    break;

  case 322: // con: conid
#line 1293 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3343 "parser.cc"
    break;

  case 323: // con: "(" consym ")"
#line 1294 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3349 "parser.cc"
    break;

  case 324: // con: sysdcon
#line 1295 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3355 "parser.cc"
    break;

  case 327: // sysdcon_no_list: "(" ")"
#line 1300 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 3361 "parser.cc"
    break;

  case 328: // sysdcon_no_list: "(" commas ")"
#line 1301 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3367 "parser.cc"
    break;

  case 329: // sysdcon_no_list: "(#" "#)"
#line 1302 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 3373 "parser.cc"
    break;

  case 330: // sysdcon_no_list: "(#" commas "#)"
#line 1303 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3379 "parser.cc"
    break;

  case 331: // sysdcon: sysdcon_no_list
#line 1305 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3385 "parser.cc"
    break;

  case 332: // sysdcon: "[" "]"
#line 1306 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 3391 "parser.cc"
    break;

  case 333: // conop: consym
#line 1308 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3397 "parser.cc"
    break;

  case 334: // conop: "`" conid "`"
#line 1309 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3403 "parser.cc"
    break;

  case 335: // qconop: qconsym
#line 1311 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3409 "parser.cc"
    break;

  case 336: // qconop: "`" qconid "`"
#line 1312 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3415 "parser.cc"
    break;

  case 337: // gtycon: ntgtycon
#line 1315 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3421 "parser.cc"
    break;

  case 338: // gtycon: "(" ")"
#line 1316 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 3427 "parser.cc"
    break;

  case 339: // gtycon: "(#" "#)"
#line 1317 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 3433 "parser.cc"
    break;

  case 340: // ntgtycon: oqtycon
#line 1319 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3439 "parser.cc"
    break;

  case 341: // ntgtycon: "(" commas ")"
#line 1320 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3445 "parser.cc"
    break;

  case 342: // ntgtycon: "(#" commas "#)"
#line 1321 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3451 "parser.cc"
    break;

  case 343: // ntgtycon: "(" "->" ")"
#line 1322 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 3457 "parser.cc"
    break;

  case 344: // ntgtycon: "[" "]"
#line 1323 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 3463 "parser.cc"
    break;

  case 345: // oqtycon: qtycon
#line 1325 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3469 "parser.cc"
    break;

  case 346: // oqtycon: "(" qtyconsym ")"
#line 1326 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3475 "parser.cc"
    break;

  case 347: // oqtycon: "(" "~" ")"
#line 1327 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3481 "parser.cc"
    break;

  case 348: // oqtycon_no_varcon: qtycon
#line 1329 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3487 "parser.cc"
    break;

  case 349: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1330 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3493 "parser.cc"
    break;

  case 350: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1331 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3499 "parser.cc"
    break;

  case 351: // oqtycon_no_varcon: "(" ":" ")"
#line 1332 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 3505 "parser.cc"
    break;

  case 352: // oqtycon_no_varcon: "(" "~" ")"
#line 1333 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3511 "parser.cc"
    break;

  case 353: // qtyconop: qtyconsym
#line 1336 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3517 "parser.cc"
    break;

  case 354: // qtyconop: "`" qtycon "`"
#line 1337 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3523 "parser.cc"
    break;

  case 355: // qtycondoc: qtycon
#line 1339 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3529 "parser.cc"
    break;

  case 356: // qtycon: "QCONID"
#line 1341 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3535 "parser.cc"
    break;

  case 357: // qtycon: tycon
#line 1342 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3541 "parser.cc"
    break;

  case 358: // tycon: "CONID"
#line 1346 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3547 "parser.cc"
    break;

  case 359: // qtyconsym: "QCONSYM"
#line 1348 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3553 "parser.cc"
    break;

  case 360: // qtyconsym: "QVARSYM"
#line 1349 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3559 "parser.cc"
    break;

  case 361: // qtyconsym: tyconsym
#line 1350 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3565 "parser.cc"
    break;

  case 362: // tyconsym: "CONSYM"
#line 1352 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3571 "parser.cc"
    break;

  case 363: // tyconsym: "VARSYM"
#line 1353 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3577 "parser.cc"
    break;

  case 364: // tyconsym: ":"
#line 1354 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 3583 "parser.cc"
    break;

  case 365: // tyconsym: "-"
#line 1355 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 3589 "parser.cc"
    break;

  case 366: // op: varop
#line 1360 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3595 "parser.cc"
    break;

  case 367: // op: conop
#line 1361 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3601 "parser.cc"
    break;

  case 368: // varop: varsym
#line 1363 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3607 "parser.cc"
    break;

  case 369: // varop: "`" varid "`"
#line 1364 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3613 "parser.cc"
    break;

  case 370: // qop: qvarop
#line 1366 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3619 "parser.cc"
    break;

  case 371: // qop: qconop
#line 1367 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3625 "parser.cc"
    break;

  case 372: // qop: hole_op
#line 1368 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3631 "parser.cc"
    break;

  case 373: // qopm: qvaropm
#line 1370 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3637 "parser.cc"
    break;

  case 374: // qopm: qconop
#line 1371 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3643 "parser.cc"
    break;

  case 375: // qopm: hole_op
#line 1372 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3649 "parser.cc"
    break;

  case 376: // hole_op: "`" "_" "`"
#line 1374 "parser.y"
                      { yylhs.value.as < std::string > () = "_"; }
#line 3655 "parser.cc"
    break;

  case 377: // qvarop: qvarsym
#line 1376 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3661 "parser.cc"
    break;

  case 378: // qvarop: "`" qvarid "`"
#line 1377 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3667 "parser.cc"
    break;

  case 379: // qvaropm: qvarsym_no_minus
#line 1379 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 3673 "parser.cc"
    break;

  case 380: // qvaropm: "`" qvarid "`"
#line 1380 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3679 "parser.cc"
    break;

  case 381: // tyvar: tyvarid
#line 1384 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3685 "parser.cc"
    break;

  case 382: // tyvarop: "`" tyvarid "`"
#line 1386 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3691 "parser.cc"
    break;

  case 383: // tyvarid: "VARID"
#line 1388 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3697 "parser.cc"
    break;

  case 384: // tyvarid: special_id
#line 1389 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3703 "parser.cc"
    break;

  case 385: // tyvarid: "unsafe"
#line 1390 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 3709 "parser.cc"
    break;

  case 386: // tyvarid: "safe"
#line 1391 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 3715 "parser.cc"
    break;

  case 387: // tyvarid: "interruptible"
#line 1392 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 3721 "parser.cc"
    break;

  case 388: // var: varid
#line 1395 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3727 "parser.cc"
    break;

  case 389: // var: "(" varsym ")"
#line 1396 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3733 "parser.cc"
    break;

  case 390: // qvar: qvarid
#line 1398 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3739 "parser.cc"
    break;

  case 391: // qvar: "(" varsym ")"
#line 1399 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3745 "parser.cc"
    break;

  case 392: // qvar: "(" qvarsym1 ")"
#line 1400 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3751 "parser.cc"
    break;

  case 393: // qvarid: varid
#line 1402 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3757 "parser.cc"
    break;

  case 394: // qvarid: "QVARID"
#line 1403 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3763 "parser.cc"
    break;

  case 395: // varid: "VARID"
#line 1405 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3769 "parser.cc"
    break;

  case 396: // varid: special_id
#line 1406 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3775 "parser.cc"
    break;

  case 397: // varid: "unsafe"
#line 1407 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 3781 "parser.cc"
    break;

  case 398: // varid: "safe"
#line 1408 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 3787 "parser.cc"
    break;

  case 399: // varid: "interruptible"
#line 1409 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 3793 "parser.cc"
    break;

  case 400: // varid: "forall"
#line 1410 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 3799 "parser.cc"
    break;

  case 401: // varid: "family"
#line 1411 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 3805 "parser.cc"
    break;

  case 402: // varid: "role"
#line 1412 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 3811 "parser.cc"
    break;

  case 403: // qvarsym: varsym
#line 1414 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3817 "parser.cc"
    break;

  case 404: // qvarsym: qvarsym1
#line 1415 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3823 "parser.cc"
    break;

  case 405: // qvarsym_no_minus: varsym_no_minus
#line 1417 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3829 "parser.cc"
    break;

  case 406: // qvarsym_no_minus: qvarsym1
#line 1418 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3835 "parser.cc"
    break;

  case 407: // qvarsym1: "QVARSYM"
#line 1420 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3841 "parser.cc"
    break;

  case 408: // varsym: varsym_no_minus
#line 1422 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3847 "parser.cc"
    break;

  case 409: // varsym: "-"
#line 1423 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 3853 "parser.cc"
    break;

  case 410: // varsym_no_minus: "VARSYM"
#line 1425 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3859 "parser.cc"
    break;

  case 411: // varsym_no_minus: special_sym
#line 1426 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3865 "parser.cc"
    break;

  case 412: // special_id: "as"
#line 1428 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 3871 "parser.cc"
    break;

  case 413: // special_id: "qualified"
#line 1429 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 3877 "parser.cc"
    break;

  case 414: // special_id: "hiding"
#line 1430 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 3883 "parser.cc"
    break;

  case 415: // special_id: "export"
#line 1431 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 3889 "parser.cc"
    break;

  case 416: // special_id: "label"
#line 1432 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 3895 "parser.cc"
    break;

  case 417: // special_id: "dynamic"
#line 1433 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 3901 "parser.cc"
    break;

  case 418: // special_id: "stdcall"
#line 1434 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 3907 "parser.cc"
    break;

  case 419: // special_id: "ccall"
#line 1435 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 3913 "parser.cc"
    break;

  case 420: // special_id: "capi"
#line 1436 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 3919 "parser.cc"
    break;

  case 421: // special_id: "prim"
#line 1437 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 3925 "parser.cc"
    break;

  case 422: // special_id: "javascript"
#line 1438 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 3931 "parser.cc"
    break;

  case 423: // special_id: "group"
#line 1439 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 3937 "parser.cc"
    break;

  case 424: // special_id: "stock"
#line 1440 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 3943 "parser.cc"
    break;

  case 425: // special_id: "anyclass"
#line 1441 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 3949 "parser.cc"
    break;

  case 426: // special_id: "via"
#line 1442 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 3955 "parser.cc"
    break;

  case 427: // special_id: "unit"
#line 1443 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 3961 "parser.cc"
    break;

  case 428: // special_id: "dependency"
#line 1444 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 3967 "parser.cc"
    break;

  case 429: // special_id: "signature"
#line 1445 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 3973 "parser.cc"
    break;

  case 430: // special_sym: "!"
#line 1447 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 3979 "parser.cc"
    break;

  case 431: // special_sym: "."
#line 1448 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 3985 "parser.cc"
    break;

  case 432: // special_sym: "*"
#line 1449 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 3991 "parser.cc"
    break;

  case 433: // qconid: conid
#line 1453 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3997 "parser.cc"
    break;

  case 434: // qconid: "QCONID"
#line 1454 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4003 "parser.cc"
    break;

  case 435: // conid: "CONID"
#line 1456 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4009 "parser.cc"
    break;

  case 436: // qconsym: consym
#line 1458 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4015 "parser.cc"
    break;

  case 437: // qconsym: "QCONSYM"
#line 1459 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4021 "parser.cc"
    break;

  case 438: // consym: "CONSYM"
#line 1461 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4027 "parser.cc"
    break;

  case 439: // consym: ":"
#line 1462 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4033 "parser.cc"
    break;

  case 440: // literal: "CHAR"
#line 1466 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < char > ();}
#line 4039 "parser.cc"
    break;

  case 441: // literal: "STRING"
#line 1467 "parser.y"
                  {yylhs.value.as < expression_ref > () = yy_make_string(yystack_[0].value.as < std::string > ());}
#line 4045 "parser.cc"
    break;

  case 442: // literal: "INTEGER"
#line 1468 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < int > ();}
#line 4051 "parser.cc"
    break;

  case 443: // literal: "RATIONAL"
#line 1469 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < double > ();}
#line 4057 "parser.cc"
    break;

  case 445: // close: error
#line 1477 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 4063 "parser.cc"
    break;

  case 446: // modid: "CONID"
#line 1481 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4069 "parser.cc"
    break;

  case 447: // modid: "QCONID"
#line 1482 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4075 "parser.cc"
    break;

  case 448: // commas: commas ","
#line 1484 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 4081 "parser.cc"
    break;

  case 449: // commas: ","
#line 1485 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 4087 "parser.cc"
    break;


#line 4091 "parser.cc"

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


  const short parser::yypact_ninf_ = -582;

  const short parser::yytable_ninf_ = -409;

  const short
  parser::yypact_[] =
  {
      49,   137,  -582,    72,  -582,  -582,  -582,  -582,  -582,   205,
     -13,    15,  -582,    51,   -23,   -23,    29,  -582,  -582,  -582,
    -582,   161,  -582,  -582,  -582,    60,  -582,   160,   166,  3825,
     262,   237,   193,  -582,   644,  -582,   -18,  -582,  -582,  -582,
    -582,   137,  -582,    54,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,   142,  -582,  -582,  -582,  -582,
     197,   207,  -582,   204,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  -582,   109,   251,   297,  -582,   223,  -582,  2134,  3487,
    -582,   233,   151,  2134,  -582,  -582,  -582,   283,   218,  -582,
    3487,  4146,   151,  2981,   248,   225,  4101,   192,  2618,  2981,
    2739,  2981,  1152,  1024,    88,  -582,  -582,  -582,  -582,  -582,
    -582,    47,   248,   230,   193,  -582,  -582,  -582,   290,  -582,
    -582,   436,  -582,  2860,  -582,   264,  -582,  -582,  -582,  -582,
    -582,   256,   281,   263,  -582,  -582,  -582,  -582,   250,  -582,
     214,  -582,  -582,   271,   272,  -582,  -582,  -582,  -582,  -582,
     273,  -582,   274,   279,   288,  -582,  -582,  -582,  3825,  3858,
    -582,  -582,  -582,  -582,  -582,  -582,   365,  -582,   -35,  1024,
     368,   293,  -582,  -582,  2134,  -582,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,  4268,  3184,  3083,   284,  4009,  -582,  -582,
    -582,  -582,  -582,   370,  3917,  -582,   311,  -582,    70,  3487,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  3285,  1529,  1529,  -582,   289,   332,   334,   341,   342,
    3285,   773,   773,  -582,   407,   348,   337,   157,  4325,   301,
     303,  -582,  -582,  -582,  -582,   -21,  4101,  -582,   358,   154,
      -9,   333,    -8,   322,   359,  -582,  -582,  2981,  -582,  -582,
    2497,  -582,  2860,   101,  -582,  -582,  3588,  -582,  -582,  -582,
     293,    85,   339,   327,  -582,  2134,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,  2739,  -582,  -582,   -20,    75,   279,   338,
     340,   346,    78,  -582,   150,  3285,  4101,  4101,  -582,   336,
     223,   316,  3487,  3285,  3588,   101,  -582,  2376,  -582,  -582,
    -582,  -582,  -582,  3917,  -582,  4042,  4268,  2981,  -582,   349,
     350,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,   352,
     331,  -582,  -582,   347,    51,  -582,   321,   373,   385,   245,
    3285,  2134,  -582,     1,   363,   351,  -582,  -582,  -582,  -582,
     366,   380,  -582,   364,   349,  -582,    -4,   357,   350,    93,
     182,   360,   361,   218,  -582,  -582,  3487,  3285,  -582,  -582,
     369,   371,   218,   151,  2981,   389,   393,   -10,  -582,  -582,
      52,  -582,   458,  -582,  -582,  -582,  -582,  -582,  -582,   370,
      74,  -582,  -582,   625,    53,  2134,  3285,   376,   372,   374,
     377,   362,   390,   412,  -582,  -582,   424,   396,   192,   217,
     429,  -582,  2134,  -582,  2134,  1892,  -582,    32,  -582,   398,
     399,   404,  2134,  2134,  1650,  1280,  -582,  1280,   754,  -582,
    1280,  -582,  1280,   408,  -582,  -582,  -582,  -582,   444,   445,
     450,  4235,   410,  -582,  -582,  -582,  -582,     6,   177,  -582,
    -582,   119,  -582,   416,  -582,  -582,  -582,  -582,   437,  -582,
     422,   457,    80,  -582,  -582,  -582,  -582,  3858,  -582,  -582,
    -582,   137,  -582,  -582,  1408,   903,  -582,  -582,  -582,  3285,
    4268,  -582,  4268,  4358,  -582,  3285,  -582,  3285,  -582,  3285,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  3285,   407,
    -582,  -582,  2134,  -582,  1529,  -582,  2134,  -582,  -582,   773,
    -582,  -582,  -582,  -582,  -582,  -582,   411,   414,   439,  -582,
    -582,  -582,  -582,  -582,   442,   421,   184,  -582,  -582,  -582,
    -582,   370,   466,   435,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  2255,   438,  -582,  -582,   475,  -582,  -582,  -582,  -582,
    -582,  3285,  3285,   443,   336,  -582,   477,  3285,   528,  -582,
     552,  -582,  -582,  4042,  1280,  3285,   451,   564,  2981,  -582,
    1771,  -582,   470,   460,  -582,   152,    51,  -582,  -582,  -582,
    -582,  3285,  4417,  -582,  -582,  -582,  -582,   463,   468,  -582,
    -582,  -582,   289,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    2134,  1892,   231,   526,  1650,  2134,  -582,    66,    67,  -582,
    -582,  -582,  -582,  -582,   493,  -582,  3917,    40,  -582,   552,
    -582,  -582,  -582,  -582,  -582,   137,    44,  -582,  -582,  -582,
    2013,  1892,  2134,  -582,    46,  -582,  -582,  -582,   498,  -582,
    -582,   569,  -582,  -582,  2134,  2134,  2134,  -582,  -582,  -582,
    -582,  3285,  -582,  4384,   528,   492,  3633,  -582,  -582,  -582,
    -582,  -582,  -582,  3386,   113,   529,  -582,  -582,  -582,  -582,
     476,  3825,  -582,  -582,  -582,   499,   370,  -582,  -582,  3285,
    2134,   535,  -582,  -582,  -582,  -582,  -582,  3917,   469,  -582,
    3917,  -582,  -582,   472,   483,  -582,  3487,  -582,  3825,   484,
    2134,  -582,   485,  -582,  2134,  3725,  -582,  3917,  3487,  -582,
    -582,   486,  -582,  -582,  -582,  -582,  -582,  -582
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    39,     0,     2,    39,     4,   446,   447,     8,
       0,    42,     1,     0,     0,     0,    18,    11,    38,    13,
      16,    67,   445,   444,    12,   114,   110,     0,     0,     0,
       0,    45,    40,    15,    14,   113,     0,     6,     7,   412,
     414,     0,   413,     0,   400,   415,   416,   417,   398,   399,
     397,   401,   402,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,   429,   428,     0,   395,   358,   394,   356,
       0,    19,    21,    24,    32,    35,   348,   357,    34,   390,
     393,   396,     0,     0,    47,    37,    41,   251,     0,     0,
      92,     0,     0,     0,    60,    61,    62,    87,     0,    93,
       0,     0,     0,     0,   211,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   435,   434,   440,   441,   442,
     443,   211,   211,    58,    65,    68,    69,    70,   100,    72,
     192,    73,   219,   223,   233,   242,   244,   246,   318,   331,
     319,     0,   245,   393,   320,   433,   247,   111,     0,    23,
       0,    33,   345,     0,     0,   409,   430,   432,   431,   410,
       0,   407,     0,     0,     0,   408,   411,    17,     0,    26,
      22,    39,    39,     3,    44,    46,    51,    36,     0,     0,
       0,   216,   224,   217,     0,   386,   387,   385,   364,   127,
     365,   126,   148,   177,     0,     0,     0,     0,   383,   363,
     362,   360,   359,   109,     0,   125,     0,    97,   134,   137,
     140,   142,   146,   340,   143,   353,   361,   147,   144,   381,
     384,   160,   305,   305,   240,   227,     0,     0,     0,     0,
       0,   104,   104,   107,     0,     0,   134,     0,     0,     0,
       0,   388,   368,   241,   232,     0,     0,   212,     0,     0,
       0,     0,     0,   325,   117,   324,   322,     0,   296,   299,
       0,   235,   221,     0,   439,   332,     0,   438,   437,   252,
     216,   257,     0,   258,   374,     0,   375,   373,   379,   406,
     405,   335,   436,   409,   327,   449,     0,     0,   406,     0,
     405,   335,     0,   329,     0,     0,     0,     0,    59,     0,
      66,     0,     0,     0,     0,     0,   371,     0,   372,   370,
     377,   404,   403,     0,   230,   312,     0,     0,   112,     0,
       0,   351,   352,   350,   349,   392,   391,    20,    31,     0,
      27,    29,    30,     0,     0,    50,    49,     0,     0,     0,
       0,     0,   225,     0,     0,   178,   180,   122,   164,   344,
       0,     0,   130,     0,   127,   151,   161,     0,   353,     0,
       0,     0,     0,     0,    78,   149,     0,     0,   141,   161,
       0,   159,     0,     0,     0,   309,     0,     0,   304,   306,
       0,   226,     0,    84,    83,    85,    86,   156,   119,   109,
       0,   195,   103,   115,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   222,   206,     0,     0,     0,     0,
       0,   297,     0,   298,     0,     0,   193,   109,   199,     0,
       0,     0,   253,   259,     0,     0,   250,     0,   254,   248,
       0,   249,     0,   391,   321,   328,   448,   330,     0,     0,
       0,     0,   203,   367,    64,   366,   333,     0,    94,   116,
     201,   131,   120,     0,   194,   220,   231,   315,     0,   311,
     314,   317,     0,   234,   347,   346,    25,     0,     9,    10,
      48,     0,   229,   228,     0,     0,   239,   215,   218,     0,
       0,   150,     0,     0,   153,     0,   343,     0,   154,     0,
     341,   342,   354,   382,   108,    96,   135,    71,     0,   310,
     307,   295,     0,   300,   303,   301,     0,    82,   105,   102,
     106,   237,   131,    79,   389,   369,    77,    75,     0,   213,
     205,   207,   323,   326,     0,     0,     0,   118,   337,   204,
     236,   109,     0,   274,   276,   197,   198,   376,   380,   336,
     261,     0,   263,   268,   269,   252,   265,   264,   256,   255,
     210,     0,     0,     0,     0,    99,     0,     0,   174,    81,
     182,   378,   243,     0,     0,     0,     0,    53,     0,   279,
       0,   292,     0,   281,   285,     0,     0,   280,   181,   121,
     179,     0,     0,   163,   165,   129,   167,     0,   162,   162,
     308,   302,   227,   101,    76,    74,   214,   338,   339,   196,
       0,     0,   423,   270,     0,   260,   123,     0,     0,   334,
      63,    98,    95,   164,   168,   170,     0,     0,    80,   183,
     185,   313,   316,   202,    28,     0,    55,   293,   282,   277,
     284,     0,     0,   286,   109,   290,   278,   128,     0,   155,
     152,     0,   200,   275,     0,     0,     0,   266,   267,   262,
     208,     0,   209,     0,   174,     0,   175,   138,   145,   172,
      90,    88,    89,     0,     0,   186,   189,   355,   184,    52,
       0,     0,    43,    54,   283,     0,   109,   288,   289,     0,
       0,     0,   272,   271,   124,   173,   169,     0,     0,   139,
       0,   190,   136,   157,     0,   187,     0,   188,     0,     0,
       0,   287,     0,   238,     0,   175,   171,   176,     0,   191,
      91,     0,    56,   291,   166,   273,   158,    57
  };

  const short
  parser::yypgoto_[] =
  {
    -582,  -582,  -582,  -582,  -582,  -582,  -582,    33,  -582,  -582,
    -514,  -582,   428,  -582,  -582,  -582,   130,  -152,  -582,   478,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,   298,  -582,   375,  -582,  -274,
    -353,   586,  -582,  -582,  -582,  -277,    39,   294,    57,  -582,
    -582,  -171,   209,   -60,  -582,   -88,  -582,   -97,  -463,  -582,
     405,  -572,  -195,   318,   -93,  -582,   395,     4,  -582,  -515,
    -582,  -582,   -28,  -582,   -68,  -582,  -582,   143,  -582,  -582,
       8,   -36,   595,   121,   326,  -582,   215,  -582,   254,  -582,
     -82,   -78,   599,   -19,  -296,    42,  -582,   -77,   -89,  -582,
    -582,   -61,  -582,  -582,  -582,  -582,    31,     5,  -582,  -582,
    -418,  -582,     9,  -582,  -582,     7,  -582,  -582,   378,  -582,
     -70,   417,   139,  -378,  -582,    82,  -582,  -582,  -582,  -582,
     238,  -582,   -86,  -581,  -121,  -582,   244,   612,  -582,  -582,
    -582,   -27,  -582,  -137,  -582,   104,   558,  -141,  -582,  -103,
    -582,  -582,  -444,  -582,   495,   -76,   -29,  -205,   -16,  -582,
    -582,    14,   -58,   -65,   -84,  -582,  -197,   -99,   -48,  -225,
    -582,  -285,   -37,   -73
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   173,     6,    10,    19,    30,
      70,    71,    72,   170,   329,   330,   331,    73,    74,    86,
      11,    20,    21,    32,    84,   176,   471,   336,   626,   672,
     673,   299,   123,   442,    33,    34,   124,   125,   126,   127,
     230,   664,   697,   128,   559,   203,   302,   390,   233,   234,
     364,    27,    36,   305,   410,   387,   450,   343,   607,   204,
     205,   388,   452,   351,   655,   352,   693,   208,   656,   209,
     210,   657,   211,   389,   694,   370,   357,   483,   583,   587,
     560,   614,   615,   616,   659,   344,   345,   346,   618,   619,
     620,   665,   391,   392,   416,   417,   418,   130,   246,   247,
     375,   181,   393,   182,   183,   382,   184,   133,   134,   135,
     136,   286,   287,   272,   273,   542,   543,   532,   533,   476,
     572,   573,   574,   633,   634,   635,   575,   376,   259,   260,
     224,   377,   378,   379,   458,   459,   460,   137,   138,   253,
     254,   139,   140,   443,   274,   527,   212,   213,    75,   214,
     666,   152,    77,   215,   216,   444,   445,   307,   275,   276,
     309,   277,   217,   218,   219,   141,   142,    79,    80,   310,
     278,   279,   312,   165,    81,   166,   144,   145,   281,   282,
     146,    24,     9,   292
  };

  const short
  parser::yytable_[] =
  {
      78,   207,    76,   236,   149,   220,   180,   164,   256,   365,
     306,   225,   235,   320,   244,   132,   220,   332,   143,   258,
     261,   255,   263,   350,   356,   240,   449,   407,   308,   206,
     269,   269,   243,   262,   270,   270,   507,   534,    13,   584,
     341,   294,   612,   242,   314,   478,   544,   280,   290,   469,
     369,   271,    22,    22,    22,   289,   670,   576,   358,   363,
     306,   420,   660,   477,   535,   291,   295,   401,   405,   421,
       1,   264,    12,   363,   446,   690,   487,   250,   308,   163,
      25,   479,   337,   555,   689,   241,   147,    17,   429,   494,
     503,   661,   662,   338,   430,   505,   148,   269,   499,   453,
     284,   270,   342,   504,   488,    26,   285,   421,   402,   510,
     220,   220,   267,   220,   290,   480,   415,   347,   456,   406,
     220,   164,   359,   360,   690,   220,   478,   288,    18,   422,
     631,   291,   451,   689,   556,   689,    29,   220,   638,    78,
      78,    76,    76,   650,   652,   311,   220,   663,     2,   306,
     245,   671,   628,    23,    23,    23,   367,   699,    67,  -132,
     565,   150,    69,   423,   702,   504,   509,   308,   411,   424,
     361,   258,    67,   314,   508,    31,    69,   241,   599,   397,
     651,   651,   414,   431,   711,   415,   435,   509,    35,   432,
     577,   164,   436,   288,   480,   311,   496,   428,   293,   425,
    -119,   490,   285,  -119,   333,   334,   262,   436,   171,   584,
     172,   220,   132,   132,   207,   143,   143,   403,   220,   220,
     663,   153,   398,   643,   705,   512,   648,   707,   463,   220,
     154,    67,   155,   156,   157,    69,   631,    37,   632,   158,
     347,   242,   206,    38,   155,   156,   157,   155,   156,   157,
     222,   158,   223,   534,   158,     7,   220,   557,   558,     8,
     437,   159,   160,   163,   436,   161,   162,   439,   440,   236,
      14,    15,   306,   159,   606,   606,   159,   161,   495,   644,
     645,   677,   220,   220,   311,   501,   461,   341,   455,    82,
     308,   636,   491,   188,   598,   251,   436,    83,   285,   252,
     241,   114,   319,   500,   190,   167,    85,   306,   578,   256,
     115,   169,   220,   511,   585,   332,   586,   231,   588,   232,
     524,   168,   255,   701,   525,   308,   526,   589,   174,   446,
     530,   175,   531,   199,   200,    67,   177,   201,   202,    69,
     221,   540,   553,   545,   474,   269,   475,   270,   269,   270,
     269,   245,   270,   248,   270,   226,   227,   228,   229,   298,
     280,   301,   280,   315,   546,   280,   547,   280,   317,   548,
     316,   549,   264,   340,   684,   296,   297,  -388,   318,   321,
     322,   323,   324,   155,   156,   157,   586,   325,   320,   335,
     158,   339,   571,   571,   512,   220,   326,   363,   285,   220,
     366,   220,   381,   220,   579,   220,   347,   311,   304,   383,
     637,   384,   159,   267,   220,   264,   161,   268,   385,   386,
     590,   658,   395,   367,   592,   398,   155,   156,   157,   396,
     399,   446,   400,   158,   567,   404,   408,   265,    78,   409,
      76,   427,   311,   426,   447,   467,   433,   468,  -408,   470,
     472,   441,   359,   360,   434,   159,   267,   464,   465,   603,
     466,   658,   473,   481,   241,   482,   241,   220,   220,   485,
     484,   489,   486,   220,  -294,   492,   493,   497,   502,   627,
     446,   220,   269,   506,   514,   498,   270,   515,   571,   520,
     132,   518,   658,   143,   519,   658,   242,   220,   220,   280,
     188,   521,   516,   622,   522,   517,   529,   353,   586,   319,
     658,   190,   658,   537,   538,   264,   303,  -115,   642,   539,
    -115,   550,  -389,   649,   554,   551,   155,   156,   157,   597,
     552,   561,   220,   158,   461,   285,   563,   562,   564,   594,
     199,   200,   595,   596,   201,   202,   349,   600,   571,   601,
     676,   304,   604,   605,   611,   159,   267,   613,   609,   161,
     268,   617,   681,   682,   683,   624,   236,   220,   625,   220,
     629,   639,   220,   630,   646,   692,   640,   654,   679,   220,
     680,   687,   696,   698,   704,   700,   708,   115,   669,   553,
     667,   709,   712,   714,   717,   220,   327,   566,   703,   236,
     448,    28,   300,   220,   623,   513,   220,   394,   710,   608,
     462,   236,   220,   438,   368,   716,   371,   653,   713,   706,
     692,   220,   715,   220,   220,   580,   686,   668,   695,   129,
     593,   454,   536,   131,   641,   647,   675,   667,   413,   674,
     380,   678,    78,   591,    76,   621,   523,    87,    39,    88,
      89,    90,    91,   528,    92,   151,    40,    93,   610,   239,
      94,    95,    96,    97,    98,     0,    99,     0,    42,    78,
     100,    76,   101,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,   362,     0,   103,    59,    60,    61,    62,    63,
      64,   104,     0,     0,   264,   303,   105,   106,     0,     0,
       0,     0,     0,     0,     0,   155,   156,   157,     0,     0,
     107,     0,   158,     0,     0,     0,   108,     0,     0,     0,
       0,     0,   109,     0,   110,   111,     0,     0,     0,     0,
     304,     0,     0,     0,   159,   267,     0,   112,   161,   268,
       0,   113,     0,   114,     0,     0,     0,     0,     0,     0,
       0,    66,   115,     0,     0,    68,   116,     0,     0,     0,
       0,   117,   118,   119,   120,     0,    87,    39,    88,     0,
       0,   121,   122,    92,     0,    40,    93,     0,     0,    94,
      95,    96,     0,    98,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,   103,    59,    60,    61,    62,    63,    64,
     104,     0,     0,   264,     0,   105,   106,     0,     0,     0,
       0,     0,     0,     0,   155,   156,   157,     0,     0,   107,
       0,   158,     0,     0,     0,   108,     0,     0,     0,     0,
       0,   109,     0,   110,   111,     0,     0,     0,     0,   304,
       0,     0,     0,   159,   267,     0,   112,   161,   268,     0,
     113,     0,   114,     0,     0,     0,     0,     0,     0,     0,
      66,   115,     0,     0,    68,   116,     0,     0,     0,     0,
     117,   118,   119,   120,    22,     0,    87,    39,    88,     0,
     121,   122,     0,    92,     0,    40,    93,     0,     0,     0,
       0,     0,     0,    98,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,   103,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,   105,   178,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   108,     0,     0,     0,     0,
       0,   109,     0,   110,   568,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    23,   112,     0,     0,     0,
     179,     0,   114,     0,     0,     0,   570,     0,     0,     0,
      66,   115,     0,     0,    68,   116,     0,    87,    39,    88,
     117,   118,   119,   120,    92,     0,    40,    93,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,   103,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,   105,   178,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   264,     0,     0,   108,     0,     0,     0,
       0,     0,   109,     0,   283,   156,   157,     0,     0,     0,
       0,   158,     0,     0,     0,     0,     0,   112,     0,     0,
       0,   179,   284,   114,     0,     0,     0,     0,   285,   266,
       0,    66,   115,   159,   267,    68,   116,   161,   268,     0,
       0,   117,   118,   119,   120,    87,    39,    88,     0,     0,
       0,     0,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,   103,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,   105,   178,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   264,     0,     0,   108,     0,     0,     0,     0,     0,
     109,     0,   110,   156,   157,     0,     0,     0,     0,   158,
       0,     0,     0,     0,     0,   112,   265,     0,     0,   179,
       0,   114,     0,     0,     0,     0,     0,   266,     0,    66,
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
       0,     0,     0,   112,     0,     0,     0,   179,     0,   114,
       0,     0,     0,     0,     0,   266,     0,    66,   115,   159,
     267,    68,   116,   161,   268,     0,     0,   117,   118,   119,
     120,    87,    39,    88,     0,     0,     0,     0,    92,     0,
      40,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,   103,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
     105,   178,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     108,     0,     0,     0,     0,     0,   109,     0,   110,   568,
       0,     0,     0,     0,     0,     0,     0,     0,   569,     0,
       0,   112,     0,     0,     0,   179,     0,   114,     0,     0,
       0,   570,     0,     0,     0,    66,   115,     0,     0,    68,
     116,     0,    87,    39,    88,   117,   118,   119,   120,    92,
       0,    40,    93,     0,     0,     0,     0,     0,     0,   372,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,    55,    56,    57,     0,   373,    58,     0,     0,   103,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,   105,   178,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   108,     0,     0,     0,     0,     0,   109,     0,   110,
     374,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   112,     0,     0,     0,   179,     0,   114,     0,
       0,     0,     0,     0,     0,     0,    66,   115,     0,     0,
      68,   116,     0,    87,    39,    88,   117,   118,   119,   120,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
     372,     0,     0,     0,    42,   541,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
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
       0,   110,   568,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   112,     0,     0,     0,   179,     0,
     114,     0,     0,     0,   570,     0,     0,     0,    66,   115,
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
       0,   109,     0,   110,   568,     0,     0,     0,     0,     0,
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
       0,     0,   602,     0,     0,   103,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,   105,   178,     0,
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
      57,     0,     0,    58,     0,     0,   103,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,   105,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   108,     0,
       0,     0,     0,     0,   109,     0,   110,     0,     0,     0,
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
       0,     0,     0,   412,     0,   109,     0,     0,   257,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     112,     0,     0,     0,   179,     0,   114,     0,     0,     0,
       0,     0,     0,     0,    66,   115,     0,     0,    68,   116,
       0,    87,    39,    88,   117,   118,   119,   120,    92,     0,
      40,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     108,     0,     0,     0,     0,     0,   109,     0,     0,   257,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   112,     0,     0,     0,   179,     0,   114,     0,     0,
       0,     0,     0,     0,     0,    66,   115,     0,     0,    68,
     116,     0,    87,    39,    88,   117,   118,   119,   120,    92,
       0,    40,    93,     0,     0,     0,     0,     0,     0,    98,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,   103,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   108,     0,     0,     0,     0,     0,   109,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,   313,     0,
       0,     0,     0,   112,     0,     0,     0,   179,     0,   114,
       0,     0,     0,     0,     0,     0,     0,    66,   115,     0,
       0,    68,   116,     0,    87,    39,    88,   117,   118,   119,
     120,    92,     0,    40,    93,     0,     0,     0,     0,     0,
       0,    98,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   108,     0,     0,     0,     0,     0,   109,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   112,     0,     0,    39,   179,     0,
     114,     0,     0,     0,     0,    40,     0,     0,    66,   115,
       0,     0,    68,   116,     0,     0,     0,    42,   117,   118,
     119,   120,   348,     0,    45,    46,    47,   185,   186,   187,
       0,     0,     0,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,     0,     0,     0,     0,     0,     0,   353,
       0,   354,     0,   190,   191,   192,     0,     0,     0,     0,
       0,     0,   193,     0,     0,     0,   194,     0,    39,     0,
     195,   355,   196,     0,     0,     0,    40,   285,   197,     0,
     198,    67,   199,   200,     0,    69,   201,   202,    42,     0,
       0,     0,     0,   348,     0,    45,    46,    47,   185,   186,
     187,     0,     0,     0,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,     0,     0,     0,     0,     0,     0,
       0,     0,   189,     0,   190,   191,   192,     0,     0,     0,
       0,     0,     0,   193,     0,     0,     0,   194,   349,    39,
       0,   195,     0,   196,     0,     0,     0,    40,     0,   197,
       0,   198,    67,   199,   200,     0,    69,   201,   202,    42,
       0,     0,     0,     0,   348,     0,    45,    46,    47,   185,
     186,   187,     0,     0,     0,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,     0,     0,     0,     0,     0,
       0,     0,     0,   189,     0,   190,   191,   192,     0,     0,
       0,     0,     0,     0,   193,     0,     0,     0,   194,     0,
      39,     0,   195,     0,   196,     0,     0,     0,    40,     0,
     197,     0,   198,    67,   199,   200,     0,    69,   201,   202,
      42,     0,     0,     0,     0,     0,     0,    45,    46,    47,
     185,   186,   187,     0,     0,     0,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,     0,     0,     0,     0,
       0,     0,     0,     0,   189,     0,   190,   191,   192,     0,
       0,     0,     0,     0,     0,   193,     0,     0,     0,   194,
       0,    39,     0,   195,   691,   196,     0,     0,     0,    40,
       0,   197,     0,   198,    67,   199,   200,     0,    69,   201,
     202,    42,     0,     0,     0,     0,     0,     0,    45,    46,
      47,   185,   186,   187,     0,     0,     0,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,     0,     0,     0,
       0,     0,     0,     0,     0,   189,     0,   190,   191,   192,
       0,     0,     0,     0,     0,     0,   193,     0,     0,     0,
     194,   419,    39,     0,   195,     0,   196,     0,     0,     0,
      40,     0,   197,     0,   198,    67,   199,   200,     0,    69,
     201,   202,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,    39,     0,    59,
      60,    61,    62,    63,    64,    40,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    42,     0,     0,
       0,     0,     0,     0,    45,    46,    47,   185,   186,   187,
       0,     0,     0,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    66,   115,     0,     0,    68,
     116,     0,   264,     0,     0,     0,     0,     0,     0,     0,
       0,   189,  -133,     0,   191,   192,     0,     0,     0,    39,
       0,     0,   193,     0,     0,     0,   194,    40,     0,     0,
     195,     0,   196,     0,     0,     0,     0,     0,   688,    42,
     198,    67,     0,   267,     0,    69,    45,    46,    47,   185,
     186,   187,     0,     0,     0,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   264,     0,     0,     0,     0,     0,
       0,     0,     0,   189,     0,     0,   191,   192,     0,     0,
       0,     0,     0,     0,   193,     0,     0,     0,   194,    39,
       0,     0,   195,     0,   196,     0,     0,    40,     0,     0,
     688,     0,   198,    67,     0,   267,    41,    69,     0,    42,
       0,    43,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,    39,    51,    52,    53,    54,    55,    56,    57,
      40,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,    42,     0,    43,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,    39,     0,     0,     0,     0,     0,     0,     0,    40,
       0,     0,    65,     0,     0,     0,   328,     0,     0,     0,
       0,    42,    66,    67,     0,     0,    68,    69,    45,    46,
      47,   185,   186,   187,     0,     0,     0,    53,    54,    55,
      56,    57,     0,     0,    58,    65,     0,     0,    59,    60,
      61,    62,    63,    64,     0,    66,    67,     0,     0,    68,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   189,     0,     0,   191,   192,
       0,     0,     0,    39,     0,     0,   193,     0,     0,     0,
     194,    40,     0,     0,   195,     0,   196,     0,     0,     0,
       0,     0,     0,    42,   198,    67,     0,     0,     0,    69,
      45,    46,    47,   185,   186,   187,    39,     0,     0,    53,
      54,    55,    56,    57,    40,     0,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,    39,     0,     0,     0,     0,
       0,     0,     0,    40,     0,     0,     0,     0,     0,     0,
     457,     0,     0,     0,     0,    42,   198,    67,     0,     0,
      44,    69,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,   249,
      39,     0,    59,    60,    61,    62,    63,    64,    40,    66,
       0,     0,     0,    68,     0,     0,     0,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,     0,    51,    52,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,   249,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    66,     0,
       0,     0,    68,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   155,   156,   157,    39,
       0,     0,     0,   158,     0,     0,     0,    40,     0,     0,
       0,     0,     0,   237,     0,     0,     0,     0,     0,    42,
       0,   238,     0,    66,    44,   159,    45,    46,    47,    48,
      49,    50,    39,    51,    52,    53,    54,    55,    56,    57,
      40,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,    39,
       0,     0,     0,     0,     0,     0,     0,    40,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    42,
       0,     0,    66,   115,    44,     0,    45,    46,    47,    48,
      49,    50,    39,    51,    52,    53,    54,    55,    56,    57,
      40,     0,    58,     0,     0,   237,    59,    60,    61,    62,
      63,    64,    42,     0,     0,    66,     0,     0,    39,    45,
      46,    47,   185,   186,   187,     0,    40,     0,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,    42,    59,
      60,    61,    62,    63,    64,    45,    46,    47,   185,   186,
     187,    39,     0,     0,    53,    54,    55,    56,    57,    40,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,    42,    66,     0,     0,     0,     0,     0,    45,    46,
      47,   185,   186,   187,     0,   581,     0,    53,    54,    55,
      56,    57,     0,     0,    58,   582,     0,     0,    59,    60,
      61,    62,    63,    64,     0,   198,     0,     0,     0,     0,
       0,   685,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   582,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   198,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   198
  };

  const short
  parser::yycheck_[] =
  {
      29,    89,    29,   100,    41,    89,    88,    65,   107,   204,
     131,    93,   100,   150,   103,    34,   100,   169,    34,   108,
     109,   107,   111,   194,   195,   101,   303,   252,   131,    89,
     112,   113,   102,   110,   112,   113,   389,   415,     5,   483,
     181,   114,   557,   101,   133,   341,   424,   112,   113,   334,
     221,   112,     1,     1,     1,   113,    12,   475,   195,    27,
     181,   266,    22,   340,   417,   113,    19,    88,    77,   266,
      21,    79,     0,    27,   299,   656,    80,   106,   181,    65,
     103,    80,   117,    77,   656,   101,   104,   100,   108,   363,
     100,    51,    52,   128,   114,   380,   114,   179,   372,   304,
     108,   179,   184,   113,   108,   128,   114,   304,   129,   394,
     194,   195,   120,   197,   179,   114,    84,   193,   313,   128,
     204,   179,   195,   196,   705,   209,   422,   113,   113,   270,
      84,   179,   303,   705,   128,   707,   107,   221,   582,   168,
     169,   168,   169,    77,    77,   131,   230,   107,    99,   270,
     103,   107,   570,   102,   102,   102,    86,   671,   118,    89,
      80,   107,   122,    78,   679,   113,   113,   270,   257,    84,
     197,   260,   118,   262,   100,    14,   122,   193,   531,   237,
     114,   114,    81,   108,   698,    84,   108,   113,   128,   114,
     475,   249,   114,   179,   114,   181,   367,   275,   110,   114,
      81,   108,   114,    84,   171,   172,   283,   114,    99,   653,
     101,   295,   231,   232,   302,   231,   232,   246,   302,   303,
     107,    79,   238,   601,   687,   396,   604,   690,   317,   313,
      88,   118,    90,    91,    92,   122,    84,    77,    86,    97,
     316,   299,   302,    77,    90,    91,    92,    90,    91,    92,
      99,    97,   101,   631,    97,   118,   340,    80,    81,   122,
     110,   119,   120,   249,   114,   123,   124,   296,   297,   366,
      65,    66,   393,   119,   551,   552,   119,   123,   366,    48,
      49,   634,   366,   367,   270,   374,   315,   428,   307,    27,
     393,   576,   110,    79,   110,   103,   114,    60,   114,   107,
     316,   109,    88,   373,    90,   108,   113,   428,   479,   408,
     118,   107,   396,   395,   485,   467,   487,    99,   489,   101,
     103,   114,   408,   676,   107,   428,   109,   498,    77,   554,
     412,    34,   414,   119,   120,   118,   113,   123,   124,   122,
     107,   423,   441,   425,    99,   427,   101,   425,   430,   427,
     432,   103,   430,   128,   432,    72,    73,    74,    75,   129,
     425,    71,   427,    99,   425,   430,   427,   432,    87,   430,
     114,   432,    79,    80,   651,   121,   122,   114,   128,   108,
     108,   108,   108,    90,    91,    92,   557,   108,   525,    24,
      97,    23,   474,   475,   565,   479,   108,    27,   114,   483,
      89,   485,   113,   487,   480,   489,   482,   393,   115,    77,
     581,    77,   119,   120,   498,    79,   123,   124,    77,    77,
     502,   616,    15,    86,   506,   441,    90,    91,    92,    81,
     129,   656,   129,    97,   471,    77,   114,   104,   467,    80,
     467,   114,   428,   104,   128,   114,   108,   100,   108,   128,
      77,   115,   525,   526,   108,   119,   120,   108,   108,   541,
     108,   656,    77,   100,   480,   114,   482,   551,   552,    89,
     104,   114,   108,   557,    85,   115,   115,   108,    85,   568,
     705,   565,   564,    25,   108,   114,   564,   115,   570,    77,
     509,   129,   687,   509,   104,   690,   554,   581,   582,   564,
      79,    77,   128,   564,   108,   128,    77,    86,   679,    88,
     705,    90,   707,   115,   115,    79,    80,    81,   600,   115,
      84,    77,   114,   605,   114,    80,    90,    91,    92,   108,
      80,   115,   616,    97,   563,   114,   114,   100,    81,   128,
     119,   120,   128,   104,   123,   124,   104,    81,   630,   114,
     632,   115,   114,    78,    77,   119,   120,    29,   115,   123,
     124,     9,   644,   645,   646,   114,   663,   651,     4,   653,
     100,   108,   656,   113,    48,   663,   108,    84,    80,   663,
      11,    89,    53,   107,    49,    86,   114,   118,   625,   688,
     617,   108,   108,   108,   108,   679,   168,   467,   680,   696,
     302,    15,   124,   687,   565,   396,   690,   232,   696,   552,
     316,   708,   696,   295,   209,   708,   221,   613,   700,   687,
     708,   705,   704,   707,   708,   482,   654,   619,   664,    34,
     509,   305,   417,    34,   592,   604,   631,   664,   260,   630,
     223,   634,   671,   504,   671,   563,   408,     3,     4,     5,
       6,     7,     8,   409,    10,    43,    12,    13,   554,   101,
      16,    17,    18,    19,    20,    -1,    22,    -1,    24,   698,
      26,   698,    28,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,   197,    -1,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    -1,    79,    80,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    91,    92,    -1,    -1,
      76,    -1,    97,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,
     115,    -1,    -1,    -1,   119,   120,    -1,   103,   123,   124,
      -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,
      -1,   127,   128,   129,   130,    -1,     3,     4,     5,    -1,
      -1,   137,   138,    10,    -1,    12,    13,    -1,    -1,    16,
      17,    18,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    -1,    79,    -1,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    91,    92,    -1,    -1,    76,
      -1,    97,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,   115,
      -1,    -1,    -1,   119,   120,    -1,   103,   123,   124,    -1,
     107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,
     127,   128,   129,   130,     1,    -1,     3,     4,     5,    -1,
     137,   138,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   102,   103,    -1,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,   113,    -1,    -1,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,     3,     4,     5,
     127,   128,   129,   130,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,
      -1,    97,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
      -1,   107,   108,   109,    -1,    -1,    -1,    -1,   114,   115,
      -1,   117,   118,   119,   120,   121,   122,   123,   124,    -1,
      -1,   127,   128,   129,   130,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
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
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
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
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
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
      41,    42,    43,    44,    -1,    46,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,     3,     4,     5,   127,   128,   129,   130,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    25,    -1,    -1,    -1,    29,
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
      55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,
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
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,    -1,
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
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    86,    -1,    88,    -1,    -1,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,     3,     4,     5,   127,   128,   129,   130,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,
     122,    -1,     3,     4,     5,   127,   128,   129,   130,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,
      -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,
      -1,   121,   122,    -1,     3,     4,     5,   127,   128,   129,
     130,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,     4,   107,    -1,
     109,    -1,    -1,    -1,    -1,    12,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,    -1,    -1,    24,   127,   128,
     129,   130,    29,    -1,    31,    32,    33,    34,    35,    36,
      -1,    -1,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    86,
      -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,    -1,   103,    -1,     4,    -1,
     107,   108,   109,    -1,    -1,    -1,    12,   114,   115,    -1,
     117,   118,   119,   120,    -1,   122,   123,   124,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    -1,    -1,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,   103,   104,     4,
      -1,   107,    -1,   109,    -1,    -1,    -1,    12,    -1,   115,
      -1,   117,   118,   119,   120,    -1,   122,   123,   124,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    -1,    -1,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,    -1,
       4,    -1,   107,    -1,   109,    -1,    -1,    -1,    12,    -1,
     115,    -1,   117,   118,   119,   120,    -1,   122,   123,   124,
      24,    -1,    -1,    -1,    -1,    -1,    -1,    31,    32,    33,
      34,    35,    36,    -1,    -1,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,
      -1,     4,    -1,   107,   108,   109,    -1,    -1,    -1,    12,
      -1,   115,    -1,   117,   118,   119,   120,    -1,   122,   123,
     124,    24,    -1,    -1,    -1,    -1,    -1,    -1,    31,    32,
      33,    34,    35,    36,    -1,    -1,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,
     103,     3,     4,    -1,   107,    -1,   109,    -1,    -1,    -1,
      12,    -1,   115,    -1,   117,   118,   119,   120,    -1,   122,
     123,   124,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    -1,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,     4,    -1,    51,
      52,    53,    54,    55,    56,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    -1,    -1,    31,    32,    33,    34,    35,    36,
      -1,    -1,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,
     122,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    88,    89,    -1,    91,    92,    -1,    -1,    -1,     4,
      -1,    -1,    99,    -1,    -1,    -1,   103,    12,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,    -1,    -1,   115,    24,
     117,   118,    -1,   120,    -1,   122,    31,    32,    33,    34,
      35,    36,    -1,    -1,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,     4,
      -1,    -1,   107,    -1,   109,    -1,    -1,    12,    -1,    -1,
     115,    -1,   117,   118,    -1,   120,    21,   122,    -1,    24,
      -1,    26,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,     4,    38,    39,    40,    41,    42,    43,    44,
      12,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    24,    -1,    26,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    -1,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      -1,    -1,   107,    -1,    -1,    -1,    78,    -1,    -1,    -1,
      -1,    24,   117,   118,    -1,    -1,   121,   122,    31,    32,
      33,    34,    35,    36,    -1,    -1,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    47,   107,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,   117,   118,    -1,    -1,   121,
     122,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    92,
      -1,    -1,    -1,     4,    -1,    -1,    99,    -1,    -1,    -1,
     103,    12,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    24,   117,   118,    -1,    -1,    -1,   122,
      31,    32,    33,    34,    35,    36,     4,    -1,    -1,    40,
      41,    42,    43,    44,    12,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    -1,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      78,    -1,    -1,    -1,    -1,    24,   117,   118,    -1,    -1,
      29,   122,    31,    32,    33,    34,    35,    36,    -1,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,   107,
       4,    -1,    51,    52,    53,    54,    55,    56,    12,   117,
      -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    -1,    38,    39,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,    -1,
      -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    92,     4,
      -1,    -1,    -1,    97,    -1,    -1,    -1,    12,    -1,    -1,
      -1,    -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,    24,
      -1,   115,    -1,   117,    29,   119,    31,    32,    33,    34,
      35,    36,     4,    38,    39,    40,    41,    42,    43,    44,
      12,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    -1,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,
      -1,    -1,   117,   118,    29,    -1,    31,    32,    33,    34,
      35,    36,     4,    38,    39,    40,    41,    42,    43,    44,
      12,    -1,    47,    -1,    -1,   107,    51,    52,    53,    54,
      55,    56,    24,    -1,    -1,   117,    -1,    -1,     4,    31,
      32,    33,    34,    35,    36,    -1,    12,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    24,    51,
      52,    53,    54,    55,    56,    31,    32,    33,    34,    35,
      36,     4,    -1,    -1,    40,    41,    42,    43,    44,    12,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    24,   117,    -1,    -1,    -1,    -1,    -1,    31,    32,
      33,    34,    35,    36,    -1,    97,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    47,   107,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,   117,    -1,    -1,    -1,    -1,
      -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117
  };

  const short
  parser::yystos_[] =
  {
       0,    21,    99,   140,   141,   142,   145,   118,   122,   321,
     146,   159,     0,   146,    65,    66,   143,   100,   113,   147,
     160,   161,     1,   102,   320,   103,   128,   190,   190,   107,
     148,    14,   162,   173,   174,   128,   191,    77,    77,     4,
      12,    21,    24,    26,    29,    31,    32,    33,    34,    35,
      36,    38,    39,    40,    41,    42,    43,    44,    47,    51,
      52,    53,    54,    55,    56,   107,   117,   118,   121,   122,
     149,   150,   151,   156,   157,   287,   290,   291,   305,   306,
     307,   313,    27,    60,   163,   113,   158,     3,     5,     6,
       7,     8,    10,    13,    16,    17,    18,    19,    20,    22,
      26,    28,    37,    50,    57,    62,    63,    76,    82,    88,
      90,    91,   103,   107,   109,   118,   122,   127,   128,   129,
     130,   137,   138,   171,   175,   176,   177,   178,   182,   231,
     236,   241,   242,   246,   247,   248,   249,   276,   277,   280,
     281,   304,   305,   307,   315,   316,   319,   104,   114,   321,
     107,   286,   290,    79,    88,    90,    91,    92,    97,   119,
     120,   123,   124,   310,   311,   312,   314,   108,   114,   107,
     152,    99,   101,   144,    77,    34,   164,   113,    63,   107,
     239,   240,   242,   243,   245,    34,    35,    36,    79,    88,
      90,    91,    92,    99,   103,   107,   109,   115,   117,   119,
     120,   123,   124,   184,   198,   199,   202,   204,   206,   208,
     209,   211,   285,   286,   288,   292,   293,   301,   302,   303,
     313,   107,    99,   101,   269,   239,    72,    73,    74,    75,
     179,    99,   101,   187,   188,   204,   206,   107,   115,   295,
     304,   307,   311,   269,   247,   103,   237,   238,   128,   107,
     305,   103,   107,   278,   279,   281,   316,    91,   247,   267,
     268,   247,   246,   247,    79,   104,   115,   120,   124,   239,
     240,   250,   252,   253,   283,   297,   298,   300,   309,   310,
     312,   317,   318,    90,   108,   114,   250,   251,   310,   311,
     312,   317,   322,   110,   322,    19,   237,   237,   129,   170,
     158,    71,   185,    80,   115,   192,   283,   296,   298,   299,
     308,   310,   311,    98,   247,    99,   114,    87,   128,    88,
     292,   108,   108,   108,   108,   108,   108,   151,    78,   153,
     154,   155,   156,   146,   146,    24,   166,   117,   128,    23,
      80,   296,   239,   196,   224,   225,   226,   304,    29,   104,
     200,   202,   204,    86,    88,   108,   200,   215,   292,   322,
     322,   290,   303,    27,   189,   211,    89,    86,   209,   200,
     214,   215,    20,    46,    91,   239,   266,   270,   271,   272,
     270,   113,   244,    77,    77,    77,    77,   194,   200,   212,
     186,   231,   232,   241,   186,    15,    81,   311,   307,   129,
     129,    88,   129,   305,    77,    77,   128,   318,   114,    80,
     193,   247,    86,   267,    81,    84,   233,   234,   235,     3,
     306,   315,   296,    78,    84,   114,   104,   114,   240,   108,
     114,   108,   114,   108,   108,   108,   114,   110,   212,   305,
     305,   115,   172,   282,   294,   295,   318,   128,   184,   194,
     195,   200,   201,   306,   233,   242,   211,    78,   273,   274,
     275,   305,   196,   247,   108,   108,   108,   114,   100,   320,
     128,   165,    77,    77,    99,   101,   258,   194,   243,    80,
     114,   100,   114,   216,   104,    89,   108,    80,   108,   114,
     108,   110,   115,   115,   188,   204,   200,   108,   114,   188,
     269,   247,    85,   100,   113,   320,    25,   189,   100,   113,
     320,   239,   200,   201,   108,   115,   128,   128,   129,   104,
      77,    77,   108,   279,   103,   107,   109,   284,   285,    77,
     239,   239,   256,   257,   272,   189,   235,   115,   115,   115,
     239,    25,   254,   255,   272,   239,   250,   250,   250,   250,
      77,    80,    80,   316,   114,    77,   128,    80,    81,   183,
     219,   115,   100,   114,    81,    80,   155,   321,    91,   100,
     113,   239,   259,   260,   261,   265,   259,   320,   200,   304,
     226,    97,   107,   217,   301,   200,   200,   218,   200,   200,
     239,   271,   239,   232,   128,   128,   104,   108,   110,   189,
      81,   114,    47,   239,   114,    78,   194,   197,   197,   115,
     294,    77,   218,    29,   220,   221,   222,     9,   227,   228,
     229,   274,   250,   195,   114,     4,   167,   247,   259,   100,
     113,    84,    86,   262,   263,   264,   320,   200,   301,   108,
     108,   244,   239,   272,    48,    49,    48,   255,   272,   239,
      77,   114,    77,   216,    84,   203,   207,   210,   211,   223,
      22,    51,    52,   107,   180,   230,   289,   290,   229,   321,
      12,   107,   168,   169,   261,   256,   239,   189,   264,    80,
      11,   239,   239,   239,   194,    97,   221,    89,   115,   210,
     282,   108,   204,   205,   213,   230,    53,   181,   107,   149,
      86,   189,   218,   239,    49,   207,   223,   207,   114,   108,
     204,   149,   108,   239,   108,   239,   213,   108
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
     252,   252,   252,   252,   253,   253,   254,   254,   254,   254,
     255,   255,   255,   255,   256,   257,   257,   258,   258,   258,
     258,   259,   259,   260,   260,   260,   261,   262,   262,   263,
     263,   264,   265,   265,   266,   266,   267,   267,   268,   268,
     269,   269,   270,   270,   270,   270,   271,   271,   272,   272,
     272,   273,   273,   274,   274,   274,   275,   275,   276,   276,
     277,   277,   278,   278,   278,   279,   279,   280,   280,   280,
     280,   281,   281,   282,   282,   283,   283,   284,   284,   284,
     285,   285,   285,   285,   285,   286,   286,   286,   287,   287,
     287,   287,   287,   288,   288,   289,   290,   290,   291,   292,
     292,   292,   293,   293,   293,   293,   294,   294,   295,   295,
     296,   296,   296,   297,   297,   297,   298,   299,   299,   300,
     300,   301,   302,   303,   303,   303,   303,   303,   304,   304,
     305,   305,   305,   306,   306,   307,   307,   307,   307,   307,
     307,   307,   307,   308,   308,   309,   309,   310,   311,   311,
     312,   312,   313,   313,   313,   313,   313,   313,   313,   313,
     313,   313,   313,   313,   313,   313,   313,   313,   313,   313,
     314,   314,   314,   315,   315,   316,   317,   317,   318,   318,
     319,   319,   319,   319,   320,   320,   321,   321,   322,   322
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
       4,     3,     5,     3,     3,     3,     3,     3,     1,     1,
       2,     4,     4,     6,     1,     3,     1,     3,     3,     2,
       2,     1,     2,     3,     2,     1,     2,     3,     2,     2,
       1,     4,     1,     2,     1,     2,     1,     2,     2,     1,
       3,     3,     3,     2,     1,     0,     1,     2,     3,     1,
       2,     1,     0,     3,     1,     1,     3,     1,     1,     1,
       1,     3,     1,     3,     1,     1,     3,     2,     3,     2,
       3,     1,     2,     1,     3,     1,     3,     1,     2,     2,
       1,     3,     3,     3,     2,     1,     3,     3,     1,     3,
       3,     3,     3,     1,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     3,
       1,     1,     1,     1,     1,     1,     3,     1,     3,     1,
       3,     1,     3,     1,     1,     1,     1,     1,     1,     3,
       1,     3,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     2,     1
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
  "alts", "alts1", "alt", "alt_rhs", "gdpats", "gdpat", "pat", "bindpat",
  "apat", "apats1", "stmtlist", "stmts", "stmt", "qual", "fbinds",
  "fbinds1", "fbind", "qcon", "gen_qcon", "con", "con_list",
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
    1084,  1085,  1086,  1087,  1089,  1090,  1091,  1092,  1094,  1096,
    1097,  1098,  1100,  1102,  1103,  1105,  1106,  1107,  1108,  1109,
    1114,  1115,  1120,  1121,  1122,  1127,  1128,  1146,  1147,  1148,
    1149,  1150,  1151,  1152,  1154,  1155,  1167,  1168,  1169,  1170,
    1172,  1173,  1174,  1175,  1178,  1180,  1181,  1184,  1185,  1186,
    1187,  1189,  1190,  1192,  1193,  1194,  1196,  1198,  1199,  1201,
    1202,  1211,  1213,  1214,  1216,  1217,  1219,  1220,  1222,  1223,
    1226,  1227,  1229,  1230,  1231,  1232,  1237,  1238,  1240,  1241,
    1242,  1247,  1248,  1250,  1251,  1252,  1254,  1255,  1287,  1288,
    1290,  1291,  1293,  1294,  1295,  1297,  1298,  1300,  1301,  1302,
    1303,  1305,  1306,  1308,  1309,  1311,  1312,  1315,  1316,  1317,
    1319,  1320,  1321,  1322,  1323,  1325,  1326,  1327,  1329,  1330,
    1331,  1332,  1333,  1336,  1337,  1339,  1341,  1342,  1346,  1348,
    1349,  1350,  1352,  1353,  1354,  1355,  1360,  1361,  1363,  1364,
    1366,  1367,  1368,  1370,  1371,  1372,  1374,  1376,  1377,  1379,
    1380,  1384,  1386,  1388,  1389,  1390,  1391,  1392,  1395,  1396,
    1398,  1399,  1400,  1402,  1403,  1405,  1406,  1407,  1408,  1409,
    1410,  1411,  1412,  1414,  1415,  1417,  1418,  1420,  1422,  1423,
    1425,  1426,  1428,  1429,  1430,  1431,  1432,  1433,  1434,  1435,
    1436,  1437,  1438,  1439,  1440,  1441,  1442,  1443,  1444,  1445,
    1447,  1448,  1449,  1453,  1454,  1456,  1458,  1459,  1461,  1462,
    1466,  1467,  1468,  1469,  1474,  1477,  1481,  1482,  1484,  1485
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
#line 5893 "parser.cc"

#line 1494 "parser.y"


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

Haskell::Alts make_alts(const vector<Haskell::Alt>& alts)
{
    return {alts};
}

Haskell::Alt yy_make_alt(const expression_ref& pat, const expression_ref& alt_rhs)
{
    return {pat, alt_rhs};
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

