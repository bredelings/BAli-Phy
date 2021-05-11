// A Bison parser, made by GNU Bison 3.7.6.

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
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
        value.YY_MOVE_OR_COPY< Haskell::Decls > (YY_MOVE (that.value));
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

      case symbol_kind::S_importdecl: // importdecl
        value.YY_MOVE_OR_COPY< Haskell::ImpDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_module: // module
        value.YY_MOVE_OR_COPY< Haskell::Module > (YY_MOVE (that.value));
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

      case symbol_kind::S_export: // export
      case symbol_kind::S_qcname_ext_w_wildcard: // qcname_ext_w_wildcard
      case symbol_kind::S_qcname_ext: // qcname_ext
      case symbol_kind::S_qcname: // qcname
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

      case symbol_kind::S_maybeas: // maybeas
        value.YY_MOVE_OR_COPY< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.YY_MOVE_OR_COPY< std::optional<std::vector<expression_ref>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.YY_MOVE_OR_COPY< std::pair<Haskell::Context,expression_ref> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.YY_MOVE_OR_COPY< std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > (YY_MOVE (that.value));
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

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.YY_MOVE_OR_COPY< std::vector<Haskell::ImpDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.YY_MOVE_OR_COPY< std::vector<Haskell::Var> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.YY_MOVE_OR_COPY< std::vector<Located<Haskell::Alt>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
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

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
        value.move< Haskell::Decls > (YY_MOVE (that.value));
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

      case symbol_kind::S_importdecl: // importdecl
        value.move< Haskell::ImpDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_module: // module
        value.move< Haskell::Module > (YY_MOVE (that.value));
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

      case symbol_kind::S_export: // export
      case symbol_kind::S_qcname_ext_w_wildcard: // qcname_ext_w_wildcard
      case symbol_kind::S_qcname_ext: // qcname_ext
      case symbol_kind::S_qcname: // qcname
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

      case symbol_kind::S_maybeas: // maybeas
        value.move< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.move< std::optional<std::vector<expression_ref>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.move< std::pair<Haskell::Context,expression_ref> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.move< std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > (YY_MOVE (that.value));
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

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.move< std::vector<Haskell::ImpDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.move< std::vector<Haskell::Var> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Located<Haskell::Alt>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
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

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
        value.copy< Haskell::Decls > (that.value);
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

      case symbol_kind::S_importdecl: // importdecl
        value.copy< Haskell::ImpDecl > (that.value);
        break;

      case symbol_kind::S_module: // module
        value.copy< Haskell::Module > (that.value);
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

      case symbol_kind::S_export: // export
      case symbol_kind::S_qcname_ext_w_wildcard: // qcname_ext_w_wildcard
      case symbol_kind::S_qcname_ext: // qcname_ext
      case symbol_kind::S_qcname: // qcname
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

      case symbol_kind::S_maybeas: // maybeas
        value.copy< std::optional<std::string> > (that.value);
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.copy< std::optional<std::vector<expression_ref>> > (that.value);
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.copy< std::pair<Haskell::Context,expression_ref> > (that.value);
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.copy< std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > (that.value);
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

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.copy< std::vector<Haskell::ImpDecl> > (that.value);
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.copy< std::vector<Haskell::Var> > (that.value);
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.copy< std::vector<Located<Haskell::Alt>> > (that.value);
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
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

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
        value.move< Haskell::Decls > (that.value);
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

      case symbol_kind::S_importdecl: // importdecl
        value.move< Haskell::ImpDecl > (that.value);
        break;

      case symbol_kind::S_module: // module
        value.move< Haskell::Module > (that.value);
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

      case symbol_kind::S_export: // export
      case symbol_kind::S_qcname_ext_w_wildcard: // qcname_ext_w_wildcard
      case symbol_kind::S_qcname_ext: // qcname_ext
      case symbol_kind::S_qcname: // qcname
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

      case symbol_kind::S_maybeas: // maybeas
        value.move< std::optional<std::string> > (that.value);
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.move< std::optional<std::vector<expression_ref>> > (that.value);
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.move< std::pair<Haskell::Context,expression_ref> > (that.value);
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.move< std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > (that.value);
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

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.move< std::vector<Haskell::ImpDecl> > (that.value);
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.move< std::vector<Haskell::Var> > (that.value);
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Located<Haskell::Alt>> > (that.value);
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
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

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
        yylhs.value.emplace< Haskell::Decls > ();
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

      case symbol_kind::S_importdecl: // importdecl
        yylhs.value.emplace< Haskell::ImpDecl > ();
        break;

      case symbol_kind::S_module: // module
        yylhs.value.emplace< Haskell::Module > ();
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

      case symbol_kind::S_export: // export
      case symbol_kind::S_qcname_ext_w_wildcard: // qcname_ext_w_wildcard
      case symbol_kind::S_qcname_ext: // qcname_ext
      case symbol_kind::S_qcname: // qcname
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

      case symbol_kind::S_maybeas: // maybeas
        yylhs.value.emplace< std::optional<std::string> > ();
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        yylhs.value.emplace< std::optional<std::vector<expression_ref>> > ();
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        yylhs.value.emplace< std::pair<Haskell::Context,expression_ref> > ();
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        yylhs.value.emplace< std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();
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

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        yylhs.value.emplace< std::vector<Haskell::ImpDecl> > ();
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        yylhs.value.emplace< std::vector<Haskell::Var> > ();
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        yylhs.value.emplace< std::vector<Located<Haskell::Alt>> > ();
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
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
             {drv.result = yystack_[0].value.as < Haskell::Module > ();}
#line 1916 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 539 "parser.y"
                                                                 {yylhs.value.as < Haskell::Module > () = make_module(yystack_[4].value.as < std::string > (),yystack_[2].value.as < std::optional<std::vector<expression_ref>> > (),yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ().second);}
#line 1922 "parser.cc"
    break;

  case 4: // module: body2
#line 540 "parser.y"
                                                                 {yylhs.value.as < Haskell::Module > () = make_module("Main",{},yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ().second);}
#line 1928 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 542 "parser.y"
                                                                 {drv.push_module_context();}
#line 1934 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 550 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 1940 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 551 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 1946 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 553 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 1952 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 554 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 1958 "parser.cc"
    break;

  case 13: // top: semis top1
#line 557 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 1964 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 559 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Haskell::ImpDecl> > (),yystack_[0].value.as < Haskell::Decls > ());}
#line 1970 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 560 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Haskell::ImpDecl> > (),yystack_[0].value.as < Haskell::Decls > ());}
#line 1976 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 561 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Haskell::ImpDecl> > (),{});}
#line 1982 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 569 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<expression_ref>> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 1988 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 570 "parser.y"
                                      {}
#line 1994 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 572 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2000 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 574 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2006 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 575 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2012 "parser.cc"
    break;

  case 22: // export: qcname_ext export_subspec
#line 577 "parser.y"
                                      {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2018 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 578 "parser.y"
                                      {yylhs.value.as < expression_ref > () = AST_node("module",yystack_[0].value.as < std::string > ());}
#line 2024 "parser.cc"
    break;

  case 26: // qcnames: %empty
#line 584 "parser.y"
                   {}
#line 2030 "parser.cc"
    break;

  case 27: // qcnames: qcnames1
#line 585 "parser.y"
                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2036 "parser.cc"
    break;

  case 28: // qcnames1: qcnames1 "," qcname_ext_w_wildcard ","
#line 587 "parser.y"
                                                  {yylhs.value.as < std::vector<expression_ref> > () = yystack_[3].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ());}
#line 2042 "parser.cc"
    break;

  case 29: // qcnames1: qcname_ext_w_wildcard
#line 588 "parser.y"
                                              {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2048 "parser.cc"
    break;

  case 30: // qcname_ext_w_wildcard: qcname_ext
#line 590 "parser.y"
                                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2054 "parser.cc"
    break;

  case 31: // qcname_ext_w_wildcard: ".."
#line 591 "parser.y"
                                     {}
#line 2060 "parser.cc"
    break;

  case 32: // qcname_ext: qcname
#line 593 "parser.y"
                                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2066 "parser.cc"
    break;

  case 33: // qcname_ext: "type" oqtycon
#line 594 "parser.y"
                                     {}
#line 2072 "parser.cc"
    break;

  case 34: // qcname: qvar
#line 596 "parser.y"
                                     {yylhs.value.as < expression_ref > () = AST_node("qvar",yystack_[0].value.as < std::string > ()); }
#line 2078 "parser.cc"
    break;

  case 35: // qcname: oqtycon_no_varcon
#line 597 "parser.y"
                                     {yylhs.value.as < expression_ref > () = AST_node("qvar",yystack_[0].value.as < std::string > ()); }
#line 2084 "parser.cc"
    break;

  case 40: // importdecls: importdecls_semi importdecl
#line 607 "parser.y"
                                         { yylhs.value.as < std::vector<Haskell::ImpDecl> > () = yystack_[1].value.as < std::vector<Haskell::ImpDecl> > (), yylhs.value.as < std::vector<Haskell::ImpDecl> > ().push_back(yystack_[0].value.as < Haskell::ImpDecl > ()); }
#line 2090 "parser.cc"
    break;

  case 41: // importdecls_semi: importdecls_semi importdecl semis1
#line 609 "parser.y"
                                                     { yylhs.value.as < std::vector<Haskell::ImpDecl> > () = yystack_[2].value.as < std::vector<Haskell::ImpDecl> > (); yylhs.value.as < std::vector<Haskell::ImpDecl> > ().push_back(yystack_[1].value.as < Haskell::ImpDecl > ()); }
#line 2096 "parser.cc"
    break;

  case 42: // importdecls_semi: %empty
#line 610 "parser.y"
                         { }
#line 2102 "parser.cc"
    break;

  case 43: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 612 "parser.y"
                                                                                                        {
    yylhs.value.as < Haskell::ImpDecl > () = Haskell::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < std::string > (),yystack_[1].value.as < std::optional<std::string> > (),yystack_[0].value.as < expression_ref > ());
}
#line 2110 "parser.cc"
    break;

  case 44: // optqualified: "qualified"
#line 625 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2116 "parser.cc"
    break;

  case 45: // optqualified: %empty
#line 626 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2122 "parser.cc"
    break;

  case 46: // maybeas: "as" modid
#line 628 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 2128 "parser.cc"
    break;

  case 47: // maybeas: %empty
#line 629 "parser.y"
                               { }
#line 2134 "parser.cc"
    break;

  case 48: // maybeimpspec: impspec
#line 631 "parser.y"
                               { yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > (); }
#line 2140 "parser.cc"
    break;

  case 49: // maybeimpspec: %empty
#line 632 "parser.y"
                               { }
#line 2146 "parser.cc"
    break;

  case 50: // impspec: "(" exportlist ")"
#line 634 "parser.y"
                                      { yylhs.value.as < expression_ref > () = expression_ref{AST_node("only"),yystack_[1].value.as < std::vector<expression_ref> > ()}; }
#line 2152 "parser.cc"
    break;

  case 51: // impspec: "hiding" "(" exportlist ")"
#line 635 "parser.y"
                                      { yylhs.value.as < expression_ref > () = expression_ref{AST_node("hiding"),yystack_[1].value.as < std::vector<expression_ref> > ()}; }
#line 2158 "parser.cc"
    break;

  case 52: // prec: %empty
#line 640 "parser.y"
                   { }
#line 2164 "parser.cc"
    break;

  case 53: // prec: "INTEGER"
#line 641 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < int > (); }
#line 2170 "parser.cc"
    break;

  case 54: // infix: "infix"
#line 643 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infix; }
#line 2176 "parser.cc"
    break;

  case 55: // infix: "infixl"
#line 644 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infixl; }
#line 2182 "parser.cc"
    break;

  case 56: // infix: "infixr"
#line 645 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infixr; }
#line 2188 "parser.cc"
    break;

  case 57: // ops: ops "," op
#line 647 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 2194 "parser.cc"
    break;

  case 58: // ops: op
#line 648 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 2200 "parser.cc"
    break;

  case 59: // topdecls: topdecls_semi topdecl
#line 652 "parser.y"
                                 { yylhs.value.as < Haskell::Decls > () = yystack_[1].value.as < Haskell::Decls > (); yylhs.value.as < Haskell::Decls > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 2206 "parser.cc"
    break;

  case 60: // topdecls_semi: topdecls_semi topdecl semis1
#line 654 "parser.y"
                                            { yylhs.value.as < Haskell::Decls > () = yystack_[2].value.as < Haskell::Decls > (); yylhs.value.as < Haskell::Decls > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2212 "parser.cc"
    break;

  case 61: // topdecls_semi: %empty
#line 655 "parser.y"
                                            { }
#line 2218 "parser.cc"
    break;

  case 62: // topdecl: cl_decl
#line 657 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2224 "parser.cc"
    break;

  case 63: // topdecl: ty_decl
#line 658 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2230 "parser.cc"
    break;

  case 64: // topdecl: inst_decl
#line 659 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2236 "parser.cc"
    break;

  case 65: // topdecl: "default" "(" comma_types0 ")"
#line 662 "parser.y"
                                               {}
#line 2242 "parser.cc"
    break;

  case 66: // topdecl: decl_no_th
#line 669 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2248 "parser.cc"
    break;

  case 67: // topdecl: infixexp_top
#line 671 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2254 "parser.cc"
    break;

  case 68: // topdecl: "builtin" var "INTEGER" "STRING" "STRING"
#line 672 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2260 "parser.cc"
    break;

  case 69: // topdecl: "builtin" var "INTEGER" "STRING"
#line 673 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2266 "parser.cc"
    break;

  case 70: // topdecl: "builtin" varop "INTEGER" "STRING" "STRING"
#line 674 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2272 "parser.cc"
    break;

  case 71: // topdecl: "builtin" varop "INTEGER" "STRING"
#line 675 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2278 "parser.cc"
    break;

  case 72: // cl_decl: "class" tycl_hdr wherebinds
#line 677 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_class_decl(yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().second,yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 2284 "parser.cc"
    break;

  case 73: // ty_decl: "type" type "=" ctypedoc
#line 679 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_type_synonym({yystack_[2].location,yystack_[2].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2290 "parser.cc"
    break;

  case 74: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 680 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < Haskell::DataOrNewtype > (),yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().second,yystack_[1].value.as < std::vector<Haskell::Constructor> > ());}
#line 2296 "parser.cc"
    break;

  case 75: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig
#line 681 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[3].value.as < Haskell::DataOrNewtype > (),yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().second,{});}
#line 2302 "parser.cc"
    break;

  case 76: // inst_decl: "instance" overlap_pragma inst_type wherebinds
#line 686 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_instance_decl({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 2308 "parser.cc"
    break;

  case 86: // data_or_newtype: "data"
#line 741 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::data;}
#line 2314 "parser.cc"
    break;

  case 87: // data_or_newtype: "newtype"
#line 742 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::newtype;}
#line 2320 "parser.cc"
    break;

  case 90: // tycl_hdr: context "=>" type
#line 754 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ()};}
#line 2326 "parser.cc"
    break;

  case 91: // tycl_hdr: type
#line 755 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {{},yystack_[0].value.as < expression_ref > ()};}
#line 2332 "parser.cc"
    break;

  case 95: // decls: decls ";" decl
#line 803 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2338 "parser.cc"
    break;

  case 96: // decls: decls ";"
#line 804 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2344 "parser.cc"
    break;

  case 97: // decls: decl
#line 805 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2350 "parser.cc"
    break;

  case 98: // decls: %empty
#line 806 "parser.y"
                        {}
#line 2356 "parser.cc"
    break;

  case 99: // decllist: "{" decls "}"
#line 808 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Decls> > () = make_decls(yystack_[1].location,yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2362 "parser.cc"
    break;

  case 100: // decllist: "vocurly" decls close
#line 809 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Decls> > () = make_decls(yystack_[1].location,yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2368 "parser.cc"
    break;

  case 101: // binds: decllist
#line 811 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Decls> > () = yystack_[0].value.as < Located<Haskell::Decls> > ();}
#line 2374 "parser.cc"
    break;

  case 102: // wherebinds: "where" binds
#line 813 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Haskell::Decls>> > () = yystack_[0].value.as < Located<Haskell::Decls> > ();}
#line 2380 "parser.cc"
    break;

  case 103: // wherebinds: %empty
#line 814 "parser.y"
                                 {}
#line 2386 "parser.cc"
    break;

  case 109: // opt_tyconsig: %empty
#line 840 "parser.y"
                     {}
#line 2392 "parser.cc"
    break;

  case 110: // opt_tyconsig: "::" gtycon
#line 841 "parser.y"
                     {yylhs.value.as < expression_ref > () = make_type_var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2398 "parser.cc"
    break;

  case 111: // sigtype: ctype
#line 843 "parser.y"
                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2404 "parser.cc"
    break;

  case 112: // sigtypedoc: ctypedoc
#line 845 "parser.y"
                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2410 "parser.cc"
    break;

  case 113: // sig_vars: sig_vars "," var
#line 847 "parser.y"
                           {yylhs.value.as < std::vector<Haskell::Var> > () = yystack_[2].value.as < std::vector<Haskell::Var> > (); yylhs.value.as < std::vector<Haskell::Var> > ().push_back(Haskell::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}));}
#line 2416 "parser.cc"
    break;

  case 114: // sig_vars: var
#line 848 "parser.y"
                           {yylhs.value.as < std::vector<Haskell::Var> > ().push_back(Haskell::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}));}
#line 2422 "parser.cc"
    break;

  case 115: // sigtypes1: sigtype
#line 850 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2428 "parser.cc"
    break;

  case 116: // sigtypes1: sigtypes1 "," sigtype
#line 851 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2434 "parser.cc"
    break;

  case 117: // strict_mark: strictness
#line 855 "parser.y"
                                            {yylhs.value.as < Haskell::StrictLazy > () = yystack_[0].value.as < Haskell::StrictLazy > ();}
#line 2440 "parser.cc"
    break;

  case 118: // strictness: "!"
#line 861 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::strict;}
#line 2446 "parser.cc"
    break;

  case 119: // strictness: "~"
#line 862 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::lazy;}
#line 2452 "parser.cc"
    break;

  case 120: // ctype: "forall" tv_bndrs "." ctype
#line 869 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_forall_type(yystack_[2].value.as < std::vector<expression_ref> > (), yystack_[0].value.as < expression_ref > ());}
#line 2458 "parser.cc"
    break;

  case 121: // ctype: context "=>" ctype
#line 870 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_constrained_type(yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ());}
#line 2464 "parser.cc"
    break;

  case 122: // ctype: type
#line 872 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2470 "parser.cc"
    break;

  case 123: // ctypedoc: ctype
#line 874 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2476 "parser.cc"
    break;

  case 124: // context: btype
#line 883 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(yystack_[0].value.as < expression_ref > ());}
#line 2482 "parser.cc"
    break;

  case 125: // context_no_ops: btype_no_ops
#line 885 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2488 "parser.cc"
    break;

  case 126: // type: btype
#line 887 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2494 "parser.cc"
    break;

  case 127: // type: btype "->" ctype
#line 888 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps({make_type_var({yystack_[1].location,"->"}),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()});}
#line 2500 "parser.cc"
    break;

  case 128: // typedoc: type
#line 890 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2506 "parser.cc"
    break;

  case 129: // btype: tyapps
#line 893 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2512 "parser.cc"
    break;

  case 130: // btype_no_ops: atype_docs
#line 895 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2518 "parser.cc"
    break;

  case 131: // btype_no_ops: btype_no_ops atype_docs
#line 896 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2524 "parser.cc"
    break;

  case 132: // tyapps: tyapp
#line 898 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2530 "parser.cc"
    break;

  case 133: // tyapps: tyapps tyapp
#line 899 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2536 "parser.cc"
    break;

  case 134: // tyapp: atype
#line 901 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2542 "parser.cc"
    break;

  case 135: // tyapp: qtyconop
#line 902 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_type_var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2548 "parser.cc"
    break;

  case 136: // tyapp: tyvarop
#line 903 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_type_var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2554 "parser.cc"
    break;

  case 137: // atype_docs: atype
#line 909 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2560 "parser.cc"
    break;

  case 138: // atype: ntgtycon
#line 916 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2566 "parser.cc"
    break;

  case 139: // atype: tyvar
#line 917 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2572 "parser.cc"
    break;

  case 140: // atype: "*"
#line 918 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var({yystack_[0].location,"*"});}
#line 2578 "parser.cc"
    break;

  case 141: // atype: strict_mark atype
#line 919 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_strict_lazy_type(yystack_[1].value.as < Haskell::StrictLazy > (),yystack_[0].value.as < expression_ref > ());}
#line 2584 "parser.cc"
    break;

  case 142: // atype: "{" fielddecls "}"
#line 920 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_field_decls(yystack_[1].value.as < std::vector<Haskell::FieldDecl> > ());}
#line 2590 "parser.cc"
    break;

  case 143: // atype: "(" ")"
#line 921 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var({yystack_[1].location,"()"});}
#line 2596 "parser.cc"
    break;

  case 144: // atype: "(" comma_types1 "," ctype ")"
#line 922 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<expression_ref> > ();ts.push_back(yystack_[1].value.as < expression_ref > ());yylhs.value.as < expression_ref > () = make_tuple_type(ts);}
#line 2602 "parser.cc"
    break;

  case 145: // atype: "[" ctype "]"
#line 928 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_list_type(yystack_[1].value.as < expression_ref > ());}
#line 2608 "parser.cc"
    break;

  case 146: // atype: "(" ctype ")"
#line 929 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2614 "parser.cc"
    break;

  case 147: // atype: "(" ctype "::" kind ")"
#line 930 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_of_kind(yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ());}
#line 2620 "parser.cc"
    break;

  case 148: // inst_type: sigtype
#line 933 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2626 "parser.cc"
    break;

  case 151: // comma_types0: comma_types1
#line 938 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2632 "parser.cc"
    break;

  case 152: // comma_types0: %empty
#line 939 "parser.y"
                                       { /* default construction OK */ }
#line 2638 "parser.cc"
    break;

  case 153: // comma_types1: ctype
#line 941 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2644 "parser.cc"
    break;

  case 154: // comma_types1: comma_types1 "," ctype
#line 942 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2650 "parser.cc"
    break;

  case 155: // tv_bndrs: tv_bndrs tv_bndr
#line 949 "parser.y"
                               {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2656 "parser.cc"
    break;

  case 156: // tv_bndrs: %empty
#line 950 "parser.y"
                               { /* default construction OK */}
#line 2662 "parser.cc"
    break;

  case 157: // tv_bndr: tyvar
#line 952 "parser.y"
                                    {yylhs.value.as < expression_ref > () = make_type_var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2668 "parser.cc"
    break;

  case 158: // tv_bndr: "(" tyvar "::" kind ")"
#line 953 "parser.y"
                                    {yylhs.value.as < expression_ref > () = make_type_var_of_kind(yystack_[3].value.as < std::string > (),yystack_[1].value.as < expression_ref > ());}
#line 2674 "parser.cc"
    break;

  case 159: // kind: ctype
#line 971 "parser.y"
             {yylhs.value.as < expression_ref > () = make_kind(yystack_[0].value.as < expression_ref > ());}
#line 2680 "parser.cc"
    break;

  case 160: // constrs: "=" constrs1
#line 977 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::Constructor> > () = yystack_[0].value.as < std::vector<Haskell::Constructor> > ();}
#line 2686 "parser.cc"
    break;

  case 161: // constrs1: constrs1 "|" constr
#line 979 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::Constructor> > () = yystack_[2].value.as < std::vector<Haskell::Constructor> > (); yylhs.value.as < std::vector<Haskell::Constructor> > ().push_back(yystack_[0].value.as < Haskell::Constructor > ());}
#line 2692 "parser.cc"
    break;

  case 162: // constrs1: constr
#line 980 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::Constructor> > ().push_back(yystack_[0].value.as < Haskell::Constructor > ());}
#line 2698 "parser.cc"
    break;

  case 163: // constr: forall context_no_ops "=>" constr_stuff
#line 982 "parser.y"
                                                {yylhs.value.as < Haskell::Constructor > () = make_constructor(yystack_[3].value.as < expression_ref > (),yystack_[2].value.as < Haskell::Context > (), yystack_[0].value.as < expression_ref > ());}
#line 2704 "parser.cc"
    break;

  case 164: // constr: forall constr_stuff
#line 983 "parser.y"
                                                {yylhs.value.as < Haskell::Constructor > () = make_constructor(yystack_[1].value.as < expression_ref > (),{}, yystack_[0].value.as < expression_ref > ());}
#line 2710 "parser.cc"
    break;

  case 165: // forall: "forall" tv_bndrs "."
#line 985 "parser.y"
                                {if (yystack_[1].value.as < std::vector<expression_ref> > ().size()>1) yylhs.value.as < expression_ref > () = make_tv_bndrs(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2716 "parser.cc"
    break;

  case 166: // forall: %empty
#line 986 "parser.y"
                                {}
#line 2722 "parser.cc"
    break;

  case 167: // constr_stuff: btype_no_ops
#line 988 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2728 "parser.cc"
    break;

  case 168: // constr_stuff: btype_no_ops conop btype_no_ops
#line 989 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps({make_type_var({yystack_[1].location,yystack_[1].value.as < std::string > ()}),make_tyapps(yystack_[2].value.as < std::vector<expression_ref> > ()),make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 2734 "parser.cc"
    break;

  case 169: // fielddecls: %empty
#line 991 "parser.y"
                                {}
#line 2740 "parser.cc"
    break;

  case 170: // fielddecls: fielddecls1
#line 992 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::FieldDecl> > () = yystack_[0].value.as < std::vector<Haskell::FieldDecl> > ();}
#line 2746 "parser.cc"
    break;

  case 171: // fielddecls1: fielddecls1 "," fielddecl
#line 994 "parser.y"
                                        {yylhs.value.as < std::vector<Haskell::FieldDecl> > () = yystack_[2].value.as < std::vector<Haskell::FieldDecl> > (); yylhs.value.as < std::vector<Haskell::FieldDecl> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2752 "parser.cc"
    break;

  case 172: // fielddecls1: fielddecl
#line 995 "parser.y"
                                        {yylhs.value.as < std::vector<Haskell::FieldDecl> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2758 "parser.cc"
    break;

  case 173: // fielddecl: sig_vars "::" ctype
#line 997 "parser.y"
                                        {yylhs.value.as < Haskell::FieldDecl > () = make_field_decl(yystack_[2].value.as < std::vector<Haskell::Var> > (),yystack_[0].value.as < expression_ref > ());}
#line 2764 "parser.cc"
    break;

  case 184: // decl_no_th: sigdecl
#line 1016 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2770 "parser.cc"
    break;

  case 185: // decl_no_th: "!" aexp rhs
#line 1018 "parser.y"
                              {yylhs.value.as < expression_ref > () = new expression(AST_node("Decl:Strict"),{(yystack_[1].value.as < expression_ref > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2776 "parser.cc"
    break;

  case 186: // decl_no_th: infixexp_top rhs
#line 1026 "parser.y"
                                  {yylhs.value.as < expression_ref > () = make_value_decl(make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ());}
#line 2782 "parser.cc"
    break;

  case 187: // decl: decl_no_th
#line 1031 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2788 "parser.cc"
    break;

  case 188: // rhs: "=" exp wherebinds
#line 1035 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_rhs({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 2794 "parser.cc"
    break;

  case 189: // rhs: gdrhs wherebinds
#line 1036 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 2800 "parser.cc"
    break;

  case 190: // gdrhs: gdrhs gdrh
#line 1038 "parser.y"
                              {yylhs.value.as < std::vector<Haskell::GuardedRHS> > () = yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (); yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 2806 "parser.cc"
    break;

  case 191: // gdrhs: gdrh
#line 1039 "parser.y"
                              {yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 2812 "parser.cc"
    break;

  case 192: // gdrh: "|" guardquals "=" exp
#line 1043 "parser.y"
                              {yylhs.value.as < Haskell::GuardedRHS > () = make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2818 "parser.cc"
    break;

  case 193: // sigdecl: sig_vars "::" sigtypedoc
#line 1053 "parser.y"
                                  { yylhs.value.as < expression_ref > () = make_type_decl(yystack_[2].value.as < std::vector<Haskell::Var> > (),yystack_[0].value.as < expression_ref > ()); }
#line 2824 "parser.cc"
    break;

  case 194: // sigdecl: infix prec ops
#line 1054 "parser.y"
                         { yylhs.value.as < expression_ref > () = make_fixity_decl(yystack_[2].value.as < Haskell::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()); }
#line 2830 "parser.cc"
    break;

  case 195: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1056 "parser.y"
                                                    {}
#line 2836 "parser.cc"
    break;

  case 196: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1057 "parser.y"
                                            {}
#line 2842 "parser.cc"
    break;

  case 197: // sigdecl: "{-# SCC" qvar "#-}"
#line 1058 "parser.y"
                              {}
#line 2848 "parser.cc"
    break;

  case 198: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1059 "parser.y"
                                     {}
#line 2854 "parser.cc"
    break;

  case 199: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1060 "parser.y"
                                                               {}
#line 2860 "parser.cc"
    break;

  case 200: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1061 "parser.y"
                                                                      {}
#line 2866 "parser.cc"
    break;

  case 201: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1062 "parser.y"
                                                     {}
#line 2872 "parser.cc"
    break;

  case 206: // exp: infixexp "::" sigtype
#line 1073 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_typed_exp(make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()); }
#line 2878 "parser.cc"
    break;

  case 207: // exp: infixexp
#line 1074 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 2884 "parser.cc"
    break;

  case 208: // infixexp: exp10
#line 1076 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2890 "parser.cc"
    break;

  case 209: // infixexp: infixexp qop exp10
#line 1077 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_var({yystack_[1].location,yystack_[1].value.as < std::string > ()})); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2896 "parser.cc"
    break;

  case 210: // infixexp_top: exp10_top
#line 1079 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2902 "parser.cc"
    break;

  case 211: // infixexp_top: infixexp_top qop exp10_top
#line 1080 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_var({yystack_[1].location,yystack_[1].value.as < std::string > ()})); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2908 "parser.cc"
    break;

  case 212: // exp10_top: "-" fexp
#line 1082 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_minus(make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2914 "parser.cc"
    break;

  case 213: // exp10_top: "{-# CORE" "STRING" "#-}"
#line 1083 "parser.y"
                                   {}
#line 2920 "parser.cc"
    break;

  case 214: // exp10_top: fexp
#line 1084 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2926 "parser.cc"
    break;

  case 215: // exp10: exp10_top
#line 1086 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2932 "parser.cc"
    break;

  case 216: // exp10: scc_annot exp
#line 1087 "parser.y"
                                 {}
#line 2938 "parser.cc"
    break;

  case 221: // fexp: fexp aexp
#line 1098 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2944 "parser.cc"
    break;

  case 222: // fexp: fexp "TYPEAPP" atype
#line 1099 "parser.y"
                                 {}
#line 2950 "parser.cc"
    break;

  case 223: // fexp: "static" aexp
#line 1100 "parser.y"
                                 {}
#line 2956 "parser.cc"
    break;

  case 224: // fexp: aexp
#line 1101 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2962 "parser.cc"
    break;

  case 225: // aexp: qvar "@" aexp
#line 1103 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_as_pattern(make_var({yystack_[2].location,yystack_[2].value.as < std::string > ()}),yystack_[0].value.as < expression_ref > ());}
#line 2968 "parser.cc"
    break;

  case 226: // aexp: "~" aexp
#line 1104 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lazy_pattern(yystack_[0].value.as < expression_ref > ());}
#line 2974 "parser.cc"
    break;

  case 227: // aexp: "\\" apats1 "->" exp
#line 1105 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lambdaexp(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2980 "parser.cc"
    break;

  case 228: // aexp: "let" binds "in" exp
#line 1106 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_let(yystack_[2].value.as < Located<Haskell::Decls> > (),{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2986 "parser.cc"
    break;

  case 229: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1108 "parser.y"
                                                       {yylhs.value.as < expression_ref > () = make_if({yystack_[6].location,yystack_[6].value.as < expression_ref > ()},{yystack_[3].location,yystack_[3].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2992 "parser.cc"
    break;

  case 230: // aexp: "case" exp "of" altslist
#line 1110 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_case(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < Haskell::Alts > ());}
#line 2998 "parser.cc"
    break;

  case 231: // aexp: "do" stmtlist
#line 1111 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_do(yystack_[0].value.as < Haskell::Stmts > ());}
#line 3004 "parser.cc"
    break;

  case 232: // aexp: "mdo" stmtlist
#line 1112 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_mdo(yystack_[0].value.as < Haskell::Stmts > ());}
#line 3010 "parser.cc"
    break;

  case 233: // aexp: aexp1
#line 1114 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3016 "parser.cc"
    break;

  case 234: // aexp1: aexp1 "{" fbinds "}"
#line 1116 "parser.y"
                              {}
#line 3022 "parser.cc"
    break;

  case 235: // aexp1: aexp2
#line 1117 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3028 "parser.cc"
    break;

  case 236: // aexp2: qvar
#line 1119 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3034 "parser.cc"
    break;

  case 237: // aexp2: qcon
#line 1120 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3040 "parser.cc"
    break;

  case 238: // aexp2: literal
#line 1121 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3046 "parser.cc"
    break;

  case 239: // aexp2: "(" texp ")"
#line 1122 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3052 "parser.cc"
    break;

  case 240: // aexp2: "(" tup_exprs ")"
#line 1123 "parser.y"
                              {yylhs.value.as < expression_ref > () = yy_make_tuple(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3058 "parser.cc"
    break;

  case 241: // aexp2: "[" list "]"
#line 1128 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3064 "parser.cc"
    break;

  case 242: // aexp2: "_"
#line 1129 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::WildcardPattern();}
#line 3070 "parser.cc"
    break;

  case 243: // texp: exp
#line 1134 "parser.y"
                      {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3076 "parser.cc"
    break;

  case 244: // texp: infixexp qop
#line 1135 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("LeftSection"),{make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ()),make_var({yystack_[0].location,yystack_[0].value.as < std::string > ()})});}
#line 3082 "parser.cc"
    break;

  case 245: // texp: qopm infixexp
#line 1136 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("RightSection"),{make_var({yystack_[1].location,yystack_[1].value.as < std::string > ()}),make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 3088 "parser.cc"
    break;

  case 246: // tup_exprs: tup_exprs "," texp
#line 1141 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3094 "parser.cc"
    break;

  case 247: // tup_exprs: texp "," texp
#line 1142 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3100 "parser.cc"
    break;

  case 248: // list: texp
#line 1160 "parser.y"
                                 { yylhs.value.as < expression_ref > () = make_list({yystack_[0].value.as < expression_ref > ()}); }
#line 3106 "parser.cc"
    break;

  case 249: // list: lexps
#line 1161 "parser.y"
                                 { yylhs.value.as < expression_ref > () = make_list(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 3112 "parser.cc"
    break;

  case 250: // list: texp ".."
#line 1162 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFrom"),{yystack_[1].value.as < expression_ref > ()}); }
#line 3118 "parser.cc"
    break;

  case 251: // list: texp "," exp ".."
#line 1163 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThen"),{yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()}); }
#line 3124 "parser.cc"
    break;

  case 252: // list: texp ".." exp
#line 1164 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromTo"),{yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 3130 "parser.cc"
    break;

  case 253: // list: texp "," exp ".." exp
#line 1165 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThenTo"),{yystack_[4].value.as < expression_ref > (),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 3136 "parser.cc"
    break;

  case 254: // list: texp "|" squals
#line 1166 "parser.y"
                                 { auto quals = yystack_[0].value.as < std::vector<expression_ref> > (); quals.push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < expression_ref > () = expression_ref(AST_node("ListComprehension"),quals); }
#line 3142 "parser.cc"
    break;

  case 255: // lexps: lexps "," texp
#line 1168 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3148 "parser.cc"
    break;

  case 256: // lexps: texp "," texp
#line 1169 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3154 "parser.cc"
    break;

  case 257: // squals: squals "," qual
#line 1182 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3160 "parser.cc"
    break;

  case 258: // squals: qual
#line 1184 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3166 "parser.cc"
    break;

  case 259: // guardquals: guardquals1
#line 1194 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3172 "parser.cc"
    break;

  case 260: // guardquals1: guardquals1 "," qual
#line 1196 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > ();yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3178 "parser.cc"
    break;

  case 261: // guardquals1: qual
#line 1197 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3184 "parser.cc"
    break;

  case 262: // altslist: "{" alts "}"
#line 1200 "parser.y"
                                 {yylhs.value.as < Haskell::Alts > () = make_alts(yystack_[1].value.as < std::vector<Located<Haskell::Alt>> > ());}
#line 3190 "parser.cc"
    break;

  case 263: // altslist: "vocurly" alts close
#line 1201 "parser.y"
                                 {yylhs.value.as < Haskell::Alts > () = make_alts(yystack_[1].value.as < std::vector<Located<Haskell::Alt>> > ());}
#line 3196 "parser.cc"
    break;

  case 264: // altslist: "{" "}"
#line 1202 "parser.y"
                                 {}
#line 3202 "parser.cc"
    break;

  case 265: // altslist: "vocurly" close
#line 1203 "parser.y"
                                 {}
#line 3208 "parser.cc"
    break;

  case 266: // alts: alts1
#line 1205 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[0].value.as < std::vector<Located<Haskell::Alt>> > ();}
#line 3214 "parser.cc"
    break;

  case 267: // alts: ";" alts
#line 1206 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[0].value.as < std::vector<Located<Haskell::Alt>> > ();}
#line 3220 "parser.cc"
    break;

  case 268: // alts1: alts1 ";" alt
#line 1208 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[2].value.as < std::vector<Located<Haskell::Alt>> > (); yylhs.value.as < std::vector<Located<Haskell::Alt>> > ().push_back(yystack_[0].value.as < Located<Haskell::Alt> > ());}
#line 3226 "parser.cc"
    break;

  case 269: // alts1: alts1 ";"
#line 1209 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[1].value.as < std::vector<Located<Haskell::Alt>> > ();}
#line 3232 "parser.cc"
    break;

  case 270: // alts1: alt
#line 1210 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > ().push_back(yystack_[0].value.as < Located<Haskell::Alt> > ());}
#line 3238 "parser.cc"
    break;

  case 271: // alt: pat alt_rhs
#line 1212 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Alt> > () = yy_make_alt(yystack_[1].location+yystack_[0].location,yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3244 "parser.cc"
    break;

  case 272: // alt_rhs: "->" exp wherebinds
#line 1214 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_rhs({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 3250 "parser.cc"
    break;

  case 273: // alt_rhs: gdpats wherebinds
#line 1215 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 3256 "parser.cc"
    break;

  case 274: // gdpats: gdpats gdpat
#line 1217 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::GuardedRHS> > () = yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (); yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 3262 "parser.cc"
    break;

  case 275: // gdpats: gdpat
#line 1218 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 3268 "parser.cc"
    break;

  case 276: // gdpat: "|" guardquals "->" exp
#line 1227 "parser.y"
                                 {yylhs.value.as < Haskell::GuardedRHS > ()=make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3274 "parser.cc"
    break;

  case 277: // pat: exp
#line 1229 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3280 "parser.cc"
    break;

  case 278: // pat: "!" aexp
#line 1230 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3286 "parser.cc"
    break;

  case 279: // bindpat: exp
#line 1232 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3292 "parser.cc"
    break;

  case 280: // bindpat: "!" aexp
#line 1233 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3298 "parser.cc"
    break;

  case 281: // apat: aexp
#line 1235 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3304 "parser.cc"
    break;

  case 282: // apat: "!" aexp
#line 1236 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3310 "parser.cc"
    break;

  case 283: // apats1: apats1 apat
#line 1238 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3316 "parser.cc"
    break;

  case 284: // apats1: apat
#line 1239 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3322 "parser.cc"
    break;

  case 285: // stmtlist: "{" stmts "}"
#line 1242 "parser.y"
                               {yylhs.value.as < Haskell::Stmts > () = make_stmts(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3328 "parser.cc"
    break;

  case 286: // stmtlist: "vocurly" stmts close
#line 1243 "parser.y"
                               {yylhs.value.as < Haskell::Stmts > () = make_stmts(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3334 "parser.cc"
    break;

  case 287: // stmts: stmts ";" stmt
#line 1245 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3340 "parser.cc"
    break;

  case 288: // stmts: stmts ";"
#line 1246 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3346 "parser.cc"
    break;

  case 289: // stmts: stmt
#line 1247 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3352 "parser.cc"
    break;

  case 290: // stmts: %empty
#line 1248 "parser.y"
                       {}
#line 3358 "parser.cc"
    break;

  case 291: // stmt: qual
#line 1253 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3364 "parser.cc"
    break;

  case 292: // stmt: "rec" stmtlist
#line 1254 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::RecStmt(yystack_[0].value.as < Haskell::Stmts > ());}
#line 3370 "parser.cc"
    break;

  case 293: // qual: bindpat "<-" exp
#line 1256 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::PatQual(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3376 "parser.cc"
    break;

  case 294: // qual: exp
#line 1257 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::SimpleQual(yystack_[0].value.as < expression_ref > ());}
#line 3382 "parser.cc"
    break;

  case 295: // qual: "let" binds
#line 1258 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::LetQual(yystack_[0].value.as < Located<Haskell::Decls> > ());}
#line 3388 "parser.cc"
    break;

  case 303: // qcon: gen_qcon
#line 1303 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3394 "parser.cc"
    break;

  case 304: // qcon: sysdcon
#line 1304 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3400 "parser.cc"
    break;

  case 305: // gen_qcon: qconid
#line 1306 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3406 "parser.cc"
    break;

  case 306: // gen_qcon: "(" qconsym ")"
#line 1307 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3412 "parser.cc"
    break;

  case 307: // con: conid
#line 1309 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3418 "parser.cc"
    break;

  case 308: // con: "(" consym ")"
#line 1310 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3424 "parser.cc"
    break;

  case 309: // con: sysdcon
#line 1311 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3430 "parser.cc"
    break;

  case 312: // sysdcon_no_list: "(" ")"
#line 1316 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 3436 "parser.cc"
    break;

  case 313: // sysdcon_no_list: "(" commas ")"
#line 1317 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3442 "parser.cc"
    break;

  case 314: // sysdcon_no_list: "(#" "#)"
#line 1318 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 3448 "parser.cc"
    break;

  case 315: // sysdcon_no_list: "(#" commas "#)"
#line 1319 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3454 "parser.cc"
    break;

  case 316: // sysdcon: sysdcon_no_list
#line 1321 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3460 "parser.cc"
    break;

  case 317: // sysdcon: "[" "]"
#line 1322 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 3466 "parser.cc"
    break;

  case 318: // conop: consym
#line 1324 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3472 "parser.cc"
    break;

  case 319: // conop: "`" conid "`"
#line 1325 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3478 "parser.cc"
    break;

  case 320: // qconop: qconsym
#line 1327 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3484 "parser.cc"
    break;

  case 321: // qconop: "`" qconid "`"
#line 1328 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3490 "parser.cc"
    break;

  case 322: // gtycon: ntgtycon
#line 1331 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3496 "parser.cc"
    break;

  case 323: // gtycon: "(" ")"
#line 1332 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 3502 "parser.cc"
    break;

  case 324: // gtycon: "(#" "#)"
#line 1333 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 3508 "parser.cc"
    break;

  case 325: // ntgtycon: oqtycon
#line 1335 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3514 "parser.cc"
    break;

  case 326: // ntgtycon: "(" commas ")"
#line 1336 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3520 "parser.cc"
    break;

  case 327: // ntgtycon: "(#" commas "#)"
#line 1337 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3526 "parser.cc"
    break;

  case 328: // ntgtycon: "(" "->" ")"
#line 1338 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 3532 "parser.cc"
    break;

  case 329: // ntgtycon: "[" "]"
#line 1339 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 3538 "parser.cc"
    break;

  case 330: // oqtycon: qtycon
#line 1341 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3544 "parser.cc"
    break;

  case 331: // oqtycon: "(" qtyconsym ")"
#line 1342 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3550 "parser.cc"
    break;

  case 332: // oqtycon: "(" "~" ")"
#line 1343 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3556 "parser.cc"
    break;

  case 333: // oqtycon_no_varcon: qtycon
#line 1345 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3562 "parser.cc"
    break;

  case 334: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1346 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3568 "parser.cc"
    break;

  case 335: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1347 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3574 "parser.cc"
    break;

  case 336: // oqtycon_no_varcon: "(" ":" ")"
#line 1348 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 3580 "parser.cc"
    break;

  case 337: // oqtycon_no_varcon: "(" "~" ")"
#line 1349 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3586 "parser.cc"
    break;

  case 338: // qtyconop: qtyconsym
#line 1352 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3592 "parser.cc"
    break;

  case 339: // qtyconop: "`" qtycon "`"
#line 1353 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3598 "parser.cc"
    break;

  case 340: // qtycondoc: qtycon
#line 1355 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3604 "parser.cc"
    break;

  case 341: // qtycon: "QCONID"
#line 1357 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3610 "parser.cc"
    break;

  case 342: // qtycon: tycon
#line 1358 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3616 "parser.cc"
    break;

  case 343: // tycon: "CONID"
#line 1362 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3622 "parser.cc"
    break;

  case 344: // qtyconsym: "QCONSYM"
#line 1364 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3628 "parser.cc"
    break;

  case 345: // qtyconsym: "QVARSYM"
#line 1365 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3634 "parser.cc"
    break;

  case 346: // qtyconsym: tyconsym
#line 1366 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3640 "parser.cc"
    break;

  case 347: // tyconsym: "CONSYM"
#line 1368 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3646 "parser.cc"
    break;

  case 348: // tyconsym: "VARSYM"
#line 1369 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3652 "parser.cc"
    break;

  case 349: // tyconsym: ":"
#line 1370 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 3658 "parser.cc"
    break;

  case 350: // tyconsym: "-"
#line 1371 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 3664 "parser.cc"
    break;

  case 351: // op: varop
#line 1376 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3670 "parser.cc"
    break;

  case 352: // op: conop
#line 1377 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3676 "parser.cc"
    break;

  case 353: // varop: varsym
#line 1379 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3682 "parser.cc"
    break;

  case 354: // varop: "`" varid "`"
#line 1380 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3688 "parser.cc"
    break;

  case 355: // qop: qvarop
#line 1382 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3694 "parser.cc"
    break;

  case 356: // qop: qconop
#line 1383 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3700 "parser.cc"
    break;

  case 357: // qop: hole_op
#line 1384 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3706 "parser.cc"
    break;

  case 358: // qopm: qvaropm
#line 1386 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3712 "parser.cc"
    break;

  case 359: // qopm: qconop
#line 1387 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3718 "parser.cc"
    break;

  case 360: // qopm: hole_op
#line 1388 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3724 "parser.cc"
    break;

  case 361: // hole_op: "`" "_" "`"
#line 1390 "parser.y"
                      { yylhs.value.as < std::string > () = "_"; }
#line 3730 "parser.cc"
    break;

  case 362: // qvarop: qvarsym
#line 1392 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3736 "parser.cc"
    break;

  case 363: // qvarop: "`" qvarid "`"
#line 1393 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3742 "parser.cc"
    break;

  case 364: // qvaropm: qvarsym_no_minus
#line 1395 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 3748 "parser.cc"
    break;

  case 365: // qvaropm: "`" qvarid "`"
#line 1396 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3754 "parser.cc"
    break;

  case 366: // tyvar: tyvarid
#line 1400 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3760 "parser.cc"
    break;

  case 367: // tyvarop: "`" tyvarid "`"
#line 1402 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3766 "parser.cc"
    break;

  case 368: // tyvarid: "VARID"
#line 1404 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3772 "parser.cc"
    break;

  case 369: // tyvarid: special_id
#line 1405 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3778 "parser.cc"
    break;

  case 370: // tyvarid: "unsafe"
#line 1406 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 3784 "parser.cc"
    break;

  case 371: // tyvarid: "safe"
#line 1407 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 3790 "parser.cc"
    break;

  case 372: // tyvarid: "interruptible"
#line 1408 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 3796 "parser.cc"
    break;

  case 373: // var: varid
#line 1411 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3802 "parser.cc"
    break;

  case 374: // var: "(" varsym ")"
#line 1412 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3808 "parser.cc"
    break;

  case 375: // qvar: qvarid
#line 1414 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3814 "parser.cc"
    break;

  case 376: // qvar: "(" varsym ")"
#line 1415 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3820 "parser.cc"
    break;

  case 377: // qvar: "(" qvarsym1 ")"
#line 1416 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3826 "parser.cc"
    break;

  case 378: // qvarid: varid
#line 1418 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3832 "parser.cc"
    break;

  case 379: // qvarid: "QVARID"
#line 1419 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3838 "parser.cc"
    break;

  case 380: // varid: "VARID"
#line 1421 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3844 "parser.cc"
    break;

  case 381: // varid: special_id
#line 1422 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3850 "parser.cc"
    break;

  case 382: // varid: "unsafe"
#line 1423 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 3856 "parser.cc"
    break;

  case 383: // varid: "safe"
#line 1424 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 3862 "parser.cc"
    break;

  case 384: // varid: "interruptible"
#line 1425 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 3868 "parser.cc"
    break;

  case 385: // varid: "forall"
#line 1426 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 3874 "parser.cc"
    break;

  case 386: // varid: "family"
#line 1427 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 3880 "parser.cc"
    break;

  case 387: // varid: "role"
#line 1428 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 3886 "parser.cc"
    break;

  case 388: // qvarsym: varsym
#line 1430 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3892 "parser.cc"
    break;

  case 389: // qvarsym: qvarsym1
#line 1431 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3898 "parser.cc"
    break;

  case 390: // qvarsym_no_minus: varsym_no_minus
#line 1433 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3904 "parser.cc"
    break;

  case 391: // qvarsym_no_minus: qvarsym1
#line 1434 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3910 "parser.cc"
    break;

  case 392: // qvarsym1: "QVARSYM"
#line 1436 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3916 "parser.cc"
    break;

  case 393: // varsym: varsym_no_minus
#line 1438 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3922 "parser.cc"
    break;

  case 394: // varsym: "-"
#line 1439 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 3928 "parser.cc"
    break;

  case 395: // varsym_no_minus: "VARSYM"
#line 1441 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3934 "parser.cc"
    break;

  case 396: // varsym_no_minus: special_sym
#line 1442 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3940 "parser.cc"
    break;

  case 397: // special_id: "as"
#line 1444 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 3946 "parser.cc"
    break;

  case 398: // special_id: "qualified"
#line 1445 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 3952 "parser.cc"
    break;

  case 399: // special_id: "hiding"
#line 1446 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 3958 "parser.cc"
    break;

  case 400: // special_id: "export"
#line 1447 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 3964 "parser.cc"
    break;

  case 401: // special_id: "label"
#line 1448 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 3970 "parser.cc"
    break;

  case 402: // special_id: "dynamic"
#line 1449 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 3976 "parser.cc"
    break;

  case 403: // special_id: "stdcall"
#line 1450 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 3982 "parser.cc"
    break;

  case 404: // special_id: "ccall"
#line 1451 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 3988 "parser.cc"
    break;

  case 405: // special_id: "capi"
#line 1452 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 3994 "parser.cc"
    break;

  case 406: // special_id: "prim"
#line 1453 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 4000 "parser.cc"
    break;

  case 407: // special_id: "javascript"
#line 1454 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 4006 "parser.cc"
    break;

  case 408: // special_id: "group"
#line 1455 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 4012 "parser.cc"
    break;

  case 409: // special_id: "stock"
#line 1456 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 4018 "parser.cc"
    break;

  case 410: // special_id: "anyclass"
#line 1457 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 4024 "parser.cc"
    break;

  case 411: // special_id: "via"
#line 1458 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 4030 "parser.cc"
    break;

  case 412: // special_id: "unit"
#line 1459 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 4036 "parser.cc"
    break;

  case 413: // special_id: "dependency"
#line 1460 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 4042 "parser.cc"
    break;

  case 414: // special_id: "signature"
#line 1461 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 4048 "parser.cc"
    break;

  case 415: // special_sym: "!"
#line 1463 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 4054 "parser.cc"
    break;

  case 416: // special_sym: "."
#line 1464 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 4060 "parser.cc"
    break;

  case 417: // special_sym: "*"
#line 1465 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 4066 "parser.cc"
    break;

  case 418: // qconid: conid
#line 1469 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4072 "parser.cc"
    break;

  case 419: // qconid: "QCONID"
#line 1470 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4078 "parser.cc"
    break;

  case 420: // conid: "CONID"
#line 1472 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4084 "parser.cc"
    break;

  case 421: // qconsym: consym
#line 1474 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4090 "parser.cc"
    break;

  case 422: // qconsym: "QCONSYM"
#line 1475 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4096 "parser.cc"
    break;

  case 423: // consym: "CONSYM"
#line 1477 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4102 "parser.cc"
    break;

  case 424: // consym: ":"
#line 1478 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4108 "parser.cc"
    break;

  case 425: // literal: "CHAR"
#line 1482 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < char > ();}
#line 4114 "parser.cc"
    break;

  case 426: // literal: "STRING"
#line 1483 "parser.y"
                  {yylhs.value.as < expression_ref > () = yy_make_string(yystack_[0].value.as < std::string > ());}
#line 4120 "parser.cc"
    break;

  case 427: // literal: "INTEGER"
#line 1484 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < int > ();}
#line 4126 "parser.cc"
    break;

  case 428: // literal: "RATIONAL"
#line 1485 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < double > ();}
#line 4132 "parser.cc"
    break;

  case 430: // close: error
#line 1493 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 4138 "parser.cc"
    break;

  case 431: // modid: "CONID"
#line 1497 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4144 "parser.cc"
    break;

  case 432: // modid: "QCONID"
#line 1498 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4150 "parser.cc"
    break;

  case 433: // commas: commas ","
#line 1500 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 4156 "parser.cc"
    break;

  case 434: // commas: ","
#line 1501 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 4162 "parser.cc"
    break;


#line 4166 "parser.cc"

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


  const short parser::yypact_ninf_ = -579;

  const short parser::yytable_ninf_ = -394;

  const short
  parser::yypact_[] =
  {
      38,   122,  -579,    84,  -579,  -579,  -579,  -579,  -579,   200,
     -21,    -1,  -579,    45,    58,    58,    68,  -579,  -579,  -579,
    -579,   173,  -579,  -579,  -579,    69,  -579,   142,   155,  3536,
     210,   223,   148,  -579,   597,  -579,   -18,  -579,  -579,  -579,
    -579,   122,  -579,    96,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,  -579,  -579,  -579,   406,  -579,  -579,  -579,  -579,
     171,   175,  -579,   191,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,   159,  -579,   122,  -579,   197,  -579,  1966,  3198,
    -579,   204,   189,  1966,  -579,  -579,  -579,   229,   208,  -579,
    3198,  3857,   189,  2692,   237,   220,  3812,   190,  2329,  2692,
    2450,  2692,  1105,   977,   140,  -579,  -579,  -579,  -579,  -579,
    -579,    49,   237,   203,   148,  -579,  -579,  -579,   247,    51,
    -579,  -579,   172,  -579,  2571,  -579,   243,  -579,  -579,  -579,
    -579,  -579,  -579,   265,    65,  -579,  -579,  -579,  -579,   226,
    -579,   301,  -579,  -579,   250,   251,  -579,  -579,  -579,  -579,
    -579,   256,  -579,   261,   262,   269,  -579,  -579,  -579,  3536,
    3569,  -579,  -579,  -579,  -579,   374,  -579,   -40,   977,   356,
     295,  -579,  -579,  1966,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,  3979,  2895,  2794,   268,  3720,  -579,  -579,  -579,
    -579,  -579,   363,  3628,  -579,   294,  -579,   192,  3198,  -579,
    -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,
    2996,  1482,  1482,  -579,   280,   317,   320,   322,   323,  2996,
     726,   726,  -579,   387,   327,   318,   151,  4036,   282,   284,
    -579,  -579,  -579,  -579,   -28,  3812,  -579,   340,   110,    -2,
     319,    54,   313,   348,  -579,  -579,  2692,  -579,  -579,  2208,
    -579,  2571,   101,  -579,  -579,  3299,  -579,  -579,  -579,   295,
      76,   325,   321,  -579,  1966,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,  2450,  -579,  -579,   -23,    56,   262,   326,   330,
     331,    70,  -579,   145,  2996,  3812,  3812,  -579,   458,   197,
     303,  3198,  2996,  3979,  1966,  1724,  3299,  -579,    36,  -579,
    -579,  2087,  -579,  -579,  -579,  -579,  -579,  3628,  -579,  3753,
    2692,  -579,   333,   334,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,   335,   332,  -579,  -579,   344,    45,   122,    46,
     372,   375,   215,  2996,  1966,  -579,    78,   351,   339,  -579,
    -579,  -579,   329,   365,  -579,   349,   333,  -579,    -8,   342,
     334,   103,   157,   343,   347,   208,  -579,  -579,  3198,  2996,
    -579,  -579,   355,   345,   208,   189,  2692,   379,   382,   -31,
    -579,  -579,    42,  -579,   443,  -579,  -579,  -579,  -579,  -579,
    -579,   363,   -24,  -579,  -579,   172,    44,  1966,  2996,   361,
     357,   352,   353,   341,   371,   402,  -579,  -579,   405,   383,
     190,   254,   413,  -579,  1966,  -579,  -579,   368,   380,   385,
    1966,  1966,  1724,  1233,  -579,  1233,   578,  -579,  1233,  -579,
    1233,    79,  -579,  -579,  -579,  -579,   424,   422,   426,  3946,
     393,  -579,  -579,  -579,  -579,     1,   144,  -579,  -579,  -579,
    -579,   363,   427,   395,  -579,   396,  -579,  -579,  -579,  -579,
    -579,   410,  -579,   398,   433,  -579,  -579,  -579,  -579,  3569,
    -579,  -579,  -579,   408,  3536,  -579,  -579,  -579,  -579,  1361,
     856,  -579,  -579,  -579,  2996,  -579,  3979,  4069,  -579,  2996,
    -579,  2996,  -579,  2996,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,  2996,   387,  -579,  -579,  1966,  -579,  1482,  -579,
    1966,  -579,  -579,   726,  -579,  -579,  -579,  -579,  -579,   389,
     390,   415,  -579,  -579,  -579,  -579,  -579,   416,   707,   158,
    -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,   407,  -579,
     445,  -579,  -579,  -579,  -579,  -579,  2996,  2996,   409,   458,
    -579,   450,  2996,   499,  -579,   522,  -579,  1966,  1724,  -579,
    -579,  3753,  1233,   418,  3536,   425,  2692,  -579,  1603,  -579,
     434,   423,  -579,   233,    45,  -579,  -579,  -579,  2996,  4128,
    -579,  -579,  -579,  -579,   430,   432,  -579,  -579,  -579,   280,
    -579,  -579,  -579,  -579,  -579,  -579,  1724,  1966,  -579,    16,
      21,  -579,  -579,  -579,  -579,  -579,   459,  -579,  3628,    73,
    -579,   522,  -579,  -579,  -579,  -579,  -579,  -579,   436,  -579,
    -579,  -579,  -579,  1845,  1724,  1966,  -579,    39,  -579,  -579,
    -579,   466,  -579,  -579,   536,  -579,  -579,  -579,  2996,  -579,
    4095,   499,   462,  3344,  -579,  -579,  -579,  -579,  -579,  -579,
    3097,   127,   500,  -579,  -579,  -579,  -579,  -579,   472,   363,
    -579,  -579,  2996,  1966,  -579,  -579,  -579,  3628,   441,  -579,
    3628,  -579,  -579,   446,   453,  -579,  3198,  -579,  1966,  -579,
     457,  -579,  3436,  -579,  3628,  3198,  -579,  -579,  -579,  -579,
    -579
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    39,     0,     2,    39,     4,   431,   432,     8,
       0,    42,     1,     0,     0,     0,    18,    11,    38,    13,
      16,    61,   430,   429,    12,   108,   104,     0,     0,     0,
       0,    45,    40,    15,    14,   107,     0,     6,     7,   397,
     399,     0,   398,     0,   385,   400,   401,   402,   383,   384,
     382,   386,   387,   403,   404,   405,   406,   407,   408,   409,
     410,   411,   412,   414,   413,     0,   380,   343,   379,   341,
       0,    19,    21,    24,    32,    35,   333,   342,    34,   375,
     378,   381,     0,    44,     0,    37,    41,   242,     0,     0,
      86,     0,     0,     0,    54,    55,    56,    81,     0,    87,
       0,     0,     0,     0,   202,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   420,   419,   425,   426,   427,
     428,   202,   202,    52,    59,    62,    63,    64,    94,     0,
      66,   184,    67,   210,   214,   224,   233,   235,   237,   303,
     316,   304,   114,   236,   378,   305,   418,   238,   105,     0,
      23,     0,    33,   330,     0,     0,   394,   415,   417,   416,
     395,     0,   392,     0,     0,     0,   393,   396,    17,     0,
      26,    22,    39,    39,     3,    47,    36,     0,     0,     0,
     207,   215,   208,     0,   371,   372,   370,   349,   119,   350,
     118,   140,   169,     0,     0,     0,     0,   368,   348,   347,
     345,   344,   103,     0,   117,     0,    91,   126,   129,   132,
     134,   138,   325,   135,   338,   346,   139,   136,   366,   369,
     152,   290,   290,   231,   218,     0,     0,     0,     0,     0,
      98,    98,   101,     0,     0,   126,     0,     0,     0,     0,
     373,   353,   232,   223,     0,     0,   203,     0,     0,     0,
       0,     0,   310,   109,   309,   307,     0,   281,   284,     0,
     226,   212,     0,   424,   317,     0,   423,   422,   243,   207,
     248,     0,   249,   359,     0,   360,   358,   364,   391,   390,
     320,   421,   394,   312,   434,     0,     0,   391,     0,   390,
     320,     0,   314,     0,     0,     0,     0,    53,     0,    60,
       0,     0,     0,     0,     0,     0,     0,   186,   103,   191,
     356,     0,   357,   355,   362,   389,   388,     0,   221,   297,
       0,   106,     0,     0,   336,   337,   335,   334,   377,   376,
      20,    31,     0,    27,    29,    30,     0,     0,     0,    49,
       0,     0,     0,     0,     0,   216,     0,     0,   170,   172,
     156,   329,     0,     0,   122,     0,   119,   143,   153,     0,
     338,     0,     0,     0,     0,     0,    72,   141,     0,     0,
     133,   153,     0,   151,     0,     0,     0,   294,     0,     0,
     289,   291,     0,   217,     0,    78,    77,    79,    80,   148,
     111,   103,     0,   187,    97,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   213,   197,     0,     0,
       0,     0,     0,   282,     0,   283,   185,     0,     0,     0,
     244,   250,     0,     0,   241,     0,   245,   239,     0,   240,
       0,   376,   306,   313,   433,   315,     0,     0,     0,     0,
     194,   352,    58,   351,   318,     0,    88,   193,   123,   112,
     113,   103,     0,   259,   261,     0,   189,   190,   211,   222,
     300,     0,   296,   299,   302,   225,   332,   331,    25,     0,
       9,    10,    46,     0,     0,    43,    48,   220,   219,     0,
       0,   230,   206,   209,     0,   142,     0,     0,   145,     0,
     328,     0,   146,     0,   326,   327,   339,   367,   102,    90,
     127,    65,     0,   295,   292,   280,     0,   285,   288,   286,
       0,    76,    99,    96,   100,   228,    73,   374,   354,    71,
      69,     0,   204,   196,   198,   308,   311,     0,     0,     0,
     110,   322,   195,   227,   361,   365,   321,   252,   254,   258,
     243,   256,   255,   247,   246,   201,     0,     0,     0,     0,
      93,     0,     0,   166,    75,   174,   188,     0,     0,   363,
     234,     0,     0,     0,     0,     0,     0,   264,     0,   277,
       0,   266,   270,     0,     0,   265,   173,   171,     0,     0,
     155,   157,   121,   159,     0,   154,   154,   293,   287,   218,
      95,    70,    68,   205,   323,   324,     0,   251,   115,     0,
       0,   319,    57,    92,    89,   156,   160,   162,     0,     0,
      74,   175,   177,   192,   260,   298,   301,    28,     0,    50,
     278,   267,   262,   269,     0,     0,   271,   103,   275,   263,
     120,     0,   147,   144,     0,   257,   253,   199,     0,   200,
       0,   166,     0,   167,   130,   137,   164,    84,    82,    83,
       0,     0,   178,   181,   340,   176,    51,   268,     0,   103,
     273,   274,     0,     0,   116,   165,   161,     0,     0,   131,
       0,   182,   128,   149,     0,   179,     0,   180,     0,   272,
       0,   229,   167,   163,   168,     0,   183,    85,   276,   158,
     150
  };

  const short
  parser::yypgoto_[] =
  {
    -579,  -579,  -579,  -579,  -579,  -579,  -579,    37,  -579,  -579,
    -422,  -579,   397,  -579,  -579,  -579,    98,  -144,  -579,   444,
    -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,   270,  -579,   338,  -579,  -202,  -278,   555,  -579,
    -579,  -311,  -579,  -157,    25,  -579,  -579,  -146,   176,   -55,
    -579,   -88,  -579,   -94,  -471,  -579,   367,  -578,  -196,   289,
    -100,  -579,   366,   -16,  -579,  -513,  -579,  -579,   -54,  -579,
     -74,  -579,  -579,   106,  -579,  -579,    -5,   -56,   574,    99,
     358,  -579,   310,  -579,   214,  -579,   -84,   -97,   577,   -26,
    -288,    35,  -579,   -70,   -90,  -579,  -579,   -57,  -579,  -579,
    -579,  -579,     3,  -579,  -579,  -429,  -579,    19,  -579,  -579,
      18,  -579,  -579,   384,  -579,   -69,   439,   138,  -274,  -579,
      94,  -579,  -579,  -579,  -579,   246,  -579,   -87,  -563,   -75,
    -579,   252,   615,  -579,  -579,  -579,   -29,  -579,  -140,  -579,
     113,   563,  -153,  -579,   -71,  -579,  -579,  -451,  -579,   469,
     -64,   -19,  -195,    -9,  -579,  -579,   -42,   -60,   -63,   -86,
    -579,  -192,  -105,   -51,  -234,  -579,  -299,   -17,   -92
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   174,     6,    10,    19,    30,
      70,    71,    72,   171,   332,   333,   334,    73,    74,    86,
      11,    20,    21,    32,    84,   339,   475,   476,   298,   123,
     440,    33,    34,   124,   125,   126,   127,   229,   651,   677,
     128,   554,   202,   301,   392,   232,   233,   366,    27,    36,
     412,   389,   447,   129,   599,   203,   204,   390,   449,   353,
     642,   354,   673,   207,   643,   208,   209,   644,   210,   391,
     674,   372,   359,   487,   580,   584,   555,   606,   607,   608,
     646,   347,   348,   349,   610,   611,   612,   652,   393,   394,
     307,   308,   309,   131,   245,   246,   377,   180,   395,   181,
     182,   384,   183,   134,   135,   136,   137,   285,   286,   271,
     272,   538,   452,   453,   481,   570,   571,   572,   626,   627,
     628,   573,   378,   258,   259,   223,   379,   380,   381,   461,
     462,   463,   138,   139,   252,   253,   140,   141,   441,   273,
     530,   211,   212,    75,   213,   653,   153,    77,   214,   215,
     442,   443,   311,   274,   275,   313,   276,   216,   217,   218,
     142,   143,    79,    80,   314,   277,   278,   316,   166,    81,
     167,   145,   146,   280,   281,   147,    24,     9,   291
  };

  const short
  parser::yytable_[] =
  {
      76,   206,   255,   219,   179,   165,   235,   367,   133,   224,
      78,   323,   234,   243,   219,   269,   269,   409,   257,   260,
     254,   262,   293,   164,   150,   144,   335,   344,   268,   268,
     456,   454,   482,   242,   205,   346,   581,   239,   471,   604,
     261,   241,    13,    22,   318,    22,    22,   352,   358,   279,
     289,   574,   565,   288,   360,   270,   483,   310,   473,     1,
     403,   312,   290,   365,   444,   669,   365,   175,   294,   507,
     418,   287,   491,   419,   371,   407,   512,   340,   550,    17,
     670,   269,   508,   509,    12,   427,   148,   249,   341,   513,
     315,   428,   240,   637,   268,   647,   149,   514,   639,   345,
     492,   404,   361,   362,   669,   310,   669,   219,   219,   312,
     219,   455,    18,   511,   419,   289,   420,   219,   165,   670,
     305,   459,   219,   624,   648,   649,   408,   290,   631,   551,
     638,   302,   483,   263,   219,   638,   287,     2,   315,   621,
      76,    76,   618,   219,    23,  -373,    23,    23,   539,   680,
      78,    78,   244,   474,   421,   508,   448,   513,   484,  -374,
     422,    25,   283,   498,   429,   303,   413,   363,   284,   257,
     430,   318,   503,   556,   266,    29,   399,   426,   433,  -373,
     650,   575,   304,   240,   434,   305,    26,    31,   165,   581,
     423,    67,   303,  -374,   310,    69,   682,    35,   312,   684,
     156,   157,   158,   151,   133,   133,   164,   159,   219,   336,
     337,   494,   261,   206,    67,   219,   219,   434,    69,    37,
     451,   144,   144,   500,   552,   553,   405,   315,   400,   160,
     465,   219,    38,   162,   650,   598,   598,    82,   241,   450,
       7,   156,   157,   158,     8,    67,   205,    83,   159,    69,
     292,   263,   448,   304,   284,   435,   305,   219,   172,   434,
     173,    85,   156,   157,   158,    14,    15,   495,   595,   159,
     160,   434,   284,   344,   235,   629,   437,   438,   369,   168,
     499,  -124,   219,   219,   614,   458,   505,   306,   221,   169,
     222,   160,   266,   250,   240,   162,   267,   251,   170,   114,
     464,   225,   226,   227,   228,   255,   504,   230,   115,   231,
     176,   220,   219,   515,   479,   444,   480,   624,   300,   625,
     310,   472,   635,   254,   312,   335,   269,   664,   269,   346,
     533,   269,   297,   269,   548,   295,   296,   537,   576,   540,
     244,   268,   319,   582,   268,   583,   268,   585,   247,   660,
     454,   310,   320,   315,   321,   312,   586,   527,   324,   325,
     279,   528,   279,   529,   326,   279,   541,   279,   542,   327,
     328,   543,    67,   544,   263,   343,    69,   329,   338,   342,
     187,   679,   284,   368,   315,   156,   157,   158,   323,   322,
     365,   189,   159,   383,   385,   569,   569,   386,   219,   387,
     388,   219,   397,   219,   369,   219,   583,   219,   398,   444,
     306,   401,   645,   402,   160,   266,   219,   406,   162,   267,
     198,   199,   587,   264,   200,   201,   589,   410,   411,   424,
     400,   445,   630,   488,   431,   425,   361,   362,  -393,   432,
      76,   466,   467,   468,   470,    76,   469,   645,   444,   477,
      78,   485,   478,   486,   489,    78,   493,   490,   496,   502,
     219,   219,   497,   501,  -279,   269,   219,   506,   510,   517,
     521,   645,   518,   613,   645,   522,   620,   240,   268,   523,
     519,   520,   524,   534,   569,   154,   645,   133,   645,   241,
     532,   525,   219,   219,   155,   535,   156,   157,   158,   279,
     536,   545,   546,   159,   144,   616,   547,   549,   557,   558,
     560,   559,   561,   636,   562,   564,   583,   591,   592,   593,
     351,   596,   219,   597,   601,   160,   161,   603,   605,   162,
     163,   609,   617,   619,   622,    76,   623,   263,   632,   569,
     633,   659,   464,   641,   656,    78,   662,   663,   156,   157,
     158,   667,   219,   676,   219,   159,   235,   219,   678,   115,
     685,   686,   672,   548,   219,   689,   330,   563,   299,   396,
      28,   446,   600,   439,   516,   370,   219,   160,   266,   681,
     654,   219,   235,   436,   219,   690,   373,   666,   687,   640,
     219,   235,   577,   683,   688,   675,   219,   672,   219,   219,
      87,    39,    88,    89,    90,    91,   655,    92,   130,    40,
      93,   132,   590,    94,    95,    96,    97,    98,   457,    99,
     416,    42,   654,   100,   634,   101,    44,   658,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,    55,
      56,    57,   657,   415,    58,   661,   588,   103,    59,    60,
      61,    62,    63,    64,   104,   615,   526,   263,   152,   105,
     106,   382,   602,   531,   238,   364,     0,     0,   156,   157,
     158,     0,     0,   107,     0,   159,     0,     0,     0,   108,
       0,     0,     0,     0,     0,   109,     0,   110,   111,     0,
       0,     0,     0,   306,     0,     0,     0,   160,   266,     0,
     112,   162,   267,     0,   113,     0,   114,     0,     0,     0,
       0,     0,     0,     0,    66,   115,     0,     0,    68,   116,
       0,     0,     0,     0,   117,   118,   119,   120,     0,    87,
      39,    88,     0,     0,   121,   122,    92,     0,    40,    93,
       0,     0,    94,    95,    96,     0,    98,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,   103,    59,    60,    61,
      62,    63,    64,   104,     0,     0,   187,     0,   105,   106,
       0,     0,     0,   355,     0,   322,     0,   189,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,   108,     0,
       0,     0,     0,     0,   109,   594,   110,   111,     0,     0,
       0,   284,     0,     0,     0,     0,   198,   199,     0,   112,
     200,   201,     0,   113,     0,   114,     0,     0,     0,     0,
       0,     0,     0,    66,   115,     0,     0,    68,   116,     0,
       0,     0,     0,   117,   118,   119,   120,    22,     0,    87,
      39,    88,     0,   121,   122,     0,    92,     0,    40,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,   103,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,   105,   177,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   108,     0,
       0,     0,     0,     0,   109,     0,   110,   566,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    23,   112,
       0,     0,     0,   178,     0,   114,     0,     0,     0,   568,
       0,     0,     0,    66,   115,     0,     0,    68,   116,     0,
      87,    39,    88,   117,   118,   119,   120,    92,     0,    40,
      93,     0,     0,     0,     0,     0,     0,    98,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,   103,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,   105,
     177,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   263,     0,     0,   108,
       0,     0,     0,     0,     0,   109,     0,   282,   157,   158,
       0,     0,     0,     0,   159,     0,     0,     0,     0,     0,
     112,     0,     0,     0,   178,   283,   114,     0,     0,     0,
       0,   284,   265,     0,    66,   115,   160,   266,    68,   116,
     162,   267,     0,     0,   117,   118,   119,   120,    87,    39,
      88,     0,     0,     0,     0,    92,     0,    40,    93,     0,
       0,     0,     0,     0,     0,    98,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,   103,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,   105,   177,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   263,     0,     0,   108,     0,     0,
       0,     0,     0,   109,     0,   110,   157,   158,     0,     0,
       0,     0,   159,     0,     0,     0,     0,     0,   112,   264,
       0,     0,   178,     0,   114,     0,     0,     0,     0,     0,
     265,     0,    66,   115,   160,   266,    68,   116,   162,   267,
       0,     0,   117,   118,   119,   120,    87,    39,    88,     0,
       0,     0,     0,    92,     0,    40,    93,     0,     0,     0,
       0,     0,     0,    98,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,   103,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,   105,   177,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   263,     0,     0,   108,     0,     0,     0,     0,
       0,   109,     0,   110,   157,   158,     0,     0,     0,     0,
     159,     0,     0,     0,     0,     0,   112,     0,     0,     0,
     178,     0,   114,     0,     0,     0,     0,     0,   265,     0,
      66,   115,   160,   266,    68,   116,   162,   267,     0,     0,
     117,   118,   119,   120,    87,    39,    88,     0,     0,     0,
       0,    92,     0,    40,    93,     0,     0,     0,     0,     0,
       0,    98,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,   103,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,   105,   177,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   108,     0,     0,     0,     0,     0,   109,
       0,   110,   566,     0,     0,     0,     0,     0,     0,     0,
       0,   567,     0,     0,   112,     0,     0,     0,   178,     0,
     114,     0,     0,     0,   568,     0,     0,     0,    66,   115,
       0,     0,    68,   116,     0,    87,    39,    88,   117,   118,
     119,   120,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,   374,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,     0,   375,    58,
       0,     0,   103,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,   105,   177,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   108,     0,     0,     0,     0,     0,
     109,     0,   110,   376,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   112,     0,     0,     0,   178,
       0,   114,     0,     0,     0,     0,     0,     0,     0,    66,
     115,     0,     0,    68,   116,     0,    87,    39,    88,   117,
     118,   119,   120,    92,     0,    40,    93,     0,     0,     0,
       0,     0,     0,    98,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,   103,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,   105,   177,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   108,     0,     0,     0,     0,
       0,   109,     0,   110,   566,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   112,     0,     0,     0,
     178,     0,   114,     0,     0,     0,   568,     0,     0,     0,
      66,   115,     0,     0,    68,   116,     0,    87,    39,    88,
     117,   118,   119,   120,    92,     0,    40,    93,     0,     0,
       0,     0,     0,     0,   374,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,   103,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,   105,   177,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   108,     0,     0,     0,
       0,     0,   109,     0,   110,   376,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   112,     0,     0,
       0,   178,     0,   114,     0,     0,     0,     0,     0,     0,
       0,    66,   115,     0,     0,    68,   116,     0,    87,    39,
      88,   117,   118,   119,   120,    92,     0,    40,    93,     0,
       0,     0,     0,     0,     0,    98,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,   103,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,   105,   177,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   108,     0,     0,
       0,     0,     0,   109,     0,   110,   566,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   112,     0,
       0,     0,   178,     0,   114,     0,     0,     0,     0,     0,
       0,     0,    66,   115,     0,     0,    68,   116,     0,    87,
      39,    88,   117,   118,   119,   120,    92,     0,    40,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,   103,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,   105,   177,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   108,     0,
       0,     0,     0,     0,   109,     0,   110,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   112,
       0,     0,     0,   178,     0,   114,     0,     0,     0,     0,
       0,     0,     0,    66,   115,     0,     0,    68,   116,     0,
      87,    39,    88,   117,   118,   119,   120,    92,     0,    40,
      93,     0,     0,     0,     0,     0,     0,    98,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,   103,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,   105,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   108,
       0,     0,     0,     0,     0,   109,     0,   110,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     112,     0,     0,     0,   178,     0,   114,     0,     0,     0,
       0,     0,     0,     0,    66,   115,     0,     0,    68,   116,
       0,    87,    39,    88,   117,   118,   119,   120,    92,     0,
      40,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     108,     0,     0,     0,   414,     0,   109,     0,     0,   256,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   112,     0,     0,     0,   178,     0,   114,     0,     0,
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
     256,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   112,     0,     0,     0,   178,     0,   114,     0,
       0,     0,     0,     0,     0,     0,    66,   115,     0,     0,
      68,   116,     0,    87,    39,    88,   117,   118,   119,   120,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
     103,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   108,     0,     0,     0,     0,     0,   109,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   112,     0,     0,     0,   178,     0,   114,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,   317,
       0,     0,     0,     0,   112,     0,     0,     0,   178,     0,
     114,     0,     0,     0,     0,     0,     0,     0,    66,   115,
       0,     0,    68,   116,     0,    87,    39,    88,   117,   118,
     119,   120,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   108,     0,     0,     0,     0,     0,
     109,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   112,     0,     0,    39,   178,
       0,   114,     0,     0,     0,     0,    40,     0,     0,    66,
     115,     0,     0,    68,   116,     0,     0,     0,    42,   117,
     118,   119,   120,   350,     0,    45,    46,    47,   184,   185,
     186,     0,     0,     0,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,     0,     0,     0,     0,
     355,     0,   356,     0,   189,   190,   191,     0,     0,     0,
       0,     0,     0,   192,     0,     0,     0,   193,     0,    39,
       0,   194,   357,   195,     0,     0,     0,    40,   284,   196,
       0,   197,    67,   198,   199,     0,    69,   200,   201,    42,
       0,     0,     0,     0,   350,     0,    45,    46,    47,   184,
     185,   186,     0,     0,     0,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,     0,     0,
       0,     0,     0,   188,     0,   189,   190,   191,     0,     0,
       0,     0,     0,     0,   192,     0,     0,     0,   193,   351,
      39,     0,   194,     0,   195,     0,     0,     0,    40,     0,
     196,     0,   197,    67,   198,   199,     0,    69,   200,   201,
      42,     0,     0,     0,     0,   350,     0,    45,    46,    47,
     184,   185,   186,     0,     0,     0,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   187,     0,     0,     0,     0,
       0,     0,     0,     0,   188,     0,   189,   190,   191,     0,
       0,     0,     0,     0,     0,   192,     0,     0,     0,   193,
       0,    39,     0,   194,     0,   195,     0,     0,     0,    40,
       0,   196,     0,   197,    67,   198,   199,     0,    69,   200,
     201,    42,     0,     0,     0,     0,     0,     0,    45,    46,
      47,   184,   185,   186,     0,     0,     0,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   187,     0,     0,     0,
       0,     0,     0,     0,     0,   188,     0,   189,   190,   191,
       0,     0,     0,     0,     0,     0,   192,     0,     0,     0,
     193,     0,    39,     0,   194,   671,   195,     0,     0,     0,
      40,     0,   196,     0,   197,    67,   198,   199,     0,    69,
     200,   201,    42,     0,     0,     0,     0,     0,     0,    45,
      46,    47,   184,   185,   186,     0,     0,     0,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   187,     0,     0,
       0,     0,     0,     0,     0,     0,   188,     0,   189,   190,
     191,     0,     0,     0,     0,     0,     0,   192,     0,     0,
       0,   193,   417,    39,     0,   194,     0,   195,     0,     0,
       0,    40,     0,   196,     0,   197,    67,   198,   199,     0,
      69,   200,   201,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,    55,    56,    57,     0,     0,    58,     0,    39,     0,
      59,    60,    61,    62,    63,    64,    40,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    42,     0,
       0,     0,     0,     0,     0,    45,    46,    47,   184,   185,
     186,     0,     0,     0,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    66,   115,     0,     0,
      68,   116,     0,   263,     0,     0,     0,     0,     0,     0,
       0,     0,   188,  -125,     0,   190,   191,     0,     0,     0,
      39,     0,     0,   192,     0,     0,     0,   193,    40,     0,
       0,   194,     0,   195,     0,     0,     0,     0,     0,   668,
      42,   197,    67,     0,   266,     0,    69,    45,    46,    47,
     184,   185,   186,     0,     0,     0,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   263,     0,     0,     0,     0,
       0,     0,     0,     0,   188,     0,     0,   190,   191,     0,
       0,     0,     0,     0,     0,   192,     0,     0,     0,   193,
      39,     0,     0,   194,     0,   195,     0,     0,    40,     0,
       0,   668,     0,   197,    67,     0,   266,    41,    69,     0,
      42,     0,    43,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,    39,    51,    52,    53,    54,    55,    56,
      57,    40,     0,    58,     0,     0,     0,    59,    60,    61,
      62,    63,    64,    42,     0,    43,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,    39,     0,     0,     0,     0,     0,     0,     0,
      40,     0,     0,    65,     0,     0,     0,   331,     0,     0,
       0,     0,    42,    66,    67,     0,     0,    68,    69,    45,
      46,    47,   184,   185,   186,     0,     0,     0,    53,    54,
      55,    56,    57,     0,     0,    58,    65,     0,     0,    59,
      60,    61,    62,    63,    64,     0,    66,    67,     0,     0,
      68,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,     0,     0,   190,
     191,     0,     0,     0,    39,     0,     0,   192,     0,     0,
       0,   193,    40,     0,     0,   194,     0,   195,     0,     0,
       0,     0,     0,     0,    42,   197,    67,     0,     0,     0,
      69,    45,    46,    47,   184,   185,   186,    39,     0,     0,
      53,    54,    55,    56,    57,    40,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
       0,    51,    52,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,    39,     0,     0,     0,
       0,     0,     0,     0,    40,     0,     0,     0,     0,     0,
       0,   460,     0,     0,     0,     0,    42,   197,    67,     0,
       0,    44,    69,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
     248,    39,     0,    59,    60,    61,    62,    63,    64,    40,
      66,     0,     0,     0,    68,     0,     0,     0,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,   248,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    66,
       0,     0,     0,    68,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   156,   157,   158,
      39,     0,     0,     0,   159,     0,     0,     0,    40,     0,
       0,     0,     0,     0,   236,     0,     0,     0,     0,     0,
      42,     0,   237,     0,    66,    44,   160,    45,    46,    47,
      48,    49,    50,    39,    51,    52,    53,    54,    55,    56,
      57,    40,     0,    58,     0,     0,     0,    59,    60,    61,
      62,    63,    64,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
      39,     0,     0,     0,     0,     0,     0,     0,    40,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      42,     0,     0,    66,   115,    44,     0,    45,    46,    47,
      48,    49,    50,    39,    51,    52,    53,    54,    55,    56,
      57,    40,     0,    58,     0,     0,   236,    59,    60,    61,
      62,    63,    64,    42,     0,     0,    66,     0,     0,    39,
      45,    46,    47,   184,   185,   186,     0,    40,     0,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,    42,
      59,    60,    61,    62,    63,    64,    45,    46,    47,   184,
     185,   186,    39,     0,     0,    53,    54,    55,    56,    57,
      40,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,    42,    66,     0,     0,     0,     0,     0,    45,
      46,    47,   184,   185,   186,     0,   578,     0,    53,    54,
      55,    56,    57,     0,     0,    58,   579,     0,     0,    59,
      60,    61,    62,    63,    64,     0,   197,     0,     0,     0,
       0,     0,   665,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   579,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   197,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   197
  };

  const short
  parser::yycheck_[] =
  {
      29,    89,   107,    89,    88,    65,   100,   203,    34,    93,
      29,   151,   100,   103,   100,   112,   113,   251,   108,   109,
     107,   111,   114,    65,    41,    34,   170,   180,   112,   113,
     308,   305,   343,   102,    89,   192,   487,   101,   337,   552,
     110,   101,     5,     1,   134,     1,     1,   193,   194,   112,
     113,   480,   474,   113,   194,   112,   344,   132,    12,    21,
      88,   132,   113,    27,   298,   643,    27,    84,    19,   100,
     265,   113,    80,   265,   220,    77,   100,   117,    77,   100,
     643,   178,   113,   382,     0,   108,   104,   106,   128,   113,
     132,   114,   101,    77,   178,    22,   114,   396,    77,   183,
     108,   129,   194,   195,   682,   180,   684,   193,   194,   180,
     196,   306,   113,   391,   306,   178,   269,   203,   178,   682,
      84,   317,   208,    84,    51,    52,   128,   178,   579,   128,
     114,    80,   420,    79,   220,   114,   178,    99,   180,   568,
     169,   170,   564,   229,   102,    80,   102,   102,   422,   662,
     169,   170,   103,   107,    78,   113,   302,   113,    80,    80,
      84,   103,   108,   365,   108,   114,   256,   196,   114,   259,
     114,   261,   374,   451,   120,   107,   236,   274,   108,   114,
     107,   480,    81,   192,   114,    84,   128,    14,   248,   640,
     114,   118,   114,   114,   269,   122,   667,   128,   269,   670,
      90,    91,    92,   107,   230,   231,   248,    97,   294,   172,
     173,   108,   282,   301,   118,   301,   302,   114,   122,    77,
     304,   230,   231,   369,    80,    81,   245,   269,   237,   119,
     320,   317,    77,   123,   107,   546,   547,    27,   298,   303,
     118,    90,    91,    92,   122,   118,   301,    24,    97,   122,
     110,    79,   398,    81,   114,   110,    84,   343,    99,   114,
     101,   113,    90,    91,    92,    65,    66,   110,   110,    97,
     119,   114,   114,   426,   368,   574,   295,   296,    86,   108,
     368,    89,   368,   369,   558,   311,   376,   115,    99,   114,
     101,   119,   120,   103,   303,   123,   124,   107,   107,   109,
     319,    72,    73,    74,    75,   410,   375,    99,   118,   101,
     113,   107,   398,   397,    99,   549,   101,    84,    71,    86,
     395,   338,   596,   410,   395,   469,   423,   638,   425,   486,
     414,   428,   129,   430,   439,   121,   122,   421,   484,   423,
     103,   425,    99,   489,   428,   491,   430,   493,   128,   627,
     624,   426,    87,   395,   128,   426,   502,   103,   108,   108,
     423,   107,   425,   109,   108,   428,   423,   430,   425,   108,
     108,   428,   118,   430,    79,    80,   122,   108,     4,    23,
      79,   659,   114,    89,   426,    90,    91,    92,   528,    88,
      27,    90,    97,   113,    77,   479,   480,    77,   484,    77,
      77,   487,    15,   489,    86,   491,   552,   493,    81,   643,
     115,   129,   608,   129,   119,   120,   502,    77,   123,   124,
     119,   120,   506,   104,   123,   124,   510,   114,    80,   104,
     439,   128,   578,   104,   108,   114,   528,   529,   108,   108,
     469,   108,   108,   108,   100,   474,   114,   643,   682,    77,
     469,   100,    77,   114,    89,   474,   114,   108,   115,   114,
     546,   547,   115,   108,    85,   562,   552,    85,    25,   108,
     129,   667,   115,   557,   670,   104,   566,   486,   562,    77,
     128,   128,    77,   115,   568,    79,   682,   513,   684,   549,
      77,   108,   578,   579,    88,   115,    90,    91,    92,   562,
     115,    77,    80,    97,   513,   562,    80,   114,    81,   114,
     100,   115,   114,   597,    81,   107,   662,   128,   128,   104,
     104,   114,   608,    78,   115,   119,   120,    77,    29,   123,
     124,     9,   114,   108,   100,   564,   113,    79,   108,   623,
     108,   625,   561,    84,   108,   564,    80,    11,    90,    91,
      92,    89,   638,    53,   640,    97,   650,   643,    86,   118,
     114,   108,   650,   668,   650,   108,   169,   469,   124,   231,
      15,   301,   547,   115,   398,   208,   662,   119,   120,   663,
     609,   667,   676,   294,   670,   685,   220,   641,   676,   605,
     676,   685,   486,   667,   678,   651,   682,   685,   684,   685,
       3,     4,     5,     6,     7,     8,   611,    10,    34,    12,
      13,    34,   513,    16,    17,    18,    19,    20,   308,    22,
     262,    24,   651,    26,   589,    28,    29,   624,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,   623,   259,    47,   627,   508,    50,    51,    52,
      53,    54,    55,    56,    57,   561,   410,    79,    43,    62,
      63,   222,   549,   411,   101,   196,    -1,    -1,    90,    91,
      92,    -1,    -1,    76,    -1,    97,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,    -1,
      -1,    -1,    -1,   115,    -1,    -1,    -1,   119,   120,    -1,
     103,   123,   124,    -1,   107,    -1,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,    -1,    -1,    -1,   127,   128,   129,   130,    -1,     3,
       4,     5,    -1,    -1,   137,   138,    10,    -1,    12,    13,
      -1,    -1,    16,    17,    18,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    -1,    79,    -1,    62,    63,
      -1,    -1,    -1,    86,    -1,    88,    -1,    90,    -1,    -1,
      -1,    -1,    76,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,   108,    90,    91,    -1,    -1,
      -1,   114,    -1,    -1,    -1,    -1,   119,   120,    -1,   103,
     123,   124,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,
      -1,    -1,    -1,   127,   128,   129,   130,     1,    -1,     3,
       4,     5,    -1,   137,   138,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,   103,
      -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,   113,
      -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,
       3,     4,     5,   127,   128,   129,   130,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,
      -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,   107,   108,   109,    -1,    -1,    -1,
      -1,   114,   115,    -1,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,    -1,   127,   128,   129,   130,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,    -1,
      -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,   103,   104,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
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
      97,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
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
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,   113,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,     3,     4,     5,   127,   128,
     129,   130,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    46,    47,
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
     107,    -1,   109,    -1,    -1,    -1,   113,    -1,    -1,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,     3,     4,     5,
     127,   128,   129,   130,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
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
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
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
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
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
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    -1,    -1,
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
      82,    -1,    -1,    -1,    86,    -1,    88,    -1,    -1,    91,
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
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,     3,     4,     5,   127,   128,   129,   130,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,     3,     4,     5,   127,   128,
     129,   130,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,     4,   107,
      -1,   109,    -1,    -1,    -1,    -1,    12,    -1,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,    -1,    -1,    24,   127,
     128,   129,   130,    29,    -1,    31,    32,    33,    34,    35,
      36,    -1,    -1,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      86,    -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,   103,    -1,     4,
      -1,   107,   108,   109,    -1,    -1,    -1,    12,   114,   115,
      -1,   117,   118,   119,   120,    -1,   122,   123,   124,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    -1,    -1,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,   104,
       4,    -1,   107,    -1,   109,    -1,    -1,    -1,    12,    -1,
     115,    -1,   117,   118,   119,   120,    -1,   122,   123,   124,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    -1,    -1,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,
      -1,     4,    -1,   107,    -1,   109,    -1,    -1,    -1,    12,
      -1,   115,    -1,   117,   118,   119,   120,    -1,   122,   123,
     124,    24,    -1,    -1,    -1,    -1,    -1,    -1,    31,    32,
      33,    34,    35,    36,    -1,    -1,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,
     103,    -1,     4,    -1,   107,   108,   109,    -1,    -1,    -1,
      12,    -1,   115,    -1,   117,   118,   119,   120,    -1,   122,
     123,   124,    24,    -1,    -1,    -1,    -1,    -1,    -1,    31,
      32,    33,    34,    35,    36,    -1,    -1,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
      -1,   103,     3,     4,    -1,   107,    -1,   109,    -1,    -1,
      -1,    12,    -1,   115,    -1,   117,   118,   119,   120,    -1,
     122,   123,   124,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    -1,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,     4,    -1,
      51,    52,    53,    54,    55,    56,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    -1,    -1,    31,    32,    33,    34,    35,
      36,    -1,    -1,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    88,    89,    -1,    91,    92,    -1,    -1,    -1,
       4,    -1,    -1,    99,    -1,    -1,    -1,   103,    12,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,   115,
      24,   117,   118,    -1,   120,    -1,   122,    31,    32,    33,
      34,    35,    36,    -1,    -1,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,
       4,    -1,    -1,   107,    -1,   109,    -1,    -1,    12,    -1,
      -1,   115,    -1,   117,   118,    -1,   120,    21,   122,    -1,
      24,    -1,    26,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,     4,    38,    39,    40,    41,    42,    43,
      44,    12,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    24,    -1,    26,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    -1,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      12,    -1,    -1,   107,    -1,    -1,    -1,    78,    -1,    -1,
      -1,    -1,    24,   117,   118,    -1,    -1,   121,   122,    31,
      32,    33,    34,    35,    36,    -1,    -1,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    47,   107,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,
      92,    -1,    -1,    -1,     4,    -1,    -1,    99,    -1,    -1,
      -1,   103,    12,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    24,   117,   118,    -1,    -1,    -1,
     122,    31,    32,    33,    34,    35,    36,     4,    -1,    -1,
      40,    41,    42,    43,    44,    12,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      -1,    38,    39,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    78,    -1,    -1,    -1,    -1,    24,   117,   118,    -1,
      -1,    29,   122,    31,    32,    33,    34,    35,    36,    -1,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
     107,     4,    -1,    51,    52,    53,    54,    55,    56,    12,
     117,    -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    -1,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,
      -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    92,
       4,    -1,    -1,    -1,    97,    -1,    -1,    -1,    12,    -1,
      -1,    -1,    -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,
      24,    -1,   115,    -1,   117,    29,   119,    31,    32,    33,
      34,    35,    36,     4,    38,    39,    40,    41,    42,    43,
      44,    12,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    -1,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
       4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      24,    -1,    -1,   117,   118,    29,    -1,    31,    32,    33,
      34,    35,    36,     4,    38,    39,    40,    41,    42,    43,
      44,    12,    -1,    47,    -1,    -1,   107,    51,    52,    53,
      54,    55,    56,    24,    -1,    -1,   117,    -1,    -1,     4,
      31,    32,    33,    34,    35,    36,    -1,    12,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    24,
      51,    52,    53,    54,    55,    56,    31,    32,    33,    34,
      35,    36,     4,    -1,    -1,    40,    41,    42,    43,    44,
      12,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    24,   117,    -1,    -1,    -1,    -1,    -1,    31,
      32,    33,    34,    35,    36,    -1,    97,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    47,   107,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,   117,    -1,    -1,    -1,
      -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   117
  };

  const short
  parser::yystos_[] =
  {
       0,    21,    99,   140,   141,   142,   145,   118,   122,   316,
     146,   159,     0,   146,    65,    66,   143,   100,   113,   147,
     160,   161,     1,   102,   315,   103,   128,   187,   187,   107,
     148,    14,   162,   170,   171,   128,   188,    77,    77,     4,
      12,    21,    24,    26,    29,    31,    32,    33,    34,    35,
      36,    38,    39,    40,    41,    42,    43,    44,    47,    51,
      52,    53,    54,    55,    56,   107,   117,   118,   121,   122,
     149,   150,   151,   156,   157,   282,   285,   286,   300,   301,
     302,   308,    27,    24,   163,   113,   158,     3,     5,     6,
       7,     8,    10,    13,    16,    17,    18,    19,    20,    22,
      26,    28,    37,    50,    57,    62,    63,    76,    82,    88,
      90,    91,   103,   107,   109,   118,   122,   127,   128,   129,
     130,   137,   138,   168,   172,   173,   174,   175,   179,   192,
     227,   232,   237,   238,   242,   243,   244,   245,   271,   272,
     275,   276,   299,   300,   302,   310,   311,   314,   104,   114,
     316,   107,   281,   285,    79,    88,    90,    91,    92,    97,
     119,   120,   123,   124,   305,   306,   307,   309,   108,   114,
     107,   152,    99,   101,   144,   316,   113,    63,   107,   235,
     236,   238,   239,   241,    34,    35,    36,    79,    88,    90,
      91,    92,    99,   103,   107,   109,   115,   117,   119,   120,
     123,   124,   181,   194,   195,   198,   200,   202,   204,   205,
     207,   280,   281,   283,   287,   288,   296,   297,   298,   308,
     107,    99,   101,   264,   235,    72,    73,    74,    75,   176,
      99,   101,   184,   185,   200,   202,   107,   115,   290,   299,
     302,   306,   264,   243,   103,   233,   234,   128,   107,   300,
     103,   107,   273,   274,   276,   311,    91,   243,   262,   263,
     243,   242,   243,    79,   104,   115,   120,   124,   235,   236,
     246,   248,   249,   278,   292,   293,   295,   304,   305,   307,
     312,   313,    90,   108,   114,   246,   247,   305,   306,   307,
     312,   317,   110,   317,    19,   233,   233,   129,   167,   158,
      71,   182,    80,   114,    81,    84,   115,   229,   230,   231,
     278,   291,   293,   294,   303,   305,   306,    98,   243,    99,
      87,   128,    88,   287,   108,   108,   108,   108,   108,   108,
     151,    78,   153,   154,   155,   156,   146,   146,     4,   164,
     117,   128,    23,    80,   291,   235,   192,   220,   221,   222,
      29,   104,   196,   198,   200,    86,    88,   108,   196,   211,
     287,   317,   317,   285,   298,    27,   186,   207,    89,    86,
     205,   196,   210,   211,    20,    46,    91,   235,   261,   265,
     266,   267,   265,   113,   240,    77,    77,    77,    77,   190,
     196,   208,   183,   227,   228,   237,   183,    15,    81,   306,
     302,   129,   129,    88,   129,   300,    77,    77,   128,   313,
     114,    80,   189,   243,    86,   262,   229,     3,   301,   310,
     291,    78,    84,   114,   104,   114,   236,   108,   114,   108,
     114,   108,   108,   108,   114,   110,   208,   300,   300,   115,
     169,   277,   289,   290,   313,   128,   181,   191,   196,   197,
     299,   235,   251,   252,   267,   301,   186,   231,   238,   207,
      78,   268,   269,   270,   300,   243,   108,   108,   108,   114,
     100,   315,   316,    12,   107,   165,   166,    77,    77,    99,
     101,   253,   190,   239,    80,   100,   114,   212,   104,    89,
     108,    80,   108,   114,   108,   110,   115,   115,   185,   200,
     196,   108,   114,   185,   264,   243,    85,   100,   113,   315,
      25,   186,   100,   113,   315,   235,   197,   108,   115,   128,
     128,   129,   104,    77,    77,   108,   274,   103,   107,   109,
     279,   280,    77,   235,   115,   115,   115,   235,   250,   267,
     235,   246,   246,   246,   246,    77,    80,    80,   311,   114,
      77,   128,    80,    81,   180,   215,   186,    81,   114,   115,
     100,   114,    81,   155,   107,   149,    91,   100,   113,   235,
     254,   255,   256,   260,   254,   315,   196,   222,    97,   107,
     213,   296,   196,   196,   214,   196,   196,   235,   266,   235,
     228,   128,   128,   104,   108,   110,   114,    78,   190,   193,
     193,   115,   289,    77,   214,    29,   216,   217,   218,     9,
     223,   224,   225,   235,   267,   269,   246,   114,   149,   108,
     243,   254,   100,   113,    84,    86,   257,   258,   259,   315,
     196,   296,   108,   108,   240,   267,   235,    77,   114,    77,
     212,    84,   199,   203,   206,   207,   219,    22,    51,    52,
     107,   177,   226,   284,   285,   225,   108,   256,   251,   235,
     186,   259,    80,    11,   190,    97,   217,    89,   115,   206,
     277,   108,   200,   201,   209,   226,    53,   178,    86,   186,
     214,   235,   203,   219,   203,   114,   108,   200,   235,   108,
     209
  };

  const short
  parser::yyr1_[] =
  {
       0,   139,   140,   141,   141,   142,   143,   143,   143,   144,
     144,   145,   145,   146,   147,   147,   147,   148,   148,   149,
     150,   150,   151,   151,   152,   152,   153,   153,   154,   154,
     155,   155,   156,   156,   157,   157,   158,   158,   159,   159,
     160,   161,   161,   162,   163,   163,   164,   164,   165,   165,
     166,   166,   167,   167,   168,   168,   168,   169,   169,   170,
     171,   171,   172,   172,   172,   172,   172,   172,   172,   172,
     172,   172,   173,   174,   174,   174,   175,   176,   176,   176,
     176,   176,   177,   177,   177,   178,   179,   179,   180,   180,
     181,   181,   182,   182,   182,   183,   183,   183,   183,   184,
     184,   185,   186,   186,   187,   187,   188,   188,   188,   189,
     189,   190,   191,   192,   192,   193,   193,   194,   195,   195,
     196,   196,   196,   197,   198,   199,   200,   200,   201,   202,
     203,   203,   204,   204,   205,   205,   205,   206,   207,   207,
     207,   207,   207,   207,   207,   207,   207,   207,   208,   209,
     209,   210,   210,   211,   211,   212,   212,   213,   213,   214,
     215,   216,   216,   217,   217,   218,   218,   219,   219,   220,
     220,   221,   221,   222,   223,   223,   224,   224,   225,   225,
     225,   226,   226,   226,   227,   227,   227,   228,   229,   229,
     230,   230,   231,   232,   232,   232,   232,   232,   232,   232,
     232,   232,   233,   233,   234,   234,   235,   235,   236,   236,
     237,   237,   238,   238,   238,   239,   239,   240,   240,   241,
     241,   242,   242,   242,   242,   243,   243,   243,   243,   243,
     243,   243,   243,   243,   244,   244,   245,   245,   245,   245,
     245,   245,   245,   246,   246,   246,   247,   247,   248,   248,
     248,   248,   248,   248,   248,   249,   249,   250,   250,   251,
     252,   252,   253,   253,   253,   253,   254,   254,   255,   255,
     255,   256,   257,   257,   258,   258,   259,   260,   260,   261,
     261,   262,   262,   263,   263,   264,   264,   265,   265,   265,
     265,   266,   266,   267,   267,   267,   268,   268,   269,   269,
     269,   270,   270,   271,   271,   272,   272,   273,   273,   273,
     274,   274,   275,   275,   275,   275,   276,   276,   277,   277,
     278,   278,   279,   279,   279,   280,   280,   280,   280,   280,
     281,   281,   281,   282,   282,   282,   282,   282,   283,   283,
     284,   285,   285,   286,   287,   287,   287,   288,   288,   288,
     288,   289,   289,   290,   290,   291,   291,   291,   292,   292,
     292,   293,   294,   294,   295,   295,   296,   297,   298,   298,
     298,   298,   298,   299,   299,   300,   300,   300,   301,   301,
     302,   302,   302,   302,   302,   302,   302,   302,   303,   303,
     304,   304,   305,   306,   306,   307,   307,   308,   308,   308,
     308,   308,   308,   308,   308,   308,   308,   308,   308,   308,
     308,   308,   308,   308,   308,   309,   309,   309,   310,   310,
     311,   312,   312,   313,   313,   314,   314,   314,   314,   315,
     315,   316,   316,   317,   317
  };

  const signed char
  parser::yyr2_[] =
  {
       0,     2,     1,     6,     1,     0,     3,     3,     0,     3,
       3,     3,     3,     2,     2,     2,     1,     3,     0,     1,
       3,     1,     2,     2,     0,     3,     0,     1,     4,     1,
       1,     1,     1,     2,     1,     1,     2,     1,     2,     0,
       2,     3,     0,     5,     1,     0,     2,     0,     1,     0,
       3,     4,     0,     1,     1,     1,     1,     3,     1,     2,
       3,     0,     1,     1,     1,     4,     1,     1,     5,     4,
       5,     4,     3,     4,     5,     4,     4,     2,     2,     2,
       2,     0,     1,     1,     1,     2,     1,     1,     0,     2,
       3,     1,     4,     3,     0,     3,     2,     1,     0,     3,
       3,     1,     2,     0,     1,     3,     3,     1,     0,     0,
       2,     1,     1,     3,     1,     1,     3,     1,     1,     1,
       4,     3,     1,     1,     1,     1,     1,     3,     1,     1,
       1,     2,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     2,     3,     2,     5,     3,     3,     5,     1,     1,
       3,     1,     0,     1,     3,     2,     0,     1,     5,     1,
       2,     3,     1,     4,     2,     3,     0,     1,     3,     0,
       1,     3,     1,     3,     0,     1,     2,     1,     2,     3,
       3,     1,     2,     3,     1,     3,     2,     1,     3,     2,
       2,     1,     4,     3,     3,     4,     4,     3,     4,     6,
       6,     4,     0,     1,     3,     4,     3,     1,     1,     3,
       1,     3,     2,     3,     1,     1,     2,     1,     0,     3,
       3,     2,     3,     2,     1,     3,     2,     4,     4,     8,
       4,     2,     2,     1,     4,     1,     1,     1,     1,     3,
       3,     3,     1,     1,     2,     2,     3,     3,     1,     1,
       2,     4,     3,     5,     3,     3,     3,     3,     1,     1,
       3,     1,     3,     3,     2,     2,     1,     2,     3,     2,
       1,     2,     3,     2,     2,     1,     4,     1,     2,     1,
       2,     1,     2,     2,     1,     3,     3,     3,     2,     1,
       0,     1,     2,     3,     1,     2,     1,     0,     3,     1,
       1,     3,     1,     1,     1,     1,     3,     1,     3,     1,
       1,     3,     2,     3,     2,     3,     1,     2,     1,     3,
       1,     3,     1,     2,     2,     1,     3,     3,     3,     2,
       1,     3,     3,     1,     3,     3,     3,     3,     1,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     1,     1,     1,     1,     1,
       1,     3,     1,     3,     1,     3,     1,     3,     1,     1,
       1,     1,     1,     1,     3,     1,     3,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     1
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
  "importdecls_semi", "importdecl", "optqualified", "maybeas",
  "maybeimpspec", "impspec", "prec", "infix", "ops", "topdecls",
  "topdecls_semi", "topdecl", "cl_decl", "ty_decl", "inst_decl",
  "overlap_pragma", "deriv_strategy_no_via", "deriv_strategy_via",
  "data_or_newtype", "opt_kind_sig", "tycl_hdr", "capi_ctype", "decls",
  "decllist", "binds", "wherebinds", "strings", "stringlist",
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
     607,   609,   610,   612,   625,   626,   628,   629,   631,   632,
     634,   635,   640,   641,   643,   644,   645,   647,   648,   652,
     654,   655,   657,   658,   659,   662,   669,   671,   672,   673,
     674,   675,   677,   679,   680,   681,   686,   691,   692,   693,
     694,   695,   697,   698,   699,   701,   741,   742,   744,   745,
     754,   755,   757,   758,   759,   803,   804,   805,   806,   808,
     809,   811,   813,   814,   822,   823,   825,   826,   827,   840,
     841,   843,   845,   847,   848,   850,   851,   855,   861,   862,
     869,   870,   872,   874,   883,   885,   887,   888,   890,   893,
     895,   896,   898,   899,   901,   902,   903,   909,   916,   917,
     918,   919,   920,   921,   922,   928,   929,   930,   933,   935,
     936,   938,   939,   941,   942,   949,   950,   952,   953,   971,
     977,   979,   980,   982,   983,   985,   986,   988,   989,   991,
     992,   994,   995,   997,   999,  1000,  1002,  1003,  1005,  1006,
    1007,  1009,  1010,  1011,  1016,  1018,  1026,  1031,  1035,  1036,
    1038,  1039,  1043,  1053,  1054,  1056,  1057,  1058,  1059,  1060,
    1061,  1062,  1065,  1066,  1068,  1069,  1073,  1074,  1076,  1077,
    1079,  1080,  1082,  1083,  1084,  1086,  1087,  1090,  1091,  1093,
    1094,  1098,  1099,  1100,  1101,  1103,  1104,  1105,  1106,  1108,
    1110,  1111,  1112,  1114,  1116,  1117,  1119,  1120,  1121,  1122,
    1123,  1128,  1129,  1134,  1135,  1136,  1141,  1142,  1160,  1161,
    1162,  1163,  1164,  1165,  1166,  1168,  1169,  1182,  1184,  1194,
    1196,  1197,  1200,  1201,  1202,  1203,  1205,  1206,  1208,  1209,
    1210,  1212,  1214,  1215,  1217,  1218,  1227,  1229,  1230,  1232,
    1233,  1235,  1236,  1238,  1239,  1242,  1243,  1245,  1246,  1247,
    1248,  1253,  1254,  1256,  1257,  1258,  1263,  1264,  1266,  1267,
    1268,  1270,  1271,  1303,  1304,  1306,  1307,  1309,  1310,  1311,
    1313,  1314,  1316,  1317,  1318,  1319,  1321,  1322,  1324,  1325,
    1327,  1328,  1331,  1332,  1333,  1335,  1336,  1337,  1338,  1339,
    1341,  1342,  1343,  1345,  1346,  1347,  1348,  1349,  1352,  1353,
    1355,  1357,  1358,  1362,  1364,  1365,  1366,  1368,  1369,  1370,
    1371,  1376,  1377,  1379,  1380,  1382,  1383,  1384,  1386,  1387,
    1388,  1390,  1392,  1393,  1395,  1396,  1400,  1402,  1404,  1405,
    1406,  1407,  1408,  1411,  1412,  1414,  1415,  1416,  1418,  1419,
    1421,  1422,  1423,  1424,  1425,  1426,  1427,  1428,  1430,  1431,
    1433,  1434,  1436,  1438,  1439,  1441,  1442,  1444,  1445,  1446,
    1447,  1448,  1449,  1450,  1451,  1452,  1453,  1454,  1455,  1456,
    1457,  1458,  1459,  1460,  1461,  1463,  1464,  1465,  1469,  1470,
    1472,  1474,  1475,  1477,  1478,  1482,  1483,  1484,  1485,  1490,
    1493,  1497,  1498,  1500,  1501
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
#line 5899 "parser.cc"

#line 1510 "parser.y"


using std::optional;
using std::string;
using std::vector;
using std::pair;

void
yy::parser::error (const location_type& l, const std::string& m)
{
    drv.push_error_message({l,m});
}

Haskell::Module make_module(const std::string& name, const std::optional<std::vector<expression_ref>>& exports, const std::vector<Haskell::ImpDecl>& impdecls, const std::optional<Haskell::Decls>& topdecls)
{
    return {name, exports, impdecls, topdecls};
}

pair<vector<Haskell::ImpDecl>, optional<Haskell::Decls>> make_body(const std::vector<Haskell::ImpDecl>& imports, const std::optional<Haskell::Decls>& topdecls)
{
    if (topdecls)
    {
        auto topdecls2 = Haskell::Decls(*topdecls, true);
        return {imports, topdecls2};
    }
    else
        return {imports, {}};
}

Haskell::BuiltinDecl make_builtin_expr(const string& name, int args, const string& s1, const string& s2)
{
    return {name, args, s1, s2};
}

Haskell::BuiltinDecl make_builtin_expr(const string& name, int args, const string& s1)
{
    return {name, args, s1};
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
    auto name = unloc(type_head.as_<Haskell::TypeVar>().name);

    return {name, type_args};
}

Haskell::FieldDecl make_field_decl(const std::vector<Haskell::Var>& field_names, const Haskell::Type& type)
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
        return (unloc(V.name) == "*");
    }
    else if (kind_args.size() == 2)
    {
        return (unloc(V.name) == "->") and check_kind(kind_args[0]) and check_kind(kind_args[1]);
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

Haskell::TypeVar make_type_var(const Located<string>& id)
{
    return {id};
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

    return {{unloc(head.as_<Haskell::TypeVar>().name), args[0].as_<Haskell::FieldDecls>()}};
}

optional<pair<string, std::vector<expression_ref>>> is_normal_con(const expression_ref& typeish)
{
    if (is_record_con(typeish)) return {};

    auto [head,args] = Haskell::decompose_type_apps(typeish);

    if (not head.is_a<Haskell::TypeVar>())
        return {};

    return {{unloc(head.as_<Haskell::TypeVar>().name), args}};
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

Haskell::FixityDecl make_fixity_decl(const Haskell::Fixity& fixity, optional<int>& prec, const vector<string>& op_names)
{
    return {fixity, prec, op_names};
}

Haskell::TypeDecl make_type_decl(const std::vector<Haskell::Var>& vars, Haskell::Type& type)
{
    return {vars, type};
}

expression_ref yy_make_string(const std::string& s)
{
    vector<expression_ref> chars;
    for(char c: s)
	chars.push_back(c);
    return make_list(chars);
}

