// A Bison parser, made by GNU Bison 3.8.2.

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
#line 52 "parser.y"

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

  /*---------.
  | symbol.  |
  `---------*/



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
        value.YY_MOVE_OR_COPY< Hs::Alts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_constr: // constr
        value.YY_MOVE_OR_COPY< Hs::Constructor > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        value.YY_MOVE_OR_COPY< Hs::Context > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        value.YY_MOVE_OR_COPY< Hs::DataOrNewtype > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
      case symbol_kind::S_decllist: // decllist
        value.YY_MOVE_OR_COPY< Hs::Decls > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_export: // export
        value.YY_MOVE_OR_COPY< Hs::Export > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.YY_MOVE_OR_COPY< Hs::FieldDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infix: // infix
        value.YY_MOVE_OR_COPY< Hs::Fixity > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.YY_MOVE_OR_COPY< Hs::GuardedRHS > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.YY_MOVE_OR_COPY< Hs::ImpDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_impspec: // impspec
        value.YY_MOVE_OR_COPY< Hs::ImpSpec > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
        value.YY_MOVE_OR_COPY< Hs::InfixExp > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_module: // module
        value.YY_MOVE_OR_COPY< Hs::Module > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_alt_rhs: // alt_rhs
        value.YY_MOVE_OR_COPY< Hs::MultiGuardedRHS > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.YY_MOVE_OR_COPY< Hs::Stmts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_strict_mark: // strict_mark
      case symbol_kind::S_strictness: // strictness
        value.YY_MOVE_OR_COPY< Hs::StrictLazy > (YY_MOVE (that.value));
        break;

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
      case symbol_kind::S_constr_stuff: // constr_stuff
        value.YY_MOVE_OR_COPY< Hs::Type > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
        value.YY_MOVE_OR_COPY< Hs::TypeVar > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alt: // alt
        value.YY_MOVE_OR_COPY< Located<Hs::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_binds: // binds
        value.YY_MOVE_OR_COPY< Located<Hs::Binds> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_qcname: // qcname
        value.YY_MOVE_OR_COPY< Located<std::string> > (YY_MOVE (that.value));
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

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
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

      case symbol_kind::S_export_subspec: // export_subspec
        value.YY_MOVE_OR_COPY< std::optional<Hs::ExportSubSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.YY_MOVE_OR_COPY< std::optional<Hs::ImpSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.YY_MOVE_OR_COPY< std::optional<Located<Hs::Binds>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_prec: // prec
        value.YY_MOVE_OR_COPY< std::optional<int> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.YY_MOVE_OR_COPY< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.YY_MOVE_OR_COPY< std::optional<std::vector<Hs::Export>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.YY_MOVE_OR_COPY< std::pair<Hs::Context,Hs::Type> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.YY_MOVE_OR_COPY< std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > (YY_MOVE (that.value));
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
        value.YY_MOVE_OR_COPY< std::vector<Hs::Constructor> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        value.YY_MOVE_OR_COPY< std::vector<Hs::Export> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.YY_MOVE_OR_COPY< std::vector<Hs::FieldDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.YY_MOVE_OR_COPY< std::vector<Hs::GuardedRHS> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.YY_MOVE_OR_COPY< std::vector<Hs::ImpDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
        value.YY_MOVE_OR_COPY< std::vector<Hs::Type> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_forall: // forall
        value.YY_MOVE_OR_COPY< std::vector<Hs::TypeVar> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.YY_MOVE_OR_COPY< std::vector<Hs::Var> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.YY_MOVE_OR_COPY< std::vector<Located<Hs::Alt>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
        value.YY_MOVE_OR_COPY< std::vector<Located<std::string>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decls: // decls
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
        value.move< Hs::Alts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_constr: // constr
        value.move< Hs::Constructor > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        value.move< Hs::Context > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        value.move< Hs::DataOrNewtype > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
      case symbol_kind::S_decllist: // decllist
        value.move< Hs::Decls > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_export: // export
        value.move< Hs::Export > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.move< Hs::FieldDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infix: // infix
        value.move< Hs::Fixity > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.move< Hs::GuardedRHS > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.move< Hs::ImpDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_impspec: // impspec
        value.move< Hs::ImpSpec > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
        value.move< Hs::InfixExp > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_module: // module
        value.move< Hs::Module > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_alt_rhs: // alt_rhs
        value.move< Hs::MultiGuardedRHS > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.move< Hs::Stmts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_strict_mark: // strict_mark
      case symbol_kind::S_strictness: // strictness
        value.move< Hs::StrictLazy > (YY_MOVE (that.value));
        break;

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
      case symbol_kind::S_constr_stuff: // constr_stuff
        value.move< Hs::Type > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
        value.move< Hs::TypeVar > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alt: // alt
        value.move< Located<Hs::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_binds: // binds
        value.move< Located<Hs::Binds> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_qcname: // qcname
        value.move< Located<std::string> > (YY_MOVE (that.value));
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

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
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

      case symbol_kind::S_export_subspec: // export_subspec
        value.move< std::optional<Hs::ExportSubSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.move< std::optional<Hs::ImpSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.move< std::optional<Located<Hs::Binds>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_prec: // prec
        value.move< std::optional<int> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.move< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.move< std::optional<std::vector<Hs::Export>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.move< std::pair<Hs::Context,Hs::Type> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.move< std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > (YY_MOVE (that.value));
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
        value.move< std::vector<Hs::Constructor> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        value.move< std::vector<Hs::Export> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.move< std::vector<Hs::FieldDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.move< std::vector<Hs::GuardedRHS> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.move< std::vector<Hs::ImpDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
        value.move< std::vector<Hs::Type> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_forall: // forall
        value.move< std::vector<Hs::TypeVar> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.move< std::vector<Hs::Var> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Located<Hs::Alt>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
        value.move< std::vector<Located<std::string>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decls: // decls
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
        value.copy< Hs::Alts > (that.value);
        break;

      case symbol_kind::S_constr: // constr
        value.copy< Hs::Constructor > (that.value);
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        value.copy< Hs::Context > (that.value);
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        value.copy< Hs::DataOrNewtype > (that.value);
        break;

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
      case symbol_kind::S_decllist: // decllist
        value.copy< Hs::Decls > (that.value);
        break;

      case symbol_kind::S_export: // export
        value.copy< Hs::Export > (that.value);
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.copy< Hs::FieldDecl > (that.value);
        break;

      case symbol_kind::S_infix: // infix
        value.copy< Hs::Fixity > (that.value);
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.copy< Hs::GuardedRHS > (that.value);
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.copy< Hs::ImpDecl > (that.value);
        break;

      case symbol_kind::S_impspec: // impspec
        value.copy< Hs::ImpSpec > (that.value);
        break;

      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
        value.copy< Hs::InfixExp > (that.value);
        break;

      case symbol_kind::S_module: // module
        value.copy< Hs::Module > (that.value);
        break;

      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_alt_rhs: // alt_rhs
        value.copy< Hs::MultiGuardedRHS > (that.value);
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.copy< Hs::Stmts > (that.value);
        break;

      case symbol_kind::S_strict_mark: // strict_mark
      case symbol_kind::S_strictness: // strictness
        value.copy< Hs::StrictLazy > (that.value);
        break;

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
      case symbol_kind::S_constr_stuff: // constr_stuff
        value.copy< Hs::Type > (that.value);
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
        value.copy< Hs::TypeVar > (that.value);
        break;

      case symbol_kind::S_alt: // alt
        value.copy< Located<Hs::Alt> > (that.value);
        break;

      case symbol_kind::S_binds: // binds
        value.copy< Located<Hs::Binds> > (that.value);
        break;

      case symbol_kind::S_qcname: // qcname
        value.copy< Located<std::string> > (that.value);
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

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
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

      case symbol_kind::S_export_subspec: // export_subspec
        value.copy< std::optional<Hs::ExportSubSpec> > (that.value);
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.copy< std::optional<Hs::ImpSpec> > (that.value);
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.copy< std::optional<Located<Hs::Binds>> > (that.value);
        break;

      case symbol_kind::S_prec: // prec
        value.copy< std::optional<int> > (that.value);
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.copy< std::optional<std::string> > (that.value);
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.copy< std::optional<std::vector<Hs::Export>> > (that.value);
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.copy< std::pair<Hs::Context,Hs::Type> > (that.value);
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.copy< std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > (that.value);
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
        value.copy< std::vector<Hs::Constructor> > (that.value);
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        value.copy< std::vector<Hs::Export> > (that.value);
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.copy< std::vector<Hs::FieldDecl> > (that.value);
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.copy< std::vector<Hs::GuardedRHS> > (that.value);
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.copy< std::vector<Hs::ImpDecl> > (that.value);
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
        value.copy< std::vector<Hs::Type> > (that.value);
        break;

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_forall: // forall
        value.copy< std::vector<Hs::TypeVar> > (that.value);
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.copy< std::vector<Hs::Var> > (that.value);
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.copy< std::vector<Located<Hs::Alt>> > (that.value);
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
        value.copy< std::vector<Located<std::string>> > (that.value);
        break;

      case symbol_kind::S_decls: // decls
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
        value.move< Hs::Alts > (that.value);
        break;

      case symbol_kind::S_constr: // constr
        value.move< Hs::Constructor > (that.value);
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        value.move< Hs::Context > (that.value);
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        value.move< Hs::DataOrNewtype > (that.value);
        break;

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
      case symbol_kind::S_decllist: // decllist
        value.move< Hs::Decls > (that.value);
        break;

      case symbol_kind::S_export: // export
        value.move< Hs::Export > (that.value);
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.move< Hs::FieldDecl > (that.value);
        break;

      case symbol_kind::S_infix: // infix
        value.move< Hs::Fixity > (that.value);
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.move< Hs::GuardedRHS > (that.value);
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.move< Hs::ImpDecl > (that.value);
        break;

      case symbol_kind::S_impspec: // impspec
        value.move< Hs::ImpSpec > (that.value);
        break;

      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
        value.move< Hs::InfixExp > (that.value);
        break;

      case symbol_kind::S_module: // module
        value.move< Hs::Module > (that.value);
        break;

      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_alt_rhs: // alt_rhs
        value.move< Hs::MultiGuardedRHS > (that.value);
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.move< Hs::Stmts > (that.value);
        break;

      case symbol_kind::S_strict_mark: // strict_mark
      case symbol_kind::S_strictness: // strictness
        value.move< Hs::StrictLazy > (that.value);
        break;

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
      case symbol_kind::S_constr_stuff: // constr_stuff
        value.move< Hs::Type > (that.value);
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
        value.move< Hs::TypeVar > (that.value);
        break;

      case symbol_kind::S_alt: // alt
        value.move< Located<Hs::Alt> > (that.value);
        break;

      case symbol_kind::S_binds: // binds
        value.move< Located<Hs::Binds> > (that.value);
        break;

      case symbol_kind::S_qcname: // qcname
        value.move< Located<std::string> > (that.value);
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

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
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

      case symbol_kind::S_export_subspec: // export_subspec
        value.move< std::optional<Hs::ExportSubSpec> > (that.value);
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.move< std::optional<Hs::ImpSpec> > (that.value);
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.move< std::optional<Located<Hs::Binds>> > (that.value);
        break;

      case symbol_kind::S_prec: // prec
        value.move< std::optional<int> > (that.value);
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.move< std::optional<std::string> > (that.value);
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.move< std::optional<std::vector<Hs::Export>> > (that.value);
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.move< std::pair<Hs::Context,Hs::Type> > (that.value);
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.move< std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > (that.value);
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
        value.move< std::vector<Hs::Constructor> > (that.value);
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        value.move< std::vector<Hs::Export> > (that.value);
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.move< std::vector<Hs::FieldDecl> > (that.value);
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.move< std::vector<Hs::GuardedRHS> > (that.value);
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.move< std::vector<Hs::ImpDecl> > (that.value);
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
        value.move< std::vector<Hs::Type> > (that.value);
        break;

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_forall: // forall
        value.move< std::vector<Hs::TypeVar> > (that.value);
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.move< std::vector<Hs::Var> > (that.value);
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Located<Hs::Alt>> > (that.value);
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
        value.move< std::vector<Located<std::string>> > (that.value);
        break;

      case symbol_kind::S_decls: // decls
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
  parser::yypop_ (int n) YY_NOEXCEPT
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
  parser::yy_pact_value_is_default_ (int yyvalue) YY_NOEXCEPT
  {
    return yyvalue == yypact_ninf_;
  }

  bool
  parser::yy_table_value_is_error_ (int yyvalue) YY_NOEXCEPT
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
        yylhs.value.emplace< Hs::Alts > ();
        break;

      case symbol_kind::S_constr: // constr
        yylhs.value.emplace< Hs::Constructor > ();
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        yylhs.value.emplace< Hs::Context > ();
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        yylhs.value.emplace< Hs::DataOrNewtype > ();
        break;

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
      case symbol_kind::S_decllist: // decllist
        yylhs.value.emplace< Hs::Decls > ();
        break;

      case symbol_kind::S_export: // export
        yylhs.value.emplace< Hs::Export > ();
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        yylhs.value.emplace< Hs::FieldDecl > ();
        break;

      case symbol_kind::S_infix: // infix
        yylhs.value.emplace< Hs::Fixity > ();
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        yylhs.value.emplace< Hs::GuardedRHS > ();
        break;

      case symbol_kind::S_importdecl: // importdecl
        yylhs.value.emplace< Hs::ImpDecl > ();
        break;

      case symbol_kind::S_impspec: // impspec
        yylhs.value.emplace< Hs::ImpSpec > ();
        break;

      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
        yylhs.value.emplace< Hs::InfixExp > ();
        break;

      case symbol_kind::S_module: // module
        yylhs.value.emplace< Hs::Module > ();
        break;

      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_alt_rhs: // alt_rhs
        yylhs.value.emplace< Hs::MultiGuardedRHS > ();
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        yylhs.value.emplace< Hs::Stmts > ();
        break;

      case symbol_kind::S_strict_mark: // strict_mark
      case symbol_kind::S_strictness: // strictness
        yylhs.value.emplace< Hs::StrictLazy > ();
        break;

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
      case symbol_kind::S_constr_stuff: // constr_stuff
        yylhs.value.emplace< Hs::Type > ();
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
        yylhs.value.emplace< Hs::TypeVar > ();
        break;

      case symbol_kind::S_alt: // alt
        yylhs.value.emplace< Located<Hs::Alt> > ();
        break;

      case symbol_kind::S_binds: // binds
        yylhs.value.emplace< Located<Hs::Binds> > ();
        break;

      case symbol_kind::S_qcname: // qcname
        yylhs.value.emplace< Located<std::string> > ();
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

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
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

      case symbol_kind::S_export_subspec: // export_subspec
        yylhs.value.emplace< std::optional<Hs::ExportSubSpec> > ();
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        yylhs.value.emplace< std::optional<Hs::ImpSpec> > ();
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        yylhs.value.emplace< std::optional<Located<Hs::Binds>> > ();
        break;

      case symbol_kind::S_prec: // prec
        yylhs.value.emplace< std::optional<int> > ();
        break;

      case symbol_kind::S_maybeas: // maybeas
        yylhs.value.emplace< std::optional<std::string> > ();
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        yylhs.value.emplace< std::optional<std::vector<Hs::Export>> > ();
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        yylhs.value.emplace< std::pair<Hs::Context,Hs::Type> > ();
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        yylhs.value.emplace< std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();
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
        yylhs.value.emplace< std::vector<Hs::Constructor> > ();
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        yylhs.value.emplace< std::vector<Hs::Export> > ();
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        yylhs.value.emplace< std::vector<Hs::FieldDecl> > ();
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        yylhs.value.emplace< std::vector<Hs::GuardedRHS> > ();
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        yylhs.value.emplace< std::vector<Hs::ImpDecl> > ();
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
        yylhs.value.emplace< std::vector<Hs::Type> > ();
        break;

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_forall: // forall
        yylhs.value.emplace< std::vector<Hs::TypeVar> > ();
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        yylhs.value.emplace< std::vector<Hs::Var> > ();
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        yylhs.value.emplace< std::vector<Located<Hs::Alt>> > ();
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
        yylhs.value.emplace< std::vector<Located<std::string>> > ();
        break;

      case symbol_kind::S_decls: // decls
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
#line 474 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2101 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 491 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < std::string > (),yystack_[2].value.as < std::optional<std::vector<Hs::Export>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2107 "parser.cc"
    break;

  case 4: // module: body2
#line 492 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{"Main",{},yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2113 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 494 "parser.y"
                                                                 {drv.push_module_context();}
#line 2119 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 502 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2125 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 503 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2131 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 505 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2137 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 506 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2143 "parser.cc"
    break;

  case 13: // top: semis top1
#line 509 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2149 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 511 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2155 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 512 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2161 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 513 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::ImpDecl> > (),{});}
#line 2167 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 521 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::Export>> > () = yystack_[1].value.as < std::vector<Hs::Export> > ();}
#line 2173 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 522 "parser.y"
                                      {}
#line 2179 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 524 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > () = yystack_[0].value.as < std::vector<Hs::Export> > ();}
#line 2185 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 526 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > () = yystack_[2].value.as < std::vector<Hs::Export> > (); yylhs.value.as < std::vector<Hs::Export> > ().push_back(yystack_[0].value.as < Hs::Export > ());}
#line 2191 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 527 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > ().push_back(yystack_[0].value.as < Hs::Export > ());}
#line 2197 "parser.cc"
    break;

  case 22: // export: qcname export_subspec
#line 529 "parser.y"
                                      {yylhs.value.as < Hs::Export > () = Hs::ExportSymbol{yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}; }
#line 2203 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 530 "parser.y"
                                      {yylhs.value.as < Hs::Export > () = Hs::ExportModule{{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2209 "parser.cc"
    break;

  case 24: // export_subspec: %empty
#line 532 "parser.y"
                                      {}
#line 2215 "parser.cc"
    break;

  case 25: // export_subspec: "(" qcnames ")"
#line 533 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpecSome{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2221 "parser.cc"
    break;

  case 26: // export_subspec: "(" ".." ")"
#line 534 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpecAll(); }
#line 2227 "parser.cc"
    break;

  case 27: // qcnames: %empty
#line 536 "parser.y"
                   {}
#line 2233 "parser.cc"
    break;

  case 28: // qcnames: qcnames1
#line 537 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2239 "parser.cc"
    break;

  case 29: // qcnames1: qcnames1 "," qcname
#line 539 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2245 "parser.cc"
    break;

  case 30: // qcnames1: qcname
#line 540 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2251 "parser.cc"
    break;

  case 31: // qcname: qvar
#line 542 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2257 "parser.cc"
    break;

  case 32: // qcname: oqtycon_no_varcon
#line 543 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2263 "parser.cc"
    break;

  case 37: // importdecls: importdecls_semi importdecl
#line 553 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[1].value.as < std::vector<Hs::ImpDecl> > (), yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[0].value.as < Hs::ImpDecl > ()); }
#line 2269 "parser.cc"
    break;

  case 38: // importdecls_semi: importdecls_semi importdecl semis1
#line 555 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[2].value.as < std::vector<Hs::ImpDecl> > (); yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[1].value.as < Hs::ImpDecl > ()); }
#line 2275 "parser.cc"
    break;

  case 39: // importdecls_semi: %empty
#line 556 "parser.y"
                         { }
#line 2281 "parser.cc"
    break;

  case 40: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 558 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::ImpDecl > () = Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < std::string > (),yystack_[1].value.as < std::optional<std::string> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ());
}
#line 2289 "parser.cc"
    break;

  case 41: // optqualified: "qualified"
#line 571 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2295 "parser.cc"
    break;

  case 42: // optqualified: %empty
#line 572 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2301 "parser.cc"
    break;

  case 43: // maybeas: "as" modid
#line 574 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 2307 "parser.cc"
    break;

  case 44: // maybeas: %empty
#line 575 "parser.y"
                               { }
#line 2313 "parser.cc"
    break;

  case 45: // maybeimpspec: impspec
#line 577 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2319 "parser.cc"
    break;

  case 46: // maybeimpspec: %empty
#line 578 "parser.y"
                               { }
#line 2325 "parser.cc"
    break;

  case 47: // impspec: "(" exportlist ")"
#line 582 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::Export> > ()}; }
#line 2331 "parser.cc"
    break;

  case 48: // impspec: "hiding" "(" exportlist ")"
#line 583 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::Export> > ()}; }
#line 2337 "parser.cc"
    break;

  case 49: // prec: %empty
#line 588 "parser.y"
                   { }
#line 2343 "parser.cc"
    break;

  case 50: // prec: "INTEGER"
#line 589 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < int > (); }
#line 2349 "parser.cc"
    break;

  case 51: // infix: "infix"
#line 591 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2355 "parser.cc"
    break;

  case 52: // infix: "infixl"
#line 592 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2361 "parser.cc"
    break;

  case 53: // infix: "infixr"
#line 593 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2367 "parser.cc"
    break;

  case 54: // ops: ops "," op
#line 595 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 2373 "parser.cc"
    break;

  case 55: // ops: op
#line 596 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 2379 "parser.cc"
    break;

  case 56: // topdecls: topdecls_semi topdecl
#line 600 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 2385 "parser.cc"
    break;

  case 57: // topdecls_semi: topdecls_semi topdecl semis1
#line 602 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2391 "parser.cc"
    break;

  case 58: // topdecls_semi: %empty
#line 603 "parser.y"
                                            { }
#line 2397 "parser.cc"
    break;

  case 59: // topdecl: cl_decl
#line 605 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2403 "parser.cc"
    break;

  case 60: // topdecl: ty_decl
#line 606 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2409 "parser.cc"
    break;

  case 61: // topdecl: inst_decl
#line 607 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2415 "parser.cc"
    break;

  case 62: // topdecl: "default" "(" comma_types0 ")"
#line 610 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Hs::DefaultDecl(yystack_[1].value.as < std::vector<Hs::Type> > ()); }
#line 2421 "parser.cc"
    break;

  case 63: // topdecl: "foreign" "import" "bpcall" "STRING" var "::" sigtypedoc
#line 611 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = Hs::ForeignDecl(yystack_[3].value.as < std::string > (), yystack_[2].value.as < std::string > (), yystack_[0].value.as < Hs::Type > ());}
#line 2427 "parser.cc"
    break;

  case 64: // topdecl: decl_no_th
#line 618 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2433 "parser.cc"
    break;

  case 65: // topdecl: infixexp_top
#line 620 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < Hs::InfixExp > ();}
#line 2439 "parser.cc"
    break;

  case 66: // cl_decl: "class" tycl_hdr wherebinds
#line 622 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_class_decl(yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().first,yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().second,yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 2445 "parser.cc"
    break;

  case 67: // ty_decl: "type" type "=" ctypedoc
#line 624 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_type_synonym({yystack_[2].location,yystack_[2].value.as < Hs::Type > ()},{yystack_[0].location,yystack_[0].value.as < Hs::Type > ()});}
#line 2451 "parser.cc"
    break;

  case 68: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 625 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (),yystack_[2].value.as < std::pair<Hs::Context,Hs::Type> > ().first,yystack_[2].value.as < std::pair<Hs::Context,Hs::Type> > ().second,yystack_[1].value.as < std::vector<Hs::Constructor> > ());}
#line 2457 "parser.cc"
    break;

  case 69: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig
#line 626 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[3].value.as < Hs::DataOrNewtype > (),yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().first,yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().second,{});}
#line 2463 "parser.cc"
    break;

  case 70: // inst_decl: "instance" overlap_pragma inst_type wherebinds
#line 631 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_instance_decl({yystack_[1].location,yystack_[1].value.as < Hs::Type > ()},yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 2469 "parser.cc"
    break;

  case 80: // data_or_newtype: "data"
#line 686 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 2475 "parser.cc"
    break;

  case 81: // data_or_newtype: "newtype"
#line 687 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 2481 "parser.cc"
    break;

  case 84: // tycl_hdr: context "=>" type
#line 699 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::Type> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::Type > ()};}
#line 2487 "parser.cc"
    break;

  case 85: // tycl_hdr: type
#line 700 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::Type> > () = {{},yystack_[0].value.as < Hs::Type > ()};}
#line 2493 "parser.cc"
    break;

  case 89: // decls: decls ";" decl
#line 748 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2499 "parser.cc"
    break;

  case 90: // decls: decls ";"
#line 749 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2505 "parser.cc"
    break;

  case 91: // decls: decl
#line 750 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2511 "parser.cc"
    break;

  case 92: // decls: %empty
#line 751 "parser.y"
                        {}
#line 2517 "parser.cc"
    break;

  case 93: // decllist: "{" decls "}"
#line 753 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2523 "parser.cc"
    break;

  case 94: // decllist: "vocurly" decls close
#line 754 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2529 "parser.cc"
    break;

  case 95: // binds: decllist
#line 756 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 2535 "parser.cc"
    break;

  case 96: // wherebinds: "where" binds
#line 758 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 2541 "parser.cc"
    break;

  case 97: // wherebinds: %empty
#line 759 "parser.y"
                                 {}
#line 2547 "parser.cc"
    break;

  case 103: // opt_tyconsig: %empty
#line 785 "parser.y"
                     {}
#line 2553 "parser.cc"
    break;

  case 104: // opt_tyconsig: "::" gtycon
#line 786 "parser.y"
                     {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2559 "parser.cc"
    break;

  case 105: // sigtype: ctype
#line 788 "parser.y"
                 {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 2565 "parser.cc"
    break;

  case 106: // sigtypedoc: ctypedoc
#line 790 "parser.y"
                     {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 2571 "parser.cc"
    break;

  case 107: // sig_vars: sig_vars "," var
#line 792 "parser.y"
                           {yylhs.value.as < std::vector<Hs::Var> > () = yystack_[2].value.as < std::vector<Hs::Var> > (); yylhs.value.as < std::vector<Hs::Var> > ().push_back(Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}));}
#line 2577 "parser.cc"
    break;

  case 108: // sig_vars: var
#line 793 "parser.y"
                           {yylhs.value.as < std::vector<Hs::Var> > ().push_back(Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}));}
#line 2583 "parser.cc"
    break;

  case 109: // sigtypes1: sigtype
#line 795 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2589 "parser.cc"
    break;

  case 110: // sigtypes1: sigtypes1 "," sigtype
#line 796 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[2].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2595 "parser.cc"
    break;

  case 111: // strict_mark: strictness
#line 800 "parser.y"
                                            {yylhs.value.as < Hs::StrictLazy > () = yystack_[0].value.as < Hs::StrictLazy > ();}
#line 2601 "parser.cc"
    break;

  case 112: // strictness: "!"
#line 806 "parser.y"
                {yylhs.value.as < Hs::StrictLazy > () = Hs::StrictLazy::strict;}
#line 2607 "parser.cc"
    break;

  case 113: // strictness: "~"
#line 807 "parser.y"
                {yylhs.value.as < Hs::StrictLazy > () = Hs::StrictLazy::lazy;}
#line 2613 "parser.cc"
    break;

  case 114: // ctype: "forall" tv_bndrs "." ctype
#line 814 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::ForallType(yystack_[2].value.as < std::vector<Hs::TypeVar> > (), yystack_[0].value.as < Hs::Type > ());}
#line 2619 "parser.cc"
    break;

  case 115: // ctype: context "=>" ctype
#line 815 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::Type > ());}
#line 2625 "parser.cc"
    break;

  case 116: // ctype: type
#line 817 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 2631 "parser.cc"
    break;

  case 117: // ctypedoc: ctype
#line 819 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 2637 "parser.cc"
    break;

  case 118: // context: btype
#line 828 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::Type > ());}
#line 2643 "parser.cc"
    break;

  case 119: // context_no_ops: btype_no_ops
#line 830 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ()));}
#line 2649 "parser.cc"
    break;

  case 120: // type: btype
#line 832 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 2655 "parser.cc"
    break;

  case 121: // type: btype "->" ctype
#line 833 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,"->"}),yystack_[2].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()});}
#line 2661 "parser.cc"
    break;

  case 122: // typedoc: type
#line 835 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 2667 "parser.cc"
    break;

  case 123: // btype: tyapps
#line 838 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ());}
#line 2673 "parser.cc"
    break;

  case 124: // btype_no_ops: atype_docs
#line 840 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2679 "parser.cc"
    break;

  case 125: // btype_no_ops: btype_no_ops atype_docs
#line 841 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[1].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2685 "parser.cc"
    break;

  case 126: // tyapps: tyapp
#line 843 "parser.y"
                                   {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2691 "parser.cc"
    break;

  case 127: // tyapps: tyapps tyapp
#line 844 "parser.y"
                                   {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[1].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2697 "parser.cc"
    break;

  case 128: // tyapp: atype
#line 846 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 2703 "parser.cc"
    break;

  case 129: // tyapp: qtyconop
#line 847 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2709 "parser.cc"
    break;

  case 130: // tyapp: tyvarop
#line 848 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2715 "parser.cc"
    break;

  case 131: // atype_docs: atype
#line 854 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 2721 "parser.cc"
    break;

  case 132: // atype: ntgtycon
#line 861 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2727 "parser.cc"
    break;

  case 133: // atype: tyvar
#line 862 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2733 "parser.cc"
    break;

  case 134: // atype: "*"
#line 863 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,"*"});}
#line 2739 "parser.cc"
    break;

  case 135: // atype: strict_mark atype
#line 864 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::StrictLazyType(yystack_[1].value.as < Hs::StrictLazy > (),yystack_[0].value.as < Hs::Type > ());}
#line 2745 "parser.cc"
    break;

  case 136: // atype: "{" fielddecls "}"
#line 865 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ());}
#line 2751 "parser.cc"
    break;

  case 137: // atype: "(" ")"
#line 866 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[1].location,"()"});}
#line 2757 "parser.cc"
    break;

  case 138: // atype: "(" comma_types1 "," ctype ")"
#line 867 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::Type> > ();ts.push_back(yystack_[1].value.as < Hs::Type > ());yylhs.value.as < Hs::Type > () = Hs::TupleType(ts);}
#line 2763 "parser.cc"
    break;

  case 139: // atype: "[" ctype "]"
#line 873 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::ListType{yystack_[1].value.as < Hs::Type > ()}; }
#line 2769 "parser.cc"
    break;

  case 140: // atype: "(" ctype ")"
#line 874 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = yystack_[1].value.as < Hs::Type > ();}
#line 2775 "parser.cc"
    break;

  case 141: // inst_type: sigtype
#line 878 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 2781 "parser.cc"
    break;

  case 144: // comma_types0: comma_types1
#line 883 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[0].value.as < std::vector<Hs::Type> > ();}
#line 2787 "parser.cc"
    break;

  case 145: // comma_types0: %empty
#line 884 "parser.y"
                                       { /* default construction OK */ }
#line 2793 "parser.cc"
    break;

  case 146: // comma_types1: ctype
#line 886 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2799 "parser.cc"
    break;

  case 147: // comma_types1: comma_types1 "," ctype
#line 887 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[2].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2805 "parser.cc"
    break;

  case 148: // tv_bndrs: tv_bndrs tv_bndr
#line 894 "parser.y"
                               {yylhs.value.as < std::vector<Hs::TypeVar> > () = yystack_[1].value.as < std::vector<Hs::TypeVar> > (); yylhs.value.as < std::vector<Hs::TypeVar> > ().push_back(yystack_[0].value.as < Hs::TypeVar > ());}
#line 2811 "parser.cc"
    break;

  case 149: // tv_bndrs: %empty
#line 895 "parser.y"
                               { /* default construction OK */}
#line 2817 "parser.cc"
    break;

  case 150: // tv_bndr: tyvar
#line 898 "parser.y"
                                    {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2823 "parser.cc"
    break;

  case 151: // tv_bndr: "(" tyvar "::" kind ")"
#line 899 "parser.y"
                                    {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[3].location,yystack_[3].value.as < std::string > ()},yystack_[1].value.as < expression_ref > ());}
#line 2829 "parser.cc"
    break;

  case 152: // kind: ctype
#line 917 "parser.y"
             {yylhs.value.as < expression_ref > () = type_to_kind(yystack_[0].value.as < Hs::Type > ());}
#line 2835 "parser.cc"
    break;

  case 153: // constrs: "=" constrs1
#line 923 "parser.y"
                                {yylhs.value.as < std::vector<Hs::Constructor> > () = yystack_[0].value.as < std::vector<Hs::Constructor> > ();}
#line 2841 "parser.cc"
    break;

  case 154: // constrs1: constrs1 "|" constr
#line 925 "parser.y"
                                {yylhs.value.as < std::vector<Hs::Constructor> > () = yystack_[2].value.as < std::vector<Hs::Constructor> > (); yylhs.value.as < std::vector<Hs::Constructor> > ().push_back(yystack_[0].value.as < Hs::Constructor > ());}
#line 2847 "parser.cc"
    break;

  case 155: // constrs1: constr
#line 926 "parser.y"
                                {yylhs.value.as < std::vector<Hs::Constructor> > ().push_back(yystack_[0].value.as < Hs::Constructor > ());}
#line 2853 "parser.cc"
    break;

  case 156: // constr: forall context_no_ops "=>" constr_stuff
#line 928 "parser.y"
                                                {yylhs.value.as < Hs::Constructor > () = make_constructor(yystack_[3].value.as < std::vector<Hs::TypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::Type > ());}
#line 2859 "parser.cc"
    break;

  case 157: // constr: forall constr_stuff
#line 929 "parser.y"
                                                {yylhs.value.as < Hs::Constructor > () = make_constructor(yystack_[1].value.as < std::vector<Hs::TypeVar> > (),{}, yystack_[0].value.as < Hs::Type > ());}
#line 2865 "parser.cc"
    break;

  case 158: // forall: "forall" tv_bndrs "."
#line 931 "parser.y"
                                {yylhs.value.as < std::vector<Hs::TypeVar> > () = yystack_[1].value.as < std::vector<Hs::TypeVar> > ();}
#line 2871 "parser.cc"
    break;

  case 159: // forall: %empty
#line 932 "parser.y"
                                {}
#line 2877 "parser.cc"
    break;

  case 160: // constr_stuff: btype_no_ops
#line 934 "parser.y"
                                                {yylhs.value.as < Hs::Type > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ());}
#line 2883 "parser.cc"
    break;

  case 161: // constr_stuff: btype_no_ops conop btype_no_ops
#line 935 "parser.y"
                                                {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,yystack_[1].value.as < std::string > ()}),Hs::make_tyapps(yystack_[2].value.as < std::vector<Hs::Type> > ()),Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ())});}
#line 2889 "parser.cc"
    break;

  case 162: // fielddecls: %empty
#line 937 "parser.y"
                                {}
#line 2895 "parser.cc"
    break;

  case 163: // fielddecls: fielddecls1
#line 938 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 2901 "parser.cc"
    break;

  case 164: // fielddecls1: fielddecls1 "," fielddecl
#line 940 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 2907 "parser.cc"
    break;

  case 165: // fielddecls1: fielddecl
#line 941 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 2913 "parser.cc"
    break;

  case 166: // fielddecl: sig_vars "::" ctype
#line 943 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::Var> > (),yystack_[0].value.as < Hs::Type > ());}
#line 2919 "parser.cc"
    break;

  case 177: // decl_no_th: sigdecl
#line 962 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2925 "parser.cc"
    break;

  case 178: // decl_no_th: "!" aexp rhs
#line 964 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::StrictValueDecl{{yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < Hs::MultiGuardedRHS > ()}; }
#line 2931 "parser.cc"
    break;

  case 179: // decl_no_th: infixexp_top rhs
#line 965 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::ValueDecl({yystack_[1].location,yystack_[1].value.as < Hs::InfixExp > ()},yystack_[0].value.as < Hs::MultiGuardedRHS > ());}
#line 2937 "parser.cc"
    break;

  case 180: // decl: decl_no_th
#line 967 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2943 "parser.cc"
    break;

  case 181: // rhs: "=" exp wherebinds
#line 971 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 2949 "parser.cc"
    break;

  case 182: // rhs: gdrhs wherebinds
#line 972 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 2955 "parser.cc"
    break;

  case 183: // gdrhs: gdrhs gdrh
#line 974 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 2961 "parser.cc"
    break;

  case 184: // gdrhs: gdrh
#line 975 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 2967 "parser.cc"
    break;

  case 185: // gdrh: "|" guardquals "=" exp
#line 979 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ()};}
#line 2973 "parser.cc"
    break;

  case 186: // sigdecl: sig_vars "::" sigtypedoc
#line 989 "parser.y"
                                  { yylhs.value.as < expression_ref > () = Hs::SignatureDecl{yystack_[2].value.as < std::vector<Hs::Var> > (),yystack_[0].value.as < Hs::Type > ()}; }
#line 2979 "parser.cc"
    break;

  case 187: // sigdecl: infix prec ops
#line 990 "parser.y"
                         { yylhs.value.as < expression_ref > () = Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()}; }
#line 2985 "parser.cc"
    break;

  case 188: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 992 "parser.y"
                                                    {}
#line 2991 "parser.cc"
    break;

  case 189: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 993 "parser.y"
                                            {}
#line 2997 "parser.cc"
    break;

  case 190: // sigdecl: "{-# SCC" qvar "#-}"
#line 994 "parser.y"
                              {}
#line 3003 "parser.cc"
    break;

  case 191: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 995 "parser.y"
                                     {}
#line 3009 "parser.cc"
    break;

  case 192: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 996 "parser.y"
                                                               {}
#line 3015 "parser.cc"
    break;

  case 193: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 997 "parser.y"
                                                                      {}
#line 3021 "parser.cc"
    break;

  case 194: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 998 "parser.y"
                                                     {}
#line 3027 "parser.cc"
    break;

  case 199: // exp: infixexp "::" sigtype
#line 1009 "parser.y"
                           { yylhs.value.as < expression_ref > () = Hs::TypedExp(yystack_[2].value.as < Hs::InfixExp > (),yystack_[0].value.as < Hs::Type > ()); }
#line 3033 "parser.cc"
    break;

  case 200: // exp: infixexp
#line 1010 "parser.y"
                           { yylhs.value.as < expression_ref > () = yystack_[0].value.as < Hs::InfixExp > (); }
#line 3039 "parser.cc"
    break;

  case 201: // infixexp: exp10
#line 1012 "parser.y"
                                {yylhs.value.as < Hs::InfixExp > () = Hs::InfixExp({yystack_[0].value.as < expression_ref > ()});}
#line 3045 "parser.cc"
    break;

  case 202: // infixexp: infixexp qop exp10
#line 1013 "parser.y"
                                {yylhs.value.as < Hs::InfixExp > () = yystack_[2].value.as < Hs::InfixExp > (); yylhs.value.as < Hs::InfixExp > ().terms.push_back(yystack_[1].value.as < expression_ref > ()); yylhs.value.as < Hs::InfixExp > ().terms.push_back(yystack_[0].value.as < expression_ref > ());}
#line 3051 "parser.cc"
    break;

  case 203: // infixexp_top: exp10_top
#line 1015 "parser.y"
                                {yylhs.value.as < Hs::InfixExp > () = Hs::InfixExp({yystack_[0].value.as < expression_ref > ()});}
#line 3057 "parser.cc"
    break;

  case 204: // infixexp_top: infixexp_top qop exp10_top
#line 1016 "parser.y"
                                          {yylhs.value.as < Hs::InfixExp > () = yystack_[2].value.as < Hs::InfixExp > (); yylhs.value.as < Hs::InfixExp > ().terms.push_back(yystack_[1].value.as < expression_ref > ()); yylhs.value.as < Hs::InfixExp > ().terms.push_back(yystack_[0].value.as < expression_ref > ());}
#line 3063 "parser.cc"
    break;

  case 205: // exp10_top: "-" fexp
#line 1018 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_minus(yystack_[0].value.as < expression_ref > ());}
#line 3069 "parser.cc"
    break;

  case 206: // exp10_top: "{-# CORE" "STRING" "#-}"
#line 1019 "parser.y"
                                   {}
#line 3075 "parser.cc"
    break;

  case 207: // exp10_top: fexp
#line 1020 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3081 "parser.cc"
    break;

  case 208: // exp10: exp10_top
#line 1022 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3087 "parser.cc"
    break;

  case 209: // exp10: scc_annot exp
#line 1023 "parser.y"
                                 {}
#line 3093 "parser.cc"
    break;

  case 214: // fexp: fexp aexp
#line 1034 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_apply(yystack_[1].value.as < expression_ref > (), yystack_[0].value.as < expression_ref > ());}
#line 3099 "parser.cc"
    break;

  case 215: // fexp: fexp "TYPEAPP" atype
#line 1035 "parser.y"
                                 {}
#line 3105 "parser.cc"
    break;

  case 216: // fexp: "static" aexp
#line 1036 "parser.y"
                                 {}
#line 3111 "parser.cc"
    break;

  case 217: // fexp: aexp
#line 1037 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3117 "parser.cc"
    break;

  case 218: // aexp: qvar "@" aexp
#line 1039 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::AsPattern(Hs::Var({yystack_[2].location,yystack_[2].value.as < std::string > ()}),yystack_[0].value.as < expression_ref > ());}
#line 3123 "parser.cc"
    break;

  case 219: // aexp: "~" aexp
#line 1040 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::LazyPattern(yystack_[0].value.as < expression_ref > ());}
#line 3129 "parser.cc"
    break;

  case 220: // aexp: "\\" apats1 "->" exp
#line 1041 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::LambdaExp(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3135 "parser.cc"
    break;

  case 221: // aexp: "let" binds "in" exp
#line 1042 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 3141 "parser.cc"
    break;

  case 222: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1044 "parser.y"
                                                       {yylhs.value.as < expression_ref > () = Hs::IfExp({yystack_[6].location,yystack_[6].value.as < expression_ref > ()},{yystack_[3].location,yystack_[3].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 3147 "parser.cc"
    break;

  case 223: // aexp: "case" exp "of" altslist
#line 1046 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::CaseExp(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < Hs::Alts > ());}
#line 3153 "parser.cc"
    break;

  case 224: // aexp: "do" stmtlist
#line 1047 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::Do(yystack_[0].value.as < Hs::Stmts > ());}
#line 3159 "parser.cc"
    break;

  case 225: // aexp: "mdo" stmtlist
#line 1048 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::MDo(yystack_[0].value.as < Hs::Stmts > ());}
#line 3165 "parser.cc"
    break;

  case 226: // aexp: aexp1
#line 1050 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3171 "parser.cc"
    break;

  case 227: // aexp1: aexp1 "{" fbinds "}"
#line 1052 "parser.y"
                              {}
#line 3177 "parser.cc"
    break;

  case 228: // aexp1: aexp2
#line 1053 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3183 "parser.cc"
    break;

  case 229: // aexp2: qvar
#line 1055 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3189 "parser.cc"
    break;

  case 230: // aexp2: qcon
#line 1056 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3195 "parser.cc"
    break;

  case 231: // aexp2: literal
#line 1057 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3201 "parser.cc"
    break;

  case 232: // aexp2: "(" texp ")"
#line 1058 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3207 "parser.cc"
    break;

  case 233: // aexp2: "(" tup_exprs ")"
#line 1059 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::Tuple(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3213 "parser.cc"
    break;

  case 234: // aexp2: "[" list "]"
#line 1064 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3219 "parser.cc"
    break;

  case 235: // aexp2: "_"
#line 1065 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::WildcardPattern();}
#line 3225 "parser.cc"
    break;

  case 236: // texp: exp
#line 1070 "parser.y"
                      {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3231 "parser.cc"
    break;

  case 237: // texp: infixexp qop
#line 1071 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::LeftSection ( yystack_[1].value.as < Hs::InfixExp > (), yystack_[0].value.as < expression_ref > () ); }
#line 3237 "parser.cc"
    break;

  case 238: // texp: qopm infixexp
#line 1072 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::RightSection( yystack_[1].value.as < expression_ref > (), yystack_[0].value.as < Hs::InfixExp > () ); }
#line 3243 "parser.cc"
    break;

  case 239: // tup_exprs: tup_exprs "," texp
#line 1077 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3249 "parser.cc"
    break;

  case 240: // tup_exprs: texp "," texp
#line 1078 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3255 "parser.cc"
    break;

  case 241: // list: texp
#line 1096 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List{{yystack_[0].value.as < expression_ref > ()}}; }
#line 3261 "parser.cc"
    break;

  case 242: // list: lexps
#line 1097 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List{yystack_[0].value.as < std::vector<expression_ref> > ()}; }
#line 3267 "parser.cc"
    break;

  case 243: // list: texp ".."
#line 1098 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < expression_ref > ()); }
#line 3273 "parser.cc"
    break;

  case 244: // list: texp "," exp ".."
#line 1099 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()); }
#line 3279 "parser.cc"
    break;

  case 245: // list: texp ".." exp
#line 1100 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()); }
#line 3285 "parser.cc"
    break;

  case 246: // list: texp "," exp ".." exp
#line 1101 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < expression_ref > (), yystack_[2].value.as < expression_ref > (), yystack_[0].value.as < expression_ref > ()); }
#line 3291 "parser.cc"
    break;

  case 247: // list: texp "|" squals
#line 1102 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < expression_ref > (), yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 3297 "parser.cc"
    break;

  case 248: // lexps: lexps "," texp
#line 1104 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3303 "parser.cc"
    break;

  case 249: // lexps: texp "," texp
#line 1105 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3309 "parser.cc"
    break;

  case 250: // squals: squals "," qual
#line 1118 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3315 "parser.cc"
    break;

  case 251: // squals: qual
#line 1120 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3321 "parser.cc"
    break;

  case 252: // guardquals: guardquals1
#line 1130 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3327 "parser.cc"
    break;

  case 253: // guardquals1: guardquals1 "," qual
#line 1132 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > ();yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3333 "parser.cc"
    break;

  case 254: // guardquals1: qual
#line 1133 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3339 "parser.cc"
    break;

  case 255: // altslist: "{" alts "}"
#line 1136 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 3345 "parser.cc"
    break;

  case 256: // altslist: "vocurly" alts close
#line 1137 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 3351 "parser.cc"
    break;

  case 257: // altslist: "{" "}"
#line 1138 "parser.y"
                                 {}
#line 3357 "parser.cc"
    break;

  case 258: // altslist: "vocurly" close
#line 1139 "parser.y"
                                 {}
#line 3363 "parser.cc"
    break;

  case 259: // alts: alts1
#line 1141 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 3369 "parser.cc"
    break;

  case 260: // alts: ";" alts
#line 1142 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 3375 "parser.cc"
    break;

  case 261: // alts1: alts1 ";" alt
#line 1144 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 3381 "parser.cc"
    break;

  case 262: // alts1: alts1 ";"
#line 1145 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 3387 "parser.cc"
    break;

  case 263: // alts1: alt
#line 1146 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 3393 "parser.cc"
    break;

  case 264: // alt: pat alt_rhs
#line 1148 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 3399 "parser.cc"
    break;

  case 265: // alt_rhs: "->" exp wherebinds
#line 1150 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3405 "parser.cc"
    break;

  case 266: // alt_rhs: gdpats wherebinds
#line 1151 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3411 "parser.cc"
    break;

  case 267: // gdpats: gdpats gdpat
#line 1153 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3417 "parser.cc"
    break;

  case 268: // gdpats: gdpat
#line 1154 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3423 "parser.cc"
    break;

  case 269: // gdpat: "|" guardquals "->" exp
#line 1163 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ()};}
#line 3429 "parser.cc"
    break;

  case 270: // pat: exp
#line 1165 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3435 "parser.cc"
    break;

  case 271: // pat: "!" aexp
#line 1166 "parser.y"
              {yylhs.value.as < expression_ref > () = Hs::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3441 "parser.cc"
    break;

  case 272: // bindpat: exp
#line 1168 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3447 "parser.cc"
    break;

  case 273: // bindpat: "!" aexp
#line 1169 "parser.y"
              {yylhs.value.as < expression_ref > () = Hs::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3453 "parser.cc"
    break;

  case 274: // apat: aexp
#line 1171 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3459 "parser.cc"
    break;

  case 275: // apat: "!" aexp
#line 1172 "parser.y"
              {yylhs.value.as < expression_ref > () = Hs::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3465 "parser.cc"
    break;

  case 276: // apats1: apats1 apat
#line 1174 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3471 "parser.cc"
    break;

  case 277: // apats1: apat
#line 1175 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3477 "parser.cc"
    break;

  case 278: // stmtlist: "{" stmts "}"
#line 1178 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 3483 "parser.cc"
    break;

  case 279: // stmtlist: "vocurly" stmts close
#line 1179 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 3489 "parser.cc"
    break;

  case 280: // stmts: stmts ";" stmt
#line 1181 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3495 "parser.cc"
    break;

  case 281: // stmts: stmts ";"
#line 1182 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3501 "parser.cc"
    break;

  case 282: // stmts: stmt
#line 1183 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3507 "parser.cc"
    break;

  case 283: // stmts: %empty
#line 1184 "parser.y"
                       {}
#line 3513 "parser.cc"
    break;

  case 284: // stmt: qual
#line 1189 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3519 "parser.cc"
    break;

  case 285: // stmt: "rec" stmtlist
#line 1190 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ());}
#line 3525 "parser.cc"
    break;

  case 286: // qual: bindpat "<-" exp
#line 1192 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::PatQual(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3531 "parser.cc"
    break;

  case 287: // qual: exp
#line 1193 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::SimpleQual(yystack_[0].value.as < expression_ref > ());}
#line 3537 "parser.cc"
    break;

  case 288: // qual: "let" binds
#line 1194 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ());}
#line 3543 "parser.cc"
    break;

  case 296: // qcon: gen_qcon
#line 1239 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3549 "parser.cc"
    break;

  case 297: // qcon: sysdcon
#line 1240 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3555 "parser.cc"
    break;

  case 298: // gen_qcon: qconid
#line 1242 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3561 "parser.cc"
    break;

  case 299: // gen_qcon: "(" qconsym ")"
#line 1243 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3567 "parser.cc"
    break;

  case 300: // con: conid
#line 1245 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3573 "parser.cc"
    break;

  case 301: // con: "(" consym ")"
#line 1246 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3579 "parser.cc"
    break;

  case 302: // con: sysdcon
#line 1247 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3585 "parser.cc"
    break;

  case 305: // sysdcon_no_list: "(" ")"
#line 1252 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 3591 "parser.cc"
    break;

  case 306: // sysdcon_no_list: "(" commas ")"
#line 1253 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3597 "parser.cc"
    break;

  case 307: // sysdcon_no_list: "(#" "#)"
#line 1254 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 3603 "parser.cc"
    break;

  case 308: // sysdcon_no_list: "(#" commas "#)"
#line 1255 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3609 "parser.cc"
    break;

  case 309: // sysdcon: sysdcon_no_list
#line 1257 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3615 "parser.cc"
    break;

  case 310: // sysdcon: "[" "]"
#line 1258 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 3621 "parser.cc"
    break;

  case 311: // conop: consym
#line 1260 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3627 "parser.cc"
    break;

  case 312: // conop: "`" conid "`"
#line 1261 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3633 "parser.cc"
    break;

  case 313: // qconop: qconsym
#line 1263 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3639 "parser.cc"
    break;

  case 314: // qconop: "`" qconid "`"
#line 1264 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3645 "parser.cc"
    break;

  case 315: // gtycon: ntgtycon
#line 1267 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3651 "parser.cc"
    break;

  case 316: // gtycon: "(" ")"
#line 1268 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 3657 "parser.cc"
    break;

  case 317: // gtycon: "(#" "#)"
#line 1269 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 3663 "parser.cc"
    break;

  case 318: // ntgtycon: oqtycon
#line 1271 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3669 "parser.cc"
    break;

  case 319: // ntgtycon: "(" commas ")"
#line 1272 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3675 "parser.cc"
    break;

  case 320: // ntgtycon: "(#" commas "#)"
#line 1273 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3681 "parser.cc"
    break;

  case 321: // ntgtycon: "(" "->" ")"
#line 1274 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 3687 "parser.cc"
    break;

  case 322: // ntgtycon: "[" "]"
#line 1275 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 3693 "parser.cc"
    break;

  case 323: // oqtycon: qtycon
#line 1277 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3699 "parser.cc"
    break;

  case 324: // oqtycon: "(" qtyconsym ")"
#line 1278 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3705 "parser.cc"
    break;

  case 325: // oqtycon: "(" "~" ")"
#line 1279 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3711 "parser.cc"
    break;

  case 326: // oqtycon_no_varcon: qtycon
#line 1281 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3717 "parser.cc"
    break;

  case 327: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1282 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3723 "parser.cc"
    break;

  case 328: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1283 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3729 "parser.cc"
    break;

  case 329: // oqtycon_no_varcon: "(" ":" ")"
#line 1284 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 3735 "parser.cc"
    break;

  case 330: // oqtycon_no_varcon: "(" "~" ")"
#line 1285 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3741 "parser.cc"
    break;

  case 331: // qtyconop: qtyconsym
#line 1288 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3747 "parser.cc"
    break;

  case 332: // qtyconop: "`" qtycon "`"
#line 1289 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3753 "parser.cc"
    break;

  case 333: // qtycondoc: qtycon
#line 1291 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3759 "parser.cc"
    break;

  case 334: // qtycon: "QCONID"
#line 1293 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3765 "parser.cc"
    break;

  case 335: // qtycon: tycon
#line 1294 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3771 "parser.cc"
    break;

  case 336: // tycon: "CONID"
#line 1298 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3777 "parser.cc"
    break;

  case 337: // qtyconsym: "QCONSYM"
#line 1300 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3783 "parser.cc"
    break;

  case 338: // qtyconsym: "QVARSYM"
#line 1301 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3789 "parser.cc"
    break;

  case 339: // qtyconsym: tyconsym
#line 1302 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3795 "parser.cc"
    break;

  case 340: // tyconsym: "CONSYM"
#line 1304 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3801 "parser.cc"
    break;

  case 341: // tyconsym: "VARSYM"
#line 1305 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3807 "parser.cc"
    break;

  case 342: // tyconsym: ":"
#line 1306 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 3813 "parser.cc"
    break;

  case 343: // tyconsym: "-"
#line 1307 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 3819 "parser.cc"
    break;

  case 344: // op: varop
#line 1312 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3825 "parser.cc"
    break;

  case 345: // op: conop
#line 1313 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3831 "parser.cc"
    break;

  case 346: // varop: varsym
#line 1315 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3837 "parser.cc"
    break;

  case 347: // varop: "`" varid "`"
#line 1316 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3843 "parser.cc"
    break;

  case 348: // qop: qvarop
#line 1318 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3849 "parser.cc"
    break;

  case 349: // qop: qconop
#line 1319 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3855 "parser.cc"
    break;

  case 350: // qopm: qvaropm
#line 1322 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3861 "parser.cc"
    break;

  case 351: // qopm: qconop
#line 1323 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3867 "parser.cc"
    break;

  case 352: // qvarop: qvarsym
#line 1328 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3873 "parser.cc"
    break;

  case 353: // qvarop: "`" qvarid "`"
#line 1329 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3879 "parser.cc"
    break;

  case 354: // qvaropm: qvarsym_no_minus
#line 1331 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 3885 "parser.cc"
    break;

  case 355: // qvaropm: "`" qvarid "`"
#line 1332 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3891 "parser.cc"
    break;

  case 356: // tyvar: tyvarid
#line 1336 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3897 "parser.cc"
    break;

  case 357: // tyvarop: "`" tyvarid "`"
#line 1338 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3903 "parser.cc"
    break;

  case 358: // tyvarid: "VARID"
#line 1340 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3909 "parser.cc"
    break;

  case 359: // tyvarid: special_id
#line 1341 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3915 "parser.cc"
    break;

  case 360: // tyvarid: "unsafe"
#line 1342 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 3921 "parser.cc"
    break;

  case 361: // tyvarid: "safe"
#line 1343 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 3927 "parser.cc"
    break;

  case 362: // tyvarid: "interruptible"
#line 1344 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 3933 "parser.cc"
    break;

  case 363: // var: varid
#line 1347 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3939 "parser.cc"
    break;

  case 364: // var: "(" varsym ")"
#line 1348 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3945 "parser.cc"
    break;

  case 365: // qvar: qvarid
#line 1350 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3951 "parser.cc"
    break;

  case 366: // qvar: "(" varsym ")"
#line 1351 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3957 "parser.cc"
    break;

  case 367: // qvar: "(" qvarsym1 ")"
#line 1352 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3963 "parser.cc"
    break;

  case 368: // qvarid: varid
#line 1354 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3969 "parser.cc"
    break;

  case 369: // qvarid: "QVARID"
#line 1355 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3975 "parser.cc"
    break;

  case 370: // varid: "VARID"
#line 1357 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3981 "parser.cc"
    break;

  case 371: // varid: special_id
#line 1358 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3987 "parser.cc"
    break;

  case 372: // varid: "unsafe"
#line 1359 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 3993 "parser.cc"
    break;

  case 373: // varid: "safe"
#line 1360 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 3999 "parser.cc"
    break;

  case 374: // varid: "interruptible"
#line 1361 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 4005 "parser.cc"
    break;

  case 375: // varid: "forall"
#line 1362 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 4011 "parser.cc"
    break;

  case 376: // varid: "family"
#line 1363 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 4017 "parser.cc"
    break;

  case 377: // varid: "role"
#line 1364 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 4023 "parser.cc"
    break;

  case 378: // qvarsym: varsym
#line 1366 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4029 "parser.cc"
    break;

  case 379: // qvarsym: qvarsym1
#line 1367 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4035 "parser.cc"
    break;

  case 380: // qvarsym_no_minus: varsym_no_minus
#line 1369 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4041 "parser.cc"
    break;

  case 381: // qvarsym_no_minus: qvarsym1
#line 1370 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4047 "parser.cc"
    break;

  case 382: // qvarsym1: "QVARSYM"
#line 1372 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4053 "parser.cc"
    break;

  case 383: // varsym: varsym_no_minus
#line 1374 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4059 "parser.cc"
    break;

  case 384: // varsym: "-"
#line 1375 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 4065 "parser.cc"
    break;

  case 385: // varsym_no_minus: "VARSYM"
#line 1377 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4071 "parser.cc"
    break;

  case 386: // varsym_no_minus: special_sym
#line 1378 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4077 "parser.cc"
    break;

  case 387: // special_id: "as"
#line 1380 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 4083 "parser.cc"
    break;

  case 388: // special_id: "qualified"
#line 1381 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 4089 "parser.cc"
    break;

  case 389: // special_id: "hiding"
#line 1382 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 4095 "parser.cc"
    break;

  case 390: // special_id: "export"
#line 1383 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 4101 "parser.cc"
    break;

  case 391: // special_id: "label"
#line 1384 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 4107 "parser.cc"
    break;

  case 392: // special_id: "dynamic"
#line 1385 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 4113 "parser.cc"
    break;

  case 393: // special_id: "stdcall"
#line 1386 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 4119 "parser.cc"
    break;

  case 394: // special_id: "ccall"
#line 1387 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 4125 "parser.cc"
    break;

  case 395: // special_id: "capi"
#line 1388 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 4131 "parser.cc"
    break;

  case 396: // special_id: "prim"
#line 1389 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 4137 "parser.cc"
    break;

  case 397: // special_id: "javascript"
#line 1390 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 4143 "parser.cc"
    break;

  case 398: // special_id: "group"
#line 1391 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 4149 "parser.cc"
    break;

  case 399: // special_id: "stock"
#line 1392 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 4155 "parser.cc"
    break;

  case 400: // special_id: "anyclass"
#line 1393 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 4161 "parser.cc"
    break;

  case 401: // special_id: "via"
#line 1394 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 4167 "parser.cc"
    break;

  case 402: // special_id: "unit"
#line 1395 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 4173 "parser.cc"
    break;

  case 403: // special_id: "dependency"
#line 1396 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 4179 "parser.cc"
    break;

  case 404: // special_id: "signature"
#line 1397 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 4185 "parser.cc"
    break;

  case 405: // special_sym: "!"
#line 1399 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 4191 "parser.cc"
    break;

  case 406: // special_sym: "."
#line 1400 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 4197 "parser.cc"
    break;

  case 407: // special_sym: "*"
#line 1401 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 4203 "parser.cc"
    break;

  case 408: // qconid: conid
#line 1405 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4209 "parser.cc"
    break;

  case 409: // qconid: "QCONID"
#line 1406 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4215 "parser.cc"
    break;

  case 410: // conid: "CONID"
#line 1408 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4221 "parser.cc"
    break;

  case 411: // qconsym: consym
#line 1410 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4227 "parser.cc"
    break;

  case 412: // qconsym: "QCONSYM"
#line 1411 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4233 "parser.cc"
    break;

  case 413: // consym: "CONSYM"
#line 1413 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4239 "parser.cc"
    break;

  case 414: // consym: ":"
#line 1414 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4245 "parser.cc"
    break;

  case 415: // literal: "CHAR"
#line 1418 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 4251 "parser.cc"
    break;

  case 416: // literal: "STRING"
#line 1419 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 4257 "parser.cc"
    break;

  case 417: // literal: "INTEGER"
#line 1420 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < int > ()});}
#line 4263 "parser.cc"
    break;

  case 418: // literal: "RATIONAL"
#line 1421 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Double{yystack_[0].value.as < double > ()});}
#line 4269 "parser.cc"
    break;

  case 419: // literal: "PRIMINTEGER"
#line 1422 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < int > ()});}
#line 4275 "parser.cc"
    break;

  case 421: // close: error
#line 1430 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 4281 "parser.cc"
    break;

  case 422: // modid: "CONID"
#line 1434 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4287 "parser.cc"
    break;

  case 423: // modid: "QCONID"
#line 1435 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4293 "parser.cc"
    break;

  case 424: // commas: commas ","
#line 1437 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 4299 "parser.cc"
    break;

  case 425: // commas: ","
#line 1438 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 4305 "parser.cc"
    break;


#line 4309 "parser.cc"

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

    const int yyn = yypact_[+yyparser_.yystack_[0].state];
    if (!yy_pact_value_is_default_ (yyn))
      {
        /* Start YYX at -YYN if negative to avoid negative indexes in
           YYCHECK.  In other words, skip the first -YYN actions for
           this state because they are default actions.  */
        const int yyxbegin = yyn < 0 ? -yyn : 0;
        // Stay within bounds of both yycheck and yytname.
        const int yychecklim = yylast_ - yyn + 1;
        const int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
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


  const short parser::yypact_ninf_ = -550;

  const short parser::yytable_ninf_ = -384;

  const short
  parser::yypact_[] =
  {
      23,    97,  -550,    75,  -550,  -550,  -550,  -550,  -550,   111,
     -39,    38,  -550,    51,    61,    61,    79,  -550,  -550,  -550,
    -550,   191,  -550,  -550,  -550,   124,  -550,   192,   203,  3737,
     260,   293,   217,  -550,   598,  -550,   -34,  -550,  -550,  -550,
    -550,    97,  -550,  -550,  -550,  -550,  -550,  -550,  -550,  -550,
    -550,  -550,  -550,  -550,  -550,  -550,  -550,  -550,  -550,  -550,
    -550,  -550,  -550,  -550,   158,  -550,  -550,  -550,  -550,   231,
     235,  -550,   246,  -550,  -550,  -550,  -550,  -550,  -550,  -550,
      -9,  -550,    97,  -550,   249,  -550,  2039,  3336,  -550,   256,
     139,  2039,  -550,  -550,  -550,   284,   194,  -550,  3336,   351,
     139,  2825,   263,   239,  4024,   161,  2432,  2825,  2563,  2825,
    1122,   991,   131,  -550,  -550,  -550,  -550,  -550,  -550,  -550,
      44,   263,   241,   217,  -550,  -550,  -550,   297,    54,  -550,
    -550,  2152,  -550,  2694,  -550,   272,  -550,  -550,  -550,  -550,
    -550,  -550,   285,    74,  -550,  -550,  -550,  -550,   247,  -550,
     265,   266,  -550,  -550,  -550,  -550,  -550,   269,  -550,   270,
     271,   273,  -550,  -550,  -550,  3737,  3774,  -550,  -550,  -550,
    -550,   372,  -550,   -46,   991,   357,   322,  -550,  -550,  2039,
    -550,  -550,  -550,  -550,  -550,  -550,  -550,  -550,  4116,  3033,
    2932,   274,  3929,  -550,  -550,  -550,  -550,  -550,   355,  3637,
    -550,   294,  -550,   216,  3336,  -550,  -550,  -550,  -550,  -550,
    -550,  -550,  -550,  -550,  -550,  -550,  -550,  3134,  1515,  1515,
    -550,   277,   308,   309,   314,   315,  3134,   729,   729,  -550,
     380,   317,   318,   364,  -550,  -550,    50,  4024,  -550,   332,
     135,    -8,   306,   -20,   301,   331,  -550,  -550,  2825,  -550,
    -550,  2301,  -550,  2694,   226,  -550,  -550,  3870,  -550,  -550,
    -550,   322,    81,   312,   303,  -550,  2039,  -550,  -550,  -550,
    -550,  -550,  -550,  2563,  -550,  -550,   125,   128,   271,   310,
     313,   316,   148,  -550,   143,  3134,  4024,  4024,  -550,    83,
     249,   292,  3336,  3134,  4116,  2039,  1777,  3870,  -550,    39,
    -550,  -550,  2170,  -550,  -550,  -550,  -550,  3637,  -550,  3966,
    2825,  -550,  -550,  -550,  -550,  -550,  -550,  -550,  -550,   319,
     321,   325,  -550,   323,    51,    97,    35,   345,   349,   245,
    3134,  2039,  -550,   132,    78,   335,   326,  -550,  -550,  -550,
    -550,   344,   347,  -550,   343,   350,  -550,   356,   339,   358,
     176,   184,   340,   342,   194,  -550,  -550,  3336,  3134,  -550,
    -550,   362,   360,   194,   139,  2825,   378,   387,    84,  -550,
    -550,    47,  -550,   454,  -550,  -550,  -550,  -550,  -550,  -550,
     355,   100,  -550,  -550,  2152,    48,  2039,  3134,   363,   354,
     384,   415,  -550,  -550,   418,   388,   161,   182,   420,  -550,
    2039,  -550,  -550,   383,   385,  2039,  2039,  1777,  1253,  -550,
    1253,   370,  -550,  1253,  -550,  1253,    87,  -550,  -550,  -550,
    -550,   422,   421,   423,  4070,   390,  -550,  -550,  -550,  -550,
    -550,     4,   180,  -550,  -550,  -550,  -550,   355,   424,   392,
    -550,   394,  -550,  -550,  -550,  -550,  -550,   402,  -550,   393,
     429,  -550,  -550,  -550,  3833,  -550,  -550,  -550,   404,  3737,
    -550,  -550,  -550,  -550,  1384,   860,  -550,  -550,  -550,   405,
    3134,  -550,  4116,  4161,  -550,  3134,  -550,  -550,  -550,  3134,
    -550,  -550,  -550,  -550,  -550,  -550,  -550,  -550,  -550,  3134,
     380,  -550,  -550,  2039,  -550,  1515,  -550,  2039,  -550,  -550,
     729,  -550,  -550,  -550,  4116,   408,  -550,  -550,  -550,  -550,
    -550,   410,   -12,   237,  -550,  -550,  -550,  -550,  -550,  -550,
    -550,   401,  -550,   439,  -550,  -550,  -550,  -550,  -550,  3134,
    3134,   403,   406,    83,  -550,   443,  3134,   494,  -550,   514,
    -550,  2039,  1777,  -550,  -550,  3966,  1253,  -550,  3737,   417,
    2825,  -550,  1646,  -550,   426,   416,  -550,   268,    51,  -550,
    -550,  -550,  -550,  3134,  4232,  -550,  -550,  -550,   427,  -550,
    -550,  -550,   277,  -550,   448,  -550,   350,  -550,   358,  -550,
    1777,  2039,  -550,   -15,    64,  -550,  -550,  -550,  -550,  -550,
    -550,  -550,   446,  -550,  3637,    21,  -550,   514,  -550,  -550,
    -550,  -550,  -550,   428,  -550,  -550,  -550,  -550,  1908,  1777,
    2039,  -550,    41,  -550,  -550,  -550,   451,  -550,   521,  3134,
    -550,  -550,  -550,  3134,  -550,  4195,   494,   445,  3437,  -550,
    -550,  -550,  -550,  -550,  -550,  3235,    65,   485,  -550,  -550,
    -550,  -550,  -550,   458,   355,  -550,  -550,  3134,  2039,  -550,
    -550,  -550,  -550,  3637,   430,  -550,  3637,  -550,  -550,   432,
     442,  -550,  3336,  -550,  2039,  -550,   444,  -550,  3537,  -550,
    3637,  3336,  -550,  -550,  -550,  -550,  -550
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    36,     0,     2,    36,     4,   422,   423,     8,
       0,    39,     1,     0,     0,     0,    18,    11,    35,    13,
      16,    58,   421,   420,    12,   102,    98,     0,     0,     0,
       0,    42,    37,    15,    14,   101,     0,     6,     7,   387,
     389,     0,   388,   375,   390,   391,   392,   373,   374,   372,
     376,   377,   393,   394,   395,   396,   397,   398,   399,   400,
     401,   402,   404,   403,     0,   370,   336,   369,   334,     0,
      19,    21,    24,    32,   326,   335,    31,   365,   368,   371,
       0,    41,     0,    34,    38,   235,     0,     0,    80,     0,
       0,     0,    51,    52,    53,    75,     0,    81,     0,     0,
       0,     0,   195,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   410,   409,   415,   416,   417,   418,   419,
     195,   195,    49,    56,    59,    60,    61,    88,     0,    64,
     177,    65,   203,   207,   217,   226,   228,   230,   296,   309,
     297,   108,   229,   368,   298,   408,   231,    99,     0,    23,
       0,     0,   384,   405,   407,   406,   385,     0,   382,     0,
       0,     0,   383,   386,    17,     0,    27,    22,    36,    36,
       3,    44,    33,     0,     0,     0,   200,   208,   201,     0,
     361,   362,   360,   342,   113,   343,   112,   134,   162,     0,
       0,     0,     0,   358,   341,   340,   338,   337,    97,     0,
     111,     0,    85,   120,   123,   126,   128,   132,   318,   129,
     323,   331,   339,   133,   130,   356,   359,   145,   283,   283,
     224,   211,     0,     0,     0,     0,     0,    92,    92,    95,
       0,     0,   120,     0,   225,   216,     0,     0,   196,     0,
       0,     0,     0,     0,   303,   103,   302,   300,     0,   274,
     277,     0,   219,   205,     0,   414,   310,     0,   413,   412,
     236,   200,   241,     0,   242,   351,     0,   350,   354,   381,
     380,   313,   411,   384,   305,   425,     0,     0,   381,     0,
     380,   313,     0,   307,     0,     0,     0,     0,    50,     0,
      57,     0,     0,     0,     0,     0,     0,     0,   179,    97,
     184,   349,     0,   348,   352,   379,   378,     0,   214,   290,
       0,   100,   329,   330,   328,   327,   367,   366,    20,     0,
       0,    28,    30,     0,     0,     0,    46,     0,     0,     0,
       0,     0,   209,     0,     0,     0,   163,   165,   363,   149,
     322,     0,     0,   116,     0,   113,   137,   146,     0,   331,
       0,     0,     0,     0,     0,    66,   135,     0,     0,   127,
     146,     0,   144,     0,     0,     0,   287,     0,     0,   282,
     284,     0,   210,     0,    72,    71,    73,    74,   141,   105,
      97,     0,   180,    91,     0,     0,     0,     0,     0,     0,
       0,     0,   206,   190,     0,     0,     0,     0,     0,   275,
       0,   276,   178,     0,     0,   237,   243,     0,     0,   234,
       0,   238,   232,     0,   233,     0,   366,   299,   306,   424,
     308,     0,     0,     0,     0,   187,   345,    55,   344,   346,
     311,     0,    82,   186,   117,   106,   107,    97,     0,   252,
     254,     0,   182,   183,   204,   215,   293,     0,   289,   292,
     295,   218,    26,    25,     0,     9,    10,    43,     0,     0,
      40,    45,   213,   212,     0,     0,   223,   199,   202,     0,
       0,   136,     0,     0,   139,     0,   321,   325,   140,     0,
     324,   319,   320,   332,   357,    96,    84,   121,    62,     0,
     288,   285,   273,     0,   278,   281,   279,     0,    70,    93,
      90,    94,   221,    67,     0,     0,   197,   189,   191,   301,
     304,     0,     0,     0,   104,   315,   188,   220,   355,   314,
     245,   247,   251,   236,   249,   248,   240,   239,   194,     0,
       0,     0,     0,     0,    87,     0,     0,   159,    69,   167,
     181,     0,     0,   353,   227,     0,     0,    29,     0,     0,
       0,   257,     0,   270,     0,   259,   263,     0,     0,   258,
     364,   166,   164,     0,     0,   148,   150,   115,   147,   147,
     286,   280,   211,    89,     0,   198,     0,   316,     0,   317,
       0,   244,   109,     0,     0,   347,   312,    54,    86,   152,
      83,   149,   153,   155,     0,     0,    68,   168,   170,   185,
     253,   291,   294,     0,    47,   271,   260,   255,   262,     0,
       0,   264,    97,   268,   256,   114,     0,   138,     0,     0,
     250,   246,   192,     0,   193,     0,   159,     0,   160,   124,
     131,   157,    78,    76,    77,     0,     0,   171,   174,   333,
     169,    48,   261,     0,    97,   266,   267,     0,     0,    63,
     110,   158,   154,     0,     0,   125,     0,   175,   122,   142,
       0,   172,     0,   173,     0,   265,     0,   222,   160,   156,
     161,     0,   176,    79,   269,   151,   143
  };

  const short
  parser::yypgoto_[] =
  {
    -550,  -550,  -550,  -550,  -550,  -550,  -550,    49,  -550,  -550,
    -402,  -550,   375,  -550,  -550,  -550,  -135,   433,  -550,  -550,
    -550,  -550,  -550,  -550,  -550,  -550,  -550,  -550,  -550,  -550,
    -550,  -550,  -550,  -550,  -550,  -550,  -550,  -550,  -550,  -550,
     255,  -550,   327,  -550,  -265,  -289,   538,  -550,  -550,  -295,
     -62,  -147,    28,  -550,  -550,  -167,   172,   -49,  -550,   -84,
    -550,   -94,  -338,  -550,   359,  -513,  -197,   275,  -109,  -550,
     348,   -27,  -550,   -78,  -550,  -550,   -55,  -550,   -80,  -550,
    -550,   102,  -550,  -550,   -25,   -61,   545,    80,   334,  -550,
     282,  -550,   240,  -550,   -65,   -81,   552,   -16,  -275,    17,
    -550,   -69,   -82,  -550,  -550,   -99,  -550,  -550,  -550,  -550,
     -19,  -550,  -550,  -400,  -550,   -17,  -550,  -550,   -14,  -550,
    -550,   341,  -550,   -72,   374,    99,  -283,  -550,    52,  -550,
    -550,  -550,  -550,   199,  -550,   -90,  -549,   -71,  -550,   212,
    -550,  -550,  -550,  -550,   -29,  -550,  -170,  -550,    63,  -550,
    -144,  -550,  -550,  -550,  -431,  -550,   431,  -257,   -21,  -162,
     -18,  -550,  -550,   -47,   -58,   -77,   -86,  -550,  -101,  -100,
     -53,  -234,  -550,  -284,    -5,  -105
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   170,     6,    10,    19,    30,
      69,    70,    71,   167,   320,   321,    72,    84,    11,    20,
      21,    32,    82,   326,   460,   461,   289,   122,   425,    33,
      34,   123,   124,   125,   126,   226,   636,   663,   127,   538,
     198,   292,   381,   229,   230,   355,    27,    36,   398,   378,
     433,   128,   583,   199,   200,   379,   435,   342,   627,   343,
     659,   203,   628,   204,   205,   629,   206,   380,   660,   361,
     348,   473,   565,   590,   539,   592,   593,   594,   631,   335,
     336,   337,   596,   597,   598,   637,   382,   383,   298,   299,
     300,   130,   237,   238,   366,   176,   384,   177,   178,   373,
     179,   133,   134,   135,   136,   276,   277,   263,   264,   521,
     438,   439,   466,   554,   555,   556,   611,   612,   613,   557,
     367,   250,   251,   220,   368,   369,   370,   447,   448,   449,
     137,   138,   244,   245,   139,   140,   426,   265,   514,   207,
     208,    73,   209,   638,   210,    75,   211,   212,   427,   428,
     302,   266,   303,   267,   213,   214,   215,   141,   142,    77,
      78,   304,   268,   269,   306,   162,    79,   163,   144,   145,
     271,   272,   146,    24,     9,   282
  };

  const short
  parser::yytable_[] =
  {
      74,   216,   356,   202,   232,   247,   161,   284,    76,   395,
     442,   262,   216,   440,   231,   246,   143,   160,   132,   235,
     349,   175,   341,   347,   249,   252,   221,   254,   234,   261,
     261,   322,   331,   270,   280,   467,   149,   436,   201,   253,
     456,   334,   566,   632,     1,   260,   260,   458,    22,    22,
     360,   308,    22,   279,    13,   430,   468,   549,   281,   255,
     301,    17,   622,   285,   278,   558,   354,   183,   354,   393,
     147,   327,   633,   634,   344,    12,   576,   171,   185,   656,
     148,   534,   328,   241,   305,   350,   351,   496,   274,   485,
     168,   498,   169,   261,   275,   403,   577,   280,   490,   623,
     258,   501,   275,   216,   216,   301,   216,   194,   195,   260,
     445,   196,   197,   216,   332,   655,   161,   405,   216,   656,
     394,   281,     2,   296,   522,   609,   434,   278,   635,   305,
     468,   216,   535,   616,   293,   441,    74,    74,   389,    66,
     216,   624,   459,    68,    76,    76,   603,   236,   540,    23,
      23,    18,   606,    23,  -363,   655,   404,   655,   470,   406,
     495,   500,   255,   352,    25,   407,   399,  -364,   294,   249,
     338,   308,   635,   152,   153,   154,    14,    15,   623,   390,
     155,   559,   161,    66,   494,   411,    29,    68,  -363,    26,
     301,   487,   294,   160,   566,   408,   404,   495,   424,   216,
     499,  -364,   156,   258,   253,    31,   216,   216,   202,   143,
     143,   132,   132,   500,   305,     7,   391,   323,   324,     8,
     434,   216,   152,   153,   154,   152,   153,   154,   451,   155,
     437,   429,   155,   412,   582,   582,   414,   150,   218,   413,
     219,   283,   415,   201,   216,   275,   151,   574,   152,   153,
     154,   156,    35,   420,   156,   155,   418,   419,   158,   600,
     536,   537,   419,   232,   242,   422,   423,   331,   243,    37,
     112,   216,   216,   486,   614,   469,   338,   156,   157,   113,
      38,   158,   159,   492,   481,   511,   444,    80,   450,   512,
     419,   513,   491,   227,   482,   228,   247,   620,   419,   430,
      66,   216,   358,   561,    68,  -118,   246,   295,   567,   524,
     296,   525,   568,   301,   526,   668,   527,    81,   670,   547,
     457,   502,   569,   645,   532,   334,   440,   261,   650,   261,
      83,   270,   261,   270,   261,   517,   270,   305,   270,   164,
     301,   520,   578,   523,   464,   260,   465,   579,   260,   165,
     260,   275,   609,   166,   610,   665,   222,   223,   224,   225,
     286,   287,   172,   217,   305,   233,   236,   239,   291,   589,
     288,   309,   310,   312,   313,   311,   325,   314,   315,   316,
     329,   317,   354,   357,   216,   374,   375,   216,   275,   216,
     372,   376,   377,   216,   430,   386,   615,   630,   387,   553,
     553,   255,   330,   216,   358,   388,   531,   350,   351,   392,
     256,   397,   152,   153,   154,   396,   409,   410,   416,   155,
     431,  -383,   462,   455,   417,    74,   463,   452,   570,   453,
      74,   630,   572,    76,   430,   471,   475,   297,    76,   454,
     472,   156,   258,   216,   216,   158,   259,   602,   474,   255,
     216,   476,   434,   479,   338,   483,   630,   484,   477,   630,
     152,   153,   154,  -272,   478,   261,   480,   155,   605,   270,
     488,   630,   493,   630,   489,   429,   599,   216,   216,   497,
     589,   260,   143,   505,   132,   297,   338,   553,   506,   156,
     258,   504,   507,   158,   259,   508,   509,   516,   518,   528,
     519,   529,   544,   530,   533,   541,   542,   545,   216,   543,
     546,   548,   575,   560,   340,   580,   621,   581,   585,    74,
     588,   586,   591,   595,   450,   604,   607,    76,   619,   608,
     626,   647,   648,   216,   653,   617,   641,   216,   662,   216,
     318,   232,   216,   553,   664,   644,   671,   432,   113,   216,
     672,   658,   675,    28,   532,   385,   290,   649,   584,   503,
     421,   216,   676,   359,   625,   362,   639,   216,   232,   666,
     216,   652,   640,   669,   562,   661,   216,   232,   673,   129,
     573,   443,   216,   667,   216,   216,   131,   658,   402,   618,
     643,   642,   401,   371,   571,   510,   587,   601,   646,   674,
       0,    85,    39,    86,    87,    88,    89,   639,    90,   515,
      40,    91,     0,     0,    92,    93,    94,    95,    96,     0,
      97,     0,    42,   353,    98,     0,    43,    99,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,   101,    58,
      59,    60,    61,    62,    63,   102,     0,     0,     0,     0,
     103,   104,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   105,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,   107,     0,   108,   109,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,     0,   111,     0,   112,     0,     0,
       0,     0,     0,     0,     0,    65,   113,     0,     0,    67,
     114,     0,     0,     0,     0,   115,   116,   117,   118,     0,
       0,   119,    85,    39,    86,   120,   121,     0,     0,    90,
       0,    40,    91,     0,     0,    92,    93,    94,     0,    96,
       0,     0,     0,    42,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,   101,
      58,    59,    60,    61,    62,    63,   102,     0,     0,     0,
       0,   103,   104,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   105,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,   107,     0,   108,
     109,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,   111,     0,   112,     0,
       0,     0,     0,     0,     0,     0,    65,   113,     0,     0,
      67,   114,     0,     0,     0,     0,   115,   116,   117,   118,
       0,    22,   119,    85,    39,    86,   120,   121,     0,     0,
      90,     0,    40,    91,     0,     0,     0,     0,     0,     0,
      96,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
     101,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,   103,   173,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,   107,     0,
     108,   550,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    23,   110,     0,     0,     0,   174,     0,   112,
       0,     0,     0,   552,     0,     0,     0,    65,   113,     0,
       0,    67,   114,     0,     0,     0,     0,   115,   116,   117,
     118,     0,     0,   119,    85,    39,    86,     0,     0,     0,
       0,    90,     0,    40,    91,     0,     0,     0,     0,     0,
       0,    96,     0,     0,     0,    42,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,   101,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,   103,   173,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     255,     0,     0,   106,     0,     0,     0,     0,     0,   107,
       0,   273,   153,   154,     0,     0,     0,     0,   155,     0,
       0,     0,     0,     0,   110,     0,     0,     0,   174,   274,
     112,     0,     0,     0,     0,   275,   257,     0,    65,   113,
     156,   258,    67,   114,   158,   259,     0,     0,   115,   116,
     117,   118,     0,     0,   119,    85,    39,    86,     0,     0,
       0,     0,    90,     0,    40,    91,     0,     0,     0,     0,
       0,     0,    96,     0,     0,     0,    42,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,   101,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,   103,   173,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   255,     0,     0,   106,     0,     0,     0,     0,     0,
     107,     0,   108,   153,   154,     0,     0,     0,     0,   155,
       0,     0,     0,     0,     0,   110,   256,     0,     0,   174,
       0,   112,     0,     0,     0,     0,     0,   257,     0,    65,
     113,   156,   258,    67,   114,   158,   259,     0,     0,   115,
     116,   117,   118,     0,     0,   119,    85,    39,    86,     0,
       0,     0,     0,    90,     0,    40,    91,     0,     0,     0,
       0,     0,     0,    96,     0,     0,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   100,
      50,    51,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,   101,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,   103,   173,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   255,     0,     0,   106,     0,     0,     0,     0,
       0,   107,     0,   108,   153,   154,     0,     0,     0,     0,
     155,     0,     0,     0,     0,     0,   110,     0,     0,     0,
     174,     0,   112,     0,     0,     0,     0,     0,   257,     0,
      65,   113,   156,   258,    67,   114,   158,   259,     0,     0,
     115,   116,   117,   118,     0,     0,   119,    85,    39,    86,
       0,     0,     0,     0,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,   103,   173,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,   107,     0,   108,   550,     0,     0,     0,     0,
       0,     0,     0,     0,   551,     0,     0,   110,     0,     0,
       0,   174,     0,   112,     0,     0,     0,   552,     0,     0,
       0,    65,   113,     0,     0,    67,   114,     0,     0,     0,
       0,   115,   116,   117,   118,     0,     0,   119,    85,    39,
      86,     0,     0,     0,     0,    90,     0,    40,    91,     0,
       0,     0,     0,     0,     0,   363,     0,     0,     0,    42,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,     0,    54,    55,    56,
       0,   364,    57,     0,     0,   101,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,   103,   173,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,   107,     0,   108,   365,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   110,     0,
       0,     0,   174,     0,   112,     0,     0,     0,     0,     0,
       0,     0,    65,   113,     0,     0,    67,   114,     0,     0,
       0,     0,   115,   116,   117,   118,     0,     0,   119,    85,
      39,    86,     0,     0,     0,     0,    90,     0,    40,    91,
       0,     0,     0,     0,     0,     0,    96,     0,     0,     0,
      42,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,   101,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,   103,   173,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,   107,     0,   108,   550,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,   174,     0,   112,     0,     0,     0,   552,
       0,     0,     0,    65,   113,     0,     0,    67,   114,     0,
       0,     0,     0,   115,   116,   117,   118,     0,     0,   119,
      85,    39,    86,     0,     0,     0,     0,    90,     0,    40,
      91,     0,     0,     0,     0,     0,     0,   363,     0,     0,
       0,    42,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,   101,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,   103,
     173,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,   107,     0,   108,   365,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,   174,     0,   112,     0,     0,     0,
       0,     0,     0,     0,    65,   113,     0,     0,    67,   114,
       0,     0,     0,     0,   115,   116,   117,   118,     0,     0,
     119,    85,    39,    86,     0,     0,     0,     0,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,    42,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,   101,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
     103,   173,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,   107,     0,   108,   550,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,     0,   174,     0,   112,     0,     0,
       0,     0,     0,     0,     0,    65,   113,     0,     0,    67,
     114,     0,     0,     0,     0,   115,   116,   117,   118,     0,
       0,   119,    85,    39,    86,     0,     0,     0,     0,    90,
       0,    40,    91,     0,     0,     0,     0,     0,     0,    96,
       0,     0,     0,    42,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,   101,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,   103,   173,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,   107,     0,   108,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,   174,     0,   112,     0,
       0,     0,     0,     0,     0,     0,    65,   113,     0,     0,
      67,   114,     0,     0,     0,     0,   115,   116,   117,   118,
       0,     0,   119,    85,    39,    86,     0,     0,     0,     0,
      90,     0,    40,    91,     0,     0,     0,     0,     0,     0,
      96,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
     101,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,   255,   103,   295,     0,     0,   296,     0,     0,     0,
       0,     0,   152,   153,   154,     0,     0,     0,     0,   155,
       0,     0,   106,     0,     0,     0,     0,     0,   107,     0,
     108,     0,     0,     0,     0,     0,     0,   297,     0,     0,
       0,   156,   258,   110,     0,   158,   259,   174,     0,   112,
       0,     0,     0,     0,     0,     0,     0,    65,   113,     0,
       0,    67,   114,     0,     0,     0,     0,   115,   116,   117,
     118,     0,     0,   119,    85,    39,    86,     0,     0,     0,
       0,    90,     0,    40,    91,     0,     0,     0,     0,     0,
       0,    96,     0,     0,     0,    42,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,   400,     0,   107,
       0,     0,   248,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   110,     0,     0,     0,   174,     0,
     112,     0,     0,     0,     0,     0,     0,     0,    65,   113,
       0,     0,    67,   114,     0,     0,     0,     0,   115,   116,
     117,   118,     0,     0,   119,    85,    39,    86,     0,     0,
       0,     0,    90,     0,    40,    91,     0,     0,     0,     0,
       0,     0,    96,     0,     0,     0,    42,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
     107,     0,     0,   248,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,   174,
       0,   112,     0,     0,     0,     0,     0,     0,     0,    65,
     113,     0,     0,    67,   114,     0,     0,     0,     0,   115,
     116,   117,   118,     0,     0,   119,    85,    39,    86,     0,
       0,     0,     0,    90,     0,    40,    91,     0,     0,     0,
       0,     0,     0,    96,     0,     0,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   100,
      50,    51,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,   101,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,   107,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
     174,     0,   112,     0,     0,     0,     0,     0,     0,     0,
      65,   113,     0,     0,    67,   114,     0,     0,     0,     0,
     115,   116,   117,   118,     0,     0,   119,    85,    39,    86,
       0,     0,     0,     0,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   307,     0,     0,     0,     0,   110,     0,     0,
       0,   174,     0,   112,     0,     0,     0,     0,     0,     0,
       0,    65,   113,     0,     0,    67,   114,     0,     0,     0,
       0,   115,   116,   117,   118,     0,     0,   119,    85,    39,
      86,     0,     0,     0,     0,    90,     0,    40,    91,     0,
       0,     0,     0,     0,     0,    96,     0,     0,     0,    42,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,   107,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   110,     0,
       0,     0,   174,     0,   112,     0,    39,     0,     0,     0,
       0,     0,    65,   113,    40,     0,    67,   114,     0,     0,
       0,     0,   115,   116,   117,   118,    42,     0,   119,     0,
     339,     0,    44,    45,    46,   180,   181,   182,     0,     0,
       0,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   183,     0,     0,     0,     0,     0,     0,   344,     0,
     345,     0,   185,   186,   187,     0,     0,     0,     0,     0,
       0,   188,     0,     0,     0,   189,     0,    39,     0,   190,
     346,   191,     0,     0,     0,    40,   275,   192,     0,   193,
      66,   194,   195,     0,    68,   196,   197,    42,     0,     0,
       0,   339,     0,    44,    45,    46,   180,   181,   182,     0,
       0,     0,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   183,     0,     0,     0,     0,     0,     0,     0,
       0,   184,     0,   185,   186,   187,     0,     0,     0,     0,
       0,     0,   188,     0,     0,     0,   189,   340,    39,     0,
     190,     0,   191,     0,     0,     0,    40,     0,   192,     0,
     193,    66,   194,   195,     0,    68,   196,   197,    42,     0,
       0,     0,   339,     0,    44,    45,    46,   180,   181,   182,
       0,     0,     0,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   183,     0,     0,     0,     0,     0,     0,
       0,     0,   184,     0,   185,   186,   187,     0,     0,     0,
       0,     0,     0,   188,     0,     0,     0,   189,     0,    39,
       0,   190,     0,   191,     0,     0,     0,    40,     0,   192,
       0,   193,    66,   194,   195,     0,    68,   196,   197,    42,
       0,     0,     0,     0,     0,    44,    45,    46,   180,   181,
     182,     0,     0,     0,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   183,     0,     0,     0,     0,     0,
       0,     0,     0,   184,     0,   185,   186,   187,     0,     0,
       0,     0,     0,     0,   188,     0,     0,     0,   189,     0,
      39,     0,   190,   657,   191,     0,     0,     0,    40,     0,
     192,     0,   193,    66,   194,   195,     0,    68,   196,   197,
      42,     0,     0,     0,     0,     0,    44,    45,    46,   180,
     181,   182,     0,     0,     0,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   183,     0,     0,     0,     0,
       0,     0,     0,     0,   184,     0,   185,   186,   187,     0,
       0,     0,     0,     0,     0,   188,     0,     0,     0,   189,
       0,    39,     0,   190,     0,   191,     0,     0,     0,    40,
       0,   192,     0,   193,    66,   194,   195,     0,    68,   196,
     197,    42,     0,     0,     0,     0,     0,    44,    45,    46,
     180,   181,   182,     0,     0,     0,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   255,     0,     0,     0,
       0,     0,     0,     0,     0,   184,  -119,     0,   186,   187,
       0,     0,     0,     0,     0,     0,   188,     0,     0,     0,
     189,    39,     0,     0,   190,     0,   191,     0,     0,    40,
       0,     0,   654,     0,   193,    66,     0,   258,     0,    68,
       0,    42,     0,     0,     0,     0,     0,    44,    45,    46,
     180,   181,   182,     0,     0,     0,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   255,     0,     0,     0,
       0,     0,     0,     0,     0,   184,     0,     0,   186,   187,
       0,     0,     0,     0,     0,     0,   188,     0,     0,     0,
     189,    39,     0,     0,   190,     0,   191,     0,     0,    40,
       0,     0,   654,     0,   193,    66,     0,   258,     0,    68,
       0,    42,     0,     0,     0,     0,     0,    44,    45,    46,
     180,   181,   182,     0,     0,     0,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   184,     0,     0,   186,   187,
       0,     0,     0,     0,     0,     0,   188,     0,     0,     0,
     189,    39,     0,     0,   190,     0,   191,     0,     0,    40,
       0,     0,     0,     0,   193,    66,     0,     0,    41,    68,
       0,    42,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,     0,    50,    51,    52,    53,    39,    54,
      55,    56,     0,     0,    57,     0,    40,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
       0,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,    39,     0,     0,
       0,     0,     0,     0,    64,    40,     0,     0,     0,     0,
       0,     0,   319,     0,    65,    66,     0,    42,    67,    68,
       0,    43,     0,    44,    45,    46,    47,    48,    49,     0,
      50,    51,    52,    53,    39,    54,    55,    56,     0,     0,
      57,    64,    40,     0,    58,    59,    60,    61,    62,    63,
       0,    65,    66,     0,    42,    67,    68,     0,    43,     0,
      44,    45,    46,    47,    48,    49,     0,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,    39,     0,     0,     0,     0,     0,     0,
      64,    40,     0,     0,     0,     0,     0,     0,     0,     0,
      65,    66,     0,    42,    67,    68,     0,     0,     0,    44,
      45,    46,   180,   181,   182,     0,     0,     0,    52,    53,
      39,    54,    55,    56,     0,     0,    57,     0,    40,     0,
      58,    59,    60,    61,    62,    63,     0,    65,   113,     0,
      42,    67,   114,     0,    43,     0,    44,    45,    46,    47,
      48,    49,     0,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,    39,     0,
       0,     0,     0,     0,     0,     0,    40,     0,     0,     0,
       0,     0,     0,     0,   446,     0,   193,    66,    42,     0,
       0,    68,    43,     0,    44,    45,    46,    47,    48,    49,
       0,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,   240,    39,    58,    59,    60,    61,    62,
      63,     0,    40,    65,     0,     0,     0,    67,     0,     0,
       0,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,     0,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
      39,    58,    59,    60,    61,    62,    63,     0,    40,     0,
       0,   240,     0,     0,     0,     0,     0,     0,     0,     0,
      42,    65,     0,     0,    43,    67,    44,    45,    46,    47,
      48,    49,     0,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,    39,     0,    58,    59,    60,
      61,    62,    63,    40,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    42,     0,    65,   113,     0,
       0,    44,    45,    46,   180,   181,   182,     0,     0,    39,
      52,    53,     0,    54,    55,    56,     0,    40,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,    42,
       0,     0,     0,   333,     0,    44,    45,    46,   180,   181,
     182,     0,     0,    65,    52,    53,    39,    54,    55,    56,
       0,     0,    57,     0,    40,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,    42,     0,   563,     0,
       0,     0,    44,    45,    46,   180,   181,   182,   564,     0,
       0,    52,    53,     0,    54,    55,    56,     0,   193,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,   651,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   564,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   193,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   193
  };

  const short
  parser::yycheck_[] =
  {
      29,    87,   199,    87,    98,   105,    64,   112,    29,   243,
     299,   110,    98,   296,    98,   105,    34,    64,    34,   101,
     190,    86,   189,   190,   106,   107,    91,   109,   100,   110,
     111,   166,   176,   110,   111,   330,    41,   294,    87,   108,
     324,   188,   473,    22,    21,   110,   111,    12,     1,     1,
     217,   133,     1,   111,     5,   289,   331,   459,   111,    79,
     131,   100,    77,    19,   111,   465,    27,    79,    27,    77,
     104,   117,    51,    52,    86,     0,    88,    82,    90,   628,
     114,    77,   128,   104,   131,   190,   191,   371,   108,   354,
      99,   380,   101,   174,   114,   257,   108,   174,   363,   114,
     120,   385,   114,   189,   190,   176,   192,   119,   120,   174,
     307,   123,   124,   199,   179,   628,   174,   261,   204,   668,
     128,   174,    99,    84,   407,    84,   293,   174,   107,   176,
     405,   217,   128,   564,    80,   297,   165,   166,    88,   118,
     226,    77,   107,   122,   165,   166,   548,   103,   437,   102,
     102,   113,   552,   102,    80,   668,   257,   670,    80,    78,
     113,   113,    79,   192,   103,    84,   248,    80,   114,   251,
     188,   253,   107,    90,    91,    92,    65,    66,   114,   129,
      97,   465,   240,   118,   100,   266,   107,   122,   114,   128,
     261,   358,   114,   240,   625,   114,   297,   113,   115,   285,
     100,   114,   119,   120,   273,    14,   292,   293,   292,   227,
     228,   227,   228,   113,   261,   118,   237,   168,   169,   122,
     387,   307,    90,    91,    92,    90,    91,    92,   310,    97,
     295,   289,    97,   108,   529,   530,   108,    79,    99,   114,
     101,   110,   114,   292,   330,   114,    88,   504,    90,    91,
      92,   119,   128,   110,   119,    97,   108,   114,   123,   542,
      80,    81,   114,   357,   103,   286,   287,   411,   107,    77,
     109,   357,   358,   357,   558,   333,   294,   119,   120,   118,
      77,   123,   124,   365,   108,   103,   302,    27,   309,   107,
     114,   109,   364,    99,   110,   101,   396,   580,   114,   533,
     118,   387,    86,   470,   122,    89,   396,    81,   475,   408,
      84,   410,   479,   384,   413,   653,   415,    24,   656,   454,
     325,   386,   489,   612,   424,   472,   609,   408,   623,   410,
     113,   408,   413,   410,   415,   400,   413,   384,   415,   108,
     411,   406,   512,   408,    99,   410,   101,   110,   413,   114,
     415,   114,    84,   107,    86,   644,    72,    73,    74,    75,
     120,   121,   113,   107,   411,    14,   103,   128,    71,   536,
     129,    99,    87,   108,   108,   128,     4,   108,   108,   108,
      23,   108,    27,    89,   470,    77,    77,   473,   114,   475,
     113,    77,    77,   479,   628,    15,   563,   594,    81,   464,
     465,    79,    80,   489,    86,    41,   424,   512,   513,    77,
     104,    80,    90,    91,    92,   114,   104,   114,   108,    97,
     128,   108,    77,   100,   108,   454,    77,   108,   493,   108,
     459,   628,   497,   454,   668,   100,    89,   115,   459,   114,
     114,   119,   120,   529,   530,   123,   124,   546,   104,    79,
     536,   108,   619,   114,   472,   115,   653,   115,   108,   656,
      90,    91,    92,    85,   108,   546,   108,    97,   550,   546,
     108,   668,    85,   670,   114,   533,   541,   563,   564,    25,
     647,   546,   500,   129,   500,   115,   504,   552,   104,   119,
     120,   128,    77,   123,   124,    77,   108,    77,   115,    77,
     115,    80,   100,    80,   114,    81,   114,   114,   594,   115,
      81,   107,   104,   108,   104,   114,   581,    78,   115,   548,
      77,   115,    28,     9,   545,   108,   100,   548,    80,   113,
      84,    80,    11,   619,    89,   108,   108,   623,    53,   625,
     165,   635,   628,   608,    86,   610,   114,   292,   118,   635,
     108,   635,   108,    15,   654,   228,   123,   619,   530,   387,
     285,   647,   671,   204,   591,   217,   595,   653,   662,   647,
     656,   626,   597,   653,   472,   636,   662,   671,   662,    34,
     500,   299,   668,   648,   670,   671,    34,   671,   254,   572,
     609,   608,   251,   219,   495,   396,   533,   545,   612,   664,
      -1,     3,     4,     5,     6,     7,     8,   636,    10,   397,
      12,    13,    -1,    -1,    16,    17,    18,    19,    20,    -1,
      22,    -1,    24,   192,    26,    -1,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,
     122,    -1,    -1,    -1,    -1,   127,   128,   129,   130,    -1,
      -1,   133,     3,     4,     5,   137,   138,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    16,    17,    18,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,   130,
      -1,     1,   133,     3,     4,     5,   137,   138,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   102,   103,    -1,    -1,    -1,   107,    -1,   109,
      -1,    -1,    -1,   113,    -1,    -1,    -1,   117,   118,    -1,
      -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,
     130,    -1,    -1,   133,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    92,    -1,    -1,    -1,    -1,    97,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,   108,
     109,    -1,    -1,    -1,    -1,   114,   115,    -1,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,   127,   128,
     129,   130,    -1,    -1,   133,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,    97,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,   115,    -1,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,   127,
     128,   129,   130,    -1,    -1,   133,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,
      97,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,    -1,    -1,   115,    -1,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    -1,
     127,   128,   129,   130,    -1,    -1,   133,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,    -1,    -1,   103,    -1,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,   113,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,
      -1,   127,   128,   129,   130,    -1,    -1,   133,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    46,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,
      -1,    -1,   127,   128,   129,   130,    -1,    -1,   133,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,   113,
      -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,
      -1,    -1,    -1,   127,   128,   129,   130,    -1,    -1,   133,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,    -1,    -1,    -1,   127,   128,   129,   130,    -1,    -1,
     133,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,
     122,    -1,    -1,    -1,    -1,   127,   128,   129,   130,    -1,
      -1,   133,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,   130,
      -1,    -1,   133,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    79,    62,    81,    -1,    -1,    84,    -1,    -1,    -1,
      -1,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,    97,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
      -1,   119,   120,   103,    -1,   123,   124,   107,    -1,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,
      -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,
     130,    -1,    -1,   133,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    86,    -1,    88,
      -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,
     129,   130,    -1,    -1,   133,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,
     128,   129,   130,    -1,    -1,   133,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,
     127,   128,   129,   130,    -1,    -1,   133,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    -1,    -1,    -1,    -1,   103,    -1,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,
      -1,   127,   128,   129,   130,    -1,    -1,   133,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,   107,    -1,   109,    -1,     4,    -1,    -1,    -1,
      -1,    -1,   117,   118,    12,    -1,   121,   122,    -1,    -1,
      -1,    -1,   127,   128,   129,   130,    24,    -1,   133,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    86,    -1,
      88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,    -1,   103,    -1,     4,    -1,   107,
     108,   109,    -1,    -1,    -1,    12,   114,   115,    -1,   117,
     118,   119,   120,    -1,   122,   123,   124,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,    -1,   103,   104,     4,    -1,
     107,    -1,   109,    -1,    -1,    -1,    12,    -1,   115,    -1,
     117,   118,   119,   120,    -1,   122,   123,   124,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,   103,    -1,     4,
      -1,   107,    -1,   109,    -1,    -1,    -1,    12,    -1,   115,
      -1,   117,   118,   119,   120,    -1,   122,   123,   124,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,    -1,
       4,    -1,   107,   108,   109,    -1,    -1,    -1,    12,    -1,
     115,    -1,   117,   118,   119,   120,    -1,   122,   123,   124,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,
      -1,     4,    -1,   107,    -1,   109,    -1,    -1,    -1,    12,
      -1,   115,    -1,   117,   118,   119,   120,    -1,   122,   123,
     124,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    89,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,
     103,     4,    -1,    -1,   107,    -1,   109,    -1,    -1,    12,
      -1,    -1,   115,    -1,   117,   118,    -1,   120,    -1,   122,
      -1,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,
     103,     4,    -1,    -1,   107,    -1,   109,    -1,    -1,    12,
      -1,    -1,   115,    -1,   117,   118,    -1,   120,    -1,   122,
      -1,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,
     103,     4,    -1,    -1,   107,    -1,   109,    -1,    -1,    12,
      -1,    -1,    -1,    -1,   117,   118,    -1,    -1,    21,   122,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,     4,    42,
      43,    44,    -1,    -1,    47,    -1,    12,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,   107,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    78,    -1,   117,   118,    -1,    24,   121,   122,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,     4,    42,    43,    44,    -1,    -1,
      47,   107,    12,    -1,    51,    52,    53,    54,    55,    56,
      -1,   117,   118,    -1,    24,   121,   122,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,
     107,    12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117,   118,    -1,    24,   121,   122,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
       4,    42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,
      51,    52,    53,    54,    55,    56,    -1,   117,   118,    -1,
      24,   121,   122,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    78,    -1,   117,   118,    24,    -1,
      -1,   122,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,   107,     4,    51,    52,    53,    54,    55,
      56,    -1,    12,   117,    -1,    -1,    -1,   121,    -1,    -1,
      -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
       4,    51,    52,    53,    54,    55,    56,    -1,    12,    -1,
      -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      24,   117,    -1,    -1,    28,   121,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,     4,    -1,    51,    52,    53,
      54,    55,    56,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    24,    -1,   117,   118,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,     4,
      39,    40,    -1,    42,    43,    44,    -1,    12,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    24,
      -1,    -1,    -1,   107,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,   117,    39,    40,     4,    42,    43,    44,
      -1,    -1,    47,    -1,    12,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    24,    -1,    97,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,   107,    -1,
      -1,    39,    40,    -1,    42,    43,    44,    -1,   117,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117
  };

  const short
  parser::yystos_[] =
  {
       0,    21,    99,   140,   141,   142,   145,   118,   122,   313,
     146,   157,     0,   146,    65,    66,   143,   100,   113,   147,
     158,   159,     1,   102,   312,   103,   128,   185,   185,   107,
     148,    14,   160,   168,   169,   128,   186,    77,    77,     4,
      12,    21,    24,    28,    30,    31,    32,    33,    34,    35,
      37,    38,    39,    40,    42,    43,    44,    47,    51,    52,
      53,    54,    55,    56,   107,   117,   118,   121,   122,   149,
     150,   151,   155,   280,   283,   284,   297,   298,   299,   305,
      27,    24,   161,   113,   156,     3,     5,     6,     7,     8,
      10,    13,    16,    17,    18,    19,    20,    22,    26,    29,
      36,    50,    57,    62,    63,    76,    82,    88,    90,    91,
     103,   107,   109,   118,   122,   127,   128,   129,   130,   133,
     137,   138,   166,   170,   171,   172,   173,   177,   190,   225,
     230,   235,   236,   240,   241,   242,   243,   269,   270,   273,
     274,   296,   297,   299,   307,   308,   311,   104,   114,   313,
      79,    88,    90,    91,    92,    97,   119,   120,   123,   124,
     302,   303,   304,   306,   108,   114,   107,   152,    99,   101,
     144,   313,   113,    63,   107,   233,   234,   236,   237,   239,
      33,    34,    35,    79,    88,    90,    91,    92,    99,   103,
     107,   109,   115,   117,   119,   120,   123,   124,   179,   192,
     193,   196,   198,   200,   202,   203,   205,   278,   279,   281,
     283,   285,   286,   293,   294,   295,   305,   107,    99,   101,
     262,   233,    72,    73,    74,    75,   174,    99,   101,   182,
     183,   198,   200,    14,   262,   241,   103,   231,   232,   128,
     107,   297,   103,   107,   271,   272,   274,   308,    91,   241,
     260,   261,   241,   240,   241,    79,   104,   115,   120,   124,
     233,   234,   244,   246,   247,   276,   290,   292,   301,   302,
     304,   309,   310,    90,   108,   114,   244,   245,   302,   303,
     304,   309,   314,   110,   314,    19,   231,   231,   129,   165,
     156,    71,   180,    80,   114,    81,    84,   115,   227,   228,
     229,   276,   289,   291,   300,   302,   303,    98,   241,    99,
      87,   128,   108,   108,   108,   108,   108,   108,   151,    78,
     153,   154,   155,   146,   146,     4,   162,   117,   128,    23,
      80,   289,   233,   107,   190,   218,   219,   220,   299,    28,
     104,   194,   196,   198,    86,    88,   108,   194,   209,   285,
     314,   314,   283,   295,    27,   184,   205,    89,    86,   203,
     194,   208,   209,    20,    46,    91,   233,   259,   263,   264,
     265,   263,   113,   238,    77,    77,    77,    77,   188,   194,
     206,   181,   225,   226,   235,   181,    15,    81,    41,    88,
     129,   297,    77,    77,   128,   310,   114,    80,   187,   241,
      86,   260,   227,   298,   307,   289,    78,    84,   114,   104,
     114,   234,   108,   114,   108,   114,   108,   108,   108,   114,
     110,   206,   297,   297,   115,   167,   275,   287,   288,   303,
     310,   128,   179,   189,   194,   195,   296,   233,   249,   250,
     265,   298,   184,   229,   236,   205,    78,   266,   267,   268,
     297,   241,   108,   108,   114,   100,   312,   313,    12,   107,
     163,   164,    77,    77,    99,   101,   251,   188,   237,   303,
      80,   100,   114,   210,   104,    89,   108,   108,   108,   114,
     108,   108,   110,   115,   115,   183,   198,   194,   108,   114,
     183,   262,   241,    85,   100,   113,   312,    25,   184,   100,
     113,   312,   233,   195,   128,   129,   104,    77,    77,   108,
     272,   103,   107,   109,   277,   278,    77,   233,   115,   115,
     233,   248,   265,   233,   244,   244,   244,   244,    77,    80,
      80,   299,   308,   114,    77,   128,    80,    81,   178,   213,
     184,    81,   114,   115,   100,   114,    81,   155,   107,   149,
      91,   100,   113,   233,   252,   253,   254,   258,   252,   312,
     108,   194,   220,    97,   107,   211,   293,   194,   194,   194,
     233,   264,   233,   226,   296,   104,    88,   108,   285,   110,
     114,    78,   188,   191,   191,   115,   115,   287,    77,   194,
     212,    28,   214,   215,   216,     9,   221,   222,   223,   233,
     265,   267,   244,   149,   108,   241,   252,   100,   113,    84,
      86,   255,   256,   257,   312,   194,   293,   108,   238,    80,
     265,   233,    77,   114,    77,   210,    84,   197,   201,   204,
     205,   217,    22,    51,    52,   107,   175,   224,   282,   283,
     223,   108,   254,   249,   233,   184,   257,    80,    11,   189,
     188,    97,   215,    89,   115,   204,   275,   108,   198,   199,
     207,   224,    53,   176,    86,   184,   212,   233,   201,   217,
     201,   114,   108,   198,   233,   108,   207
  };

  const short
  parser::yyr1_[] =
  {
       0,   139,   140,   141,   141,   142,   143,   143,   143,   144,
     144,   145,   145,   146,   147,   147,   147,   148,   148,   149,
     150,   150,   151,   151,   152,   152,   152,   153,   153,   154,
     154,   155,   155,   156,   156,   157,   157,   158,   159,   159,
     160,   161,   161,   162,   162,   163,   163,   164,   164,   165,
     165,   166,   166,   166,   167,   167,   168,   169,   169,   170,
     170,   170,   170,   170,   170,   170,   171,   172,   172,   172,
     173,   174,   174,   174,   174,   174,   175,   175,   175,   176,
     177,   177,   178,   178,   179,   179,   180,   180,   180,   181,
     181,   181,   181,   182,   182,   183,   184,   184,   185,   185,
     186,   186,   186,   187,   187,   188,   189,   190,   190,   191,
     191,   192,   193,   193,   194,   194,   194,   195,   196,   197,
     198,   198,   199,   200,   201,   201,   202,   202,   203,   203,
     203,   204,   205,   205,   205,   205,   205,   205,   205,   205,
     205,   206,   207,   207,   208,   208,   209,   209,   210,   210,
     211,   211,   212,   213,   214,   214,   215,   215,   216,   216,
     217,   217,   218,   218,   219,   219,   220,   221,   221,   222,
     222,   223,   223,   223,   224,   224,   224,   225,   225,   225,
     226,   227,   227,   228,   228,   229,   230,   230,   230,   230,
     230,   230,   230,   230,   230,   231,   231,   232,   232,   233,
     233,   234,   234,   235,   235,   236,   236,   236,   237,   237,
     238,   238,   239,   239,   240,   240,   240,   240,   241,   241,
     241,   241,   241,   241,   241,   241,   241,   242,   242,   243,
     243,   243,   243,   243,   243,   243,   244,   244,   244,   245,
     245,   246,   246,   246,   246,   246,   246,   246,   247,   247,
     248,   248,   249,   250,   250,   251,   251,   251,   251,   252,
     252,   253,   253,   253,   254,   255,   255,   256,   256,   257,
     258,   258,   259,   259,   260,   260,   261,   261,   262,   262,
     263,   263,   263,   263,   264,   264,   265,   265,   265,   266,
     266,   267,   267,   267,   268,   268,   269,   269,   270,   270,
     271,   271,   271,   272,   272,   273,   273,   273,   273,   274,
     274,   275,   275,   276,   276,   277,   277,   277,   278,   278,
     278,   278,   278,   279,   279,   279,   280,   280,   280,   280,
     280,   281,   281,   282,   283,   283,   284,   285,   285,   285,
     286,   286,   286,   286,   287,   287,   288,   288,   289,   289,
     290,   290,   291,   291,   292,   292,   293,   294,   295,   295,
     295,   295,   295,   296,   296,   297,   297,   297,   298,   298,
     299,   299,   299,   299,   299,   299,   299,   299,   300,   300,
     301,   301,   302,   303,   303,   304,   304,   305,   305,   305,
     305,   305,   305,   305,   305,   305,   305,   305,   305,   305,
     305,   305,   305,   305,   305,   306,   306,   306,   307,   307,
     308,   309,   309,   310,   310,   311,   311,   311,   311,   311,
     312,   312,   313,   313,   314,   314
  };

  const signed char
  parser::yyr2_[] =
  {
       0,     2,     1,     6,     1,     0,     3,     3,     0,     3,
       3,     3,     3,     2,     2,     2,     1,     3,     0,     1,
       3,     1,     2,     2,     0,     3,     3,     0,     1,     3,
       1,     1,     1,     2,     1,     2,     0,     2,     3,     0,
       5,     1,     0,     2,     0,     1,     0,     3,     4,     0,
       1,     1,     1,     1,     3,     1,     2,     3,     0,     1,
       1,     1,     4,     7,     1,     1,     3,     4,     5,     4,
       4,     2,     2,     2,     2,     0,     1,     1,     1,     2,
       1,     1,     0,     2,     3,     1,     4,     3,     0,     3,
       2,     1,     0,     3,     3,     1,     2,     0,     1,     3,
       3,     1,     0,     0,     2,     1,     1,     3,     1,     1,
       3,     1,     1,     1,     4,     3,     1,     1,     1,     1,
       1,     3,     1,     1,     1,     2,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     2,     3,     2,     5,     3,
       3,     1,     1,     3,     1,     0,     1,     3,     2,     0,
       1,     5,     1,     2,     3,     1,     4,     2,     3,     0,
       1,     3,     0,     1,     3,     1,     3,     0,     1,     2,
       1,     2,     3,     3,     1,     2,     3,     1,     3,     2,
       1,     3,     2,     2,     1,     4,     3,     3,     4,     4,
       3,     4,     6,     6,     4,     0,     1,     3,     4,     3,
       1,     1,     3,     1,     3,     2,     3,     1,     1,     2,
       1,     0,     3,     3,     2,     3,     2,     1,     3,     2,
       4,     4,     8,     4,     2,     2,     1,     4,     1,     1,
       1,     1,     3,     3,     3,     1,     1,     2,     2,     3,
       3,     1,     1,     2,     4,     3,     5,     3,     3,     3,
       3,     1,     1,     3,     1,     3,     3,     2,     2,     1,
       2,     3,     2,     1,     2,     3,     2,     2,     1,     4,
       1,     2,     1,     2,     1,     2,     2,     1,     3,     3,
       3,     2,     1,     0,     1,     2,     3,     1,     2,     1,
       0,     3,     1,     1,     3,     1,     1,     1,     1,     3,
       1,     3,     1,     1,     3,     2,     3,     2,     3,     1,
       2,     1,     3,     1,     3,     1,     2,     2,     1,     3,
       3,     3,     2,     1,     3,     3,     1,     3,     3,     3,
       3,     1,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     3,     1,     1,
       1,     1,     1,     3,     1,     3,     1,     3,     1,     1,
       1,     1,     1,     1,     3,     1,     3,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     1
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
  "\"type\"", "\"where\"", "\"forall\"", "\"foreign\"", "\"export\"",
  "\"label\"", "\"dynamic\"", "\"safe\"", "\"interruptible\"",
  "\"unsafe\"", "\"mdo\"", "\"family\"", "\"role\"", "\"stdcall\"",
  "\"ccall\"", "\"bpcall\"", "\"capi\"", "\"prim\"", "\"javascript\"",
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
  "export_subspec", "qcnames", "qcnames1", "qcname", "semis1", "semis",
  "importdecls", "importdecls_semi", "importdecl", "optqualified",
  "maybeas", "maybeimpspec", "impspec", "prec", "infix", "ops", "topdecls",
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
  "op", "varop", "qop", "qopm", "qvarop", "qvaropm", "tyvar", "tyvarop",
  "tyvarid", "var", "qvar", "qvarid", "varid", "qvarsym",
  "qvarsym_no_minus", "qvarsym1", "varsym", "varsym_no_minus",
  "special_id", "special_sym", "qconid", "conid", "qconsym", "consym",
  "literal", "close", "modid", "commas", YY_NULLPTR
  };
#endif


#if YYDEBUG
  const short
  parser::yyrline_[] =
  {
       0,   474,   474,   491,   492,   494,   498,   499,   500,   502,
     503,   505,   506,   509,   511,   512,   513,   521,   522,   524,
     526,   527,   529,   530,   532,   533,   534,   536,   537,   539,
     540,   542,   543,   547,   548,   550,   551,   553,   555,   556,
     558,   571,   572,   574,   575,   577,   578,   582,   583,   588,
     589,   591,   592,   593,   595,   596,   600,   602,   603,   605,
     606,   607,   610,   611,   618,   620,   622,   624,   625,   626,
     631,   636,   637,   638,   639,   640,   642,   643,   644,   646,
     686,   687,   689,   690,   699,   700,   702,   703,   704,   748,
     749,   750,   751,   753,   754,   756,   758,   759,   767,   768,
     770,   771,   772,   785,   786,   788,   790,   792,   793,   795,
     796,   800,   806,   807,   814,   815,   817,   819,   828,   830,
     832,   833,   835,   838,   840,   841,   843,   844,   846,   847,
     848,   854,   861,   862,   863,   864,   865,   866,   867,   873,
     874,   878,   880,   881,   883,   884,   886,   887,   894,   895,
     898,   899,   917,   923,   925,   926,   928,   929,   931,   932,
     934,   935,   937,   938,   940,   941,   943,   945,   946,   948,
     949,   951,   952,   953,   955,   956,   957,   962,   964,   965,
     967,   971,   972,   974,   975,   979,   989,   990,   992,   993,
     994,   995,   996,   997,   998,  1001,  1002,  1004,  1005,  1009,
    1010,  1012,  1013,  1015,  1016,  1018,  1019,  1020,  1022,  1023,
    1026,  1027,  1029,  1030,  1034,  1035,  1036,  1037,  1039,  1040,
    1041,  1042,  1044,  1046,  1047,  1048,  1050,  1052,  1053,  1055,
    1056,  1057,  1058,  1059,  1064,  1065,  1070,  1071,  1072,  1077,
    1078,  1096,  1097,  1098,  1099,  1100,  1101,  1102,  1104,  1105,
    1118,  1120,  1130,  1132,  1133,  1136,  1137,  1138,  1139,  1141,
    1142,  1144,  1145,  1146,  1148,  1150,  1151,  1153,  1154,  1163,
    1165,  1166,  1168,  1169,  1171,  1172,  1174,  1175,  1178,  1179,
    1181,  1182,  1183,  1184,  1189,  1190,  1192,  1193,  1194,  1199,
    1200,  1202,  1203,  1204,  1206,  1207,  1239,  1240,  1242,  1243,
    1245,  1246,  1247,  1249,  1250,  1252,  1253,  1254,  1255,  1257,
    1258,  1260,  1261,  1263,  1264,  1267,  1268,  1269,  1271,  1272,
    1273,  1274,  1275,  1277,  1278,  1279,  1281,  1282,  1283,  1284,
    1285,  1288,  1289,  1291,  1293,  1294,  1298,  1300,  1301,  1302,
    1304,  1305,  1306,  1307,  1312,  1313,  1315,  1316,  1318,  1319,
    1322,  1323,  1328,  1329,  1331,  1332,  1336,  1338,  1340,  1341,
    1342,  1343,  1344,  1347,  1348,  1350,  1351,  1352,  1354,  1355,
    1357,  1358,  1359,  1360,  1361,  1362,  1363,  1364,  1366,  1367,
    1369,  1370,  1372,  1374,  1375,  1377,  1378,  1380,  1381,  1382,
    1383,  1384,  1385,  1386,  1387,  1388,  1389,  1390,  1391,  1392,
    1393,  1394,  1395,  1396,  1397,  1399,  1400,  1401,  1405,  1406,
    1408,  1410,  1411,  1413,  1414,  1418,  1419,  1420,  1421,  1422,
    1427,  1430,  1434,  1435,  1437,  1438
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
#line 6055 "parser.cc"

#line 1447 "parser.y"


using std::optional;
using std::string;
using std::vector;
using std::pair;

void
yy::parser::error (const location_type& l, const std::string& m)
{
    drv.push_error_message({l,m});
}

pair<vector<Hs::ImpDecl>, optional<Hs::Decls>> make_body(const std::vector<Hs::ImpDecl>& imports, const std::optional<Hs::Decls>& topdecls)
{
    if (topdecls)
    {
        auto topdecls2 = Hs::Decls(*topdecls);
        return {imports, topdecls2};
    }
    else
        return {imports, {}};
}

// See PostProcess.hs:checkTyClHdr
std::tuple<string, vector<Hs::Type>>
check_type_or_class_header(const Hs::Type& type)
{
    auto [type_head, type_args] = Hs::decompose_type_apps(type);

    // FIXME -- add location!
    if (not type_head.is_a<Hs::TypeCon>())
        throw myexception()<<"Malformed type or class header '"<<type<<"'";
    auto name = unloc(type_head.as_<Hs::TypeCon>().name);

    return {name, type_args};
}

vector<Hs::TypeVar> check_all_type_vars(const vector<Hs::Type>& types)
{
    vector<Hs::TypeVar> type_vars;
    for(auto& type: types)
    {
        if (type.is_a<Hs::TypeVar>())
        {
            type_vars.push_back(type.as_<Hs::TypeVar>());
        }
        else
        {
            throw myexception()<<"Type '"<<type<<"' is not a type variable";
        }
    }
    return type_vars;
}

Hs::TypeSynonymDecl make_type_synonym(const Located<Hs::Type>& lhs_type, const Located<Hs::Type>& rhs_type)
{
    auto [name, type_args] = check_type_or_class_header(unloc(lhs_type));
    return {name, check_all_type_vars(type_args), rhs_type};
}

Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context&  context,
                                           const Hs::Type& header, const vector<Hs::Constructor>& constrs)
{
    auto [name, type_args] = check_type_or_class_header(header);
    if (d_or_n == Hs::DataOrNewtype::newtype and constrs.size() != 1)
        throw myexception()<<"newtype '"<<name<<"' may only have 1 constructors with 1 field";
    return {d_or_n, name, check_all_type_vars(type_args), context, constrs};
}

Hs::InstanceDecl make_instance_decl(const Located<Hs::Type>& ltype, const optional<Located<Hs::Binds>>& binds)
{
    // GHC stores the instance as a polytype?
    // This would seem to allow (instance forall a.Eq a => forall a.Eq [a] x y ....)

    auto type = unloc(ltype);
    if (type.is_a<Hs::ForallType>())
        throw myexception()<<"instance declaration '"<<type<<"' is malformed";
    Hs::Context context;
    if (type.is_a<Hs::ConstrainedType>())
    {
        auto& T = type.as_<Hs::ConstrainedType>();
        context = T.context;
        type = T.type;
    }

    return {context, type, binds};
}

Hs::ClassDecl make_class_decl(const Hs::Context& context, const Hs::Type& header, const optional<Located<Hs::Binds>>& binds)
{
    auto [name, type_args] = check_type_or_class_header(header);
    return {name, check_all_type_vars(type_args), context, binds};
}

// Can we change the context parsing rule to expect:
// nothing
// | ctype => header
// | ( ctypes2 ) => header
Hs::Context make_context(const Hs::Type& context)
{
    vector<Hs::Type> constraints;
    if (context.is_a<Hs::TupleType>())
    {
        constraints = context.as_<Hs::TupleType>().element_types;
    }
    else
        constraints.push_back(context);

    return {constraints};
}

std::optional<Hs::Kind> type_to_kind_(const Hs::Type& kind)
{
    auto [kind_head, kind_args] = Hs::decompose_type_apps(kind);

    if (not kind_head.is_a<Hs::TypeCon>()) return {};
    auto V = kind_head.as_<Hs::TypeCon>();
    auto head_name = unloc(V.name);

    if (kind_args.empty())
    {
        if (head_name == "*")
            return kind_star();
        else
            return {};
    }
    else if (kind_args.size() == 2)
    {
        auto k1 = type_to_kind_(kind_args[0]);
        auto k2 = type_to_kind_(kind_args[1]);
        if (k1 and k2 and head_name == "->")
            return kind_arrow(*k1, *k2);
        else
            return {};
    }
    else
        return {};
}

Hs::Kind type_to_kind(const Hs::Type& kind)
{
    auto maybe_kind = type_to_kind_(kind);

    if (not maybe_kind)
        throw myexception()<<"Kind '"<<kind<<"' is malformed";

    return *maybe_kind;
}

optional<pair<string, Hs::FieldDecls>> is_record_con(const Hs::Type& typeish)
{
    auto [head,args] = Hs::decompose_type_apps(typeish);

    if (args.size() != 1) return {};

    if (not head.is_a<Hs::TypeCon>()) return {};

    if (not args[0].is_a<Hs::FieldDecls>()) return {};

    return {{unloc(head.as_<Hs::TypeCon>().name), args[0].as_<Hs::FieldDecls>()}};
}

optional<pair<string, std::vector<Hs::Type>>> is_normal_con(const Hs::Type& typeish)
{
    if (is_record_con(typeish)) return {};

    auto [head,args] = Hs::decompose_type_apps(typeish);

    if (not head.is_a<Hs::TypeCon>())
        return {};

    return {{unloc(head.as_<Hs::TypeCon>().name), args}};
}

Hs::Constructor make_constructor(const vector<Hs::TypeVar>& forall, const std::optional<Hs::Context>& c, const Hs::Type& typeish)
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

expression_ref make_minus(const expression_ref& exp)
{
    return Hs::InfixExp({Hs::Neg(),exp});
}

Hs::ApplyExp make_apply(const Hs::Exp& head, const Hs::Exp& arg)
{
    if (auto app = head.to<Hs::ApplyExp>())
    {
        auto App = *app;
        App.args.push_back(arg);
        return App;
    }
    else
        return Hs::ApplyExp(head, {arg});
}
