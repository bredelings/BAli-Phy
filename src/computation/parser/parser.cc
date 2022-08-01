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
      case symbol_kind::S_infixtype: // infixtype
      case symbol_kind::S_ftype: // ftype
      case symbol_kind::S_tyarg: // tyarg
      case symbol_kind::S_tyop: // tyop
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
      case symbol_kind::S_infixtype: // infixtype
      case symbol_kind::S_ftype: // ftype
      case symbol_kind::S_tyarg: // tyarg
      case symbol_kind::S_tyop: // tyop
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
      case symbol_kind::S_infixtype: // infixtype
      case symbol_kind::S_ftype: // ftype
      case symbol_kind::S_tyarg: // tyarg
      case symbol_kind::S_tyop: // tyop
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
      case symbol_kind::S_infixtype: // infixtype
      case symbol_kind::S_ftype: // ftype
      case symbol_kind::S_tyarg: // tyarg
      case symbol_kind::S_tyop: // tyop
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
      case symbol_kind::S_infixtype: // infixtype
      case symbol_kind::S_ftype: // ftype
      case symbol_kind::S_tyarg: // tyarg
      case symbol_kind::S_tyop: // tyop
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
#line 478 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2111 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 495 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < std::string > (),yystack_[2].value.as < std::optional<std::vector<Hs::Export>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2117 "parser.cc"
    break;

  case 4: // module: body2
#line 496 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{"Main",{},yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2123 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 498 "parser.y"
                                                                 {drv.push_module_context();}
#line 2129 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 506 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2135 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 507 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2141 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 509 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2147 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 510 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2153 "parser.cc"
    break;

  case 13: // top: semis top1
#line 513 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2159 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 515 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2165 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 516 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2171 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 517 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::ImpDecl> > (),{});}
#line 2177 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 525 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::Export>> > () = yystack_[1].value.as < std::vector<Hs::Export> > ();}
#line 2183 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 526 "parser.y"
                                      {}
#line 2189 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 528 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > () = yystack_[0].value.as < std::vector<Hs::Export> > ();}
#line 2195 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 530 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > () = yystack_[2].value.as < std::vector<Hs::Export> > (); yylhs.value.as < std::vector<Hs::Export> > ().push_back(yystack_[0].value.as < Hs::Export > ());}
#line 2201 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 531 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > ().push_back(yystack_[0].value.as < Hs::Export > ());}
#line 2207 "parser.cc"
    break;

  case 22: // export: qcname export_subspec
#line 533 "parser.y"
                                      {yylhs.value.as < Hs::Export > () = Hs::ExportSymbol{yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}; }
#line 2213 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 534 "parser.y"
                                      {yylhs.value.as < Hs::Export > () = Hs::ExportModule{{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2219 "parser.cc"
    break;

  case 24: // export_subspec: %empty
#line 536 "parser.y"
                                      {}
#line 2225 "parser.cc"
    break;

  case 25: // export_subspec: "(" qcnames ")"
#line 537 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpecSome{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2231 "parser.cc"
    break;

  case 26: // export_subspec: "(" ".." ")"
#line 538 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpecAll(); }
#line 2237 "parser.cc"
    break;

  case 27: // qcnames: %empty
#line 540 "parser.y"
                   {}
#line 2243 "parser.cc"
    break;

  case 28: // qcnames: qcnames1
#line 541 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2249 "parser.cc"
    break;

  case 29: // qcnames1: qcnames1 "," qcname
#line 543 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2255 "parser.cc"
    break;

  case 30: // qcnames1: qcname
#line 544 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2261 "parser.cc"
    break;

  case 31: // qcname: qvar
#line 546 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2267 "parser.cc"
    break;

  case 32: // qcname: oqtycon_no_varcon
#line 547 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2273 "parser.cc"
    break;

  case 37: // importdecls: importdecls_semi importdecl
#line 557 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[1].value.as < std::vector<Hs::ImpDecl> > (), yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[0].value.as < Hs::ImpDecl > ()); }
#line 2279 "parser.cc"
    break;

  case 38: // importdecls_semi: importdecls_semi importdecl semis1
#line 559 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[2].value.as < std::vector<Hs::ImpDecl> > (); yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[1].value.as < Hs::ImpDecl > ()); }
#line 2285 "parser.cc"
    break;

  case 39: // importdecls_semi: %empty
#line 560 "parser.y"
                         { }
#line 2291 "parser.cc"
    break;

  case 40: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 562 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::ImpDecl > () = Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < std::string > (),yystack_[1].value.as < std::optional<std::string> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ());
}
#line 2299 "parser.cc"
    break;

  case 41: // optqualified: "qualified"
#line 575 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2305 "parser.cc"
    break;

  case 42: // optqualified: %empty
#line 576 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2311 "parser.cc"
    break;

  case 43: // maybeas: "as" modid
#line 578 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 2317 "parser.cc"
    break;

  case 44: // maybeas: %empty
#line 579 "parser.y"
                               { }
#line 2323 "parser.cc"
    break;

  case 45: // maybeimpspec: impspec
#line 581 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2329 "parser.cc"
    break;

  case 46: // maybeimpspec: %empty
#line 582 "parser.y"
                               { }
#line 2335 "parser.cc"
    break;

  case 47: // impspec: "(" exportlist ")"
#line 586 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::Export> > ()}; }
#line 2341 "parser.cc"
    break;

  case 48: // impspec: "hiding" "(" exportlist ")"
#line 587 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::Export> > ()}; }
#line 2347 "parser.cc"
    break;

  case 49: // prec: %empty
#line 592 "parser.y"
                   { }
#line 2353 "parser.cc"
    break;

  case 50: // prec: "INTEGER"
#line 593 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < int > (); }
#line 2359 "parser.cc"
    break;

  case 51: // infix: "infix"
#line 595 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2365 "parser.cc"
    break;

  case 52: // infix: "infixl"
#line 596 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2371 "parser.cc"
    break;

  case 53: // infix: "infixr"
#line 597 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2377 "parser.cc"
    break;

  case 54: // ops: ops "," op
#line 599 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 2383 "parser.cc"
    break;

  case 55: // ops: op
#line 600 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 2389 "parser.cc"
    break;

  case 56: // topdecls: topdecls_semi topdecl
#line 604 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 2395 "parser.cc"
    break;

  case 57: // topdecls_semi: topdecls_semi topdecl semis1
#line 606 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2401 "parser.cc"
    break;

  case 58: // topdecls_semi: %empty
#line 607 "parser.y"
                                            { }
#line 2407 "parser.cc"
    break;

  case 59: // topdecl: cl_decl
#line 609 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2413 "parser.cc"
    break;

  case 60: // topdecl: ty_decl
#line 610 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2419 "parser.cc"
    break;

  case 61: // topdecl: inst_decl
#line 611 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2425 "parser.cc"
    break;

  case 62: // topdecl: "default" "(" comma_types0 ")"
#line 614 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Hs::DefaultDecl(yystack_[1].value.as < std::vector<Hs::Type> > ()); }
#line 2431 "parser.cc"
    break;

  case 63: // topdecl: "foreign" "import" "bpcall" "STRING" var "::" sigtypedoc
#line 615 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = Hs::ForeignDecl(yystack_[3].value.as < std::string > (), yystack_[2].value.as < std::string > (), yystack_[0].value.as < Hs::Type > ());}
#line 2437 "parser.cc"
    break;

  case 64: // topdecl: decl_no_th
#line 622 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2443 "parser.cc"
    break;

  case 65: // topdecl: infixexp_top
#line 624 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < Hs::InfixExp > ();}
#line 2449 "parser.cc"
    break;

  case 66: // cl_decl: "class" tycl_hdr wherebinds
#line 626 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_class_decl(yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().first,yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().second,yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 2455 "parser.cc"
    break;

  case 67: // ty_decl: "type" type "=" ctypedoc
#line 628 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_type_synonym({yystack_[2].location,yystack_[2].value.as < Hs::Type > ()},{yystack_[0].location,yystack_[0].value.as < Hs::Type > ()});}
#line 2461 "parser.cc"
    break;

  case 68: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 629 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (),yystack_[2].value.as < std::pair<Hs::Context,Hs::Type> > ().first,yystack_[2].value.as < std::pair<Hs::Context,Hs::Type> > ().second,yystack_[1].value.as < std::vector<Hs::Constructor> > ());}
#line 2467 "parser.cc"
    break;

  case 69: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig
#line 630 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[3].value.as < Hs::DataOrNewtype > (),yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().first,yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().second,{});}
#line 2473 "parser.cc"
    break;

  case 70: // inst_decl: "instance" overlap_pragma inst_type wherebinds
#line 635 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_instance_decl({yystack_[1].location,yystack_[1].value.as < Hs::Type > ()},yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 2479 "parser.cc"
    break;

  case 80: // data_or_newtype: "data"
#line 690 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 2485 "parser.cc"
    break;

  case 81: // data_or_newtype: "newtype"
#line 691 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 2491 "parser.cc"
    break;

  case 84: // tycl_hdr: context "=>" type
#line 703 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::Type> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::Type > ()};}
#line 2497 "parser.cc"
    break;

  case 85: // tycl_hdr: type
#line 704 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::Type> > () = {{},yystack_[0].value.as < Hs::Type > ()};}
#line 2503 "parser.cc"
    break;

  case 89: // decls: decls ";" decl
#line 752 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2509 "parser.cc"
    break;

  case 90: // decls: decls ";"
#line 753 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2515 "parser.cc"
    break;

  case 91: // decls: decl
#line 754 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2521 "parser.cc"
    break;

  case 92: // decls: %empty
#line 755 "parser.y"
                        {}
#line 2527 "parser.cc"
    break;

  case 93: // decllist: "{" decls "}"
#line 757 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2533 "parser.cc"
    break;

  case 94: // decllist: "vocurly" decls close
#line 758 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2539 "parser.cc"
    break;

  case 95: // binds: decllist
#line 760 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 2545 "parser.cc"
    break;

  case 96: // wherebinds: "where" binds
#line 762 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 2551 "parser.cc"
    break;

  case 97: // wherebinds: %empty
#line 763 "parser.y"
                                 {}
#line 2557 "parser.cc"
    break;

  case 103: // opt_tyconsig: %empty
#line 789 "parser.y"
                     {}
#line 2563 "parser.cc"
    break;

  case 104: // opt_tyconsig: "::" gtycon
#line 790 "parser.y"
                     {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2569 "parser.cc"
    break;

  case 105: // sigtype: ctype
#line 792 "parser.y"
         { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2575 "parser.cc"
    break;

  case 106: // sigtypedoc: ctypedoc
#line 794 "parser.y"
            { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2581 "parser.cc"
    break;

  case 107: // sig_vars: sig_vars "," var
#line 796 "parser.y"
                           {yylhs.value.as < std::vector<Hs::Var> > () = yystack_[2].value.as < std::vector<Hs::Var> > (); yylhs.value.as < std::vector<Hs::Var> > ().push_back(Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}));}
#line 2587 "parser.cc"
    break;

  case 108: // sig_vars: var
#line 797 "parser.y"
                           {yylhs.value.as < std::vector<Hs::Var> > ().push_back(Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}));}
#line 2593 "parser.cc"
    break;

  case 109: // sigtypes1: sigtype
#line 799 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2599 "parser.cc"
    break;

  case 110: // sigtypes1: sigtypes1 "," sigtype
#line 800 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[2].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2605 "parser.cc"
    break;

  case 111: // strict_mark: strictness
#line 804 "parser.y"
             { yylhs.value.as < Hs::StrictLazy > () = yystack_[0].value.as < Hs::StrictLazy > (); }
#line 2611 "parser.cc"
    break;

  case 112: // strictness: PREFIX_BANG
#line 810 "parser.y"
                         {yylhs.value.as < Hs::StrictLazy > () = Hs::StrictLazy::strict;}
#line 2617 "parser.cc"
    break;

  case 113: // strictness: PREFIX_TILDE
#line 811 "parser.y"
                         {yylhs.value.as < Hs::StrictLazy > () = Hs::StrictLazy::lazy;}
#line 2623 "parser.cc"
    break;

  case 114: // ctype: "forall" tv_bndrs "." ctype
#line 818 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::ForallType(yystack_[2].value.as < std::vector<Hs::TypeVar> > (), yystack_[0].value.as < Hs::Type > ());}
#line 2629 "parser.cc"
    break;

  case 115: // ctype: context "=>" ctype
#line 819 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::Type > ());}
#line 2635 "parser.cc"
    break;

  case 116: // ctype: type
#line 821 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2641 "parser.cc"
    break;

  case 117: // ctypedoc: ctype
#line 823 "parser.y"
          { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2647 "parser.cc"
    break;

  case 118: // context: btype
#line 832 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::Type > ());}
#line 2653 "parser.cc"
    break;

  case 119: // context_no_ops: btype_no_ops
#line 834 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ()));}
#line 2659 "parser.cc"
    break;

  case 120: // type: btype
#line 836 "parser.y"
      { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2665 "parser.cc"
    break;

  case 121: // type: btype "->" ctype
#line 837 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,"->"}),yystack_[2].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()});}
#line 2671 "parser.cc"
    break;

  case 122: // typedoc: type
#line 839 "parser.y"
         { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2677 "parser.cc"
    break;

  case 123: // btype: infixtype
#line 842 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2683 "parser.cc"
    break;

  case 124: // infixtype: ftype
#line 844 "parser.y"
           { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2689 "parser.cc"
    break;

  case 125: // infixtype: btype "~" btype
#line 845 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,"~"}),yystack_[2].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()});}
#line 2695 "parser.cc"
    break;

  case 126: // btype_no_ops: atype_docs
#line 847 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2701 "parser.cc"
    break;

  case 127: // btype_no_ops: btype_no_ops atype_docs
#line 848 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[1].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2707 "parser.cc"
    break;

  case 128: // ftype: atype
#line 850 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2713 "parser.cc"
    break;

  case 129: // ftype: tyop
#line 851 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2719 "parser.cc"
    break;

  case 130: // ftype: ftype tyarg
#line 852 "parser.y"
                                   { yylhs.value.as < Hs::Type > () = Hs::TypeApp(yystack_[1].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()); }
#line 2725 "parser.cc"
    break;

  case 131: // tyarg: atype
#line 854 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2731 "parser.cc"
    break;

  case 132: // tyop: qtyconop
#line 856 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2737 "parser.cc"
    break;

  case 133: // tyop: tyvarop
#line 857 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2743 "parser.cc"
    break;

  case 134: // atype_docs: atype
#line 864 "parser.y"
            { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2749 "parser.cc"
    break;

  case 135: // atype: ntgtycon
#line 871 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2755 "parser.cc"
    break;

  case 136: // atype: tyvar
#line 872 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2761 "parser.cc"
    break;

  case 137: // atype: "*"
#line 873 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,"*"});}
#line 2767 "parser.cc"
    break;

  case 138: // atype: strict_mark atype
#line 874 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::StrictLazyType(yystack_[1].value.as < Hs::StrictLazy > (),yystack_[0].value.as < Hs::Type > ());}
#line 2773 "parser.cc"
    break;

  case 139: // atype: "{" fielddecls "}"
#line 875 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ());}
#line 2779 "parser.cc"
    break;

  case 140: // atype: "(" ")"
#line 876 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[1].location,"()"});}
#line 2785 "parser.cc"
    break;

  case 141: // atype: "(" comma_types1 "," ctype ")"
#line 877 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::Type> > ();ts.push_back(yystack_[1].value.as < Hs::Type > ());yylhs.value.as < Hs::Type > () = Hs::TupleType(ts);}
#line 2791 "parser.cc"
    break;

  case 142: // atype: "[" ctype "]"
#line 883 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::ListType{yystack_[1].value.as < Hs::Type > ()}; }
#line 2797 "parser.cc"
    break;

  case 143: // atype: "(" ctype ")"
#line 884 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = yystack_[1].value.as < Hs::Type > ();}
#line 2803 "parser.cc"
    break;

  case 144: // inst_type: sigtype
#line 888 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 2809 "parser.cc"
    break;

  case 147: // comma_types0: comma_types1
#line 893 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[0].value.as < std::vector<Hs::Type> > ();}
#line 2815 "parser.cc"
    break;

  case 148: // comma_types0: %empty
#line 894 "parser.y"
                                       { /* default construction OK */ }
#line 2821 "parser.cc"
    break;

  case 149: // comma_types1: ctype
#line 896 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2827 "parser.cc"
    break;

  case 150: // comma_types1: comma_types1 "," ctype
#line 897 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[2].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2833 "parser.cc"
    break;

  case 151: // tv_bndrs: tv_bndrs tv_bndr
#line 904 "parser.y"
                               {yylhs.value.as < std::vector<Hs::TypeVar> > () = yystack_[1].value.as < std::vector<Hs::TypeVar> > (); yylhs.value.as < std::vector<Hs::TypeVar> > ().push_back(yystack_[0].value.as < Hs::TypeVar > ());}
#line 2839 "parser.cc"
    break;

  case 152: // tv_bndrs: %empty
#line 905 "parser.y"
                               { /* default construction OK */}
#line 2845 "parser.cc"
    break;

  case 153: // tv_bndr: tyvar
#line 908 "parser.y"
                                    {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2851 "parser.cc"
    break;

  case 154: // tv_bndr: "(" tyvar "::" kind ")"
#line 909 "parser.y"
                                    {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[3].location,yystack_[3].value.as < std::string > ()},yystack_[1].value.as < expression_ref > ());}
#line 2857 "parser.cc"
    break;

  case 155: // kind: ctype
#line 927 "parser.y"
             {yylhs.value.as < expression_ref > () = type_to_kind(yystack_[0].value.as < Hs::Type > ());}
#line 2863 "parser.cc"
    break;

  case 156: // constrs: "=" constrs1
#line 933 "parser.y"
                                {yylhs.value.as < std::vector<Hs::Constructor> > () = yystack_[0].value.as < std::vector<Hs::Constructor> > ();}
#line 2869 "parser.cc"
    break;

  case 157: // constrs1: constrs1 "|" constr
#line 935 "parser.y"
                                {yylhs.value.as < std::vector<Hs::Constructor> > () = yystack_[2].value.as < std::vector<Hs::Constructor> > (); yylhs.value.as < std::vector<Hs::Constructor> > ().push_back(yystack_[0].value.as < Hs::Constructor > ());}
#line 2875 "parser.cc"
    break;

  case 158: // constrs1: constr
#line 936 "parser.y"
                                {yylhs.value.as < std::vector<Hs::Constructor> > ().push_back(yystack_[0].value.as < Hs::Constructor > ());}
#line 2881 "parser.cc"
    break;

  case 159: // constr: forall context_no_ops "=>" constr_stuff
#line 938 "parser.y"
                                                {yylhs.value.as < Hs::Constructor > () = make_constructor(yystack_[3].value.as < std::vector<Hs::TypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::Type > ());}
#line 2887 "parser.cc"
    break;

  case 160: // constr: forall constr_stuff
#line 939 "parser.y"
                                                {yylhs.value.as < Hs::Constructor > () = make_constructor(yystack_[1].value.as < std::vector<Hs::TypeVar> > (),{}, yystack_[0].value.as < Hs::Type > ());}
#line 2893 "parser.cc"
    break;

  case 161: // forall: "forall" tv_bndrs "."
#line 941 "parser.y"
                                {yylhs.value.as < std::vector<Hs::TypeVar> > () = yystack_[1].value.as < std::vector<Hs::TypeVar> > ();}
#line 2899 "parser.cc"
    break;

  case 162: // forall: %empty
#line 942 "parser.y"
                                {}
#line 2905 "parser.cc"
    break;

  case 163: // constr_stuff: btype_no_ops
#line 944 "parser.y"
                                                {yylhs.value.as < Hs::Type > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ());}
#line 2911 "parser.cc"
    break;

  case 164: // constr_stuff: btype_no_ops conop btype_no_ops
#line 945 "parser.y"
                                                {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,yystack_[1].value.as < std::string > ()}),Hs::make_tyapps(yystack_[2].value.as < std::vector<Hs::Type> > ()),Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ())});}
#line 2917 "parser.cc"
    break;

  case 165: // fielddecls: %empty
#line 947 "parser.y"
                                {}
#line 2923 "parser.cc"
    break;

  case 166: // fielddecls: fielddecls1
#line 948 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 2929 "parser.cc"
    break;

  case 167: // fielddecls1: fielddecls1 "," fielddecl
#line 950 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 2935 "parser.cc"
    break;

  case 168: // fielddecls1: fielddecl
#line 951 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 2941 "parser.cc"
    break;

  case 169: // fielddecl: sig_vars "::" ctype
#line 953 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::Var> > (),yystack_[0].value.as < Hs::Type > ());}
#line 2947 "parser.cc"
    break;

  case 180: // decl_no_th: sigdecl
#line 972 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2953 "parser.cc"
    break;

  case 181: // decl_no_th: PREFIX_BANG aexp rhs
#line 974 "parser.y"
                                      {yylhs.value.as < expression_ref > () = Hs::StrictValueDecl{{yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < Hs::MultiGuardedRHS > ()}; }
#line 2959 "parser.cc"
    break;

  case 182: // decl_no_th: infixexp_top rhs
#line 975 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::ValueDecl({yystack_[1].location,yystack_[1].value.as < Hs::InfixExp > ()},yystack_[0].value.as < Hs::MultiGuardedRHS > ());}
#line 2965 "parser.cc"
    break;

  case 183: // decl: decl_no_th
#line 977 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2971 "parser.cc"
    break;

  case 184: // rhs: "=" exp wherebinds
#line 981 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 2977 "parser.cc"
    break;

  case 185: // rhs: gdrhs wherebinds
#line 982 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 2983 "parser.cc"
    break;

  case 186: // gdrhs: gdrhs gdrh
#line 984 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 2989 "parser.cc"
    break;

  case 187: // gdrhs: gdrh
#line 985 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 2995 "parser.cc"
    break;

  case 188: // gdrh: "|" guardquals "=" exp
#line 989 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ()};}
#line 3001 "parser.cc"
    break;

  case 189: // sigdecl: sig_vars "::" sigtypedoc
#line 999 "parser.y"
                                  { yylhs.value.as < expression_ref > () = Hs::SignatureDecl{yystack_[2].value.as < std::vector<Hs::Var> > (),yystack_[0].value.as < Hs::Type > ()}; }
#line 3007 "parser.cc"
    break;

  case 190: // sigdecl: infix prec ops
#line 1000 "parser.y"
                         { yylhs.value.as < expression_ref > () = Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()}; }
#line 3013 "parser.cc"
    break;

  case 191: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1002 "parser.y"
                                                    {}
#line 3019 "parser.cc"
    break;

  case 192: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1003 "parser.y"
                                            {}
#line 3025 "parser.cc"
    break;

  case 193: // sigdecl: "{-# SCC" qvar "#-}"
#line 1004 "parser.y"
                              {}
#line 3031 "parser.cc"
    break;

  case 194: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1005 "parser.y"
                                     {}
#line 3037 "parser.cc"
    break;

  case 195: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1006 "parser.y"
                                                               {}
#line 3043 "parser.cc"
    break;

  case 196: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1007 "parser.y"
                                                                      {}
#line 3049 "parser.cc"
    break;

  case 197: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1008 "parser.y"
                                                     {}
#line 3055 "parser.cc"
    break;

  case 202: // exp: infixexp "::" sigtype
#line 1019 "parser.y"
                           { yylhs.value.as < expression_ref > () = Hs::TypedExp(yystack_[2].value.as < Hs::InfixExp > (),yystack_[0].value.as < Hs::Type > ()); }
#line 3061 "parser.cc"
    break;

  case 203: // exp: infixexp
#line 1020 "parser.y"
                           { yylhs.value.as < expression_ref > () = yystack_[0].value.as < Hs::InfixExp > (); }
#line 3067 "parser.cc"
    break;

  case 204: // infixexp: exp10
#line 1022 "parser.y"
                                {yylhs.value.as < Hs::InfixExp > () = Hs::InfixExp({yystack_[0].value.as < expression_ref > ()});}
#line 3073 "parser.cc"
    break;

  case 205: // infixexp: infixexp qop exp10
#line 1023 "parser.y"
                                {yylhs.value.as < Hs::InfixExp > () = yystack_[2].value.as < Hs::InfixExp > (); yylhs.value.as < Hs::InfixExp > ().terms.push_back(yystack_[1].value.as < expression_ref > ()); yylhs.value.as < Hs::InfixExp > ().terms.push_back(yystack_[0].value.as < expression_ref > ());}
#line 3079 "parser.cc"
    break;

  case 206: // infixexp_top: exp10_top
#line 1025 "parser.y"
                                {yylhs.value.as < Hs::InfixExp > () = Hs::InfixExp({yystack_[0].value.as < expression_ref > ()});}
#line 3085 "parser.cc"
    break;

  case 207: // infixexp_top: infixexp_top qop exp10_top
#line 1026 "parser.y"
                                          {yylhs.value.as < Hs::InfixExp > () = yystack_[2].value.as < Hs::InfixExp > (); yylhs.value.as < Hs::InfixExp > ().terms.push_back(yystack_[1].value.as < expression_ref > ()); yylhs.value.as < Hs::InfixExp > ().terms.push_back(yystack_[0].value.as < expression_ref > ());}
#line 3091 "parser.cc"
    break;

  case 208: // exp10_top: "-" fexp
#line 1028 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_minus(yystack_[0].value.as < expression_ref > ());}
#line 3097 "parser.cc"
    break;

  case 209: // exp10_top: "{-# CORE" "STRING" "#-}"
#line 1029 "parser.y"
                                   {}
#line 3103 "parser.cc"
    break;

  case 210: // exp10_top: fexp
#line 1030 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3109 "parser.cc"
    break;

  case 211: // exp10: exp10_top
#line 1032 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3115 "parser.cc"
    break;

  case 212: // exp10: scc_annot exp
#line 1033 "parser.y"
                                 {}
#line 3121 "parser.cc"
    break;

  case 217: // fexp: fexp aexp
#line 1044 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_apply(yystack_[1].value.as < expression_ref > (), yystack_[0].value.as < expression_ref > ());}
#line 3127 "parser.cc"
    break;

  case 218: // fexp: fexp "TYPEAPP" atype
#line 1045 "parser.y"
                                 {}
#line 3133 "parser.cc"
    break;

  case 219: // fexp: "static" aexp
#line 1046 "parser.y"
                                 {}
#line 3139 "parser.cc"
    break;

  case 220: // fexp: aexp
#line 1047 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3145 "parser.cc"
    break;

  case 221: // aexp: qvar "@" aexp
#line 1049 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::AsPattern(Hs::Var({yystack_[2].location,yystack_[2].value.as < std::string > ()}),yystack_[0].value.as < expression_ref > ());}
#line 3151 "parser.cc"
    break;

  case 222: // aexp: PREFIX_TILDE aexp
#line 1050 "parser.y"
                                          {yylhs.value.as < expression_ref > () = Hs::LazyPattern(yystack_[0].value.as < expression_ref > ());}
#line 3157 "parser.cc"
    break;

  case 223: // aexp: "\\" apats1 "->" exp
#line 1051 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::LambdaExp(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3163 "parser.cc"
    break;

  case 224: // aexp: "let" binds "in" exp
#line 1052 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 3169 "parser.cc"
    break;

  case 225: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1054 "parser.y"
                                                       {yylhs.value.as < expression_ref > () = Hs::IfExp({yystack_[6].location,yystack_[6].value.as < expression_ref > ()},{yystack_[3].location,yystack_[3].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 3175 "parser.cc"
    break;

  case 226: // aexp: "case" exp "of" altslist
#line 1056 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::CaseExp(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < Hs::Alts > ());}
#line 3181 "parser.cc"
    break;

  case 227: // aexp: "do" stmtlist
#line 1057 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::Do(yystack_[0].value.as < Hs::Stmts > ());}
#line 3187 "parser.cc"
    break;

  case 228: // aexp: "mdo" stmtlist
#line 1058 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::MDo(yystack_[0].value.as < Hs::Stmts > ());}
#line 3193 "parser.cc"
    break;

  case 229: // aexp: aexp1
#line 1060 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3199 "parser.cc"
    break;

  case 230: // aexp1: aexp1 "{" fbinds "}"
#line 1062 "parser.y"
                              {}
#line 3205 "parser.cc"
    break;

  case 231: // aexp1: aexp2
#line 1063 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3211 "parser.cc"
    break;

  case 232: // aexp2: qvar
#line 1065 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3217 "parser.cc"
    break;

  case 233: // aexp2: qcon
#line 1066 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3223 "parser.cc"
    break;

  case 234: // aexp2: literal
#line 1067 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3229 "parser.cc"
    break;

  case 235: // aexp2: "(" texp ")"
#line 1068 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3235 "parser.cc"
    break;

  case 236: // aexp2: "(" tup_exprs ")"
#line 1069 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::Tuple(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3241 "parser.cc"
    break;

  case 237: // aexp2: "[" list "]"
#line 1074 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3247 "parser.cc"
    break;

  case 238: // aexp2: "_"
#line 1075 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::WildcardPattern();}
#line 3253 "parser.cc"
    break;

  case 239: // texp: exp
#line 1080 "parser.y"
                      {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3259 "parser.cc"
    break;

  case 240: // texp: infixexp qop
#line 1081 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::LeftSection ( yystack_[1].value.as < Hs::InfixExp > (), yystack_[0].value.as < expression_ref > () ); }
#line 3265 "parser.cc"
    break;

  case 241: // texp: qopm infixexp
#line 1082 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::RightSection( yystack_[1].value.as < expression_ref > (), yystack_[0].value.as < Hs::InfixExp > () ); }
#line 3271 "parser.cc"
    break;

  case 242: // tup_exprs: tup_exprs "," texp
#line 1087 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3277 "parser.cc"
    break;

  case 243: // tup_exprs: texp "," texp
#line 1088 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3283 "parser.cc"
    break;

  case 244: // list: texp
#line 1106 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List{{yystack_[0].value.as < expression_ref > ()}}; }
#line 3289 "parser.cc"
    break;

  case 245: // list: lexps
#line 1107 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List{yystack_[0].value.as < std::vector<expression_ref> > ()}; }
#line 3295 "parser.cc"
    break;

  case 246: // list: texp ".."
#line 1108 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < expression_ref > ()); }
#line 3301 "parser.cc"
    break;

  case 247: // list: texp "," exp ".."
#line 1109 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()); }
#line 3307 "parser.cc"
    break;

  case 248: // list: texp ".." exp
#line 1110 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()); }
#line 3313 "parser.cc"
    break;

  case 249: // list: texp "," exp ".." exp
#line 1111 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < expression_ref > (), yystack_[2].value.as < expression_ref > (), yystack_[0].value.as < expression_ref > ()); }
#line 3319 "parser.cc"
    break;

  case 250: // list: texp "|" squals
#line 1112 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < expression_ref > (), yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 3325 "parser.cc"
    break;

  case 251: // lexps: lexps "," texp
#line 1114 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3331 "parser.cc"
    break;

  case 252: // lexps: texp "," texp
#line 1115 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3337 "parser.cc"
    break;

  case 253: // squals: squals "," qual
#line 1128 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3343 "parser.cc"
    break;

  case 254: // squals: qual
#line 1130 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3349 "parser.cc"
    break;

  case 255: // guardquals: guardquals1
#line 1140 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3355 "parser.cc"
    break;

  case 256: // guardquals1: guardquals1 "," qual
#line 1142 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > ();yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3361 "parser.cc"
    break;

  case 257: // guardquals1: qual
#line 1143 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3367 "parser.cc"
    break;

  case 258: // altslist: "{" alts "}"
#line 1146 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 3373 "parser.cc"
    break;

  case 259: // altslist: "vocurly" alts close
#line 1147 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 3379 "parser.cc"
    break;

  case 260: // altslist: "{" "}"
#line 1148 "parser.y"
                                 {}
#line 3385 "parser.cc"
    break;

  case 261: // altslist: "vocurly" close
#line 1149 "parser.y"
                                 {}
#line 3391 "parser.cc"
    break;

  case 262: // alts: alts1
#line 1151 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 3397 "parser.cc"
    break;

  case 263: // alts: ";" alts
#line 1152 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 3403 "parser.cc"
    break;

  case 264: // alts1: alts1 ";" alt
#line 1154 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 3409 "parser.cc"
    break;

  case 265: // alts1: alts1 ";"
#line 1155 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 3415 "parser.cc"
    break;

  case 266: // alts1: alt
#line 1156 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 3421 "parser.cc"
    break;

  case 267: // alt: pat alt_rhs
#line 1158 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 3427 "parser.cc"
    break;

  case 268: // alt_rhs: "->" exp wherebinds
#line 1160 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3433 "parser.cc"
    break;

  case 269: // alt_rhs: gdpats wherebinds
#line 1161 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3439 "parser.cc"
    break;

  case 270: // gdpats: gdpats gdpat
#line 1163 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3445 "parser.cc"
    break;

  case 271: // gdpats: gdpat
#line 1164 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3451 "parser.cc"
    break;

  case 272: // gdpat: "|" guardquals "->" exp
#line 1173 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ()};}
#line 3457 "parser.cc"
    break;

  case 273: // pat: exp
#line 1175 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3463 "parser.cc"
    break;

  case 274: // pat: PREFIX_BANG aexp
#line 1176 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3469 "parser.cc"
    break;

  case 275: // bindpat: exp
#line 1178 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3475 "parser.cc"
    break;

  case 276: // bindpat: PREFIX_BANG aexp
#line 1179 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3481 "parser.cc"
    break;

  case 277: // apat: aexp
#line 1181 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3487 "parser.cc"
    break;

  case 278: // apat: PREFIX_BANG aexp
#line 1182 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3493 "parser.cc"
    break;

  case 279: // apats1: apats1 apat
#line 1184 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3499 "parser.cc"
    break;

  case 280: // apats1: apat
#line 1185 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3505 "parser.cc"
    break;

  case 281: // stmtlist: "{" stmts "}"
#line 1188 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 3511 "parser.cc"
    break;

  case 282: // stmtlist: "vocurly" stmts close
#line 1189 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 3517 "parser.cc"
    break;

  case 283: // stmts: stmts ";" stmt
#line 1191 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3523 "parser.cc"
    break;

  case 284: // stmts: stmts ";"
#line 1192 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3529 "parser.cc"
    break;

  case 285: // stmts: stmt
#line 1193 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3535 "parser.cc"
    break;

  case 286: // stmts: %empty
#line 1194 "parser.y"
                       {}
#line 3541 "parser.cc"
    break;

  case 287: // stmt: qual
#line 1199 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3547 "parser.cc"
    break;

  case 288: // stmt: "rec" stmtlist
#line 1200 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ());}
#line 3553 "parser.cc"
    break;

  case 289: // qual: bindpat "<-" exp
#line 1202 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::PatQual(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3559 "parser.cc"
    break;

  case 290: // qual: exp
#line 1203 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::SimpleQual(yystack_[0].value.as < expression_ref > ());}
#line 3565 "parser.cc"
    break;

  case 291: // qual: "let" binds
#line 1204 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ());}
#line 3571 "parser.cc"
    break;

  case 299: // qcon: gen_qcon
#line 1249 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3577 "parser.cc"
    break;

  case 300: // qcon: sysdcon
#line 1250 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3583 "parser.cc"
    break;

  case 301: // gen_qcon: qconid
#line 1252 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3589 "parser.cc"
    break;

  case 302: // gen_qcon: "(" qconsym ")"
#line 1253 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3595 "parser.cc"
    break;

  case 303: // con: conid
#line 1255 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3601 "parser.cc"
    break;

  case 304: // con: "(" consym ")"
#line 1256 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3607 "parser.cc"
    break;

  case 305: // con: sysdcon
#line 1257 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3613 "parser.cc"
    break;

  case 308: // sysdcon_no_list: "(" ")"
#line 1262 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 3619 "parser.cc"
    break;

  case 309: // sysdcon_no_list: "(" commas ")"
#line 1263 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3625 "parser.cc"
    break;

  case 310: // sysdcon_no_list: "(#" "#)"
#line 1264 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 3631 "parser.cc"
    break;

  case 311: // sysdcon_no_list: "(#" commas "#)"
#line 1265 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3637 "parser.cc"
    break;

  case 312: // sysdcon: sysdcon_no_list
#line 1267 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3643 "parser.cc"
    break;

  case 313: // sysdcon: "[" "]"
#line 1268 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 3649 "parser.cc"
    break;

  case 314: // conop: consym
#line 1270 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3655 "parser.cc"
    break;

  case 315: // conop: "`" conid "`"
#line 1271 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3661 "parser.cc"
    break;

  case 316: // qconop: qconsym
#line 1273 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3667 "parser.cc"
    break;

  case 317: // qconop: "`" qconid "`"
#line 1274 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3673 "parser.cc"
    break;

  case 318: // gtycon: ntgtycon
#line 1277 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3679 "parser.cc"
    break;

  case 319: // gtycon: "(" ")"
#line 1278 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 3685 "parser.cc"
    break;

  case 320: // gtycon: "(#" "#)"
#line 1279 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 3691 "parser.cc"
    break;

  case 321: // ntgtycon: oqtycon
#line 1281 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3697 "parser.cc"
    break;

  case 322: // ntgtycon: "(" commas ")"
#line 1282 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3703 "parser.cc"
    break;

  case 323: // ntgtycon: "(#" commas "#)"
#line 1283 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3709 "parser.cc"
    break;

  case 324: // ntgtycon: "(" "->" ")"
#line 1284 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 3715 "parser.cc"
    break;

  case 325: // ntgtycon: "[" "]"
#line 1285 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 3721 "parser.cc"
    break;

  case 326: // oqtycon: qtycon
#line 1287 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3727 "parser.cc"
    break;

  case 327: // oqtycon: "(" qtyconsym ")"
#line 1288 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3733 "parser.cc"
    break;

  case 328: // oqtycon: "(" "~" ")"
#line 1289 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3739 "parser.cc"
    break;

  case 329: // oqtycon_no_varcon: qtycon
#line 1291 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3745 "parser.cc"
    break;

  case 330: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1292 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3751 "parser.cc"
    break;

  case 331: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1293 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3757 "parser.cc"
    break;

  case 332: // oqtycon_no_varcon: "(" ":" ")"
#line 1294 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 3763 "parser.cc"
    break;

  case 333: // oqtycon_no_varcon: "(" "~" ")"
#line 1295 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3769 "parser.cc"
    break;

  case 334: // qtyconop: qtyconsym
#line 1298 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3775 "parser.cc"
    break;

  case 335: // qtyconop: "`" qtycon "`"
#line 1299 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3781 "parser.cc"
    break;

  case 336: // qtycondoc: qtycon
#line 1301 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3787 "parser.cc"
    break;

  case 337: // qtycon: "QCONID"
#line 1303 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3793 "parser.cc"
    break;

  case 338: // qtycon: tycon
#line 1304 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3799 "parser.cc"
    break;

  case 339: // tycon: "CONID"
#line 1308 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3805 "parser.cc"
    break;

  case 340: // qtyconsym: "QCONSYM"
#line 1310 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3811 "parser.cc"
    break;

  case 341: // qtyconsym: "QVARSYM"
#line 1311 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3817 "parser.cc"
    break;

  case 342: // qtyconsym: tyconsym
#line 1312 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3823 "parser.cc"
    break;

  case 343: // tyconsym: "CONSYM"
#line 1314 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3829 "parser.cc"
    break;

  case 344: // tyconsym: "VARSYM"
#line 1315 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3835 "parser.cc"
    break;

  case 345: // tyconsym: ":"
#line 1316 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 3841 "parser.cc"
    break;

  case 346: // tyconsym: "-"
#line 1317 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 3847 "parser.cc"
    break;

  case 347: // op: varop
#line 1322 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3853 "parser.cc"
    break;

  case 348: // op: conop
#line 1323 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3859 "parser.cc"
    break;

  case 349: // varop: varsym
#line 1325 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3865 "parser.cc"
    break;

  case 350: // varop: "`" varid "`"
#line 1326 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3871 "parser.cc"
    break;

  case 351: // qop: qvarop
#line 1328 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3877 "parser.cc"
    break;

  case 352: // qop: qconop
#line 1329 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3883 "parser.cc"
    break;

  case 353: // qopm: qvaropm
#line 1332 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3889 "parser.cc"
    break;

  case 354: // qopm: qconop
#line 1333 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3895 "parser.cc"
    break;

  case 355: // qvarop: qvarsym
#line 1338 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3901 "parser.cc"
    break;

  case 356: // qvarop: "`" qvarid "`"
#line 1339 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3907 "parser.cc"
    break;

  case 357: // qvaropm: qvarsym_no_minus
#line 1341 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 3913 "parser.cc"
    break;

  case 358: // qvaropm: "`" qvarid "`"
#line 1342 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3919 "parser.cc"
    break;

  case 359: // tyvar: tyvarid
#line 1346 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3925 "parser.cc"
    break;

  case 360: // tyvarop: "`" tyvarid "`"
#line 1348 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3931 "parser.cc"
    break;

  case 361: // tyvarid: "VARID"
#line 1350 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3937 "parser.cc"
    break;

  case 362: // tyvarid: special_id
#line 1351 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3943 "parser.cc"
    break;

  case 363: // tyvarid: "unsafe"
#line 1352 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 3949 "parser.cc"
    break;

  case 364: // tyvarid: "safe"
#line 1353 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 3955 "parser.cc"
    break;

  case 365: // tyvarid: "interruptible"
#line 1354 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 3961 "parser.cc"
    break;

  case 366: // var: varid
#line 1357 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3967 "parser.cc"
    break;

  case 367: // var: "(" varsym ")"
#line 1358 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3973 "parser.cc"
    break;

  case 368: // qvar: qvarid
#line 1360 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3979 "parser.cc"
    break;

  case 369: // qvar: "(" varsym ")"
#line 1361 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3985 "parser.cc"
    break;

  case 370: // qvar: "(" qvarsym1 ")"
#line 1362 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3991 "parser.cc"
    break;

  case 371: // qvarid: varid
#line 1364 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3997 "parser.cc"
    break;

  case 372: // qvarid: "QVARID"
#line 1365 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4003 "parser.cc"
    break;

  case 373: // varid: "VARID"
#line 1367 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4009 "parser.cc"
    break;

  case 374: // varid: special_id
#line 1368 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4015 "parser.cc"
    break;

  case 375: // varid: "unsafe"
#line 1369 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 4021 "parser.cc"
    break;

  case 376: // varid: "safe"
#line 1370 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 4027 "parser.cc"
    break;

  case 377: // varid: "interruptible"
#line 1371 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 4033 "parser.cc"
    break;

  case 378: // varid: "forall"
#line 1372 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 4039 "parser.cc"
    break;

  case 379: // varid: "family"
#line 1373 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 4045 "parser.cc"
    break;

  case 380: // varid: "role"
#line 1374 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 4051 "parser.cc"
    break;

  case 381: // qvarsym: varsym
#line 1376 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4057 "parser.cc"
    break;

  case 382: // qvarsym: qvarsym1
#line 1377 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4063 "parser.cc"
    break;

  case 383: // qvarsym_no_minus: varsym_no_minus
#line 1379 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4069 "parser.cc"
    break;

  case 384: // qvarsym_no_minus: qvarsym1
#line 1380 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4075 "parser.cc"
    break;

  case 385: // qvarsym1: "QVARSYM"
#line 1382 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4081 "parser.cc"
    break;

  case 386: // varsym: varsym_no_minus
#line 1384 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4087 "parser.cc"
    break;

  case 387: // varsym: "-"
#line 1385 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 4093 "parser.cc"
    break;

  case 388: // varsym_no_minus: "VARSYM"
#line 1387 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4099 "parser.cc"
    break;

  case 389: // varsym_no_minus: special_sym
#line 1388 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4105 "parser.cc"
    break;

  case 390: // special_id: "as"
#line 1390 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 4111 "parser.cc"
    break;

  case 391: // special_id: "qualified"
#line 1391 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 4117 "parser.cc"
    break;

  case 392: // special_id: "hiding"
#line 1392 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 4123 "parser.cc"
    break;

  case 393: // special_id: "export"
#line 1393 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 4129 "parser.cc"
    break;

  case 394: // special_id: "label"
#line 1394 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 4135 "parser.cc"
    break;

  case 395: // special_id: "dynamic"
#line 1395 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 4141 "parser.cc"
    break;

  case 396: // special_id: "stdcall"
#line 1396 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 4147 "parser.cc"
    break;

  case 397: // special_id: "ccall"
#line 1397 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 4153 "parser.cc"
    break;

  case 398: // special_id: "capi"
#line 1398 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 4159 "parser.cc"
    break;

  case 399: // special_id: "prim"
#line 1399 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 4165 "parser.cc"
    break;

  case 400: // special_id: "javascript"
#line 1400 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 4171 "parser.cc"
    break;

  case 401: // special_id: "group"
#line 1401 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 4177 "parser.cc"
    break;

  case 402: // special_id: "stock"
#line 1402 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 4183 "parser.cc"
    break;

  case 403: // special_id: "anyclass"
#line 1403 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 4189 "parser.cc"
    break;

  case 404: // special_id: "via"
#line 1404 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 4195 "parser.cc"
    break;

  case 405: // special_id: "unit"
#line 1405 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 4201 "parser.cc"
    break;

  case 406: // special_id: "dependency"
#line 1406 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 4207 "parser.cc"
    break;

  case 407: // special_id: "signature"
#line 1407 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 4213 "parser.cc"
    break;

  case 408: // special_sym: "!"
#line 1409 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 4219 "parser.cc"
    break;

  case 409: // special_sym: "."
#line 1410 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 4225 "parser.cc"
    break;

  case 410: // special_sym: "*"
#line 1411 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 4231 "parser.cc"
    break;

  case 411: // qconid: conid
#line 1415 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4237 "parser.cc"
    break;

  case 412: // qconid: "QCONID"
#line 1416 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4243 "parser.cc"
    break;

  case 413: // conid: "CONID"
#line 1418 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4249 "parser.cc"
    break;

  case 414: // qconsym: consym
#line 1420 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4255 "parser.cc"
    break;

  case 415: // qconsym: "QCONSYM"
#line 1421 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4261 "parser.cc"
    break;

  case 416: // consym: "CONSYM"
#line 1423 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4267 "parser.cc"
    break;

  case 417: // consym: ":"
#line 1424 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4273 "parser.cc"
    break;

  case 418: // literal: "CHAR"
#line 1428 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 4279 "parser.cc"
    break;

  case 419: // literal: "STRING"
#line 1429 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 4285 "parser.cc"
    break;

  case 420: // literal: "INTEGER"
#line 1430 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < int > ()});}
#line 4291 "parser.cc"
    break;

  case 421: // literal: "RATIONAL"
#line 1431 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Double{yystack_[0].value.as < double > ()});}
#line 4297 "parser.cc"
    break;

  case 422: // literal: "PRIMINTEGER"
#line 1432 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < int > ()});}
#line 4303 "parser.cc"
    break;

  case 424: // close: error
#line 1440 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 4309 "parser.cc"
    break;

  case 425: // modid: "CONID"
#line 1444 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4315 "parser.cc"
    break;

  case 426: // modid: "QCONID"
#line 1445 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4321 "parser.cc"
    break;

  case 427: // commas: commas ","
#line 1447 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 4327 "parser.cc"
    break;

  case 428: // commas: ","
#line 1448 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 4333 "parser.cc"
    break;


#line 4337 "parser.cc"

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


  const short parser::yypact_ninf_ = -557;

  const short parser::yytable_ninf_ = -387;

  const short
  parser::yypact_[] =
  {
      28,   -35,  -557,    99,  -557,  -557,  -557,  -557,  -557,   230,
     -24,    76,  -557,    49,    60,    60,   -23,  -557,  -557,  -557,
    -557,   192,  -557,  -557,  -557,   100,  -557,   143,   173,  3780,
     228,   240,   177,  -557,   589,  -557,    53,  -557,  -557,  -557,
    -557,   -35,  -557,  -557,  -557,  -557,  -557,  -557,  -557,  -557,
    -557,  -557,  -557,  -557,  -557,  -557,  -557,  -557,  -557,  -557,
    -557,  -557,  -557,  -557,   380,  -557,  -557,  -557,  -557,   171,
     196,  -557,   212,  -557,  -557,  -557,  -557,  -557,  -557,  -557,
     158,  -557,   -35,  -557,   195,  -557,  2052,  3371,  -557,   213,
     159,  2052,  -557,  -557,  -557,   105,   166,  -557,  3371,   310,
     159,  2850,   232,   208,  4050,   129,  2451,  2850,  2584,  2850,
    1121,   988,   107,  -557,  -557,  -557,  -557,  -557,  -557,  -557,
      40,   232,   211,   177,  -557,  -557,  -557,   277,   -16,  -557,
    -557,   172,  -557,  2717,  -557,   254,  -557,  -557,  -557,  -557,
    -557,  -557,   269,    51,  -557,  -557,  -557,  -557,   227,  -557,
     253,   258,  -557,  -557,  -557,  -557,  -557,   261,  -557,   262,
     263,   264,  -557,  -557,  -557,  3780,  3817,  -557,  -557,  -557,
    -557,   360,  -557,   -49,   988,   352,   440,  -557,  -557,  2052,
    -557,  -557,  -557,  -557,  -557,  -557,  -557,  -557,  4146,  3062,
    2959,   260,  3976,  -557,  -557,  -557,  -557,  -557,   351,  3678,
    -557,   289,  -557,   168,  -557,  3678,  -557,  -557,  -557,  -557,
    -557,  -557,  -557,  -557,  -557,  -557,  -557,  -557,  3165,  1520,
    1520,  -557,   266,   305,   308,   309,   311,  3165,   722,   722,
    -557,   376,   313,   132,   354,  -557,  -557,   -17,  4050,  -557,
     320,   268,   -21,   293,    46,   284,   322,  -557,  -557,  2850,
    -557,  -557,  2318,  -557,  2717,   148,  -557,  -557,  3915,  -557,
    -557,  -557,   440,    66,   299,   290,  -557,  2052,  -557,  -557,
    -557,  -557,  -557,  -557,  2584,  -557,  -557,    82,    83,   263,
     298,   300,   301,    86,  -557,   123,  3165,  4050,  4050,  -557,
     701,   195,   279,  3371,  3165,  4146,  2052,  1786,  3915,  -557,
      34,  -557,  -557,  2185,  -557,  -557,  -557,  -557,  3678,  -557,
    4013,  2850,  -557,  -557,  -557,  -557,  -557,  -557,  -557,  -557,
     302,   306,   303,  -557,   316,    49,   -35,    39,   338,   343,
     183,  3165,  2052,  -557,    90,    78,   319,   307,  -557,  -557,
    -557,  -557,   318,   332,  -557,   317,   321,  -557,   324,   314,
     328,    93,   125,   312,   315,   166,  -557,  -557,  3371,  3165,
    3371,  -557,  -557,  -557,   329,   325,   166,   159,  2850,   341,
     355,   -19,  -557,  -557,    42,  -557,   417,  -557,  -557,  -557,
    -557,  -557,  -557,   351,    71,  -557,  -557,   172,    45,  2052,
    3165,   323,   326,   344,   367,  -557,  -557,   372,   342,   129,
     -32,   378,  -557,  2052,  -557,  -557,   339,   347,  2052,  2052,
    1786,  1254,  -557,  1254,   570,  -557,  1254,  -557,  1254,    81,
    -557,  -557,  -557,  -557,   381,   385,   387,  4109,   369,  -557,
    -557,  -557,  -557,  -557,   -14,   222,  -557,  -557,  -557,  -557,
     351,   389,   370,  -557,   371,  -557,  -557,  -557,  -557,  -557,
     366,  -557,   374,   406,  -557,  -557,  -557,  3878,  -557,  -557,
    -557,   383,  3780,  -557,  -557,  -557,  -557,  1387,   855,  -557,
    -557,  -557,   384,  3165,  -557,  4146,  4183,  -557,  3165,  -557,
    -557,  -557,  3165,  -557,  -557,  -557,  -557,  -557,  -557,  -557,
    -557,   404,  -557,  3165,   376,  -557,  -557,  2052,  -557,  1520,
    -557,  2052,  -557,  -557,   722,  -557,  -557,  -557,  4146,   390,
    -557,  -557,  -557,  -557,  -557,   391,   244,   130,  -557,  -557,
    -557,  -557,  -557,  -557,  -557,   382,  -557,   421,  -557,  -557,
    -557,  -557,  -557,  3165,  3165,   386,   392,   701,  -557,   427,
    3165,   479,  -557,   501,  -557,  2052,  1786,  -557,  -557,  4013,
    1254,  -557,  3780,   402,  2850,  -557,  1653,  -557,   411,   399,
    -557,   201,    49,  -557,  -557,  -557,  -557,  3165,  4277,  -557,
    -557,  -557,   405,  -557,  -557,  -557,   266,  -557,   436,  -557,
    -557,   328,  -557,  1786,  2052,  -557,    -9,    56,  -557,  -557,
    -557,  -557,  -557,  -557,  -557,   433,  -557,  3678,    15,  -557,
     501,  -557,  -557,  -557,  -557,  -557,   408,  -557,  -557,  -557,
    -557,  1919,  1786,  2052,  -557,    38,  -557,  -557,  -557,   441,
    -557,   511,  3165,  -557,  -557,  -557,  3165,  -557,  4244,   479,
     434,  3474,  -557,  -557,  -557,  -557,  -557,  -557,  3268,   -40,
     472,  -557,  -557,  -557,  -557,  -557,   442,   351,  -557,  -557,
    3165,  2052,  -557,  -557,  -557,  -557,  3678,   410,  -557,  3678,
    -557,  -557,   416,   425,  -557,  3371,  -557,  2052,  -557,   437,
    -557,  3576,  -557,  3678,  3371,  -557,  -557,  -557,  -557,  -557
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    36,     0,     2,    36,     4,   425,   426,     8,
       0,    39,     1,     0,     0,     0,    18,    11,    35,    13,
      16,    58,   424,   423,    12,   102,    98,     0,     0,     0,
       0,    42,    37,    15,    14,   101,     0,     6,     7,   390,
     392,     0,   391,   378,   393,   394,   395,   376,   377,   375,
     379,   380,   396,   397,   398,   399,   400,   401,   402,   403,
     404,   405,   407,   406,     0,   373,   339,   372,   337,     0,
      19,    21,    24,    32,   329,   338,    31,   368,   371,   374,
       0,    41,     0,    34,    38,   238,     0,     0,    80,     0,
       0,     0,    51,    52,    53,    75,     0,    81,     0,     0,
       0,     0,   198,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   413,   412,   418,   419,   420,   421,   422,
     198,   198,    49,    56,    59,    60,    61,    88,     0,    64,
     180,    65,   206,   210,   220,   229,   231,   233,   299,   312,
     300,   108,   232,   371,   301,   411,   234,    99,     0,    23,
       0,     0,   387,   408,   410,   409,   388,     0,   385,     0,
       0,     0,   386,   389,    17,     0,    27,    22,    36,    36,
       3,    44,    33,     0,     0,     0,   203,   211,   204,     0,
     364,   365,   363,   345,   113,   346,   112,   137,   165,     0,
       0,     0,     0,   361,   344,   343,   341,   340,    97,     0,
     111,     0,    85,   120,   123,   124,   129,   128,   135,   321,
     132,   326,   334,   342,   136,   133,   359,   362,   148,   286,
     286,   227,   214,     0,     0,     0,     0,     0,    92,    92,
      95,     0,     0,   120,     0,   228,   219,     0,     0,   199,
       0,     0,     0,     0,     0,   306,   103,   305,   303,     0,
     277,   280,     0,   222,   208,     0,   417,   313,     0,   416,
     415,   239,   203,   244,     0,   245,   354,     0,   353,   357,
     384,   383,   316,   414,   387,   308,   428,     0,     0,   384,
       0,   383,   316,     0,   310,     0,     0,     0,     0,    50,
       0,    57,     0,     0,     0,     0,     0,     0,     0,   182,
      97,   187,   352,     0,   351,   355,   382,   381,     0,   217,
     293,     0,   100,   332,   333,   331,   330,   370,   369,    20,
       0,     0,    28,    30,     0,     0,     0,    46,     0,     0,
       0,     0,     0,   212,     0,     0,     0,   166,   168,   366,
     152,   325,     0,     0,   116,     0,     0,   140,   149,     0,
     334,     0,     0,     0,     0,     0,    66,   138,     0,     0,
       0,   130,   131,   149,     0,   147,     0,     0,     0,   290,
       0,     0,   285,   287,     0,   213,     0,    72,    71,    73,
      74,   144,   105,    97,     0,   183,    91,     0,     0,     0,
       0,     0,     0,     0,     0,   209,   193,     0,     0,     0,
       0,     0,   278,     0,   279,   181,     0,     0,   240,   246,
       0,     0,   237,     0,   241,   235,     0,   236,     0,   369,
     302,   309,   427,   311,     0,     0,     0,     0,   190,   348,
      55,   347,   349,   314,     0,    82,   189,   117,   106,   107,
      97,     0,   255,   257,     0,   185,   186,   207,   218,   296,
       0,   292,   295,   298,   221,    26,    25,     0,     9,    10,
      43,     0,     0,    40,    45,   216,   215,     0,     0,   226,
     202,   205,     0,     0,   139,     0,     0,   142,     0,   324,
     328,   143,     0,   327,   322,   323,   335,   360,    96,    84,
     121,   125,    62,     0,   291,   288,   276,     0,   281,   284,
     282,     0,    70,    93,    90,    94,   224,    67,     0,     0,
     200,   192,   194,   304,   307,     0,     0,     0,   104,   318,
     191,   223,   358,   317,   248,   250,   254,   239,   252,   251,
     243,   242,   197,     0,     0,     0,     0,     0,    87,     0,
       0,   162,    69,   170,   184,     0,     0,   356,   230,     0,
       0,    29,     0,     0,     0,   260,     0,   273,     0,   262,
     266,     0,     0,   261,   367,   169,   167,     0,     0,   151,
     153,   115,   150,   150,   289,   283,   214,    89,     0,   201,
     319,     0,   320,     0,   247,   109,     0,     0,   350,   315,
      54,    86,   155,    83,   152,   156,   158,     0,     0,    68,
     171,   173,   188,   256,   294,   297,     0,    47,   274,   263,
     258,   265,     0,     0,   267,    97,   271,   259,   114,     0,
     141,     0,     0,   253,   249,   195,     0,   196,     0,   162,
       0,   163,   126,   134,   160,    78,    76,    77,     0,     0,
     174,   177,   336,   172,    48,   264,     0,    97,   269,   270,
       0,     0,    63,   110,   161,   157,     0,     0,   127,     0,
     178,   122,   145,     0,   175,     0,   176,     0,   268,     0,
     225,   163,   159,   164,     0,   179,    79,   272,   154,   146
  };

  const short
  parser::yypgoto_[] =
  {
    -557,  -557,  -557,  -557,  -557,  -557,  -557,    36,  -557,  -557,
    -409,  -557,   361,  -557,  -557,  -557,  -141,   414,  -557,  -557,
    -557,  -557,  -557,  -557,  -557,  -557,  -557,  -557,  -557,  -557,
    -557,  -557,  -557,  -557,  -557,  -557,  -557,  -557,  -557,  -557,
     250,  -557,   327,  -557,  -284,  -289,   529,  -557,  -557,  -309,
     -71,  -174,    12,  -557,  -557,  -142,   160,   -67,  -557,   -83,
    -557,   -90,  -557,  -412,  -557,  -557,  -557,  -556,  -196,   267,
    -125,  -557,   336,   -31,  -557,   -91,  -557,  -557,   -69,  -557,
     -88,  -557,  -557,    96,  -557,  -557,   -28,   -65,   542,    73,
     331,  -557,   278,  -557,   194,  -557,   -84,   -72,   546,   -15,
    -274,     5,  -557,   -73,   -78,  -557,  -557,  -105,  -557,  -557,
    -557,  -557,   -22,  -557,  -557,  -416,  -557,    -7,  -557,  -557,
     -26,  -557,  -557,   346,  -557,   -76,   394,   101,  -263,  -557,
      54,  -557,  -557,  -557,  -557,   217,  -557,   -92,  -544,   -34,
    -557,   234,  -557,  -557,  -557,  -557,   -29,  -557,  -173,  -557,
      75,  -557,  -136,  -557,  -557,  -557,  -440,  -557,   438,  -265,
     -11,  -160,   -13,  -557,  -557,   -10,   -54,   -66,   -86,  -557,
    -128,   -99,   -51,  -228,  -557,  -283,    -8,   -80
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   170,     6,    10,    19,    30,
      69,    70,    71,   167,   321,   322,    72,    84,    11,    20,
      21,    32,    82,   327,   463,   464,   290,   122,   428,    33,
      34,   123,   124,   125,   126,   227,   639,   666,   127,   542,
     198,   293,   384,   230,   231,   356,    27,    36,   401,   381,
     436,   128,   586,   199,   200,   382,   438,   343,   630,   344,
     662,   203,   204,   631,   205,   361,   206,   632,   207,   383,
     663,   364,   349,   476,   569,   593,   543,   595,   596,   597,
     634,   336,   337,   338,   599,   600,   601,   640,   385,   386,
     299,   300,   301,   130,   238,   239,   369,   176,   387,   177,
     178,   376,   179,   133,   134,   135,   136,   277,   278,   264,
     265,   525,   441,   442,   469,   558,   559,   560,   614,   615,
     616,   561,   370,   251,   252,   221,   371,   372,   373,   450,
     451,   452,   137,   138,   245,   246,   139,   140,   429,   266,
     518,   208,   209,    73,   210,   641,   211,    75,   212,   213,
     430,   431,   303,   267,   304,   268,   214,   215,   216,   141,
     142,    77,    78,   305,   269,   270,   307,   162,    79,   163,
     144,   145,   272,   273,   146,    24,     9,   283
  };

  const short
  parser::yytable_[] =
  {
      74,   217,   175,   357,   202,   263,   248,   222,   233,   362,
     161,   445,   217,   247,   335,   232,   398,   350,    76,   132,
     201,   143,   470,   236,   235,   323,   261,   261,   250,   253,
     439,   255,   285,   149,   443,   254,   570,   635,   262,   262,
     332,    13,   459,    22,   271,   281,    22,   342,   348,     1,
      22,   461,   562,   553,   160,   309,   396,   280,   471,   286,
     282,   355,   433,   538,   294,   355,   636,   637,   625,   638,
     328,   488,   392,   515,   171,   658,   363,   516,    17,   517,
      66,   329,   494,   498,    68,     7,    29,   659,    66,     8,
     261,   500,    68,   242,   502,   333,   499,   302,   406,    12,
     295,   279,   262,   217,   217,   505,   217,   626,   281,   397,
     351,   352,   448,   217,   393,   658,   539,   658,   297,   217,
     161,   306,   612,   282,   638,   256,   408,   659,   619,     2,
     407,  -366,   217,   627,   471,    66,    74,    74,   444,    68,
     609,   217,   302,   606,   409,   237,    23,   526,   462,    23,
     410,   544,   437,    23,    76,    76,   275,   499,   473,   147,
     504,  -367,   276,   353,   279,    25,   306,  -366,   259,   148,
     407,   402,   626,   503,   250,   339,   309,   223,   224,   225,
     226,   152,   411,   153,   154,   563,   504,   161,   570,   155,
      26,    18,   415,   417,   295,   414,   421,  -367,   416,   418,
     217,   254,   422,   484,   324,   325,    31,   217,   217,   422,
     202,   156,   440,   132,   132,   143,   143,   490,   359,   284,
      37,   360,   217,   276,   585,   585,   201,   394,   302,   296,
      35,   160,   297,   454,   243,   423,   432,   485,   244,   422,
     112,   422,   582,   578,   671,   217,   276,   673,   437,   113,
      38,   256,   306,   296,   359,    80,   297,   360,  -118,   168,
     219,   169,   220,   152,    81,   153,   154,   228,   233,   229,
     491,   155,   217,   217,   217,   489,   425,   426,   332,   617,
     472,   164,   339,   603,   467,   612,   468,   613,   447,   298,
     496,   495,    83,   156,   259,    14,    15,   158,   260,   453,
     248,   335,   540,   541,   217,   506,   528,   247,   529,   433,
     172,   530,   165,   531,   287,   288,   551,   653,   460,   521,
     623,   166,   218,   183,   234,   524,   648,   527,   536,   261,
     345,   565,   261,   346,   261,   185,   571,   237,   240,   262,
     572,   262,   289,   581,   262,   271,   262,   271,   292,   443,
     271,   573,   271,   302,   580,   310,   311,   312,   668,   152,
     276,   153,   154,   313,   326,   194,   195,   155,   314,   196,
     197,   315,   316,   317,   318,   330,   276,   306,   355,   358,
     302,   375,   377,   557,   557,   378,   379,   217,   380,   156,
     217,   389,   217,   158,   390,   391,   217,   395,   592,   257,
     399,   633,   400,   433,   306,   412,   413,   217,   419,   434,
    -386,   420,   455,   574,   535,   465,   456,   576,   458,   457,
     466,   474,   478,   475,   477,   618,  -275,   479,    74,   486,
     482,   480,   487,    74,   481,   633,   351,   352,   483,   492,
     497,   493,   501,   433,   511,   605,    76,   217,   217,   512,
     510,    76,   513,   508,   217,   520,   522,   509,   532,   150,
     633,   602,   339,   633,   523,   533,   261,   534,   548,   151,
     545,   152,   557,   153,   154,   633,   608,   633,   262,   155,
     437,   217,   217,   432,   271,   537,   546,   550,   547,   132,
     549,   143,   552,   360,   564,   339,   579,   341,   583,   584,
     624,   156,   157,   588,   591,   158,   159,   594,   592,   589,
     598,   217,   607,   610,   611,   620,   622,   629,   644,   256,
     331,   650,   651,    74,   656,   665,   319,   557,   667,   647,
     113,   152,   674,   153,   154,   675,   217,   291,   453,   155,
     217,    76,   217,   435,    28,   217,   587,   678,   233,   679,
     507,   652,   217,   424,   365,   661,   388,   298,   536,   669,
     655,   156,   259,   628,   217,   158,   260,   670,   672,   642,
     217,   566,   643,   217,   664,   233,   129,   577,   446,   217,
     131,   621,   676,   677,   233,   217,   405,   217,   217,   649,
     646,   661,    85,    39,    86,    87,    88,    89,   404,    90,
     575,    40,    91,   604,   645,    92,    93,    94,    95,    96,
     642,    97,   590,    42,   374,    98,   514,    43,    99,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
     354,    54,    55,    56,   519,     0,    57,     0,     0,   101,
      58,    59,    60,    61,    62,    63,   102,     0,     0,   256,
       0,   103,   104,     0,     0,     0,     0,     0,     0,     0,
       0,   152,     0,   153,   154,   105,     0,     0,     0,   155,
       0,   106,     0,     0,     0,     0,     0,   107,     0,     0,
     108,   109,     0,     0,     0,     0,     0,   298,     0,     0,
       0,   156,   259,     0,   110,   158,   260,     0,   111,     0,
     112,     0,     0,     0,     0,     0,     0,     0,    65,   113,
       0,     0,    67,   114,     0,     0,     0,     0,   115,   116,
     117,   118,     0,     0,   119,    85,    39,    86,   120,   121,
       0,     0,    90,     0,    40,    91,     0,     0,    92,    93,
      94,     0,    96,     0,     0,     0,    42,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,   101,    58,    59,    60,    61,    62,    63,   102,
     256,     0,     0,     0,   103,   104,     0,     0,     0,     0,
       0,     0,   152,     0,   153,   154,     0,     0,   105,     0,
     155,     0,     0,     0,   106,     0,     0,     0,     0,     0,
     107,     0,     0,   108,   109,     0,     0,     0,   427,     0,
       0,     0,   156,   259,     0,     0,     0,   110,     0,     0,
       0,   111,     0,   112,     0,     0,     0,     0,     0,     0,
       0,    65,   113,     0,     0,    67,   114,     0,     0,     0,
       0,   115,   116,   117,   118,     0,    22,   119,    85,    39,
      86,   120,   121,     0,     0,    90,     0,    40,    91,     0,
       0,     0,     0,     0,     0,    96,     0,     0,     0,    42,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,   101,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,   103,   173,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,   107,     0,     0,   108,   554,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    23,
     110,     0,     0,     0,   174,     0,   112,     0,     0,     0,
     556,     0,     0,     0,    65,   113,     0,     0,    67,   114,
       0,     0,     0,     0,   115,   116,   117,   118,     0,     0,
     119,    85,    39,    86,     0,     0,     0,     0,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,    42,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,   101,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
     103,   173,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   256,     0,     0,
     106,     0,     0,     0,     0,     0,   107,     0,     0,   274,
       0,   153,   154,     0,     0,     0,     0,   155,     0,     0,
       0,     0,     0,   110,     0,     0,     0,   174,   275,   112,
       0,     0,     0,     0,   276,   258,     0,    65,   113,   156,
     259,    67,   114,   158,   260,     0,     0,   115,   116,   117,
     118,     0,     0,   119,    85,    39,    86,     0,     0,     0,
       0,    90,     0,    40,    91,     0,     0,     0,     0,     0,
       0,    96,     0,     0,     0,    42,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,   101,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,   103,   173,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     256,     0,     0,   106,     0,     0,     0,     0,     0,   107,
       0,     0,   108,     0,   153,   154,     0,     0,     0,     0,
     155,     0,     0,     0,     0,     0,   110,   257,     0,     0,
     174,     0,   112,     0,     0,     0,     0,     0,   258,     0,
      65,   113,   156,   259,    67,   114,   158,   260,     0,     0,
     115,   116,   117,   118,     0,     0,   119,    85,    39,    86,
       0,     0,     0,     0,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,   103,   173,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   256,     0,     0,   106,     0,     0,     0,
       0,     0,   107,     0,     0,   108,     0,   153,   154,     0,
       0,     0,     0,   155,     0,     0,     0,     0,     0,   110,
       0,     0,     0,   174,     0,   112,     0,     0,     0,     0,
       0,   258,     0,    65,   113,   156,   259,    67,   114,   158,
     260,     0,     0,   115,   116,   117,   118,     0,     0,   119,
      85,    39,    86,     0,     0,     0,     0,    90,     0,    40,
      91,     0,     0,     0,     0,     0,     0,    96,     0,     0,
       0,    42,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,   101,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,   103,
     173,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,   107,     0,     0,   108,   554,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   555,
       0,     0,   110,     0,     0,     0,   174,     0,   112,     0,
       0,     0,   556,     0,     0,     0,    65,   113,     0,     0,
      67,   114,     0,     0,     0,     0,   115,   116,   117,   118,
       0,     0,   119,    85,    39,    86,     0,     0,     0,     0,
      90,     0,    40,    91,     0,     0,     0,     0,     0,     0,
     366,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,     0,    54,    55,    56,     0,   367,    57,     0,     0,
     101,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,   103,   173,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,   107,     0,
       0,   108,   368,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,   174,
       0,   112,     0,     0,     0,     0,     0,     0,     0,    65,
     113,     0,     0,    67,   114,     0,     0,     0,     0,   115,
     116,   117,   118,     0,     0,   119,    85,    39,    86,     0,
       0,     0,     0,    90,     0,    40,    91,     0,     0,     0,
       0,     0,     0,    96,     0,     0,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   100,
      50,    51,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,   101,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,   103,   173,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,   107,     0,     0,   108,   554,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   110,     0,
       0,     0,   174,     0,   112,     0,     0,     0,   556,     0,
       0,     0,    65,   113,     0,     0,    67,   114,     0,     0,
       0,     0,   115,   116,   117,   118,     0,     0,   119,    85,
      39,    86,     0,     0,     0,     0,    90,     0,    40,    91,
       0,     0,     0,     0,     0,     0,   366,     0,     0,     0,
      42,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,   101,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,   103,   173,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,   107,     0,     0,   108,   368,     0,
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
       0,   106,     0,     0,     0,     0,     0,   107,     0,     0,
     108,   554,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   110,     0,     0,     0,   174,     0,
     112,     0,     0,     0,     0,     0,     0,     0,    65,   113,
       0,     0,    67,   114,     0,     0,     0,     0,   115,   116,
     117,   118,     0,     0,   119,    85,    39,    86,     0,     0,
       0,     0,    90,     0,    40,    91,     0,     0,     0,     0,
       0,     0,    96,     0,     0,     0,    42,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,   101,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,   103,   173,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
     107,     0,     0,   108,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   110,     0,     0,
       0,   174,     0,   112,     0,     0,     0,     0,     0,     0,
       0,    65,   113,     0,     0,    67,   114,     0,     0,     0,
       0,   115,   116,   117,   118,     0,     0,   119,    85,    39,
      86,     0,     0,     0,     0,    90,     0,    40,    91,     0,
       0,     0,     0,     0,     0,    96,     0,     0,     0,    42,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,   101,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,   103,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,   107,     0,     0,   108,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,   174,     0,   112,     0,     0,     0,
       0,     0,     0,     0,    65,   113,     0,     0,    67,   114,
       0,     0,     0,     0,   115,   116,   117,   118,     0,     0,
     119,    85,    39,    86,     0,     0,     0,     0,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,    42,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,   403,     0,   107,     0,     0,     0,
     249,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,     0,   174,     0,   112,
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
       0,     0,     0,   106,     0,     0,     0,     0,     0,   107,
       0,     0,     0,   249,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
     174,     0,   112,     0,     0,     0,     0,     0,     0,     0,
      65,   113,     0,     0,    67,   114,     0,     0,     0,     0,
     115,   116,   117,   118,     0,     0,   119,    85,    39,    86,
       0,     0,     0,     0,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,   174,     0,   112,     0,     0,     0,     0,
       0,     0,     0,    65,   113,     0,     0,    67,   114,     0,
       0,     0,     0,   115,   116,   117,   118,     0,     0,   119,
      85,    39,    86,     0,     0,     0,     0,    90,     0,    40,
      91,     0,     0,     0,     0,     0,     0,    96,     0,     0,
       0,    42,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   308,     0,     0,
       0,     0,   110,     0,     0,     0,   174,     0,   112,     0,
       0,     0,     0,     0,     0,     0,    65,   113,     0,     0,
      67,   114,     0,     0,     0,     0,   115,   116,   117,   118,
       0,     0,   119,    85,    39,    86,     0,     0,     0,     0,
      90,     0,    40,    91,     0,     0,     0,     0,     0,     0,
      96,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,   174,
       0,   112,     0,    39,     0,     0,     0,     0,     0,    65,
     113,    40,     0,    67,   114,     0,     0,     0,     0,   115,
     116,   117,   118,    42,     0,   119,     0,   340,     0,    44,
      45,    46,   180,   181,   182,     0,     0,     0,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   183,     0,
       0,     0,     0,     0,     0,   345,     0,   184,   346,     0,
     185,   186,     0,   187,     0,     0,     0,     0,     0,     0,
     188,     0,     0,     0,   189,     0,    39,     0,   190,   347,
     191,     0,     0,     0,    40,   276,   192,     0,   193,    66,
     194,   195,     0,    68,   196,   197,    42,     0,     0,     0,
     340,     0,    44,    45,    46,   180,   181,   182,     0,     0,
       0,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   183,     0,     0,     0,     0,     0,     0,     0,     0,
     184,     0,     0,   185,   186,     0,   187,     0,     0,     0,
       0,     0,     0,   188,     0,     0,     0,   189,   341,    39,
       0,   190,     0,   191,     0,     0,     0,    40,     0,   192,
       0,   193,    66,   194,   195,     0,    68,   196,   197,    42,
       0,     0,     0,   340,     0,    44,    45,    46,   180,   181,
     182,     0,     0,     0,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   183,     0,     0,     0,     0,     0,
       0,     0,     0,   184,     0,     0,   185,   186,     0,   187,
       0,     0,     0,     0,     0,     0,   188,     0,     0,     0,
     189,     0,    39,     0,   190,     0,   191,     0,     0,     0,
      40,     0,   192,     0,   193,    66,   194,   195,     0,    68,
     196,   197,    42,     0,     0,     0,     0,     0,    44,    45,
      46,   180,   181,   182,     0,     0,     0,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   183,     0,     0,
       0,     0,     0,     0,     0,     0,   184,     0,     0,   185,
     186,     0,   187,     0,     0,     0,     0,     0,     0,   188,
       0,     0,     0,   189,     0,    39,     0,   190,   660,   191,
       0,     0,     0,    40,     0,   192,     0,   193,    66,   194,
     195,     0,    68,   196,   197,    42,     0,     0,     0,     0,
       0,    44,    45,    46,   180,   181,   182,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     183,     0,     0,     0,     0,     0,     0,     0,     0,   184,
       0,     0,   185,   186,     0,   187,     0,     0,     0,     0,
       0,     0,   188,     0,     0,     0,   189,     0,    39,     0,
     190,     0,   191,     0,     0,     0,    40,     0,   192,     0,
     193,    66,   194,   195,     0,    68,   196,   197,    42,     0,
       0,     0,     0,     0,    44,    45,    46,   180,   181,   182,
       0,     0,     0,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   256,     0,     0,     0,     0,     0,     0,
       0,     0,   184,     0,  -119,     0,   186,     0,   187,     0,
       0,     0,     0,     0,     0,   188,     0,     0,     0,   189,
      39,     0,     0,   190,     0,   191,     0,     0,    40,     0,
       0,   657,     0,   193,    66,     0,   259,     0,    68,     0,
      42,     0,     0,     0,     0,     0,    44,    45,    46,   180,
     181,   182,     0,     0,     0,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   256,     0,     0,     0,     0,
       0,     0,     0,     0,   184,     0,     0,     0,   186,     0,
     187,     0,     0,     0,     0,     0,     0,   188,     0,     0,
       0,   189,    39,     0,     0,   190,     0,   191,     0,     0,
      40,     0,     0,   657,     0,   193,    66,     0,   259,     0,
      68,     0,    42,     0,     0,     0,     0,     0,    44,    45,
      46,   180,   181,   182,     0,     0,     0,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   184,     0,     0,     0,
     186,     0,   187,     0,     0,     0,     0,     0,     0,   188,
       0,     0,     0,   189,    39,     0,     0,   190,     0,   191,
       0,     0,    40,     0,     0,     0,     0,   193,    66,     0,
       0,    41,    68,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,     0,    50,    51,    52,
      53,    39,    54,    55,    56,     0,     0,    57,     0,    40,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,    42,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,     0,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,    39,     0,     0,     0,     0,     0,     0,    64,
      40,     0,     0,     0,     0,   320,     0,     0,     0,    65,
      66,     0,    42,    67,    68,     0,    43,     0,    44,    45,
      46,    47,    48,    49,     0,    50,    51,    52,    53,    39,
      54,    55,    56,     0,     0,    57,    64,    40,     0,    58,
      59,    60,    61,    62,    63,     0,    65,    66,     0,    42,
      67,    68,     0,    43,     0,    44,    45,    46,    47,    48,
      49,     0,    50,    51,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
      39,     0,     0,     0,     0,     0,     0,    64,    40,     0,
       0,     0,     0,     0,     0,     0,     0,    65,    66,     0,
      42,    67,    68,     0,     0,     0,    44,    45,    46,   180,
     181,   182,     0,     0,     0,    52,    53,    39,    54,    55,
      56,     0,     0,    57,     0,    40,     0,    58,    59,    60,
      61,    62,    63,     0,    65,   113,     0,    42,    67,   114,
       0,    43,     0,    44,    45,    46,    47,    48,    49,     0,
      50,    51,    52,    53,    39,    54,    55,    56,     0,     0,
      57,     0,    40,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,     0,    50,    51,    52,
      53,   449,    54,    55,    56,   193,    66,    57,     0,     0,
      68,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,    39,     0,     0,     0,     0,     0,     0,
       0,    40,   241,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    65,    42,     0,     0,    67,    43,     0,    44,
      45,    46,    47,    48,    49,     0,    50,    51,    52,    53,
      39,    54,    55,    56,     0,     0,    57,     0,    40,   241,
      58,    59,    60,    61,    62,    63,     0,     0,     0,    65,
      42,     0,     0,    67,    43,     0,    44,    45,    46,    47,
      48,    49,     0,    50,    51,    52,    53,    39,    54,    55,
      56,     0,     0,    57,     0,    40,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,    42,     0,     0,
       0,     0,     0,    44,    45,    46,   180,   181,   182,     0,
       0,     0,    52,    53,     0,    54,    55,    56,    65,   113,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,    39,     0,
       0,     0,     0,     0,     0,   334,    40,     0,     0,     0,
       0,     0,     0,     0,     0,    65,     0,     0,    42,     0,
       0,     0,     0,     0,    44,    45,    46,   180,   181,   182,
       0,    39,   567,    52,    53,     0,    54,    55,    56,    40,
       0,    57,   568,     0,     0,    58,    59,    60,    61,    62,
      63,    42,   193,     0,     0,     0,     0,    44,    45,    46,
     180,   181,   182,     0,     0,     0,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   654,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   568,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   193,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   193
  };

  const short
  parser::yycheck_[] =
  {
      29,    87,    86,   199,    87,   110,   105,    91,    98,   205,
      64,   300,    98,   105,   188,    98,   244,   190,    29,    34,
      87,    34,   331,   101,   100,   166,   110,   111,   106,   107,
     295,   109,   112,    41,   297,   108,   476,    22,   110,   111,
     176,     5,   325,     1,   110,   111,     1,   189,   190,    21,
       1,    12,   468,   462,    64,   133,    77,   111,   332,    19,
     111,    27,   290,    77,    80,    27,    51,    52,    77,   109,
     119,   355,    89,   105,    82,   631,   218,   109,   102,   111,
     120,   130,   366,   102,   124,   120,   109,   631,   120,   124,
     174,   374,   124,   104,   383,   179,   115,   131,   258,     0,
     116,   111,   174,   189,   190,   388,   192,   116,   174,   130,
     190,   191,   308,   199,   131,   671,   130,   673,    84,   205,
     174,   131,    84,   174,   109,    79,   262,   671,   568,   101,
     258,    80,   218,    77,   408,   120,   165,   166,   298,   124,
     556,   227,   176,   552,    78,   105,   104,   410,   109,   104,
      84,   440,   294,   104,   165,   166,   110,   115,    80,   106,
     115,    80,   116,   192,   174,   105,   176,   116,   122,   116,
     298,   249,   116,   102,   252,   188,   254,    72,    73,    74,
      75,    91,   116,    93,    94,   468,   115,   241,   628,    99,
     130,   115,   110,   110,   116,   267,   110,   116,   116,   116,
     286,   274,   116,   110,   168,   169,    14,   293,   294,   116,
     293,   121,   296,   228,   229,   228,   229,   359,    86,   112,
      77,    89,   308,   116,   533,   534,   293,   238,   262,    81,
     130,   241,    84,   311,   105,   112,   290,   112,   109,   116,
     111,   116,   112,   508,   656,   331,   116,   659,   390,   120,
      77,    79,   262,    81,    86,    27,    84,    89,    90,   101,
     101,   103,   103,    91,    24,    93,    94,   101,   358,   103,
     360,    99,   358,   359,   360,   358,   287,   288,   414,   562,
     334,   110,   295,   546,   101,    84,   103,    86,   303,   117,
     368,   367,   115,   121,   122,    65,    66,   125,   126,   310,
     399,   475,    80,    81,   390,   389,   411,   399,   413,   537,
     115,   416,   116,   418,   120,   121,   457,   626,   326,   403,
     583,   109,   109,    79,    14,   409,   615,   411,   427,   413,
      86,   473,   416,    89,   418,    91,   478,   105,   130,   411,
     482,   413,   131,   516,   416,   411,   418,   413,    71,   612,
     416,   493,   418,   387,   110,   101,    87,   130,   647,    91,
     116,    93,    94,   110,     4,   121,   122,    99,   110,   125,
     126,   110,   110,   110,   110,    23,   116,   387,    27,    90,
     414,   115,    77,   467,   468,    77,    77,   473,    77,   121,
     476,    15,   478,   125,    81,    41,   482,    77,   540,   106,
     116,   597,    80,   631,   414,   106,   116,   493,   110,   130,
     110,   110,   110,   497,   427,    77,   110,   501,   102,   116,
      77,   102,    90,   116,   106,   567,    85,   110,   457,   117,
     116,   110,   117,   462,   110,   631,   516,   517,   110,   110,
      85,   116,    25,   671,    77,   550,   457,   533,   534,    77,
     106,   462,   110,   130,   540,    77,   117,   131,    77,    79,
     656,   545,   475,   659,   117,    80,   550,    80,   102,    89,
      81,    91,   556,    93,    94,   671,   554,   673,   550,    99,
     622,   567,   568,   537,   550,   116,   116,    81,   117,   504,
     116,   504,   109,    89,   110,   508,   106,   106,   116,    78,
     584,   121,   122,   117,    77,   125,   126,    28,   650,   117,
       9,   597,   110,   102,   115,   110,    80,    84,   110,    79,
      80,    80,    11,   552,    90,    53,   165,   611,    86,   613,
     120,    91,   116,    93,    94,   110,   622,   123,   549,    99,
     626,   552,   628,   293,    15,   631,   534,   110,   638,   674,
     390,   622,   638,   286,   218,   638,   229,   117,   657,   650,
     629,   121,   122,   594,   650,   125,   126,   651,   656,   598,
     656,   475,   600,   659,   639,   665,    34,   504,   300,   665,
      34,   576,   665,   667,   674,   671,   255,   673,   674,   615,
     612,   674,     3,     4,     5,     6,     7,     8,   252,    10,
     499,    12,    13,   549,   611,    16,    17,    18,    19,    20,
     639,    22,   537,    24,   220,    26,   399,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
     192,    42,    43,    44,   400,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    -1,    79,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    -1,    93,    94,    76,    -1,    -1,    -1,    99,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,
      91,    92,    -1,    -1,    -1,    -1,    -1,   117,    -1,    -1,
      -1,   121,   122,    -1,   105,   125,   126,    -1,   109,    -1,
     111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,   120,
      -1,    -1,   123,   124,    -1,    -1,    -1,    -1,   129,   130,
     131,   132,    -1,    -1,   135,     3,     4,     5,   139,   140,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    16,    17,
      18,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    57,
      79,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    93,    94,    -1,    -1,    76,    -1,
      99,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    91,    92,    -1,    -1,    -1,   117,    -1,
      -1,    -1,   121,   122,    -1,    -1,    -1,   105,    -1,    -1,
      -1,   109,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   119,   120,    -1,    -1,   123,   124,    -1,    -1,    -1,
      -1,   129,   130,   131,   132,    -1,     1,   135,     3,     4,
       5,   139,   140,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,    -1,    -1,   109,    -1,   111,    -1,    -1,    -1,
     115,    -1,    -1,    -1,   119,   120,    -1,    -1,   123,   124,
      -1,    -1,    -1,    -1,   129,   130,   131,   132,    -1,    -1,
     135,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,
      -1,    93,    94,    -1,    -1,    -1,    -1,    99,    -1,    -1,
      -1,    -1,    -1,   105,    -1,    -1,    -1,   109,   110,   111,
      -1,    -1,    -1,    -1,   116,   117,    -1,   119,   120,   121,
     122,   123,   124,   125,   126,    -1,    -1,   129,   130,   131,
     132,    -1,    -1,   135,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    -1,    91,    -1,    93,    94,    -1,    -1,    -1,    -1,
      99,    -1,    -1,    -1,    -1,    -1,   105,   106,    -1,    -1,
     109,    -1,   111,    -1,    -1,    -1,    -1,    -1,   117,    -1,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,   132,    -1,    -1,   135,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    -1,    91,    -1,    93,    94,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,    -1,   105,
      -1,    -1,    -1,   109,    -1,   111,    -1,    -1,    -1,    -1,
      -1,   117,    -1,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,    -1,   129,   130,   131,   132,    -1,    -1,   135,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,
      -1,    -1,   105,    -1,    -1,    -1,   109,    -1,   111,    -1,
      -1,    -1,   115,    -1,    -1,    -1,   119,   120,    -1,    -1,
     123,   124,    -1,    -1,    -1,    -1,   129,   130,   131,   132,
      -1,    -1,   135,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    46,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,
      -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,
     120,    -1,    -1,   123,   124,    -1,    -1,    -1,    -1,   129,
     130,   131,   132,    -1,    -1,   135,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,
      -1,    -1,   109,    -1,   111,    -1,    -1,    -1,   115,    -1,
      -1,    -1,   119,   120,    -1,    -1,   123,   124,    -1,    -1,
      -1,    -1,   129,   130,   131,   132,    -1,    -1,   135,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   105,    -1,    -1,    -1,   109,    -1,   111,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   119,   120,    -1,    -1,   123,
     124,    -1,    -1,    -1,    -1,   129,   130,   131,   132,    -1,
      -1,   135,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,
      91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,
     111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,   120,
      -1,    -1,   123,   124,    -1,    -1,    -1,    -1,   129,   130,
     131,   132,    -1,    -1,   135,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,
      -1,   109,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   119,   120,    -1,    -1,   123,   124,    -1,    -1,    -1,
      -1,   129,   130,   131,   132,    -1,    -1,   135,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     105,    -1,    -1,    -1,   109,    -1,   111,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   119,   120,    -1,    -1,   123,   124,
      -1,    -1,    -1,    -1,   129,   130,   131,   132,    -1,    -1,
     135,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    86,    -1,    88,    -1,    -1,    -1,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,   111,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,   120,    -1,
      -1,   123,   124,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,    -1,    -1,   135,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,
     109,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     119,   120,    -1,    -1,   123,   124,    -1,    -1,    -1,    -1,
     129,   130,   131,   132,    -1,    -1,   135,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,
      -1,    -1,    -1,   109,    -1,   111,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   119,   120,    -1,    -1,   123,   124,    -1,
      -1,    -1,    -1,   129,   130,   131,   132,    -1,    -1,   135,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,
      -1,    -1,   105,    -1,    -1,    -1,   109,    -1,   111,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   119,   120,    -1,    -1,
     123,   124,    -1,    -1,    -1,    -1,   129,   130,   131,   132,
      -1,    -1,   135,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,
      -1,   111,    -1,     4,    -1,    -1,    -1,    -1,    -1,   119,
     120,    12,    -1,   123,   124,    -1,    -1,    -1,    -1,   129,
     130,   131,   132,    24,    -1,   135,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    86,    -1,    88,    89,    -1,
      91,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,    -1,   105,    -1,     4,    -1,   109,   110,
     111,    -1,    -1,    -1,    12,   116,   117,    -1,   119,   120,
     121,   122,    -1,   124,   125,   126,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    91,    92,    -1,    94,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,   106,     4,
      -1,   109,    -1,   111,    -1,    -1,    -1,    12,    -1,   117,
      -1,   119,   120,   121,   122,    -1,   124,   125,   126,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    91,    92,    -1,    94,
      -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,
     105,    -1,     4,    -1,   109,    -1,   111,    -1,    -1,    -1,
      12,    -1,   117,    -1,   119,   120,   121,   122,    -1,   124,
     125,   126,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,
      92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,   105,    -1,     4,    -1,   109,   110,   111,
      -1,    -1,    -1,    12,    -1,   117,    -1,   119,   120,   121,
     122,    -1,   124,   125,   126,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    -1,    91,    92,    -1,    94,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,   105,    -1,     4,    -1,
     109,    -1,   111,    -1,    -1,    -1,    12,    -1,   117,    -1,
     119,   120,   121,   122,    -1,   124,   125,   126,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    -1,    92,    -1,    94,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,
       4,    -1,    -1,   109,    -1,   111,    -1,    -1,    12,    -1,
      -1,   117,    -1,   119,   120,    -1,   122,    -1,   124,    -1,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,    92,    -1,
      94,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,   105,     4,    -1,    -1,   109,    -1,   111,    -1,    -1,
      12,    -1,    -1,   117,    -1,   119,   120,    -1,   122,    -1,
     124,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,
      92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,   105,     4,    -1,    -1,   109,    -1,   111,
      -1,    -1,    12,    -1,    -1,    -1,    -1,   119,   120,    -1,
      -1,    21,   124,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,     4,    42,    43,    44,    -1,    -1,    47,    -1,    12,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,   109,
      12,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,   119,
     120,    -1,    24,   123,   124,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,     4,
      42,    43,    44,    -1,    -1,    47,   109,    12,    -1,    51,
      52,    53,    54,    55,    56,    -1,   119,   120,    -1,    24,
     123,   124,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       4,    -1,    -1,    -1,    -1,    -1,    -1,   109,    12,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,   120,    -1,
      24,   123,   124,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,     4,    42,    43,
      44,    -1,    -1,    47,    -1,    12,    -1,    51,    52,    53,
      54,    55,    56,    -1,   119,   120,    -1,    24,   123,   124,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,     4,    42,    43,    44,    -1,    -1,
      47,    -1,    12,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,    78,    42,    43,    44,   119,   120,    47,    -1,    -1,
     124,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   119,    24,    -1,    -1,   123,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
       4,    42,    43,    44,    -1,    -1,    47,    -1,    12,   109,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,   119,
      24,    -1,    -1,   123,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,     4,    42,    43,
      44,    -1,    -1,    47,    -1,    12,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    40,    -1,    42,    43,    44,   119,   120,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,    -1,    -1,   109,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   119,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      -1,     4,    99,    39,    40,    -1,    42,    43,    44,    12,
      -1,    47,   109,    -1,    -1,    51,    52,    53,    54,    55,
      56,    24,   119,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   119
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   101,   142,   143,   144,   147,   120,   124,   317,
     148,   159,     0,   148,    65,    66,   145,   102,   115,   149,
     160,   161,     1,   104,   316,   105,   130,   187,   187,   109,
     150,    14,   162,   170,   171,   130,   188,    77,    77,     4,
      12,    21,    24,    28,    30,    31,    32,    33,    34,    35,
      37,    38,    39,    40,    42,    43,    44,    47,    51,    52,
      53,    54,    55,    56,   109,   119,   120,   123,   124,   151,
     152,   153,   157,   284,   287,   288,   301,   302,   303,   309,
      27,    24,   163,   115,   158,     3,     5,     6,     7,     8,
      10,    13,    16,    17,    18,    19,    20,    22,    26,    29,
      36,    50,    57,    62,    63,    76,    82,    88,    91,    92,
     105,   109,   111,   120,   124,   129,   130,   131,   132,   135,
     139,   140,   168,   172,   173,   174,   175,   179,   192,   229,
     234,   239,   240,   244,   245,   246,   247,   273,   274,   277,
     278,   300,   301,   303,   311,   312,   315,   106,   116,   317,
      79,    89,    91,    93,    94,    99,   121,   122,   125,   126,
     306,   307,   308,   310,   110,   116,   109,   154,   101,   103,
     146,   317,   115,    63,   109,   237,   238,   240,   241,   243,
      33,    34,    35,    79,    88,    91,    92,    94,   101,   105,
     109,   111,   117,   119,   121,   122,   125,   126,   181,   194,
     195,   198,   200,   202,   203,   205,   207,   209,   282,   283,
     285,   287,   289,   290,   297,   298,   299,   309,   109,   101,
     103,   266,   237,    72,    73,    74,    75,   176,   101,   103,
     184,   185,   200,   202,    14,   266,   245,   105,   235,   236,
     130,   109,   301,   105,   109,   275,   276,   278,   312,    92,
     245,   264,   265,   245,   244,   245,    79,   106,   117,   122,
     126,   237,   238,   248,   250,   251,   280,   294,   296,   305,
     306,   308,   313,   314,    91,   110,   116,   248,   249,   306,
     307,   308,   313,   318,   112,   318,    19,   235,   235,   131,
     167,   158,    71,   182,    80,   116,    81,    84,   117,   231,
     232,   233,   280,   293,   295,   304,   306,   307,   100,   245,
     101,    87,   130,   110,   110,   110,   110,   110,   110,   153,
      78,   155,   156,   157,   148,   148,     4,   164,   119,   130,
      23,    80,   293,   237,   109,   192,   222,   223,   224,   303,
      28,   106,   196,   198,   200,    86,    89,   110,   196,   213,
     289,   318,   318,   287,   299,    27,   186,   209,    90,    86,
      89,   206,   209,   196,   212,   213,    20,    46,    92,   237,
     263,   267,   268,   269,   267,   115,   242,    77,    77,    77,
      77,   190,   196,   210,   183,   229,   230,   239,   183,    15,
      81,    41,    89,   131,   301,    77,    77,   130,   314,   116,
      80,   189,   245,    86,   264,   231,   302,   311,   293,    78,
      84,   116,   106,   116,   238,   110,   116,   110,   116,   110,
     110,   110,   116,   112,   210,   301,   301,   117,   169,   279,
     291,   292,   307,   314,   130,   181,   191,   196,   197,   300,
     237,   253,   254,   269,   302,   186,   233,   240,   209,    78,
     270,   271,   272,   301,   245,   110,   110,   116,   102,   316,
     317,    12,   109,   165,   166,    77,    77,   101,   103,   255,
     190,   241,   307,    80,   102,   116,   214,   106,    90,   110,
     110,   110,   116,   110,   110,   112,   117,   117,   185,   200,
     196,   202,   110,   116,   185,   266,   245,    85,   102,   115,
     316,    25,   186,   102,   115,   316,   237,   197,   130,   131,
     106,    77,    77,   110,   276,   105,   109,   111,   281,   282,
      77,   237,   117,   117,   237,   252,   269,   237,   248,   248,
     248,   248,    77,    80,    80,   303,   312,   116,    77,   130,
      80,    81,   180,   217,   186,    81,   116,   117,   102,   116,
      81,   157,   109,   151,    92,   102,   115,   237,   256,   257,
     258,   262,   256,   316,   110,   196,   224,    99,   109,   215,
     297,   196,   196,   196,   237,   268,   237,   230,   300,   106,
     110,   289,   112,   116,    78,   190,   193,   193,   117,   117,
     291,    77,   196,   216,    28,   218,   219,   220,     9,   225,
     226,   227,   237,   269,   271,   248,   151,   110,   245,   256,
     102,   115,    84,    86,   259,   260,   261,   316,   196,   297,
     110,   242,    80,   269,   237,    77,   116,    77,   214,    84,
     199,   204,   208,   209,   221,    22,    51,    52,   109,   177,
     228,   286,   287,   227,   110,   258,   253,   237,   186,   261,
      80,    11,   191,   190,    99,   219,    90,   117,   208,   279,
     110,   200,   201,   211,   228,    53,   178,    86,   186,   216,
     237,   204,   221,   204,   116,   110,   200,   237,   110,   211
  };

  const short
  parser::yyr1_[] =
  {
       0,   141,   142,   143,   143,   144,   145,   145,   145,   146,
     146,   147,   147,   148,   149,   149,   149,   150,   150,   151,
     152,   152,   153,   153,   154,   154,   154,   155,   155,   156,
     156,   157,   157,   158,   158,   159,   159,   160,   161,   161,
     162,   163,   163,   164,   164,   165,   165,   166,   166,   167,
     167,   168,   168,   168,   169,   169,   170,   171,   171,   172,
     172,   172,   172,   172,   172,   172,   173,   174,   174,   174,
     175,   176,   176,   176,   176,   176,   177,   177,   177,   178,
     179,   179,   180,   180,   181,   181,   182,   182,   182,   183,
     183,   183,   183,   184,   184,   185,   186,   186,   187,   187,
     188,   188,   188,   189,   189,   190,   191,   192,   192,   193,
     193,   194,   195,   195,   196,   196,   196,   197,   198,   199,
     200,   200,   201,   202,   203,   203,   204,   204,   205,   205,
     205,   206,   207,   207,   208,   209,   209,   209,   209,   209,
     209,   209,   209,   209,   210,   211,   211,   212,   212,   213,
     213,   214,   214,   215,   215,   216,   217,   218,   218,   219,
     219,   220,   220,   221,   221,   222,   222,   223,   223,   224,
     225,   225,   226,   226,   227,   227,   227,   228,   228,   228,
     229,   229,   229,   230,   231,   231,   232,   232,   233,   234,
     234,   234,   234,   234,   234,   234,   234,   234,   235,   235,
     236,   236,   237,   237,   238,   238,   239,   239,   240,   240,
     240,   241,   241,   242,   242,   243,   243,   244,   244,   244,
     244,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     246,   246,   247,   247,   247,   247,   247,   247,   247,   248,
     248,   248,   249,   249,   250,   250,   250,   250,   250,   250,
     250,   251,   251,   252,   252,   253,   254,   254,   255,   255,
     255,   255,   256,   256,   257,   257,   257,   258,   259,   259,
     260,   260,   261,   262,   262,   263,   263,   264,   264,   265,
     265,   266,   266,   267,   267,   267,   267,   268,   268,   269,
     269,   269,   270,   270,   271,   271,   271,   272,   272,   273,
     273,   274,   274,   275,   275,   275,   276,   276,   277,   277,
     277,   277,   278,   278,   279,   279,   280,   280,   281,   281,
     281,   282,   282,   282,   282,   282,   283,   283,   283,   284,
     284,   284,   284,   284,   285,   285,   286,   287,   287,   288,
     289,   289,   289,   290,   290,   290,   290,   291,   291,   292,
     292,   293,   293,   294,   294,   295,   295,   296,   296,   297,
     298,   299,   299,   299,   299,   299,   300,   300,   301,   301,
     301,   302,   302,   303,   303,   303,   303,   303,   303,   303,
     303,   304,   304,   305,   305,   306,   307,   307,   308,   308,
     309,   309,   309,   309,   309,   309,   309,   309,   309,   309,
     309,   309,   309,   309,   309,   309,   309,   309,   310,   310,
     310,   311,   311,   312,   313,   313,   314,   314,   315,   315,
     315,   315,   315,   316,   316,   317,   317,   318,   318
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
       1,     3,     1,     1,     1,     3,     1,     2,     1,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     2,     3,
       2,     5,     3,     3,     1,     1,     3,     1,     0,     1,
       3,     2,     0,     1,     5,     1,     2,     3,     1,     4,
       2,     3,     0,     1,     3,     0,     1,     3,     1,     3,
       0,     1,     2,     1,     2,     3,     3,     1,     2,     3,
       1,     3,     2,     1,     3,     2,     2,     1,     4,     3,
       3,     4,     4,     3,     4,     6,     6,     4,     0,     1,
       3,     4,     3,     1,     1,     3,     1,     3,     2,     3,
       1,     1,     2,     1,     0,     3,     3,     2,     3,     2,
       1,     3,     2,     4,     4,     8,     4,     2,     2,     1,
       4,     1,     1,     1,     1,     3,     3,     3,     1,     1,
       2,     2,     3,     3,     1,     1,     2,     4,     3,     5,
       3,     3,     3,     3,     1,     1,     3,     1,     3,     3,
       2,     2,     1,     2,     3,     2,     1,     2,     3,     2,
       2,     1,     4,     1,     2,     1,     2,     1,     2,     2,
       1,     3,     3,     3,     2,     1,     0,     1,     2,     3,
       1,     2,     1,     0,     3,     1,     1,     3,     1,     1,
       1,     1,     3,     1,     3,     1,     1,     3,     2,     3,
       2,     3,     1,     2,     1,     3,     1,     3,     1,     2,
       2,     1,     3,     3,     3,     2,     1,     3,     3,     1,
       3,     3,     3,     3,     1,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     1,     1,     1,     1,     1,     3,     1,     3,     1,
       3,     1,     1,     1,     1,     1,     1,     3,     1,     3,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     1
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
  "\"\\\\\"", "\"lcase\"", "\"|\"", "\"<-\"", "\"->\"", "\"@\"",
  "PREFIX_TILDE", "\"~\"", "\"=>\"", "\"-\"", "PREFIX_BANG", "\"!\"",
  "\"*\"", "\"-<\"", "\">-\"", "\"-<<\"", "\">>-\"", "\".\"",
  "\"TYPEAPP\"", "\"{\"", "\"}\"", "\"vocurly\"", "\"vccurly\"", "\"[\"",
  "\"]\"", "\"[:\"", "\":]\"", "\"(\"", "\")\"", "\"(#\"", "\"#)\"",
  "\"(|\"", "\"|)\"", "\";\"", "\",\"", "\"`\"", "\"'\"", "\"VARID\"",
  "\"CONID\"", "\"VARSYM\"", "\"CONSYM\"", "\"QVARID\"", "\"QCONID\"",
  "\"QVARSYM\"", "\"QCONSYM\"", "\"IPDUPVARID\"", "\"LABELVARID\"",
  "\"CHAR\"", "\"STRING\"", "\"INTEGER\"", "\"RATIONAL\"", "\"PRIMCHAR\"",
  "\"PRIMSTRING\"", "\"PRIMINTEGER\"", "\"PRIMWORD\"", "\"PRIMFLOAT\"",
  "\"PRIMDOUBLE\"", "\"{-# SPECIALISE\"", "\"{-# SPECIALISE_INLINE\"",
  "$accept", "unit", "module", "missing_module_keyword", "maybemodwarning",
  "body", "body2", "top", "top1", "maybeexports", "exportlist",
  "exportlist1", "export", "export_subspec", "qcnames", "qcnames1",
  "qcname", "semis1", "semis", "importdecls", "importdecls_semi",
  "importdecl", "optqualified", "maybeas", "maybeimpspec", "impspec",
  "prec", "infix", "ops", "topdecls", "topdecls_semi", "topdecl",
  "cl_decl", "ty_decl", "inst_decl", "overlap_pragma",
  "deriv_strategy_no_via", "deriv_strategy_via", "data_or_newtype",
  "opt_kind_sig", "tycl_hdr", "capi_ctype", "decls", "decllist", "binds",
  "wherebinds", "strings", "stringlist", "opt_tyconsig", "sigtype",
  "sigtypedoc", "sig_vars", "sigtypes1", "strict_mark", "strictness",
  "ctype", "ctypedoc", "context", "context_no_ops", "type", "typedoc",
  "btype", "infixtype", "btype_no_ops", "ftype", "tyarg", "tyop",
  "atype_docs", "atype", "inst_type", "deriv_types", "comma_types0",
  "comma_types1", "tv_bndrs", "tv_bndr", "kind", "constrs", "constrs1",
  "constr", "forall", "constr_stuff", "fielddecls", "fielddecls1",
  "fielddecl", "maybe_derivings", "derivings", "deriving",
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
       0,   478,   478,   495,   496,   498,   502,   503,   504,   506,
     507,   509,   510,   513,   515,   516,   517,   525,   526,   528,
     530,   531,   533,   534,   536,   537,   538,   540,   541,   543,
     544,   546,   547,   551,   552,   554,   555,   557,   559,   560,
     562,   575,   576,   578,   579,   581,   582,   586,   587,   592,
     593,   595,   596,   597,   599,   600,   604,   606,   607,   609,
     610,   611,   614,   615,   622,   624,   626,   628,   629,   630,
     635,   640,   641,   642,   643,   644,   646,   647,   648,   650,
     690,   691,   693,   694,   703,   704,   706,   707,   708,   752,
     753,   754,   755,   757,   758,   760,   762,   763,   771,   772,
     774,   775,   776,   789,   790,   792,   794,   796,   797,   799,
     800,   804,   810,   811,   818,   819,   821,   823,   832,   834,
     836,   837,   839,   842,   844,   845,   847,   848,   850,   851,
     852,   854,   856,   857,   864,   871,   872,   873,   874,   875,
     876,   877,   883,   884,   888,   890,   891,   893,   894,   896,
     897,   904,   905,   908,   909,   927,   933,   935,   936,   938,
     939,   941,   942,   944,   945,   947,   948,   950,   951,   953,
     955,   956,   958,   959,   961,   962,   963,   965,   966,   967,
     972,   974,   975,   977,   981,   982,   984,   985,   989,   999,
    1000,  1002,  1003,  1004,  1005,  1006,  1007,  1008,  1011,  1012,
    1014,  1015,  1019,  1020,  1022,  1023,  1025,  1026,  1028,  1029,
    1030,  1032,  1033,  1036,  1037,  1039,  1040,  1044,  1045,  1046,
    1047,  1049,  1050,  1051,  1052,  1054,  1056,  1057,  1058,  1060,
    1062,  1063,  1065,  1066,  1067,  1068,  1069,  1074,  1075,  1080,
    1081,  1082,  1087,  1088,  1106,  1107,  1108,  1109,  1110,  1111,
    1112,  1114,  1115,  1128,  1130,  1140,  1142,  1143,  1146,  1147,
    1148,  1149,  1151,  1152,  1154,  1155,  1156,  1158,  1160,  1161,
    1163,  1164,  1173,  1175,  1176,  1178,  1179,  1181,  1182,  1184,
    1185,  1188,  1189,  1191,  1192,  1193,  1194,  1199,  1200,  1202,
    1203,  1204,  1209,  1210,  1212,  1213,  1214,  1216,  1217,  1249,
    1250,  1252,  1253,  1255,  1256,  1257,  1259,  1260,  1262,  1263,
    1264,  1265,  1267,  1268,  1270,  1271,  1273,  1274,  1277,  1278,
    1279,  1281,  1282,  1283,  1284,  1285,  1287,  1288,  1289,  1291,
    1292,  1293,  1294,  1295,  1298,  1299,  1301,  1303,  1304,  1308,
    1310,  1311,  1312,  1314,  1315,  1316,  1317,  1322,  1323,  1325,
    1326,  1328,  1329,  1332,  1333,  1338,  1339,  1341,  1342,  1346,
    1348,  1350,  1351,  1352,  1353,  1354,  1357,  1358,  1360,  1361,
    1362,  1364,  1365,  1367,  1368,  1369,  1370,  1371,  1372,  1373,
    1374,  1376,  1377,  1379,  1380,  1382,  1384,  1385,  1387,  1388,
    1390,  1391,  1392,  1393,  1394,  1395,  1396,  1397,  1398,  1399,
    1400,  1401,  1402,  1403,  1404,  1405,  1406,  1407,  1409,  1410,
    1411,  1415,  1416,  1418,  1420,  1421,  1423,  1424,  1428,  1429,
    1430,  1431,  1432,  1437,  1440,  1444,  1445,  1447,  1448
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
#line 6094 "parser.cc"

#line 1457 "parser.y"


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
