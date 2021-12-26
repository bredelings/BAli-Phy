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
#line 53 "parser.y"

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
      case symbol_kind::S_decllist: // decllist
        value.YY_MOVE_OR_COPY< Haskell::Decls > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_export: // export
        value.YY_MOVE_OR_COPY< Haskell::Export > (YY_MOVE (that.value));
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

      case symbol_kind::S_impspec: // impspec
        value.YY_MOVE_OR_COPY< Haskell::ImpSpec > (YY_MOVE (that.value));
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

      case symbol_kind::S_tv_bndr: // tv_bndr
        value.YY_MOVE_OR_COPY< Haskell::TypeVar > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_alt_rhs: // alt_rhs
        value.YY_MOVE_OR_COPY< Hs::MultiGuardedRHS > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alt: // alt
        value.YY_MOVE_OR_COPY< Located<Haskell::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_binds: // binds
        value.YY_MOVE_OR_COPY< Located<Haskell::Binds> > (YY_MOVE (that.value));
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
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_constr_stuff: // constr_stuff
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
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
        value.YY_MOVE_OR_COPY< std::optional<Haskell::ExportSubSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.YY_MOVE_OR_COPY< std::optional<Haskell::ImpSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.YY_MOVE_OR_COPY< std::optional<Located<Haskell::Binds>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_prec: // prec
        value.YY_MOVE_OR_COPY< std::optional<int> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.YY_MOVE_OR_COPY< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.YY_MOVE_OR_COPY< std::optional<std::vector<Haskell::Export>> > (YY_MOVE (that.value));
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

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        value.YY_MOVE_OR_COPY< std::vector<Haskell::Export> > (YY_MOVE (that.value));
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

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_forall: // forall
        value.YY_MOVE_OR_COPY< std::vector<Haskell::TypeVar> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.YY_MOVE_OR_COPY< std::vector<Haskell::Var> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.YY_MOVE_OR_COPY< std::vector<Located<Haskell::Alt>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
        value.YY_MOVE_OR_COPY< std::vector<Located<std::string>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decls: // decls
      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
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
      case symbol_kind::S_decllist: // decllist
        value.move< Haskell::Decls > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_export: // export
        value.move< Haskell::Export > (YY_MOVE (that.value));
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

      case symbol_kind::S_impspec: // impspec
        value.move< Haskell::ImpSpec > (YY_MOVE (that.value));
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

      case symbol_kind::S_tv_bndr: // tv_bndr
        value.move< Haskell::TypeVar > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_alt_rhs: // alt_rhs
        value.move< Hs::MultiGuardedRHS > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alt: // alt
        value.move< Located<Haskell::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_binds: // binds
        value.move< Located<Haskell::Binds> > (YY_MOVE (that.value));
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
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_constr_stuff: // constr_stuff
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
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
        value.move< std::optional<Haskell::ExportSubSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.move< std::optional<Haskell::ImpSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.move< std::optional<Located<Haskell::Binds>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_prec: // prec
        value.move< std::optional<int> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.move< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.move< std::optional<std::vector<Haskell::Export>> > (YY_MOVE (that.value));
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

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        value.move< std::vector<Haskell::Export> > (YY_MOVE (that.value));
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

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_forall: // forall
        value.move< std::vector<Haskell::TypeVar> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.move< std::vector<Haskell::Var> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Located<Haskell::Alt>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
        value.move< std::vector<Located<std::string>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decls: // decls
      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
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
      case symbol_kind::S_decllist: // decllist
        value.copy< Haskell::Decls > (that.value);
        break;

      case symbol_kind::S_export: // export
        value.copy< Haskell::Export > (that.value);
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

      case symbol_kind::S_impspec: // impspec
        value.copy< Haskell::ImpSpec > (that.value);
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

      case symbol_kind::S_tv_bndr: // tv_bndr
        value.copy< Haskell::TypeVar > (that.value);
        break;

      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_alt_rhs: // alt_rhs
        value.copy< Hs::MultiGuardedRHS > (that.value);
        break;

      case symbol_kind::S_alt: // alt
        value.copy< Located<Haskell::Alt> > (that.value);
        break;

      case symbol_kind::S_binds: // binds
        value.copy< Located<Haskell::Binds> > (that.value);
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
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_constr_stuff: // constr_stuff
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
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
        value.copy< std::optional<Haskell::ExportSubSpec> > (that.value);
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.copy< std::optional<Haskell::ImpSpec> > (that.value);
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.copy< std::optional<Located<Haskell::Binds>> > (that.value);
        break;

      case symbol_kind::S_prec: // prec
        value.copy< std::optional<int> > (that.value);
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.copy< std::optional<std::string> > (that.value);
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.copy< std::optional<std::vector<Haskell::Export>> > (that.value);
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

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        value.copy< std::vector<Haskell::Export> > (that.value);
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

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_forall: // forall
        value.copy< std::vector<Haskell::TypeVar> > (that.value);
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.copy< std::vector<Haskell::Var> > (that.value);
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.copy< std::vector<Located<Haskell::Alt>> > (that.value);
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
        value.copy< std::vector<Located<std::string>> > (that.value);
        break;

      case symbol_kind::S_decls: // decls
      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
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
      case symbol_kind::S_decllist: // decllist
        value.move< Haskell::Decls > (that.value);
        break;

      case symbol_kind::S_export: // export
        value.move< Haskell::Export > (that.value);
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

      case symbol_kind::S_impspec: // impspec
        value.move< Haskell::ImpSpec > (that.value);
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

      case symbol_kind::S_tv_bndr: // tv_bndr
        value.move< Haskell::TypeVar > (that.value);
        break;

      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_alt_rhs: // alt_rhs
        value.move< Hs::MultiGuardedRHS > (that.value);
        break;

      case symbol_kind::S_alt: // alt
        value.move< Located<Haskell::Alt> > (that.value);
        break;

      case symbol_kind::S_binds: // binds
        value.move< Located<Haskell::Binds> > (that.value);
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
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_constr_stuff: // constr_stuff
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
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
        value.move< std::optional<Haskell::ExportSubSpec> > (that.value);
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.move< std::optional<Haskell::ImpSpec> > (that.value);
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.move< std::optional<Located<Haskell::Binds>> > (that.value);
        break;

      case symbol_kind::S_prec: // prec
        value.move< std::optional<int> > (that.value);
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.move< std::optional<std::string> > (that.value);
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.move< std::optional<std::vector<Haskell::Export>> > (that.value);
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

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        value.move< std::vector<Haskell::Export> > (that.value);
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

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_forall: // forall
        value.move< std::vector<Haskell::TypeVar> > (that.value);
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.move< std::vector<Haskell::Var> > (that.value);
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Located<Haskell::Alt>> > (that.value);
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
        value.move< std::vector<Located<std::string>> > (that.value);
        break;

      case symbol_kind::S_decls: // decls
      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
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
      case symbol_kind::S_decllist: // decllist
        yylhs.value.emplace< Haskell::Decls > ();
        break;

      case symbol_kind::S_export: // export
        yylhs.value.emplace< Haskell::Export > ();
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

      case symbol_kind::S_impspec: // impspec
        yylhs.value.emplace< Haskell::ImpSpec > ();
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

      case symbol_kind::S_tv_bndr: // tv_bndr
        yylhs.value.emplace< Haskell::TypeVar > ();
        break;

      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_alt_rhs: // alt_rhs
        yylhs.value.emplace< Hs::MultiGuardedRHS > ();
        break;

      case symbol_kind::S_alt: // alt
        yylhs.value.emplace< Located<Haskell::Alt> > ();
        break;

      case symbol_kind::S_binds: // binds
        yylhs.value.emplace< Located<Haskell::Binds> > ();
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
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_constr_stuff: // constr_stuff
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
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
        yylhs.value.emplace< std::optional<Haskell::ExportSubSpec> > ();
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        yylhs.value.emplace< std::optional<Haskell::ImpSpec> > ();
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        yylhs.value.emplace< std::optional<Located<Haskell::Binds>> > ();
        break;

      case symbol_kind::S_prec: // prec
        yylhs.value.emplace< std::optional<int> > ();
        break;

      case symbol_kind::S_maybeas: // maybeas
        yylhs.value.emplace< std::optional<std::string> > ();
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        yylhs.value.emplace< std::optional<std::vector<Haskell::Export>> > ();
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

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        yylhs.value.emplace< std::vector<Haskell::Export> > ();
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

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_forall: // forall
        yylhs.value.emplace< std::vector<Haskell::TypeVar> > ();
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        yylhs.value.emplace< std::vector<Haskell::Var> > ();
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        yylhs.value.emplace< std::vector<Located<Haskell::Alt>> > ();
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
        yylhs.value.emplace< std::vector<Located<std::string>> > ();
        break;

      case symbol_kind::S_decls: // decls
      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
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
#line 476 "parser.y"
             {drv.result = yystack_[0].value.as < Haskell::Module > ();}
#line 2056 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 493 "parser.y"
                                                                 {yylhs.value.as < Haskell::Module > () = Haskell::Module{yystack_[4].value.as < std::string > (),yystack_[2].value.as < std::optional<std::vector<Haskell::Export>> > (),yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ().second};}
#line 2062 "parser.cc"
    break;

  case 4: // module: body2
#line 494 "parser.y"
                                                                 {yylhs.value.as < Haskell::Module > () = Haskell::Module{"Main",{},yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ().second};}
#line 2068 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 496 "parser.y"
                                                                 {drv.push_module_context();}
#line 2074 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 504 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 2080 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 505 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 2086 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 507 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 2092 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 508 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 2098 "parser.cc"
    break;

  case 13: // top: semis top1
#line 511 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 2104 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 513 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Haskell::ImpDecl> > (),yystack_[0].value.as < Haskell::Decls > ());}
#line 2110 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 514 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Haskell::ImpDecl> > (),yystack_[0].value.as < Haskell::Decls > ());}
#line 2116 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 515 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Haskell::ImpDecl> > (),{});}
#line 2122 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 523 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Haskell::Export>> > () = yystack_[1].value.as < std::vector<Haskell::Export> > ();}
#line 2128 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 524 "parser.y"
                                      {}
#line 2134 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 526 "parser.y"
                                      {yylhs.value.as < std::vector<Haskell::Export> > () = yystack_[0].value.as < std::vector<Haskell::Export> > ();}
#line 2140 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 528 "parser.y"
                                      {yylhs.value.as < std::vector<Haskell::Export> > () = yystack_[2].value.as < std::vector<Haskell::Export> > (); yylhs.value.as < std::vector<Haskell::Export> > ().push_back(yystack_[0].value.as < Haskell::Export > ());}
#line 2146 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 529 "parser.y"
                                      {yylhs.value.as < std::vector<Haskell::Export> > ().push_back(yystack_[0].value.as < Haskell::Export > ());}
#line 2152 "parser.cc"
    break;

  case 22: // export: qcname export_subspec
#line 531 "parser.y"
                                      {yylhs.value.as < Haskell::Export > () = Haskell::ExportSymbol{yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Haskell::ExportSubSpec> > ()}; }
#line 2158 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 532 "parser.y"
                                      {yylhs.value.as < Haskell::Export > () = Haskell::ExportModule{{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2164 "parser.cc"
    break;

  case 24: // export_subspec: %empty
#line 534 "parser.y"
                                      {}
#line 2170 "parser.cc"
    break;

  case 25: // export_subspec: "(" qcnames ")"
#line 535 "parser.y"
                                      { yylhs.value.as < std::optional<Haskell::ExportSubSpec> > () = Haskell::ExportSubSpecSome{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2176 "parser.cc"
    break;

  case 26: // export_subspec: "(" ".." ")"
#line 536 "parser.y"
                                      { yylhs.value.as < std::optional<Haskell::ExportSubSpec> > () = Haskell::ExportSubSpecAll(); }
#line 2182 "parser.cc"
    break;

  case 27: // qcnames: %empty
#line 538 "parser.y"
                   {}
#line 2188 "parser.cc"
    break;

  case 28: // qcnames: qcnames1
#line 539 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2194 "parser.cc"
    break;

  case 29: // qcnames1: qcnames1 "," qcname
#line 541 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2200 "parser.cc"
    break;

  case 30: // qcnames1: qcname
#line 542 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2206 "parser.cc"
    break;

  case 31: // qcname: qvar
#line 544 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2212 "parser.cc"
    break;

  case 32: // qcname: oqtycon_no_varcon
#line 545 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2218 "parser.cc"
    break;

  case 37: // importdecls: importdecls_semi importdecl
#line 555 "parser.y"
                                         { yylhs.value.as < std::vector<Haskell::ImpDecl> > () = yystack_[1].value.as < std::vector<Haskell::ImpDecl> > (), yylhs.value.as < std::vector<Haskell::ImpDecl> > ().push_back(yystack_[0].value.as < Haskell::ImpDecl > ()); }
#line 2224 "parser.cc"
    break;

  case 38: // importdecls_semi: importdecls_semi importdecl semis1
#line 557 "parser.y"
                                                     { yylhs.value.as < std::vector<Haskell::ImpDecl> > () = yystack_[2].value.as < std::vector<Haskell::ImpDecl> > (); yylhs.value.as < std::vector<Haskell::ImpDecl> > ().push_back(yystack_[1].value.as < Haskell::ImpDecl > ()); }
#line 2230 "parser.cc"
    break;

  case 39: // importdecls_semi: %empty
#line 558 "parser.y"
                         { }
#line 2236 "parser.cc"
    break;

  case 40: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 560 "parser.y"
                                                                                                        {
    yylhs.value.as < Haskell::ImpDecl > () = Haskell::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < std::string > (),yystack_[1].value.as < std::optional<std::string> > (),yystack_[0].value.as < std::optional<Haskell::ImpSpec> > ());
}
#line 2244 "parser.cc"
    break;

  case 41: // optqualified: "qualified"
#line 573 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2250 "parser.cc"
    break;

  case 42: // optqualified: %empty
#line 574 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2256 "parser.cc"
    break;

  case 43: // maybeas: "as" modid
#line 576 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 2262 "parser.cc"
    break;

  case 44: // maybeas: %empty
#line 577 "parser.y"
                               { }
#line 2268 "parser.cc"
    break;

  case 45: // maybeimpspec: impspec
#line 579 "parser.y"
                               { yylhs.value.as < std::optional<Haskell::ImpSpec> > () = yystack_[0].value.as < Haskell::ImpSpec > (); }
#line 2274 "parser.cc"
    break;

  case 46: // maybeimpspec: %empty
#line 580 "parser.y"
                               { }
#line 2280 "parser.cc"
    break;

  case 47: // impspec: "(" exportlist ")"
#line 584 "parser.y"
                                      { yylhs.value.as < Haskell::ImpSpec > () = Haskell::ImpSpec{false, yystack_[1].value.as < std::vector<Haskell::Export> > ()}; }
#line 2286 "parser.cc"
    break;

  case 48: // impspec: "hiding" "(" exportlist ")"
#line 585 "parser.y"
                                      { yylhs.value.as < Haskell::ImpSpec > () = Haskell::ImpSpec{true,  yystack_[1].value.as < std::vector<Haskell::Export> > ()}; }
#line 2292 "parser.cc"
    break;

  case 49: // prec: %empty
#line 590 "parser.y"
                   { }
#line 2298 "parser.cc"
    break;

  case 50: // prec: "INTEGER"
#line 591 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < int > (); }
#line 2304 "parser.cc"
    break;

  case 51: // infix: "infix"
#line 593 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infix; }
#line 2310 "parser.cc"
    break;

  case 52: // infix: "infixl"
#line 594 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infixl; }
#line 2316 "parser.cc"
    break;

  case 53: // infix: "infixr"
#line 595 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infixr; }
#line 2322 "parser.cc"
    break;

  case 54: // ops: ops "," op
#line 597 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 2328 "parser.cc"
    break;

  case 55: // ops: op
#line 598 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 2334 "parser.cc"
    break;

  case 56: // topdecls: topdecls_semi topdecl
#line 602 "parser.y"
                                 { yylhs.value.as < Haskell::Decls > () = yystack_[1].value.as < Haskell::Decls > (); yylhs.value.as < Haskell::Decls > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 2340 "parser.cc"
    break;

  case 57: // topdecls_semi: topdecls_semi topdecl semis1
#line 604 "parser.y"
                                            { yylhs.value.as < Haskell::Decls > () = yystack_[2].value.as < Haskell::Decls > (); yylhs.value.as < Haskell::Decls > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2346 "parser.cc"
    break;

  case 58: // topdecls_semi: %empty
#line 605 "parser.y"
                                            { }
#line 2352 "parser.cc"
    break;

  case 59: // topdecl: cl_decl
#line 607 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2358 "parser.cc"
    break;

  case 60: // topdecl: ty_decl
#line 608 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2364 "parser.cc"
    break;

  case 61: // topdecl: inst_decl
#line 609 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2370 "parser.cc"
    break;

  case 62: // topdecl: "default" "(" comma_types0 ")"
#line 612 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Haskell::DefaultDecl(yystack_[1].value.as < std::vector<expression_ref> > ()); }
#line 2376 "parser.cc"
    break;

  case 63: // topdecl: decl_no_th
#line 619 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2382 "parser.cc"
    break;

  case 64: // topdecl: infixexp_top
#line 621 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2388 "parser.cc"
    break;

  case 65: // topdecl: "builtin" var "INTEGER" "STRING" "STRING"
#line 622 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Haskell::BuiltinDecl(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2394 "parser.cc"
    break;

  case 66: // topdecl: "builtin" var "INTEGER" "STRING"
#line 623 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Haskell::BuiltinDecl(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2400 "parser.cc"
    break;

  case 67: // topdecl: "builtin" varop "INTEGER" "STRING" "STRING"
#line 624 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Haskell::BuiltinDecl(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2406 "parser.cc"
    break;

  case 68: // topdecl: "builtin" varop "INTEGER" "STRING"
#line 625 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Haskell::BuiltinDecl(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2412 "parser.cc"
    break;

  case 69: // cl_decl: "class" tycl_hdr wherebinds
#line 627 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_class_decl(yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().second,yystack_[0].value.as < std::optional<Located<Haskell::Binds>> > ());}
#line 2418 "parser.cc"
    break;

  case 70: // ty_decl: "type" type "=" ctypedoc
#line 629 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_type_synonym({yystack_[2].location,yystack_[2].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2424 "parser.cc"
    break;

  case 71: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 630 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < Haskell::DataOrNewtype > (),yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().second,yystack_[1].value.as < std::vector<Haskell::Constructor> > ());}
#line 2430 "parser.cc"
    break;

  case 72: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig
#line 631 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[3].value.as < Haskell::DataOrNewtype > (),yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().second,{});}
#line 2436 "parser.cc"
    break;

  case 73: // inst_decl: "instance" overlap_pragma inst_type wherebinds
#line 636 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_instance_decl({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Haskell::Binds>> > ());}
#line 2442 "parser.cc"
    break;

  case 83: // data_or_newtype: "data"
#line 691 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::data;}
#line 2448 "parser.cc"
    break;

  case 84: // data_or_newtype: "newtype"
#line 692 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::newtype;}
#line 2454 "parser.cc"
    break;

  case 87: // tycl_hdr: context "=>" type
#line 704 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ()};}
#line 2460 "parser.cc"
    break;

  case 88: // tycl_hdr: type
#line 705 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {{},yystack_[0].value.as < expression_ref > ()};}
#line 2466 "parser.cc"
    break;

  case 92: // decls: decls ";" decl
#line 753 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2472 "parser.cc"
    break;

  case 93: // decls: decls ";"
#line 754 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2478 "parser.cc"
    break;

  case 94: // decls: decl
#line 755 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2484 "parser.cc"
    break;

  case 95: // decls: %empty
#line 756 "parser.y"
                        {}
#line 2490 "parser.cc"
    break;

  case 96: // decllist: "{" decls "}"
#line 758 "parser.y"
                                 {yylhs.value.as < Haskell::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2496 "parser.cc"
    break;

  case 97: // decllist: "vocurly" decls close
#line 759 "parser.y"
                                 {yylhs.value.as < Haskell::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2502 "parser.cc"
    break;

  case 98: // binds: decllist
#line 761 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Haskell::Decls > ()}};}
#line 2508 "parser.cc"
    break;

  case 99: // wherebinds: "where" binds
#line 763 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Haskell::Binds>> > () = yystack_[0].value.as < Located<Haskell::Binds> > ();}
#line 2514 "parser.cc"
    break;

  case 100: // wherebinds: %empty
#line 764 "parser.y"
                                 {}
#line 2520 "parser.cc"
    break;

  case 106: // opt_tyconsig: %empty
#line 790 "parser.y"
                     {}
#line 2526 "parser.cc"
    break;

  case 107: // opt_tyconsig: "::" gtycon
#line 791 "parser.y"
                     {yylhs.value.as < expression_ref > () = Haskell::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2532 "parser.cc"
    break;

  case 108: // sigtype: ctype
#line 793 "parser.y"
                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2538 "parser.cc"
    break;

  case 109: // sigtypedoc: ctypedoc
#line 795 "parser.y"
                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2544 "parser.cc"
    break;

  case 110: // sig_vars: sig_vars "," var
#line 797 "parser.y"
                           {yylhs.value.as < std::vector<Haskell::Var> > () = yystack_[2].value.as < std::vector<Haskell::Var> > (); yylhs.value.as < std::vector<Haskell::Var> > ().push_back(Haskell::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}));}
#line 2550 "parser.cc"
    break;

  case 111: // sig_vars: var
#line 798 "parser.y"
                           {yylhs.value.as < std::vector<Haskell::Var> > ().push_back(Haskell::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}));}
#line 2556 "parser.cc"
    break;

  case 112: // sigtypes1: sigtype
#line 800 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2562 "parser.cc"
    break;

  case 113: // sigtypes1: sigtypes1 "," sigtype
#line 801 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2568 "parser.cc"
    break;

  case 114: // strict_mark: strictness
#line 805 "parser.y"
                                            {yylhs.value.as < Haskell::StrictLazy > () = yystack_[0].value.as < Haskell::StrictLazy > ();}
#line 2574 "parser.cc"
    break;

  case 115: // strictness: "!"
#line 811 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::strict;}
#line 2580 "parser.cc"
    break;

  case 116: // strictness: "~"
#line 812 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::lazy;}
#line 2586 "parser.cc"
    break;

  case 117: // ctype: "forall" tv_bndrs "." ctype
#line 819 "parser.y"
                                   {yylhs.value.as < expression_ref > () = Haskell::ForallType(yystack_[2].value.as < std::vector<Haskell::TypeVar> > (), yystack_[0].value.as < expression_ref > ());}
#line 2592 "parser.cc"
    break;

  case 118: // ctype: context "=>" ctype
#line 820 "parser.y"
                                   {yylhs.value.as < expression_ref > () = Haskell::ConstrainedType(yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ());}
#line 2598 "parser.cc"
    break;

  case 119: // ctype: type
#line 822 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2604 "parser.cc"
    break;

  case 120: // ctypedoc: ctype
#line 824 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2610 "parser.cc"
    break;

  case 121: // context: btype
#line 833 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(yystack_[0].value.as < expression_ref > ());}
#line 2616 "parser.cc"
    break;

  case 122: // context_no_ops: btype_no_ops
#line 835 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(Hs::make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2622 "parser.cc"
    break;

  case 123: // type: btype
#line 837 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2628 "parser.cc"
    break;

  case 124: // type: btype "->" ctype
#line 838 "parser.y"
                                   {yylhs.value.as < expression_ref > () = Hs::make_tyapps({Haskell::TypeCon({yystack_[1].location,"->"}),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()});}
#line 2634 "parser.cc"
    break;

  case 125: // typedoc: type
#line 840 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2640 "parser.cc"
    break;

  case 126: // btype: tyapps
#line 843 "parser.y"
                                   {yylhs.value.as < expression_ref > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2646 "parser.cc"
    break;

  case 127: // btype_no_ops: atype_docs
#line 845 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2652 "parser.cc"
    break;

  case 128: // btype_no_ops: btype_no_ops atype_docs
#line 846 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2658 "parser.cc"
    break;

  case 129: // tyapps: tyapp
#line 848 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2664 "parser.cc"
    break;

  case 130: // tyapps: tyapps tyapp
#line 849 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2670 "parser.cc"
    break;

  case 131: // tyapp: atype
#line 851 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2676 "parser.cc"
    break;

  case 132: // tyapp: qtyconop
#line 852 "parser.y"
                                   {yylhs.value.as < expression_ref > () = Haskell::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2682 "parser.cc"
    break;

  case 133: // tyapp: tyvarop
#line 853 "parser.y"
                                   {yylhs.value.as < expression_ref > () = Haskell::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2688 "parser.cc"
    break;

  case 134: // atype_docs: atype
#line 859 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2694 "parser.cc"
    break;

  case 135: // atype: ntgtycon
#line 866 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Haskell::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2700 "parser.cc"
    break;

  case 136: // atype: tyvar
#line 867 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Haskell::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2706 "parser.cc"
    break;

  case 137: // atype: "*"
#line 868 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Haskell::TypeCon({yystack_[0].location,"*"});}
#line 2712 "parser.cc"
    break;

  case 138: // atype: strict_mark atype
#line 869 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Haskell::StrictLazyType(yystack_[1].value.as < Haskell::StrictLazy > (),yystack_[0].value.as < expression_ref > ());}
#line 2718 "parser.cc"
    break;

  case 139: // atype: "{" fielddecls "}"
#line 870 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Haskell::FieldDecls(yystack_[1].value.as < std::vector<Haskell::FieldDecl> > ());}
#line 2724 "parser.cc"
    break;

  case 140: // atype: "(" ")"
#line 871 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Haskell::TypeCon({yystack_[1].location,"()"});}
#line 2730 "parser.cc"
    break;

  case 141: // atype: "(" comma_types1 "," ctype ")"
#line 872 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<expression_ref> > ();ts.push_back(yystack_[1].value.as < expression_ref > ());yylhs.value.as < expression_ref > () = Haskell::TupleType(ts);}
#line 2736 "parser.cc"
    break;

  case 142: // atype: "[" ctype "]"
#line 878 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Haskell::ListType{yystack_[1].value.as < expression_ref > ()}; }
#line 2742 "parser.cc"
    break;

  case 143: // atype: "(" ctype ")"
#line 879 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2748 "parser.cc"
    break;

  case 144: // inst_type: sigtype
#line 883 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2754 "parser.cc"
    break;

  case 147: // comma_types0: comma_types1
#line 888 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2760 "parser.cc"
    break;

  case 148: // comma_types0: %empty
#line 889 "parser.y"
                                       { /* default construction OK */ }
#line 2766 "parser.cc"
    break;

  case 149: // comma_types1: ctype
#line 891 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2772 "parser.cc"
    break;

  case 150: // comma_types1: comma_types1 "," ctype
#line 892 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2778 "parser.cc"
    break;

  case 151: // tv_bndrs: tv_bndrs tv_bndr
#line 899 "parser.y"
                               {yylhs.value.as < std::vector<Haskell::TypeVar> > () = yystack_[1].value.as < std::vector<Haskell::TypeVar> > (); yylhs.value.as < std::vector<Haskell::TypeVar> > ().push_back(yystack_[0].value.as < Haskell::TypeVar > ());}
#line 2784 "parser.cc"
    break;

  case 152: // tv_bndrs: %empty
#line 900 "parser.y"
                               { /* default construction OK */}
#line 2790 "parser.cc"
    break;

  case 153: // tv_bndr: tyvar
#line 903 "parser.y"
                                    {yylhs.value.as < Haskell::TypeVar > () = Haskell::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2796 "parser.cc"
    break;

  case 154: // tv_bndr: "(" tyvar "::" kind ")"
#line 904 "parser.y"
                                    {yylhs.value.as < Haskell::TypeVar > () = Haskell::TypeVar({yystack_[3].location,yystack_[3].value.as < std::string > ()},yystack_[1].value.as < expression_ref > ());}
#line 2802 "parser.cc"
    break;

  case 155: // kind: ctype
#line 922 "parser.y"
             {yylhs.value.as < expression_ref > () = make_kind(yystack_[0].value.as < expression_ref > ());}
#line 2808 "parser.cc"
    break;

  case 156: // constrs: "=" constrs1
#line 928 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::Constructor> > () = yystack_[0].value.as < std::vector<Haskell::Constructor> > ();}
#line 2814 "parser.cc"
    break;

  case 157: // constrs1: constrs1 "|" constr
#line 930 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::Constructor> > () = yystack_[2].value.as < std::vector<Haskell::Constructor> > (); yylhs.value.as < std::vector<Haskell::Constructor> > ().push_back(yystack_[0].value.as < Haskell::Constructor > ());}
#line 2820 "parser.cc"
    break;

  case 158: // constrs1: constr
#line 931 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::Constructor> > ().push_back(yystack_[0].value.as < Haskell::Constructor > ());}
#line 2826 "parser.cc"
    break;

  case 159: // constr: forall context_no_ops "=>" constr_stuff
#line 933 "parser.y"
                                                {yylhs.value.as < Haskell::Constructor > () = make_constructor(yystack_[3].value.as < std::vector<Haskell::TypeVar> > (),yystack_[2].value.as < Haskell::Context > (), yystack_[0].value.as < expression_ref > ());}
#line 2832 "parser.cc"
    break;

  case 160: // constr: forall constr_stuff
#line 934 "parser.y"
                                                {yylhs.value.as < Haskell::Constructor > () = make_constructor(yystack_[1].value.as < std::vector<Haskell::TypeVar> > (),{}, yystack_[0].value.as < expression_ref > ());}
#line 2838 "parser.cc"
    break;

  case 161: // forall: "forall" tv_bndrs "."
#line 936 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::TypeVar> > () = yystack_[1].value.as < std::vector<Haskell::TypeVar> > ();}
#line 2844 "parser.cc"
    break;

  case 162: // forall: %empty
#line 937 "parser.y"
                                {}
#line 2850 "parser.cc"
    break;

  case 163: // constr_stuff: btype_no_ops
#line 939 "parser.y"
                                                {yylhs.value.as < expression_ref > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2856 "parser.cc"
    break;

  case 164: // constr_stuff: btype_no_ops conop btype_no_ops
#line 940 "parser.y"
                                                {yylhs.value.as < expression_ref > () = Hs::make_tyapps({Haskell::TypeCon({yystack_[1].location,yystack_[1].value.as < std::string > ()}),Hs::make_tyapps(yystack_[2].value.as < std::vector<expression_ref> > ()),Hs::make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 2862 "parser.cc"
    break;

  case 165: // fielddecls: %empty
#line 942 "parser.y"
                                {}
#line 2868 "parser.cc"
    break;

  case 166: // fielddecls: fielddecls1
#line 943 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::FieldDecl> > () = yystack_[0].value.as < std::vector<Haskell::FieldDecl> > ();}
#line 2874 "parser.cc"
    break;

  case 167: // fielddecls1: fielddecls1 "," fielddecl
#line 945 "parser.y"
                                        {yylhs.value.as < std::vector<Haskell::FieldDecl> > () = yystack_[2].value.as < std::vector<Haskell::FieldDecl> > (); yylhs.value.as < std::vector<Haskell::FieldDecl> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2880 "parser.cc"
    break;

  case 168: // fielddecls1: fielddecl
#line 946 "parser.y"
                                        {yylhs.value.as < std::vector<Haskell::FieldDecl> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2886 "parser.cc"
    break;

  case 169: // fielddecl: sig_vars "::" ctype
#line 948 "parser.y"
                                        {yylhs.value.as < Haskell::FieldDecl > () = Haskell::FieldDecl(yystack_[2].value.as < std::vector<Haskell::Var> > (),yystack_[0].value.as < expression_ref > ());}
#line 2892 "parser.cc"
    break;

  case 180: // decl_no_th: sigdecl
#line 967 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2898 "parser.cc"
    break;

  case 181: // decl_no_th: "!" aexp rhs
#line 969 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::StrictValueDecl{yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}; }
#line 2904 "parser.cc"
    break;

  case 182: // decl_no_th: infixexp_top rhs
#line 970 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::ValueDecl(make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < Hs::MultiGuardedRHS > ());}
#line 2910 "parser.cc"
    break;

  case 183: // decl: decl_no_th
#line 972 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2916 "parser.cc"
    break;

  case 184: // rhs: "=" exp wherebinds
#line 976 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Haskell::SimpleRHS({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Haskell::Binds>> > ());}
#line 2922 "parser.cc"
    break;

  case 185: // rhs: gdrhs wherebinds
#line 977 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Haskell::MultiGuardedRHS{yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Haskell::Binds>> > ()};}
#line 2928 "parser.cc"
    break;

  case 186: // gdrhs: gdrhs gdrh
#line 979 "parser.y"
                              {yylhs.value.as < std::vector<Haskell::GuardedRHS> > () = yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (); yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 2934 "parser.cc"
    break;

  case 187: // gdrhs: gdrh
#line 980 "parser.y"
                              {yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 2940 "parser.cc"
    break;

  case 188: // gdrh: "|" guardquals "=" exp
#line 984 "parser.y"
                              {yylhs.value.as < Haskell::GuardedRHS > () = Haskell::GuardedRHS{yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ()};}
#line 2946 "parser.cc"
    break;

  case 189: // sigdecl: sig_vars "::" sigtypedoc
#line 994 "parser.y"
                                  { yylhs.value.as < expression_ref > () = Haskell::SignatureDecl{yystack_[2].value.as < std::vector<Haskell::Var> > (),yystack_[0].value.as < expression_ref > ()}; }
#line 2952 "parser.cc"
    break;

  case 190: // sigdecl: infix prec ops
#line 995 "parser.y"
                         { yylhs.value.as < expression_ref > () = Haskell::FixityDecl{yystack_[2].value.as < Haskell::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()}; }
#line 2958 "parser.cc"
    break;

  case 191: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 997 "parser.y"
                                                    {}
#line 2964 "parser.cc"
    break;

  case 192: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 998 "parser.y"
                                            {}
#line 2970 "parser.cc"
    break;

  case 193: // sigdecl: "{-# SCC" qvar "#-}"
#line 999 "parser.y"
                              {}
#line 2976 "parser.cc"
    break;

  case 194: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1000 "parser.y"
                                     {}
#line 2982 "parser.cc"
    break;

  case 195: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1001 "parser.y"
                                                               {}
#line 2988 "parser.cc"
    break;

  case 196: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1002 "parser.y"
                                                                      {}
#line 2994 "parser.cc"
    break;

  case 197: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1003 "parser.y"
                                                     {}
#line 3000 "parser.cc"
    break;

  case 202: // exp: infixexp "::" sigtype
#line 1014 "parser.y"
                           { yylhs.value.as < expression_ref > () = Hs::TypedExp(make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()); }
#line 3006 "parser.cc"
    break;

  case 203: // exp: infixexp
#line 1015 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 3012 "parser.cc"
    break;

  case 204: // infixexp: exp10
#line 1017 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3018 "parser.cc"
    break;

  case 205: // infixexp: infixexp qop exp10
#line 1018 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3024 "parser.cc"
    break;

  case 206: // infixexp_top: exp10_top
#line 1020 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3030 "parser.cc"
    break;

  case 207: // infixexp_top: infixexp_top qop exp10_top
#line 1021 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3036 "parser.cc"
    break;

  case 208: // exp10_top: "-" fexp
#line 1023 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_minus(make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 3042 "parser.cc"
    break;

  case 209: // exp10_top: "{-# CORE" "STRING" "#-}"
#line 1024 "parser.y"
                                   {}
#line 3048 "parser.cc"
    break;

  case 210: // exp10_top: fexp
#line 1025 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 3054 "parser.cc"
    break;

  case 211: // exp10: exp10_top
#line 1027 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3060 "parser.cc"
    break;

  case 212: // exp10: scc_annot exp
#line 1028 "parser.y"
                                 {}
#line 3066 "parser.cc"
    break;

  case 217: // fexp: fexp aexp
#line 1039 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3072 "parser.cc"
    break;

  case 218: // fexp: fexp "TYPEAPP" atype
#line 1040 "parser.y"
                                 {}
#line 3078 "parser.cc"
    break;

  case 219: // fexp: "static" aexp
#line 1041 "parser.y"
                                 {}
#line 3084 "parser.cc"
    break;

  case 220: // fexp: aexp
#line 1042 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3090 "parser.cc"
    break;

  case 221: // aexp: qvar "@" aexp
#line 1044 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Haskell::AsPattern(Haskell::Var({yystack_[2].location,yystack_[2].value.as < std::string > ()}),yystack_[0].value.as < expression_ref > ());}
#line 3096 "parser.cc"
    break;

  case 222: // aexp: "~" aexp
#line 1045 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Haskell::LazyPattern(yystack_[0].value.as < expression_ref > ());}
#line 3102 "parser.cc"
    break;

  case 223: // aexp: "\\" apats1 "->" exp
#line 1046 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Haskell::LambdaExp(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3108 "parser.cc"
    break;

  case 224: // aexp: "let" binds "in" exp
#line 1047 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Haskell::LetExp(yystack_[2].value.as < Located<Haskell::Binds> > (),{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 3114 "parser.cc"
    break;

  case 225: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1049 "parser.y"
                                                       {yylhs.value.as < expression_ref > () = Haskell::IfExp({yystack_[6].location,yystack_[6].value.as < expression_ref > ()},{yystack_[3].location,yystack_[3].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 3120 "parser.cc"
    break;

  case 226: // aexp: "case" exp "of" altslist
#line 1051 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Haskell::CaseExp(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < Haskell::Alts > ());}
#line 3126 "parser.cc"
    break;

  case 227: // aexp: "do" stmtlist
#line 1052 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Haskell::Do(yystack_[0].value.as < Haskell::Stmts > ());}
#line 3132 "parser.cc"
    break;

  case 228: // aexp: "mdo" stmtlist
#line 1053 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Haskell::MDo(yystack_[0].value.as < Haskell::Stmts > ());}
#line 3138 "parser.cc"
    break;

  case 229: // aexp: aexp1
#line 1055 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3144 "parser.cc"
    break;

  case 230: // aexp1: aexp1 "{" fbinds "}"
#line 1057 "parser.y"
                              {}
#line 3150 "parser.cc"
    break;

  case 231: // aexp1: aexp2
#line 1058 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3156 "parser.cc"
    break;

  case 232: // aexp2: qvar
#line 1060 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3162 "parser.cc"
    break;

  case 233: // aexp2: qcon
#line 1061 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3168 "parser.cc"
    break;

  case 234: // aexp2: literal
#line 1062 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3174 "parser.cc"
    break;

  case 235: // aexp2: "(" texp ")"
#line 1063 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3180 "parser.cc"
    break;

  case 236: // aexp2: "(" tup_exprs ")"
#line 1064 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::Tuple(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3186 "parser.cc"
    break;

  case 237: // aexp2: "[" list "]"
#line 1069 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3192 "parser.cc"
    break;

  case 238: // aexp2: "_"
#line 1070 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::WildcardPattern();}
#line 3198 "parser.cc"
    break;

  case 239: // texp: exp
#line 1075 "parser.y"
                      {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3204 "parser.cc"
    break;

  case 240: // texp: infixexp qop
#line 1076 "parser.y"
                      {yylhs.value.as < expression_ref > () = Haskell::LeftSection ( make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ()), yystack_[0].value.as < expression_ref > () ); }
#line 3210 "parser.cc"
    break;

  case 241: // texp: qopm infixexp
#line 1077 "parser.y"
                      {yylhs.value.as < expression_ref > () = Haskell::RightSection( yystack_[1].value.as < expression_ref > (), make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ()) ); }
#line 3216 "parser.cc"
    break;

  case 242: // tup_exprs: tup_exprs "," texp
#line 1082 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3222 "parser.cc"
    break;

  case 243: // tup_exprs: texp "," texp
#line 1083 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3228 "parser.cc"
    break;

  case 244: // list: texp
#line 1101 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Haskell::List{{yystack_[0].value.as < expression_ref > ()}}; }
#line 3234 "parser.cc"
    break;

  case 245: // list: lexps
#line 1102 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Haskell::List{yystack_[0].value.as < std::vector<expression_ref> > ()}; }
#line 3240 "parser.cc"
    break;

  case 246: // list: texp ".."
#line 1103 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Haskell::ListFrom(yystack_[1].value.as < expression_ref > ()); }
#line 3246 "parser.cc"
    break;

  case 247: // list: texp "," exp ".."
#line 1104 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Haskell::ListFromThen(yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()); }
#line 3252 "parser.cc"
    break;

  case 248: // list: texp ".." exp
#line 1105 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Haskell::ListFromTo(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()); }
#line 3258 "parser.cc"
    break;

  case 249: // list: texp "," exp ".." exp
#line 1106 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Haskell::ListFromThenTo(yystack_[4].value.as < expression_ref > (), yystack_[2].value.as < expression_ref > (), yystack_[0].value.as < expression_ref > ()); }
#line 3264 "parser.cc"
    break;

  case 250: // list: texp "|" squals
#line 1107 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Haskell::ListComprehension(yystack_[2].value.as < expression_ref > (), yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 3270 "parser.cc"
    break;

  case 251: // lexps: lexps "," texp
#line 1109 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3276 "parser.cc"
    break;

  case 252: // lexps: texp "," texp
#line 1110 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3282 "parser.cc"
    break;

  case 253: // squals: squals "," qual
#line 1123 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3288 "parser.cc"
    break;

  case 254: // squals: qual
#line 1125 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3294 "parser.cc"
    break;

  case 255: // guardquals: guardquals1
#line 1135 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3300 "parser.cc"
    break;

  case 256: // guardquals1: guardquals1 "," qual
#line 1137 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > ();yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3306 "parser.cc"
    break;

  case 257: // guardquals1: qual
#line 1138 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3312 "parser.cc"
    break;

  case 258: // altslist: "{" alts "}"
#line 1141 "parser.y"
                                 {yylhs.value.as < Haskell::Alts > () = Haskell::Alts{yystack_[1].value.as < std::vector<Located<Haskell::Alt>> > ()};}
#line 3318 "parser.cc"
    break;

  case 259: // altslist: "vocurly" alts close
#line 1142 "parser.y"
                                 {yylhs.value.as < Haskell::Alts > () = Haskell::Alts{yystack_[1].value.as < std::vector<Located<Haskell::Alt>> > ()};}
#line 3324 "parser.cc"
    break;

  case 260: // altslist: "{" "}"
#line 1143 "parser.y"
                                 {}
#line 3330 "parser.cc"
    break;

  case 261: // altslist: "vocurly" close
#line 1144 "parser.y"
                                 {}
#line 3336 "parser.cc"
    break;

  case 262: // alts: alts1
#line 1146 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[0].value.as < std::vector<Located<Haskell::Alt>> > ();}
#line 3342 "parser.cc"
    break;

  case 263: // alts: ";" alts
#line 1147 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[0].value.as < std::vector<Located<Haskell::Alt>> > ();}
#line 3348 "parser.cc"
    break;

  case 264: // alts1: alts1 ";" alt
#line 1149 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[2].value.as < std::vector<Located<Haskell::Alt>> > (); yylhs.value.as < std::vector<Located<Haskell::Alt>> > ().push_back(yystack_[0].value.as < Located<Haskell::Alt> > ());}
#line 3354 "parser.cc"
    break;

  case 265: // alts1: alts1 ";"
#line 1150 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[1].value.as < std::vector<Located<Haskell::Alt>> > ();}
#line 3360 "parser.cc"
    break;

  case 266: // alts1: alt
#line 1151 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > ().push_back(yystack_[0].value.as < Located<Haskell::Alt> > ());}
#line 3366 "parser.cc"
    break;

  case 267: // alt: pat alt_rhs
#line 1153 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Alt> > () = Located<Haskell::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 3372 "parser.cc"
    break;

  case 268: // alt_rhs: "->" exp wherebinds
#line 1155 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Haskell::SimpleRHS({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Haskell::Binds>> > ());}
#line 3378 "parser.cc"
    break;

  case 269: // alt_rhs: gdpats wherebinds
#line 1156 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Haskell::MultiGuardedRHS(yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Haskell::Binds>> > ());}
#line 3384 "parser.cc"
    break;

  case 270: // gdpats: gdpats gdpat
#line 1158 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::GuardedRHS> > () = yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (); yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 3390 "parser.cc"
    break;

  case 271: // gdpats: gdpat
#line 1159 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 3396 "parser.cc"
    break;

  case 272: // gdpat: "|" guardquals "->" exp
#line 1168 "parser.y"
                                 {yylhs.value.as < Haskell::GuardedRHS > ()=Haskell::GuardedRHS{yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ()};}
#line 3402 "parser.cc"
    break;

  case 273: // pat: exp
#line 1170 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3408 "parser.cc"
    break;

  case 274: // pat: "!" aexp
#line 1171 "parser.y"
              {yylhs.value.as < expression_ref > () = Haskell::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3414 "parser.cc"
    break;

  case 275: // bindpat: exp
#line 1173 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3420 "parser.cc"
    break;

  case 276: // bindpat: "!" aexp
#line 1174 "parser.y"
              {yylhs.value.as < expression_ref > () = Haskell::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3426 "parser.cc"
    break;

  case 277: // apat: aexp
#line 1176 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3432 "parser.cc"
    break;

  case 278: // apat: "!" aexp
#line 1177 "parser.y"
              {yylhs.value.as < expression_ref > () = Haskell::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3438 "parser.cc"
    break;

  case 279: // apats1: apats1 apat
#line 1179 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3444 "parser.cc"
    break;

  case 280: // apats1: apat
#line 1180 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3450 "parser.cc"
    break;

  case 281: // stmtlist: "{" stmts "}"
#line 1183 "parser.y"
                               {yylhs.value.as < Haskell::Stmts > () = Haskell::Stmts{yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 3456 "parser.cc"
    break;

  case 282: // stmtlist: "vocurly" stmts close
#line 1184 "parser.y"
                               {yylhs.value.as < Haskell::Stmts > () = Haskell::Stmts{yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 3462 "parser.cc"
    break;

  case 283: // stmts: stmts ";" stmt
#line 1186 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3468 "parser.cc"
    break;

  case 284: // stmts: stmts ";"
#line 1187 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3474 "parser.cc"
    break;

  case 285: // stmts: stmt
#line 1188 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3480 "parser.cc"
    break;

  case 286: // stmts: %empty
#line 1189 "parser.y"
                       {}
#line 3486 "parser.cc"
    break;

  case 287: // stmt: qual
#line 1194 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3492 "parser.cc"
    break;

  case 288: // stmt: "rec" stmtlist
#line 1195 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::RecStmt(yystack_[0].value.as < Haskell::Stmts > ());}
#line 3498 "parser.cc"
    break;

  case 289: // qual: bindpat "<-" exp
#line 1197 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::PatQual(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3504 "parser.cc"
    break;

  case 290: // qual: exp
#line 1198 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::SimpleQual(yystack_[0].value.as < expression_ref > ());}
#line 3510 "parser.cc"
    break;

  case 291: // qual: "let" binds
#line 1199 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::LetQual(yystack_[0].value.as < Located<Haskell::Binds> > ());}
#line 3516 "parser.cc"
    break;

  case 299: // qcon: gen_qcon
#line 1244 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3522 "parser.cc"
    break;

  case 300: // qcon: sysdcon
#line 1245 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3528 "parser.cc"
    break;

  case 301: // gen_qcon: qconid
#line 1247 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3534 "parser.cc"
    break;

  case 302: // gen_qcon: "(" qconsym ")"
#line 1248 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3540 "parser.cc"
    break;

  case 303: // con: conid
#line 1250 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3546 "parser.cc"
    break;

  case 304: // con: "(" consym ")"
#line 1251 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3552 "parser.cc"
    break;

  case 305: // con: sysdcon
#line 1252 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3558 "parser.cc"
    break;

  case 308: // sysdcon_no_list: "(" ")"
#line 1257 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 3564 "parser.cc"
    break;

  case 309: // sysdcon_no_list: "(" commas ")"
#line 1258 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3570 "parser.cc"
    break;

  case 310: // sysdcon_no_list: "(#" "#)"
#line 1259 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 3576 "parser.cc"
    break;

  case 311: // sysdcon_no_list: "(#" commas "#)"
#line 1260 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3582 "parser.cc"
    break;

  case 312: // sysdcon: sysdcon_no_list
#line 1262 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3588 "parser.cc"
    break;

  case 313: // sysdcon: "[" "]"
#line 1263 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 3594 "parser.cc"
    break;

  case 314: // conop: consym
#line 1265 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3600 "parser.cc"
    break;

  case 315: // conop: "`" conid "`"
#line 1266 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3606 "parser.cc"
    break;

  case 316: // qconop: qconsym
#line 1268 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3612 "parser.cc"
    break;

  case 317: // qconop: "`" qconid "`"
#line 1269 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3618 "parser.cc"
    break;

  case 318: // gtycon: ntgtycon
#line 1272 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3624 "parser.cc"
    break;

  case 319: // gtycon: "(" ")"
#line 1273 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 3630 "parser.cc"
    break;

  case 320: // gtycon: "(#" "#)"
#line 1274 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 3636 "parser.cc"
    break;

  case 321: // ntgtycon: oqtycon
#line 1276 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3642 "parser.cc"
    break;

  case 322: // ntgtycon: "(" commas ")"
#line 1277 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3648 "parser.cc"
    break;

  case 323: // ntgtycon: "(#" commas "#)"
#line 1278 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3654 "parser.cc"
    break;

  case 324: // ntgtycon: "(" "->" ")"
#line 1279 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 3660 "parser.cc"
    break;

  case 325: // ntgtycon: "[" "]"
#line 1280 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 3666 "parser.cc"
    break;

  case 326: // oqtycon: qtycon
#line 1282 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3672 "parser.cc"
    break;

  case 327: // oqtycon: "(" qtyconsym ")"
#line 1283 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3678 "parser.cc"
    break;

  case 328: // oqtycon: "(" "~" ")"
#line 1284 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3684 "parser.cc"
    break;

  case 329: // oqtycon_no_varcon: qtycon
#line 1286 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3690 "parser.cc"
    break;

  case 330: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1287 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3696 "parser.cc"
    break;

  case 331: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1288 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3702 "parser.cc"
    break;

  case 332: // oqtycon_no_varcon: "(" ":" ")"
#line 1289 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 3708 "parser.cc"
    break;

  case 333: // oqtycon_no_varcon: "(" "~" ")"
#line 1290 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3714 "parser.cc"
    break;

  case 334: // qtyconop: qtyconsym
#line 1293 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3720 "parser.cc"
    break;

  case 335: // qtyconop: "`" qtycon "`"
#line 1294 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3726 "parser.cc"
    break;

  case 336: // qtycondoc: qtycon
#line 1296 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3732 "parser.cc"
    break;

  case 337: // qtycon: "QCONID"
#line 1298 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3738 "parser.cc"
    break;

  case 338: // qtycon: tycon
#line 1299 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3744 "parser.cc"
    break;

  case 339: // tycon: "CONID"
#line 1303 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3750 "parser.cc"
    break;

  case 340: // qtyconsym: "QCONSYM"
#line 1305 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3756 "parser.cc"
    break;

  case 341: // qtyconsym: "QVARSYM"
#line 1306 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3762 "parser.cc"
    break;

  case 342: // qtyconsym: tyconsym
#line 1307 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3768 "parser.cc"
    break;

  case 343: // tyconsym: "CONSYM"
#line 1309 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3774 "parser.cc"
    break;

  case 344: // tyconsym: "VARSYM"
#line 1310 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3780 "parser.cc"
    break;

  case 345: // tyconsym: ":"
#line 1311 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 3786 "parser.cc"
    break;

  case 346: // tyconsym: "-"
#line 1312 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 3792 "parser.cc"
    break;

  case 347: // op: varop
#line 1317 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3798 "parser.cc"
    break;

  case 348: // op: conop
#line 1318 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3804 "parser.cc"
    break;

  case 349: // varop: varsym
#line 1320 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3810 "parser.cc"
    break;

  case 350: // varop: "`" varid "`"
#line 1321 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3816 "parser.cc"
    break;

  case 351: // qop: qvarop
#line 1323 "parser.y"
                { yylhs.value.as < expression_ref > () = Haskell::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3822 "parser.cc"
    break;

  case 352: // qop: qconop
#line 1324 "parser.y"
                { yylhs.value.as < expression_ref > () = Haskell::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3828 "parser.cc"
    break;

  case 353: // qopm: qvaropm
#line 1327 "parser.y"
                { yylhs.value.as < expression_ref > () = Haskell::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3834 "parser.cc"
    break;

  case 354: // qopm: qconop
#line 1328 "parser.y"
                { yylhs.value.as < expression_ref > () = Haskell::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3840 "parser.cc"
    break;

  case 355: // qvarop: qvarsym
#line 1333 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3846 "parser.cc"
    break;

  case 356: // qvarop: "`" qvarid "`"
#line 1334 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3852 "parser.cc"
    break;

  case 357: // qvaropm: qvarsym_no_minus
#line 1336 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 3858 "parser.cc"
    break;

  case 358: // qvaropm: "`" qvarid "`"
#line 1337 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3864 "parser.cc"
    break;

  case 359: // tyvar: tyvarid
#line 1341 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3870 "parser.cc"
    break;

  case 360: // tyvarop: "`" tyvarid "`"
#line 1343 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3876 "parser.cc"
    break;

  case 361: // tyvarid: "VARID"
#line 1345 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3882 "parser.cc"
    break;

  case 362: // tyvarid: special_id
#line 1346 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3888 "parser.cc"
    break;

  case 363: // tyvarid: "unsafe"
#line 1347 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 3894 "parser.cc"
    break;

  case 364: // tyvarid: "safe"
#line 1348 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 3900 "parser.cc"
    break;

  case 365: // tyvarid: "interruptible"
#line 1349 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 3906 "parser.cc"
    break;

  case 366: // var: varid
#line 1352 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3912 "parser.cc"
    break;

  case 367: // var: "(" varsym ")"
#line 1353 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3918 "parser.cc"
    break;

  case 368: // qvar: qvarid
#line 1355 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3924 "parser.cc"
    break;

  case 369: // qvar: "(" varsym ")"
#line 1356 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3930 "parser.cc"
    break;

  case 370: // qvar: "(" qvarsym1 ")"
#line 1357 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3936 "parser.cc"
    break;

  case 371: // qvarid: varid
#line 1359 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3942 "parser.cc"
    break;

  case 372: // qvarid: "QVARID"
#line 1360 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3948 "parser.cc"
    break;

  case 373: // varid: "VARID"
#line 1362 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3954 "parser.cc"
    break;

  case 374: // varid: special_id
#line 1363 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3960 "parser.cc"
    break;

  case 375: // varid: "unsafe"
#line 1364 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 3966 "parser.cc"
    break;

  case 376: // varid: "safe"
#line 1365 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 3972 "parser.cc"
    break;

  case 377: // varid: "interruptible"
#line 1366 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 3978 "parser.cc"
    break;

  case 378: // varid: "forall"
#line 1367 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 3984 "parser.cc"
    break;

  case 379: // varid: "family"
#line 1368 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 3990 "parser.cc"
    break;

  case 380: // varid: "role"
#line 1369 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 3996 "parser.cc"
    break;

  case 381: // qvarsym: varsym
#line 1371 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4002 "parser.cc"
    break;

  case 382: // qvarsym: qvarsym1
#line 1372 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4008 "parser.cc"
    break;

  case 383: // qvarsym_no_minus: varsym_no_minus
#line 1374 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4014 "parser.cc"
    break;

  case 384: // qvarsym_no_minus: qvarsym1
#line 1375 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4020 "parser.cc"
    break;

  case 385: // qvarsym1: "QVARSYM"
#line 1377 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4026 "parser.cc"
    break;

  case 386: // varsym: varsym_no_minus
#line 1379 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4032 "parser.cc"
    break;

  case 387: // varsym: "-"
#line 1380 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 4038 "parser.cc"
    break;

  case 388: // varsym_no_minus: "VARSYM"
#line 1382 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4044 "parser.cc"
    break;

  case 389: // varsym_no_minus: special_sym
#line 1383 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4050 "parser.cc"
    break;

  case 390: // special_id: "as"
#line 1385 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 4056 "parser.cc"
    break;

  case 391: // special_id: "qualified"
#line 1386 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 4062 "parser.cc"
    break;

  case 392: // special_id: "hiding"
#line 1387 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 4068 "parser.cc"
    break;

  case 393: // special_id: "export"
#line 1388 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 4074 "parser.cc"
    break;

  case 394: // special_id: "label"
#line 1389 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 4080 "parser.cc"
    break;

  case 395: // special_id: "dynamic"
#line 1390 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 4086 "parser.cc"
    break;

  case 396: // special_id: "stdcall"
#line 1391 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 4092 "parser.cc"
    break;

  case 397: // special_id: "ccall"
#line 1392 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 4098 "parser.cc"
    break;

  case 398: // special_id: "capi"
#line 1393 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 4104 "parser.cc"
    break;

  case 399: // special_id: "prim"
#line 1394 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 4110 "parser.cc"
    break;

  case 400: // special_id: "javascript"
#line 1395 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 4116 "parser.cc"
    break;

  case 401: // special_id: "group"
#line 1396 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 4122 "parser.cc"
    break;

  case 402: // special_id: "stock"
#line 1397 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 4128 "parser.cc"
    break;

  case 403: // special_id: "anyclass"
#line 1398 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 4134 "parser.cc"
    break;

  case 404: // special_id: "via"
#line 1399 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 4140 "parser.cc"
    break;

  case 405: // special_id: "unit"
#line 1400 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 4146 "parser.cc"
    break;

  case 406: // special_id: "dependency"
#line 1401 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 4152 "parser.cc"
    break;

  case 407: // special_id: "signature"
#line 1402 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 4158 "parser.cc"
    break;

  case 408: // special_sym: "!"
#line 1404 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 4164 "parser.cc"
    break;

  case 409: // special_sym: "."
#line 1405 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 4170 "parser.cc"
    break;

  case 410: // special_sym: "*"
#line 1406 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 4176 "parser.cc"
    break;

  case 411: // qconid: conid
#line 1410 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4182 "parser.cc"
    break;

  case 412: // qconid: "QCONID"
#line 1411 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4188 "parser.cc"
    break;

  case 413: // conid: "CONID"
#line 1413 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4194 "parser.cc"
    break;

  case 414: // qconsym: consym
#line 1415 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4200 "parser.cc"
    break;

  case 415: // qconsym: "QCONSYM"
#line 1416 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4206 "parser.cc"
    break;

  case 416: // consym: "CONSYM"
#line 1418 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4212 "parser.cc"
    break;

  case 417: // consym: ":"
#line 1419 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4218 "parser.cc"
    break;

  case 418: // literal: "CHAR"
#line 1423 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < char > ();}
#line 4224 "parser.cc"
    break;

  case 419: // literal: "STRING"
#line 1424 "parser.y"
                  {yylhs.value.as < expression_ref > () = yy_make_string(yystack_[0].value.as < std::string > ());}
#line 4230 "parser.cc"
    break;

  case 420: // literal: "INTEGER"
#line 1425 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < int > ();}
#line 4236 "parser.cc"
    break;

  case 421: // literal: "RATIONAL"
#line 1426 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < double > ();}
#line 4242 "parser.cc"
    break;

  case 423: // close: error
#line 1434 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 4248 "parser.cc"
    break;

  case 424: // modid: "CONID"
#line 1438 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4254 "parser.cc"
    break;

  case 425: // modid: "QCONID"
#line 1439 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4260 "parser.cc"
    break;

  case 426: // commas: commas ","
#line 1441 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 4266 "parser.cc"
    break;

  case 427: // commas: ","
#line 1442 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 4272 "parser.cc"
    break;


#line 4276 "parser.cc"

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


  const short parser::yypact_ninf_ = -565;

  const short parser::yytable_ninf_ = -387;

  const short
  parser::yypact_[] =
  {
      44,   -42,  -565,    73,  -565,  -565,  -565,  -565,  -565,   124,
     -12,   -19,  -565,    38,    46,    46,    47,  -565,  -565,  -565,
    -565,   132,  -565,  -565,  -565,    33,  -565,    99,   151,  3574,
     174,   211,   129,  -565,   596,  -565,   109,  -565,  -565,  -565,
    -565,   -42,  -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,
    -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,
    -565,  -565,  -565,  -565,   159,  -565,  -565,  -565,  -565,   154,
     157,  -565,   178,  -565,  -565,  -565,  -565,  -565,  -565,  -565,
     190,  -565,   -42,  -565,   177,  -565,  1965,  3197,  -565,   194,
     212,  1965,  -565,  -565,  -565,   293,   221,  -565,  3197,  3942,
     212,  2691,   209,   169,  3897,   163,  2328,  2691,  2449,  2691,
    1104,   976,   -15,  -565,  -565,  -565,  -565,  -565,  -565,    22,
     209,   205,   129,  -565,  -565,  -565,   248,    54,  -565,  -565,
    2068,  -565,  2570,  -565,   236,  -565,  -565,  -565,  -565,  -565,
    -565,   262,    65,  -565,  -565,  -565,  -565,   216,  -565,   243,
     249,  -565,  -565,  -565,  -565,  -565,   250,  -565,   252,   254,
     261,  -565,  -565,  -565,  3574,  3607,  -565,  -565,  -565,  -565,
     366,  -565,   -46,   976,   348,   328,  -565,  -565,  1965,  -565,
    -565,  -565,  -565,  -565,  -565,  -565,  -565,  4064,  2894,  2793,
     258,  3795,  -565,  -565,  -565,  -565,  -565,   346,  3482,  -565,
     285,  -565,   166,  3197,  -565,  -565,  -565,  -565,  -565,  -565,
    -565,  -565,  -565,  -565,  -565,  -565,  2995,  1481,  1481,  -565,
     263,   298,   300,   301,   302,  2995,   725,   725,  -565,   365,
     303,   295,   127,  4121,   253,   256,  -565,  -565,  -565,  -565,
     -20,  3897,  -565,   306,   202,    -2,   284,    -7,   275,   311,
    -565,  -565,  2691,  -565,  -565,  2207,  -565,  2570,   203,  -565,
    -565,  3736,  -565,  -565,  -565,   328,     6,   291,   283,  -565,
    1965,  -565,  -565,  -565,  -565,  -565,  -565,  2449,  -565,  -565,
     122,   123,   254,   290,   292,   294,   125,  -565,   131,  2995,
    3897,  3897,  -565,   101,   177,   271,  3197,  2995,  4064,  1965,
    1723,  3736,  -565,    31,  -565,  -565,  2086,  -565,  -565,  -565,
    -565,  3482,  -565,  3840,  2691,  -565,  -565,  -565,  -565,  -565,
    -565,  -565,  -565,   296,   305,   287,  -565,   316,    38,   -42,
      24,   326,   340,   227,  2995,  1965,  -565,    72,   321,   308,
    -565,  -565,  -565,   319,   335,  -565,   318,   320,  -565,   322,
     313,   325,   146,   143,   324,   327,   221,  -565,  -565,  3197,
    2995,  -565,  -565,   332,   330,   221,   212,  2691,   350,   360,
     -26,  -565,  -565,    42,  -565,   421,  -565,  -565,  -565,  -565,
    -565,  -565,   346,    64,  -565,  -565,  2068,    45,  1965,  2995,
     345,   339,   331,   333,   329,   352,   383,  -565,  -565,   386,
     356,   163,    63,   389,  -565,  1965,  -565,  -565,   353,   355,
    1965,  1965,  1723,  1232,  -565,  1232,   414,  -565,  1232,  -565,
    1232,    80,  -565,  -565,  -565,  -565,   390,   391,   392,  4031,
     361,  -565,  -565,  -565,  -565,     1,   224,  -565,  -565,  -565,
    -565,   346,   393,   362,  -565,   368,  -565,  -565,  -565,  -565,
    -565,   380,  -565,   370,   407,  -565,  -565,  -565,  3703,  -565,
    -565,  -565,   384,  3574,  -565,  -565,  -565,  -565,  1360,   855,
    -565,  -565,  -565,  2995,  -565,  4064,  4154,  -565,  2995,  -565,
    -565,  -565,  2995,  -565,  -565,  -565,  -565,  -565,  -565,  -565,
    -565,  -565,  2995,   365,  -565,  -565,  1965,  -565,  1481,  -565,
    1965,  -565,  -565,   725,  -565,  -565,  -565,  -565,  -565,   369,
     371,   396,  -565,  -565,  -565,  -565,  -565,   397,   442,   155,
    -565,  -565,  -565,  -565,  -565,  -565,  -565,   376,  -565,   416,
    -565,  -565,  -565,  -565,  -565,  2995,  2995,   381,   101,  -565,
     426,  2995,   478,  -565,   499,  -565,  1965,  1723,  -565,  -565,
    3840,  1232,  -565,  3574,   401,  2691,  -565,  1602,  -565,   410,
     400,  -565,   257,    38,  -565,  -565,  -565,  2995,  4213,  -565,
    -565,  -565,   406,  -565,  -565,  -565,   263,  -565,  -565,  -565,
    -565,   320,  -565,   325,  -565,  1723,  1965,  -565,    19,    27,
    -565,  -565,  -565,  -565,  -565,  -565,   431,  -565,  3482,    41,
    -565,   499,  -565,  -565,  -565,  -565,  -565,   408,  -565,  -565,
    -565,  -565,  1844,  1723,  1965,  -565,    37,  -565,  -565,  -565,
     437,  -565,   507,  -565,  -565,  -565,  2995,  -565,  4180,   478,
     430,  3298,  -565,  -565,  -565,  -565,  -565,  -565,  3096,    60,
     467,  -565,  -565,  -565,  -565,  -565,   436,   346,  -565,  -565,
    2995,  1965,  -565,  -565,  -565,  3482,   413,  -565,  3482,  -565,
    -565,   411,   427,  -565,  3197,  -565,  1965,  -565,   428,  -565,
    3390,  -565,  3482,  3197,  -565,  -565,  -565,  -565,  -565
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    36,     0,     2,    36,     4,   424,   425,     8,
       0,    39,     1,     0,     0,     0,    18,    11,    35,    13,
      16,    58,   423,   422,    12,   105,   101,     0,     0,     0,
       0,    42,    37,    15,    14,   104,     0,     6,     7,   390,
     392,     0,   391,   378,   393,   394,   395,   376,   377,   375,
     379,   380,   396,   397,   398,   399,   400,   401,   402,   403,
     404,   405,   407,   406,     0,   373,   339,   372,   337,     0,
      19,    21,    24,    32,   329,   338,    31,   368,   371,   374,
       0,    41,     0,    34,    38,   238,     0,     0,    83,     0,
       0,     0,    51,    52,    53,    78,     0,    84,     0,     0,
       0,     0,   198,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   413,   412,   418,   419,   420,   421,   198,
     198,    49,    56,    59,    60,    61,    91,     0,    63,   180,
      64,   206,   210,   220,   229,   231,   233,   299,   312,   300,
     111,   232,   371,   301,   411,   234,   102,     0,    23,     0,
       0,   387,   408,   410,   409,   388,     0,   385,     0,     0,
       0,   386,   389,    17,     0,    27,    22,    36,    36,     3,
      44,    33,     0,     0,     0,   203,   211,   204,     0,   364,
     365,   363,   345,   116,   346,   115,   137,   165,     0,     0,
       0,     0,   361,   344,   343,   341,   340,   100,     0,   114,
       0,    88,   123,   126,   129,   131,   135,   321,   132,   326,
     334,   342,   136,   133,   359,   362,   148,   286,   286,   227,
     214,     0,     0,     0,     0,     0,    95,    95,    98,     0,
       0,   123,     0,     0,     0,     0,   366,   349,   228,   219,
       0,     0,   199,     0,     0,     0,     0,     0,   306,   106,
     305,   303,     0,   277,   280,     0,   222,   208,     0,   417,
     313,     0,   416,   415,   239,   203,   244,     0,   245,   354,
       0,   353,   357,   384,   383,   316,   414,   387,   308,   427,
       0,     0,   384,     0,   383,   316,     0,   310,     0,     0,
       0,     0,    50,     0,    57,     0,     0,     0,     0,     0,
       0,     0,   182,   100,   187,   352,     0,   351,   355,   382,
     381,     0,   217,   293,     0,   103,   332,   333,   331,   330,
     370,   369,    20,     0,     0,    28,    30,     0,     0,     0,
      46,     0,     0,     0,     0,     0,   212,     0,     0,   166,
     168,   152,   325,     0,     0,   119,     0,   116,   140,   149,
       0,   334,     0,     0,     0,     0,     0,    69,   138,     0,
       0,   130,   149,     0,   147,     0,     0,     0,   290,     0,
       0,   285,   287,     0,   213,     0,    75,    74,    76,    77,
     144,   108,   100,     0,   183,    94,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   209,   193,     0,
       0,     0,     0,     0,   278,     0,   279,   181,     0,     0,
     240,   246,     0,     0,   237,     0,   241,   235,     0,   236,
       0,   369,   302,   309,   426,   311,     0,     0,     0,     0,
     190,   348,    55,   347,   314,     0,    85,   189,   120,   109,
     110,   100,     0,   255,   257,     0,   185,   186,   207,   218,
     296,     0,   292,   295,   298,   221,    26,    25,     0,     9,
      10,    43,     0,     0,    40,    45,   216,   215,     0,     0,
     226,   202,   205,     0,   139,     0,     0,   142,     0,   324,
     328,   143,     0,   327,   322,   323,   335,   360,    99,    87,
     124,    62,     0,   291,   288,   276,     0,   281,   284,   282,
       0,    73,    96,    93,    97,   224,    70,   367,   350,    68,
      66,     0,   200,   192,   194,   304,   307,     0,     0,     0,
     107,   318,   191,   223,   358,   317,   248,   250,   254,   239,
     252,   251,   243,   242,   197,     0,     0,     0,     0,    90,
       0,     0,   162,    72,   170,   184,     0,     0,   356,   230,
       0,     0,    29,     0,     0,     0,   260,     0,   273,     0,
     262,   266,     0,     0,   261,   169,   167,     0,     0,   151,
     153,   118,   150,   150,   289,   283,   214,    92,    67,    65,
     201,     0,   319,     0,   320,     0,   247,   112,     0,     0,
     315,    54,    89,   155,    86,   152,   156,   158,     0,     0,
      71,   171,   173,   188,   256,   294,   297,     0,    47,   274,
     263,   258,   265,     0,     0,   267,   100,   271,   259,   117,
       0,   141,     0,   253,   249,   195,     0,   196,     0,   162,
       0,   163,   127,   134,   160,    81,    79,    80,     0,     0,
     174,   177,   336,   172,    48,   264,     0,   100,   269,   270,
       0,     0,   113,   161,   157,     0,     0,   128,     0,   178,
     125,   145,     0,   175,     0,   176,     0,   268,     0,   225,
     163,   159,   164,     0,   179,    82,   272,   154,   146
  };

  const short
  parser::yypgoto_[] =
  {
    -565,  -565,  -565,  -565,  -565,  -565,  -565,    39,  -565,  -565,
    -403,  -565,   375,  -565,  -565,  -565,  -148,   419,  -565,  -565,
    -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,
    -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,
     247,  -565,   317,  -565,  -181,  -284,   531,  -565,  -565,  -309,
    -565,  -167,    12,  -565,  -565,  -155,   160,   -64,  -565,   -84,
    -565,   -91,  -360,  -565,   357,  -499,  -193,   269,  -122,  -565,
     343,   -32,  -565,   -83,  -565,  -565,   -61,  -565,   -81,  -565,
    -565,    96,  -565,  -565,   -25,   -60,   541,    74,   323,  -565,
     280,  -565,   196,  -565,   -59,   -82,   551,   -30,  -288,    14,
    -565,   -77,   -53,  -565,  -565,   -65,  -565,  -565,  -565,  -565,
     -17,  -565,  -565,  -429,  -565,   -24,  -565,  -565,   -21,  -565,
    -565,   336,  -565,   -78,   379,    95,  -289,  -565,    48,  -565,
    -565,  -565,  -565,   193,  -565,   -92,  -564,  -109,  -565,   215,
    -565,  -565,  -565,  -565,   -29,  -565,  -179,  -565,    67,   512,
    -149,  -565,  -565,  -565,  -441,  -565,   432,   -69,   -27,  -204,
     -18,  -565,  -565,   -22,   -49,   -73,   -86,  -565,  -191,   -99,
     -62,  -238,  -565,  -304,   -23,  -104
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   169,     6,    10,    19,    30,
      69,    70,    71,   166,   324,   325,    72,    84,    11,    20,
      21,    32,    82,   330,   464,   465,   293,   121,   430,    33,
      34,   122,   123,   124,   125,   225,   639,   665,   126,   543,
     197,   296,   383,   228,   229,   357,    27,    36,   403,   380,
     437,   127,   588,   198,   199,   381,   439,   344,   630,   345,
     661,   202,   631,   203,   204,   632,   205,   382,   662,   363,
     350,   476,   569,   594,   544,   596,   597,   598,   634,   338,
     339,   340,   600,   601,   602,   640,   384,   385,   302,   303,
     304,   129,   241,   242,   368,   175,   386,   176,   177,   375,
     178,   132,   133,   134,   135,   280,   281,   267,   268,   527,
     442,   443,   470,   559,   560,   561,   615,   616,   617,   562,
     369,   254,   255,   219,   370,   371,   372,   451,   452,   453,
     136,   137,   248,   249,   138,   139,   431,   269,   520,   206,
     207,    73,   208,   641,   209,    75,   210,   211,   432,   433,
     306,   270,   307,   271,   212,   213,   214,   140,   141,    77,
      78,   308,   272,   273,   310,   161,    79,   162,   143,   144,
     275,   276,   145,    24,     9,   286
  };

  const short
  parser::yytable_[] =
  {
      74,   215,    76,   201,   131,   358,   251,   231,   288,   400,
     351,   444,   215,   250,   230,   160,   142,   326,   148,   446,
     337,   305,   238,   200,   460,   471,   335,   174,   265,   265,
     235,   257,   220,   343,   349,   570,   462,   274,   284,    22,
     563,   289,   159,    22,    13,   266,    22,   472,   239,   285,
     237,   264,   264,   253,   256,   434,   258,   408,   356,   170,
     554,   362,   283,   635,   356,     1,   305,   658,   394,   499,
     409,   331,   259,    12,   497,   398,     7,   245,   539,   312,
       8,   236,   332,   504,   411,   352,   353,   498,    17,   282,
     412,   265,   636,   637,    18,   287,   625,   445,   501,   279,
     284,   278,   215,   215,   627,   215,   658,   279,   309,   395,
     409,   285,   215,   262,   264,   300,   410,   215,   449,   336,
     413,   613,   472,   528,   160,   240,   399,   620,   610,   540,
     215,   463,   657,   626,   297,    74,    74,    76,    76,   215,
      23,   626,   438,     2,    23,  -366,    31,    23,   638,    25,
     607,   282,   473,   309,    29,   498,   305,   545,   503,    66,
    -367,    35,   354,    68,   502,   564,   517,   638,   298,   236,
     518,   657,   519,   657,    26,   488,    37,   503,    66,  -366,
     259,    66,    68,   390,   493,    68,   298,   570,   416,    14,
      15,   151,   152,   153,  -367,   160,   131,   131,   154,   404,
     257,    80,   253,   215,   312,   490,   327,   328,   142,   142,
     215,   215,   201,   146,   396,   391,   429,   151,   152,   153,
     155,   262,   159,   147,   154,   215,   587,   587,    38,   440,
     417,   419,   200,   423,   438,    81,   418,   420,   149,   424,
     441,   425,    83,   309,   237,   424,   155,   150,   215,   151,
     152,   153,   360,   485,   484,  -121,   154,   424,   604,   618,
     424,   455,   163,   427,   428,   584,   246,   335,   231,   279,
     247,   164,   112,   215,   215,   489,   448,   305,   155,   156,
     236,   113,   157,   158,   299,   165,   454,   300,   494,   167,
     171,   168,   151,   152,   153,   670,   623,   243,   672,   154,
     434,   216,   251,   215,   541,   542,   461,   305,   337,   250,
     552,   217,   240,   218,   495,   290,   291,   652,   565,   295,
     226,   155,   227,   571,   444,   157,   468,   572,   469,   505,
     537,   265,   648,   265,   292,   313,   265,   573,   265,   583,
     274,   613,   274,   614,   315,   274,   523,   274,   530,   314,
     531,   316,   526,   532,   529,   533,   264,   317,   318,   264,
     319,   264,   320,   667,   309,   221,   222,   223,   224,   321,
     329,   333,   279,   356,   359,   376,   374,   377,   378,   379,
     388,   360,   392,   397,   389,   393,   593,   215,   260,   401,
     215,   402,   215,   434,   309,   414,   215,   415,   421,   435,
    -386,   458,   422,   466,   456,   633,   215,   259,   334,   558,
     558,   391,   619,   457,   352,   353,   459,   467,   151,   152,
     153,   474,   475,   477,   478,   154,   479,   482,   480,    74,
     481,    76,   434,   483,    74,  -275,    76,   574,   633,   486,
     491,   576,   487,   301,   492,   496,   500,   155,   262,   215,
     215,   157,   263,   507,   508,   215,   512,   236,   511,   509,
     513,   510,   633,   514,   515,   633,   522,   534,   524,   265,
     525,   535,   536,   131,   546,   538,   547,   633,   274,   633,
     549,   215,   215,   548,   550,   142,   606,   603,   551,   237,
     585,   553,   264,   259,   586,   593,   590,   578,   558,   579,
     580,   342,   609,   592,   151,   152,   153,   595,   599,   608,
     611,   154,   215,   612,   621,   629,   644,   650,   651,   655,
     664,   182,   666,   454,    74,   673,    76,   624,   346,   301,
     581,   113,   184,   155,   262,   674,   677,   157,   263,   322,
     215,   294,   215,   436,   387,   215,    28,   231,   589,   506,
     582,   678,   215,   558,   660,   647,   279,   537,   426,   364,
     361,   193,   194,   628,   215,   195,   196,   668,   654,   215,
     642,   566,   215,   231,   671,   128,   643,   577,   215,   663,
     675,   407,   231,   447,   215,   130,   215,   215,   645,   660,
     622,   406,   669,   575,   516,   649,   646,   373,   605,    85,
      39,    86,    87,    88,    89,   591,    90,   676,    40,    91,
     642,   234,    92,    93,    94,    95,    96,   521,    97,     0,
      42,     0,    98,   355,    99,    43,     0,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,    54,    55,
      56,     0,     0,    57,     0,     0,   101,    58,    59,    60,
      61,    62,    63,   102,     0,     0,     0,     0,   103,   104,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   105,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,   107,     0,   108,   109,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,   111,     0,   112,     0,     0,     0,     0,
       0,     0,     0,    65,   113,     0,     0,    67,   114,     0,
       0,     0,     0,   115,   116,   117,   118,     0,    85,    39,
      86,     0,     0,   119,   120,    90,     0,    40,    91,     0,
       0,    92,    93,    94,     0,    96,     0,     0,     0,    42,
       0,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,    54,    55,    56,
       0,     0,    57,     0,     0,   101,    58,    59,    60,    61,
      62,    63,   102,     0,     0,     0,     0,   103,   104,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   105,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,   107,     0,   108,   109,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   110,     0,
       0,     0,   111,     0,   112,     0,     0,     0,     0,     0,
       0,     0,    65,   113,     0,     0,    67,   114,     0,     0,
       0,     0,   115,   116,   117,   118,    22,     0,    85,    39,
      86,     0,   119,   120,     0,    90,     0,    40,    91,     0,
       0,     0,     0,     0,     0,    96,     0,     0,     0,    42,
       0,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,    54,    55,    56,
       0,     0,    57,     0,     0,   101,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,   103,   172,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,   107,     0,   108,   555,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    23,   110,     0,
       0,     0,   173,     0,   112,     0,     0,     0,   557,     0,
       0,     0,    65,   113,     0,     0,    67,   114,     0,    85,
      39,    86,   115,   116,   117,   118,    90,     0,    40,    91,
       0,     0,     0,     0,     0,     0,    96,     0,     0,     0,
      42,     0,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,    54,    55,
      56,     0,     0,    57,     0,     0,   101,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,   103,   172,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   259,     0,     0,   106,     0,
       0,     0,     0,     0,   107,     0,   277,   152,   153,     0,
       0,     0,     0,   154,     0,     0,     0,     0,     0,   110,
       0,     0,     0,   173,   278,   112,     0,     0,     0,     0,
     279,   261,     0,    65,   113,   155,   262,    67,   114,   157,
     263,     0,     0,   115,   116,   117,   118,    85,    39,    86,
       0,     0,     0,     0,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,    42,     0,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,   103,   172,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   259,     0,     0,   106,     0,     0,     0,
       0,     0,   107,     0,   108,   152,   153,     0,     0,     0,
       0,   154,     0,     0,     0,     0,     0,   110,   260,     0,
       0,   173,     0,   112,     0,     0,     0,     0,     0,   261,
       0,    65,   113,   155,   262,    67,   114,   157,   263,     0,
       0,   115,   116,   117,   118,    85,    39,    86,     0,     0,
       0,     0,    90,     0,    40,    91,     0,     0,     0,     0,
       0,     0,    96,     0,     0,     0,    42,     0,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   100,
      50,    51,    52,    53,    54,    55,    56,     0,     0,    57,
       0,     0,   101,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,   103,   172,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   259,     0,     0,   106,     0,     0,     0,     0,     0,
     107,     0,   108,   152,   153,     0,     0,     0,     0,   154,
       0,     0,     0,     0,     0,   110,     0,     0,     0,   173,
       0,   112,     0,     0,     0,     0,     0,   261,     0,    65,
     113,   155,   262,    67,   114,   157,   263,     0,     0,   115,
     116,   117,   118,    85,    39,    86,     0,     0,     0,     0,
      90,     0,    40,    91,     0,     0,     0,     0,     0,     0,
      96,     0,     0,     0,    42,     0,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,    54,    55,    56,     0,     0,    57,     0,     0,
     101,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,   103,   172,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,   107,     0,
     108,   555,     0,     0,     0,     0,     0,     0,     0,     0,
     556,     0,     0,   110,     0,     0,     0,   173,     0,   112,
       0,     0,     0,   557,     0,     0,     0,    65,   113,     0,
       0,    67,   114,     0,    85,    39,    86,   115,   116,   117,
     118,    90,     0,    40,    91,     0,     0,     0,     0,     0,
       0,   365,     0,     0,     0,    42,     0,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,    54,    55,    56,     0,   366,    57,     0,
       0,   101,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,   103,   172,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,   107,
       0,   108,   367,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   110,     0,     0,     0,   173,     0,
     112,     0,     0,     0,     0,     0,     0,     0,    65,   113,
       0,     0,    67,   114,     0,    85,    39,    86,   115,   116,
     117,   118,    90,     0,    40,    91,     0,     0,     0,     0,
       0,     0,    96,     0,     0,     0,    42,     0,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   100,
      50,    51,    52,    53,    54,    55,    56,     0,     0,    57,
       0,     0,   101,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,   103,   172,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
     107,     0,   108,   555,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,   173,
       0,   112,     0,     0,     0,   557,     0,     0,     0,    65,
     113,     0,     0,    67,   114,     0,    85,    39,    86,   115,
     116,   117,   118,    90,     0,    40,    91,     0,     0,     0,
       0,     0,     0,   365,     0,     0,     0,    42,     0,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,    54,    55,    56,     0,     0,
      57,     0,     0,   101,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,   103,   172,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,   107,     0,   108,   367,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
     173,     0,   112,     0,     0,     0,     0,     0,     0,     0,
      65,   113,     0,     0,    67,   114,     0,    85,    39,    86,
     115,   116,   117,   118,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,    42,     0,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,   103,   172,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,   107,     0,   108,   555,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   110,     0,     0,
       0,   173,     0,   112,     0,     0,     0,     0,     0,     0,
       0,    65,   113,     0,     0,    67,   114,     0,    85,    39,
      86,   115,   116,   117,   118,    90,     0,    40,    91,     0,
       0,     0,     0,     0,     0,    96,     0,     0,     0,    42,
       0,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,    54,    55,    56,
       0,     0,    57,     0,     0,   101,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,   103,   172,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,   107,     0,   108,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   110,     0,
       0,     0,   173,     0,   112,     0,     0,     0,     0,     0,
       0,     0,    65,   113,     0,     0,    67,   114,     0,    85,
      39,    86,   115,   116,   117,   118,    90,     0,    40,    91,
       0,     0,     0,     0,     0,     0,    96,     0,     0,     0,
      42,     0,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,    54,    55,
      56,     0,     0,    57,     0,     0,   101,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,   259,   103,   299,
       0,     0,   300,     0,     0,     0,     0,     0,   151,   152,
     153,     0,     0,     0,     0,   154,     0,     0,   106,     0,
       0,     0,     0,     0,   107,     0,   108,     0,     0,     0,
       0,     0,     0,   301,     0,     0,     0,   155,   262,   110,
       0,   157,   263,   173,     0,   112,     0,     0,     0,     0,
       0,     0,     0,    65,   113,     0,     0,    67,   114,     0,
      85,    39,    86,   115,   116,   117,   118,    90,     0,    40,
      91,     0,     0,     0,     0,     0,     0,    96,     0,     0,
       0,    42,     0,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,   405,     0,   107,     0,     0,   252,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,   173,     0,   112,     0,     0,     0,
       0,     0,     0,     0,    65,   113,     0,     0,    67,   114,
       0,    85,    39,    86,   115,   116,   117,   118,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,    42,     0,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,   107,     0,     0,   252,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,     0,   173,     0,   112,     0,     0,
       0,     0,     0,     0,     0,    65,   113,     0,     0,    67,
     114,     0,    85,    39,    86,   115,   116,   117,   118,    90,
       0,    40,    91,     0,     0,     0,     0,     0,     0,    96,
       0,     0,     0,    42,     0,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,    54,    55,    56,     0,     0,    57,     0,     0,   101,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,   107,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,   173,     0,   112,     0,
       0,     0,     0,     0,     0,     0,    65,   113,     0,     0,
      67,   114,     0,    85,    39,    86,   115,   116,   117,   118,
      90,     0,    40,    91,     0,     0,     0,     0,     0,     0,
      96,     0,     0,     0,    42,     0,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   311,     0,
       0,     0,     0,   110,     0,     0,     0,   173,     0,   112,
       0,     0,     0,     0,     0,     0,     0,    65,   113,     0,
       0,    67,   114,     0,    85,    39,    86,   115,   116,   117,
     118,    90,     0,    40,    91,     0,     0,     0,     0,     0,
       0,    96,     0,     0,     0,    42,     0,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,   107,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   110,     0,     0,    39,   173,     0,
     112,     0,     0,     0,     0,    40,     0,     0,    65,   113,
       0,     0,    67,   114,     0,     0,     0,    42,   115,   116,
     117,   118,   341,     0,    44,    45,    46,   179,   180,   181,
       0,     0,     0,    52,    53,    54,    55,    56,     0,     0,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   182,     0,     0,     0,     0,     0,     0,   346,
       0,   347,     0,   184,   185,   186,     0,     0,     0,     0,
       0,     0,   187,     0,     0,     0,   188,     0,    39,     0,
     189,   348,   190,     0,     0,     0,    40,   279,   191,     0,
     192,    66,   193,   194,     0,    68,   195,   196,    42,     0,
       0,     0,     0,   341,     0,    44,    45,    46,   179,   180,
     181,     0,     0,     0,    52,    53,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   182,     0,     0,     0,     0,     0,     0,
       0,     0,   183,     0,   184,   185,   186,     0,     0,     0,
       0,     0,     0,   187,     0,     0,     0,   188,   342,    39,
       0,   189,     0,   190,     0,     0,     0,    40,     0,   191,
       0,   192,    66,   193,   194,     0,    68,   195,   196,    42,
       0,     0,     0,     0,   341,     0,    44,    45,    46,   179,
     180,   181,     0,     0,     0,    52,    53,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   182,     0,     0,     0,     0,     0,
       0,     0,     0,   183,     0,   184,   185,   186,     0,     0,
       0,     0,     0,     0,   187,     0,     0,     0,   188,     0,
      39,     0,   189,     0,   190,     0,     0,     0,    40,     0,
     191,     0,   192,    66,   193,   194,     0,    68,   195,   196,
      42,     0,     0,     0,     0,     0,     0,    44,    45,    46,
     179,   180,   181,     0,     0,     0,    52,    53,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   182,     0,     0,     0,     0,
       0,     0,     0,     0,   183,     0,   184,   185,   186,     0,
       0,     0,     0,     0,     0,   187,     0,     0,     0,   188,
       0,    39,     0,   189,   659,   190,     0,     0,     0,    40,
       0,   191,     0,   192,    66,   193,   194,     0,    68,   195,
     196,    42,     0,     0,     0,     0,     0,     0,    44,    45,
      46,   179,   180,   181,     0,     0,     0,    52,    53,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   182,     0,     0,     0,
       0,     0,     0,     0,     0,   183,     0,   184,   185,   186,
       0,     0,     0,     0,     0,     0,   187,     0,     0,     0,
     188,     0,    39,     0,   189,     0,   190,     0,     0,     0,
      40,     0,   191,     0,   192,    66,   193,   194,     0,    68,
     195,   196,    42,     0,     0,     0,     0,     0,     0,    44,
      45,    46,   179,   180,   181,     0,     0,     0,    52,    53,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   259,     0,     0,
       0,     0,     0,     0,     0,     0,   183,  -122,     0,   185,
     186,     0,     0,     0,    39,     0,     0,   187,     0,     0,
       0,   188,    40,     0,     0,   189,     0,   190,     0,     0,
       0,     0,     0,   656,    42,   192,    66,     0,   262,     0,
      68,    44,    45,    46,   179,   180,   181,     0,     0,     0,
      52,    53,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   259,
       0,     0,     0,     0,     0,     0,     0,     0,   183,     0,
       0,   185,   186,     0,     0,     0,    39,     0,     0,   187,
       0,     0,     0,   188,    40,     0,     0,   189,     0,   190,
       0,     0,     0,     0,     0,   656,    42,   192,    66,     0,
     262,     0,    68,    44,    45,    46,   179,   180,   181,     0,
       0,     0,    52,    53,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     183,     0,     0,   185,   186,     0,     0,     0,    39,     0,
       0,   187,     0,     0,     0,   188,    40,     0,     0,   189,
       0,   190,     0,     0,     0,    41,     0,     0,    42,   192,
      66,     0,     0,    43,    68,    44,    45,    46,    47,    48,
      49,    39,    50,    51,    52,    53,    54,    55,    56,    40,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,    42,     0,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,     0,    50,    51,    52,    53,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,     0,     0,     0,   323,     0,     0,     0,     0,
       0,    65,    66,     0,     0,    67,    68,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    39,     0,     0,
       0,     0,     0,     0,    64,    40,     0,     0,     0,     0,
       0,     0,     0,     0,    65,    66,     0,    42,    67,    68,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
      39,    50,    51,    52,    53,    54,    55,    56,    40,     0,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
      42,     0,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,     0,    50,    51,    52,    53,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,    39,
       0,     0,     0,     0,     0,     0,     0,    40,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     0,     0,    42,
      65,    66,     0,     0,    67,    68,    44,    45,    46,   179,
     180,   181,     0,     0,     0,    52,    53,    54,    55,    56,
       0,     0,    57,     0,    39,     0,    58,    59,    60,    61,
      62,    63,    40,    65,   113,     0,     0,    67,   114,     0,
       0,     0,     0,     0,    42,     0,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,     0,    50,    51,
      52,    53,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,    39,     0,     0,     0,     0,     0,     0,     0,    40,
       0,     0,   192,    66,     0,     0,     0,    68,   450,     0,
       0,    42,     0,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,     0,    50,    51,    52,    53,    54,
      55,    56,     0,     0,    57,     0,    39,   244,    58,    59,
      60,    61,    62,    63,    40,     0,     0,    65,     0,     0,
       0,    67,     0,     0,     0,     0,    42,     0,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,     0,
      50,    51,    52,    53,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,   244,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    65,     0,     0,     0,    67,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   151,   152,   153,    39,     0,     0,     0,   154,
       0,     0,     0,    40,     0,     0,     0,     0,     0,   232,
       0,     0,     0,     0,     0,    42,     0,   233,     0,    65,
      43,   155,    44,    45,    46,    47,    48,    49,    39,    50,
      51,    52,    53,    54,    55,    56,    40,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,    42,     0,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,     0,    50,    51,    52,    53,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,    39,     0,     0,     0,     0,
       0,     0,     0,    40,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    42,     0,     0,    65,   113,
      43,     0,    44,    45,    46,    47,    48,    49,    39,    50,
      51,    52,    53,    54,    55,    56,    40,     0,    57,     0,
       0,   232,    58,    59,    60,    61,    62,    63,    42,     0,
       0,    65,     0,     0,    39,    44,    45,    46,   179,   180,
     181,     0,    40,     0,    52,    53,    54,    55,    56,     0,
       0,    57,     0,     0,    42,    58,    59,    60,    61,    62,
      63,    44,    45,    46,   179,   180,   181,    39,     0,     0,
      52,    53,    54,    55,    56,    40,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,    42,    65,     0,
       0,     0,     0,     0,    44,    45,    46,   179,   180,   181,
       0,   567,     0,    52,    53,    54,    55,    56,     0,     0,
      57,   568,     0,     0,    58,    59,    60,    61,    62,    63,
       0,   192,     0,     0,     0,     0,     0,   653,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   568,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   192,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     192
  };

  const short
  parser::yycheck_[] =
  {
      29,    87,    29,    87,    34,   198,   105,    98,   112,   247,
     189,   300,    98,   105,    98,    64,    34,   165,    41,   303,
     187,   130,   100,    87,   328,   334,   175,    86,   110,   111,
      99,   108,    91,   188,   189,   476,    12,   110,   111,     1,
     469,    19,    64,     1,     5,   110,     1,   335,   101,   111,
      99,   110,   111,   106,   107,   293,   109,   261,    27,    82,
     463,   216,   111,    22,    27,    21,   175,   631,    88,   373,
     261,   117,    79,     0,   100,    77,   118,   104,    77,   132,
     122,    99,   128,   387,    78,   189,   190,   113,   100,   111,
      84,   173,    51,    52,   113,   110,    77,   301,   382,   114,
     173,   108,   188,   189,    77,   191,   670,   114,   130,   129,
     301,   173,   198,   120,   173,    84,   265,   203,   311,   178,
     114,    84,   410,   412,   173,   103,   128,   568,   557,   128,
     216,   107,   631,   114,    80,   164,   165,   164,   165,   225,
     102,   114,   297,    99,   102,    80,    14,   102,   107,   103,
     553,   173,    80,   175,   107,   113,   265,   441,   113,   118,
      80,   128,   191,   122,   100,   469,   103,   107,   114,   187,
     107,   670,   109,   672,   128,   356,    77,   113,   118,   114,
      79,   118,   122,   232,   365,   122,   114,   628,   270,    65,
      66,    90,    91,    92,   114,   244,   226,   227,    97,   252,
     277,    27,   255,   289,   257,   360,   167,   168,   226,   227,
     296,   297,   296,   104,   241,   233,   115,    90,    91,    92,
     119,   120,   244,   114,    97,   311,   535,   536,    77,   298,
     108,   108,   296,   108,   389,    24,   114,   114,    79,   114,
     299,   110,   113,   265,   293,   114,   119,    88,   334,    90,
      91,    92,    86,   110,   108,    89,    97,   114,   547,   563,
     114,   314,   108,   290,   291,   110,   103,   416,   359,   114,
     107,   114,   109,   359,   360,   359,   306,   386,   119,   120,
     298,   118,   123,   124,    81,   107,   313,    84,   366,    99,
     113,   101,    90,    91,    92,   655,   585,   128,   658,    97,
     538,   107,   401,   389,    80,    81,   329,   416,   475,   401,
     458,    99,   103,   101,   367,   119,   120,   626,   473,    71,
      99,   119,   101,   478,   613,   123,    99,   482,   101,   388,
     429,   413,   616,   415,   129,    99,   418,   492,   420,   518,
     413,    84,   415,    86,   128,   418,   405,   420,   413,    87,
     415,   108,   411,   418,   413,   420,   415,   108,   108,   418,
     108,   420,   108,   647,   386,    72,    73,    74,    75,   108,
       4,    23,   114,    27,    89,    77,   113,    77,    77,    77,
      15,    86,   129,    77,    81,   129,   541,   473,   104,   114,
     476,    80,   478,   631,   416,   104,   482,   114,   108,   128,
     108,   114,   108,    77,   108,   598,   492,    79,    80,   468,
     469,   429,   567,   108,   518,   519,   100,    77,    90,    91,
      92,   100,   114,   104,    89,    97,   108,   114,   108,   458,
     108,   458,   670,   108,   463,    85,   463,   496,   631,   115,
     108,   500,   115,   115,   114,    85,    25,   119,   120,   535,
     536,   123,   124,   108,   115,   541,   104,   475,   129,   128,
      77,   128,   655,    77,   108,   658,    77,    77,   115,   551,
     115,    80,    80,   503,    81,   114,   114,   670,   551,   672,
     100,   567,   568,   115,   114,   503,   551,   546,    81,   538,
     114,   107,   551,    79,    78,   650,   115,   128,   557,   128,
     104,   104,   555,    77,    90,    91,    92,    29,     9,   108,
     100,    97,   598,   113,   108,    84,   108,    80,    11,    89,
      53,    79,    86,   550,   553,   114,   553,   586,    86,   115,
      88,   118,    90,   119,   120,   108,   108,   123,   124,   164,
     626,   122,   628,   296,   227,   631,    15,   638,   536,   389,
     108,   673,   638,   612,   638,   614,   114,   656,   289,   216,
     203,   119,   120,   595,   650,   123,   124,   650,   629,   655,
     599,   475,   658,   664,   655,    34,   601,   503,   664,   639,
     664,   258,   673,   303,   670,    34,   672,   673,   612,   673,
     576,   255,   651,   498,   401,   616,   613,   218,   550,     3,
       4,     5,     6,     7,     8,   538,    10,   666,    12,    13,
     639,    99,    16,    17,    18,    19,    20,   402,    22,    -1,
      24,    -1,    26,   191,    28,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,
      -1,    -1,    -1,   127,   128,   129,   130,    -1,     3,     4,
       5,    -1,    -1,   137,   138,    10,    -1,    12,    13,    -1,
      -1,    16,    17,    18,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    -1,    -1,    -1,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    76,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,
      -1,    -1,   127,   128,   129,   130,     1,    -1,     3,     4,
       5,    -1,   137,   138,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
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
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
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
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,
      -1,    97,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,   115,
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
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,   115,    -1,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,   127,
     128,   129,   130,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     100,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,
      -1,    -1,    -1,   113,    -1,    -1,    -1,   117,   118,    -1,
      -1,   121,   122,    -1,     3,     4,     5,   127,   128,   129,
     130,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    46,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,
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
      -1,   109,    -1,    -1,    -1,   113,    -1,    -1,    -1,   117,
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
      -1,    -1,    -1,    88,    -1,    90,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,     3,
       4,     5,   127,   128,   129,   130,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    79,    62,    81,
      -1,    -1,    84,    -1,    -1,    -1,    -1,    -1,    90,    91,
      92,    -1,    -1,    -1,    -1,    97,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    -1,    -1,    -1,
      -1,    -1,    -1,   115,    -1,    -1,    -1,   119,   120,   103,
      -1,   123,   124,   107,    -1,   109,    -1,    -1,    -1,    -1,
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
     103,    -1,     4,    -1,   107,    -1,   109,    -1,    -1,    -1,
      12,    -1,   115,    -1,   117,   118,   119,   120,    -1,   122,
     123,   124,    24,    -1,    -1,    -1,    -1,    -1,    -1,    31,
      32,    33,    34,    35,    36,    -1,    -1,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    89,    -1,    91,
      92,    -1,    -1,    -1,     4,    -1,    -1,    99,    -1,    -1,
      -1,   103,    12,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,    -1,    -1,   115,    24,   117,   118,    -1,   120,    -1,
     122,    31,    32,    33,    34,    35,    36,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      -1,    91,    92,    -1,    -1,    -1,     4,    -1,    -1,    99,
      -1,    -1,    -1,   103,    12,    -1,    -1,   107,    -1,   109,
      -1,    -1,    -1,    -1,    -1,   115,    24,   117,   118,    -1,
     120,    -1,   122,    31,    32,    33,    34,    35,    36,    -1,
      -1,    -1,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    91,    92,    -1,    -1,    -1,     4,    -1,
      -1,    99,    -1,    -1,    -1,   103,    12,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    21,    -1,    -1,    24,   117,
     118,    -1,    -1,    29,   122,    31,    32,    33,    34,    35,
      36,     4,    38,    39,    40,    41,    42,    43,    44,    12,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    -1,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   107,    -1,    -1,    -1,    78,    -1,    -1,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,   107,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,   118,    -1,    24,   121,   122,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
       4,    38,    39,    40,    41,    42,    43,    44,    12,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    -1,    38,    39,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,
     117,   118,    -1,    -1,   121,   122,    31,    32,    33,    34,
      35,    36,    -1,    -1,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,     4,    -1,    51,    52,    53,    54,
      55,    56,    12,   117,   118,    -1,    -1,   121,   122,    -1,
      -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    -1,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      -1,    -1,   117,   118,    -1,    -1,    -1,   122,    78,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    -1,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,     4,   107,    51,    52,
      53,    54,    55,    56,    12,    -1,    -1,   117,    -1,    -1,
      -1,   121,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,
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
       0,    21,    99,   140,   141,   142,   145,   118,   122,   313,
     146,   157,     0,   146,    65,    66,   143,   100,   113,   147,
     158,   159,     1,   102,   312,   103,   128,   185,   185,   107,
     148,    14,   160,   168,   169,   128,   186,    77,    77,     4,
      12,    21,    24,    29,    31,    32,    33,    34,    35,    36,
      38,    39,    40,    41,    42,    43,    44,    47,    51,    52,
      53,    54,    55,    56,   107,   117,   118,   121,   122,   149,
     150,   151,   155,   280,   283,   284,   297,   298,   299,   305,
      27,    24,   161,   113,   156,     3,     5,     6,     7,     8,
      10,    13,    16,    17,    18,    19,    20,    22,    26,    28,
      37,    50,    57,    62,    63,    76,    82,    88,    90,    91,
     103,   107,   109,   118,   122,   127,   128,   129,   130,   137,
     138,   166,   170,   171,   172,   173,   177,   190,   225,   230,
     235,   236,   240,   241,   242,   243,   269,   270,   273,   274,
     296,   297,   299,   307,   308,   311,   104,   114,   313,    79,
      88,    90,    91,    92,    97,   119,   120,   123,   124,   302,
     303,   304,   306,   108,   114,   107,   152,    99,   101,   144,
     313,   113,    63,   107,   233,   234,   236,   237,   239,    34,
      35,    36,    79,    88,    90,    91,    92,    99,   103,   107,
     109,   115,   117,   119,   120,   123,   124,   179,   192,   193,
     196,   198,   200,   202,   203,   205,   278,   279,   281,   283,
     285,   286,   293,   294,   295,   305,   107,    99,   101,   262,
     233,    72,    73,    74,    75,   174,    99,   101,   182,   183,
     198,   200,   107,   115,   288,   296,   299,   303,   262,   241,
     103,   231,   232,   128,   107,   297,   103,   107,   271,   272,
     274,   308,    91,   241,   260,   261,   241,   240,   241,    79,
     104,   115,   120,   124,   233,   234,   244,   246,   247,   276,
     290,   292,   301,   302,   304,   309,   310,    90,   108,   114,
     244,   245,   302,   303,   304,   309,   314,   110,   314,    19,
     231,   231,   129,   165,   156,    71,   180,    80,   114,    81,
      84,   115,   227,   228,   229,   276,   289,   291,   300,   302,
     303,    98,   241,    99,    87,   128,   108,   108,   108,   108,
     108,   108,   151,    78,   153,   154,   155,   146,   146,     4,
     162,   117,   128,    23,    80,   289,   233,   190,   218,   219,
     220,    29,   104,   194,   196,   198,    86,    88,   108,   194,
     209,   285,   314,   314,   283,   295,    27,   184,   205,    89,
      86,   203,   194,   208,   209,    20,    46,    91,   233,   259,
     263,   264,   265,   263,   113,   238,    77,    77,    77,    77,
     188,   194,   206,   181,   225,   226,   235,   181,    15,    81,
     303,   299,   129,   129,    88,   129,   297,    77,    77,   128,
     310,   114,    80,   187,   241,    86,   260,   227,   298,   307,
     289,    78,    84,   114,   104,   114,   234,   108,   114,   108,
     114,   108,   108,   108,   114,   110,   206,   297,   297,   115,
     167,   275,   287,   288,   310,   128,   179,   189,   194,   195,
     296,   233,   249,   250,   265,   298,   184,   229,   236,   205,
      78,   266,   267,   268,   297,   241,   108,   108,   114,   100,
     312,   313,    12,   107,   163,   164,    77,    77,    99,   101,
     251,   188,   237,    80,   100,   114,   210,   104,    89,   108,
     108,   108,   114,   108,   108,   110,   115,   115,   183,   198,
     194,   108,   114,   183,   262,   241,    85,   100,   113,   312,
      25,   184,   100,   113,   312,   233,   195,   108,   115,   128,
     128,   129,   104,    77,    77,   108,   272,   103,   107,   109,
     277,   278,    77,   233,   115,   115,   233,   248,   265,   233,
     244,   244,   244,   244,    77,    80,    80,   308,   114,    77,
     128,    80,    81,   178,   213,   184,    81,   114,   115,   100,
     114,    81,   155,   107,   149,    91,   100,   113,   233,   252,
     253,   254,   258,   252,   312,   194,   220,    97,   107,   211,
     293,   194,   194,   194,   233,   264,   233,   226,   128,   128,
     104,    88,   108,   285,   110,   114,    78,   188,   191,   191,
     115,   287,    77,   194,   212,    29,   214,   215,   216,     9,
     221,   222,   223,   233,   265,   267,   244,   149,   108,   241,
     252,   100,   113,    84,    86,   255,   256,   257,   312,   194,
     293,   108,   238,   265,   233,    77,   114,    77,   210,    84,
     197,   201,   204,   205,   217,    22,    51,    52,   107,   175,
     224,   282,   283,   223,   108,   254,   249,   233,   184,   257,
      80,    11,   188,    97,   215,    89,   115,   204,   275,   108,
     198,   199,   207,   224,    53,   176,    86,   184,   212,   233,
     201,   217,   201,   114,   108,   198,   233,   108,   207
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
     170,   170,   170,   170,   170,   170,   170,   170,   170,   171,
     172,   172,   172,   173,   174,   174,   174,   174,   174,   175,
     175,   175,   176,   177,   177,   178,   178,   179,   179,   180,
     180,   180,   181,   181,   181,   181,   182,   182,   183,   184,
     184,   185,   185,   186,   186,   186,   187,   187,   188,   189,
     190,   190,   191,   191,   192,   193,   193,   194,   194,   194,
     195,   196,   197,   198,   198,   199,   200,   201,   201,   202,
     202,   203,   203,   203,   204,   205,   205,   205,   205,   205,
     205,   205,   205,   205,   206,   207,   207,   208,   208,   209,
     209,   210,   210,   211,   211,   212,   213,   214,   214,   215,
     215,   216,   216,   217,   217,   218,   218,   219,   219,   220,
     221,   221,   222,   222,   223,   223,   223,   224,   224,   224,
     225,   225,   225,   226,   227,   227,   228,   228,   229,   230,
     230,   230,   230,   230,   230,   230,   230,   230,   231,   231,
     232,   232,   233,   233,   234,   234,   235,   235,   236,   236,
     236,   237,   237,   238,   238,   239,   239,   240,   240,   240,
     240,   241,   241,   241,   241,   241,   241,   241,   241,   241,
     242,   242,   243,   243,   243,   243,   243,   243,   243,   244,
     244,   244,   245,   245,   246,   246,   246,   246,   246,   246,
     246,   247,   247,   248,   248,   249,   250,   250,   251,   251,
     251,   251,   252,   252,   253,   253,   253,   254,   255,   255,
     256,   256,   257,   258,   258,   259,   259,   260,   260,   261,
     261,   262,   262,   263,   263,   263,   263,   264,   264,   265,
     265,   265,   266,   266,   267,   267,   267,   268,   268,   269,
     269,   270,   270,   271,   271,   271,   272,   272,   273,   273,
     273,   273,   274,   274,   275,   275,   276,   276,   277,   277,
     277,   278,   278,   278,   278,   278,   279,   279,   279,   280,
     280,   280,   280,   280,   281,   281,   282,   283,   283,   284,
     285,   285,   285,   286,   286,   286,   286,   287,   287,   288,
     288,   289,   289,   290,   290,   291,   291,   292,   292,   293,
     294,   295,   295,   295,   295,   295,   296,   296,   297,   297,
     297,   298,   298,   299,   299,   299,   299,   299,   299,   299,
     299,   300,   300,   301,   301,   302,   303,   303,   304,   304,
     305,   305,   305,   305,   305,   305,   305,   305,   305,   305,
     305,   305,   305,   305,   305,   305,   305,   305,   306,   306,
     306,   307,   307,   308,   309,   309,   310,   310,   311,   311,
     311,   311,   312,   312,   313,   313,   314,   314
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
       1,     1,     4,     1,     1,     5,     4,     5,     4,     3,
       4,     5,     4,     4,     2,     2,     2,     2,     0,     1,
       1,     1,     2,     1,     1,     0,     2,     3,     1,     4,
       3,     0,     3,     2,     1,     0,     3,     3,     1,     2,
       0,     1,     3,     3,     1,     0,     0,     2,     1,     1,
       3,     1,     1,     3,     1,     1,     1,     4,     3,     1,
       1,     1,     1,     1,     3,     1,     1,     1,     2,     1,
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
       1,     1,     1,     1,     1,     1,     2,     1
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
       0,   476,   476,   493,   494,   496,   500,   501,   502,   504,
     505,   507,   508,   511,   513,   514,   515,   523,   524,   526,
     528,   529,   531,   532,   534,   535,   536,   538,   539,   541,
     542,   544,   545,   549,   550,   552,   553,   555,   557,   558,
     560,   573,   574,   576,   577,   579,   580,   584,   585,   590,
     591,   593,   594,   595,   597,   598,   602,   604,   605,   607,
     608,   609,   612,   619,   621,   622,   623,   624,   625,   627,
     629,   630,   631,   636,   641,   642,   643,   644,   645,   647,
     648,   649,   651,   691,   692,   694,   695,   704,   705,   707,
     708,   709,   753,   754,   755,   756,   758,   759,   761,   763,
     764,   772,   773,   775,   776,   777,   790,   791,   793,   795,
     797,   798,   800,   801,   805,   811,   812,   819,   820,   822,
     824,   833,   835,   837,   838,   840,   843,   845,   846,   848,
     849,   851,   852,   853,   859,   866,   867,   868,   869,   870,
     871,   872,   878,   879,   883,   885,   886,   888,   889,   891,
     892,   899,   900,   903,   904,   922,   928,   930,   931,   933,
     934,   936,   937,   939,   940,   942,   943,   945,   946,   948,
     950,   951,   953,   954,   956,   957,   958,   960,   961,   962,
     967,   969,   970,   972,   976,   977,   979,   980,   984,   994,
     995,   997,   998,   999,  1000,  1001,  1002,  1003,  1006,  1007,
    1009,  1010,  1014,  1015,  1017,  1018,  1020,  1021,  1023,  1024,
    1025,  1027,  1028,  1031,  1032,  1034,  1035,  1039,  1040,  1041,
    1042,  1044,  1045,  1046,  1047,  1049,  1051,  1052,  1053,  1055,
    1057,  1058,  1060,  1061,  1062,  1063,  1064,  1069,  1070,  1075,
    1076,  1077,  1082,  1083,  1101,  1102,  1103,  1104,  1105,  1106,
    1107,  1109,  1110,  1123,  1125,  1135,  1137,  1138,  1141,  1142,
    1143,  1144,  1146,  1147,  1149,  1150,  1151,  1153,  1155,  1156,
    1158,  1159,  1168,  1170,  1171,  1173,  1174,  1176,  1177,  1179,
    1180,  1183,  1184,  1186,  1187,  1188,  1189,  1194,  1195,  1197,
    1198,  1199,  1204,  1205,  1207,  1208,  1209,  1211,  1212,  1244,
    1245,  1247,  1248,  1250,  1251,  1252,  1254,  1255,  1257,  1258,
    1259,  1260,  1262,  1263,  1265,  1266,  1268,  1269,  1272,  1273,
    1274,  1276,  1277,  1278,  1279,  1280,  1282,  1283,  1284,  1286,
    1287,  1288,  1289,  1290,  1293,  1294,  1296,  1298,  1299,  1303,
    1305,  1306,  1307,  1309,  1310,  1311,  1312,  1317,  1318,  1320,
    1321,  1323,  1324,  1327,  1328,  1333,  1334,  1336,  1337,  1341,
    1343,  1345,  1346,  1347,  1348,  1349,  1352,  1353,  1355,  1356,
    1357,  1359,  1360,  1362,  1363,  1364,  1365,  1366,  1367,  1368,
    1369,  1371,  1372,  1374,  1375,  1377,  1379,  1380,  1382,  1383,
    1385,  1386,  1387,  1388,  1389,  1390,  1391,  1392,  1393,  1394,
    1395,  1396,  1397,  1398,  1399,  1400,  1401,  1402,  1404,  1405,
    1406,  1410,  1411,  1413,  1415,  1416,  1418,  1419,  1423,  1424,
    1425,  1426,  1431,  1434,  1438,  1439,  1441,  1442
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
#line 6020 "parser.cc"

#line 1451 "parser.y"


using std::optional;
using std::string;
using std::vector;
using std::pair;

void
yy::parser::error (const location_type& l, const std::string& m)
{
    drv.push_error_message({l,m});
}

pair<vector<Haskell::ImpDecl>, optional<Haskell::Decls>> make_body(const std::vector<Haskell::ImpDecl>& imports, const std::optional<Haskell::Decls>& topdecls)
{
    if (topdecls)
    {
        auto topdecls2 = Haskell::Decls(*topdecls);
        return {imports, topdecls2};
    }
    else
        return {imports, {}};
}

// See PostProcess.hs:checkTyClHdr
std::tuple<string, vector<expression_ref>>
check_type_or_class_header(expression_ref type)
{
    auto [type_head, type_args] = Haskell::decompose_type_apps(type);

    // FIXME -- add location!
    if (not type_head.is_a<Haskell::TypeCon>())
        throw myexception()<<"Malformed type or class header '"<<type<<"'";
    auto name = unloc(type_head.as_<Haskell::TypeCon>().name);

    return {name, type_args};
}

vector<Haskell::TypeVar> check_all_type_vars(const vector<Haskell::Type>& types)
{
    vector<Haskell::TypeVar> type_vars;
    for(auto& type: types)
    {
        if (type.is_a<Haskell::TypeVar>())
        {
            type_vars.push_back(type.as_<Haskell::TypeVar>());
        }
        else
        {
            throw myexception()<<"Type '"<<type<<"' is not a type variable";
        }
    }
    return type_vars;
}

Haskell::TypeSynonymDecl make_type_synonym(const Located<expression_ref>& lhs_type, const Located<expression_ref>& rhs_type)
{
    auto [name, type_args] = check_type_or_class_header(unloc(lhs_type));
    return {name, check_all_type_vars(type_args), rhs_type};
}

Haskell::DataOrNewtypeDecl make_data_or_newtype(const Haskell::DataOrNewtype& d_or_n, const Haskell::Context&  context,
                                                const expression_ref& header, const vector<Haskell::Constructor>& constrs)
{
    auto [name, type_args] = check_type_or_class_header(header);
    if (d_or_n == Haskell::DataOrNewtype::newtype and constrs.size() != 1)
        throw myexception()<<"newtype '"<<name<<"' may only have 1 constructors with 1 field";
    return {d_or_n, name, check_all_type_vars(type_args), context, constrs};
}

Haskell::InstanceDecl make_instance_decl(const Located<expression_ref>& ltype, const optional<Located<Haskell::Binds>>& binds)
{
    // GHC stores the instance as a polytype?
    // This would seem to allow (instance forall a.Eq a => forall a.Eq [a] x y ....)

    auto type = unloc(ltype);
    if (type.is_a<Haskell::ForallType>())
        throw myexception()<<"instance declaration '"<<type<<"' is malformed";
    Haskell::Context context;
    if (type.is_a<Haskell::ConstrainedType>())
    {
        auto& T = type.as_<Haskell::ConstrainedType>();
        context = T.context;
        type = T.type;
    }

    auto [name, type_args] = check_type_or_class_header(type);
    return {context, name, type_args, make_opt_decls(binds)};
}

Haskell::ClassDecl make_class_decl(const Haskell::Context& context, const expression_ref& header, const optional<Located<Haskell::Binds>>& binds)
{
    auto [name, type_args] = check_type_or_class_header(header);
    return {name, check_all_type_vars(type_args), context, make_opt_decls(binds)};
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

bool check_kind(const Haskell::Kind& kind)
{
    auto [kind_head, kind_args] = Haskell::decompose_type_apps(kind);

    if (not kind_head.is_a<Haskell::TypeCon>()) return false;

    auto V = kind_head.as_<Haskell::TypeCon>();
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

Haskell::Type make_kind(const Haskell::Kind& kind)
{
    if (not check_kind(kind))
        throw myexception()<<"Kind '"<<kind<<"' is malformed";

    return kind;
}

optional<pair<string, Haskell::FieldDecls>> is_record_con(const expression_ref& typeish)
{
    auto [head,args] = Haskell::decompose_type_apps(typeish);

    if (args.size() != 1) return {};

    if (not head.is_a<Haskell::TypeCon>()) return {};

    if (not args[0].is_a<Haskell::FieldDecls>()) return {};

    return {{unloc(head.as_<Haskell::TypeCon>().name), args[0].as_<Haskell::FieldDecls>()}};
}

optional<pair<string, std::vector<expression_ref>>> is_normal_con(const expression_ref& typeish)
{
    if (is_record_con(typeish)) return {};

    auto [head,args] = Haskell::decompose_type_apps(typeish);

    if (not head.is_a<Haskell::TypeCon>())
        return {};

    return {{unloc(head.as_<Haskell::TypeCon>().name), args}};
}

Haskell::Constructor make_constructor(const vector<Haskell::TypeVar>& forall, const std::optional<Haskell::Context>& c, const expression_ref& typeish)
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

expression_ref make_infixexp(const vector<expression_ref>& args)
{
    if (args.size() == 1)
	return args[0];
    else
	return Hs::InfixExp(args);
}


expression_ref make_minus(const expression_ref& exp)
{
    return Hs::InfixExp({Hs::Neg(),exp});
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

expression_ref yy_make_string(const std::string& s)
{
    vector<expression_ref> chars;
    for(char c: s)
	chars.push_back(c);
    return Haskell::List(chars);
}

std::optional<Located<Hs::Decls>> make_opt_decls(const std::optional<Located<Hs::Binds>>& binds)
{
    std::optional<Located<Hs::Decls>> decls;
    if (binds)
        decls = {binds->loc, unloc(*binds)[0]};
    return decls;
}

