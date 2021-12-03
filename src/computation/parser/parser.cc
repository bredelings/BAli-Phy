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
#line 54 "parser.y"

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

      case symbol_kind::S_alt: // alt
        value.YY_MOVE_OR_COPY< Located<Haskell::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
        value.YY_MOVE_OR_COPY< Located<Haskell::Decls> > (YY_MOVE (that.value));
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

      case symbol_kind::S_export_subspec: // export_subspec
        value.YY_MOVE_OR_COPY< std::optional<Haskell::ExportSubSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.YY_MOVE_OR_COPY< std::optional<Haskell::ImpSpec> > (YY_MOVE (that.value));
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

      case symbol_kind::S_alt: // alt
        value.move< Located<Haskell::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
        value.move< Located<Haskell::Decls> > (YY_MOVE (that.value));
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

      case symbol_kind::S_export_subspec: // export_subspec
        value.move< std::optional<Haskell::ExportSubSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.move< std::optional<Haskell::ImpSpec> > (YY_MOVE (that.value));
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

      case symbol_kind::S_alt: // alt
        value.copy< Located<Haskell::Alt> > (that.value);
        break;

      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
        value.copy< Located<Haskell::Decls> > (that.value);
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

      case symbol_kind::S_export_subspec: // export_subspec
        value.copy< std::optional<Haskell::ExportSubSpec> > (that.value);
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.copy< std::optional<Haskell::ImpSpec> > (that.value);
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

      case symbol_kind::S_alt: // alt
        value.move< Located<Haskell::Alt> > (that.value);
        break;

      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
        value.move< Located<Haskell::Decls> > (that.value);
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

      case symbol_kind::S_export_subspec: // export_subspec
        value.move< std::optional<Haskell::ExportSubSpec> > (that.value);
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.move< std::optional<Haskell::ImpSpec> > (that.value);
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

      case symbol_kind::S_alt: // alt
        yylhs.value.emplace< Located<Haskell::Alt> > ();
        break;

      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
        yylhs.value.emplace< Located<Haskell::Decls> > ();
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

      case symbol_kind::S_export_subspec: // export_subspec
        yylhs.value.emplace< std::optional<Haskell::ExportSubSpec> > ();
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        yylhs.value.emplace< std::optional<Haskell::ImpSpec> > ();
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
#line 477 "parser.y"
             {drv.result = yystack_[0].value.as < Haskell::Module > ();}
#line 2046 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 494 "parser.y"
                                                                 {yylhs.value.as < Haskell::Module > () = Haskell::Module{yystack_[4].value.as < std::string > (),yystack_[2].value.as < std::optional<std::vector<Haskell::Export>> > (),yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ().second};}
#line 2052 "parser.cc"
    break;

  case 4: // module: body2
#line 495 "parser.y"
                                                                 {yylhs.value.as < Haskell::Module > () = Haskell::Module{"Main",{},yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ().second};}
#line 2058 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 497 "parser.y"
                                                                 {drv.push_module_context();}
#line 2064 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 505 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 2070 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 506 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 2076 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 508 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 2082 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 509 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 2088 "parser.cc"
    break;

  case 13: // top: semis top1
#line 512 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 2094 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 514 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Haskell::ImpDecl> > (),yystack_[0].value.as < Haskell::Decls > ());}
#line 2100 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 515 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Haskell::ImpDecl> > (),yystack_[0].value.as < Haskell::Decls > ());}
#line 2106 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 516 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Haskell::ImpDecl> > (),{});}
#line 2112 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 524 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Haskell::Export>> > () = yystack_[1].value.as < std::vector<Haskell::Export> > ();}
#line 2118 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 525 "parser.y"
                                      {}
#line 2124 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 527 "parser.y"
                                      {yylhs.value.as < std::vector<Haskell::Export> > () = yystack_[0].value.as < std::vector<Haskell::Export> > ();}
#line 2130 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 529 "parser.y"
                                      {yylhs.value.as < std::vector<Haskell::Export> > () = yystack_[2].value.as < std::vector<Haskell::Export> > (); yylhs.value.as < std::vector<Haskell::Export> > ().push_back(yystack_[0].value.as < Haskell::Export > ());}
#line 2136 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 530 "parser.y"
                                      {yylhs.value.as < std::vector<Haskell::Export> > ().push_back(yystack_[0].value.as < Haskell::Export > ());}
#line 2142 "parser.cc"
    break;

  case 22: // export: qcname export_subspec
#line 532 "parser.y"
                                      {yylhs.value.as < Haskell::Export > () = Haskell::ExportSymbol{yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Haskell::ExportSubSpec> > ()}; }
#line 2148 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 533 "parser.y"
                                      {yylhs.value.as < Haskell::Export > () = Haskell::ExportModule{{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2154 "parser.cc"
    break;

  case 24: // export_subspec: %empty
#line 535 "parser.y"
                                      {}
#line 2160 "parser.cc"
    break;

  case 25: // export_subspec: "(" qcnames ")"
#line 536 "parser.y"
                                      { yylhs.value.as < std::optional<Haskell::ExportSubSpec> > () = Haskell::ExportSubSpecSome{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2166 "parser.cc"
    break;

  case 26: // export_subspec: "(" ".." ")"
#line 537 "parser.y"
                                      { yylhs.value.as < std::optional<Haskell::ExportSubSpec> > () = Haskell::ExportSubSpecAll(); }
#line 2172 "parser.cc"
    break;

  case 27: // qcnames: %empty
#line 539 "parser.y"
                   {}
#line 2178 "parser.cc"
    break;

  case 28: // qcnames: qcnames1
#line 540 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2184 "parser.cc"
    break;

  case 29: // qcnames1: qcnames1 "," qcname
#line 542 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2190 "parser.cc"
    break;

  case 30: // qcnames1: qcname
#line 543 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2196 "parser.cc"
    break;

  case 31: // qcname: qvar
#line 545 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2202 "parser.cc"
    break;

  case 32: // qcname: oqtycon_no_varcon
#line 546 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2208 "parser.cc"
    break;

  case 37: // importdecls: importdecls_semi importdecl
#line 556 "parser.y"
                                         { yylhs.value.as < std::vector<Haskell::ImpDecl> > () = yystack_[1].value.as < std::vector<Haskell::ImpDecl> > (), yylhs.value.as < std::vector<Haskell::ImpDecl> > ().push_back(yystack_[0].value.as < Haskell::ImpDecl > ()); }
#line 2214 "parser.cc"
    break;

  case 38: // importdecls_semi: importdecls_semi importdecl semis1
#line 558 "parser.y"
                                                     { yylhs.value.as < std::vector<Haskell::ImpDecl> > () = yystack_[2].value.as < std::vector<Haskell::ImpDecl> > (); yylhs.value.as < std::vector<Haskell::ImpDecl> > ().push_back(yystack_[1].value.as < Haskell::ImpDecl > ()); }
#line 2220 "parser.cc"
    break;

  case 39: // importdecls_semi: %empty
#line 559 "parser.y"
                         { }
#line 2226 "parser.cc"
    break;

  case 40: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 561 "parser.y"
                                                                                                        {
    yylhs.value.as < Haskell::ImpDecl > () = Haskell::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < std::string > (),yystack_[1].value.as < std::optional<std::string> > (),yystack_[0].value.as < std::optional<Haskell::ImpSpec> > ());
}
#line 2234 "parser.cc"
    break;

  case 41: // optqualified: "qualified"
#line 574 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2240 "parser.cc"
    break;

  case 42: // optqualified: %empty
#line 575 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2246 "parser.cc"
    break;

  case 43: // maybeas: "as" modid
#line 577 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 2252 "parser.cc"
    break;

  case 44: // maybeas: %empty
#line 578 "parser.y"
                               { }
#line 2258 "parser.cc"
    break;

  case 45: // maybeimpspec: impspec
#line 580 "parser.y"
                               { yylhs.value.as < std::optional<Haskell::ImpSpec> > () = yystack_[0].value.as < Haskell::ImpSpec > (); }
#line 2264 "parser.cc"
    break;

  case 46: // maybeimpspec: %empty
#line 581 "parser.y"
                               { }
#line 2270 "parser.cc"
    break;

  case 47: // impspec: "(" exportlist ")"
#line 585 "parser.y"
                                      { yylhs.value.as < Haskell::ImpSpec > () = Haskell::ImpSpec{false, yystack_[1].value.as < std::vector<Haskell::Export> > ()}; }
#line 2276 "parser.cc"
    break;

  case 48: // impspec: "hiding" "(" exportlist ")"
#line 586 "parser.y"
                                      { yylhs.value.as < Haskell::ImpSpec > () = Haskell::ImpSpec{true,  yystack_[1].value.as < std::vector<Haskell::Export> > ()}; }
#line 2282 "parser.cc"
    break;

  case 49: // prec: %empty
#line 591 "parser.y"
                   { }
#line 2288 "parser.cc"
    break;

  case 50: // prec: "INTEGER"
#line 592 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < int > (); }
#line 2294 "parser.cc"
    break;

  case 51: // infix: "infix"
#line 594 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infix; }
#line 2300 "parser.cc"
    break;

  case 52: // infix: "infixl"
#line 595 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infixl; }
#line 2306 "parser.cc"
    break;

  case 53: // infix: "infixr"
#line 596 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infixr; }
#line 2312 "parser.cc"
    break;

  case 54: // ops: ops "," op
#line 598 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 2318 "parser.cc"
    break;

  case 55: // ops: op
#line 599 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 2324 "parser.cc"
    break;

  case 56: // topdecls: topdecls_semi topdecl
#line 603 "parser.y"
                                 { yylhs.value.as < Haskell::Decls > () = yystack_[1].value.as < Haskell::Decls > (); yylhs.value.as < Haskell::Decls > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 2330 "parser.cc"
    break;

  case 57: // topdecls_semi: topdecls_semi topdecl semis1
#line 605 "parser.y"
                                            { yylhs.value.as < Haskell::Decls > () = yystack_[2].value.as < Haskell::Decls > (); yylhs.value.as < Haskell::Decls > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2336 "parser.cc"
    break;

  case 58: // topdecls_semi: %empty
#line 606 "parser.y"
                                            { }
#line 2342 "parser.cc"
    break;

  case 59: // topdecl: cl_decl
#line 608 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2348 "parser.cc"
    break;

  case 60: // topdecl: ty_decl
#line 609 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2354 "parser.cc"
    break;

  case 61: // topdecl: inst_decl
#line 610 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2360 "parser.cc"
    break;

  case 62: // topdecl: "default" "(" comma_types0 ")"
#line 613 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Haskell::DefaultDecl(yystack_[1].value.as < std::vector<expression_ref> > ()); }
#line 2366 "parser.cc"
    break;

  case 63: // topdecl: decl_no_th
#line 620 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2372 "parser.cc"
    break;

  case 64: // topdecl: infixexp_top
#line 622 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2378 "parser.cc"
    break;

  case 65: // topdecl: "builtin" var "INTEGER" "STRING" "STRING"
#line 623 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Haskell::BuiltinDecl(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2384 "parser.cc"
    break;

  case 66: // topdecl: "builtin" var "INTEGER" "STRING"
#line 624 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Haskell::BuiltinDecl(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2390 "parser.cc"
    break;

  case 67: // topdecl: "builtin" varop "INTEGER" "STRING" "STRING"
#line 625 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Haskell::BuiltinDecl(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2396 "parser.cc"
    break;

  case 68: // topdecl: "builtin" varop "INTEGER" "STRING"
#line 626 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Haskell::BuiltinDecl(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2402 "parser.cc"
    break;

  case 69: // cl_decl: "class" tycl_hdr wherebinds
#line 628 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_class_decl(yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().second,yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 2408 "parser.cc"
    break;

  case 70: // ty_decl: "type" type "=" ctypedoc
#line 630 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_type_synonym({yystack_[2].location,yystack_[2].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2414 "parser.cc"
    break;

  case 71: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 631 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < Haskell::DataOrNewtype > (),yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().second,yystack_[1].value.as < std::vector<Haskell::Constructor> > ());}
#line 2420 "parser.cc"
    break;

  case 72: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig
#line 632 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[3].value.as < Haskell::DataOrNewtype > (),yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().second,{});}
#line 2426 "parser.cc"
    break;

  case 73: // inst_decl: "instance" overlap_pragma inst_type wherebinds
#line 637 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_instance_decl({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 2432 "parser.cc"
    break;

  case 83: // data_or_newtype: "data"
#line 692 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::data;}
#line 2438 "parser.cc"
    break;

  case 84: // data_or_newtype: "newtype"
#line 693 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::newtype;}
#line 2444 "parser.cc"
    break;

  case 87: // tycl_hdr: context "=>" type
#line 705 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ()};}
#line 2450 "parser.cc"
    break;

  case 88: // tycl_hdr: type
#line 706 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {{},yystack_[0].value.as < expression_ref > ()};}
#line 2456 "parser.cc"
    break;

  case 92: // decls: decls ";" decl
#line 754 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2462 "parser.cc"
    break;

  case 93: // decls: decls ";"
#line 755 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2468 "parser.cc"
    break;

  case 94: // decls: decl
#line 756 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2474 "parser.cc"
    break;

  case 95: // decls: %empty
#line 757 "parser.y"
                        {}
#line 2480 "parser.cc"
    break;

  case 96: // decllist: "{" decls "}"
#line 759 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Decls> > () = Located<Haskell::Decls>(yystack_[1].location,yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2486 "parser.cc"
    break;

  case 97: // decllist: "vocurly" decls close
#line 760 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Decls> > () = Located<Haskell::Decls>(yystack_[1].location,yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2492 "parser.cc"
    break;

  case 98: // binds: decllist
#line 762 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Decls> > () = yystack_[0].value.as < Located<Haskell::Decls> > ();}
#line 2498 "parser.cc"
    break;

  case 99: // wherebinds: "where" binds
#line 764 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Haskell::Decls>> > () = yystack_[0].value.as < Located<Haskell::Decls> > ();}
#line 2504 "parser.cc"
    break;

  case 100: // wherebinds: %empty
#line 765 "parser.y"
                                 {}
#line 2510 "parser.cc"
    break;

  case 106: // opt_tyconsig: %empty
#line 791 "parser.y"
                     {}
#line 2516 "parser.cc"
    break;

  case 107: // opt_tyconsig: "::" gtycon
#line 792 "parser.y"
                     {yylhs.value.as < expression_ref > () = Haskell::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2522 "parser.cc"
    break;

  case 108: // sigtype: ctype
#line 794 "parser.y"
                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2528 "parser.cc"
    break;

  case 109: // sigtypedoc: ctypedoc
#line 796 "parser.y"
                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2534 "parser.cc"
    break;

  case 110: // sig_vars: sig_vars "," var
#line 798 "parser.y"
                           {yylhs.value.as < std::vector<Haskell::Var> > () = yystack_[2].value.as < std::vector<Haskell::Var> > (); yylhs.value.as < std::vector<Haskell::Var> > ().push_back(Haskell::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}));}
#line 2540 "parser.cc"
    break;

  case 111: // sig_vars: var
#line 799 "parser.y"
                           {yylhs.value.as < std::vector<Haskell::Var> > ().push_back(Haskell::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}));}
#line 2546 "parser.cc"
    break;

  case 112: // sigtypes1: sigtype
#line 801 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2552 "parser.cc"
    break;

  case 113: // sigtypes1: sigtypes1 "," sigtype
#line 802 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2558 "parser.cc"
    break;

  case 114: // strict_mark: strictness
#line 806 "parser.y"
                                            {yylhs.value.as < Haskell::StrictLazy > () = yystack_[0].value.as < Haskell::StrictLazy > ();}
#line 2564 "parser.cc"
    break;

  case 115: // strictness: "!"
#line 812 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::strict;}
#line 2570 "parser.cc"
    break;

  case 116: // strictness: "~"
#line 813 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::lazy;}
#line 2576 "parser.cc"
    break;

  case 117: // ctype: "forall" tv_bndrs "." ctype
#line 820 "parser.y"
                                   {yylhs.value.as < expression_ref > () = Haskell::ForallType(yystack_[2].value.as < std::vector<Haskell::TypeVar> > (), yystack_[0].value.as < expression_ref > ());}
#line 2582 "parser.cc"
    break;

  case 118: // ctype: context "=>" ctype
#line 821 "parser.y"
                                   {yylhs.value.as < expression_ref > () = Haskell::ConstrainedType(yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ());}
#line 2588 "parser.cc"
    break;

  case 119: // ctype: type
#line 823 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2594 "parser.cc"
    break;

  case 120: // ctypedoc: ctype
#line 825 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2600 "parser.cc"
    break;

  case 121: // context: btype
#line 834 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(yystack_[0].value.as < expression_ref > ());}
#line 2606 "parser.cc"
    break;

  case 122: // context_no_ops: btype_no_ops
#line 836 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2612 "parser.cc"
    break;

  case 123: // type: btype
#line 838 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2618 "parser.cc"
    break;

  case 124: // type: btype "->" ctype
#line 839 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps({Haskell::TypeCon({yystack_[1].location,"->"}),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()});}
#line 2624 "parser.cc"
    break;

  case 125: // typedoc: type
#line 841 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2630 "parser.cc"
    break;

  case 126: // btype: tyapps
#line 844 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2636 "parser.cc"
    break;

  case 127: // btype_no_ops: atype_docs
#line 846 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2642 "parser.cc"
    break;

  case 128: // btype_no_ops: btype_no_ops atype_docs
#line 847 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2648 "parser.cc"
    break;

  case 129: // tyapps: tyapp
#line 849 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2654 "parser.cc"
    break;

  case 130: // tyapps: tyapps tyapp
#line 850 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2660 "parser.cc"
    break;

  case 131: // tyapp: atype
#line 852 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2666 "parser.cc"
    break;

  case 132: // tyapp: qtyconop
#line 853 "parser.y"
                                   {yylhs.value.as < expression_ref > () = Haskell::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2672 "parser.cc"
    break;

  case 133: // tyapp: tyvarop
#line 854 "parser.y"
                                   {yylhs.value.as < expression_ref > () = Haskell::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2678 "parser.cc"
    break;

  case 134: // atype_docs: atype
#line 860 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2684 "parser.cc"
    break;

  case 135: // atype: ntgtycon
#line 867 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Haskell::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2690 "parser.cc"
    break;

  case 136: // atype: tyvar
#line 868 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Haskell::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2696 "parser.cc"
    break;

  case 137: // atype: "*"
#line 869 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Haskell::TypeCon({yystack_[0].location,"*"});}
#line 2702 "parser.cc"
    break;

  case 138: // atype: strict_mark atype
#line 870 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Haskell::StrictLazyType(yystack_[1].value.as < Haskell::StrictLazy > (),yystack_[0].value.as < expression_ref > ());}
#line 2708 "parser.cc"
    break;

  case 139: // atype: "{" fielddecls "}"
#line 871 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Haskell::FieldDecls(yystack_[1].value.as < std::vector<Haskell::FieldDecl> > ());}
#line 2714 "parser.cc"
    break;

  case 140: // atype: "(" ")"
#line 872 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Haskell::TypeCon({yystack_[1].location,"()"});}
#line 2720 "parser.cc"
    break;

  case 141: // atype: "(" comma_types1 "," ctype ")"
#line 873 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<expression_ref> > ();ts.push_back(yystack_[1].value.as < expression_ref > ());yylhs.value.as < expression_ref > () = Haskell::TupleType(ts);}
#line 2726 "parser.cc"
    break;

  case 142: // atype: "[" ctype "]"
#line 879 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Haskell::ListType{yystack_[1].value.as < expression_ref > ()}; }
#line 2732 "parser.cc"
    break;

  case 143: // atype: "(" ctype ")"
#line 880 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2738 "parser.cc"
    break;

  case 144: // inst_type: sigtype
#line 884 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2744 "parser.cc"
    break;

  case 147: // comma_types0: comma_types1
#line 889 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2750 "parser.cc"
    break;

  case 148: // comma_types0: %empty
#line 890 "parser.y"
                                       { /* default construction OK */ }
#line 2756 "parser.cc"
    break;

  case 149: // comma_types1: ctype
#line 892 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2762 "parser.cc"
    break;

  case 150: // comma_types1: comma_types1 "," ctype
#line 893 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2768 "parser.cc"
    break;

  case 151: // tv_bndrs: tv_bndrs tv_bndr
#line 900 "parser.y"
                               {yylhs.value.as < std::vector<Haskell::TypeVar> > () = yystack_[1].value.as < std::vector<Haskell::TypeVar> > (); yylhs.value.as < std::vector<Haskell::TypeVar> > ().push_back(yystack_[0].value.as < Haskell::TypeVar > ());}
#line 2774 "parser.cc"
    break;

  case 152: // tv_bndrs: %empty
#line 901 "parser.y"
                               { /* default construction OK */}
#line 2780 "parser.cc"
    break;

  case 153: // tv_bndr: tyvar
#line 904 "parser.y"
                                    {yylhs.value.as < Haskell::TypeVar > () = Haskell::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2786 "parser.cc"
    break;

  case 154: // tv_bndr: "(" tyvar "::" kind ")"
#line 905 "parser.y"
                                    {yylhs.value.as < Haskell::TypeVar > () = Haskell::TypeVar({yystack_[3].location,yystack_[3].value.as < std::string > ()},yystack_[1].value.as < expression_ref > ());}
#line 2792 "parser.cc"
    break;

  case 155: // kind: ctype
#line 923 "parser.y"
             {yylhs.value.as < expression_ref > () = make_kind(yystack_[0].value.as < expression_ref > ());}
#line 2798 "parser.cc"
    break;

  case 156: // constrs: "=" constrs1
#line 929 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::Constructor> > () = yystack_[0].value.as < std::vector<Haskell::Constructor> > ();}
#line 2804 "parser.cc"
    break;

  case 157: // constrs1: constrs1 "|" constr
#line 931 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::Constructor> > () = yystack_[2].value.as < std::vector<Haskell::Constructor> > (); yylhs.value.as < std::vector<Haskell::Constructor> > ().push_back(yystack_[0].value.as < Haskell::Constructor > ());}
#line 2810 "parser.cc"
    break;

  case 158: // constrs1: constr
#line 932 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::Constructor> > ().push_back(yystack_[0].value.as < Haskell::Constructor > ());}
#line 2816 "parser.cc"
    break;

  case 159: // constr: forall context_no_ops "=>" constr_stuff
#line 934 "parser.y"
                                                {yylhs.value.as < Haskell::Constructor > () = make_constructor(yystack_[3].value.as < std::vector<Haskell::TypeVar> > (),yystack_[2].value.as < Haskell::Context > (), yystack_[0].value.as < expression_ref > ());}
#line 2822 "parser.cc"
    break;

  case 160: // constr: forall constr_stuff
#line 935 "parser.y"
                                                {yylhs.value.as < Haskell::Constructor > () = make_constructor(yystack_[1].value.as < std::vector<Haskell::TypeVar> > (),{}, yystack_[0].value.as < expression_ref > ());}
#line 2828 "parser.cc"
    break;

  case 161: // forall: "forall" tv_bndrs "."
#line 937 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::TypeVar> > () = yystack_[1].value.as < std::vector<Haskell::TypeVar> > ();}
#line 2834 "parser.cc"
    break;

  case 162: // forall: %empty
#line 938 "parser.y"
                                {}
#line 2840 "parser.cc"
    break;

  case 163: // constr_stuff: btype_no_ops
#line 940 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2846 "parser.cc"
    break;

  case 164: // constr_stuff: btype_no_ops conop btype_no_ops
#line 941 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps({Haskell::TypeVar({yystack_[1].location,yystack_[1].value.as < std::string > ()}),make_tyapps(yystack_[2].value.as < std::vector<expression_ref> > ()),make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 2852 "parser.cc"
    break;

  case 165: // fielddecls: %empty
#line 943 "parser.y"
                                {}
#line 2858 "parser.cc"
    break;

  case 166: // fielddecls: fielddecls1
#line 944 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::FieldDecl> > () = yystack_[0].value.as < std::vector<Haskell::FieldDecl> > ();}
#line 2864 "parser.cc"
    break;

  case 167: // fielddecls1: fielddecls1 "," fielddecl
#line 946 "parser.y"
                                        {yylhs.value.as < std::vector<Haskell::FieldDecl> > () = yystack_[2].value.as < std::vector<Haskell::FieldDecl> > (); yylhs.value.as < std::vector<Haskell::FieldDecl> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2870 "parser.cc"
    break;

  case 168: // fielddecls1: fielddecl
#line 947 "parser.y"
                                        {yylhs.value.as < std::vector<Haskell::FieldDecl> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2876 "parser.cc"
    break;

  case 169: // fielddecl: sig_vars "::" ctype
#line 949 "parser.y"
                                        {yylhs.value.as < Haskell::FieldDecl > () = Haskell::FieldDecl(yystack_[2].value.as < std::vector<Haskell::Var> > (),yystack_[0].value.as < expression_ref > ());}
#line 2882 "parser.cc"
    break;

  case 180: // decl_no_th: sigdecl
#line 968 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2888 "parser.cc"
    break;

  case 181: // decl_no_th: "!" aexp rhs
#line 970 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::StrictValueDecl{yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}; }
#line 2894 "parser.cc"
    break;

  case 182: // decl_no_th: infixexp_top rhs
#line 971 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::ValueDecl(make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ());}
#line 2900 "parser.cc"
    break;

  case 183: // decl: decl_no_th
#line 973 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2906 "parser.cc"
    break;

  case 184: // rhs: "=" exp wherebinds
#line 977 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::SimpleRHS{{yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ()};}
#line 2912 "parser.cc"
    break;

  case 185: // rhs: gdrhs wherebinds
#line 978 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::MultiGuardedRHS{yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ()};}
#line 2918 "parser.cc"
    break;

  case 186: // gdrhs: gdrhs gdrh
#line 980 "parser.y"
                              {yylhs.value.as < std::vector<Haskell::GuardedRHS> > () = yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (); yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 2924 "parser.cc"
    break;

  case 187: // gdrhs: gdrh
#line 981 "parser.y"
                              {yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 2930 "parser.cc"
    break;

  case 188: // gdrh: "|" guardquals "=" exp
#line 985 "parser.y"
                              {yylhs.value.as < Haskell::GuardedRHS > () = Haskell::GuardedRHS{yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ()};}
#line 2936 "parser.cc"
    break;

  case 189: // sigdecl: sig_vars "::" sigtypedoc
#line 995 "parser.y"
                                  { yylhs.value.as < expression_ref > () = Haskell::TypeDecl{yystack_[2].value.as < std::vector<Haskell::Var> > (),yystack_[0].value.as < expression_ref > ()}; }
#line 2942 "parser.cc"
    break;

  case 190: // sigdecl: infix prec ops
#line 996 "parser.y"
                         { yylhs.value.as < expression_ref > () = Haskell::FixityDecl{yystack_[2].value.as < Haskell::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()}; }
#line 2948 "parser.cc"
    break;

  case 191: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 998 "parser.y"
                                                    {}
#line 2954 "parser.cc"
    break;

  case 192: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 999 "parser.y"
                                            {}
#line 2960 "parser.cc"
    break;

  case 193: // sigdecl: "{-# SCC" qvar "#-}"
#line 1000 "parser.y"
                              {}
#line 2966 "parser.cc"
    break;

  case 194: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1001 "parser.y"
                                     {}
#line 2972 "parser.cc"
    break;

  case 195: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1002 "parser.y"
                                                               {}
#line 2978 "parser.cc"
    break;

  case 196: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1003 "parser.y"
                                                                      {}
#line 2984 "parser.cc"
    break;

  case 197: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1004 "parser.y"
                                                     {}
#line 2990 "parser.cc"
    break;

  case 202: // exp: infixexp "::" sigtype
#line 1015 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_typed_exp(make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()); }
#line 2996 "parser.cc"
    break;

  case 203: // exp: infixexp
#line 1016 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 3002 "parser.cc"
    break;

  case 204: // infixexp: exp10
#line 1018 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3008 "parser.cc"
    break;

  case 205: // infixexp: infixexp qop exp10
#line 1019 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(Haskell::Var({yystack_[1].location,yystack_[1].value.as < std::string > ()})); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3014 "parser.cc"
    break;

  case 206: // infixexp_top: exp10_top
#line 1021 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3020 "parser.cc"
    break;

  case 207: // infixexp_top: infixexp_top qop exp10_top
#line 1022 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(Haskell::Var({yystack_[1].location,yystack_[1].value.as < std::string > ()})); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3026 "parser.cc"
    break;

  case 208: // exp10_top: "-" fexp
#line 1024 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_minus(make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 3032 "parser.cc"
    break;

  case 209: // exp10_top: "{-# CORE" "STRING" "#-}"
#line 1025 "parser.y"
                                   {}
#line 3038 "parser.cc"
    break;

  case 210: // exp10_top: fexp
#line 1026 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 3044 "parser.cc"
    break;

  case 211: // exp10: exp10_top
#line 1028 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3050 "parser.cc"
    break;

  case 212: // exp10: scc_annot exp
#line 1029 "parser.y"
                                 {}
#line 3056 "parser.cc"
    break;

  case 217: // fexp: fexp aexp
#line 1040 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3062 "parser.cc"
    break;

  case 218: // fexp: fexp "TYPEAPP" atype
#line 1041 "parser.y"
                                 {}
#line 3068 "parser.cc"
    break;

  case 219: // fexp: "static" aexp
#line 1042 "parser.y"
                                 {}
#line 3074 "parser.cc"
    break;

  case 220: // fexp: aexp
#line 1043 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3080 "parser.cc"
    break;

  case 221: // aexp: qvar "@" aexp
#line 1045 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Haskell::AsPattern(Haskell::Var({yystack_[2].location,yystack_[2].value.as < std::string > ()}),yystack_[0].value.as < expression_ref > ());}
#line 3086 "parser.cc"
    break;

  case 222: // aexp: "~" aexp
#line 1046 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Haskell::LazyPattern(yystack_[0].value.as < expression_ref > ());}
#line 3092 "parser.cc"
    break;

  case 223: // aexp: "\\" apats1 "->" exp
#line 1047 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Haskell::LambdaExp(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3098 "parser.cc"
    break;

  case 224: // aexp: "let" binds "in" exp
#line 1048 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Haskell::LetExp(yystack_[2].value.as < Located<Haskell::Decls> > (),{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 3104 "parser.cc"
    break;

  case 225: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1050 "parser.y"
                                                       {yylhs.value.as < expression_ref > () = Haskell::IfExp({yystack_[6].location,yystack_[6].value.as < expression_ref > ()},{yystack_[3].location,yystack_[3].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 3110 "parser.cc"
    break;

  case 226: // aexp: "case" exp "of" altslist
#line 1052 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Haskell::CaseExp(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < Haskell::Alts > ());}
#line 3116 "parser.cc"
    break;

  case 227: // aexp: "do" stmtlist
#line 1053 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Haskell::Do(yystack_[0].value.as < Haskell::Stmts > ());}
#line 3122 "parser.cc"
    break;

  case 228: // aexp: "mdo" stmtlist
#line 1054 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Haskell::MDo(yystack_[0].value.as < Haskell::Stmts > ());}
#line 3128 "parser.cc"
    break;

  case 229: // aexp: aexp1
#line 1056 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3134 "parser.cc"
    break;

  case 230: // aexp1: aexp1 "{" fbinds "}"
#line 1058 "parser.y"
                              {}
#line 3140 "parser.cc"
    break;

  case 231: // aexp1: aexp2
#line 1059 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3146 "parser.cc"
    break;

  case 232: // aexp2: qvar
#line 1061 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3152 "parser.cc"
    break;

  case 233: // aexp2: qcon
#line 1062 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3158 "parser.cc"
    break;

  case 234: // aexp2: literal
#line 1063 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3164 "parser.cc"
    break;

  case 235: // aexp2: "(" texp ")"
#line 1064 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3170 "parser.cc"
    break;

  case 236: // aexp2: "(" tup_exprs ")"
#line 1065 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::Tuple(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3176 "parser.cc"
    break;

  case 237: // aexp2: "[" list "]"
#line 1070 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3182 "parser.cc"
    break;

  case 238: // aexp2: "_"
#line 1071 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::WildcardPattern();}
#line 3188 "parser.cc"
    break;

  case 239: // texp: exp
#line 1076 "parser.y"
                      {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3194 "parser.cc"
    break;

  case 240: // texp: infixexp qop
#line 1077 "parser.y"
                      {yylhs.value.as < expression_ref > () = Haskell::LeftSection ( make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ()), Haskell::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}) ); }
#line 3200 "parser.cc"
    break;

  case 241: // texp: qopm infixexp
#line 1078 "parser.y"
                      {yylhs.value.as < expression_ref > () = Haskell::RightSection( Haskell::Var({yystack_[1].location,yystack_[1].value.as < std::string > ()}), make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ()) ); }
#line 3206 "parser.cc"
    break;

  case 242: // tup_exprs: tup_exprs "," texp
#line 1083 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3212 "parser.cc"
    break;

  case 243: // tup_exprs: texp "," texp
#line 1084 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3218 "parser.cc"
    break;

  case 244: // list: texp
#line 1102 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Haskell::List{{yystack_[0].value.as < expression_ref > ()}}; }
#line 3224 "parser.cc"
    break;

  case 245: // list: lexps
#line 1103 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Haskell::List{yystack_[0].value.as < std::vector<expression_ref> > ()}; }
#line 3230 "parser.cc"
    break;

  case 246: // list: texp ".."
#line 1104 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Haskell::ListFrom(yystack_[1].value.as < expression_ref > ()); }
#line 3236 "parser.cc"
    break;

  case 247: // list: texp "," exp ".."
#line 1105 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Haskell::ListFromThen(yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()); }
#line 3242 "parser.cc"
    break;

  case 248: // list: texp ".." exp
#line 1106 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Haskell::ListFromTo(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()); }
#line 3248 "parser.cc"
    break;

  case 249: // list: texp "," exp ".." exp
#line 1107 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Haskell::ListFromThenTo(yystack_[4].value.as < expression_ref > (), yystack_[2].value.as < expression_ref > (), yystack_[0].value.as < expression_ref > ()); }
#line 3254 "parser.cc"
    break;

  case 250: // list: texp "|" squals
#line 1108 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Haskell::ListComprehension(yystack_[2].value.as < expression_ref > (), yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 3260 "parser.cc"
    break;

  case 251: // lexps: lexps "," texp
#line 1110 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3266 "parser.cc"
    break;

  case 252: // lexps: texp "," texp
#line 1111 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3272 "parser.cc"
    break;

  case 253: // squals: squals "," qual
#line 1124 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3278 "parser.cc"
    break;

  case 254: // squals: qual
#line 1126 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3284 "parser.cc"
    break;

  case 255: // guardquals: guardquals1
#line 1136 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3290 "parser.cc"
    break;

  case 256: // guardquals1: guardquals1 "," qual
#line 1138 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > ();yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3296 "parser.cc"
    break;

  case 257: // guardquals1: qual
#line 1139 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3302 "parser.cc"
    break;

  case 258: // altslist: "{" alts "}"
#line 1142 "parser.y"
                                 {yylhs.value.as < Haskell::Alts > () = Haskell::Alts{yystack_[1].value.as < std::vector<Located<Haskell::Alt>> > ()};}
#line 3308 "parser.cc"
    break;

  case 259: // altslist: "vocurly" alts close
#line 1143 "parser.y"
                                 {yylhs.value.as < Haskell::Alts > () = Haskell::Alts{yystack_[1].value.as < std::vector<Located<Haskell::Alt>> > ()};}
#line 3314 "parser.cc"
    break;

  case 260: // altslist: "{" "}"
#line 1144 "parser.y"
                                 {}
#line 3320 "parser.cc"
    break;

  case 261: // altslist: "vocurly" close
#line 1145 "parser.y"
                                 {}
#line 3326 "parser.cc"
    break;

  case 262: // alts: alts1
#line 1147 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[0].value.as < std::vector<Located<Haskell::Alt>> > ();}
#line 3332 "parser.cc"
    break;

  case 263: // alts: ";" alts
#line 1148 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[0].value.as < std::vector<Located<Haskell::Alt>> > ();}
#line 3338 "parser.cc"
    break;

  case 264: // alts1: alts1 ";" alt
#line 1150 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[2].value.as < std::vector<Located<Haskell::Alt>> > (); yylhs.value.as < std::vector<Located<Haskell::Alt>> > ().push_back(yystack_[0].value.as < Located<Haskell::Alt> > ());}
#line 3344 "parser.cc"
    break;

  case 265: // alts1: alts1 ";"
#line 1151 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[1].value.as < std::vector<Located<Haskell::Alt>> > ();}
#line 3350 "parser.cc"
    break;

  case 266: // alts1: alt
#line 1152 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > ().push_back(yystack_[0].value.as < Located<Haskell::Alt> > ());}
#line 3356 "parser.cc"
    break;

  case 267: // alt: pat alt_rhs
#line 1154 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Alt> > () = Located<Haskell::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}};}
#line 3362 "parser.cc"
    break;

  case 268: // alt_rhs: "->" exp wherebinds
#line 1156 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Haskell::SimpleRHS{{yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ()};}
#line 3368 "parser.cc"
    break;

  case 269: // alt_rhs: gdpats wherebinds
#line 1157 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Haskell::MultiGuardedRHS{yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ()};}
#line 3374 "parser.cc"
    break;

  case 270: // gdpats: gdpats gdpat
#line 1159 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::GuardedRHS> > () = yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (); yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 3380 "parser.cc"
    break;

  case 271: // gdpats: gdpat
#line 1160 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 3386 "parser.cc"
    break;

  case 272: // gdpat: "|" guardquals "->" exp
#line 1169 "parser.y"
                                 {yylhs.value.as < Haskell::GuardedRHS > ()=Haskell::GuardedRHS{yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ()};}
#line 3392 "parser.cc"
    break;

  case 273: // pat: exp
#line 1171 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3398 "parser.cc"
    break;

  case 274: // pat: "!" aexp
#line 1172 "parser.y"
              {yylhs.value.as < expression_ref > () = Haskell::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3404 "parser.cc"
    break;

  case 275: // bindpat: exp
#line 1174 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3410 "parser.cc"
    break;

  case 276: // bindpat: "!" aexp
#line 1175 "parser.y"
              {yylhs.value.as < expression_ref > () = Haskell::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3416 "parser.cc"
    break;

  case 277: // apat: aexp
#line 1177 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3422 "parser.cc"
    break;

  case 278: // apat: "!" aexp
#line 1178 "parser.y"
              {yylhs.value.as < expression_ref > () = Haskell::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3428 "parser.cc"
    break;

  case 279: // apats1: apats1 apat
#line 1180 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3434 "parser.cc"
    break;

  case 280: // apats1: apat
#line 1181 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3440 "parser.cc"
    break;

  case 281: // stmtlist: "{" stmts "}"
#line 1184 "parser.y"
                               {yylhs.value.as < Haskell::Stmts > () = Haskell::Stmts{yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 3446 "parser.cc"
    break;

  case 282: // stmtlist: "vocurly" stmts close
#line 1185 "parser.y"
                               {yylhs.value.as < Haskell::Stmts > () = Haskell::Stmts{yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 3452 "parser.cc"
    break;

  case 283: // stmts: stmts ";" stmt
#line 1187 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3458 "parser.cc"
    break;

  case 284: // stmts: stmts ";"
#line 1188 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3464 "parser.cc"
    break;

  case 285: // stmts: stmt
#line 1189 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3470 "parser.cc"
    break;

  case 286: // stmts: %empty
#line 1190 "parser.y"
                       {}
#line 3476 "parser.cc"
    break;

  case 287: // stmt: qual
#line 1195 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3482 "parser.cc"
    break;

  case 288: // stmt: "rec" stmtlist
#line 1196 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::RecStmt(yystack_[0].value.as < Haskell::Stmts > ());}
#line 3488 "parser.cc"
    break;

  case 289: // qual: bindpat "<-" exp
#line 1198 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::PatQual(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3494 "parser.cc"
    break;

  case 290: // qual: exp
#line 1199 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::SimpleQual(yystack_[0].value.as < expression_ref > ());}
#line 3500 "parser.cc"
    break;

  case 291: // qual: "let" binds
#line 1200 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::LetQual(yystack_[0].value.as < Located<Haskell::Decls> > ());}
#line 3506 "parser.cc"
    break;

  case 299: // qcon: gen_qcon
#line 1245 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3512 "parser.cc"
    break;

  case 300: // qcon: sysdcon
#line 1246 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3518 "parser.cc"
    break;

  case 301: // gen_qcon: qconid
#line 1248 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3524 "parser.cc"
    break;

  case 302: // gen_qcon: "(" qconsym ")"
#line 1249 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3530 "parser.cc"
    break;

  case 303: // con: conid
#line 1251 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3536 "parser.cc"
    break;

  case 304: // con: "(" consym ")"
#line 1252 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3542 "parser.cc"
    break;

  case 305: // con: sysdcon
#line 1253 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3548 "parser.cc"
    break;

  case 308: // sysdcon_no_list: "(" ")"
#line 1258 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 3554 "parser.cc"
    break;

  case 309: // sysdcon_no_list: "(" commas ")"
#line 1259 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3560 "parser.cc"
    break;

  case 310: // sysdcon_no_list: "(#" "#)"
#line 1260 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 3566 "parser.cc"
    break;

  case 311: // sysdcon_no_list: "(#" commas "#)"
#line 1261 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3572 "parser.cc"
    break;

  case 312: // sysdcon: sysdcon_no_list
#line 1263 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3578 "parser.cc"
    break;

  case 313: // sysdcon: "[" "]"
#line 1264 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 3584 "parser.cc"
    break;

  case 314: // conop: consym
#line 1266 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3590 "parser.cc"
    break;

  case 315: // conop: "`" conid "`"
#line 1267 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3596 "parser.cc"
    break;

  case 316: // qconop: qconsym
#line 1269 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3602 "parser.cc"
    break;

  case 317: // qconop: "`" qconid "`"
#line 1270 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3608 "parser.cc"
    break;

  case 318: // gtycon: ntgtycon
#line 1273 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3614 "parser.cc"
    break;

  case 319: // gtycon: "(" ")"
#line 1274 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 3620 "parser.cc"
    break;

  case 320: // gtycon: "(#" "#)"
#line 1275 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 3626 "parser.cc"
    break;

  case 321: // ntgtycon: oqtycon
#line 1277 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3632 "parser.cc"
    break;

  case 322: // ntgtycon: "(" commas ")"
#line 1278 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3638 "parser.cc"
    break;

  case 323: // ntgtycon: "(#" commas "#)"
#line 1279 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3644 "parser.cc"
    break;

  case 324: // ntgtycon: "(" "->" ")"
#line 1280 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 3650 "parser.cc"
    break;

  case 325: // ntgtycon: "[" "]"
#line 1281 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 3656 "parser.cc"
    break;

  case 326: // oqtycon: qtycon
#line 1283 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3662 "parser.cc"
    break;

  case 327: // oqtycon: "(" qtyconsym ")"
#line 1284 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3668 "parser.cc"
    break;

  case 328: // oqtycon: "(" "~" ")"
#line 1285 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3674 "parser.cc"
    break;

  case 329: // oqtycon_no_varcon: qtycon
#line 1287 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3680 "parser.cc"
    break;

  case 330: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1288 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3686 "parser.cc"
    break;

  case 331: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1289 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3692 "parser.cc"
    break;

  case 332: // oqtycon_no_varcon: "(" ":" ")"
#line 1290 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 3698 "parser.cc"
    break;

  case 333: // oqtycon_no_varcon: "(" "~" ")"
#line 1291 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3704 "parser.cc"
    break;

  case 334: // qtyconop: qtyconsym
#line 1294 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3710 "parser.cc"
    break;

  case 335: // qtyconop: "`" qtycon "`"
#line 1295 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3716 "parser.cc"
    break;

  case 336: // qtycondoc: qtycon
#line 1297 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3722 "parser.cc"
    break;

  case 337: // qtycon: "QCONID"
#line 1299 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3728 "parser.cc"
    break;

  case 338: // qtycon: tycon
#line 1300 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3734 "parser.cc"
    break;

  case 339: // tycon: "CONID"
#line 1304 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3740 "parser.cc"
    break;

  case 340: // qtyconsym: "QCONSYM"
#line 1306 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3746 "parser.cc"
    break;

  case 341: // qtyconsym: "QVARSYM"
#line 1307 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3752 "parser.cc"
    break;

  case 342: // qtyconsym: tyconsym
#line 1308 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3758 "parser.cc"
    break;

  case 343: // tyconsym: "CONSYM"
#line 1310 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3764 "parser.cc"
    break;

  case 344: // tyconsym: "VARSYM"
#line 1311 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3770 "parser.cc"
    break;

  case 345: // tyconsym: ":"
#line 1312 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 3776 "parser.cc"
    break;

  case 346: // tyconsym: "-"
#line 1313 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 3782 "parser.cc"
    break;

  case 347: // op: varop
#line 1318 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3788 "parser.cc"
    break;

  case 348: // op: conop
#line 1319 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3794 "parser.cc"
    break;

  case 349: // varop: varsym
#line 1321 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3800 "parser.cc"
    break;

  case 350: // varop: "`" varid "`"
#line 1322 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3806 "parser.cc"
    break;

  case 351: // qop: qvarop
#line 1324 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3812 "parser.cc"
    break;

  case 352: // qop: qconop
#line 1325 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3818 "parser.cc"
    break;

  case 353: // qop: hole_op
#line 1326 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3824 "parser.cc"
    break;

  case 354: // qopm: qvaropm
#line 1328 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3830 "parser.cc"
    break;

  case 355: // qopm: qconop
#line 1329 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3836 "parser.cc"
    break;

  case 356: // qopm: hole_op
#line 1330 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3842 "parser.cc"
    break;

  case 357: // hole_op: "`" "_" "`"
#line 1332 "parser.y"
                      { yylhs.value.as < std::string > () = "_"; }
#line 3848 "parser.cc"
    break;

  case 358: // qvarop: qvarsym
#line 1334 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3854 "parser.cc"
    break;

  case 359: // qvarop: "`" qvarid "`"
#line 1335 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3860 "parser.cc"
    break;

  case 360: // qvaropm: qvarsym_no_minus
#line 1337 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 3866 "parser.cc"
    break;

  case 361: // qvaropm: "`" qvarid "`"
#line 1338 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3872 "parser.cc"
    break;

  case 362: // tyvar: tyvarid
#line 1342 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3878 "parser.cc"
    break;

  case 363: // tyvarop: "`" tyvarid "`"
#line 1344 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3884 "parser.cc"
    break;

  case 364: // tyvarid: "VARID"
#line 1346 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3890 "parser.cc"
    break;

  case 365: // tyvarid: special_id
#line 1347 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3896 "parser.cc"
    break;

  case 366: // tyvarid: "unsafe"
#line 1348 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 3902 "parser.cc"
    break;

  case 367: // tyvarid: "safe"
#line 1349 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 3908 "parser.cc"
    break;

  case 368: // tyvarid: "interruptible"
#line 1350 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 3914 "parser.cc"
    break;

  case 369: // var: varid
#line 1353 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3920 "parser.cc"
    break;

  case 370: // var: "(" varsym ")"
#line 1354 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3926 "parser.cc"
    break;

  case 371: // qvar: qvarid
#line 1356 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3932 "parser.cc"
    break;

  case 372: // qvar: "(" varsym ")"
#line 1357 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3938 "parser.cc"
    break;

  case 373: // qvar: "(" qvarsym1 ")"
#line 1358 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3944 "parser.cc"
    break;

  case 374: // qvarid: varid
#line 1360 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3950 "parser.cc"
    break;

  case 375: // qvarid: "QVARID"
#line 1361 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3956 "parser.cc"
    break;

  case 376: // varid: "VARID"
#line 1363 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3962 "parser.cc"
    break;

  case 377: // varid: special_id
#line 1364 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3968 "parser.cc"
    break;

  case 378: // varid: "unsafe"
#line 1365 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 3974 "parser.cc"
    break;

  case 379: // varid: "safe"
#line 1366 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 3980 "parser.cc"
    break;

  case 380: // varid: "interruptible"
#line 1367 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 3986 "parser.cc"
    break;

  case 381: // varid: "forall"
#line 1368 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 3992 "parser.cc"
    break;

  case 382: // varid: "family"
#line 1369 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 3998 "parser.cc"
    break;

  case 383: // varid: "role"
#line 1370 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 4004 "parser.cc"
    break;

  case 384: // qvarsym: varsym
#line 1372 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4010 "parser.cc"
    break;

  case 385: // qvarsym: qvarsym1
#line 1373 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4016 "parser.cc"
    break;

  case 386: // qvarsym_no_minus: varsym_no_minus
#line 1375 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4022 "parser.cc"
    break;

  case 387: // qvarsym_no_minus: qvarsym1
#line 1376 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4028 "parser.cc"
    break;

  case 388: // qvarsym1: "QVARSYM"
#line 1378 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4034 "parser.cc"
    break;

  case 389: // varsym: varsym_no_minus
#line 1380 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4040 "parser.cc"
    break;

  case 390: // varsym: "-"
#line 1381 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 4046 "parser.cc"
    break;

  case 391: // varsym_no_minus: "VARSYM"
#line 1383 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4052 "parser.cc"
    break;

  case 392: // varsym_no_minus: special_sym
#line 1384 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4058 "parser.cc"
    break;

  case 393: // special_id: "as"
#line 1386 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 4064 "parser.cc"
    break;

  case 394: // special_id: "qualified"
#line 1387 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 4070 "parser.cc"
    break;

  case 395: // special_id: "hiding"
#line 1388 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 4076 "parser.cc"
    break;

  case 396: // special_id: "export"
#line 1389 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 4082 "parser.cc"
    break;

  case 397: // special_id: "label"
#line 1390 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 4088 "parser.cc"
    break;

  case 398: // special_id: "dynamic"
#line 1391 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 4094 "parser.cc"
    break;

  case 399: // special_id: "stdcall"
#line 1392 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 4100 "parser.cc"
    break;

  case 400: // special_id: "ccall"
#line 1393 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 4106 "parser.cc"
    break;

  case 401: // special_id: "capi"
#line 1394 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 4112 "parser.cc"
    break;

  case 402: // special_id: "prim"
#line 1395 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 4118 "parser.cc"
    break;

  case 403: // special_id: "javascript"
#line 1396 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 4124 "parser.cc"
    break;

  case 404: // special_id: "group"
#line 1397 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 4130 "parser.cc"
    break;

  case 405: // special_id: "stock"
#line 1398 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 4136 "parser.cc"
    break;

  case 406: // special_id: "anyclass"
#line 1399 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 4142 "parser.cc"
    break;

  case 407: // special_id: "via"
#line 1400 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 4148 "parser.cc"
    break;

  case 408: // special_id: "unit"
#line 1401 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 4154 "parser.cc"
    break;

  case 409: // special_id: "dependency"
#line 1402 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 4160 "parser.cc"
    break;

  case 410: // special_id: "signature"
#line 1403 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 4166 "parser.cc"
    break;

  case 411: // special_sym: "!"
#line 1405 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 4172 "parser.cc"
    break;

  case 412: // special_sym: "."
#line 1406 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 4178 "parser.cc"
    break;

  case 413: // special_sym: "*"
#line 1407 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 4184 "parser.cc"
    break;

  case 414: // qconid: conid
#line 1411 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4190 "parser.cc"
    break;

  case 415: // qconid: "QCONID"
#line 1412 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4196 "parser.cc"
    break;

  case 416: // conid: "CONID"
#line 1414 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4202 "parser.cc"
    break;

  case 417: // qconsym: consym
#line 1416 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4208 "parser.cc"
    break;

  case 418: // qconsym: "QCONSYM"
#line 1417 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4214 "parser.cc"
    break;

  case 419: // consym: "CONSYM"
#line 1419 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4220 "parser.cc"
    break;

  case 420: // consym: ":"
#line 1420 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4226 "parser.cc"
    break;

  case 421: // literal: "CHAR"
#line 1424 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < char > ();}
#line 4232 "parser.cc"
    break;

  case 422: // literal: "STRING"
#line 1425 "parser.y"
                  {yylhs.value.as < expression_ref > () = yy_make_string(yystack_[0].value.as < std::string > ());}
#line 4238 "parser.cc"
    break;

  case 423: // literal: "INTEGER"
#line 1426 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < int > ();}
#line 4244 "parser.cc"
    break;

  case 424: // literal: "RATIONAL"
#line 1427 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < double > ();}
#line 4250 "parser.cc"
    break;

  case 426: // close: error
#line 1435 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 4256 "parser.cc"
    break;

  case 427: // modid: "CONID"
#line 1439 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4262 "parser.cc"
    break;

  case 428: // modid: "QCONID"
#line 1440 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4268 "parser.cc"
    break;

  case 429: // commas: commas ","
#line 1442 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 4274 "parser.cc"
    break;

  case 430: // commas: ","
#line 1443 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 4280 "parser.cc"
    break;


#line 4284 "parser.cc"

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

  const short parser::yytable_ninf_ = -390;

  const short
  parser::yypact_[] =
  {
      27,    51,  -565,   100,  -565,  -565,  -565,  -565,  -565,   219,
      33,   -28,  -565,    44,   -31,   -31,    64,  -565,  -565,  -565,
    -565,   168,  -565,  -565,  -565,    62,  -565,   170,   175,  3623,
     180,   233,   153,  -565,   600,  -565,   -23,  -565,  -565,  -565,
    -565,    51,  -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,
    -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,
    -565,  -565,  -565,  -565,   163,  -565,  -565,  -565,  -565,   188,
     166,  -565,   183,  -565,  -565,  -565,  -565,  -565,  -565,  -565,
     117,  -565,    51,  -565,   194,  -565,  1969,  3201,  -565,   193,
     127,  1969,  -565,  -565,  -565,   391,   134,  -565,  3201,  3968,
     127,  2695,   209,   192,  3910,    61,  2332,  2695,  2453,  2695,
    1108,   980,    92,  -565,  -565,  -565,  -565,  -565,  -565,    38,
     209,   196,   153,  -565,  -565,  -565,   256,    14,  -565,  -565,
    2072,  -565,  2574,  -565,   234,  -565,  -565,  -565,  -565,  -565,
    -565,   260,    15,  -565,  -565,  -565,  -565,   218,  -565,   244,
     246,  -565,  -565,  -565,  -565,  -565,   250,  -565,   252,   253,
     261,  -565,  -565,  -565,  3623,  3656,  -565,  -565,  -565,  -565,
     359,  -565,   -38,   980,   348,   418,  -565,  -565,  1969,  -565,
    -565,  -565,  -565,  -565,  -565,  -565,  -565,  4057,  2898,  2797,
     262,  3785,  -565,  -565,  -565,  -565,  -565,   346,  3531,  -565,
     286,  -565,   205,  3201,  -565,  -565,  -565,  -565,  -565,  -565,
    -565,  -565,  -565,  -565,  -565,  -565,  2999,  1485,  1485,  -565,
     264,   303,   304,   307,   311,  2999,   729,   729,  -565,   374,
     310,   308,   295,  4102,   267,   269,  -565,  -565,  -565,  -565,
     -20,  3910,  -565,   323,   140,   -13,   297,    46,   290,   325,
    -565,  -565,  2695,  -565,  -565,  2211,  -565,  2574,   187,  -565,
    -565,  3302,  -565,  -565,  -565,   418,    94,   312,   293,  -565,
    1969,  -565,  -565,  -565,  -565,  -565,  -565,  -565,  2453,  -565,
    -565,   -32,    91,   253,   300,   313,   314,   101,  -565,   107,
    2999,  3910,  3910,  -565,   182,   194,   283,  3201,  2999,  4057,
    1969,  1727,  3302,  -565,    23,  -565,  -565,  2090,  -565,  -565,
    -565,  -565,  -565,  3531,  -565,  3877,  2695,  -565,  -565,  -565,
    -565,  -565,  -565,  -565,  -565,   315,   317,   301,  -565,   318,
      44,    51,    25,   350,   353,   147,  2999,  1969,  -565,    69,
     331,   319,  -565,  -565,  -565,   332,   349,  -565,   327,   333,
    -565,   334,   335,   339,   111,   124,   336,   340,   134,  -565,
    -565,  3201,  2999,  -565,  -565,   342,   354,   134,   127,  2695,
     384,   386,   -17,  -565,  -565,    32,  -565,   447,  -565,  -565,
    -565,  -565,  -565,  -565,   346,    85,  -565,  -565,  2072,    42,
    1969,  2999,   365,   360,   351,   355,   345,   373,   401,  -565,
    -565,   404,   381,    61,   186,   413,  -565,  1969,  -565,  -565,
     377,   378,   379,  1969,  1969,  1727,  1236,  -565,  1236,   259,
    -565,  1236,  -565,  1236,    78,  -565,  -565,  -565,  -565,   422,
     415,   420,  4001,   387,  -565,  -565,  -565,  -565,    -4,   255,
    -565,  -565,  -565,  -565,   346,   424,   389,  -565,   392,  -565,
    -565,  -565,  -565,  -565,   406,  -565,   397,   432,  -565,  -565,
    -565,  3752,  -565,  -565,  -565,   407,  3623,  -565,  -565,  -565,
    -565,  1364,   859,  -565,  -565,  -565,  2999,  -565,  4057,  4135,
    -565,  2999,  -565,  -565,  -565,  2999,  -565,  -565,  -565,  -565,
    -565,  -565,  -565,  -565,  -565,  2999,   374,  -565,  -565,  1969,
    -565,  1485,  -565,  1969,  -565,  -565,   729,  -565,  -565,  -565,
    -565,  -565,   390,   393,   416,  -565,  -565,  -565,  -565,  -565,
     419,   338,   130,  -565,  -565,  -565,  -565,  -565,  -565,  -565,
    -565,   403,  -565,   441,  -565,  -565,  -565,  -565,  -565,  2999,
    2999,   409,   182,  -565,   445,  2999,   496,  -565,   517,  -565,
    1969,  1727,  -565,  -565,  3877,  1236,  -565,  3623,   421,  2695,
    -565,  1606,  -565,   434,   423,  -565,   172,    44,  -565,  -565,
    -565,  2999,  4194,  -565,  -565,  -565,   427,  -565,  -565,  -565,
     264,  -565,  -565,  -565,  -565,   333,  -565,   339,  -565,  1727,
    1969,  -565,   -15,   -10,  -565,  -565,  -565,  -565,  -565,  -565,
     448,  -565,  3531,    41,  -565,   517,  -565,  -565,  -565,  -565,
    -565,   431,  -565,  -565,  -565,  -565,  1848,  1727,  1969,  -565,
      39,  -565,  -565,  -565,   460,  -565,   532,  -565,  -565,  -565,
    2999,  -565,  4161,   496,   456,  3347,  -565,  -565,  -565,  -565,
    -565,  -565,  3100,    73,   494,  -565,  -565,  -565,  -565,  -565,
     462,   346,  -565,  -565,  2999,  1969,  -565,  -565,  -565,  3531,
     435,  -565,  3531,  -565,  -565,   436,   444,  -565,  3201,  -565,
    1969,  -565,   446,  -565,  3439,  -565,  3531,  3201,  -565,  -565,
    -565,  -565,  -565
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    36,     0,     2,    36,     4,   427,   428,     8,
       0,    39,     1,     0,     0,     0,    18,    11,    35,    13,
      16,    58,   426,   425,    12,   105,   101,     0,     0,     0,
       0,    42,    37,    15,    14,   104,     0,     6,     7,   393,
     395,     0,   394,   381,   396,   397,   398,   379,   380,   378,
     382,   383,   399,   400,   401,   402,   403,   404,   405,   406,
     407,   408,   410,   409,     0,   376,   339,   375,   337,     0,
      19,    21,    24,    32,   329,   338,    31,   371,   374,   377,
       0,    41,     0,    34,    38,   238,     0,     0,    83,     0,
       0,     0,    51,    52,    53,    78,     0,    84,     0,     0,
       0,     0,   198,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   416,   415,   421,   422,   423,   424,   198,
     198,    49,    56,    59,    60,    61,    91,     0,    63,   180,
      64,   206,   210,   220,   229,   231,   233,   299,   312,   300,
     111,   232,   374,   301,   414,   234,   102,     0,    23,     0,
       0,   390,   411,   413,   412,   391,     0,   388,     0,     0,
       0,   389,   392,    17,     0,    27,    22,    36,    36,     3,
      44,    33,     0,     0,     0,   203,   211,   204,     0,   367,
     368,   366,   345,   116,   346,   115,   137,   165,     0,     0,
       0,     0,   364,   344,   343,   341,   340,   100,     0,   114,
       0,    88,   123,   126,   129,   131,   135,   321,   132,   326,
     334,   342,   136,   133,   362,   365,   148,   286,   286,   227,
     214,     0,     0,     0,     0,     0,    95,    95,    98,     0,
       0,   123,     0,     0,     0,     0,   369,   349,   228,   219,
       0,     0,   199,     0,     0,     0,     0,     0,   306,   106,
     305,   303,     0,   277,   280,     0,   222,   208,     0,   420,
     313,     0,   419,   418,   239,   203,   244,     0,   245,   355,
       0,   356,   354,   360,   387,   386,   316,   417,   390,   308,
     430,     0,     0,   387,     0,   386,   316,     0,   310,     0,
       0,     0,     0,    50,     0,    57,     0,     0,     0,     0,
       0,     0,     0,   182,   100,   187,   352,     0,   353,   351,
     358,   385,   384,     0,   217,   293,     0,   103,   332,   333,
     331,   330,   373,   372,    20,     0,     0,    28,    30,     0,
       0,     0,    46,     0,     0,     0,     0,     0,   212,     0,
       0,   166,   168,   152,   325,     0,     0,   119,     0,   116,
     140,   149,     0,   334,     0,     0,     0,     0,     0,    69,
     138,     0,     0,   130,   149,     0,   147,     0,     0,     0,
     290,     0,     0,   285,   287,     0,   213,     0,    75,    74,
      76,    77,   144,   108,   100,     0,   183,    94,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   209,
     193,     0,     0,     0,     0,     0,   278,     0,   279,   181,
       0,     0,     0,   240,   246,     0,     0,   237,     0,   241,
     235,     0,   236,     0,   372,   302,   309,   429,   311,     0,
       0,     0,     0,   190,   348,    55,   347,   314,     0,    85,
     189,   120,   109,   110,   100,     0,   255,   257,     0,   185,
     186,   207,   218,   296,     0,   292,   295,   298,   221,    26,
      25,     0,     9,    10,    43,     0,     0,    40,    45,   216,
     215,     0,     0,   226,   202,   205,     0,   139,     0,     0,
     142,     0,   324,   328,   143,     0,   327,   322,   323,   335,
     363,    99,    87,   124,    62,     0,   291,   288,   276,     0,
     281,   284,   282,     0,    73,    96,    93,    97,   224,    70,
     370,   350,    68,    66,     0,   200,   192,   194,   304,   307,
       0,     0,     0,   107,   318,   191,   223,   357,   361,   317,
     248,   250,   254,   239,   252,   251,   243,   242,   197,     0,
       0,     0,     0,    90,     0,     0,   162,    72,   170,   184,
       0,     0,   359,   230,     0,     0,    29,     0,     0,     0,
     260,     0,   273,     0,   262,   266,     0,     0,   261,   169,
     167,     0,     0,   151,   153,   118,   150,   150,   289,   283,
     214,    92,    67,    65,   201,     0,   319,     0,   320,     0,
     247,   112,     0,     0,   315,    54,    89,   155,    86,   152,
     156,   158,     0,     0,    71,   171,   173,   188,   256,   294,
     297,     0,    47,   274,   263,   258,   265,     0,     0,   267,
     100,   271,   259,   117,     0,   141,     0,   253,   249,   195,
       0,   196,     0,   162,     0,   163,   127,   134,   160,    81,
      79,    80,     0,     0,   174,   177,   336,   172,    48,   264,
       0,   100,   269,   270,     0,     0,   113,   161,   157,     0,
       0,   128,     0,   178,   125,   145,     0,   175,     0,   176,
       0,   268,     0,   225,   163,   159,   164,     0,   179,    82,
     272,   154,   146
  };

  const short
  parser::yypgoto_[] =
  {
    -565,  -565,  -565,  -565,  -565,  -565,  -565,    26,  -565,  -565,
    -417,  -565,   398,  -565,  -565,  -565,  -143,   433,  -565,  -565,
    -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,
    -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,  -565,
     266,  -565,   337,  -565,  -193,  -283,   546,  -565,  -565,  -316,
    -565,  -168,    29,  -565,  -565,  -142,   174,   -61,  -565,   -84,
    -565,   -91,  -353,  -565,   363,  -524,  -192,   277,  -107,  -565,
     356,   -24,  -565,   -83,  -565,  -565,   -54,  -565,   -81,  -565,
    -565,   102,  -565,  -565,   -18,   -62,   549,    79,   341,  -565,
     285,  -565,   221,  -565,   -59,   -95,   558,   -26,  -282,    17,
    -565,   -68,   -71,  -565,  -565,   -99,  -565,  -565,  -565,  -565,
     -19,  -565,  -565,  -418,  -565,   -21,  -565,  -565,   -11,  -565,
    -565,   347,  -565,   -76,   376,    99,  -273,  -565,    47,  -565,
    -565,  -565,  -565,   212,  -565,   -88,  -564,   -89,  -565,   217,
    -565,  -565,  -565,  -565,   -29,  -565,  -176,  -565,    81,   526,
    -152,  -565,   -77,  -565,  -565,  -445,  -565,   439,   -70,   -27,
    -186,   -30,  -565,  -565,   -22,   -55,   -51,   -86,  -565,  -155,
    -100,   -53,  -229,  -565,  -305,    -2,  -102
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   169,     6,    10,    19,    30,
      69,    70,    71,   166,   326,   327,    72,    84,    11,    20,
      21,    32,    82,   332,   467,   468,   294,   121,   433,    33,
      34,   122,   123,   124,   125,   225,   643,   669,   126,   547,
     197,   297,   385,   228,   229,   359,    27,    36,   405,   382,
     440,   127,   592,   198,   199,   383,   442,   346,   634,   347,
     665,   202,   635,   203,   204,   636,   205,   384,   666,   365,
     352,   479,   573,   598,   548,   600,   601,   602,   638,   340,
     341,   342,   604,   605,   606,   644,   386,   387,   303,   304,
     305,   129,   241,   242,   370,   175,   388,   176,   177,   377,
     178,   132,   133,   134,   135,   281,   282,   267,   268,   531,
     445,   446,   473,   563,   564,   565,   619,   620,   621,   566,
     371,   254,   255,   219,   372,   373,   374,   454,   455,   456,
     136,   137,   248,   249,   138,   139,   434,   269,   523,   206,
     207,    73,   208,   645,   209,    75,   210,   211,   435,   436,
     307,   270,   271,   309,   272,   212,   213,   214,   140,   141,
      77,    78,   310,   273,   274,   312,   161,    79,   162,   143,
     144,   276,   277,   145,    24,     9,   287
  };

  const short
  parser::yytable_[] =
  {
      74,   215,    76,   201,   142,   251,   360,   231,   131,   160,
     289,   266,   215,   353,   230,   265,   265,   250,   402,   339,
     474,   449,   328,   337,   238,   463,   200,   174,   447,   235,
     239,    13,   220,    22,   574,   253,   256,   465,   258,   148,
     257,   306,   159,    22,   237,    22,   345,   351,     1,   558,
     358,   264,   264,   308,   567,   475,   284,   290,   286,   275,
     285,   314,   629,   639,   400,   437,   358,   631,   396,   236,
     502,   662,    25,   543,   364,   411,   420,   245,   265,   333,
     170,   146,   421,   500,   507,    18,   306,   354,   355,   283,
     334,   147,   640,   641,   298,  -369,   501,    26,   308,   630,
      12,   504,   215,   215,   630,   215,   412,   301,   311,   397,
     662,   661,   215,   413,   264,   401,   448,   215,   160,   338,
     286,   452,   285,   617,   544,   259,     2,   624,   299,  -369,
     215,   475,   466,    17,    23,    74,    74,    76,    76,   215,
     611,   240,   532,   614,    23,   501,    23,   412,   642,   476,
     661,   283,   661,   311,   279,   506,   441,   236,  -370,    66,
     280,   549,   356,    68,   246,   491,   262,   568,   247,     7,
     112,    29,   414,     8,   496,   419,   306,   392,   415,   113,
     642,   406,    31,   299,   253,   505,   314,   574,   308,   160,
      35,    66,  -370,   329,   330,    68,   142,   142,   506,   422,
     131,   131,   288,   393,   215,   423,   280,    80,   416,   426,
     257,   215,   215,   201,   398,   427,   167,   428,   168,   487,
     493,   427,   159,   591,   591,   427,   217,   215,   218,   443,
     151,   152,   153,   226,   488,   227,   200,   154,   427,   237,
     588,   444,   149,   311,   280,   458,   471,    37,   472,   441,
     215,   150,    38,   151,   152,   153,   617,    81,   618,   155,
     154,   259,   622,   157,   430,   431,    83,   337,   300,   236,
     231,   301,   151,   152,   153,   215,   215,   492,   608,   154,
     164,   451,   155,   156,    14,    15,   157,   158,   457,   520,
     165,   362,   497,   521,  -121,   522,   163,   432,   498,   306,
     216,   155,   262,   251,    66,   215,   674,   171,    68,   676,
     339,   308,   240,   437,   656,   250,   627,   534,   556,   535,
     243,   265,   536,   265,   537,   293,   265,   296,   265,   464,
     306,   508,   541,   315,   569,   545,   546,   652,   259,   575,
     291,   292,   308,   576,   447,   587,   317,   316,   526,   151,
     152,   153,   318,   577,   319,   530,   154,   533,   320,   264,
     321,   322,   264,   331,   264,   275,   311,   275,   671,   323,
     275,   335,   275,   358,   302,   361,   280,   376,   155,   262,
     378,   379,   157,   263,   380,   151,   152,   153,   381,   390,
     215,   391,   154,   215,   362,   215,   394,   311,   395,   215,
     399,   260,   393,   597,   403,   404,   437,   418,   424,   215,
     637,   438,   562,   562,   155,   461,   417,   182,   462,   354,
     355,  -389,   425,   459,   348,   460,   585,   469,   184,   623,
     470,   477,    74,   478,    76,   482,   480,    74,   481,    76,
     578,   483,   484,   637,   580,   437,   586,   486,   236,   485,
     494,   489,   280,   215,   215,   490,   610,   193,   194,   215,
     265,   195,   196,   221,   222,   223,   224,   637,   495,  -275,
     637,   499,   503,   510,   514,   511,   142,   515,   516,   512,
     131,   517,   637,   513,   637,   215,   215,   237,   613,   518,
     525,   607,   527,   528,   529,   539,   264,   259,   336,   538,
     540,   542,   562,   551,   275,   550,   553,   552,   151,   152,
     153,   554,   597,   555,   557,   154,   215,   589,   582,   590,
     584,   583,   596,   344,   594,   599,   603,   457,    74,   612,
      76,   628,   633,   302,   615,   625,   616,   155,   262,   648,
     654,   157,   263,   655,   215,   659,   215,   668,   670,   215,
     677,   231,   678,   113,   681,   295,   215,   562,   664,   651,
     541,    28,   324,   439,   389,   509,   363,   429,   215,   593,
     682,   672,   366,   215,   646,   632,   215,   231,   675,   658,
     570,   667,   215,   128,   679,   581,   231,   647,   215,   450,
     215,   215,   130,   664,   375,   649,   673,   626,   650,   409,
     579,   609,   408,    85,    39,    86,    87,    88,    89,   653,
      90,   680,    40,    91,   646,   519,    92,    93,    94,    95,
      96,   524,    97,   595,    42,   234,    98,     0,    99,    43,
     357,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,    54,    55,    56,     0,     0,    57,     0,     0,
     101,    58,    59,    60,    61,    62,    63,   102,     0,     0,
       0,     0,   103,   104,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   105,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,   107,     0,
     108,   109,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,     0,   111,     0,   112,
       0,     0,     0,     0,     0,     0,     0,    65,   113,     0,
       0,    67,   114,     0,     0,     0,     0,   115,   116,   117,
     118,     0,    85,    39,    86,     0,     0,   119,   120,    90,
       0,    40,    91,     0,     0,    92,    93,    94,     0,    96,
       0,     0,     0,    42,     0,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,    54,    55,    56,     0,     0,    57,     0,     0,   101,
      58,    59,    60,    61,    62,    63,   102,     0,     0,     0,
       0,   103,   104,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   105,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,   107,     0,   108,
     109,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,   111,     0,   112,     0,
       0,     0,     0,     0,     0,     0,    65,   113,     0,     0,
      67,   114,     0,     0,     0,     0,   115,   116,   117,   118,
      22,     0,    85,    39,    86,     0,   119,   120,     0,    90,
       0,    40,    91,     0,     0,     0,     0,     0,     0,    96,
       0,     0,     0,    42,     0,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,    54,    55,    56,     0,     0,    57,     0,     0,   101,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,   103,   172,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,   107,     0,   108,
     559,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    23,   110,     0,     0,     0,   173,     0,   112,     0,
       0,     0,   561,     0,     0,     0,    65,   113,     0,     0,
      67,   114,     0,    85,    39,    86,   115,   116,   117,   118,
      90,     0,    40,    91,     0,     0,     0,     0,     0,     0,
      96,     0,     0,     0,    42,     0,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,    54,    55,    56,     0,     0,    57,     0,     0,
     101,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,   103,   172,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   259,
       0,     0,   106,     0,     0,     0,     0,     0,   107,     0,
     278,   152,   153,     0,     0,     0,     0,   154,     0,     0,
       0,     0,     0,   110,     0,     0,     0,   173,   279,   112,
       0,     0,     0,     0,   280,   261,     0,    65,   113,   155,
     262,    67,   114,   157,   263,     0,     0,   115,   116,   117,
     118,    85,    39,    86,     0,     0,     0,     0,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,    42,     0,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
      54,    55,    56,     0,     0,    57,     0,     0,   101,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
     103,   172,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   259,     0,     0,
     106,     0,     0,     0,     0,     0,   107,     0,   108,   152,
     153,     0,     0,     0,     0,   154,     0,     0,     0,     0,
       0,   110,   260,     0,     0,   173,     0,   112,     0,     0,
       0,     0,     0,   261,     0,    65,   113,   155,   262,    67,
     114,   157,   263,     0,     0,   115,   116,   117,   118,    85,
      39,    86,     0,     0,     0,     0,    90,     0,    40,    91,
       0,     0,     0,     0,     0,     0,    96,     0,     0,     0,
      42,     0,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,    54,    55,
      56,     0,     0,    57,     0,     0,   101,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,   103,   172,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   259,     0,     0,   106,     0,
       0,     0,     0,     0,   107,     0,   108,   152,   153,     0,
       0,     0,     0,   154,     0,     0,     0,     0,     0,   110,
       0,     0,     0,   173,     0,   112,     0,     0,     0,     0,
       0,   261,     0,    65,   113,   155,   262,    67,   114,   157,
     263,     0,     0,   115,   116,   117,   118,    85,    39,    86,
       0,     0,     0,     0,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,    42,     0,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,   103,   172,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,   107,     0,   108,   559,     0,     0,     0,     0,
       0,     0,     0,     0,   560,     0,     0,   110,     0,     0,
       0,   173,     0,   112,     0,     0,     0,   561,     0,     0,
       0,    65,   113,     0,     0,    67,   114,     0,    85,    39,
      86,   115,   116,   117,   118,    90,     0,    40,    91,     0,
       0,     0,     0,     0,     0,   367,     0,     0,     0,    42,
       0,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,    54,    55,    56,
       0,   368,    57,     0,     0,   101,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,   103,   172,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,   107,     0,   108,   369,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   110,     0,
       0,     0,   173,     0,   112,     0,     0,     0,     0,     0,
       0,     0,    65,   113,     0,     0,    67,   114,     0,    85,
      39,    86,   115,   116,   117,   118,    90,     0,    40,    91,
       0,     0,     0,     0,     0,     0,    96,     0,     0,     0,
      42,     0,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,    54,    55,
      56,     0,     0,    57,     0,     0,   101,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,   103,   172,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,   107,     0,   108,   559,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,   173,     0,   112,     0,     0,     0,   561,
       0,     0,     0,    65,   113,     0,     0,    67,   114,     0,
      85,    39,    86,   115,   116,   117,   118,    90,     0,    40,
      91,     0,     0,     0,     0,     0,     0,   367,     0,     0,
       0,    42,     0,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,    54,
      55,    56,     0,     0,    57,     0,     0,   101,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,   103,
     172,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,   107,     0,   108,   369,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,   173,     0,   112,     0,     0,     0,
       0,     0,     0,     0,    65,   113,     0,     0,    67,   114,
       0,    85,    39,    86,   115,   116,   117,   118,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,    42,     0,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
      54,    55,    56,     0,     0,    57,     0,     0,   101,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
     103,   172,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,   107,     0,   108,   559,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,     0,   173,     0,   112,     0,     0,
       0,     0,     0,     0,     0,    65,   113,     0,     0,    67,
     114,     0,    85,    39,    86,   115,   116,   117,   118,    90,
       0,    40,    91,     0,     0,     0,     0,     0,     0,    96,
       0,     0,     0,    42,     0,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,    54,    55,    56,     0,     0,    57,     0,     0,   101,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,   103,   172,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,   107,     0,   108,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,   173,     0,   112,     0,
       0,     0,     0,     0,     0,     0,    65,   113,     0,     0,
      67,   114,     0,    85,    39,    86,   115,   116,   117,   118,
      90,     0,    40,    91,     0,     0,     0,     0,     0,     0,
      96,     0,     0,     0,    42,     0,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,    54,    55,    56,     0,     0,    57,     0,     0,
     101,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,   259,   103,   300,     0,     0,   301,     0,     0,     0,
       0,     0,   151,   152,   153,     0,     0,     0,     0,   154,
       0,     0,   106,     0,     0,     0,     0,     0,   107,     0,
     108,     0,     0,     0,     0,     0,     0,   302,     0,     0,
       0,   155,   262,   110,     0,   157,   263,   173,     0,   112,
       0,     0,     0,     0,     0,     0,     0,    65,   113,     0,
       0,    67,   114,     0,    85,    39,    86,   115,   116,   117,
     118,    90,     0,    40,    91,     0,     0,     0,     0,     0,
       0,    96,     0,     0,     0,    42,     0,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,   407,     0,   107,
       0,     0,   252,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   110,     0,     0,     0,   173,     0,
     112,     0,     0,     0,     0,     0,     0,     0,    65,   113,
       0,     0,    67,   114,     0,    85,    39,    86,   115,   116,
     117,   118,    90,     0,    40,    91,     0,     0,     0,     0,
       0,     0,    96,     0,     0,     0,    42,     0,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   100,
      50,    51,    52,    53,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
     107,     0,     0,   252,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,   173,
       0,   112,     0,     0,     0,     0,     0,     0,     0,    65,
     113,     0,     0,    67,   114,     0,    85,    39,    86,   115,
     116,   117,   118,    90,     0,    40,    91,     0,     0,     0,
       0,     0,     0,    96,     0,     0,     0,    42,     0,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,    54,    55,    56,     0,     0,
      57,     0,     0,   101,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,   107,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
     173,     0,   112,     0,     0,     0,     0,     0,     0,     0,
      65,   113,     0,     0,    67,   114,     0,    85,    39,    86,
     115,   116,   117,   118,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,    42,     0,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   313,     0,     0,     0,     0,   110,     0,     0,
       0,   173,     0,   112,     0,     0,     0,     0,     0,     0,
       0,    65,   113,     0,     0,    67,   114,     0,    85,    39,
      86,   115,   116,   117,   118,    90,     0,    40,    91,     0,
       0,     0,     0,     0,     0,    96,     0,     0,     0,    42,
       0,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,   107,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   110,     0,
       0,    39,   173,     0,   112,     0,     0,     0,     0,    40,
       0,     0,    65,   113,     0,     0,    67,   114,     0,     0,
       0,    42,   115,   116,   117,   118,   343,     0,    44,    45,
      46,   179,   180,   181,     0,     0,     0,    52,    53,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   182,     0,     0,     0,
       0,     0,     0,   348,     0,   349,     0,   184,   185,   186,
       0,     0,     0,     0,     0,     0,   187,     0,     0,     0,
     188,     0,    39,     0,   189,   350,   190,     0,     0,     0,
      40,   280,   191,     0,   192,    66,   193,   194,     0,    68,
     195,   196,    42,     0,     0,     0,     0,   343,     0,    44,
      45,    46,   179,   180,   181,     0,     0,     0,    52,    53,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   182,     0,     0,
       0,     0,     0,     0,     0,     0,   183,     0,   184,   185,
     186,     0,     0,     0,     0,     0,     0,   187,     0,     0,
       0,   188,   344,    39,     0,   189,     0,   190,     0,     0,
       0,    40,     0,   191,     0,   192,    66,   193,   194,     0,
      68,   195,   196,    42,     0,     0,     0,     0,   343,     0,
      44,    45,    46,   179,   180,   181,     0,     0,     0,    52,
      53,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   182,     0,
       0,     0,     0,     0,     0,     0,     0,   183,     0,   184,
     185,   186,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,   188,     0,    39,     0,   189,     0,   190,     0,
       0,     0,    40,     0,   191,     0,   192,    66,   193,   194,
       0,    68,   195,   196,    42,     0,     0,     0,     0,     0,
       0,    44,    45,    46,   179,   180,   181,     0,     0,     0,
      52,    53,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   182,
       0,     0,     0,     0,     0,     0,     0,     0,   183,     0,
     184,   185,   186,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,   188,     0,    39,     0,   189,   663,   190,
       0,     0,     0,    40,     0,   191,     0,   192,    66,   193,
     194,     0,    68,   195,   196,    42,     0,     0,     0,     0,
       0,     0,    44,    45,    46,   179,   180,   181,     0,     0,
       0,    52,    53,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     182,     0,     0,     0,     0,     0,     0,     0,     0,   183,
       0,   184,   185,   186,     0,     0,     0,     0,     0,     0,
     187,     0,     0,     0,   188,   410,    39,     0,   189,     0,
     190,     0,     0,     0,    40,     0,   191,     0,   192,    66,
     193,   194,     0,    68,   195,   196,    42,     0,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,     0,
      50,    51,    52,    53,    54,    55,    56,     0,     0,    57,
       0,    39,     0,    58,    59,    60,    61,    62,    63,    40,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    42,     0,     0,     0,     0,     0,     0,    44,    45,
      46,   179,   180,   181,     0,     0,     0,    52,    53,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    65,
     113,     0,     0,    67,   114,     0,   259,     0,     0,     0,
       0,     0,     0,     0,     0,   183,  -122,     0,   185,   186,
       0,     0,     0,    39,     0,     0,   187,     0,     0,     0,
     188,    40,     0,     0,   189,     0,   190,     0,     0,     0,
       0,     0,   660,    42,   192,    66,     0,   262,     0,    68,
      44,    45,    46,   179,   180,   181,     0,     0,     0,    52,
      53,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   259,     0,
       0,     0,     0,     0,     0,     0,     0,   183,     0,     0,
     185,   186,     0,     0,     0,    39,     0,     0,   187,     0,
       0,     0,   188,    40,     0,     0,   189,     0,   190,     0,
       0,     0,     0,     0,   660,    42,   192,    66,     0,   262,
       0,    68,    44,    45,    46,   179,   180,   181,     0,     0,
       0,    52,    53,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   183,
       0,     0,   185,   186,     0,     0,     0,    39,     0,     0,
     187,     0,     0,     0,   188,    40,     0,     0,   189,     0,
     190,     0,     0,     0,    41,     0,     0,    42,   192,    66,
       0,     0,    43,    68,    44,    45,    46,    47,    48,    49,
      39,    50,    51,    52,    53,    54,    55,    56,    40,     0,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
      42,     0,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,     0,    50,    51,    52,    53,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      64,     0,     0,     0,   325,     0,     0,     0,     0,     0,
      65,    66,     0,     0,    67,    68,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    39,     0,     0,     0,
       0,     0,     0,    64,    40,     0,     0,     0,     0,     0,
       0,     0,     0,    65,    66,     0,    42,    67,    68,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,    39,
      50,    51,    52,    53,    54,    55,    56,    40,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,    42,
       0,     0,     0,     0,     0,     0,    44,    45,    46,   179,
     180,   181,     0,     0,     0,    52,    53,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    65,
      66,     0,     0,    67,    68,     0,     0,     0,     0,     0,
       0,    39,     0,     0,     0,     0,     0,     0,     0,    40,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    42,   192,    66,     0,     0,    43,    68,    44,    45,
      46,    47,    48,    49,    39,    50,    51,    52,    53,    54,
      55,    56,    40,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,    42,     0,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,     0,    50,    51,
      52,    53,    54,    55,    56,   453,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,    39,     0,     0,     0,     0,     0,     0,     0,
      40,     0,     0,     0,   244,     0,     0,     0,     0,     0,
       0,     0,    42,     0,    65,     0,     0,    43,    67,    44,
      45,    46,    47,    48,    49,    39,    50,    51,    52,    53,
      54,    55,    56,    40,     0,    57,     0,   244,     0,    58,
      59,    60,    61,    62,    63,    42,     0,    65,     0,     0,
      43,    67,    44,    45,    46,    47,    48,    49,     0,    50,
      51,    52,    53,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,   151,   152,
     153,    39,     0,     0,     0,   154,     0,     0,     0,    40,
       0,     0,     0,     0,     0,   232,     0,     0,     0,     0,
       0,    42,     0,   233,     0,    65,    43,   155,    44,    45,
      46,    47,    48,    49,     0,    50,    51,    52,    53,    54,
      55,    56,     0,     0,    57,     0,    39,     0,    58,    59,
      60,    61,    62,    63,    40,     0,     0,     0,    65,   113,
       0,     0,     0,     0,     0,     0,    42,     0,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,    39,
      50,    51,    52,    53,    54,    55,    56,    40,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,    42,
       0,     0,     0,     0,   232,    39,    44,    45,    46,   179,
     180,   181,     0,    40,    65,    52,    53,    54,    55,    56,
       0,     0,    57,     0,     0,    42,    58,    59,    60,    61,
      62,    63,    44,    45,    46,   179,   180,   181,    39,     0,
       0,    52,    53,    54,    55,    56,    40,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,    42,    65,
       0,     0,     0,     0,     0,    44,    45,    46,   179,   180,
     181,     0,   571,     0,    52,    53,    54,    55,    56,     0,
       0,    57,   572,     0,     0,    58,    59,    60,    61,    62,
      63,     0,   192,     0,     0,     0,     0,     0,   657,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   572,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   192,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   192
  };

  const short
  parser::yycheck_[] =
  {
      29,    87,    29,    87,    34,   105,   198,    98,    34,    64,
     112,   110,    98,   189,    98,   110,   111,   105,   247,   187,
     336,   304,   165,   175,   100,   330,    87,    86,   301,    99,
     101,     5,    91,     1,   479,   106,   107,    12,   109,    41,
     108,   130,    64,     1,    99,     1,   188,   189,    21,   466,
      27,   110,   111,   130,   472,   337,   111,    19,   111,   110,
     111,   132,    77,    22,    77,   294,    27,    77,    88,    99,
     375,   635,   103,    77,   216,   261,   108,   104,   173,   117,
      82,   104,   114,   100,   389,   113,   175,   189,   190,   111,
     128,   114,    51,    52,    80,    80,   113,   128,   175,   114,
       0,   384,   188,   189,   114,   191,   261,    84,   130,   129,
     674,   635,   198,   265,   173,   128,   302,   203,   173,   178,
     173,   313,   173,    84,   128,    79,    99,   572,   114,   114,
     216,   413,   107,   100,   102,   164,   165,   164,   165,   225,
     557,   103,   415,   561,   102,   113,   102,   302,   107,    80,
     674,   173,   676,   175,   108,   113,   298,   187,    80,   118,
     114,   444,   191,   122,   103,   358,   120,   472,   107,   118,
     109,   107,    78,   122,   367,   270,   265,   232,    84,   118,
     107,   252,    14,   114,   255,   100,   257,   632,   265,   244,
     128,   118,   114,   167,   168,   122,   226,   227,   113,   108,
     226,   227,   110,   233,   290,   114,   114,    27,   114,   108,
     278,   297,   298,   297,   241,   114,    99,   110,   101,   108,
     362,   114,   244,   539,   540,   114,    99,   313,   101,   299,
      90,    91,    92,    99,   110,   101,   297,    97,   114,   294,
     110,   300,    79,   265,   114,   316,    99,    77,   101,   391,
     336,    88,    77,    90,    91,    92,    84,    24,    86,   119,
      97,    79,   567,   123,   291,   292,   113,   419,    81,   299,
     361,    84,    90,    91,    92,   361,   362,   361,   551,    97,
     114,   307,   119,   120,    65,    66,   123,   124,   315,   103,
     107,    86,   368,   107,    89,   109,   108,   115,   369,   388,
     107,   119,   120,   403,   118,   391,   659,   113,   122,   662,
     478,   388,   103,   542,   630,   403,   589,   416,   461,   418,
     128,   416,   421,   418,   423,   129,   421,    71,   423,   331,
     419,   390,   432,    99,   476,    80,    81,   620,    79,   481,
     119,   120,   419,   485,   617,   521,   128,    87,   407,    90,
      91,    92,   108,   495,   108,   414,    97,   416,   108,   418,
     108,   108,   421,     4,   423,   416,   388,   418,   651,   108,
     421,    23,   423,    27,   115,    89,   114,   113,   119,   120,
      77,    77,   123,   124,    77,    90,    91,    92,    77,    15,
     476,    81,    97,   479,    86,   481,   129,   419,   129,   485,
      77,   104,   432,   545,   114,    80,   635,   114,   108,   495,
     602,   128,   471,   472,   119,   114,   104,    79,   100,   521,
     522,   108,   108,   108,    86,   108,    88,    77,    90,   571,
      77,   100,   461,   114,   461,   108,   104,   466,    89,   466,
     499,   108,   108,   635,   503,   674,   108,   108,   478,   114,
     108,   115,   114,   539,   540,   115,   555,   119,   120,   545,
     555,   123,   124,    72,    73,    74,    75,   659,   114,    85,
     662,    85,    25,   108,   129,   115,   506,   104,    77,   128,
     506,    77,   674,   128,   676,   571,   572,   542,   559,   108,
      77,   550,   115,   115,   115,    80,   555,    79,    80,    77,
      80,   114,   561,   114,   555,    81,   100,   115,    90,    91,
      92,   114,   654,    81,   107,    97,   602,   114,   128,    78,
     104,   128,    77,   104,   115,    29,     9,   554,   557,   108,
     557,   590,    84,   115,   100,   108,   113,   119,   120,   108,
      80,   123,   124,    11,   630,    89,   632,    53,    86,   635,
     114,   642,   108,   118,   108,   122,   642,   616,   642,   618,
     660,    15,   164,   297,   227,   391,   203,   290,   654,   540,
     677,   654,   216,   659,   603,   599,   662,   668,   659,   633,
     478,   643,   668,    34,   668,   506,   677,   605,   674,   304,
     676,   677,    34,   677,   218,   616,   655,   580,   617,   258,
     501,   554,   255,     3,     4,     5,     6,     7,     8,   620,
      10,   670,    12,    13,   643,   403,    16,    17,    18,    19,
      20,   404,    22,   542,    24,    99,    26,    -1,    28,    29,
     191,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,
      -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,
     130,    -1,     3,     4,     5,    -1,    -1,   137,   138,    10,
      -1,    12,    13,    -1,    -1,    16,    17,    18,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,   130,
       1,    -1,     3,     4,     5,    -1,   137,   138,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   102,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,   113,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,     3,     4,     5,   127,   128,   129,   130,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    92,    -1,    -1,    -1,    -1,    97,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,    -1,   107,   108,   109,
      -1,    -1,    -1,    -1,   114,   115,    -1,   117,   118,   119,
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
      -1,   103,   104,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,    -1,    -1,   115,    -1,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,   127,   128,   129,   130,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,
      -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,
      -1,   115,    -1,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,   127,   128,   129,   130,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,    -1,    -1,   103,    -1,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,   113,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,     3,     4,
       5,   127,   128,   129,   130,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    46,    47,    -1,    -1,    50,    51,    52,    53,    54,
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
      -1,    -1,    -1,    -1,    88,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,   113,
      -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,
       3,     4,     5,   127,   128,   129,   130,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,
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
      -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,
     122,    -1,     3,     4,     5,   127,   128,   129,   130,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,     3,     4,     5,   127,   128,   129,   130,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    79,    62,    81,    -1,    -1,    84,    -1,    -1,    -1,
      -1,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,    97,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
      -1,   119,   120,   103,    -1,   123,   124,   107,    -1,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,
      -1,   121,   122,    -1,     3,     4,     5,   127,   128,   129,
     130,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    86,    -1,    88,
      -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      88,    -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,     3,     4,     5,   127,
     128,   129,   130,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    -1,    -1,    -1,    -1,   103,    -1,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,     3,     4,
       5,   127,   128,   129,   130,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,     4,   107,    -1,   109,    -1,    -1,    -1,    -1,    12,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,
      -1,    24,   127,   128,   129,   130,    29,    -1,    31,    32,
      33,    34,    35,    36,    -1,    -1,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    86,    -1,    88,    -1,    90,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,
     103,    -1,     4,    -1,   107,   108,   109,    -1,    -1,    -1,
      12,   114,   115,    -1,   117,   118,   119,   120,    -1,   122,
     123,   124,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    -1,    -1,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
      -1,   103,   104,     4,    -1,   107,    -1,   109,    -1,    -1,
      -1,    12,    -1,   115,    -1,   117,   118,   119,   120,    -1,
     122,   123,   124,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    -1,    -1,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,
      -1,    -1,   103,    -1,     4,    -1,   107,    -1,   109,    -1,
      -1,    -1,    12,    -1,   115,    -1,   117,   118,   119,   120,
      -1,   122,   123,   124,    24,    -1,    -1,    -1,    -1,    -1,
      -1,    31,    32,    33,    34,    35,    36,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,    -1,   103,    -1,     4,    -1,   107,   108,   109,
      -1,    -1,    -1,    12,    -1,   115,    -1,   117,   118,   119,
     120,    -1,   122,   123,   124,    24,    -1,    -1,    -1,    -1,
      -1,    -1,    31,    32,    33,    34,    35,    36,    -1,    -1,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,    -1,   103,     3,     4,    -1,   107,    -1,
     109,    -1,    -1,    -1,    12,    -1,   115,    -1,   117,   118,
     119,   120,    -1,   122,   123,   124,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    -1,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,     4,    -1,    51,    52,    53,    54,    55,    56,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    -1,    -1,    31,    32,
      33,    34,    35,    36,    -1,    -1,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    89,    -1,    91,    92,
      -1,    -1,    -1,     4,    -1,    -1,    99,    -1,    -1,    -1,
     103,    12,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,
      -1,    -1,   115,    24,   117,   118,    -1,   120,    -1,   122,
      31,    32,    33,    34,    35,    36,    -1,    -1,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,
      91,    92,    -1,    -1,    -1,     4,    -1,    -1,    99,    -1,
      -1,    -1,   103,    12,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,   115,    24,   117,   118,    -1,   120,
      -1,   122,    31,    32,    33,    34,    35,    36,    -1,    -1,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    -1,    91,    92,    -1,    -1,    -1,     4,    -1,    -1,
      99,    -1,    -1,    -1,   103,    12,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,    21,    -1,    -1,    24,   117,   118,
      -1,    -1,    29,   122,    31,    32,    33,    34,    35,    36,
       4,    38,    39,    40,    41,    42,    43,    44,    12,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    -1,    38,    39,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     107,    -1,    -1,    -1,    78,    -1,    -1,    -1,    -1,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,   107,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   117,   118,    -1,    24,   121,   122,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,     4,
      38,    39,    40,    41,    42,    43,    44,    12,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    24,
      -1,    -1,    -1,    -1,    -1,    -1,    31,    32,    33,    34,
      35,    36,    -1,    -1,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,
      -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    24,   117,   118,    -1,    -1,    29,   122,    31,    32,
      33,    34,    35,    36,     4,    38,    39,    40,    41,    42,
      43,    44,    12,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    -1,    38,    39,
      40,    41,    42,    43,    44,    78,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      12,    -1,    -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    24,    -1,   117,    -1,    -1,    29,   121,    31,
      32,    33,    34,    35,    36,     4,    38,    39,    40,    41,
      42,    43,    44,    12,    -1,    47,    -1,   107,    -1,    51,
      52,    53,    54,    55,    56,    24,    -1,   117,    -1,    -1,
      29,   121,    31,    32,    33,    34,    35,    36,    -1,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    90,    91,
      92,     4,    -1,    -1,    -1,    97,    -1,    -1,    -1,    12,
      -1,    -1,    -1,    -1,    -1,   107,    -1,    -1,    -1,    -1,
      -1,    24,    -1,   115,    -1,   117,    29,   119,    31,    32,
      33,    34,    35,    36,    -1,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,     4,    -1,    51,    52,
      53,    54,    55,    56,    12,    -1,    -1,    -1,   117,   118,
      -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,     4,
      38,    39,    40,    41,    42,    43,    44,    12,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    24,
      -1,    -1,    -1,    -1,   107,     4,    31,    32,    33,    34,
      35,    36,    -1,    12,   117,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    24,    51,    52,    53,    54,
      55,    56,    31,    32,    33,    34,    35,    36,     4,    -1,
      -1,    40,    41,    42,    43,    44,    12,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    24,   117,
      -1,    -1,    -1,    -1,    -1,    31,    32,    33,    34,    35,
      36,    -1,    97,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    47,   107,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,   117,    -1,    -1,    -1,    -1,    -1,    97,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117
  };

  const short
  parser::yystos_[] =
  {
       0,    21,    99,   140,   141,   142,   145,   118,   122,   314,
     146,   157,     0,   146,    65,    66,   143,   100,   113,   147,
     158,   159,     1,   102,   313,   103,   128,   185,   185,   107,
     148,    14,   160,   168,   169,   128,   186,    77,    77,     4,
      12,    21,    24,    29,    31,    32,    33,    34,    35,    36,
      38,    39,    40,    41,    42,    43,    44,    47,    51,    52,
      53,    54,    55,    56,   107,   117,   118,   121,   122,   149,
     150,   151,   155,   280,   283,   284,   298,   299,   300,   306,
      27,    24,   161,   113,   156,     3,     5,     6,     7,     8,
      10,    13,    16,    17,    18,    19,    20,    22,    26,    28,
      37,    50,    57,    62,    63,    76,    82,    88,    90,    91,
     103,   107,   109,   118,   122,   127,   128,   129,   130,   137,
     138,   166,   170,   171,   172,   173,   177,   190,   225,   230,
     235,   236,   240,   241,   242,   243,   269,   270,   273,   274,
     297,   298,   300,   308,   309,   312,   104,   114,   314,    79,
      88,    90,    91,    92,    97,   119,   120,   123,   124,   303,
     304,   305,   307,   108,   114,   107,   152,    99,   101,   144,
     314,   113,    63,   107,   233,   234,   236,   237,   239,    34,
      35,    36,    79,    88,    90,    91,    92,    99,   103,   107,
     109,   115,   117,   119,   120,   123,   124,   179,   192,   193,
     196,   198,   200,   202,   203,   205,   278,   279,   281,   283,
     285,   286,   294,   295,   296,   306,   107,    99,   101,   262,
     233,    72,    73,    74,    75,   174,    99,   101,   182,   183,
     198,   200,   107,   115,   288,   297,   300,   304,   262,   241,
     103,   231,   232,   128,   107,   298,   103,   107,   271,   272,
     274,   309,    91,   241,   260,   261,   241,   240,   241,    79,
     104,   115,   120,   124,   233,   234,   244,   246,   247,   276,
     290,   291,   293,   302,   303,   305,   310,   311,    90,   108,
     114,   244,   245,   303,   304,   305,   310,   315,   110,   315,
      19,   231,   231,   129,   165,   156,    71,   180,    80,   114,
      81,    84,   115,   227,   228,   229,   276,   289,   291,   292,
     301,   303,   304,    98,   241,    99,    87,   128,   108,   108,
     108,   108,   108,   108,   151,    78,   153,   154,   155,   146,
     146,     4,   162,   117,   128,    23,    80,   289,   233,   190,
     218,   219,   220,    29,   104,   194,   196,   198,    86,    88,
     108,   194,   209,   285,   315,   315,   283,   296,    27,   184,
     205,    89,    86,   203,   194,   208,   209,    20,    46,    91,
     233,   259,   263,   264,   265,   263,   113,   238,    77,    77,
      77,    77,   188,   194,   206,   181,   225,   226,   235,   181,
      15,    81,   304,   300,   129,   129,    88,   129,   298,    77,
      77,   128,   311,   114,    80,   187,   241,    86,   260,   227,
       3,   299,   308,   289,    78,    84,   114,   104,   114,   234,
     108,   114,   108,   114,   108,   108,   108,   114,   110,   206,
     298,   298,   115,   167,   275,   287,   288,   311,   128,   179,
     189,   194,   195,   297,   233,   249,   250,   265,   299,   184,
     229,   236,   205,    78,   266,   267,   268,   298,   241,   108,
     108,   114,   100,   313,   314,    12,   107,   163,   164,    77,
      77,    99,   101,   251,   188,   237,    80,   100,   114,   210,
     104,    89,   108,   108,   108,   114,   108,   108,   110,   115,
     115,   183,   198,   194,   108,   114,   183,   262,   241,    85,
     100,   113,   313,    25,   184,   100,   113,   313,   233,   195,
     108,   115,   128,   128,   129,   104,    77,    77,   108,   272,
     103,   107,   109,   277,   278,    77,   233,   115,   115,   115,
     233,   248,   265,   233,   244,   244,   244,   244,    77,    80,
      80,   309,   114,    77,   128,    80,    81,   178,   213,   184,
      81,   114,   115,   100,   114,    81,   155,   107,   149,    91,
     100,   113,   233,   252,   253,   254,   258,   252,   313,   194,
     220,    97,   107,   211,   294,   194,   194,   194,   233,   264,
     233,   226,   128,   128,   104,    88,   108,   285,   110,   114,
      78,   188,   191,   191,   115,   287,    77,   194,   212,    29,
     214,   215,   216,     9,   221,   222,   223,   233,   265,   267,
     244,   149,   108,   241,   252,   100,   113,    84,    86,   255,
     256,   257,   313,   194,   294,   108,   238,   265,   233,    77,
     114,    77,   210,    84,   197,   201,   204,   205,   217,    22,
      51,    52,   107,   175,   224,   282,   283,   223,   108,   254,
     249,   233,   184,   257,    80,    11,   188,    97,   215,    89,
     115,   204,   275,   108,   198,   199,   207,   224,    53,   176,
      86,   184,   212,   233,   201,   217,   201,   114,   108,   198,
     233,   108,   207
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
     288,   289,   289,   289,   290,   290,   290,   291,   292,   292,
     293,   293,   294,   295,   296,   296,   296,   296,   296,   297,
     297,   298,   298,   298,   299,   299,   300,   300,   300,   300,
     300,   300,   300,   300,   301,   301,   302,   302,   303,   304,
     304,   305,   305,   306,   306,   306,   306,   306,   306,   306,
     306,   306,   306,   306,   306,   306,   306,   306,   306,   306,
     306,   307,   307,   307,   308,   308,   309,   310,   310,   311,
     311,   312,   312,   312,   312,   313,   313,   314,   314,   315,
     315
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
       3,     1,     1,     1,     1,     1,     1,     3,     1,     3,
       1,     3,     1,     3,     1,     1,     1,     1,     1,     1,
       3,     1,     3,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       1
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
       0,   477,   477,   494,   495,   497,   501,   502,   503,   505,
     506,   508,   509,   512,   514,   515,   516,   524,   525,   527,
     529,   530,   532,   533,   535,   536,   537,   539,   540,   542,
     543,   545,   546,   550,   551,   553,   554,   556,   558,   559,
     561,   574,   575,   577,   578,   580,   581,   585,   586,   591,
     592,   594,   595,   596,   598,   599,   603,   605,   606,   608,
     609,   610,   613,   620,   622,   623,   624,   625,   626,   628,
     630,   631,   632,   637,   642,   643,   644,   645,   646,   648,
     649,   650,   652,   692,   693,   695,   696,   705,   706,   708,
     709,   710,   754,   755,   756,   757,   759,   760,   762,   764,
     765,   773,   774,   776,   777,   778,   791,   792,   794,   796,
     798,   799,   801,   802,   806,   812,   813,   820,   821,   823,
     825,   834,   836,   838,   839,   841,   844,   846,   847,   849,
     850,   852,   853,   854,   860,   867,   868,   869,   870,   871,
     872,   873,   879,   880,   884,   886,   887,   889,   890,   892,
     893,   900,   901,   904,   905,   923,   929,   931,   932,   934,
     935,   937,   938,   940,   941,   943,   944,   946,   947,   949,
     951,   952,   954,   955,   957,   958,   959,   961,   962,   963,
     968,   970,   971,   973,   977,   978,   980,   981,   985,   995,
     996,   998,   999,  1000,  1001,  1002,  1003,  1004,  1007,  1008,
    1010,  1011,  1015,  1016,  1018,  1019,  1021,  1022,  1024,  1025,
    1026,  1028,  1029,  1032,  1033,  1035,  1036,  1040,  1041,  1042,
    1043,  1045,  1046,  1047,  1048,  1050,  1052,  1053,  1054,  1056,
    1058,  1059,  1061,  1062,  1063,  1064,  1065,  1070,  1071,  1076,
    1077,  1078,  1083,  1084,  1102,  1103,  1104,  1105,  1106,  1107,
    1108,  1110,  1111,  1124,  1126,  1136,  1138,  1139,  1142,  1143,
    1144,  1145,  1147,  1148,  1150,  1151,  1152,  1154,  1156,  1157,
    1159,  1160,  1169,  1171,  1172,  1174,  1175,  1177,  1178,  1180,
    1181,  1184,  1185,  1187,  1188,  1189,  1190,  1195,  1196,  1198,
    1199,  1200,  1205,  1206,  1208,  1209,  1210,  1212,  1213,  1245,
    1246,  1248,  1249,  1251,  1252,  1253,  1255,  1256,  1258,  1259,
    1260,  1261,  1263,  1264,  1266,  1267,  1269,  1270,  1273,  1274,
    1275,  1277,  1278,  1279,  1280,  1281,  1283,  1284,  1285,  1287,
    1288,  1289,  1290,  1291,  1294,  1295,  1297,  1299,  1300,  1304,
    1306,  1307,  1308,  1310,  1311,  1312,  1313,  1318,  1319,  1321,
    1322,  1324,  1325,  1326,  1328,  1329,  1330,  1332,  1334,  1335,
    1337,  1338,  1342,  1344,  1346,  1347,  1348,  1349,  1350,  1353,
    1354,  1356,  1357,  1358,  1360,  1361,  1363,  1364,  1365,  1366,
    1367,  1368,  1369,  1370,  1372,  1373,  1375,  1376,  1378,  1380,
    1381,  1383,  1384,  1386,  1387,  1388,  1389,  1390,  1391,  1392,
    1393,  1394,  1395,  1396,  1397,  1398,  1399,  1400,  1401,  1402,
    1403,  1405,  1406,  1407,  1411,  1412,  1414,  1416,  1417,  1419,
    1420,  1424,  1425,  1426,  1427,  1432,  1435,  1439,  1440,  1442,
    1443
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
#line 6030 "parser.cc"

#line 1452 "parser.y"


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
        auto topdecls2 = Haskell::Decls(*topdecls, true);
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

Haskell::InstanceDecl make_instance_decl(const Located<expression_ref>& ltype, const optional<Located<Haskell::Decls>>& decls)
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
    return {context, name, type_args, decls};
}

Haskell::ClassDecl make_class_decl(const Haskell::Context& context, const expression_ref& header, const optional<Located<Haskell::Decls>>& decls)
{
    auto [name, type_args] = check_type_or_class_header(header);
    return {name, check_all_type_vars(type_args), context, decls};
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

expression_ref make_tyapps(const std::vector<expression_ref>& tyapps)
{
    assert(not tyapps.empty());
    expression_ref E = tyapps[0];
    for(int i=1;i<tyapps.size();i++)
	E = Haskell::TypeApp(E,tyapps[i]);
    return E;
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

expression_ref make_typed_exp(const expression_ref& exp, const expression_ref& type)
{
    return new expression(AST_node("typed_exp"),{exp,type});
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

expression_ref yy_make_string(const std::string& s)
{
    vector<expression_ref> chars;
    for(char c: s)
	chars.push_back(c);
    return Haskell::List(chars);
}

