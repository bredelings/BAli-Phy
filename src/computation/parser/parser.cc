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
        value.YY_MOVE_OR_COPY< std::pair<Hs::Context,expression_ref> > (YY_MOVE (that.value));
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
        value.move< std::pair<Hs::Context,expression_ref> > (YY_MOVE (that.value));
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
        value.copy< std::pair<Hs::Context,expression_ref> > (that.value);
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
        value.move< std::pair<Hs::Context,expression_ref> > (that.value);
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
        yylhs.value.emplace< std::pair<Hs::Context,expression_ref> > ();
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
#line 475 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2056 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 492 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < std::string > (),yystack_[2].value.as < std::optional<std::vector<Hs::Export>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2062 "parser.cc"
    break;

  case 4: // module: body2
#line 493 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{"Main",{},yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2068 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 495 "parser.y"
                                                                 {drv.push_module_context();}
#line 2074 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 503 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2080 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 504 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2086 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 506 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2092 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 507 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2098 "parser.cc"
    break;

  case 13: // top: semis top1
#line 510 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2104 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 512 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2110 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 513 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2116 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 514 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::ImpDecl> > (),{});}
#line 2122 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 522 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::Export>> > () = yystack_[1].value.as < std::vector<Hs::Export> > ();}
#line 2128 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 523 "parser.y"
                                      {}
#line 2134 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 525 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > () = yystack_[0].value.as < std::vector<Hs::Export> > ();}
#line 2140 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 527 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > () = yystack_[2].value.as < std::vector<Hs::Export> > (); yylhs.value.as < std::vector<Hs::Export> > ().push_back(yystack_[0].value.as < Hs::Export > ());}
#line 2146 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 528 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > ().push_back(yystack_[0].value.as < Hs::Export > ());}
#line 2152 "parser.cc"
    break;

  case 22: // export: qcname export_subspec
#line 530 "parser.y"
                                      {yylhs.value.as < Hs::Export > () = Hs::ExportSymbol{yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}; }
#line 2158 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 531 "parser.y"
                                      {yylhs.value.as < Hs::Export > () = Hs::ExportModule{{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2164 "parser.cc"
    break;

  case 24: // export_subspec: %empty
#line 533 "parser.y"
                                      {}
#line 2170 "parser.cc"
    break;

  case 25: // export_subspec: "(" qcnames ")"
#line 534 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpecSome{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2176 "parser.cc"
    break;

  case 26: // export_subspec: "(" ".." ")"
#line 535 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpecAll(); }
#line 2182 "parser.cc"
    break;

  case 27: // qcnames: %empty
#line 537 "parser.y"
                   {}
#line 2188 "parser.cc"
    break;

  case 28: // qcnames: qcnames1
#line 538 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2194 "parser.cc"
    break;

  case 29: // qcnames1: qcnames1 "," qcname
#line 540 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2200 "parser.cc"
    break;

  case 30: // qcnames1: qcname
#line 541 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2206 "parser.cc"
    break;

  case 31: // qcname: qvar
#line 543 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2212 "parser.cc"
    break;

  case 32: // qcname: oqtycon_no_varcon
#line 544 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2218 "parser.cc"
    break;

  case 37: // importdecls: importdecls_semi importdecl
#line 554 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[1].value.as < std::vector<Hs::ImpDecl> > (), yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[0].value.as < Hs::ImpDecl > ()); }
#line 2224 "parser.cc"
    break;

  case 38: // importdecls_semi: importdecls_semi importdecl semis1
#line 556 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[2].value.as < std::vector<Hs::ImpDecl> > (); yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[1].value.as < Hs::ImpDecl > ()); }
#line 2230 "parser.cc"
    break;

  case 39: // importdecls_semi: %empty
#line 557 "parser.y"
                         { }
#line 2236 "parser.cc"
    break;

  case 40: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 559 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::ImpDecl > () = Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < std::string > (),yystack_[1].value.as < std::optional<std::string> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ());
}
#line 2244 "parser.cc"
    break;

  case 41: // optqualified: "qualified"
#line 572 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2250 "parser.cc"
    break;

  case 42: // optqualified: %empty
#line 573 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2256 "parser.cc"
    break;

  case 43: // maybeas: "as" modid
#line 575 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 2262 "parser.cc"
    break;

  case 44: // maybeas: %empty
#line 576 "parser.y"
                               { }
#line 2268 "parser.cc"
    break;

  case 45: // maybeimpspec: impspec
#line 578 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2274 "parser.cc"
    break;

  case 46: // maybeimpspec: %empty
#line 579 "parser.y"
                               { }
#line 2280 "parser.cc"
    break;

  case 47: // impspec: "(" exportlist ")"
#line 583 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::Export> > ()}; }
#line 2286 "parser.cc"
    break;

  case 48: // impspec: "hiding" "(" exportlist ")"
#line 584 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::Export> > ()}; }
#line 2292 "parser.cc"
    break;

  case 49: // prec: %empty
#line 589 "parser.y"
                   { }
#line 2298 "parser.cc"
    break;

  case 50: // prec: "INTEGER"
#line 590 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < int > (); }
#line 2304 "parser.cc"
    break;

  case 51: // infix: "infix"
#line 592 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2310 "parser.cc"
    break;

  case 52: // infix: "infixl"
#line 593 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2316 "parser.cc"
    break;

  case 53: // infix: "infixr"
#line 594 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2322 "parser.cc"
    break;

  case 54: // ops: ops "," op
#line 596 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 2328 "parser.cc"
    break;

  case 55: // ops: op
#line 597 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 2334 "parser.cc"
    break;

  case 56: // topdecls: topdecls_semi topdecl
#line 601 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 2340 "parser.cc"
    break;

  case 57: // topdecls_semi: topdecls_semi topdecl semis1
#line 603 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2346 "parser.cc"
    break;

  case 58: // topdecls_semi: %empty
#line 604 "parser.y"
                                            { }
#line 2352 "parser.cc"
    break;

  case 59: // topdecl: cl_decl
#line 606 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2358 "parser.cc"
    break;

  case 60: // topdecl: ty_decl
#line 607 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2364 "parser.cc"
    break;

  case 61: // topdecl: inst_decl
#line 608 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2370 "parser.cc"
    break;

  case 62: // topdecl: "default" "(" comma_types0 ")"
#line 611 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Hs::DefaultDecl(yystack_[1].value.as < std::vector<expression_ref> > ()); }
#line 2376 "parser.cc"
    break;

  case 63: // topdecl: decl_no_th
#line 618 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2382 "parser.cc"
    break;

  case 64: // topdecl: infixexp_top
#line 620 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2388 "parser.cc"
    break;

  case 65: // topdecl: "builtin" var "INTEGER" "STRING"
#line 621 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Hs::BuiltinDecl(yystack_[0].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[2].value.as < std::string > ());}
#line 2394 "parser.cc"
    break;

  case 66: // cl_decl: "class" tycl_hdr wherebinds
#line 623 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_class_decl(yystack_[1].value.as < std::pair<Hs::Context,expression_ref> > ().first,yystack_[1].value.as < std::pair<Hs::Context,expression_ref> > ().second,yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 2400 "parser.cc"
    break;

  case 67: // ty_decl: "type" type "=" ctypedoc
#line 625 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_type_synonym({yystack_[2].location,yystack_[2].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2406 "parser.cc"
    break;

  case 68: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 626 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (),yystack_[2].value.as < std::pair<Hs::Context,expression_ref> > ().first,yystack_[2].value.as < std::pair<Hs::Context,expression_ref> > ().second,yystack_[1].value.as < std::vector<Hs::Constructor> > ());}
#line 2412 "parser.cc"
    break;

  case 69: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig
#line 627 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[3].value.as < Hs::DataOrNewtype > (),yystack_[1].value.as < std::pair<Hs::Context,expression_ref> > ().first,yystack_[1].value.as < std::pair<Hs::Context,expression_ref> > ().second,{});}
#line 2418 "parser.cc"
    break;

  case 70: // inst_decl: "instance" overlap_pragma inst_type wherebinds
#line 632 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_instance_decl({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 2424 "parser.cc"
    break;

  case 80: // data_or_newtype: "data"
#line 687 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 2430 "parser.cc"
    break;

  case 81: // data_or_newtype: "newtype"
#line 688 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 2436 "parser.cc"
    break;

  case 84: // tycl_hdr: context "=>" type
#line 700 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,expression_ref> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < expression_ref > ()};}
#line 2442 "parser.cc"
    break;

  case 85: // tycl_hdr: type
#line 701 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,expression_ref> > () = {{},yystack_[0].value.as < expression_ref > ()};}
#line 2448 "parser.cc"
    break;

  case 89: // decls: decls ";" decl
#line 749 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2454 "parser.cc"
    break;

  case 90: // decls: decls ";"
#line 750 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2460 "parser.cc"
    break;

  case 91: // decls: decl
#line 751 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2466 "parser.cc"
    break;

  case 92: // decls: %empty
#line 752 "parser.y"
                        {}
#line 2472 "parser.cc"
    break;

  case 93: // decllist: "{" decls "}"
#line 754 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2478 "parser.cc"
    break;

  case 94: // decllist: "vocurly" decls close
#line 755 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2484 "parser.cc"
    break;

  case 95: // binds: decllist
#line 757 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 2490 "parser.cc"
    break;

  case 96: // wherebinds: "where" binds
#line 759 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 2496 "parser.cc"
    break;

  case 97: // wherebinds: %empty
#line 760 "parser.y"
                                 {}
#line 2502 "parser.cc"
    break;

  case 103: // opt_tyconsig: %empty
#line 786 "parser.y"
                     {}
#line 2508 "parser.cc"
    break;

  case 104: // opt_tyconsig: "::" gtycon
#line 787 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2514 "parser.cc"
    break;

  case 105: // sigtype: ctype
#line 789 "parser.y"
                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2520 "parser.cc"
    break;

  case 106: // sigtypedoc: ctypedoc
#line 791 "parser.y"
                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2526 "parser.cc"
    break;

  case 107: // sig_vars: sig_vars "," var
#line 793 "parser.y"
                           {yylhs.value.as < std::vector<Hs::Var> > () = yystack_[2].value.as < std::vector<Hs::Var> > (); yylhs.value.as < std::vector<Hs::Var> > ().push_back(Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}));}
#line 2532 "parser.cc"
    break;

  case 108: // sig_vars: var
#line 794 "parser.y"
                           {yylhs.value.as < std::vector<Hs::Var> > ().push_back(Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}));}
#line 2538 "parser.cc"
    break;

  case 109: // sigtypes1: sigtype
#line 796 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2544 "parser.cc"
    break;

  case 110: // sigtypes1: sigtypes1 "," sigtype
#line 797 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2550 "parser.cc"
    break;

  case 111: // strict_mark: strictness
#line 801 "parser.y"
                                            {yylhs.value.as < Hs::StrictLazy > () = yystack_[0].value.as < Hs::StrictLazy > ();}
#line 2556 "parser.cc"
    break;

  case 112: // strictness: "!"
#line 807 "parser.y"
                {yylhs.value.as < Hs::StrictLazy > () = Hs::StrictLazy::strict;}
#line 2562 "parser.cc"
    break;

  case 113: // strictness: "~"
#line 808 "parser.y"
                {yylhs.value.as < Hs::StrictLazy > () = Hs::StrictLazy::lazy;}
#line 2568 "parser.cc"
    break;

  case 114: // ctype: "forall" tv_bndrs "." ctype
#line 815 "parser.y"
                                   {yylhs.value.as < expression_ref > () = Hs::ForallType(yystack_[2].value.as < std::vector<Hs::TypeVar> > (), yystack_[0].value.as < expression_ref > ());}
#line 2574 "parser.cc"
    break;

  case 115: // ctype: context "=>" ctype
#line 816 "parser.y"
                                   {yylhs.value.as < expression_ref > () = Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < expression_ref > ());}
#line 2580 "parser.cc"
    break;

  case 116: // ctype: type
#line 818 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2586 "parser.cc"
    break;

  case 117: // ctypedoc: ctype
#line 820 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2592 "parser.cc"
    break;

  case 118: // context: btype
#line 829 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < expression_ref > ());}
#line 2598 "parser.cc"
    break;

  case 119: // context_no_ops: btype_no_ops
#line 831 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2604 "parser.cc"
    break;

  case 120: // type: btype
#line 833 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2610 "parser.cc"
    break;

  case 121: // type: btype "->" ctype
#line 834 "parser.y"
                                   {yylhs.value.as < expression_ref > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,"->"}),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()});}
#line 2616 "parser.cc"
    break;

  case 122: // typedoc: type
#line 836 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2622 "parser.cc"
    break;

  case 123: // btype: tyapps
#line 839 "parser.y"
                                   {yylhs.value.as < expression_ref > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2628 "parser.cc"
    break;

  case 124: // btype_no_ops: atype_docs
#line 841 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2634 "parser.cc"
    break;

  case 125: // btype_no_ops: btype_no_ops atype_docs
#line 842 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2640 "parser.cc"
    break;

  case 126: // tyapps: tyapp
#line 844 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2646 "parser.cc"
    break;

  case 127: // tyapps: tyapps tyapp
#line 845 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2652 "parser.cc"
    break;

  case 128: // tyapp: atype
#line 847 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2658 "parser.cc"
    break;

  case 129: // tyapp: qtyconop
#line 848 "parser.y"
                                   {yylhs.value.as < expression_ref > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2664 "parser.cc"
    break;

  case 130: // tyapp: tyvarop
#line 849 "parser.y"
                                   {yylhs.value.as < expression_ref > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2670 "parser.cc"
    break;

  case 131: // atype_docs: atype
#line 855 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2676 "parser.cc"
    break;

  case 132: // atype: ntgtycon
#line 862 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2682 "parser.cc"
    break;

  case 133: // atype: tyvar
#line 863 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2688 "parser.cc"
    break;

  case 134: // atype: "*"
#line 864 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Hs::TypeCon({yystack_[0].location,"*"});}
#line 2694 "parser.cc"
    break;

  case 135: // atype: strict_mark atype
#line 865 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Hs::StrictLazyType(yystack_[1].value.as < Hs::StrictLazy > (),yystack_[0].value.as < expression_ref > ());}
#line 2700 "parser.cc"
    break;

  case 136: // atype: "{" fielddecls "}"
#line 866 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ());}
#line 2706 "parser.cc"
    break;

  case 137: // atype: "(" ")"
#line 867 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Hs::TypeCon({yystack_[1].location,"()"});}
#line 2712 "parser.cc"
    break;

  case 138: // atype: "(" comma_types1 "," ctype ")"
#line 868 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<expression_ref> > ();ts.push_back(yystack_[1].value.as < expression_ref > ());yylhs.value.as < expression_ref > () = Hs::TupleType(ts);}
#line 2718 "parser.cc"
    break;

  case 139: // atype: "[" ctype "]"
#line 874 "parser.y"
                                       {yylhs.value.as < expression_ref > () = Hs::ListType{yystack_[1].value.as < expression_ref > ()}; }
#line 2724 "parser.cc"
    break;

  case 140: // atype: "(" ctype ")"
#line 875 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2730 "parser.cc"
    break;

  case 141: // inst_type: sigtype
#line 879 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2736 "parser.cc"
    break;

  case 144: // comma_types0: comma_types1
#line 884 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2742 "parser.cc"
    break;

  case 145: // comma_types0: %empty
#line 885 "parser.y"
                                       { /* default construction OK */ }
#line 2748 "parser.cc"
    break;

  case 146: // comma_types1: ctype
#line 887 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2754 "parser.cc"
    break;

  case 147: // comma_types1: comma_types1 "," ctype
#line 888 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2760 "parser.cc"
    break;

  case 148: // tv_bndrs: tv_bndrs tv_bndr
#line 895 "parser.y"
                               {yylhs.value.as < std::vector<Hs::TypeVar> > () = yystack_[1].value.as < std::vector<Hs::TypeVar> > (); yylhs.value.as < std::vector<Hs::TypeVar> > ().push_back(yystack_[0].value.as < Hs::TypeVar > ());}
#line 2766 "parser.cc"
    break;

  case 149: // tv_bndrs: %empty
#line 896 "parser.y"
                               { /* default construction OK */}
#line 2772 "parser.cc"
    break;

  case 150: // tv_bndr: tyvar
#line 899 "parser.y"
                                    {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2778 "parser.cc"
    break;

  case 151: // tv_bndr: "(" tyvar "::" kind ")"
#line 900 "parser.y"
                                    {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[3].location,yystack_[3].value.as < std::string > ()},yystack_[1].value.as < expression_ref > ());}
#line 2784 "parser.cc"
    break;

  case 152: // kind: ctype
#line 918 "parser.y"
             {yylhs.value.as < expression_ref > () = make_kind(yystack_[0].value.as < expression_ref > ());}
#line 2790 "parser.cc"
    break;

  case 153: // constrs: "=" constrs1
#line 924 "parser.y"
                                {yylhs.value.as < std::vector<Hs::Constructor> > () = yystack_[0].value.as < std::vector<Hs::Constructor> > ();}
#line 2796 "parser.cc"
    break;

  case 154: // constrs1: constrs1 "|" constr
#line 926 "parser.y"
                                {yylhs.value.as < std::vector<Hs::Constructor> > () = yystack_[2].value.as < std::vector<Hs::Constructor> > (); yylhs.value.as < std::vector<Hs::Constructor> > ().push_back(yystack_[0].value.as < Hs::Constructor > ());}
#line 2802 "parser.cc"
    break;

  case 155: // constrs1: constr
#line 927 "parser.y"
                                {yylhs.value.as < std::vector<Hs::Constructor> > ().push_back(yystack_[0].value.as < Hs::Constructor > ());}
#line 2808 "parser.cc"
    break;

  case 156: // constr: forall context_no_ops "=>" constr_stuff
#line 929 "parser.y"
                                                {yylhs.value.as < Hs::Constructor > () = make_constructor(yystack_[3].value.as < std::vector<Hs::TypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < expression_ref > ());}
#line 2814 "parser.cc"
    break;

  case 157: // constr: forall constr_stuff
#line 930 "parser.y"
                                                {yylhs.value.as < Hs::Constructor > () = make_constructor(yystack_[1].value.as < std::vector<Hs::TypeVar> > (),{}, yystack_[0].value.as < expression_ref > ());}
#line 2820 "parser.cc"
    break;

  case 158: // forall: "forall" tv_bndrs "."
#line 932 "parser.y"
                                {yylhs.value.as < std::vector<Hs::TypeVar> > () = yystack_[1].value.as < std::vector<Hs::TypeVar> > ();}
#line 2826 "parser.cc"
    break;

  case 159: // forall: %empty
#line 933 "parser.y"
                                {}
#line 2832 "parser.cc"
    break;

  case 160: // constr_stuff: btype_no_ops
#line 935 "parser.y"
                                                {yylhs.value.as < expression_ref > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2838 "parser.cc"
    break;

  case 161: // constr_stuff: btype_no_ops conop btype_no_ops
#line 936 "parser.y"
                                                {yylhs.value.as < expression_ref > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,yystack_[1].value.as < std::string > ()}),Hs::make_tyapps(yystack_[2].value.as < std::vector<expression_ref> > ()),Hs::make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 2844 "parser.cc"
    break;

  case 162: // fielddecls: %empty
#line 938 "parser.y"
                                {}
#line 2850 "parser.cc"
    break;

  case 163: // fielddecls: fielddecls1
#line 939 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 2856 "parser.cc"
    break;

  case 164: // fielddecls1: fielddecls1 "," fielddecl
#line 941 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 2862 "parser.cc"
    break;

  case 165: // fielddecls1: fielddecl
#line 942 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 2868 "parser.cc"
    break;

  case 166: // fielddecl: sig_vars "::" ctype
#line 944 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::Var> > (),yystack_[0].value.as < expression_ref > ());}
#line 2874 "parser.cc"
    break;

  case 177: // decl_no_th: sigdecl
#line 963 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2880 "parser.cc"
    break;

  case 178: // decl_no_th: "!" aexp rhs
#line 965 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::StrictValueDecl{{yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < Hs::MultiGuardedRHS > ()}; }
#line 2886 "parser.cc"
    break;

  case 179: // decl_no_th: infixexp_top rhs
#line 966 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::ValueDecl({yystack_[1].location,make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ());}
#line 2892 "parser.cc"
    break;

  case 180: // decl: decl_no_th
#line 968 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2898 "parser.cc"
    break;

  case 181: // rhs: "=" exp wherebinds
#line 972 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 2904 "parser.cc"
    break;

  case 182: // rhs: gdrhs wherebinds
#line 973 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 2910 "parser.cc"
    break;

  case 183: // gdrhs: gdrhs gdrh
#line 975 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 2916 "parser.cc"
    break;

  case 184: // gdrhs: gdrh
#line 976 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 2922 "parser.cc"
    break;

  case 185: // gdrh: "|" guardquals "=" exp
#line 980 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ()};}
#line 2928 "parser.cc"
    break;

  case 186: // sigdecl: sig_vars "::" sigtypedoc
#line 990 "parser.y"
                                  { yylhs.value.as < expression_ref > () = Hs::SignatureDecl{yystack_[2].value.as < std::vector<Hs::Var> > (),yystack_[0].value.as < expression_ref > ()}; }
#line 2934 "parser.cc"
    break;

  case 187: // sigdecl: infix prec ops
#line 991 "parser.y"
                         { yylhs.value.as < expression_ref > () = Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()}; }
#line 2940 "parser.cc"
    break;

  case 188: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 993 "parser.y"
                                                    {}
#line 2946 "parser.cc"
    break;

  case 189: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 994 "parser.y"
                                            {}
#line 2952 "parser.cc"
    break;

  case 190: // sigdecl: "{-# SCC" qvar "#-}"
#line 995 "parser.y"
                              {}
#line 2958 "parser.cc"
    break;

  case 191: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 996 "parser.y"
                                     {}
#line 2964 "parser.cc"
    break;

  case 192: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 997 "parser.y"
                                                               {}
#line 2970 "parser.cc"
    break;

  case 193: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 998 "parser.y"
                                                                      {}
#line 2976 "parser.cc"
    break;

  case 194: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 999 "parser.y"
                                                     {}
#line 2982 "parser.cc"
    break;

  case 199: // exp: infixexp "::" sigtype
#line 1010 "parser.y"
                           { yylhs.value.as < expression_ref > () = Hs::TypedExp(make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()); }
#line 2988 "parser.cc"
    break;

  case 200: // exp: infixexp
#line 1011 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 2994 "parser.cc"
    break;

  case 201: // infixexp: exp10
#line 1013 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3000 "parser.cc"
    break;

  case 202: // infixexp: infixexp qop exp10
#line 1014 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3006 "parser.cc"
    break;

  case 203: // infixexp_top: exp10_top
#line 1016 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3012 "parser.cc"
    break;

  case 204: // infixexp_top: infixexp_top qop exp10_top
#line 1017 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3018 "parser.cc"
    break;

  case 205: // exp10_top: "-" fexp
#line 1019 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_minus(make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 3024 "parser.cc"
    break;

  case 206: // exp10_top: "{-# CORE" "STRING" "#-}"
#line 1020 "parser.y"
                                   {}
#line 3030 "parser.cc"
    break;

  case 207: // exp10_top: fexp
#line 1021 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 3036 "parser.cc"
    break;

  case 208: // exp10: exp10_top
#line 1023 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3042 "parser.cc"
    break;

  case 209: // exp10: scc_annot exp
#line 1024 "parser.y"
                                 {}
#line 3048 "parser.cc"
    break;

  case 214: // fexp: fexp aexp
#line 1035 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3054 "parser.cc"
    break;

  case 215: // fexp: fexp "TYPEAPP" atype
#line 1036 "parser.y"
                                 {}
#line 3060 "parser.cc"
    break;

  case 216: // fexp: "static" aexp
#line 1037 "parser.y"
                                 {}
#line 3066 "parser.cc"
    break;

  case 217: // fexp: aexp
#line 1038 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3072 "parser.cc"
    break;

  case 218: // aexp: qvar "@" aexp
#line 1040 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::AsPattern(Hs::Var({yystack_[2].location,yystack_[2].value.as < std::string > ()}),yystack_[0].value.as < expression_ref > ());}
#line 3078 "parser.cc"
    break;

  case 219: // aexp: "~" aexp
#line 1041 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::LazyPattern(yystack_[0].value.as < expression_ref > ());}
#line 3084 "parser.cc"
    break;

  case 220: // aexp: "\\" apats1 "->" exp
#line 1042 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::LambdaExp(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3090 "parser.cc"
    break;

  case 221: // aexp: "let" binds "in" exp
#line 1043 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 3096 "parser.cc"
    break;

  case 222: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1045 "parser.y"
                                                       {yylhs.value.as < expression_ref > () = Hs::IfExp({yystack_[6].location,yystack_[6].value.as < expression_ref > ()},{yystack_[3].location,yystack_[3].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 3102 "parser.cc"
    break;

  case 223: // aexp: "case" exp "of" altslist
#line 1047 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::CaseExp(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < Hs::Alts > ());}
#line 3108 "parser.cc"
    break;

  case 224: // aexp: "do" stmtlist
#line 1048 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::Do(yystack_[0].value.as < Hs::Stmts > ());}
#line 3114 "parser.cc"
    break;

  case 225: // aexp: "mdo" stmtlist
#line 1049 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::MDo(yystack_[0].value.as < Hs::Stmts > ());}
#line 3120 "parser.cc"
    break;

  case 226: // aexp: aexp1
#line 1051 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3126 "parser.cc"
    break;

  case 227: // aexp1: aexp1 "{" fbinds "}"
#line 1053 "parser.y"
                              {}
#line 3132 "parser.cc"
    break;

  case 228: // aexp1: aexp2
#line 1054 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3138 "parser.cc"
    break;

  case 229: // aexp2: qvar
#line 1056 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3144 "parser.cc"
    break;

  case 230: // aexp2: qcon
#line 1057 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3150 "parser.cc"
    break;

  case 231: // aexp2: literal
#line 1058 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3156 "parser.cc"
    break;

  case 232: // aexp2: "(" texp ")"
#line 1059 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3162 "parser.cc"
    break;

  case 233: // aexp2: "(" tup_exprs ")"
#line 1060 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::Tuple(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3168 "parser.cc"
    break;

  case 234: // aexp2: "[" list "]"
#line 1065 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3174 "parser.cc"
    break;

  case 235: // aexp2: "_"
#line 1066 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::WildcardPattern();}
#line 3180 "parser.cc"
    break;

  case 236: // texp: exp
#line 1071 "parser.y"
                      {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3186 "parser.cc"
    break;

  case 237: // texp: infixexp qop
#line 1072 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::LeftSection ( make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ()), yystack_[0].value.as < expression_ref > () ); }
#line 3192 "parser.cc"
    break;

  case 238: // texp: qopm infixexp
#line 1073 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::RightSection( yystack_[1].value.as < expression_ref > (), make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ()) ); }
#line 3198 "parser.cc"
    break;

  case 239: // tup_exprs: tup_exprs "," texp
#line 1078 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3204 "parser.cc"
    break;

  case 240: // tup_exprs: texp "," texp
#line 1079 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3210 "parser.cc"
    break;

  case 241: // list: texp
#line 1097 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List{{yystack_[0].value.as < expression_ref > ()}}; }
#line 3216 "parser.cc"
    break;

  case 242: // list: lexps
#line 1098 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List{yystack_[0].value.as < std::vector<expression_ref> > ()}; }
#line 3222 "parser.cc"
    break;

  case 243: // list: texp ".."
#line 1099 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < expression_ref > ()); }
#line 3228 "parser.cc"
    break;

  case 244: // list: texp "," exp ".."
#line 1100 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()); }
#line 3234 "parser.cc"
    break;

  case 245: // list: texp ".." exp
#line 1101 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()); }
#line 3240 "parser.cc"
    break;

  case 246: // list: texp "," exp ".." exp
#line 1102 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < expression_ref > (), yystack_[2].value.as < expression_ref > (), yystack_[0].value.as < expression_ref > ()); }
#line 3246 "parser.cc"
    break;

  case 247: // list: texp "|" squals
#line 1103 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < expression_ref > (), yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 3252 "parser.cc"
    break;

  case 248: // lexps: lexps "," texp
#line 1105 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3258 "parser.cc"
    break;

  case 249: // lexps: texp "," texp
#line 1106 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3264 "parser.cc"
    break;

  case 250: // squals: squals "," qual
#line 1119 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3270 "parser.cc"
    break;

  case 251: // squals: qual
#line 1121 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3276 "parser.cc"
    break;

  case 252: // guardquals: guardquals1
#line 1131 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3282 "parser.cc"
    break;

  case 253: // guardquals1: guardquals1 "," qual
#line 1133 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > ();yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3288 "parser.cc"
    break;

  case 254: // guardquals1: qual
#line 1134 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3294 "parser.cc"
    break;

  case 255: // altslist: "{" alts "}"
#line 1137 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 3300 "parser.cc"
    break;

  case 256: // altslist: "vocurly" alts close
#line 1138 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 3306 "parser.cc"
    break;

  case 257: // altslist: "{" "}"
#line 1139 "parser.y"
                                 {}
#line 3312 "parser.cc"
    break;

  case 258: // altslist: "vocurly" close
#line 1140 "parser.y"
                                 {}
#line 3318 "parser.cc"
    break;

  case 259: // alts: alts1
#line 1142 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 3324 "parser.cc"
    break;

  case 260: // alts: ";" alts
#line 1143 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 3330 "parser.cc"
    break;

  case 261: // alts1: alts1 ";" alt
#line 1145 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 3336 "parser.cc"
    break;

  case 262: // alts1: alts1 ";"
#line 1146 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 3342 "parser.cc"
    break;

  case 263: // alts1: alt
#line 1147 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 3348 "parser.cc"
    break;

  case 264: // alt: pat alt_rhs
#line 1149 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 3354 "parser.cc"
    break;

  case 265: // alt_rhs: "->" exp wherebinds
#line 1151 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3360 "parser.cc"
    break;

  case 266: // alt_rhs: gdpats wherebinds
#line 1152 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3366 "parser.cc"
    break;

  case 267: // gdpats: gdpats gdpat
#line 1154 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3372 "parser.cc"
    break;

  case 268: // gdpats: gdpat
#line 1155 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3378 "parser.cc"
    break;

  case 269: // gdpat: "|" guardquals "->" exp
#line 1164 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ()};}
#line 3384 "parser.cc"
    break;

  case 270: // pat: exp
#line 1166 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3390 "parser.cc"
    break;

  case 271: // pat: "!" aexp
#line 1167 "parser.y"
              {yylhs.value.as < expression_ref > () = Hs::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3396 "parser.cc"
    break;

  case 272: // bindpat: exp
#line 1169 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3402 "parser.cc"
    break;

  case 273: // bindpat: "!" aexp
#line 1170 "parser.y"
              {yylhs.value.as < expression_ref > () = Hs::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3408 "parser.cc"
    break;

  case 274: // apat: aexp
#line 1172 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3414 "parser.cc"
    break;

  case 275: // apat: "!" aexp
#line 1173 "parser.y"
              {yylhs.value.as < expression_ref > () = Hs::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3420 "parser.cc"
    break;

  case 276: // apats1: apats1 apat
#line 1175 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3426 "parser.cc"
    break;

  case 277: // apats1: apat
#line 1176 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3432 "parser.cc"
    break;

  case 278: // stmtlist: "{" stmts "}"
#line 1179 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 3438 "parser.cc"
    break;

  case 279: // stmtlist: "vocurly" stmts close
#line 1180 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 3444 "parser.cc"
    break;

  case 280: // stmts: stmts ";" stmt
#line 1182 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3450 "parser.cc"
    break;

  case 281: // stmts: stmts ";"
#line 1183 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3456 "parser.cc"
    break;

  case 282: // stmts: stmt
#line 1184 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3462 "parser.cc"
    break;

  case 283: // stmts: %empty
#line 1185 "parser.y"
                       {}
#line 3468 "parser.cc"
    break;

  case 284: // stmt: qual
#line 1190 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3474 "parser.cc"
    break;

  case 285: // stmt: "rec" stmtlist
#line 1191 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ());}
#line 3480 "parser.cc"
    break;

  case 286: // qual: bindpat "<-" exp
#line 1193 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::PatQual(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3486 "parser.cc"
    break;

  case 287: // qual: exp
#line 1194 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::SimpleQual(yystack_[0].value.as < expression_ref > ());}
#line 3492 "parser.cc"
    break;

  case 288: // qual: "let" binds
#line 1195 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ());}
#line 3498 "parser.cc"
    break;

  case 296: // qcon: gen_qcon
#line 1240 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3504 "parser.cc"
    break;

  case 297: // qcon: sysdcon
#line 1241 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3510 "parser.cc"
    break;

  case 298: // gen_qcon: qconid
#line 1243 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3516 "parser.cc"
    break;

  case 299: // gen_qcon: "(" qconsym ")"
#line 1244 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3522 "parser.cc"
    break;

  case 300: // con: conid
#line 1246 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3528 "parser.cc"
    break;

  case 301: // con: "(" consym ")"
#line 1247 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3534 "parser.cc"
    break;

  case 302: // con: sysdcon
#line 1248 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3540 "parser.cc"
    break;

  case 305: // sysdcon_no_list: "(" ")"
#line 1253 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 3546 "parser.cc"
    break;

  case 306: // sysdcon_no_list: "(" commas ")"
#line 1254 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3552 "parser.cc"
    break;

  case 307: // sysdcon_no_list: "(#" "#)"
#line 1255 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 3558 "parser.cc"
    break;

  case 308: // sysdcon_no_list: "(#" commas "#)"
#line 1256 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3564 "parser.cc"
    break;

  case 309: // sysdcon: sysdcon_no_list
#line 1258 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3570 "parser.cc"
    break;

  case 310: // sysdcon: "[" "]"
#line 1259 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 3576 "parser.cc"
    break;

  case 311: // conop: consym
#line 1261 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3582 "parser.cc"
    break;

  case 312: // conop: "`" conid "`"
#line 1262 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3588 "parser.cc"
    break;

  case 313: // qconop: qconsym
#line 1264 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3594 "parser.cc"
    break;

  case 314: // qconop: "`" qconid "`"
#line 1265 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3600 "parser.cc"
    break;

  case 315: // gtycon: ntgtycon
#line 1268 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3606 "parser.cc"
    break;

  case 316: // gtycon: "(" ")"
#line 1269 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 3612 "parser.cc"
    break;

  case 317: // gtycon: "(#" "#)"
#line 1270 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 3618 "parser.cc"
    break;

  case 318: // ntgtycon: oqtycon
#line 1272 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3624 "parser.cc"
    break;

  case 319: // ntgtycon: "(" commas ")"
#line 1273 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3630 "parser.cc"
    break;

  case 320: // ntgtycon: "(#" commas "#)"
#line 1274 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3636 "parser.cc"
    break;

  case 321: // ntgtycon: "(" "->" ")"
#line 1275 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 3642 "parser.cc"
    break;

  case 322: // ntgtycon: "[" "]"
#line 1276 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 3648 "parser.cc"
    break;

  case 323: // oqtycon: qtycon
#line 1278 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3654 "parser.cc"
    break;

  case 324: // oqtycon: "(" qtyconsym ")"
#line 1279 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3660 "parser.cc"
    break;

  case 325: // oqtycon: "(" "~" ")"
#line 1280 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3666 "parser.cc"
    break;

  case 326: // oqtycon_no_varcon: qtycon
#line 1282 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3672 "parser.cc"
    break;

  case 327: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1283 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3678 "parser.cc"
    break;

  case 328: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1284 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3684 "parser.cc"
    break;

  case 329: // oqtycon_no_varcon: "(" ":" ")"
#line 1285 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 3690 "parser.cc"
    break;

  case 330: // oqtycon_no_varcon: "(" "~" ")"
#line 1286 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3696 "parser.cc"
    break;

  case 331: // qtyconop: qtyconsym
#line 1289 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3702 "parser.cc"
    break;

  case 332: // qtyconop: "`" qtycon "`"
#line 1290 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3708 "parser.cc"
    break;

  case 333: // qtycondoc: qtycon
#line 1292 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3714 "parser.cc"
    break;

  case 334: // qtycon: "QCONID"
#line 1294 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3720 "parser.cc"
    break;

  case 335: // qtycon: tycon
#line 1295 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3726 "parser.cc"
    break;

  case 336: // tycon: "CONID"
#line 1299 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3732 "parser.cc"
    break;

  case 337: // qtyconsym: "QCONSYM"
#line 1301 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3738 "parser.cc"
    break;

  case 338: // qtyconsym: "QVARSYM"
#line 1302 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3744 "parser.cc"
    break;

  case 339: // qtyconsym: tyconsym
#line 1303 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3750 "parser.cc"
    break;

  case 340: // tyconsym: "CONSYM"
#line 1305 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3756 "parser.cc"
    break;

  case 341: // tyconsym: "VARSYM"
#line 1306 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3762 "parser.cc"
    break;

  case 342: // tyconsym: ":"
#line 1307 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 3768 "parser.cc"
    break;

  case 343: // tyconsym: "-"
#line 1308 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 3774 "parser.cc"
    break;

  case 344: // op: varop
#line 1313 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3780 "parser.cc"
    break;

  case 345: // op: conop
#line 1314 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3786 "parser.cc"
    break;

  case 346: // varop: varsym
#line 1316 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3792 "parser.cc"
    break;

  case 347: // varop: "`" varid "`"
#line 1317 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3798 "parser.cc"
    break;

  case 348: // qop: qvarop
#line 1319 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3804 "parser.cc"
    break;

  case 349: // qop: qconop
#line 1320 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3810 "parser.cc"
    break;

  case 350: // qopm: qvaropm
#line 1323 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3816 "parser.cc"
    break;

  case 351: // qopm: qconop
#line 1324 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3822 "parser.cc"
    break;

  case 352: // qvarop: qvarsym
#line 1329 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3828 "parser.cc"
    break;

  case 353: // qvarop: "`" qvarid "`"
#line 1330 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3834 "parser.cc"
    break;

  case 354: // qvaropm: qvarsym_no_minus
#line 1332 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 3840 "parser.cc"
    break;

  case 355: // qvaropm: "`" qvarid "`"
#line 1333 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3846 "parser.cc"
    break;

  case 356: // tyvar: tyvarid
#line 1337 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3852 "parser.cc"
    break;

  case 357: // tyvarop: "`" tyvarid "`"
#line 1339 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3858 "parser.cc"
    break;

  case 358: // tyvarid: "VARID"
#line 1341 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3864 "parser.cc"
    break;

  case 359: // tyvarid: special_id
#line 1342 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3870 "parser.cc"
    break;

  case 360: // tyvarid: "unsafe"
#line 1343 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 3876 "parser.cc"
    break;

  case 361: // tyvarid: "safe"
#line 1344 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 3882 "parser.cc"
    break;

  case 362: // tyvarid: "interruptible"
#line 1345 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 3888 "parser.cc"
    break;

  case 363: // var: varid
#line 1348 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3894 "parser.cc"
    break;

  case 364: // var: "(" varsym ")"
#line 1349 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3900 "parser.cc"
    break;

  case 365: // qvar: qvarid
#line 1351 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3906 "parser.cc"
    break;

  case 366: // qvar: "(" varsym ")"
#line 1352 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3912 "parser.cc"
    break;

  case 367: // qvar: "(" qvarsym1 ")"
#line 1353 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3918 "parser.cc"
    break;

  case 368: // qvarid: varid
#line 1355 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3924 "parser.cc"
    break;

  case 369: // qvarid: "QVARID"
#line 1356 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3930 "parser.cc"
    break;

  case 370: // varid: "VARID"
#line 1358 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3936 "parser.cc"
    break;

  case 371: // varid: special_id
#line 1359 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3942 "parser.cc"
    break;

  case 372: // varid: "unsafe"
#line 1360 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 3948 "parser.cc"
    break;

  case 373: // varid: "safe"
#line 1361 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 3954 "parser.cc"
    break;

  case 374: // varid: "interruptible"
#line 1362 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 3960 "parser.cc"
    break;

  case 375: // varid: "forall"
#line 1363 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 3966 "parser.cc"
    break;

  case 376: // varid: "family"
#line 1364 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 3972 "parser.cc"
    break;

  case 377: // varid: "role"
#line 1365 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 3978 "parser.cc"
    break;

  case 378: // qvarsym: varsym
#line 1367 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3984 "parser.cc"
    break;

  case 379: // qvarsym: qvarsym1
#line 1368 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3990 "parser.cc"
    break;

  case 380: // qvarsym_no_minus: varsym_no_minus
#line 1370 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3996 "parser.cc"
    break;

  case 381: // qvarsym_no_minus: qvarsym1
#line 1371 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4002 "parser.cc"
    break;

  case 382: // qvarsym1: "QVARSYM"
#line 1373 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4008 "parser.cc"
    break;

  case 383: // varsym: varsym_no_minus
#line 1375 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4014 "parser.cc"
    break;

  case 384: // varsym: "-"
#line 1376 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 4020 "parser.cc"
    break;

  case 385: // varsym_no_minus: "VARSYM"
#line 1378 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4026 "parser.cc"
    break;

  case 386: // varsym_no_minus: special_sym
#line 1379 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4032 "parser.cc"
    break;

  case 387: // special_id: "as"
#line 1381 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 4038 "parser.cc"
    break;

  case 388: // special_id: "qualified"
#line 1382 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 4044 "parser.cc"
    break;

  case 389: // special_id: "hiding"
#line 1383 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 4050 "parser.cc"
    break;

  case 390: // special_id: "export"
#line 1384 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 4056 "parser.cc"
    break;

  case 391: // special_id: "label"
#line 1385 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 4062 "parser.cc"
    break;

  case 392: // special_id: "dynamic"
#line 1386 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 4068 "parser.cc"
    break;

  case 393: // special_id: "stdcall"
#line 1387 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 4074 "parser.cc"
    break;

  case 394: // special_id: "ccall"
#line 1388 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 4080 "parser.cc"
    break;

  case 395: // special_id: "capi"
#line 1389 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 4086 "parser.cc"
    break;

  case 396: // special_id: "prim"
#line 1390 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 4092 "parser.cc"
    break;

  case 397: // special_id: "javascript"
#line 1391 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 4098 "parser.cc"
    break;

  case 398: // special_id: "group"
#line 1392 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 4104 "parser.cc"
    break;

  case 399: // special_id: "stock"
#line 1393 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 4110 "parser.cc"
    break;

  case 400: // special_id: "anyclass"
#line 1394 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 4116 "parser.cc"
    break;

  case 401: // special_id: "via"
#line 1395 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 4122 "parser.cc"
    break;

  case 402: // special_id: "unit"
#line 1396 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 4128 "parser.cc"
    break;

  case 403: // special_id: "dependency"
#line 1397 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 4134 "parser.cc"
    break;

  case 404: // special_id: "signature"
#line 1398 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 4140 "parser.cc"
    break;

  case 405: // special_sym: "!"
#line 1400 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 4146 "parser.cc"
    break;

  case 406: // special_sym: "."
#line 1401 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 4152 "parser.cc"
    break;

  case 407: // special_sym: "*"
#line 1402 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 4158 "parser.cc"
    break;

  case 408: // qconid: conid
#line 1406 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4164 "parser.cc"
    break;

  case 409: // qconid: "QCONID"
#line 1407 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4170 "parser.cc"
    break;

  case 410: // conid: "CONID"
#line 1409 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4176 "parser.cc"
    break;

  case 411: // qconsym: consym
#line 1411 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4182 "parser.cc"
    break;

  case 412: // qconsym: "QCONSYM"
#line 1412 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4188 "parser.cc"
    break;

  case 413: // consym: "CONSYM"
#line 1414 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4194 "parser.cc"
    break;

  case 414: // consym: ":"
#line 1415 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4200 "parser.cc"
    break;

  case 415: // literal: "CHAR"
#line 1419 "parser.y"
                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < char > ();}
#line 4206 "parser.cc"
    break;

  case 416: // literal: "STRING"
#line 1420 "parser.y"
                     {yylhs.value.as < expression_ref > () = yy_make_string(yystack_[0].value.as < std::string > ());}
#line 4212 "parser.cc"
    break;

  case 417: // literal: "INTEGER"
#line 1421 "parser.y"
                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < int > ();}
#line 4218 "parser.cc"
    break;

  case 418: // literal: "RATIONAL"
#line 1422 "parser.y"
                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < double > ();}
#line 4224 "parser.cc"
    break;

  case 419: // literal: "PRIMINTEGER"
#line 1423 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < int > ()});}
#line 4230 "parser.cc"
    break;

  case 421: // close: error
#line 1431 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 4236 "parser.cc"
    break;

  case 422: // modid: "CONID"
#line 1435 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4242 "parser.cc"
    break;

  case 423: // modid: "QCONID"
#line 1436 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4248 "parser.cc"
    break;

  case 424: // commas: commas ","
#line 1438 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 4254 "parser.cc"
    break;

  case 425: // commas: ","
#line 1439 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 4260 "parser.cc"
    break;


#line 4264 "parser.cc"

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


  const short parser::yypact_ninf_ = -558;

  const short parser::yytable_ninf_ = -384;

  const short
  parser::yypact_[] =
  {
      28,    99,  -558,    76,  -558,  -558,  -558,  -558,  -558,   278,
      -1,    11,  -558,    46,   -36,   -36,    22,  -558,  -558,  -558,
    -558,    68,  -558,  -558,  -558,    21,  -558,    92,   113,  3706,
     130,   152,    84,  -558,   591,  -558,   -26,  -558,  -558,  -558,
    -558,    99,  -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,
    -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,
    -558,  -558,  -558,  -558,   157,  -558,  -558,  -558,  -558,   123,
     128,  -558,   146,  -558,  -558,  -558,  -558,  -558,  -558,  -558,
     169,  -558,    99,  -558,   150,  -558,  2032,  3329,  -558,   171,
     195,  2032,  -558,  -558,  -558,   322,   212,  -558,  3329,  4120,
     195,  2818,   186,   167,  4029,    61,  2425,  2818,  2556,  2818,
    1115,   984,   -16,  -558,  -558,  -558,  -558,  -558,  -558,  -558,
      39,   186,   177,    84,  -558,  -558,  -558,   229,   -17,  -558,
    -558,  2145,  -558,  2687,  -558,   213,  -558,  -558,  -558,  -558,
    -558,  -558,   228,    73,  -558,  -558,  -558,  -558,   191,  -558,
     225,   231,  -558,  -558,  -558,  -558,  -558,   233,  -558,   241,
     243,   252,  -558,  -558,  -558,  3706,  3739,  -558,  -558,  -558,
    -558,   362,  -558,    78,   984,   344,   437,  -558,  -558,  2032,
    -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,  4120,  3026,
    2925,   254,  3927,  -558,  -558,  -558,  -558,  -558,   345,  3614,
    -558,   282,  -558,   178,  3329,  -558,  -558,  -558,  -558,  -558,
    -558,  -558,  -558,  -558,  -558,  -558,  -558,  3127,  1508,  1508,
    -558,   265,   302,   303,   306,   308,  3127,   722,   722,  -558,
     371,   307,   304,   285,   263,  -558,  -558,  -558,   -24,  4029,
    -558,   321,   168,    -7,   295,   110,   287,   325,  -558,  -558,
    2818,  -558,  -558,  2294,  -558,  2687,   202,  -558,  -558,  3868,
    -558,  -558,  -558,   437,    -5,   298,   294,  -558,  2032,  -558,
    -558,  -558,  -558,  -558,  -558,  2556,  -558,  -558,    59,    90,
     243,   301,   309,   310,   127,  -558,   112,  3127,  4029,  4029,
    -558,   137,   150,   286,  3329,  3127,  4120,  2032,  1770,  3868,
    -558,    23,  -558,  -558,  2163,  -558,  -558,  -558,  -558,  3614,
    -558,  3972,  2818,  -558,  -558,  -558,  -558,  -558,  -558,  -558,
    -558,   311,   312,   313,  -558,   315,    46,    99,    47,   339,
     346,   217,  3127,  2032,  -558,    79,   324,   316,  -558,  -558,
    -558,   317,   333,  -558,   327,   328,  -558,   329,   318,   332,
     136,   129,   314,   326,   212,  -558,  -558,  3329,  3127,  -558,
    -558,   334,   335,   212,   195,  2818,   340,   361,   -32,  -558,
    -558,    42,  -558,   422,  -558,  -558,  -558,  -558,  -558,  -558,
     345,    72,  -558,  -558,  2145,    45,  2032,  3127,   342,   320,
     323,   349,   377,  -558,  -558,   378,   348,    61,    43,   380,
    -558,  2032,  -558,  -558,   347,   350,  2032,  2032,  1770,  1246,
    -558,  1246,   572,  -558,  1246,  -558,  1246,    80,  -558,  -558,
    -558,  -558,   383,   381,   384,  4074,   352,  -558,  -558,  -558,
    -558,  -558,    -6,   266,  -558,  -558,  -558,  -558,   345,   382,
     355,  -558,   356,  -558,  -558,  -558,  -558,  -558,   372,  -558,
     360,   399,  -558,  -558,  -558,  3835,  -558,  -558,  -558,   374,
    3706,  -558,  -558,  -558,  -558,  1377,   853,  -558,  -558,  -558,
    3127,  -558,  4120,  4153,  -558,  3127,  -558,  -558,  -558,  3127,
    -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,  3127,
     371,  -558,  -558,  2032,  -558,  1508,  -558,  2032,  -558,  -558,
     722,  -558,  -558,  -558,  -558,  -558,   385,  -558,  -558,  -558,
    -558,  -558,   386,   250,   141,  -558,  -558,  -558,  -558,  -558,
    -558,  -558,   373,  -558,   407,  -558,  -558,  -558,  -558,  -558,
    3127,  3127,   376,   387,   137,  -558,   416,  3127,   466,  -558,
     487,  -558,  2032,  1770,  -558,  -558,  3972,  1246,  -558,  3706,
     389,  2818,  -558,  1639,  -558,   398,   388,  -558,   238,    46,
    -558,  -558,  -558,  3127,  4244,  -558,  -558,  -558,   391,  -558,
    -558,  -558,   265,  -558,  -558,   328,  -558,   332,  -558,  1770,
    2032,  -558,    -3,    14,  -558,  -558,  -558,  -558,  -558,  -558,
    -558,   419,  -558,  3614,    44,  -558,   487,  -558,  -558,  -558,
    -558,  -558,   392,  -558,  -558,  -558,  -558,  1901,  1770,  2032,
    -558,    33,  -558,  -558,  -558,   424,  -558,   494,  -558,  -558,
    -558,  3127,  -558,  4211,   466,   417,  3430,  -558,  -558,  -558,
    -558,  -558,  -558,  3228,    70,   455,  -558,  -558,  -558,  -558,
    -558,   423,   345,  -558,  -558,  3127,  2032,  -558,  -558,  -558,
    3614,   393,  -558,  3614,  -558,  -558,   396,   404,  -558,  3329,
    -558,  2032,  -558,   405,  -558,  3522,  -558,  3614,  3329,  -558,
    -558,  -558,  -558,  -558
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
     195,   195,    49,    56,    59,    60,    61,    88,     0,    63,
     177,    64,   203,   207,   217,   226,   228,   230,   296,   309,
     297,   108,   229,   368,   298,   408,   231,    99,     0,    23,
       0,     0,   384,   405,   407,   406,   385,     0,   382,     0,
       0,     0,   383,   386,    17,     0,    27,    22,    36,    36,
       3,    44,    33,     0,     0,     0,   200,   208,   201,     0,
     361,   362,   360,   342,   113,   343,   112,   134,   162,     0,
       0,     0,     0,   358,   341,   340,   338,   337,    97,     0,
     111,     0,    85,   120,   123,   126,   128,   132,   318,   129,
     323,   331,   339,   133,   130,   356,   359,   145,   283,   283,
     224,   211,     0,     0,     0,     0,     0,    92,    92,    95,
       0,     0,   120,     0,     0,   363,   225,   216,     0,     0,
     196,     0,     0,     0,     0,     0,   303,   103,   302,   300,
       0,   274,   277,     0,   219,   205,     0,   414,   310,     0,
     413,   412,   236,   200,   241,     0,   242,   351,     0,   350,
     354,   381,   380,   313,   411,   384,   305,   425,     0,     0,
     381,     0,   380,   313,     0,   307,     0,     0,     0,     0,
      50,     0,    57,     0,     0,     0,     0,     0,     0,     0,
     179,    97,   184,   349,     0,   348,   352,   379,   378,     0,
     214,   290,     0,   100,   329,   330,   328,   327,   367,   366,
      20,     0,     0,    28,    30,     0,     0,     0,    46,     0,
       0,     0,     0,     0,   209,     0,     0,   163,   165,   149,
     322,     0,     0,   116,     0,   113,   137,   146,     0,   331,
       0,     0,     0,     0,     0,    66,   135,     0,     0,   127,
     146,     0,   144,     0,     0,     0,   287,     0,     0,   282,
     284,     0,   210,     0,    72,    71,    73,    74,   141,   105,
      97,     0,   180,    91,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   206,   190,     0,     0,     0,     0,     0,
     275,     0,   276,   178,     0,     0,   237,   243,     0,     0,
     234,     0,   238,   232,     0,   233,     0,   366,   299,   306,
     424,   308,     0,     0,     0,     0,   187,   345,    55,   344,
     346,   311,     0,    82,   186,   117,   106,   107,    97,     0,
     252,   254,     0,   182,   183,   204,   215,   293,     0,   289,
     292,   295,   218,    26,    25,     0,     9,    10,    43,     0,
       0,    40,    45,   213,   212,     0,     0,   223,   199,   202,
       0,   136,     0,     0,   139,     0,   321,   325,   140,     0,
     324,   319,   320,   332,   357,    96,    84,   121,    62,     0,
     288,   285,   273,     0,   278,   281,   279,     0,    70,    93,
      90,    94,   221,    67,   364,    65,     0,   197,   189,   191,
     301,   304,     0,     0,     0,   104,   315,   188,   220,   355,
     314,   245,   247,   251,   236,   249,   248,   240,   239,   194,
       0,     0,     0,     0,     0,    87,     0,     0,   159,    69,
     167,   181,     0,     0,   353,   227,     0,     0,    29,     0,
       0,     0,   257,     0,   270,     0,   259,   263,     0,     0,
     258,   166,   164,     0,     0,   148,   150,   115,   147,   147,
     286,   280,   211,    89,   198,     0,   316,     0,   317,     0,
     244,   109,     0,     0,   347,   312,    54,    86,   152,    83,
     149,   153,   155,     0,     0,    68,   168,   170,   185,   253,
     291,   294,     0,    47,   271,   260,   255,   262,     0,     0,
     264,    97,   268,   256,   114,     0,   138,     0,   250,   246,
     192,     0,   193,     0,   159,     0,   160,   124,   131,   157,
      78,    76,    77,     0,     0,   171,   174,   333,   169,    48,
     261,     0,    97,   266,   267,     0,     0,   110,   158,   154,
       0,     0,   125,     0,   175,   122,   142,     0,   172,     0,
     173,     0,   265,     0,   222,   160,   156,   161,     0,   176,
      79,   269,   151,   143
  };

  const short
  parser::yypgoto_[] =
  {
    -558,  -558,  -558,  -558,  -558,  -558,  -558,    31,  -558,  -558,
    -416,  -558,   353,  -558,  -558,  -558,  -151,   400,  -558,  -558,
    -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,
    -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,
     220,  -558,   296,  -558,  -158,  -297,   500,  -558,  -558,  -311,
    -558,  -163,     0,  -558,  -558,  -172,   138,   -57,  -558,   -84,
    -558,   -91,  -365,  -558,   337,  -557,  -183,   239,  -136,  -558,
     319,   -60,  -558,  -112,  -558,  -558,   -85,  -558,  -107,  -558,
    -558,    66,  -558,  -558,   -52,   -89,   512,    53,   299,  -558,
     253,  -558,   242,  -558,   -59,   -88,   524,   -25,  -276,   -10,
    -558,   -68,   -72,  -558,  -558,   -79,  -558,  -558,  -558,  -558,
     -45,  -558,  -558,  -418,  -558,   -41,  -558,  -558,   -42,  -558,
    -558,   330,  -558,   -74,   351,    77,  -274,  -558,    25,  -558,
    -558,  -558,  -558,   179,  -558,   -95,  -551,   -92,  -558,   176,
    -558,  -558,  -558,  -558,   -29,  -558,  -182,  -558,    51,  -558,
    -138,  -558,  -558,  -558,  -432,  -558,   394,   -71,   -27,  -197,
     -14,  -558,  -558,   -31,   -58,   -55,   -86,  -558,  -187,  -100,
     -18,  -226,  -558,  -284,   -28,  -101
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   170,     6,    10,    19,    30,
      69,    70,    71,   167,   322,   323,    72,    84,    11,    20,
      21,    32,    82,   328,   461,   462,   291,   122,   426,    33,
      34,   123,   124,   125,   126,   226,   634,   660,   127,   539,
     198,   294,   381,   229,   230,   355,    27,    36,   399,   378,
     434,   128,   582,   199,   200,   379,   436,   342,   625,   343,
     656,   203,   626,   204,   205,   627,   206,   380,   657,   361,
     348,   473,   565,   589,   540,   591,   592,   593,   629,   336,
     337,   338,   595,   596,   597,   635,   382,   383,   300,   301,
     302,   130,   239,   240,   366,   176,   384,   177,   178,   373,
     179,   133,   134,   135,   136,   278,   279,   265,   266,   522,
     439,   440,   467,   555,   556,   557,   610,   611,   612,   558,
     367,   252,   253,   220,   368,   369,   370,   448,   449,   450,
     137,   138,   246,   247,   139,   140,   427,   267,   515,   207,
     208,    73,   209,   636,   210,    75,   211,   212,   428,   429,
     304,   268,   305,   269,   213,   214,   215,   141,   142,    77,
      78,   306,   270,   271,   308,   162,    79,   163,   144,   145,
     273,   274,   146,    24,     9,   284
  };

  const short
  parser::yytable_[] =
  {
      74,   216,    76,   202,   443,   249,   161,   232,   349,   132,
     248,   286,   216,   149,   231,   324,   356,   341,   347,   396,
     143,   468,   263,   263,   441,   335,   236,   175,   234,   237,
     201,   264,   221,   160,   251,   254,    13,   256,   333,   303,
     255,   566,   457,    22,   550,   360,    22,    22,   559,     1,
     354,   262,   262,   281,   171,   272,   282,   469,   287,   459,
     354,   310,   404,   295,   390,   431,   630,    25,   494,   652,
     394,   535,   405,   407,   620,   653,    12,   243,   147,   408,
     280,   495,    31,   498,   303,   235,   263,   496,   148,   350,
     351,   622,    26,   283,   285,   631,   632,   296,   277,    17,
     307,   501,   442,   216,   216,   391,   216,   298,   652,   409,
     652,   621,   405,   216,   653,   262,   161,   608,   216,   282,
     334,   395,   536,   435,    18,   406,   446,     2,   621,    29,
     469,   216,   615,   602,   523,   605,    74,    74,    76,    76,
     216,   541,   238,   280,    23,   307,   512,    23,    23,    35,
     513,   633,   514,  -363,   460,   495,   283,    80,   500,   470,
    -364,    66,    66,   352,   244,    68,    68,   413,   245,    37,
     112,   303,   499,   414,   235,   388,    81,   633,   400,   113,
     412,   251,   560,   310,   161,   500,   487,  -363,    66,   257,
      38,   566,    68,   296,  -364,   329,   485,    83,   415,   325,
     326,   216,   132,   132,   416,   490,   330,   255,   216,   216,
     202,   160,   392,   143,   143,   435,   257,     7,   276,   581,
     581,     8,   421,   216,   277,   437,   420,   152,   153,   154,
     260,   164,   307,   430,   155,   419,   150,   201,   438,   482,
     452,   420,   165,   420,   481,   151,   216,   152,   153,   154,
     420,   578,   425,   166,   155,   277,   156,   260,   152,   153,
     154,   423,   424,   172,   358,   155,   232,  -118,   168,   599,
     169,   216,   216,   486,   333,   613,   156,   157,   217,   445,
     158,   159,   235,   297,   451,   665,   298,   156,   667,   238,
     491,   158,   303,   492,   218,   241,   219,   249,   561,   458,
     293,   216,   248,   567,   548,   618,   290,   568,   431,   335,
     647,   227,   311,   228,   643,   312,   465,   569,   466,   313,
     303,   263,   608,   263,   609,   533,   263,   502,   263,   183,
     525,   577,   526,   314,   441,   527,   344,   528,   575,   315,
     185,   316,   518,    14,    15,   662,   537,   538,   521,   317,
     524,   318,   262,   307,   272,   262,   272,   262,   576,   272,
     319,   272,   288,   289,   277,   588,   327,   331,   277,   194,
     195,   357,   354,   196,   197,   152,   153,   154,   372,   374,
     375,   307,   155,   376,   216,   377,   386,   216,   387,   216,
     358,   614,   389,   216,   222,   223,   224,   225,   393,   258,
     431,   397,   410,   216,   156,   398,   554,   554,   411,   417,
     628,   532,   350,   351,   432,   456,   463,  -383,   418,   453,
     454,   474,   475,   464,   471,  -272,    74,   455,    76,   483,
     472,    74,   479,    76,   570,   476,   477,   478,   572,   431,
     480,   484,   488,   628,   216,   216,   493,   497,   505,   489,
     504,   216,   506,   507,   508,   509,   510,   517,   235,   263,
     529,   530,   519,   542,   531,   520,   534,   628,   601,   543,
     628,   544,   545,   588,   546,   132,   430,   216,   216,   604,
     547,   549,   628,   598,   628,   580,   143,   579,   262,   574,
     340,   584,   272,   587,   554,   590,   594,   603,   606,   616,
     639,   607,   585,   624,   645,   646,   650,   216,   659,   661,
     668,   113,   669,   672,   433,    28,   257,   332,   320,   451,
      74,   619,    76,   292,   385,   503,   422,   152,   153,   154,
     623,   583,   673,   663,   155,   216,   362,   216,   562,   649,
     216,   359,   232,   666,   638,   658,   129,   216,   554,   655,
     642,   533,   299,   573,   444,   403,   156,   260,   131,   216,
     158,   261,   617,   641,   216,   637,   640,   216,   232,   644,
     371,   600,   571,   216,   516,   670,   511,   232,     0,   216,
       0,   216,   216,   402,   655,   586,   353,   664,     0,     0,
       0,     0,     0,     0,    85,    39,    86,    87,    88,    89,
       0,    90,   671,    40,    91,   637,     0,    92,    93,    94,
      95,    96,     0,    97,     0,    42,     0,    98,     0,    99,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,    54,    55,    56,     0,     0,    57,     0,
       0,   101,    58,    59,    60,    61,    62,    63,   102,     0,
       0,   257,     0,   103,   104,     0,     0,     0,     0,     0,
       0,     0,   152,   153,   154,     0,     0,   105,     0,   155,
       0,     0,     0,   106,     0,     0,     0,     0,     0,   107,
       0,   108,   109,     0,     0,     0,     0,   299,     0,     0,
       0,   156,   260,     0,   110,   158,   261,     0,   111,     0,
     112,     0,     0,     0,     0,     0,     0,     0,    65,   113,
       0,     0,    67,   114,     0,     0,     0,     0,   115,   116,
     117,   118,     0,     0,   119,    85,    39,    86,   120,   121,
       0,     0,    90,     0,    40,    91,     0,     0,    92,    93,
      94,     0,    96,     0,     0,     0,    42,     0,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   100,
      50,    51,    52,    53,    54,    55,    56,     0,     0,    57,
       0,     0,   101,    58,    59,    60,    61,    62,    63,   102,
       0,     0,     0,     0,   103,   104,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   105,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
     107,     0,   108,   109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,   111,
       0,   112,     0,     0,     0,     0,     0,     0,     0,    65,
     113,     0,     0,    67,   114,     0,     0,     0,     0,   115,
     116,   117,   118,     0,    22,   119,    85,    39,    86,   120,
     121,     0,     0,    90,     0,    40,    91,     0,     0,     0,
       0,     0,     0,    96,     0,     0,     0,    42,     0,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,    54,    55,    56,     0,     0,
      57,     0,     0,   101,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,   103,   173,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,   107,     0,   108,   551,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    23,   110,     0,     0,     0,
     174,     0,   112,     0,     0,     0,   553,     0,     0,     0,
      65,   113,     0,     0,    67,   114,     0,     0,     0,     0,
     115,   116,   117,   118,     0,     0,   119,    85,    39,    86,
       0,     0,     0,     0,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,    42,     0,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,   103,   173,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   257,     0,     0,   106,     0,     0,     0,
       0,     0,   107,     0,   275,   153,   154,     0,     0,     0,
       0,   155,     0,     0,     0,     0,     0,   110,     0,     0,
       0,   174,   276,   112,     0,     0,     0,     0,   277,   259,
       0,    65,   113,   156,   260,    67,   114,   158,   261,     0,
       0,   115,   116,   117,   118,     0,     0,   119,    85,    39,
      86,     0,     0,     0,     0,    90,     0,    40,    91,     0,
       0,     0,     0,     0,     0,    96,     0,     0,     0,    42,
       0,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,    54,    55,    56,
       0,     0,    57,     0,     0,   101,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,   103,   173,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   257,     0,     0,   106,     0,     0,
       0,     0,     0,   107,     0,   108,   153,   154,     0,     0,
       0,     0,   155,     0,     0,     0,     0,     0,   110,   258,
       0,     0,   174,     0,   112,     0,     0,     0,     0,     0,
     259,     0,    65,   113,   156,   260,    67,   114,   158,   261,
       0,     0,   115,   116,   117,   118,     0,     0,   119,    85,
      39,    86,     0,     0,     0,     0,    90,     0,    40,    91,
       0,     0,     0,     0,     0,     0,    96,     0,     0,     0,
      42,     0,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,    54,    55,
      56,     0,     0,    57,     0,     0,   101,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,   103,   173,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   257,     0,     0,   106,     0,
       0,     0,     0,     0,   107,     0,   108,   153,   154,     0,
       0,     0,     0,   155,     0,     0,     0,     0,     0,   110,
       0,     0,     0,   174,     0,   112,     0,     0,     0,     0,
       0,   259,     0,    65,   113,   156,   260,    67,   114,   158,
     261,     0,     0,   115,   116,   117,   118,     0,     0,   119,
      85,    39,    86,     0,     0,     0,     0,    90,     0,    40,
      91,     0,     0,     0,     0,     0,     0,    96,     0,     0,
       0,    42,     0,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,    54,
      55,    56,     0,     0,    57,     0,     0,   101,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,   103,
     173,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,   107,     0,   108,   551,     0,
       0,     0,     0,     0,     0,     0,     0,   552,     0,     0,
     110,     0,     0,     0,   174,     0,   112,     0,     0,     0,
     553,     0,     0,     0,    65,   113,     0,     0,    67,   114,
       0,     0,     0,     0,   115,   116,   117,   118,     0,     0,
     119,    85,    39,    86,     0,     0,     0,     0,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,   363,     0,
       0,     0,    42,     0,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
      54,    55,    56,     0,   364,    57,     0,     0,   101,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
     103,   173,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,   107,     0,   108,   365,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,     0,   174,     0,   112,     0,     0,
       0,     0,     0,     0,     0,    65,   113,     0,     0,    67,
     114,     0,     0,     0,     0,   115,   116,   117,   118,     0,
       0,   119,    85,    39,    86,     0,     0,     0,     0,    90,
       0,    40,    91,     0,     0,     0,     0,     0,     0,    96,
       0,     0,     0,    42,     0,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,    54,    55,    56,     0,     0,    57,     0,     0,   101,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,   103,   173,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,   107,     0,   108,
     551,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,   174,     0,   112,     0,
       0,     0,   553,     0,     0,     0,    65,   113,     0,     0,
      67,   114,     0,     0,     0,     0,   115,   116,   117,   118,
       0,     0,   119,    85,    39,    86,     0,     0,     0,     0,
      90,     0,    40,    91,     0,     0,     0,     0,     0,     0,
     363,     0,     0,     0,    42,     0,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,    54,    55,    56,     0,     0,    57,     0,     0,
     101,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,   103,   173,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,   107,     0,
     108,   365,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,     0,   174,     0,   112,
       0,     0,     0,     0,     0,     0,     0,    65,   113,     0,
       0,    67,   114,     0,     0,     0,     0,   115,   116,   117,
     118,     0,     0,   119,    85,    39,    86,     0,     0,     0,
       0,    90,     0,    40,    91,     0,     0,     0,     0,     0,
       0,    96,     0,     0,     0,    42,     0,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,    54,    55,    56,     0,     0,    57,     0,
       0,   101,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,   103,   173,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,   107,
       0,   108,   551,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   110,     0,     0,     0,   174,     0,
     112,     0,     0,     0,     0,     0,     0,     0,    65,   113,
       0,     0,    67,   114,     0,     0,     0,     0,   115,   116,
     117,   118,     0,     0,   119,    85,    39,    86,     0,     0,
       0,     0,    90,     0,    40,    91,     0,     0,     0,     0,
       0,     0,    96,     0,     0,     0,    42,     0,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   100,
      50,    51,    52,    53,    54,    55,    56,     0,     0,    57,
       0,     0,   101,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,   103,   173,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
     107,     0,   108,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,   174,
       0,   112,     0,     0,     0,     0,     0,     0,     0,    65,
     113,     0,     0,    67,   114,     0,     0,     0,     0,   115,
     116,   117,   118,     0,     0,   119,    85,    39,    86,     0,
       0,     0,     0,    90,     0,    40,    91,     0,     0,     0,
       0,     0,     0,    96,     0,     0,     0,    42,     0,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,    54,    55,    56,     0,     0,
      57,     0,     0,   101,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,   257,   103,   297,     0,     0,   298,
       0,     0,     0,     0,     0,   152,   153,   154,     0,     0,
       0,     0,   155,     0,     0,   106,     0,     0,     0,     0,
       0,   107,     0,   108,     0,     0,     0,     0,     0,     0,
     299,     0,     0,     0,   156,   260,   110,     0,   158,   261,
     174,     0,   112,     0,     0,     0,     0,     0,     0,     0,
      65,   113,     0,     0,    67,   114,     0,     0,     0,     0,
     115,   116,   117,   118,     0,     0,   119,    85,    39,    86,
       0,     0,     0,     0,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,    42,     0,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
     401,     0,   107,     0,     0,   250,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   110,     0,     0,
       0,   174,     0,   112,     0,     0,     0,     0,     0,     0,
       0,    65,   113,     0,     0,    67,   114,     0,     0,     0,
       0,   115,   116,   117,   118,     0,     0,   119,    85,    39,
      86,     0,     0,     0,     0,    90,     0,    40,    91,     0,
       0,     0,     0,     0,     0,    96,     0,     0,     0,    42,
       0,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,   107,     0,     0,   250,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   110,     0,
       0,     0,   174,     0,   112,     0,     0,     0,     0,     0,
       0,     0,    65,   113,     0,     0,    67,   114,     0,     0,
       0,     0,   115,   116,   117,   118,     0,     0,   119,    85,
      39,    86,     0,     0,     0,     0,    90,     0,    40,    91,
       0,     0,     0,     0,     0,     0,    96,     0,     0,     0,
      42,     0,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,    54,    55,
      56,     0,     0,    57,     0,     0,   101,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,   174,     0,   112,     0,     0,     0,     0,
       0,     0,     0,    65,   113,     0,     0,    67,   114,     0,
       0,     0,     0,   115,   116,   117,   118,     0,     0,   119,
      85,    39,    86,     0,     0,     0,     0,    90,     0,    40,
      91,     0,     0,     0,     0,     0,     0,    96,     0,     0,
       0,    42,     0,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   309,     0,     0,     0,     0,
     110,     0,     0,     0,   174,     0,   112,     0,     0,     0,
       0,     0,     0,     0,    65,   113,     0,     0,    67,   114,
       0,     0,     0,     0,   115,   116,   117,   118,     0,     0,
     119,    85,    39,    86,     0,     0,     0,     0,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,    42,     0,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,     0,   174,     0,   112,     0,    39,
       0,     0,     0,     0,     0,    65,   113,    40,     0,    67,
     114,     0,     0,     0,     0,   115,   116,   117,   118,    42,
       0,   119,     0,     0,   339,     0,    44,    45,    46,   180,
     181,   182,     0,     0,     0,    52,    53,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   183,     0,     0,     0,     0,     0,
       0,   344,     0,   345,     0,   185,   186,   187,     0,     0,
       0,     0,     0,     0,   188,     0,     0,     0,   189,     0,
      39,     0,   190,   346,   191,     0,     0,     0,    40,   277,
     192,     0,   193,    66,   194,   195,     0,    68,   196,   197,
      42,     0,     0,     0,     0,   339,     0,    44,    45,    46,
     180,   181,   182,     0,     0,     0,    52,    53,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   183,     0,     0,     0,     0,
       0,     0,     0,     0,   184,     0,   185,   186,   187,     0,
       0,     0,     0,     0,     0,   188,     0,     0,     0,   189,
     340,    39,     0,   190,     0,   191,     0,     0,     0,    40,
       0,   192,     0,   193,    66,   194,   195,     0,    68,   196,
     197,    42,     0,     0,     0,     0,   339,     0,    44,    45,
      46,   180,   181,   182,     0,     0,     0,    52,    53,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   183,     0,     0,     0,
       0,     0,     0,     0,     0,   184,     0,   185,   186,   187,
       0,     0,     0,     0,     0,     0,   188,     0,     0,     0,
     189,     0,    39,     0,   190,     0,   191,     0,     0,     0,
      40,     0,   192,     0,   193,    66,   194,   195,     0,    68,
     196,   197,    42,     0,     0,     0,     0,     0,     0,    44,
      45,    46,   180,   181,   182,     0,     0,     0,    52,    53,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   183,     0,     0,
       0,     0,     0,     0,     0,     0,   184,     0,   185,   186,
     187,     0,     0,     0,     0,     0,     0,   188,     0,     0,
       0,   189,     0,    39,     0,   190,   654,   191,     0,     0,
       0,    40,     0,   192,     0,   193,    66,   194,   195,     0,
      68,   196,   197,    42,     0,     0,     0,     0,     0,     0,
      44,    45,    46,   180,   181,   182,     0,     0,     0,    52,
      53,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   183,     0,
       0,     0,     0,     0,     0,     0,     0,   184,     0,   185,
     186,   187,     0,     0,     0,     0,     0,     0,   188,     0,
       0,     0,   189,     0,    39,     0,   190,     0,   191,     0,
       0,     0,    40,     0,   192,     0,   193,    66,   194,   195,
       0,    68,   196,   197,    42,     0,     0,     0,     0,     0,
       0,    44,    45,    46,   180,   181,   182,     0,     0,     0,
      52,    53,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   257,
       0,     0,     0,     0,     0,     0,     0,     0,   184,  -119,
       0,   186,   187,     0,     0,     0,    39,     0,     0,   188,
       0,     0,     0,   189,    40,     0,     0,   190,     0,   191,
       0,     0,     0,     0,     0,   651,    42,   193,    66,     0,
     260,     0,    68,    44,    45,    46,   180,   181,   182,     0,
       0,     0,    52,    53,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   257,     0,     0,     0,     0,     0,     0,     0,     0,
     184,     0,     0,   186,   187,     0,     0,     0,    39,     0,
       0,   188,     0,     0,     0,   189,    40,     0,     0,   190,
       0,   191,     0,     0,     0,     0,     0,   651,    42,   193,
      66,     0,   260,     0,    68,    44,    45,    46,   180,   181,
     182,     0,     0,     0,    52,    53,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   184,     0,     0,   186,   187,     0,     0,     0,
      39,     0,     0,   188,     0,     0,     0,   189,    40,     0,
       0,   190,     0,   191,     0,     0,     0,    41,     0,     0,
      42,   193,    66,     0,     0,    43,    68,    44,    45,    46,
      47,    48,    49,    39,    50,    51,    52,    53,    54,    55,
      56,    40,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,    42,     0,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,     0,    50,    51,    52,
      53,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    64,     0,     0,     0,   321,     0,     0,
       0,     0,     0,    65,    66,     0,     0,    67,    68,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    39,
       0,     0,     0,     0,     0,     0,    64,    40,     0,     0,
       0,     0,     0,     0,     0,     0,    65,    66,     0,    42,
      67,    68,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,    39,    50,    51,    52,    53,    54,    55,    56,
      40,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,    42,     0,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,     0,    50,    51,    52,    53,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,    39,     0,     0,     0,     0,     0,     0,     0,    40,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,    42,    65,    66,     0,     0,    67,    68,    44,    45,
      46,   180,   181,   182,     0,     0,     0,    52,    53,    54,
      55,    56,     0,     0,    57,     0,    39,     0,    58,    59,
      60,    61,    62,    63,    40,    65,   113,     0,     0,    67,
     114,     0,     0,     0,     0,     0,    42,     0,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,     0,
      50,    51,    52,    53,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,    39,     0,     0,     0,     0,     0,     0,
       0,    40,     0,     0,   193,    66,     0,     0,     0,    68,
     447,     0,     0,    42,     0,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,     0,    50,    51,    52,
      53,    54,    55,    56,     0,     0,    57,     0,    39,   242,
      58,    59,    60,    61,    62,    63,    40,     0,     0,    65,
       0,     0,     0,    67,     0,     0,     0,     0,    42,     0,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,     0,    50,    51,    52,    53,    54,    55,    56,     0,
       0,    57,     0,     0,    39,    58,    59,    60,    61,    62,
      63,     0,    40,     0,     0,     0,   242,     0,     0,     0,
       0,     0,     0,     0,    42,     0,    65,     0,     0,    43,
      67,    44,    45,    46,    47,    48,    49,    39,    50,    51,
      52,    53,    54,    55,    56,    40,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,    42,     0,     0,
       0,     0,     0,     0,    44,    45,    46,   180,   181,   182,
       0,    65,   113,    52,    53,    54,    55,    56,     0,     0,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,    39,     0,     0,     0,     0,
       0,     0,     0,    40,     0,     0,     0,   233,     0,     0,
       0,     0,     0,     0,     0,    42,     0,    65,     0,     0,
       0,     0,    44,    45,    46,   180,   181,   182,    39,     0,
     563,    52,    53,    54,    55,    56,    40,     0,    57,     0,
     564,     0,    58,    59,    60,    61,    62,    63,    42,     0,
     193,     0,     0,     0,     0,    44,    45,    46,   180,   181,
     182,     0,     0,     0,    52,    53,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,   648,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   564,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   193,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   193
  };

  const short
  parser::yycheck_[] =
  {
      29,    87,    29,    87,   301,   105,    64,    98,   190,    34,
     105,   112,    98,    41,    98,   166,   199,   189,   190,   245,
      34,   332,   110,   111,   298,   188,   100,    86,    99,   101,
      87,   110,    91,    64,   106,   107,     5,   109,   176,   131,
     108,   473,   326,     1,   460,   217,     1,     1,   466,    21,
      27,   110,   111,   111,    82,   110,   111,   333,    19,    12,
      27,   133,   259,    80,    88,   291,    22,   103,   100,   626,
      77,    77,   259,    78,    77,   626,     0,   104,   104,    84,
     111,   113,    14,   380,   176,    99,   174,   371,   114,   190,
     191,    77,   128,   111,   110,    51,    52,   114,   114,   100,
     131,   385,   299,   189,   190,   129,   192,    84,   665,   114,
     667,   114,   299,   199,   665,   174,   174,    84,   204,   174,
     179,   128,   128,   295,   113,   263,   309,    99,   114,   107,
     406,   217,   564,   549,   408,   553,   165,   166,   165,   166,
     226,   438,   103,   174,   102,   176,   103,   102,   102,   128,
     107,   107,   109,    80,   107,   113,   174,    27,   113,    80,
      80,   118,   118,   192,   103,   122,   122,   108,   107,    77,
     109,   263,   100,   114,   188,   233,    24,   107,   250,   118,
     268,   253,   466,   255,   242,   113,   358,   114,   118,    79,
      77,   623,   122,   114,   114,   117,   354,   113,   108,   168,
     169,   287,   227,   228,   114,   363,   128,   275,   294,   295,
     294,   242,   239,   227,   228,   387,    79,   118,   108,   530,
     531,   122,   110,   309,   114,   296,   114,    90,    91,    92,
     120,   108,   263,   291,    97,   108,    79,   294,   297,   110,
     312,   114,   114,   114,   108,    88,   332,    90,    91,    92,
     114,   110,   115,   107,    97,   114,   119,   120,    90,    91,
      92,   288,   289,   113,    86,    97,   357,    89,    99,   543,
     101,   357,   358,   357,   412,   559,   119,   120,   107,   304,
     123,   124,   296,    81,   311,   650,    84,   119,   653,   103,
     364,   123,   384,   365,    99,   128,   101,   397,   470,   327,
      71,   387,   397,   475,   455,   579,   129,   479,   534,   472,
     621,    99,    99,   101,   611,    87,    99,   489,   101,   128,
     412,   409,    84,   411,    86,   425,   414,   386,   416,    79,
     409,   513,   411,   108,   608,   414,    86,   416,    88,   108,
      90,   108,   401,    65,    66,   642,    80,    81,   407,   108,
     409,   108,   411,   384,   409,   414,   411,   416,   108,   414,
     108,   416,   120,   121,   114,   537,     4,    23,   114,   119,
     120,    89,    27,   123,   124,    90,    91,    92,   113,    77,
      77,   412,    97,    77,   470,    77,    15,   473,    81,   475,
      86,   563,   129,   479,    72,    73,    74,    75,    77,   104,
     626,   114,   104,   489,   119,    80,   465,   466,   114,   108,
     593,   425,   513,   514,   128,   100,    77,   108,   108,   108,
     108,   104,    89,    77,   100,    85,   455,   114,   455,   115,
     114,   460,   114,   460,   493,   108,   108,   108,   497,   665,
     108,   115,   108,   626,   530,   531,    85,    25,   128,   114,
     108,   537,   129,   104,    77,    77,   108,    77,   472,   547,
      77,    80,   115,    81,    80,   115,   114,   650,   547,   114,
     653,   115,   100,   645,   114,   500,   534,   563,   564,   551,
      81,   107,   665,   542,   667,    78,   500,   114,   547,   104,
     104,   115,   547,    77,   553,    29,     9,   108,   100,   108,
     108,   113,   115,    84,    80,    11,    89,   593,    53,    86,
     114,   118,   108,   108,   294,    15,    79,    80,   165,   546,
     549,   580,   549,   123,   228,   387,   287,    90,    91,    92,
     590,   531,   668,   645,    97,   621,   217,   623,   472,   624,
     626,   204,   633,   650,   596,   634,    34,   633,   607,   633,
     609,   651,   115,   500,   301,   256,   119,   120,    34,   645,
     123,   124,   572,   608,   650,   594,   607,   653,   659,   611,
     219,   546,   495,   659,   398,   659,   397,   668,    -1,   665,
      -1,   667,   668,   253,   668,   534,   192,   646,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
      -1,    10,   661,    12,    13,   634,    -1,    16,    17,    18,
      19,    20,    -1,    22,    -1,    24,    -1,    26,    -1,    28,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      -1,    79,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    91,    92,    -1,    -1,    76,    -1,    97,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    -1,    -1,    -1,    -1,   115,    -1,    -1,
      -1,   119,   120,    -1,   103,   123,   124,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,
     129,   130,    -1,    -1,   133,     3,     4,     5,   137,   138,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    16,    17,
      18,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,
     128,   129,   130,    -1,     1,   133,     3,     4,     5,   137,
     138,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
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
     117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,
     127,   128,   129,   130,    -1,    -1,   133,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
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
      -1,   127,   128,   129,   130,    -1,    -1,   133,     3,     4,
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
      -1,    -1,   127,   128,   129,   130,    -1,    -1,   133,     3,
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
     124,    -1,    -1,   127,   128,   129,   130,    -1,    -1,   133,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,
     103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,
     113,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,    -1,    -1,    -1,   127,   128,   129,   130,    -1,    -1,
     133,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    46,    47,    -1,    -1,    50,    51,
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
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,   113,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,   130,
      -1,    -1,   133,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,
      -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,
     130,    -1,    -1,   133,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,
     129,   130,    -1,    -1,   133,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,
     128,   129,   130,    -1,    -1,   133,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    79,    62,    81,    -1,    -1,    84,
      -1,    -1,    -1,    -1,    -1,    90,    91,    92,    -1,    -1,
      -1,    -1,    97,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,
     115,    -1,    -1,    -1,   119,   120,   103,    -1,   123,   124,
     107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,
     127,   128,   129,   130,    -1,    -1,   133,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      86,    -1,    88,    -1,    -1,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,
      -1,   127,   128,   129,   130,    -1,    -1,   133,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,
      -1,    -1,   127,   128,   129,   130,    -1,    -1,   133,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,
      -1,    -1,    -1,   127,   128,   129,   130,    -1,    -1,   133,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,    -1,    -1,    -1,   127,   128,   129,   130,    -1,    -1,
     133,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,     4,
      -1,    -1,    -1,    -1,    -1,   117,   118,    12,    -1,   121,
     122,    -1,    -1,    -1,    -1,   127,   128,   129,   130,    24,
      -1,   133,    -1,    -1,    29,    -1,    31,    32,    33,    34,
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
      -1,    -1,   103,    -1,     4,    -1,   107,    -1,   109,    -1,
      -1,    -1,    12,    -1,   115,    -1,   117,   118,   119,   120,
      -1,   122,   123,   124,    24,    -1,    -1,    -1,    -1,    -1,
      -1,    31,    32,    33,    34,    35,    36,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    89,
      -1,    91,    92,    -1,    -1,    -1,     4,    -1,    -1,    99,
      -1,    -1,    -1,   103,    12,    -1,    -1,   107,    -1,   109,
      -1,    -1,    -1,    -1,    -1,   115,    24,   117,   118,    -1,
     120,    -1,   122,    31,    32,    33,    34,    35,    36,    -1,
      -1,    -1,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    91,    92,    -1,    -1,    -1,     4,    -1,
      -1,    99,    -1,    -1,    -1,   103,    12,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,   115,    24,   117,
     118,    -1,   120,    -1,   122,    31,    32,    33,    34,    35,
      36,    -1,    -1,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    -1,    91,    92,    -1,    -1,    -1,
       4,    -1,    -1,    99,    -1,    -1,    -1,   103,    12,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,    21,    -1,    -1,
      24,   117,   118,    -1,    -1,    29,   122,    31,    32,    33,
      34,    35,    36,     4,    38,    39,    40,    41,    42,    43,
      44,    12,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    -1,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   107,    -1,    -1,    -1,    78,    -1,    -1,
      -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,   107,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    24,
     121,   122,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,     4,    38,    39,    40,    41,    42,    43,    44,
      12,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    -1,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    24,   117,   118,    -1,    -1,   121,   122,    31,    32,
      33,    34,    35,    36,    -1,    -1,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,     4,    -1,    51,    52,
      53,    54,    55,    56,    12,   117,   118,    -1,    -1,   121,
     122,    -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    -1,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    -1,    -1,   117,   118,    -1,    -1,    -1,   122,
      78,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    -1,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,     4,   107,
      51,    52,    53,    54,    55,    56,    12,    -1,    -1,   117,
      -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    -1,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,     4,    51,    52,    53,    54,    55,
      56,    -1,    12,    -1,    -1,    -1,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    24,    -1,   117,    -1,    -1,    29,
     121,    31,    32,    33,    34,    35,    36,     4,    38,    39,
      40,    41,    42,    43,    44,    12,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    24,    -1,    -1,
      -1,    -1,    -1,    -1,    31,    32,    33,    34,    35,    36,
      -1,   117,   118,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    12,    -1,    -1,    -1,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    24,    -1,   117,    -1,    -1,
      -1,    -1,    31,    32,    33,    34,    35,    36,     4,    -1,
      97,    40,    41,    42,    43,    44,    12,    -1,    47,    -1,
     107,    -1,    51,    52,    53,    54,    55,    56,    24,    -1,
     117,    -1,    -1,    -1,    -1,    31,    32,    33,    34,    35,
      36,    -1,    -1,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    97,    -1,
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
     103,   107,   109,   118,   122,   127,   128,   129,   130,   133,
     137,   138,   166,   170,   171,   172,   173,   177,   190,   225,
     230,   235,   236,   240,   241,   242,   243,   269,   270,   273,
     274,   296,   297,   299,   307,   308,   311,   104,   114,   313,
      79,    88,    90,    91,    92,    97,   119,   120,   123,   124,
     302,   303,   304,   306,   108,   114,   107,   152,    99,   101,
     144,   313,   113,    63,   107,   233,   234,   236,   237,   239,
      34,    35,    36,    79,    88,    90,    91,    92,    99,   103,
     107,   109,   115,   117,   119,   120,   123,   124,   179,   192,
     193,   196,   198,   200,   202,   203,   205,   278,   279,   281,
     283,   285,   286,   293,   294,   295,   305,   107,    99,   101,
     262,   233,    72,    73,    74,    75,   174,    99,   101,   182,
     183,   198,   200,   107,   296,   299,   262,   241,   103,   231,
     232,   128,   107,   297,   103,   107,   271,   272,   274,   308,
      91,   241,   260,   261,   241,   240,   241,    79,   104,   115,
     120,   124,   233,   234,   244,   246,   247,   276,   290,   292,
     301,   302,   304,   309,   310,    90,   108,   114,   244,   245,
     302,   303,   304,   309,   314,   110,   314,    19,   231,   231,
     129,   165,   156,    71,   180,    80,   114,    81,    84,   115,
     227,   228,   229,   276,   289,   291,   300,   302,   303,    98,
     241,    99,    87,   128,   108,   108,   108,   108,   108,   108,
     151,    78,   153,   154,   155,   146,   146,     4,   162,   117,
     128,    23,    80,   289,   233,   190,   218,   219,   220,    29,
     104,   194,   196,   198,    86,    88,   108,   194,   209,   285,
     314,   314,   283,   295,    27,   184,   205,    89,    86,   203,
     194,   208,   209,    20,    46,    91,   233,   259,   263,   264,
     265,   263,   113,   238,    77,    77,    77,    77,   188,   194,
     206,   181,   225,   226,   235,   181,    15,    81,   303,   129,
      88,   129,   297,    77,    77,   128,   310,   114,    80,   187,
     241,    86,   260,   227,   298,   307,   289,    78,    84,   114,
     104,   114,   234,   108,   114,   108,   114,   108,   108,   108,
     114,   110,   206,   297,   297,   115,   167,   275,   287,   288,
     303,   310,   128,   179,   189,   194,   195,   296,   233,   249,
     250,   265,   298,   184,   229,   236,   205,    78,   266,   267,
     268,   297,   241,   108,   108,   114,   100,   312,   313,    12,
     107,   163,   164,    77,    77,    99,   101,   251,   188,   237,
      80,   100,   114,   210,   104,    89,   108,   108,   108,   114,
     108,   108,   110,   115,   115,   183,   198,   194,   108,   114,
     183,   262,   241,    85,   100,   113,   312,    25,   184,   100,
     113,   312,   233,   195,   108,   128,   129,   104,    77,    77,
     108,   272,   103,   107,   109,   277,   278,    77,   233,   115,
     115,   233,   248,   265,   233,   244,   244,   244,   244,    77,
      80,    80,   299,   308,   114,    77,   128,    80,    81,   178,
     213,   184,    81,   114,   115,   100,   114,    81,   155,   107,
     149,    91,   100,   113,   233,   252,   253,   254,   258,   252,
     312,   194,   220,    97,   107,   211,   293,   194,   194,   194,
     233,   264,   233,   226,   104,    88,   108,   285,   110,   114,
      78,   188,   191,   191,   115,   115,   287,    77,   194,   212,
      29,   214,   215,   216,     9,   221,   222,   223,   233,   265,
     267,   244,   149,   108,   241,   252,   100,   113,    84,    86,
     255,   256,   257,   312,   194,   293,   108,   238,   265,   233,
      77,   114,    77,   210,    84,   197,   201,   204,   205,   217,
      22,    51,    52,   107,   175,   224,   282,   283,   223,   108,
     254,   249,   233,   184,   257,    80,    11,   188,    97,   215,
      89,   115,   204,   275,   108,   198,   199,   207,   224,    53,
     176,    86,   184,   212,   233,   201,   217,   201,   114,   108,
     198,   233,   108,   207
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
       1,     1,     4,     1,     1,     4,     3,     4,     5,     4,
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
       0,   475,   475,   492,   493,   495,   499,   500,   501,   503,
     504,   506,   507,   510,   512,   513,   514,   522,   523,   525,
     527,   528,   530,   531,   533,   534,   535,   537,   538,   540,
     541,   543,   544,   548,   549,   551,   552,   554,   556,   557,
     559,   572,   573,   575,   576,   578,   579,   583,   584,   589,
     590,   592,   593,   594,   596,   597,   601,   603,   604,   606,
     607,   608,   611,   618,   620,   621,   623,   625,   626,   627,
     632,   637,   638,   639,   640,   641,   643,   644,   645,   647,
     687,   688,   690,   691,   700,   701,   703,   704,   705,   749,
     750,   751,   752,   754,   755,   757,   759,   760,   768,   769,
     771,   772,   773,   786,   787,   789,   791,   793,   794,   796,
     797,   801,   807,   808,   815,   816,   818,   820,   829,   831,
     833,   834,   836,   839,   841,   842,   844,   845,   847,   848,
     849,   855,   862,   863,   864,   865,   866,   867,   868,   874,
     875,   879,   881,   882,   884,   885,   887,   888,   895,   896,
     899,   900,   918,   924,   926,   927,   929,   930,   932,   933,
     935,   936,   938,   939,   941,   942,   944,   946,   947,   949,
     950,   952,   953,   954,   956,   957,   958,   963,   965,   966,
     968,   972,   973,   975,   976,   980,   990,   991,   993,   994,
     995,   996,   997,   998,   999,  1002,  1003,  1005,  1006,  1010,
    1011,  1013,  1014,  1016,  1017,  1019,  1020,  1021,  1023,  1024,
    1027,  1028,  1030,  1031,  1035,  1036,  1037,  1038,  1040,  1041,
    1042,  1043,  1045,  1047,  1048,  1049,  1051,  1053,  1054,  1056,
    1057,  1058,  1059,  1060,  1065,  1066,  1071,  1072,  1073,  1078,
    1079,  1097,  1098,  1099,  1100,  1101,  1102,  1103,  1105,  1106,
    1119,  1121,  1131,  1133,  1134,  1137,  1138,  1139,  1140,  1142,
    1143,  1145,  1146,  1147,  1149,  1151,  1152,  1154,  1155,  1164,
    1166,  1167,  1169,  1170,  1172,  1173,  1175,  1176,  1179,  1180,
    1182,  1183,  1184,  1185,  1190,  1191,  1193,  1194,  1195,  1200,
    1201,  1203,  1204,  1205,  1207,  1208,  1240,  1241,  1243,  1244,
    1246,  1247,  1248,  1250,  1251,  1253,  1254,  1255,  1256,  1258,
    1259,  1261,  1262,  1264,  1265,  1268,  1269,  1270,  1272,  1273,
    1274,  1275,  1276,  1278,  1279,  1280,  1282,  1283,  1284,  1285,
    1286,  1289,  1290,  1292,  1294,  1295,  1299,  1301,  1302,  1303,
    1305,  1306,  1307,  1308,  1313,  1314,  1316,  1317,  1319,  1320,
    1323,  1324,  1329,  1330,  1332,  1333,  1337,  1339,  1341,  1342,
    1343,  1344,  1345,  1348,  1349,  1351,  1352,  1353,  1355,  1356,
    1358,  1359,  1360,  1361,  1362,  1363,  1364,  1365,  1367,  1368,
    1370,  1371,  1373,  1375,  1376,  1378,  1379,  1381,  1382,  1383,
    1384,  1385,  1386,  1387,  1388,  1389,  1390,  1391,  1392,  1393,
    1394,  1395,  1396,  1397,  1398,  1400,  1401,  1402,  1406,  1407,
    1409,  1411,  1412,  1414,  1415,  1419,  1420,  1421,  1422,  1423,
    1428,  1431,  1435,  1436,  1438,  1439
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
#line 6014 "parser.cc"

#line 1448 "parser.y"


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
std::tuple<string, vector<expression_ref>>
check_type_or_class_header(const Hs::Type& type)
{
    auto [type_head, type_args] = decompose_type_apps(type);

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

Hs::TypeSynonymDecl make_type_synonym(const Located<expression_ref>& lhs_type, const Located<expression_ref>& rhs_type)
{
    auto [name, type_args] = check_type_or_class_header(unloc(lhs_type));
    return {name, check_all_type_vars(type_args), rhs_type};
}

Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context&  context,
                                                const expression_ref& header, const vector<Hs::Constructor>& constrs)
{
    auto [name, type_args] = check_type_or_class_header(header);
    if (d_or_n == Hs::DataOrNewtype::newtype and constrs.size() != 1)
        throw myexception()<<"newtype '"<<name<<"' may only have 1 constructors with 1 field";
    return {d_or_n, name, check_all_type_vars(type_args), context, constrs};
}

Hs::InstanceDecl make_instance_decl(const Located<expression_ref>& ltype, const optional<Located<Hs::Binds>>& binds)
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

Hs::ClassDecl make_class_decl(const Hs::Context& context, const expression_ref& header, const optional<Located<Hs::Binds>>& binds)
{
    auto [name, type_args] = check_type_or_class_header(header);
    return {name, check_all_type_vars(type_args), context, binds};
}

// Can we change the context parsing rule to expect:
// nothing
// | ctype => header
// | ( ctypes2 ) => header
Hs::Context make_context(const expression_ref& context)
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

bool check_kind(const Hs::Kind& kind)
{
    auto [kind_head, kind_args] = decompose_type_apps(kind);

    if (not kind_head.is_a<Hs::TypeCon>()) return false;

    auto V = kind_head.as_<Hs::TypeCon>();
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

Hs::Type make_kind(const Hs::Kind& kind)
{
    if (not check_kind(kind))
        throw myexception()<<"Kind '"<<kind<<"' is malformed";

    return kind;
}

optional<pair<string, Hs::FieldDecls>> is_record_con(const expression_ref& typeish)
{
    auto [head,args] = decompose_type_apps(typeish);

    if (args.size() != 1) return {};

    if (not head.is_a<Hs::TypeCon>()) return {};

    if (not args[0].is_a<Hs::FieldDecls>()) return {};

    return {{unloc(head.as_<Hs::TypeCon>().name), args[0].as_<Hs::FieldDecls>()}};
}

optional<pair<string, std::vector<expression_ref>>> is_normal_con(const expression_ref& typeish)
{
    if (is_record_con(typeish)) return {};

    auto [head,args] = decompose_type_apps(typeish);

    if (not head.is_a<Hs::TypeCon>())
        return {};

    return {{unloc(head.as_<Hs::TypeCon>().name), args}};
}

Hs::Constructor make_constructor(const vector<Hs::TypeVar>& forall, const std::optional<Hs::Context>& c, const expression_ref& typeish)
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
    return Hs::List(chars);
}
