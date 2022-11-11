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
#line 55 "parser.y"

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
        value.YY_MOVE_OR_COPY< Hs::ConstructorDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        value.YY_MOVE_OR_COPY< Hs::ConstructorsDecl > (YY_MOVE (that.value));
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
      case symbol_kind::S_decls_cls: // decls_cls
      case symbol_kind::S_decls_inst: // decls_inst
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

      case symbol_kind::S_gadt_constr: // gadt_constr
        value.YY_MOVE_OR_COPY< Hs::GADTConstructorDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gadt_constrs: // gadt_constrs
        value.YY_MOVE_OR_COPY< Hs::GADTConstructorsDecl > (YY_MOVE (that.value));
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
      case symbol_kind::S_sigktype: // sigktype
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ktype: // ktype
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
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        value.YY_MOVE_OR_COPY< Hs::TypeVar > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alt: // alt
        value.YY_MOVE_OR_COPY< Located<Hs::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decllist_cls: // decllist_cls
      case symbol_kind::S_decllist_inst: // decllist_inst
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
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
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

      case symbol_kind::S_gadt_constrlist: // gadt_constrlist
        value.YY_MOVE_OR_COPY< std::optional<Hs::GADTConstructorsDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.YY_MOVE_OR_COPY< std::optional<Hs::ImpSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_opt_kind_sig: // opt_kind_sig
        value.YY_MOVE_OR_COPY< std::optional<Hs::Kind> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
      case symbol_kind::S_wherebinds: // wherebinds
        value.YY_MOVE_OR_COPY< std::optional<Located<Hs::Binds>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        value.YY_MOVE_OR_COPY< std::optional<Located<Hs::Kind>> > (YY_MOVE (that.value));
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
      case symbol_kind::S_con_list: // con_list
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
        value.move< Hs::ConstructorDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        value.move< Hs::ConstructorsDecl > (YY_MOVE (that.value));
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
      case symbol_kind::S_decls_cls: // decls_cls
      case symbol_kind::S_decls_inst: // decls_inst
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

      case symbol_kind::S_gadt_constr: // gadt_constr
        value.move< Hs::GADTConstructorDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gadt_constrs: // gadt_constrs
        value.move< Hs::GADTConstructorsDecl > (YY_MOVE (that.value));
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
      case symbol_kind::S_sigktype: // sigktype
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ktype: // ktype
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
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        value.move< Hs::TypeVar > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alt: // alt
        value.move< Located<Hs::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decllist_cls: // decllist_cls
      case symbol_kind::S_decllist_inst: // decllist_inst
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
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
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

      case symbol_kind::S_gadt_constrlist: // gadt_constrlist
        value.move< std::optional<Hs::GADTConstructorsDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.move< std::optional<Hs::ImpSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_opt_kind_sig: // opt_kind_sig
        value.move< std::optional<Hs::Kind> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
      case symbol_kind::S_wherebinds: // wherebinds
        value.move< std::optional<Located<Hs::Binds>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        value.move< std::optional<Located<Hs::Kind>> > (YY_MOVE (that.value));
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
      case symbol_kind::S_con_list: // con_list
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
        value.copy< Hs::ConstructorDecl > (that.value);
        break;

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        value.copy< Hs::ConstructorsDecl > (that.value);
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
      case symbol_kind::S_decls_cls: // decls_cls
      case symbol_kind::S_decls_inst: // decls_inst
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

      case symbol_kind::S_gadt_constr: // gadt_constr
        value.copy< Hs::GADTConstructorDecl > (that.value);
        break;

      case symbol_kind::S_gadt_constrs: // gadt_constrs
        value.copy< Hs::GADTConstructorsDecl > (that.value);
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
      case symbol_kind::S_sigktype: // sigktype
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ktype: // ktype
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
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        value.copy< Hs::TypeVar > (that.value);
        break;

      case symbol_kind::S_alt: // alt
        value.copy< Located<Hs::Alt> > (that.value);
        break;

      case symbol_kind::S_decllist_cls: // decllist_cls
      case symbol_kind::S_decllist_inst: // decllist_inst
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
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
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

      case symbol_kind::S_gadt_constrlist: // gadt_constrlist
        value.copy< std::optional<Hs::GADTConstructorsDecl> > (that.value);
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.copy< std::optional<Hs::ImpSpec> > (that.value);
        break;

      case symbol_kind::S_opt_kind_sig: // opt_kind_sig
        value.copy< std::optional<Hs::Kind> > (that.value);
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
      case symbol_kind::S_wherebinds: // wherebinds
        value.copy< std::optional<Located<Hs::Binds>> > (that.value);
        break;

      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        value.copy< std::optional<Located<Hs::Kind>> > (that.value);
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
      case symbol_kind::S_con_list: // con_list
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
        value.move< Hs::ConstructorDecl > (that.value);
        break;

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        value.move< Hs::ConstructorsDecl > (that.value);
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
      case symbol_kind::S_decls_cls: // decls_cls
      case symbol_kind::S_decls_inst: // decls_inst
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

      case symbol_kind::S_gadt_constr: // gadt_constr
        value.move< Hs::GADTConstructorDecl > (that.value);
        break;

      case symbol_kind::S_gadt_constrs: // gadt_constrs
        value.move< Hs::GADTConstructorsDecl > (that.value);
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
      case symbol_kind::S_sigktype: // sigktype
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ktype: // ktype
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
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        value.move< Hs::TypeVar > (that.value);
        break;

      case symbol_kind::S_alt: // alt
        value.move< Located<Hs::Alt> > (that.value);
        break;

      case symbol_kind::S_decllist_cls: // decllist_cls
      case symbol_kind::S_decllist_inst: // decllist_inst
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
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
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

      case symbol_kind::S_gadt_constrlist: // gadt_constrlist
        value.move< std::optional<Hs::GADTConstructorsDecl> > (that.value);
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.move< std::optional<Hs::ImpSpec> > (that.value);
        break;

      case symbol_kind::S_opt_kind_sig: // opt_kind_sig
        value.move< std::optional<Hs::Kind> > (that.value);
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
      case symbol_kind::S_wherebinds: // wherebinds
        value.move< std::optional<Located<Hs::Binds>> > (that.value);
        break;

      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        value.move< std::optional<Located<Hs::Kind>> > (that.value);
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
      case symbol_kind::S_con_list: // con_list
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
        yylhs.value.emplace< Hs::ConstructorDecl > ();
        break;

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        yylhs.value.emplace< Hs::ConstructorsDecl > ();
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
      case symbol_kind::S_decls_cls: // decls_cls
      case symbol_kind::S_decls_inst: // decls_inst
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

      case symbol_kind::S_gadt_constr: // gadt_constr
        yylhs.value.emplace< Hs::GADTConstructorDecl > ();
        break;

      case symbol_kind::S_gadt_constrs: // gadt_constrs
        yylhs.value.emplace< Hs::GADTConstructorsDecl > ();
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
      case symbol_kind::S_sigktype: // sigktype
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ktype: // ktype
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
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        yylhs.value.emplace< Hs::TypeVar > ();
        break;

      case symbol_kind::S_alt: // alt
        yylhs.value.emplace< Located<Hs::Alt> > ();
        break;

      case symbol_kind::S_decllist_cls: // decllist_cls
      case symbol_kind::S_decllist_inst: // decllist_inst
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
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
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

      case symbol_kind::S_gadt_constrlist: // gadt_constrlist
        yylhs.value.emplace< std::optional<Hs::GADTConstructorsDecl> > ();
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        yylhs.value.emplace< std::optional<Hs::ImpSpec> > ();
        break;

      case symbol_kind::S_opt_kind_sig: // opt_kind_sig
        yylhs.value.emplace< std::optional<Hs::Kind> > ();
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
      case symbol_kind::S_wherebinds: // wherebinds
        yylhs.value.emplace< std::optional<Located<Hs::Binds>> > ();
        break;

      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        yylhs.value.emplace< std::optional<Located<Hs::Kind>> > ();
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
      case symbol_kind::S_con_list: // con_list
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
#line 497 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2281 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 514 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < std::string > (),yystack_[2].value.as < std::optional<std::vector<Hs::Export>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2287 "parser.cc"
    break;

  case 4: // module: body2
#line 515 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{"Main",{},yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2293 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 517 "parser.y"
                                                                 {drv.push_module_context();}
#line 2299 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 525 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2305 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 526 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2311 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 528 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2317 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 529 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2323 "parser.cc"
    break;

  case 13: // top: semis top1
#line 532 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2329 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 534 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2335 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 535 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2341 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 536 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::ImpDecl> > (),{});}
#line 2347 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 544 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::Export>> > () = yystack_[1].value.as < std::vector<Hs::Export> > ();}
#line 2353 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 545 "parser.y"
                                      {}
#line 2359 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 547 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > () = yystack_[0].value.as < std::vector<Hs::Export> > ();}
#line 2365 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 549 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > () = yystack_[2].value.as < std::vector<Hs::Export> > (); yylhs.value.as < std::vector<Hs::Export> > ().push_back(yystack_[0].value.as < Hs::Export > ());}
#line 2371 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 550 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > ().push_back(yystack_[0].value.as < Hs::Export > ());}
#line 2377 "parser.cc"
    break;

  case 22: // export: qcname export_subspec
#line 552 "parser.y"
                                      {yylhs.value.as < Hs::Export > () = Hs::ExportSymbol{yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}; }
#line 2383 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 553 "parser.y"
                                      {yylhs.value.as < Hs::Export > () = Hs::ExportModule{{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2389 "parser.cc"
    break;

  case 24: // export_subspec: %empty
#line 555 "parser.y"
                                      {}
#line 2395 "parser.cc"
    break;

  case 25: // export_subspec: "(" qcnames ")"
#line 556 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpecSome{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2401 "parser.cc"
    break;

  case 26: // export_subspec: "(" ".." ")"
#line 557 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpecAll(); }
#line 2407 "parser.cc"
    break;

  case 27: // qcnames: %empty
#line 559 "parser.y"
                   {}
#line 2413 "parser.cc"
    break;

  case 28: // qcnames: qcnames1
#line 560 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2419 "parser.cc"
    break;

  case 29: // qcnames1: qcnames1 "," qcname
#line 562 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2425 "parser.cc"
    break;

  case 30: // qcnames1: qcname
#line 563 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2431 "parser.cc"
    break;

  case 31: // qcname: qvar
#line 565 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2437 "parser.cc"
    break;

  case 32: // qcname: oqtycon_no_varcon
#line 566 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2443 "parser.cc"
    break;

  case 37: // importdecls: importdecls_semi importdecl
#line 576 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[1].value.as < std::vector<Hs::ImpDecl> > (), yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[0].value.as < Hs::ImpDecl > ()); }
#line 2449 "parser.cc"
    break;

  case 38: // importdecls_semi: importdecls_semi importdecl semis1
#line 578 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[2].value.as < std::vector<Hs::ImpDecl> > (); yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[1].value.as < Hs::ImpDecl > ()); }
#line 2455 "parser.cc"
    break;

  case 39: // importdecls_semi: %empty
#line 579 "parser.y"
                         { }
#line 2461 "parser.cc"
    break;

  case 40: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 581 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::ImpDecl > () = Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < std::string > (),yystack_[1].value.as < std::optional<std::string> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ());
}
#line 2469 "parser.cc"
    break;

  case 41: // optqualified: "qualified"
#line 594 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2475 "parser.cc"
    break;

  case 42: // optqualified: %empty
#line 595 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2481 "parser.cc"
    break;

  case 43: // maybeas: "as" modid
#line 597 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 2487 "parser.cc"
    break;

  case 44: // maybeas: %empty
#line 598 "parser.y"
                               { }
#line 2493 "parser.cc"
    break;

  case 45: // maybeimpspec: impspec
#line 600 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2499 "parser.cc"
    break;

  case 46: // maybeimpspec: %empty
#line 601 "parser.y"
                               { }
#line 2505 "parser.cc"
    break;

  case 47: // impspec: "(" exportlist ")"
#line 605 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::Export> > ()}; }
#line 2511 "parser.cc"
    break;

  case 48: // impspec: "hiding" "(" exportlist ")"
#line 606 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::Export> > ()}; }
#line 2517 "parser.cc"
    break;

  case 49: // prec: %empty
#line 611 "parser.y"
                   { }
#line 2523 "parser.cc"
    break;

  case 50: // prec: "INTEGER"
#line 612 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < int > (); }
#line 2529 "parser.cc"
    break;

  case 51: // infix: "infix"
#line 614 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2535 "parser.cc"
    break;

  case 52: // infix: "infixl"
#line 615 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2541 "parser.cc"
    break;

  case 53: // infix: "infixr"
#line 616 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2547 "parser.cc"
    break;

  case 54: // ops: ops "," op
#line 618 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 2553 "parser.cc"
    break;

  case 55: // ops: op
#line 619 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 2559 "parser.cc"
    break;

  case 56: // topdecls: topdecls_semi topdecl
#line 623 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 2565 "parser.cc"
    break;

  case 57: // topdecls_semi: topdecls_semi topdecl semis1
#line 625 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2571 "parser.cc"
    break;

  case 58: // topdecls_semi: %empty
#line 626 "parser.y"
                                            { }
#line 2577 "parser.cc"
    break;

  case 59: // topdecl: cl_decl
#line 628 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2583 "parser.cc"
    break;

  case 60: // topdecl: ty_decl
#line 629 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2589 "parser.cc"
    break;

  case 61: // topdecl: standalone_kind_sig
#line 630 "parser.y"
                                               {}
#line 2595 "parser.cc"
    break;

  case 62: // topdecl: inst_decl
#line 631 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2601 "parser.cc"
    break;

  case 63: // topdecl: "default" "(" comma_types0 ")"
#line 634 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Hs::DefaultDecl(yystack_[1].value.as < std::vector<Hs::Type> > ()); }
#line 2607 "parser.cc"
    break;

  case 64: // topdecl: "foreign" "import" "bpcall" "STRING" var "::" sigtypedoc
#line 635 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = Hs::ForeignDecl(yystack_[3].value.as < std::string > (), yystack_[2].value.as < std::string > (), yystack_[0].value.as < Hs::Type > ());}
#line 2613 "parser.cc"
    break;

  case 65: // topdecl: decl_no_th
#line 642 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2619 "parser.cc"
    break;

  case 66: // topdecl: infixexp_top
#line 644 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < Hs::InfixExp > ();}
#line 2625 "parser.cc"
    break;

  case 67: // cl_decl: "class" tycl_hdr where_cls
#line 646 "parser.y"
                                              {yylhs.value.as < expression_ref > () = make_class_decl(yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().first,yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().second,yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 2631 "parser.cc"
    break;

  case 68: // ty_decl: "type" type "=" ktype
#line 648 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_type_synonym({yystack_[2].location, yystack_[2].value.as < Hs::Type > ()},{yystack_[0].location, yystack_[0].value.as < Hs::Type > ()});}
#line 2637 "parser.cc"
    break;

  case 69: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 649 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::Type> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::Type> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > ());}
#line 2643 "parser.cc"
    break;

  case 70: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 651 "parser.y"
                                                                                          {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::Type> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::Type> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < std::optional<Hs::GADTConstructorsDecl> > ());}
#line 2649 "parser.cc"
    break;

  case 71: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 652 "parser.y"
                                                                                          {yylhs.value.as < expression_ref > () = make_type_family({yystack_[3].location,yystack_[3].value.as < Hs::Type > ()}, yystack_[2].value.as < std::optional<Located<Hs::Kind>> > ());}
#line 2655 "parser.cc"
    break;

  case 75: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 661 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_instance_decl({yystack_[1].location,yystack_[1].value.as < Hs::Type > ()},yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 2661 "parser.cc"
    break;

  case 76: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 662 "parser.y"
                                                                           {}
#line 2667 "parser.cc"
    break;

  case 102: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 713 "parser.y"
                                                               {}
#line 2673 "parser.cc"
    break;

  case 103: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 715 "parser.y"
                                                               { yylhs.value.as < expression_ref > () = make_type_family({yystack_[1].location,yystack_[1].value.as < Hs::Type > ()}, yystack_[0].value.as < std::optional<Located<Hs::Kind>> > ()); }
#line 2679 "parser.cc"
    break;

  case 104: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 717 "parser.y"
                                                                    { yylhs.value.as < expression_ref > () = make_type_family({yystack_[1].location,yystack_[1].value.as < Hs::Type > ()}, yystack_[0].value.as < std::optional<Located<Hs::Kind>> > ()); }
#line 2685 "parser.cc"
    break;

  case 105: // at_decl_cls: "type" ty_fam_inst_eqn
#line 719 "parser.y"
                                                               {}
#line 2691 "parser.cc"
    break;

  case 106: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 720 "parser.y"
                                                               {}
#line 2697 "parser.cc"
    break;

  case 112: // data_or_newtype: "data"
#line 730 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 2703 "parser.cc"
    break;

  case 113: // data_or_newtype: "newtype"
#line 731 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 2709 "parser.cc"
    break;

  case 114: // opt_kind_sig: %empty
#line 735 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 2715 "parser.cc"
    break;

  case 115: // opt_kind_sig: "::" kind
#line 736 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < expression_ref > ();}
#line 2721 "parser.cc"
    break;

  case 118: // opt_tyfam_kind_sig: %empty
#line 741 "parser.y"
                                      {}
#line 2727 "parser.cc"
    break;

  case 119: // opt_tyfam_kind_sig: "::" kind
#line 742 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < expression_ref > ()}};}
#line 2733 "parser.cc"
    break;

  case 120: // opt_tyfam_kind_sig: "=" tv_bndr
#line 743 "parser.y"
                                      {}
#line 2739 "parser.cc"
    break;

  case 121: // opt_at_kind_inj_sig: %empty
#line 745 "parser.y"
                                      {}
#line 2745 "parser.cc"
    break;

  case 122: // opt_at_kind_inj_sig: "::" kind
#line 746 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < expression_ref > ()}};}
#line 2751 "parser.cc"
    break;

  case 123: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 747 "parser.y"
                                                                  {}
#line 2757 "parser.cc"
    break;

  case 124: // tycl_hdr: context "=>" type
#line 751 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::Type> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::Type > ()};}
#line 2763 "parser.cc"
    break;

  case 125: // tycl_hdr: type
#line 752 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::Type> > () = {{},yystack_[0].value.as < Hs::Type > ()};}
#line 2769 "parser.cc"
    break;

  case 129: // decl_cls: at_decl_cls
#line 798 "parser.y"
                        {}
#line 2775 "parser.cc"
    break;

  case 130: // decl_cls: decl
#line 799 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2781 "parser.cc"
    break;

  case 131: // decls_cls: decls_cls ";" decl_cls
#line 801 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2787 "parser.cc"
    break;

  case 132: // decls_cls: decls_cls ";"
#line 802 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 2793 "parser.cc"
    break;

  case 133: // decls_cls: decl_cls
#line 803 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2799 "parser.cc"
    break;

  case 134: // decls_cls: %empty
#line 804 "parser.y"
                                           {}
#line 2805 "parser.cc"
    break;

  case 135: // decllist_cls: "{" decls_cls "}"
#line 806 "parser.y"
                                           {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[1].location,{yystack_[1].value.as < Hs::Decls > ()}};}
#line 2811 "parser.cc"
    break;

  case 136: // decllist_cls: "vocurly" decls_cls close
#line 807 "parser.y"
                                           {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[1].location,{yystack_[1].value.as < Hs::Decls > ()}};}
#line 2817 "parser.cc"
    break;

  case 137: // where_cls: "where" decllist_cls
#line 809 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 2823 "parser.cc"
    break;

  case 138: // where_cls: %empty
#line 810 "parser.y"
                                           {}
#line 2829 "parser.cc"
    break;

  case 139: // decl_inst: at_decl_inst
#line 812 "parser.y"
                                           {}
#line 2835 "parser.cc"
    break;

  case 140: // decl_inst: decl
#line 813 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2841 "parser.cc"
    break;

  case 141: // decls_inst: decls_inst ";" decl_inst
#line 815 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2847 "parser.cc"
    break;

  case 142: // decls_inst: decls_inst ";"
#line 816 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 2853 "parser.cc"
    break;

  case 143: // decls_inst: decl_inst
#line 817 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2859 "parser.cc"
    break;

  case 144: // decls_inst: %empty
#line 818 "parser.y"
                                           {}
#line 2865 "parser.cc"
    break;

  case 145: // decllist_inst: "{" decls_inst "}"
#line 820 "parser.y"
                                           {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[1].location,{yystack_[1].value.as < Hs::Decls > ()}};}
#line 2871 "parser.cc"
    break;

  case 146: // decllist_inst: "vocurly" decls_inst close
#line 821 "parser.y"
                                           {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[1].location,{yystack_[1].value.as < Hs::Decls > ()}};}
#line 2877 "parser.cc"
    break;

  case 147: // where_inst: "where" decllist_inst
#line 823 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 2883 "parser.cc"
    break;

  case 148: // where_inst: %empty
#line 824 "parser.y"
                                           {}
#line 2889 "parser.cc"
    break;

  case 149: // decls: decls ";" decl
#line 827 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2895 "parser.cc"
    break;

  case 150: // decls: decls ";"
#line 828 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2901 "parser.cc"
    break;

  case 151: // decls: decl
#line 829 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2907 "parser.cc"
    break;

  case 152: // decls: %empty
#line 830 "parser.y"
                        {}
#line 2913 "parser.cc"
    break;

  case 153: // decllist: "{" decls "}"
#line 832 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2919 "parser.cc"
    break;

  case 154: // decllist: "vocurly" decls close
#line 833 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2925 "parser.cc"
    break;

  case 155: // binds: decllist
#line 835 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 2931 "parser.cc"
    break;

  case 156: // wherebinds: "where" binds
#line 837 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 2937 "parser.cc"
    break;

  case 157: // wherebinds: %empty
#line 838 "parser.y"
                                 {}
#line 2943 "parser.cc"
    break;

  case 163: // opt_tyconsig: %empty
#line 864 "parser.y"
                                 {}
#line 2949 "parser.cc"
    break;

  case 164: // opt_tyconsig: "::" gtycon
#line 865 "parser.y"
                                 {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2955 "parser.cc"
    break;

  case 165: // sigktype: sigtype
#line 867 "parser.y"
                                 {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 2961 "parser.cc"
    break;

  case 166: // sigktype: ctype "::" kind
#line 868 "parser.y"
                                 {yylhs.value.as < Hs::Type > () = Hs::TypeOfKind(yystack_[2].value.as < Hs::Type > (), yystack_[0].value.as < expression_ref > ());}
#line 2967 "parser.cc"
    break;

  case 167: // sigtype: ctype
#line 872 "parser.y"
         { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2973 "parser.cc"
    break;

  case 168: // sigtypedoc: ctypedoc
#line 874 "parser.y"
            { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2979 "parser.cc"
    break;

  case 169: // sig_vars: sig_vars "," var
#line 876 "parser.y"
                           {yylhs.value.as < std::vector<Hs::Var> > () = yystack_[2].value.as < std::vector<Hs::Var> > (); yylhs.value.as < std::vector<Hs::Var> > ().push_back(Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}));}
#line 2985 "parser.cc"
    break;

  case 170: // sig_vars: var
#line 877 "parser.y"
                           {yylhs.value.as < std::vector<Hs::Var> > ().push_back(Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}));}
#line 2991 "parser.cc"
    break;

  case 171: // sigtypes1: sigtype
#line 879 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2997 "parser.cc"
    break;

  case 172: // sigtypes1: sigtypes1 "," sigtype
#line 880 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[2].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3003 "parser.cc"
    break;

  case 173: // strict_mark: strictness
#line 884 "parser.y"
             { yylhs.value.as < Hs::StrictLazy > () = yystack_[0].value.as < Hs::StrictLazy > (); }
#line 3009 "parser.cc"
    break;

  case 174: // strictness: PREFIX_BANG
#line 890 "parser.y"
                         {yylhs.value.as < Hs::StrictLazy > () = Hs::StrictLazy::strict;}
#line 3015 "parser.cc"
    break;

  case 175: // strictness: PREFIX_TILDE
#line 891 "parser.y"
                         {yylhs.value.as < Hs::StrictLazy > () = Hs::StrictLazy::lazy;}
#line 3021 "parser.cc"
    break;

  case 176: // ktype: ctype
#line 898 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 3027 "parser.cc"
    break;

  case 177: // ktype: ctype "::" kind
#line 899 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeOfKind(yystack_[2].value.as < Hs::Type > (), yystack_[0].value.as < expression_ref > ());}
#line 3033 "parser.cc"
    break;

  case 178: // ctype: "forall" tv_bndrs "." ctype
#line 901 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::ForallType(yystack_[2].value.as < std::vector<Hs::TypeVar> > (), yystack_[0].value.as < Hs::Type > ());}
#line 3039 "parser.cc"
    break;

  case 179: // ctype: context "=>" ctype
#line 902 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::Type > ());}
#line 3045 "parser.cc"
    break;

  case 180: // ctype: type
#line 904 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3051 "parser.cc"
    break;

  case 181: // ctypedoc: ctype
#line 906 "parser.y"
          { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3057 "parser.cc"
    break;

  case 182: // context: btype
#line 915 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::Type > ());}
#line 3063 "parser.cc"
    break;

  case 183: // context_no_ops: btype_no_ops
#line 917 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ()));}
#line 3069 "parser.cc"
    break;

  case 184: // type: btype
#line 919 "parser.y"
      { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3075 "parser.cc"
    break;

  case 185: // type: btype "->" ctype
#line 920 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,"->"}),yystack_[2].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()});}
#line 3081 "parser.cc"
    break;

  case 186: // typedoc: type
#line 922 "parser.y"
         { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3087 "parser.cc"
    break;

  case 187: // btype: infixtype
#line 925 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3093 "parser.cc"
    break;

  case 188: // infixtype: ftype
#line 927 "parser.y"
           { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3099 "parser.cc"
    break;

  case 189: // infixtype: btype "~" btype
#line 928 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,"~"}),yystack_[2].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()});}
#line 3105 "parser.cc"
    break;

  case 190: // btype_no_ops: atype_docs
#line 930 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3111 "parser.cc"
    break;

  case 191: // btype_no_ops: btype_no_ops atype_docs
#line 931 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[1].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3117 "parser.cc"
    break;

  case 192: // ftype: atype
#line 933 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3123 "parser.cc"
    break;

  case 193: // ftype: tyop
#line 934 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3129 "parser.cc"
    break;

  case 194: // ftype: ftype tyarg
#line 935 "parser.y"
                                   { yylhs.value.as < Hs::Type > () = Hs::TypeApp(yystack_[1].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()); }
#line 3135 "parser.cc"
    break;

  case 195: // tyarg: atype
#line 937 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3141 "parser.cc"
    break;

  case 196: // tyop: qtyconop
#line 939 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3147 "parser.cc"
    break;

  case 197: // tyop: tyvarop
#line 940 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3153 "parser.cc"
    break;

  case 198: // atype_docs: atype
#line 947 "parser.y"
            { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3159 "parser.cc"
    break;

  case 199: // atype: ntgtycon
#line 954 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3165 "parser.cc"
    break;

  case 200: // atype: tyvar
#line 955 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3171 "parser.cc"
    break;

  case 201: // atype: "*"
#line 956 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,"*"});}
#line 3177 "parser.cc"
    break;

  case 202: // atype: strict_mark atype
#line 957 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::StrictLazyType(yystack_[1].value.as < Hs::StrictLazy > (),yystack_[0].value.as < Hs::Type > ());}
#line 3183 "parser.cc"
    break;

  case 203: // atype: "{" fielddecls "}"
#line 958 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ());}
#line 3189 "parser.cc"
    break;

  case 204: // atype: "(" ")"
#line 959 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[1].location,"()"});}
#line 3195 "parser.cc"
    break;

  case 205: // atype: "(" comma_types1 "," ktype ")"
#line 960 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::Type> > ();ts.push_back(yystack_[1].value.as < Hs::Type > ());yylhs.value.as < Hs::Type > () = Hs::TupleType(ts);}
#line 3201 "parser.cc"
    break;

  case 206: // atype: "[" ktype "]"
#line 966 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::ListType{yystack_[1].value.as < Hs::Type > ()}; }
#line 3207 "parser.cc"
    break;

  case 207: // atype: "(" ktype ")"
#line 967 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = yystack_[1].value.as < Hs::Type > ();}
#line 3213 "parser.cc"
    break;

  case 208: // inst_type: sigtype
#line 970 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 3219 "parser.cc"
    break;

  case 211: // comma_types0: comma_types1
#line 975 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[0].value.as < std::vector<Hs::Type> > ();}
#line 3225 "parser.cc"
    break;

  case 212: // comma_types0: %empty
#line 976 "parser.y"
                                       { /* default construction OK */ }
#line 3231 "parser.cc"
    break;

  case 213: // comma_types1: ktype
#line 978 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3237 "parser.cc"
    break;

  case 214: // comma_types1: comma_types1 "," ktype
#line 979 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[2].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3243 "parser.cc"
    break;

  case 215: // tv_bndrs: tv_bndrs tv_bndr
#line 986 "parser.y"
                               {yylhs.value.as < std::vector<Hs::TypeVar> > () = yystack_[1].value.as < std::vector<Hs::TypeVar> > (); yylhs.value.as < std::vector<Hs::TypeVar> > ().push_back(yystack_[0].value.as < Hs::TypeVar > ());}
#line 3249 "parser.cc"
    break;

  case 216: // tv_bndrs: %empty
#line 987 "parser.y"
                               { /* default construction OK */}
#line 3255 "parser.cc"
    break;

  case 217: // tv_bndr: tv_bndr_no_braces
#line 989 "parser.y"
                                 {yylhs.value.as < Hs::TypeVar > () = yystack_[0].value.as < Hs::TypeVar > ();}
#line 3261 "parser.cc"
    break;

  case 218: // tv_bndr: "{" tyvar "}"
#line 990 "parser.y"
                                 {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[1].location,yystack_[1].value.as < std::string > ()});}
#line 3267 "parser.cc"
    break;

  case 219: // tv_bndr: "{" tyvar "::" kind "}"
#line 991 "parser.y"
                                 {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[3].location,yystack_[3].value.as < std::string > ()});}
#line 3273 "parser.cc"
    break;

  case 220: // tv_bndr_no_braces: tyvar
#line 994 "parser.y"
                                              {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3279 "parser.cc"
    break;

  case 221: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 995 "parser.y"
                                              {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[3].location,yystack_[3].value.as < std::string > ()},yystack_[1].value.as < expression_ref > ());}
#line 3285 "parser.cc"
    break;

  case 222: // kind: ctype
#line 1013 "parser.y"
             {yylhs.value.as < expression_ref > () = type_to_kind(yystack_[0].value.as < Hs::Type > ());}
#line 3291 "parser.cc"
    break;

  case 223: // gadt_constrlist: "where" "{" gadt_constrs "}"
#line 1019 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3297 "parser.cc"
    break;

  case 224: // gadt_constrlist: "where" "vocurly" gadt_constrs close
#line 1020 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3303 "parser.cc"
    break;

  case 225: // gadt_constrlist: %empty
#line 1021 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = {};}
#line 3309 "parser.cc"
    break;

  case 226: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1023 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3315 "parser.cc"
    break;

  case 227: // gadt_constrs: gadt_constr
#line 1024 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3321 "parser.cc"
    break;

  case 228: // gadt_constr: optSemi con_list "::" sigtype
#line 1026 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),{{},yystack_[0].value.as < Hs::Type > ()});}
#line 3327 "parser.cc"
    break;

  case 229: // constrs: "=" constrs1
#line 1028 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 3333 "parser.cc"
    break;

  case 230: // constrs1: constrs1 "|" constr
#line 1030 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3339 "parser.cc"
    break;

  case 231: // constrs1: constr
#line 1031 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3345 "parser.cc"
    break;

  case 232: // constr: forall context_no_ops "=>" constr_stuff
#line 1033 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::TypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::Type > ());}
#line 3351 "parser.cc"
    break;

  case 233: // constr: forall constr_stuff
#line 1034 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::TypeVar> > (),{}, yystack_[0].value.as < Hs::Type > ());}
#line 3357 "parser.cc"
    break;

  case 234: // forall: "forall" tv_bndrs "."
#line 1036 "parser.y"
                                {yylhs.value.as < std::vector<Hs::TypeVar> > () = yystack_[1].value.as < std::vector<Hs::TypeVar> > ();}
#line 3363 "parser.cc"
    break;

  case 235: // forall: %empty
#line 1037 "parser.y"
                                {}
#line 3369 "parser.cc"
    break;

  case 236: // constr_stuff: btype_no_ops
#line 1039 "parser.y"
                                                {yylhs.value.as < Hs::Type > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ());}
#line 3375 "parser.cc"
    break;

  case 237: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1040 "parser.y"
                                                {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,yystack_[1].value.as < std::string > ()}),Hs::make_tyapps(yystack_[2].value.as < std::vector<Hs::Type> > ()),Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ())});}
#line 3381 "parser.cc"
    break;

  case 238: // fielddecls: %empty
#line 1042 "parser.y"
                                {}
#line 3387 "parser.cc"
    break;

  case 239: // fielddecls: fielddecls1
#line 1043 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 3393 "parser.cc"
    break;

  case 240: // fielddecls1: fielddecls1 "," fielddecl
#line 1045 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3399 "parser.cc"
    break;

  case 241: // fielddecls1: fielddecl
#line 1046 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3405 "parser.cc"
    break;

  case 242: // fielddecl: sig_vars "::" ctype
#line 1048 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::Var> > (),yystack_[0].value.as < Hs::Type > ());}
#line 3411 "parser.cc"
    break;

  case 253: // decl_no_th: sigdecl
#line 1067 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3417 "parser.cc"
    break;

  case 254: // decl_no_th: PREFIX_BANG aexp rhs
#line 1069 "parser.y"
                                      {yylhs.value.as < expression_ref > () = Hs::StrictValueDecl{{yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < Hs::MultiGuardedRHS > ()}; }
#line 3423 "parser.cc"
    break;

  case 255: // decl_no_th: infixexp_top rhs
#line 1070 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::ValueDecl({yystack_[1].location,yystack_[1].value.as < Hs::InfixExp > ()},yystack_[0].value.as < Hs::MultiGuardedRHS > ());}
#line 3429 "parser.cc"
    break;

  case 256: // decl: decl_no_th
#line 1072 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3435 "parser.cc"
    break;

  case 257: // rhs: "=" exp wherebinds
#line 1076 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3441 "parser.cc"
    break;

  case 258: // rhs: gdrhs wherebinds
#line 1077 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 3447 "parser.cc"
    break;

  case 259: // gdrhs: gdrhs gdrh
#line 1079 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3453 "parser.cc"
    break;

  case 260: // gdrhs: gdrh
#line 1080 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3459 "parser.cc"
    break;

  case 261: // gdrh: "|" guardquals "=" exp
#line 1084 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ()};}
#line 3465 "parser.cc"
    break;

  case 262: // sigdecl: sig_vars "::" sigtypedoc
#line 1094 "parser.y"
                                  { yylhs.value.as < expression_ref > () = Hs::SignatureDecl{yystack_[2].value.as < std::vector<Hs::Var> > (),yystack_[0].value.as < Hs::Type > ()}; }
#line 3471 "parser.cc"
    break;

  case 263: // sigdecl: infix prec ops
#line 1095 "parser.y"
                         { yylhs.value.as < expression_ref > () = Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()}; }
#line 3477 "parser.cc"
    break;

  case 264: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1097 "parser.y"
                                                    {}
#line 3483 "parser.cc"
    break;

  case 265: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1098 "parser.y"
                                            {}
#line 3489 "parser.cc"
    break;

  case 266: // sigdecl: "{-# SCC" qvar "#-}"
#line 1099 "parser.y"
                              {}
#line 3495 "parser.cc"
    break;

  case 267: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1100 "parser.y"
                                     {}
#line 3501 "parser.cc"
    break;

  case 268: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1101 "parser.y"
                                                               {}
#line 3507 "parser.cc"
    break;

  case 269: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1102 "parser.y"
                                                                      {}
#line 3513 "parser.cc"
    break;

  case 270: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1103 "parser.y"
                                                     {}
#line 3519 "parser.cc"
    break;

  case 275: // exp: infixexp "::" sigtype
#line 1115 "parser.y"
                           { yylhs.value.as < expression_ref > () = Hs::TypedExp(yystack_[2].value.as < Hs::InfixExp > (),yystack_[0].value.as < Hs::Type > ()); }
#line 3525 "parser.cc"
    break;

  case 276: // exp: infixexp
#line 1116 "parser.y"
                           { yylhs.value.as < expression_ref > () = yystack_[0].value.as < Hs::InfixExp > (); }
#line 3531 "parser.cc"
    break;

  case 277: // infixexp: exp10
#line 1120 "parser.y"
                                {yylhs.value.as < Hs::InfixExp > () = Hs::InfixExp({yystack_[0].value.as < expression_ref > ()});}
#line 3537 "parser.cc"
    break;

  case 278: // infixexp: infixexp qop exp10
#line 1121 "parser.y"
                                {yylhs.value.as < Hs::InfixExp > () = yystack_[2].value.as < Hs::InfixExp > (); yylhs.value.as < Hs::InfixExp > ().terms.push_back(yystack_[1].value.as < expression_ref > ()); yylhs.value.as < Hs::InfixExp > ().terms.push_back(yystack_[0].value.as < expression_ref > ());}
#line 3543 "parser.cc"
    break;

  case 279: // infixexp_top: exp10_top
#line 1123 "parser.y"
                                {yylhs.value.as < Hs::InfixExp > () = Hs::InfixExp({yystack_[0].value.as < expression_ref > ()});}
#line 3549 "parser.cc"
    break;

  case 280: // infixexp_top: infixexp_top qop exp10_top
#line 1124 "parser.y"
                                          {yylhs.value.as < Hs::InfixExp > () = yystack_[2].value.as < Hs::InfixExp > (); yylhs.value.as < Hs::InfixExp > ().terms.push_back(yystack_[1].value.as < expression_ref > ()); yylhs.value.as < Hs::InfixExp > ().terms.push_back(yystack_[0].value.as < expression_ref > ());}
#line 3555 "parser.cc"
    break;

  case 281: // exp10_top: "-" fexp
#line 1126 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_minus(yystack_[0].value.as < expression_ref > ());}
#line 3561 "parser.cc"
    break;

  case 282: // exp10_top: "{-# CORE" "STRING" "#-}"
#line 1127 "parser.y"
                                   {}
#line 3567 "parser.cc"
    break;

  case 283: // exp10_top: fexp
#line 1128 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3573 "parser.cc"
    break;

  case 284: // exp10: exp10_top
#line 1131 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3579 "parser.cc"
    break;

  case 285: // exp10: scc_annot exp
#line 1132 "parser.y"
                                 {}
#line 3585 "parser.cc"
    break;

  case 290: // fexp: fexp aexp
#line 1144 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_apply(yystack_[1].value.as < expression_ref > (), yystack_[0].value.as < expression_ref > ());}
#line 3591 "parser.cc"
    break;

  case 291: // fexp: fexp "TYPEAPP" atype
#line 1145 "parser.y"
                                 {}
#line 3597 "parser.cc"
    break;

  case 292: // fexp: "static" aexp
#line 1146 "parser.y"
                                 {}
#line 3603 "parser.cc"
    break;

  case 293: // fexp: aexp
#line 1147 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3609 "parser.cc"
    break;

  case 294: // aexp: qvar "@" aexp
#line 1150 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::AsPattern(Hs::Var({yystack_[2].location,yystack_[2].value.as < std::string > ()}),yystack_[0].value.as < expression_ref > ());}
#line 3615 "parser.cc"
    break;

  case 295: // aexp: PREFIX_TILDE aexp
#line 1151 "parser.y"
                                          {yylhs.value.as < expression_ref > () = Hs::LazyPattern(yystack_[0].value.as < expression_ref > ());}
#line 3621 "parser.cc"
    break;

  case 296: // aexp: "\\" apats1 "->" exp
#line 1152 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::LambdaExp(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3627 "parser.cc"
    break;

  case 297: // aexp: "let" binds "in" exp
#line 1153 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 3633 "parser.cc"
    break;

  case 298: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1155 "parser.y"
                                                       {yylhs.value.as < expression_ref > () = Hs::IfExp({yystack_[6].location,yystack_[6].value.as < expression_ref > ()},{yystack_[3].location,yystack_[3].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 3639 "parser.cc"
    break;

  case 299: // aexp: "case" exp "of" altslist
#line 1157 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::CaseExp(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < Hs::Alts > ());}
#line 3645 "parser.cc"
    break;

  case 300: // aexp: "do" stmtlist
#line 1158 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::Do(yystack_[0].value.as < Hs::Stmts > ());}
#line 3651 "parser.cc"
    break;

  case 301: // aexp: "mdo" stmtlist
#line 1159 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::MDo(yystack_[0].value.as < Hs::Stmts > ());}
#line 3657 "parser.cc"
    break;

  case 302: // aexp: aexp1
#line 1161 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3663 "parser.cc"
    break;

  case 303: // aexp1: aexp1 "{" fbinds "}"
#line 1164 "parser.y"
                              {}
#line 3669 "parser.cc"
    break;

  case 304: // aexp1: aexp2
#line 1165 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3675 "parser.cc"
    break;

  case 305: // aexp2: qvar
#line 1168 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3681 "parser.cc"
    break;

  case 306: // aexp2: qcon
#line 1169 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3687 "parser.cc"
    break;

  case 307: // aexp2: literal
#line 1170 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3693 "parser.cc"
    break;

  case 308: // aexp2: "(" texp ")"
#line 1171 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3699 "parser.cc"
    break;

  case 309: // aexp2: "(" tup_exprs ")"
#line 1172 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::Tuple(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3705 "parser.cc"
    break;

  case 310: // aexp2: "[" list "]"
#line 1177 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3711 "parser.cc"
    break;

  case 311: // aexp2: "_"
#line 1178 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::WildcardPattern();}
#line 3717 "parser.cc"
    break;

  case 312: // texp: exp
#line 1184 "parser.y"
                      {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3723 "parser.cc"
    break;

  case 313: // texp: infixexp qop
#line 1185 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::LeftSection ( yystack_[1].value.as < Hs::InfixExp > (), yystack_[0].value.as < expression_ref > () ); }
#line 3729 "parser.cc"
    break;

  case 314: // texp: qopm infixexp
#line 1186 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::RightSection( yystack_[1].value.as < expression_ref > (), yystack_[0].value.as < Hs::InfixExp > () ); }
#line 3735 "parser.cc"
    break;

  case 315: // tup_exprs: tup_exprs "," texp
#line 1191 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3741 "parser.cc"
    break;

  case 316: // tup_exprs: texp "," texp
#line 1192 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3747 "parser.cc"
    break;

  case 317: // list: texp
#line 1210 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List{{yystack_[0].value.as < expression_ref > ()}}; }
#line 3753 "parser.cc"
    break;

  case 318: // list: lexps
#line 1211 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List{yystack_[0].value.as < std::vector<expression_ref> > ()}; }
#line 3759 "parser.cc"
    break;

  case 319: // list: texp ".."
#line 1212 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < expression_ref > ()); }
#line 3765 "parser.cc"
    break;

  case 320: // list: texp "," exp ".."
#line 1213 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()); }
#line 3771 "parser.cc"
    break;

  case 321: // list: texp ".." exp
#line 1214 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()); }
#line 3777 "parser.cc"
    break;

  case 322: // list: texp "," exp ".." exp
#line 1215 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < expression_ref > (), yystack_[2].value.as < expression_ref > (), yystack_[0].value.as < expression_ref > ()); }
#line 3783 "parser.cc"
    break;

  case 323: // list: texp "|" squals
#line 1216 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < expression_ref > (), yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 3789 "parser.cc"
    break;

  case 324: // lexps: lexps "," texp
#line 1218 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3795 "parser.cc"
    break;

  case 325: // lexps: texp "," texp
#line 1219 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3801 "parser.cc"
    break;

  case 326: // squals: squals "," qual
#line 1232 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3807 "parser.cc"
    break;

  case 327: // squals: qual
#line 1234 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3813 "parser.cc"
    break;

  case 328: // guardquals: guardquals1
#line 1244 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3819 "parser.cc"
    break;

  case 329: // guardquals1: guardquals1 "," qual
#line 1246 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > ();yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3825 "parser.cc"
    break;

  case 330: // guardquals1: qual
#line 1247 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3831 "parser.cc"
    break;

  case 331: // altslist: "{" alts "}"
#line 1250 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 3837 "parser.cc"
    break;

  case 332: // altslist: "vocurly" alts close
#line 1251 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 3843 "parser.cc"
    break;

  case 333: // altslist: "{" "}"
#line 1252 "parser.y"
                                 {}
#line 3849 "parser.cc"
    break;

  case 334: // altslist: "vocurly" close
#line 1253 "parser.y"
                                 {}
#line 3855 "parser.cc"
    break;

  case 335: // alts: alts1
#line 1255 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 3861 "parser.cc"
    break;

  case 336: // alts: ";" alts
#line 1256 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 3867 "parser.cc"
    break;

  case 337: // alts1: alts1 ";" alt
#line 1258 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 3873 "parser.cc"
    break;

  case 338: // alts1: alts1 ";"
#line 1259 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 3879 "parser.cc"
    break;

  case 339: // alts1: alt
#line 1260 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 3885 "parser.cc"
    break;

  case 340: // alt: pat alt_rhs
#line 1262 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 3891 "parser.cc"
    break;

  case 341: // alt_rhs: "->" exp wherebinds
#line 1264 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3897 "parser.cc"
    break;

  case 342: // alt_rhs: gdpats wherebinds
#line 1265 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3903 "parser.cc"
    break;

  case 343: // gdpats: gdpats gdpat
#line 1267 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3909 "parser.cc"
    break;

  case 344: // gdpats: gdpat
#line 1268 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3915 "parser.cc"
    break;

  case 345: // gdpat: "|" guardquals "->" exp
#line 1277 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ()};}
#line 3921 "parser.cc"
    break;

  case 346: // pat: exp
#line 1279 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3927 "parser.cc"
    break;

  case 347: // pat: PREFIX_BANG aexp
#line 1280 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3933 "parser.cc"
    break;

  case 348: // bindpat: exp
#line 1282 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3939 "parser.cc"
    break;

  case 349: // bindpat: PREFIX_BANG aexp
#line 1283 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3945 "parser.cc"
    break;

  case 350: // apat: aexp
#line 1285 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3951 "parser.cc"
    break;

  case 351: // apat: PREFIX_BANG aexp
#line 1286 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3957 "parser.cc"
    break;

  case 352: // apats1: apats1 apat
#line 1288 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3963 "parser.cc"
    break;

  case 353: // apats1: apat
#line 1289 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3969 "parser.cc"
    break;

  case 354: // stmtlist: "{" stmts "}"
#line 1292 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 3975 "parser.cc"
    break;

  case 355: // stmtlist: "vocurly" stmts close
#line 1293 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 3981 "parser.cc"
    break;

  case 356: // stmts: stmts ";" stmt
#line 1295 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3987 "parser.cc"
    break;

  case 357: // stmts: stmts ";"
#line 1296 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3993 "parser.cc"
    break;

  case 358: // stmts: stmt
#line 1297 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3999 "parser.cc"
    break;

  case 359: // stmts: %empty
#line 1298 "parser.y"
                       {}
#line 4005 "parser.cc"
    break;

  case 360: // stmt: qual
#line 1303 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 4011 "parser.cc"
    break;

  case 361: // stmt: "rec" stmtlist
#line 1304 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ());}
#line 4017 "parser.cc"
    break;

  case 362: // qual: bindpat "<-" exp
#line 1306 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::PatQual(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 4023 "parser.cc"
    break;

  case 363: // qual: exp
#line 1307 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::SimpleQual(yystack_[0].value.as < expression_ref > ());}
#line 4029 "parser.cc"
    break;

  case 364: // qual: "let" binds
#line 1308 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ());}
#line 4035 "parser.cc"
    break;

  case 372: // qcon: gen_qcon
#line 1353 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4041 "parser.cc"
    break;

  case 373: // qcon: sysdcon
#line 1354 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4047 "parser.cc"
    break;

  case 374: // gen_qcon: qconid
#line 1356 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4053 "parser.cc"
    break;

  case 375: // gen_qcon: "(" qconsym ")"
#line 1357 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4059 "parser.cc"
    break;

  case 376: // con: conid
#line 1359 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4065 "parser.cc"
    break;

  case 377: // con: "(" consym ")"
#line 1360 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4071 "parser.cc"
    break;

  case 378: // con: sysdcon
#line 1361 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4077 "parser.cc"
    break;

  case 379: // con_list: con_list "," con
#line 1363 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4083 "parser.cc"
    break;

  case 380: // con_list: con
#line 1364 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4089 "parser.cc"
    break;

  case 381: // sysdcon_no_list: "(" ")"
#line 1366 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 4095 "parser.cc"
    break;

  case 382: // sysdcon_no_list: "(" commas ")"
#line 1367 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4101 "parser.cc"
    break;

  case 383: // sysdcon_no_list: "(#" "#)"
#line 1368 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 4107 "parser.cc"
    break;

  case 384: // sysdcon_no_list: "(#" commas "#)"
#line 1369 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4113 "parser.cc"
    break;

  case 385: // sysdcon: sysdcon_no_list
#line 1371 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4119 "parser.cc"
    break;

  case 386: // sysdcon: "[" "]"
#line 1372 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 4125 "parser.cc"
    break;

  case 387: // conop: consym
#line 1374 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4131 "parser.cc"
    break;

  case 388: // conop: "`" conid "`"
#line 1375 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4137 "parser.cc"
    break;

  case 389: // qconop: qconsym
#line 1377 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4143 "parser.cc"
    break;

  case 390: // qconop: "`" qconid "`"
#line 1378 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4149 "parser.cc"
    break;

  case 391: // gtycon: ntgtycon
#line 1381 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4155 "parser.cc"
    break;

  case 392: // gtycon: "(" ")"
#line 1382 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 4161 "parser.cc"
    break;

  case 393: // gtycon: "(#" "#)"
#line 1383 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 4167 "parser.cc"
    break;

  case 394: // ntgtycon: oqtycon
#line 1385 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4173 "parser.cc"
    break;

  case 395: // ntgtycon: "(" commas ")"
#line 1386 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4179 "parser.cc"
    break;

  case 396: // ntgtycon: "(#" commas "#)"
#line 1387 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4185 "parser.cc"
    break;

  case 397: // ntgtycon: "(" "->" ")"
#line 1388 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 4191 "parser.cc"
    break;

  case 398: // ntgtycon: "[" "]"
#line 1389 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 4197 "parser.cc"
    break;

  case 399: // oqtycon: qtycon
#line 1391 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4203 "parser.cc"
    break;

  case 400: // oqtycon: "(" qtyconsym ")"
#line 1392 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4209 "parser.cc"
    break;

  case 401: // oqtycon: "(" "~" ")"
#line 1393 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 4215 "parser.cc"
    break;

  case 402: // oqtycon_no_varcon: qtycon
#line 1395 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4221 "parser.cc"
    break;

  case 403: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1396 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4227 "parser.cc"
    break;

  case 404: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1397 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4233 "parser.cc"
    break;

  case 405: // oqtycon_no_varcon: "(" ":" ")"
#line 1398 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 4239 "parser.cc"
    break;

  case 406: // oqtycon_no_varcon: "(" "~" ")"
#line 1399 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 4245 "parser.cc"
    break;

  case 407: // qtyconop: qtyconsym
#line 1402 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4251 "parser.cc"
    break;

  case 408: // qtyconop: "`" qtycon "`"
#line 1403 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4257 "parser.cc"
    break;

  case 409: // qtycondoc: qtycon
#line 1405 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4263 "parser.cc"
    break;

  case 410: // qtycon: "QCONID"
#line 1407 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4269 "parser.cc"
    break;

  case 411: // qtycon: tycon
#line 1408 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4275 "parser.cc"
    break;

  case 412: // tycon: "CONID"
#line 1412 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4281 "parser.cc"
    break;

  case 413: // qtyconsym: "QCONSYM"
#line 1414 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4287 "parser.cc"
    break;

  case 414: // qtyconsym: "QVARSYM"
#line 1415 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4293 "parser.cc"
    break;

  case 415: // qtyconsym: tyconsym
#line 1416 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4299 "parser.cc"
    break;

  case 416: // tyconsym: "CONSYM"
#line 1418 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4305 "parser.cc"
    break;

  case 417: // tyconsym: "VARSYM"
#line 1419 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4311 "parser.cc"
    break;

  case 418: // tyconsym: ":"
#line 1420 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4317 "parser.cc"
    break;

  case 419: // tyconsym: "-"
#line 1421 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 4323 "parser.cc"
    break;

  case 420: // op: varop
#line 1426 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4329 "parser.cc"
    break;

  case 421: // op: conop
#line 1427 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4335 "parser.cc"
    break;

  case 422: // varop: varsym
#line 1429 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4341 "parser.cc"
    break;

  case 423: // varop: "`" varid "`"
#line 1430 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4347 "parser.cc"
    break;

  case 424: // qop: qvarop
#line 1432 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 4353 "parser.cc"
    break;

  case 425: // qop: qconop
#line 1433 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 4359 "parser.cc"
    break;

  case 426: // qopm: qvaropm
#line 1436 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 4365 "parser.cc"
    break;

  case 427: // qopm: qconop
#line 1437 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 4371 "parser.cc"
    break;

  case 428: // qvarop: qvarsym
#line 1442 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4377 "parser.cc"
    break;

  case 429: // qvarop: "`" qvarid "`"
#line 1443 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4383 "parser.cc"
    break;

  case 430: // qvaropm: qvarsym_no_minus
#line 1445 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 4389 "parser.cc"
    break;

  case 431: // qvaropm: "`" qvarid "`"
#line 1446 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4395 "parser.cc"
    break;

  case 432: // tyvar: tyvarid
#line 1450 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4401 "parser.cc"
    break;

  case 433: // tyvarop: "`" tyvarid "`"
#line 1452 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4407 "parser.cc"
    break;

  case 434: // tyvarid: "VARID"
#line 1454 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4413 "parser.cc"
    break;

  case 435: // tyvarid: special_id
#line 1455 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4419 "parser.cc"
    break;

  case 436: // tyvarid: "unsafe"
#line 1456 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 4425 "parser.cc"
    break;

  case 437: // tyvarid: "safe"
#line 1457 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 4431 "parser.cc"
    break;

  case 438: // tyvarid: "interruptible"
#line 1458 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 4437 "parser.cc"
    break;

  case 439: // var: varid
#line 1461 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4443 "parser.cc"
    break;

  case 440: // var: "(" varsym ")"
#line 1462 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4449 "parser.cc"
    break;

  case 441: // qvar: qvarid
#line 1464 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4455 "parser.cc"
    break;

  case 442: // qvar: "(" varsym ")"
#line 1465 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4461 "parser.cc"
    break;

  case 443: // qvar: "(" qvarsym1 ")"
#line 1466 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4467 "parser.cc"
    break;

  case 444: // qvarid: varid
#line 1468 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4473 "parser.cc"
    break;

  case 445: // qvarid: "QVARID"
#line 1469 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4479 "parser.cc"
    break;

  case 446: // varid: "VARID"
#line 1471 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4485 "parser.cc"
    break;

  case 447: // varid: special_id
#line 1472 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4491 "parser.cc"
    break;

  case 448: // varid: "unsafe"
#line 1473 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 4497 "parser.cc"
    break;

  case 449: // varid: "safe"
#line 1474 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 4503 "parser.cc"
    break;

  case 450: // varid: "interruptible"
#line 1475 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 4509 "parser.cc"
    break;

  case 451: // varid: "forall"
#line 1476 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 4515 "parser.cc"
    break;

  case 452: // varid: "family"
#line 1477 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 4521 "parser.cc"
    break;

  case 453: // varid: "role"
#line 1478 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 4527 "parser.cc"
    break;

  case 454: // qvarsym: varsym
#line 1480 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4533 "parser.cc"
    break;

  case 455: // qvarsym: qvarsym1
#line 1481 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4539 "parser.cc"
    break;

  case 456: // qvarsym_no_minus: varsym_no_minus
#line 1483 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4545 "parser.cc"
    break;

  case 457: // qvarsym_no_minus: qvarsym1
#line 1484 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4551 "parser.cc"
    break;

  case 458: // qvarsym1: "QVARSYM"
#line 1486 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4557 "parser.cc"
    break;

  case 459: // varsym: varsym_no_minus
#line 1488 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4563 "parser.cc"
    break;

  case 460: // varsym: "-"
#line 1489 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 4569 "parser.cc"
    break;

  case 461: // varsym_no_minus: "VARSYM"
#line 1491 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4575 "parser.cc"
    break;

  case 462: // varsym_no_minus: special_sym
#line 1492 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4581 "parser.cc"
    break;

  case 463: // special_id: "as"
#line 1494 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 4587 "parser.cc"
    break;

  case 464: // special_id: "qualified"
#line 1495 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 4593 "parser.cc"
    break;

  case 465: // special_id: "hiding"
#line 1496 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 4599 "parser.cc"
    break;

  case 466: // special_id: "export"
#line 1497 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 4605 "parser.cc"
    break;

  case 467: // special_id: "label"
#line 1498 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 4611 "parser.cc"
    break;

  case 468: // special_id: "dynamic"
#line 1499 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 4617 "parser.cc"
    break;

  case 469: // special_id: "stdcall"
#line 1500 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 4623 "parser.cc"
    break;

  case 470: // special_id: "ccall"
#line 1501 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 4629 "parser.cc"
    break;

  case 471: // special_id: "capi"
#line 1502 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 4635 "parser.cc"
    break;

  case 472: // special_id: "prim"
#line 1503 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 4641 "parser.cc"
    break;

  case 473: // special_id: "javascript"
#line 1504 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 4647 "parser.cc"
    break;

  case 474: // special_id: "group"
#line 1505 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 4653 "parser.cc"
    break;

  case 475: // special_id: "stock"
#line 1506 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 4659 "parser.cc"
    break;

  case 476: // special_id: "anyclass"
#line 1507 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 4665 "parser.cc"
    break;

  case 477: // special_id: "via"
#line 1508 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 4671 "parser.cc"
    break;

  case 478: // special_id: "unit"
#line 1509 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 4677 "parser.cc"
    break;

  case 479: // special_id: "dependency"
#line 1510 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 4683 "parser.cc"
    break;

  case 480: // special_id: "signature"
#line 1511 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 4689 "parser.cc"
    break;

  case 481: // special_sym: "!"
#line 1513 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 4695 "parser.cc"
    break;

  case 482: // special_sym: "."
#line 1514 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 4701 "parser.cc"
    break;

  case 483: // special_sym: "*"
#line 1515 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 4707 "parser.cc"
    break;

  case 484: // qconid: conid
#line 1519 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4713 "parser.cc"
    break;

  case 485: // qconid: "QCONID"
#line 1520 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4719 "parser.cc"
    break;

  case 486: // conid: "CONID"
#line 1522 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4725 "parser.cc"
    break;

  case 487: // qconsym: consym
#line 1524 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4731 "parser.cc"
    break;

  case 488: // qconsym: "QCONSYM"
#line 1525 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4737 "parser.cc"
    break;

  case 489: // consym: "CONSYM"
#line 1527 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4743 "parser.cc"
    break;

  case 490: // consym: ":"
#line 1528 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4749 "parser.cc"
    break;

  case 491: // literal: "CHAR"
#line 1532 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 4755 "parser.cc"
    break;

  case 492: // literal: "STRING"
#line 1533 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 4761 "parser.cc"
    break;

  case 493: // literal: "INTEGER"
#line 1534 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < int > ()});}
#line 4767 "parser.cc"
    break;

  case 494: // literal: "RATIONAL"
#line 1535 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Double{yystack_[0].value.as < double > ()});}
#line 4773 "parser.cc"
    break;

  case 495: // literal: "PRIMINTEGER"
#line 1536 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < int > ()});}
#line 4779 "parser.cc"
    break;

  case 497: // close: error
#line 1544 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 4785 "parser.cc"
    break;

  case 498: // modid: "CONID"
#line 1548 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4791 "parser.cc"
    break;

  case 499: // modid: "QCONID"
#line 1549 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4797 "parser.cc"
    break;

  case 500: // commas: commas ","
#line 1551 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 4803 "parser.cc"
    break;

  case 501: // commas: ","
#line 1552 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 4809 "parser.cc"
    break;


#line 4813 "parser.cc"

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


  const short parser::yypact_ninf_ = -665;

  const short parser::yytable_ninf_ = -460;

  const short
  parser::yypact_[] =
  {
      41,   177,  -665,   101,  -665,  -665,  -665,  -665,  -665,   298,
      43,    37,  -665,    47,    -5,    -5,    64,  -665,  -665,  -665,
    -665,   189,  -665,  -665,  -665,   120,  -665,   141,   190,  4819,
     255,   282,   194,  -665,   827,  -665,   118,  -665,  -665,  -665,
    -665,   177,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,
    -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,
    -665,  -665,  -665,  -665,   291,  -665,  -665,  -665,  -665,   257,
     215,  -665,   269,  -665,  -665,  -665,  -665,  -665,  -665,  -665,
     139,  -665,   177,  -665,   266,  -665,  2567,  4410,  -665,   277,
     180,  2567,  -665,  -665,  -665,   353,   238,  -665,  3585,   379,
     180,  3365,   301,   289,  5052,    88,  2966,  3365,  3099,  3365,
    1636,  1503,   129,  -665,  -665,  -665,  -665,  -665,  -665,  -665,
      51,   301,   287,   194,  -665,  -665,  -665,  -665,   364,    78,
    -665,  -665,   360,  -665,  3232,  -665,   342,  -665,  -665,  -665,
    -665,  -665,  -665,   344,   107,  -665,  -665,  -665,  -665,   322,
    -665,   336,   345,  -665,  -665,  -665,  -665,  -665,   350,  -665,
     363,   366,   369,  -665,  -665,  -665,  4819,  4856,  -665,  -665,
    -665,  -665,   476,  -665,     5,  1503,   461,   449,  -665,  -665,
    2567,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  5148,
    3688,  3474,   372,  4954,  -665,  -665,  -665,  -665,  -665,   463,
    4717,  -665,   404,  -665,    -8,  -665,  4717,  -665,  -665,  -665,
    -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  3895,
    2035,  2035,  -665,   382,   421,   423,   424,   428,  3895,  1237,
    1237,  -665,   494,  4410,  4410,   112,   429,    21,   121,   475,
    -665,  -665,   -20,  5052,  -665,   444,   600,   -16,   418,   117,
    -665,   127,  -665,  -665,  3365,  -665,  -665,  2833,  -665,  3232,
     178,  -665,  -665,   768,  -665,  -665,  -665,   449,    12,   419,
     410,  -665,  2567,  -665,  -665,  -665,  -665,  -665,  -665,  3099,
    -665,  -665,   109,   150,   366,   417,   422,   427,   216,  -665,
     205,  3895,  5052,  5052,  -665,   275,   266,   401,  4410,  3895,
    5148,  2567,  2301,   768,  -665,    38,  -665,  -665,  2700,  -665,
    -665,  -665,  -665,  4717,  -665,  5015,  3365,  -665,  -665,  -665,
    -665,  -665,  -665,  -665,  -665,   431,   434,   430,  -665,   436,
      47,   177,    34,   456,   472,   272,  3895,  2567,  -665,   582,
     130,   443,   437,  -665,  -665,  -665,  -665,   446,   470,   466,
    -665,   450,   452,  -665,   453,   448,   457,   241,   244,   455,
     460,   276,  -665,  -665,  4410,  3895,  4410,  -665,  -665,  -665,
     459,   462,   238,   180,  3365,   488,   496,   100,  -665,  -665,
      49,  -665,   566,  -665,  -665,  -665,  -665,  -665,  -665,   568,
     154,  -665,  -665,   360,    50,  2567,  -665,   519,   330,  3895,
     -12,  3895,   471,   477,   504,   534,  -665,  -665,   536,   505,
     165,    88,   537,  -665,  2567,  -665,  -665,   502,   503,  2567,
    2567,  2301,  1769,  -665,  1769,   613,  -665,  1769,  -665,  1769,
     136,  -665,  -665,  -665,  -665,   546,   544,   547,  5111,   510,
    -665,  -665,  -665,  -665,  -665,    -4,   334,  -665,  -665,  -665,
    -665,   601,   548,   514,  -665,   515,   238,  -665,  -665,  -665,
    -665,  -665,   529,  -665,   521,   557,  -665,  -665,  -665,  4917,
    -665,  -665,  -665,   535,  4819,  -665,  -665,  -665,  -665,  1902,
    1370,  -665,  -665,  -665,   533,  3895,  -665,  5148,  5185,  -665,
    3895,  3895,  -665,  -665,  -665,  3895,  -665,  -665,  -665,  -665,
    -665,   971,   971,  -665,  -665,  -665,   556,  -665,  3895,   494,
    -665,  -665,  2567,  -665,  2035,  -665,  2567,   288,  -665,  -665,
    1237,  -665,  -665,  3895,  3895,  5291,   564,  -665,  -665,   576,
     468,  -665,  -665,  5148,   551,  -665,  -665,  -665,  -665,   554,
     665,   246,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,
     550,  -665,   585,  -665,  -665,  -665,  -665,  -665,  3895,  3895,
     552,   555,   275,  -665,   591,  3895,   652,   658,   677,  -665,
    2567,  2301,  -665,  -665,  -665,  5015,  1769,  -665,  4819,   577,
    3365,  -665,  2168,  -665,   587,   575,  -665,   316,    47,  -665,
    -665,  -665,  -665,  3895,  5384,  5384,  -665,  -665,  -665,  -665,
    -665,  -665,   586,   661,  3792,  -665,  -665,   169,  -665,    67,
    -665,  -665,  -665,   382,  1104,  1104,  -665,  -665,  -665,  -665,
    -665,  5384,   673,  3895,   457,   621,  -665,  -665,  -665,  2301,
    2567,  -665,     0,    14,  -665,  -665,  -665,  -665,  -665,  -665,
     624,  -665,  4717,   306,   677,    66,  -665,   677,  -665,  -665,
    -665,  -665,  -665,   599,  -665,  -665,  -665,  -665,  2434,  2301,
    2567,  -665,    39,  -665,  -665,  -665,    13,   631,  -665,  -665,
    4410,  4410,  4410,  -665,   383,  -665,   971,  -665,   702,   697,
    -665,  -665,   203,  -665,    70,  -665,   632,   329,  -665,  -665,
    3895,  -665,  -665,  -665,  3895,  -665,  5258,   652,   630,  4513,
    -665,  -665,  -665,   382,   382,  -665,  -665,  -665,  -665,  3998,
     144,   669,  -665,  -665,  -665,  -665,  -665,   637,   601,  -665,
    -665,  3895,  -665,  3895,   644,  -665,   385,  3895,  4101,  -665,
    -665,  2567,  -665,  4410,  -665,  1104,  -665,  5384,  4204,  4307,
    -665,  -665,  -665,  -665,  -665,  4717,   608,  -665,  4717,   213,
    -665,    88,    73,  -665,  -665,   615,   619,  -665,  4410,  -665,
    2567,  -665,   634,   622,  3895,  -665,  5351,  -665,  -665,  3474,
     656,   657,  -665,  -665,  -665,  5384,  -665,   640,   221,   628,
      47,    74,  4615,  -665,  4717,  -665,   382,   142,  -665,  4410,
    -665,  -665,  -665,  -665,  -665,  -665,   631,  5384,  -665,  -665,
    -665,  4410,  -665,  -665,  -665,  -665,  3895,  -665,  -665,  -665,
    -665
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    36,     0,     2,    36,     4,   498,   499,     8,
       0,    39,     1,     0,     0,     0,    18,    11,    35,    13,
      16,    58,   497,   496,    12,   162,   158,     0,     0,     0,
       0,    42,    37,    15,    14,   161,     0,     6,     7,   463,
     465,     0,   464,   451,   466,   467,   468,   449,   450,   448,
     452,   453,   469,   470,   471,   472,   473,   474,   475,   476,
     477,   478,   480,   479,     0,   446,   412,   445,   410,     0,
      19,    21,    24,    32,   402,   411,    31,   441,   444,   447,
       0,    41,     0,    34,    38,   311,     0,     0,   112,     0,
       0,     0,    51,    52,    53,    81,     0,   113,     0,     0,
       0,     0,   271,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   486,   485,   491,   492,   493,   494,   495,
     271,   271,    49,    56,    59,    60,    61,    62,   128,     0,
      65,   253,    66,   279,   283,   293,   302,   304,   306,   372,
     385,   373,   170,   305,   444,   374,   484,   307,   159,     0,
      23,     0,     0,   460,   481,   483,   482,   461,     0,   458,
       0,     0,     0,   459,   462,    17,     0,    27,    22,    36,
      36,     3,    44,    33,     0,     0,     0,   276,   284,   277,
       0,   437,   438,   436,   418,   175,   419,   174,   201,   238,
       0,     0,     0,     0,   434,   417,   416,   414,   413,   138,
       0,   173,     0,   125,   184,   187,   188,   193,   192,   199,
     394,   196,   399,   407,   415,   200,   197,   432,   435,   212,
     359,   359,   300,   287,     0,     0,     0,     0,     0,   152,
     152,   155,     0,     0,     0,     0,     0,   184,   394,     0,
     301,   292,     0,     0,   272,     0,     0,     0,     0,     0,
     380,   163,   378,   376,     0,   350,   353,     0,   295,   281,
       0,   490,   386,     0,   489,   488,   312,   276,   317,     0,
     318,   427,     0,   426,   430,   457,   456,   389,   487,   460,
     381,   501,     0,     0,   457,     0,   456,   389,     0,   383,
       0,     0,     0,     0,    50,     0,    57,     0,     0,     0,
       0,     0,     0,     0,   255,   157,   260,   425,     0,   424,
     428,   455,   454,     0,   290,   366,     0,   160,   405,   406,
     404,   403,   443,   442,    20,     0,     0,    28,    30,     0,
       0,     0,    46,     0,     0,     0,     0,     0,   285,     0,
       0,     0,   239,   241,   439,   216,   398,     0,   176,     0,
     180,     0,     0,   204,   213,     0,   407,     0,     0,     0,
       0,     0,    67,   202,     0,     0,     0,   194,   195,   213,
       0,   211,     0,     0,     0,   363,     0,     0,   358,   360,
       0,   286,     0,    78,    77,    79,    80,   208,   167,   148,
       0,   256,   151,     0,     0,     0,    76,     0,   118,     0,
       0,     0,     0,     0,     0,     0,   282,   266,     0,     0,
       0,     0,     0,   351,     0,   352,   254,     0,     0,   313,
     319,     0,     0,   310,     0,   314,   308,     0,   309,     0,
     442,   375,   382,   500,   384,     0,     0,     0,     0,   263,
     421,    55,   420,   422,   387,     0,   114,   262,   181,   168,
     169,   157,     0,   328,   330,     0,     0,   258,   259,   280,
     291,   369,     0,   365,   368,   371,   294,    26,    25,     0,
       9,    10,    43,     0,     0,    40,    45,   289,   288,     0,
       0,   299,   275,   278,     0,     0,   203,     0,     0,   206,
       0,     0,   397,   401,   207,     0,   400,   395,   396,   408,
     433,   134,   134,   137,   124,   185,   189,    63,     0,   364,
     361,   349,     0,   354,   357,   355,     0,     0,    75,   153,
     150,   154,   297,     0,     0,     0,    86,    72,   165,   167,
       0,    73,    68,     0,     0,   273,   265,   267,   377,     0,
       0,     0,   164,   391,   379,   264,   296,   431,   390,   321,
     323,   327,   312,   325,   324,   316,   315,   270,     0,     0,
       0,     0,     0,   127,     0,     0,   235,   225,   243,   257,
       0,     0,   429,   156,   303,     0,     0,    29,     0,     0,
       0,   333,     0,   346,     0,   335,   339,     0,     0,   334,
     440,   242,   240,     0,     0,     0,   215,   217,   220,   222,
     177,   179,   214,   107,     0,   129,   133,     0,   130,     0,
     214,   362,   356,   287,   144,   144,   147,   149,   101,   119,
     120,     0,    91,     0,     0,     0,   274,   392,   393,     0,
     320,   171,     0,     0,   423,   388,    54,   126,   115,   216,
     229,   231,     0,     0,   243,     0,    69,   244,   246,   261,
     329,   367,   370,     0,    47,   347,   336,   331,   338,     0,
       0,   340,   157,   344,   332,   178,     0,     0,   205,   108,
       0,     0,     0,   105,   121,   135,   132,   136,     0,   109,
     139,   143,     0,   140,     0,    87,     0,     0,    71,   166,
       0,   326,   322,   268,     0,   269,     0,   235,     0,   236,
     190,   198,   233,   287,   287,    70,    84,    82,    83,     0,
       0,   247,   250,   409,   245,    48,   337,     0,   157,   342,
     343,     0,   218,     0,   116,   106,   121,     0,     0,   103,
     131,     0,   110,     0,   145,   142,   146,     0,   100,   100,
      92,    64,   172,   234,   230,     0,     0,   191,     0,     0,
     227,     0,     0,   251,   186,   209,     0,   248,     0,   249,
       0,   341,     0,     0,     0,   102,     0,   104,   122,     0,
       0,   200,   298,   111,   141,    88,    90,     0,     0,    99,
       0,     0,   236,   232,   237,   223,   287,     0,   224,     0,
     252,    85,   345,   219,   221,   117,   200,     0,    89,    95,
      93,     0,    98,    96,    94,   226,     0,   210,   123,    97,
     228
  };

  const short
  parser::yypgoto_[] =
  {
    -665,  -665,  -665,  -665,  -665,  -665,  -665,    36,  -665,  -665,
    -430,  -665,   579,  -665,  -665,  -665,  -140,   623,  -665,  -665,
    -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,
    -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,
    -665,   -50,  -665,  -665,  -665,     9,  -221,  -665,  -665,  -665,
    -665,  -665,  -665,  -665,  -665,    23,   465,  -665,    77,   248,
    -665,  -665,    20,   146,  -665,  -665,   527,  -665,  -312,  -414,
     744,  -665,  -665,  -665,  -323,    76,  -157,   209,  -665,  -665,
    -147,  -136,  -665,   -47,  -665,   -84,  -665,   -74,  -665,  -614,
    -665,  -665,  -665,  -612,  -180,   473,   -19,  -665,   558,   132,
     249,  -664,  -466,  -665,    65,   -13,  -665,  -665,    79,  -665,
      33,  -665,  -665,   292,   138,  -665,   137,    75,   749,  -220,
     528,  -665,   484,  -665,   348,  -665,   338,   -89,   759,    -9,
    -258,  -208,  -665,   -66,   -78,  -665,  -665,  -102,  -665,  -665,
    -665,  -665,   135,  -665,  -665,  -413,  -665,   151,  -665,  -665,
     133,  -665,  -665,   540,  -665,   -62,   583,   299,  -283,  -665,
     239,  -665,  -665,  -665,   405,    85,  -665,   -98,  -646,   -83,
    -665,   415,   -63,  -665,  -665,  -665,   -27,  -665,  -187,  -665,
     264,  -665,  -130,  -665,  -665,  -665,  -308,  -665,  -176,  -261,
       1,  -183,     2,  -665,  -665,    98,   -48,   -77,   -87,  -665,
    -174,  -104,   -54,  -243,  -665,  -285,   -23,  -107
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   171,     6,    10,    19,    30,
      69,    70,    71,   168,   326,   327,    72,    84,    11,    20,
      21,    32,    82,   332,   475,   476,   295,   122,   439,    33,
      34,   123,   124,   125,   126,   235,   127,   228,   710,   759,
     622,   685,   775,   688,   740,   778,   779,   605,   670,   733,
     680,   128,   567,   765,   526,   729,   199,   298,   606,   607,
     503,   362,   681,   682,   616,   518,   390,   231,   232,   457,
      27,    36,   412,   527,   387,   447,   129,   632,   200,   201,
     354,   599,   449,   349,   698,   350,   755,   204,   205,   699,
     206,   367,   207,   700,   208,   389,   756,   370,   355,   488,
     596,   597,   600,   644,   749,   750,   568,   640,   641,   642,
     702,   341,   342,   343,   646,   647,   648,   711,   391,   608,
     304,   305,   306,   131,   243,   244,   375,   177,   393,   178,
     179,   751,   180,   134,   135,   136,   137,   282,   283,   269,
     270,   550,   452,   453,   481,   584,   585,   586,   661,   662,
     663,   587,   376,   256,   257,   222,   377,   378,   379,   462,
     463,   464,   138,   139,   250,   251,   140,   141,   440,   271,
     542,   209,   210,    73,   211,   712,   212,    75,   213,   214,
     441,   442,   308,   272,   309,   273,   215,   216,   217,   142,
     143,    77,    78,   310,   274,   275,   312,   163,    79,   164,
     145,   146,   277,   278,   147,    24,     9,   288
  };

  const short
  parser::yytable_[] =
  {
     218,   253,    74,   203,   356,   290,   409,   252,   268,   392,
     392,   218,   396,   482,   236,   382,   162,   360,   150,   454,
     363,   267,   267,   241,   237,   133,   368,   328,   255,   258,
      76,   260,   340,   276,   286,   238,   144,   569,   240,   450,
     202,    13,   259,   347,   579,   471,   473,   337,    22,   307,
      22,    22,   444,   748,   348,   348,   314,   287,   619,   172,
     509,   407,     1,   285,   770,   456,   456,   588,    22,   403,
     291,    22,   369,   563,    22,    22,   528,   693,   365,   483,
     417,   366,  -182,   348,   357,   358,   267,   747,   706,   418,
     420,   695,   388,   721,   307,   515,   421,   530,   286,   638,
      25,    12,   770,   218,   218,   247,   218,   365,    66,   521,
     366,   404,    68,   218,   408,   722,   694,   707,   708,   218,
     455,   287,   302,   659,   333,    26,   564,   162,   422,   418,
     694,   782,   218,   460,   784,   334,   748,   419,   551,    74,
      74,   218,     2,   474,   573,    17,   218,   218,   653,   397,
     398,    23,    18,    23,    23,   388,   242,   689,   299,   237,
     237,   483,   161,   448,   514,   520,   359,    76,    76,   656,
     747,    23,   747,    29,    23,   709,   413,    23,    23,   255,
     598,   314,   676,   425,   307,   735,    66,  -439,   786,   801,
      68,   344,   399,   248,   300,   589,   261,   249,   162,   112,
     388,   -74,   513,    31,   218,   329,   330,   410,   113,   284,
     485,   218,   218,   259,   203,   514,  -440,   598,    37,   426,
     133,   133,   806,  -439,   148,   427,   218,   280,   400,   505,
     311,   144,   144,   281,   149,   631,   631,   -74,   466,   264,
     169,   289,   170,   411,   405,   281,   300,   443,   719,   218,
      35,   202,  -440,   709,   532,   762,   519,   763,   411,   301,
     428,   768,   302,   529,    66,   348,   429,    38,    68,   520,
     539,   675,   625,   284,   540,   311,   541,   218,   218,   218,
     504,   220,    80,   221,   676,    66,   666,   667,   650,    68,
     237,   484,   506,   436,   437,   337,   511,     7,   795,   459,
     617,     8,   344,   664,   761,   734,    81,   253,   472,    83,
     307,   510,   218,   252,   218,   785,   465,   434,   735,   444,
     553,   433,   554,   800,   677,   555,   432,   556,   786,   577,
     340,   166,   433,   267,   561,   267,   801,   531,   267,   229,
     267,   230,   307,   624,   161,   276,   691,   276,   602,   591,
     276,   497,   276,   624,   261,   601,   498,   433,   628,   348,
     433,   610,   281,    14,    15,   311,   153,   165,   154,   155,
     151,   742,   348,   479,   156,   480,   454,   501,   167,   502,
     152,   173,   153,   673,   154,   155,   219,   618,   598,   614,
     156,   615,   438,   239,   683,   683,   157,   264,   218,   736,
     659,   218,   660,   218,   218,   678,   242,   703,   218,   704,
     524,   525,   157,   158,   565,   566,   159,   160,   294,   245,
     771,   218,   388,   388,   176,   224,   225,   226,   227,   223,
     738,   316,   739,   357,   358,   297,   218,   218,   218,   261,
     560,   301,    74,   315,   302,   686,   318,    74,   266,   266,
     725,   153,   317,   154,   155,   319,   444,   665,   598,   156,
     320,   796,   701,   727,   728,   727,   766,   788,   292,   293,
      76,   218,   218,   321,   652,    76,   322,   303,   218,   323,
     331,   157,   264,   810,   335,   159,   265,   267,   281,   344,
     361,   311,   133,   133,   364,   803,   804,   381,   383,   276,
     384,   385,   655,   144,   144,   386,   218,   218,   218,   395,
     401,   133,   773,   266,   443,   683,   402,   218,   338,   701,
     674,   406,   144,   311,   262,   423,   424,   430,   261,   336,
     237,   445,  -459,   477,   218,   344,   218,   431,   470,   444,
     153,   467,   154,   155,   468,   486,   469,   184,   156,   478,
     490,    74,   489,   487,   448,   218,   491,   352,   388,   186,
     492,   776,   493,   494,   495,   701,   303,   496,   701,   507,
     157,   264,   499,  -348,   159,   265,   465,   500,   508,    76,
     809,   512,   356,   218,   218,   218,   724,   397,   726,   195,
     196,   516,   618,   197,   198,   517,   237,   237,   237,   798,
     523,   533,   701,   218,   701,   133,   133,   218,   534,   218,
     535,   536,   218,   537,   545,   538,   144,   144,   713,   547,
     548,   686,   218,   557,   558,   754,   562,   559,   456,   570,
     571,   574,   572,   348,   218,   237,   218,   575,   576,   451,
     218,   218,   561,   590,   578,   366,   218,   253,   621,   397,
     218,   218,   218,   252,   397,   397,   623,   626,   218,   237,
     346,   218,   357,   630,   237,   237,   629,   133,   637,   634,
     388,   218,   635,   153,   791,   154,   155,   218,   144,   218,
     639,   156,   218,   713,   237,   643,   645,   654,   218,   657,
     658,   153,   261,   154,   155,   218,   668,   218,   669,   156,
     687,   690,   218,   157,   153,   754,   154,   155,   697,   715,
     218,   723,   156,   731,   218,   237,   732,   397,   737,   218,
     745,   157,   758,   760,   764,   159,   133,   237,   113,   790,
     303,   789,   794,   522,   157,   264,   793,   144,   159,   265,
     797,  -220,   799,   802,   184,   324,   296,   808,   781,   767,
     609,   351,   546,   730,   352,   774,   186,   394,   549,    28,
     552,   684,   266,   446,   435,   266,   741,   266,   633,   752,
     807,   696,    39,   805,   620,   627,   744,   371,   783,   592,
      40,   281,   705,   130,   714,   757,   195,   196,   416,   458,
     197,   198,    42,   132,   717,   720,    43,   415,    44,    45,
      46,    47,    48,    49,   380,    50,    51,    52,    53,   716,
      54,    55,    56,   612,   651,    57,   544,   583,   583,    58,
      59,    60,    61,    62,    63,   543,   636,     0,     0,     0,
      85,    39,    86,    87,    88,    89,   787,    90,     0,    40,
      91,     0,     0,    92,    93,    94,    95,    96,     0,    97,
     611,    42,     0,    98,   613,    43,    99,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,   101,    58,    59,
      60,    61,    62,    63,   102,     0,     0,    65,   113,   103,
     104,    67,   114,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   105,     0,     0,     0,     0,   649,   106,
       0,     0,     0,     0,   266,   107,     0,     0,   108,   109,
     583,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,   111,     0,   112,     0,
       0,     0,     0,     0,     0,     0,    65,   113,     0,     0,
      67,   114,     0,     0,     0,     0,   115,   116,   117,   118,
       0,     0,   119,     0,     0,     0,   120,   121,   692,     0,
       0,     0,     0,     0,    85,    39,    86,     0,   603,     0,
       0,    90,     0,    40,    91,     0,     0,    92,    93,    94,
       0,    96,     0,     0,     0,    42,   583,   604,   718,    43,
       0,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,   101,    58,    59,    60,    61,    62,    63,   102,     0,
       0,     0,     0,   103,   104,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   105,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,   107,
       0,     0,   108,   109,     0,     0,     0,     0,     0,   772,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
     111,     0,   112,     0,     0,     0,     0,     0,     0,     0,
      65,   113,     0,     0,    67,   114,     0,     0,   792,     0,
     115,   116,   117,   118,     0,     0,   119,    85,    39,    86,
     120,   121,     0,     0,    90,     0,    40,    91,     0,     0,
      92,    93,    94,     0,    96,     0,     0,     0,    42,     0,
     679,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,   102,     0,     0,     0,     0,   103,   104,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     105,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,   107,     0,     0,   108,   109,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,   111,     0,   112,     0,     0,     0,     0,
       0,     0,     0,    65,   113,     0,     0,    67,   114,     0,
       0,     0,     0,   115,   116,   117,   118,     0,     0,   119,
      85,    39,    86,   120,   121,     0,     0,    90,     0,    40,
      91,     0,     0,    92,    93,    94,     0,    96,     0,     0,
       0,    42,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,   101,    58,    59,
      60,    61,    62,    63,   102,     0,     0,     0,     0,   103,
     104,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   105,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,   107,     0,     0,   108,   109,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,   111,     0,   112,     0,
       0,     0,     0,     0,     0,     0,    65,   113,     0,     0,
      67,   114,     0,     0,     0,     0,   115,   116,   117,   118,
       0,    22,   119,    85,    39,    86,   120,   121,     0,     0,
      90,     0,    40,    91,     0,     0,     0,     0,     0,     0,
      96,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
     101,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,   103,   174,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,   107,     0,
       0,   108,   580,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    23,   110,     0,     0,     0,   175,
       0,   112,     0,     0,     0,   582,     0,     0,     0,    65,
     113,     0,     0,    67,   114,     0,     0,     0,     0,   115,
     116,   117,   118,     0,     0,   119,    85,    39,    86,     0,
       0,     0,     0,    90,     0,    40,    91,     0,     0,     0,
       0,     0,     0,    96,     0,     0,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   100,
      50,    51,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,   101,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,   103,   174,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   261,     0,     0,   106,     0,     0,     0,     0,
       0,   107,     0,     0,   279,     0,   154,   155,     0,     0,
       0,     0,   156,     0,     0,     0,     0,     0,   110,     0,
       0,     0,   175,   280,   112,     0,     0,     0,     0,   281,
     263,     0,    65,   113,   157,   264,    67,   114,   159,   265,
       0,     0,   115,   116,   117,   118,     0,     0,   119,    85,
      39,    86,     0,     0,     0,     0,    90,     0,    40,    91,
       0,     0,     0,     0,     0,     0,    96,     0,     0,     0,
      42,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,   101,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,   103,   174,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   261,     0,     0,   106,     0,
       0,     0,     0,     0,   107,     0,     0,   108,     0,   154,
     155,     0,     0,     0,     0,   156,     0,     0,     0,     0,
       0,   110,   262,     0,     0,   175,     0,   112,     0,     0,
       0,     0,     0,   263,     0,    65,   113,   157,   264,    67,
     114,   159,   265,     0,     0,   115,   116,   117,   118,     0,
       0,   119,    85,    39,    86,     0,     0,     0,     0,    90,
       0,    40,    91,     0,     0,     0,     0,     0,     0,    96,
       0,     0,     0,    42,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,   101,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,   103,   174,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   261,     0,
       0,   106,     0,     0,     0,     0,     0,   107,     0,     0,
     108,     0,   154,   155,     0,     0,     0,     0,   156,     0,
       0,     0,     0,     0,   110,     0,     0,     0,   175,     0,
     112,     0,     0,     0,     0,     0,   263,     0,    65,   113,
     157,   264,    67,   114,   159,   265,     0,     0,   115,   116,
     117,   118,     0,     0,   119,    85,    39,    86,     0,     0,
       0,     0,    90,     0,    40,    91,     0,     0,     0,     0,
       0,     0,    96,     0,     0,     0,    42,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,   101,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,   103,   174,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
     107,     0,     0,   108,   580,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   581,     0,     0,   110,     0,     0,
       0,   175,     0,   112,     0,     0,     0,   582,     0,     0,
       0,    65,   113,     0,     0,    67,   114,     0,     0,     0,
       0,   115,   116,   117,   118,     0,     0,   119,    85,    39,
      86,     0,     0,     0,     0,    90,     0,    40,    91,     0,
       0,     0,     0,     0,     0,   372,     0,     0,     0,    42,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,     0,    54,    55,    56,
       0,   373,    57,     0,     0,   101,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,   103,   174,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,   107,     0,     0,   108,   374,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,   175,     0,   112,     0,     0,     0,
       0,     0,     0,     0,    65,   113,     0,     0,    67,   114,
       0,     0,     0,     0,   115,   116,   117,   118,     0,     0,
     119,    85,    39,    86,     0,     0,     0,     0,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,    42,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,   101,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
     103,   174,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,   107,     0,     0,   108,
     580,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,     0,   175,     0,   112,
       0,     0,     0,   582,     0,     0,     0,    65,   113,     0,
       0,    67,   114,     0,     0,     0,     0,   115,   116,   117,
     118,     0,     0,   119,    85,    39,    86,     0,     0,     0,
       0,    90,     0,    40,    91,     0,     0,     0,     0,     0,
       0,   372,     0,     0,     0,    42,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,   101,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,   103,   174,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,   107,
       0,     0,   108,   374,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
     175,     0,   112,     0,     0,     0,     0,     0,     0,     0,
      65,   113,     0,     0,    67,   114,     0,     0,     0,     0,
     115,   116,   117,   118,     0,     0,   119,    85,    39,    86,
       0,     0,     0,     0,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,   103,   174,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,   107,     0,     0,   108,   580,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,   175,     0,   112,     0,     0,     0,     0,
       0,     0,     0,    65,   113,     0,     0,    67,   114,     0,
       0,     0,     0,   115,   116,   117,   118,     0,     0,   119,
      85,    39,    86,     0,     0,     0,     0,    90,     0,    40,
      91,     0,     0,     0,     0,     0,     0,    96,     0,     0,
       0,    42,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,   101,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,   103,
     174,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,   107,     0,     0,   108,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,   175,     0,   112,     0,
       0,     0,     0,     0,     0,     0,    65,   113,     0,     0,
      67,   114,     0,     0,     0,     0,   115,   116,   117,   118,
       0,     0,   119,    85,    39,    86,     0,     0,     0,     0,
      90,     0,    40,    91,     0,     0,     0,     0,     0,     0,
      96,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
     101,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,   103,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,   107,     0,
       0,   108,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,   175,
       0,   112,     0,     0,     0,     0,     0,     0,     0,    65,
     113,     0,     0,    67,   114,     0,     0,     0,     0,   115,
     116,   117,   118,     0,     0,   119,    85,    39,    86,     0,
       0,     0,     0,    90,     0,    40,    91,     0,     0,     0,
       0,     0,     0,    96,     0,     0,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   100,
      50,    51,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,   414,
       0,   107,     0,     0,     0,   254,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   110,     0,
       0,     0,   175,     0,   112,     0,     0,     0,     0,     0,
       0,     0,    65,   113,     0,     0,    67,   114,     0,     0,
       0,     0,   115,   116,   117,   118,     0,     0,   119,    85,
      39,    86,     0,     0,     0,     0,    90,     0,    40,    91,
       0,     0,     0,     0,     0,     0,    96,     0,     0,     0,
      42,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,   107,     0,     0,     0,   254,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,     0,   175,     0,   112,     0,     0,
       0,     0,     0,     0,     0,    65,   113,     0,     0,    67,
     114,     0,     0,     0,     0,   115,   116,   117,   118,     0,
       0,   119,    85,    39,    86,     0,     0,     0,     0,    90,
       0,    40,    91,     0,     0,     0,     0,     0,     0,    96,
       0,     0,     0,    42,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,   101,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,   107,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   110,     0,     0,     0,   175,     0,
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
     107,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   313,     0,     0,     0,     0,   110,     0,     0,
       0,   175,     0,   112,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,   175,     0,   112,     0,    39,     0,
       0,     0,     0,     0,    65,   113,    40,     0,    67,   114,
       0,     0,     0,     0,   115,   116,   117,   118,    42,     0,
     119,     0,   345,     0,    44,    45,    46,   181,   182,   183,
       0,     0,     0,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   184,     0,     0,     0,     0,     0,     0,
     351,     0,   185,   352,     0,   186,   187,     0,   188,     0,
       0,     0,     0,     0,     0,   189,     0,     0,     0,   190,
       0,     0,     0,   191,   353,   192,     0,     0,     0,    39,
     281,   193,     0,   194,    66,   195,   196,    40,    68,   197,
     198,     0,     0,     0,   233,     0,     0,     0,     0,    42,
       0,     0,     0,     0,     0,    44,    45,    46,   181,   182,
     183,     0,   234,     0,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   184,     0,     0,     0,     0,     0,
       0,     0,     0,   185,     0,     0,   186,   187,     0,   188,
       0,     0,     0,     0,     0,     0,   189,     0,     0,     0,
     190,     0,    39,     0,   191,     0,   192,     0,     0,     0,
      40,     0,   193,     0,   194,    66,   195,   196,     0,    68,
     197,   198,    42,     0,     0,     0,   345,     0,    44,    45,
      46,   181,   182,   183,     0,     0,     0,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   184,     0,     0,
       0,     0,     0,     0,     0,     0,   185,     0,     0,   186,
     187,     0,   188,     0,     0,     0,     0,     0,     0,   189,
       0,     0,     0,   190,   346,     0,    39,   191,     0,   192,
       0,     0,     0,     0,    40,   193,     0,   194,    66,   195,
     196,   671,    68,   197,   198,     0,    42,     0,     0,     0,
       0,     0,    44,    45,    46,   181,   182,   183,     0,   672,
       0,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   184,     0,     0,     0,     0,     0,     0,     0,     0,
     185,     0,     0,   186,   187,     0,   188,     0,     0,     0,
       0,     0,     0,   189,     0,     0,     0,   190,     0,    39,
       0,   191,     0,   192,     0,     0,     0,    40,     0,   193,
       0,   194,    66,   195,   196,     0,    68,   197,   198,    42,
       0,     0,     0,   345,     0,    44,    45,    46,   181,   182,
     183,     0,     0,     0,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   184,     0,     0,     0,     0,     0,
       0,     0,     0,   185,     0,     0,   186,   187,     0,   188,
       0,     0,     0,     0,     0,     0,   189,     0,     0,     0,
     190,     0,    39,     0,   191,     0,   192,     0,     0,     0,
      40,     0,   193,     0,   194,    66,   195,   196,     0,    68,
     197,   198,    42,     0,     0,     0,     0,     0,    44,    45,
      46,   181,   182,   183,     0,     0,     0,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   184,     0,     0,
       0,     0,     0,     0,     0,     0,   185,     0,     0,   186,
     187,     0,   188,     0,     0,     0,     0,     0,     0,   189,
       0,     0,     0,   190,     0,    39,     0,   191,   753,   192,
       0,     0,     0,    40,     0,   193,     0,   194,    66,   195,
     196,     0,    68,   197,   198,    42,     0,     0,     0,   345,
       0,    44,    45,    46,   181,   182,   183,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     184,     0,     0,     0,     0,     0,     0,     0,     0,   185,
       0,     0,   186,   187,     0,   188,     0,     0,     0,     0,
       0,     0,   189,     0,     0,     0,   190,     0,    39,     0,
     769,     0,   192,     0,     0,     0,    40,     0,   193,     0,
     194,    66,   195,   196,     0,    68,   197,   198,    42,     0,
       0,     0,     0,     0,    44,    45,    46,   181,   182,   183,
       0,     0,     0,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   777,   184,     0,     0,     0,     0,     0,     0,
       0,     0,   185,     0,     0,   186,   187,     0,   188,     0,
       0,     0,     0,     0,     0,   189,     0,     0,     0,   190,
       0,    39,     0,   191,     0,   192,     0,     0,     0,    40,
       0,   193,     0,   194,    66,   195,   196,     0,    68,   197,
     198,    42,     0,     0,     0,     0,     0,    44,    45,    46,
     181,   182,   183,     0,     0,     0,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   780,   184,     0,     0,     0,
       0,     0,     0,     0,     0,   185,     0,     0,   186,   187,
       0,   188,     0,     0,     0,     0,     0,     0,   189,     0,
       0,     0,   190,     0,    39,     0,   191,     0,   192,     0,
       0,     0,    40,     0,   193,     0,   194,    66,   195,   196,
       0,    68,   197,   198,    42,     0,     0,     0,     0,     0,
      44,    45,    46,   181,   182,   183,     0,     0,     0,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   184,
       0,     0,     0,     0,     0,     0,     0,     0,   185,     0,
       0,   186,   187,     0,   188,     0,     0,     0,     0,     0,
       0,   189,     0,     0,     0,   190,     0,    39,     0,   191,
       0,   192,     0,     0,     0,    40,     0,   193,     0,   194,
      66,   195,   196,     0,    68,   197,   198,    42,     0,     0,
       0,     0,     0,    44,    45,    46,   181,   182,   183,     0,
       0,     0,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   261,     0,     0,     0,     0,     0,     0,     0,
       0,   185,     0,  -183,     0,   187,     0,   188,     0,     0,
       0,     0,     0,     0,   189,     0,     0,     0,   190,    39,
       0,     0,   191,     0,   192,     0,     0,    40,     0,     0,
     746,     0,   194,    66,     0,   264,     0,    68,     0,    42,
       0,     0,     0,     0,     0,    44,    45,    46,   181,   182,
     183,     0,     0,     0,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   261,     0,     0,     0,     0,     0,
       0,     0,     0,   185,     0,     0,     0,   187,     0,   188,
       0,     0,     0,     0,     0,     0,   189,     0,     0,     0,
     190,    39,     0,     0,   191,     0,   192,     0,     0,    40,
       0,     0,   746,     0,   194,    66,     0,   264,     0,    68,
       0,    42,     0,     0,     0,     0,     0,    44,    45,    46,
     181,   182,   183,     0,     0,     0,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   185,     0,     0,     0,   187,
       0,   188,     0,     0,     0,     0,     0,     0,   189,     0,
       0,     0,   190,    39,     0,     0,   191,     0,   192,     0,
       0,    40,     0,     0,     0,     0,   194,    66,     0,     0,
      41,    68,     0,    42,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,     0,    50,    51,    52,    53,
      39,    54,    55,    56,     0,     0,    57,     0,    40,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
      42,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,     0,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,    39,     0,     0,     0,     0,     0,     0,    64,    40,
       0,     0,     0,     0,   325,     0,     0,     0,    65,    66,
       0,    42,    67,    68,     0,    43,     0,    44,    45,    46,
      47,    48,    49,     0,    50,    51,    52,    53,    39,    54,
      55,    56,     0,     0,    57,    64,    40,     0,    58,    59,
      60,    61,    62,    63,     0,    65,    66,     0,    42,    67,
      68,     0,     0,     0,    44,    45,    46,   181,   182,   183,
       0,     0,     0,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,    39,
       0,     0,     0,     0,     0,     0,    64,    40,     0,     0,
       0,     0,     0,     0,     0,     0,    65,    66,     0,    42,
      67,    68,     0,    43,     0,    44,    45,    46,    47,    48,
      49,     0,    50,    51,    52,    53,    39,    54,    55,    56,
       0,     0,    57,     0,    40,     0,    58,    59,    60,    61,
      62,    63,     0,   194,    66,     0,    42,     0,    68,     0,
      43,     0,    44,    45,    46,    47,    48,    49,     0,    50,
      51,    52,    53,   461,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,    39,     0,     0,     0,     0,
       0,     0,     0,    40,   246,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    65,    42,     0,     0,    67,    43,
       0,    44,    45,    46,    47,    48,    49,     0,    50,    51,
      52,    53,    39,    54,    55,    56,     0,     0,    57,     0,
      40,   246,    58,    59,    60,    61,    62,    63,     0,     0,
       0,    65,    42,     0,     0,    67,    43,     0,    44,    45,
      46,    47,    48,    49,     0,    50,    51,    52,    53,    39,
      54,    55,    56,     0,     0,    57,     0,    40,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,    42,
       0,     0,     0,     0,     0,    44,    45,    46,   181,   182,
     183,     0,     0,     0,    52,    53,     0,    54,    55,    56,
      65,   113,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   339,     0,     0,
       0,     0,    39,     0,     0,     0,     0,    65,     0,     0,
      40,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    42,     0,   593,     0,   594,     0,    44,    45,
      46,   181,   182,   183,   595,    39,     0,    52,    53,     0,
      54,    55,    56,    40,   194,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,    42,     0,     0,     0,     0,
       0,    44,    45,    46,   181,   182,   183,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,    39,     0,   743,     0,   594,
       0,     0,     0,    40,     0,     0,     0,   595,     0,     0,
       0,     0,     0,     0,     0,    42,     0,   194,     0,     0,
       0,    44,    45,    46,   181,   182,   183,     0,    39,     0,
      52,    53,   594,    54,    55,    56,    40,     0,    57,     0,
     595,     0,    58,    59,    60,    61,    62,    63,    42,     0,
     194,     0,     0,     0,    44,    45,    46,   181,   182,   183,
       0,     0,     0,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     595,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     194,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   194
  };

  const short
  parser::yycheck_[] =
  {
      87,   105,    29,    87,   191,   112,   249,   105,   110,   229,
     230,    98,   233,   336,    98,   223,    64,   193,    41,   302,
     200,   110,   111,   101,    98,    34,   206,   167,   106,   107,
      29,   109,   189,   110,   111,    98,    34,   451,   100,   300,
      87,     5,   108,   190,   474,   330,    12,   177,     1,   132,
       1,     1,   295,   699,   190,   191,   134,   111,   524,    82,
     372,    77,    21,   111,   728,    27,    27,   480,     1,    89,
      19,     1,   219,    77,     1,     1,   399,    77,    86,   337,
     263,    89,    90,   219,   191,   192,   175,   699,    22,   263,
      78,    77,   228,    80,   177,   380,    84,   109,   175,   565,
     105,     0,   766,   190,   191,   104,   193,    86,   120,   394,
      89,   131,   124,   200,   130,   102,   116,    51,    52,   206,
     303,   175,    84,    84,   119,   130,   130,   175,   116,   303,
     116,   745,   219,   313,   748,   130,   782,   267,   421,   166,
     167,   228,   101,   109,   456,   102,   233,   234,   578,   233,
     234,   104,   115,   104,   104,   291,   105,   623,    80,   233,
     234,   419,    64,   299,   115,   115,   193,   166,   167,   582,
     782,   104,   784,   109,   104,   109,   254,   104,   104,   257,
     488,   259,   115,   272,   267,   115,   120,    80,   115,   115,
     124,   189,    80,   105,   116,   480,    79,   109,   246,   111,
     336,    80,   102,    14,   291,   169,   170,    80,   120,   111,
      80,   298,   299,   279,   298,   115,    80,   525,    77,   110,
     229,   230,    80,   116,   106,   116,   313,   110,   116,   365,
     132,   229,   230,   116,   116,   558,   559,   116,   316,   122,
     101,   112,   103,   116,   243,   116,   116,   295,   662,   336,
     130,   298,   116,   109,   401,   721,   102,   723,   116,    81,
     110,   727,    84,   399,   120,   401,   116,    77,   124,   115,
     105,   102,   533,   175,   109,   177,   111,   364,   365,   366,
     364,   101,    27,   103,   115,   120,   594,   595,   571,   124,
     364,   339,   366,   292,   293,   425,   374,   120,   764,   308,
     520,   124,   300,   588,   718,   102,    24,   411,   331,   115,
     393,   373,   399,   411,   401,   102,   315,   112,   115,   562,
     422,   116,   424,   102,   609,   427,   110,   429,   115,   469,
     487,   116,   116,   422,   438,   424,   115,   400,   427,   101,
     429,   103,   425,   530,   246,   422,   629,   424,   495,   485,
     427,   110,   429,   540,    79,   491,   112,   116,   112,   495,
     116,   508,   116,    65,    66,   267,    91,   110,    93,    94,
      79,   694,   508,   101,    99,   103,   659,   101,   109,   103,
      89,   115,    91,   604,    93,    94,   109,   523,   696,   101,
      99,   103,   117,    14,   614,   615,   121,   122,   485,   684,
      84,   488,    86,   490,   491,   613,   105,   101,   495,   103,
      80,    81,   121,   122,    80,    81,   125,   126,   131,   130,
     728,   508,   558,   559,    86,    72,    73,    74,    75,    91,
     101,    87,   103,   540,   541,    71,   523,   524,   525,    79,
     438,    81,   469,   101,    84,   621,   110,   474,   110,   111,
     671,    91,   130,    93,    94,   110,   699,   593,   766,    99,
     110,   769,   642,    80,    81,    80,    81,   752,   120,   121,
     469,   558,   559,   110,   576,   474,   110,   117,   565,   110,
       4,   121,   122,   806,    23,   125,   126,   576,   116,   487,
      27,   393,   501,   502,    90,   780,   781,   115,    77,   576,
      77,    77,   580,   501,   502,    77,   593,   594,   595,    15,
      81,   520,   733,   175,   562,   735,    41,   604,   180,   699,
     604,    77,   520,   425,   106,   106,   116,   110,    79,    80,
     604,   130,   110,    77,   621,   533,   623,   110,   102,   782,
      91,   110,    93,    94,   110,   102,   116,    79,    99,    77,
      80,   578,   106,   116,   690,   642,    90,    89,   694,    91,
     110,   737,   110,   110,   116,   745,   117,   110,   748,   110,
     121,   122,   117,    85,   125,   126,   575,   117,   116,   578,
     801,    85,   769,   670,   671,   672,   670,   671,   672,   121,
     122,    25,   728,   125,   126,    27,   670,   671,   672,   775,
      81,   130,   782,   690,   784,   614,   615,   694,   131,   696,
     106,    77,   699,    77,    77,   110,   614,   615,   645,   117,
     117,   797,   709,    77,    80,   709,   116,    80,    27,    81,
     116,   102,   117,   769,   721,   709,   723,   116,    81,   301,
     727,   728,   746,   110,   109,    89,   733,   751,    84,   733,
     737,   738,   739,   751,   738,   739,    80,   106,   745,   733,
     106,   748,   769,    78,   738,   739,   116,   676,    77,   117,
     806,   758,   117,    91,   758,    93,    94,   764,   676,   766,
      28,    99,   769,   710,   758,    27,     9,   110,   775,   102,
     115,    91,    79,    93,    94,   782,   110,   784,    37,    99,
      27,    80,   789,   121,    91,   789,    93,    94,    84,   110,
     797,    80,    99,    11,   801,   789,    19,   801,    86,   806,
      90,   121,    53,    86,    80,   125,   735,   801,   120,   110,
     117,   116,   110,   395,   121,   122,   102,   735,   125,   126,
      84,    84,   102,   115,    79,   166,   123,   797,   739,   726,
     502,    86,   414,   676,    89,   735,    91,   230,   420,    15,
     422,   615,   424,   298,   291,   427,   690,   429,   559,   704,
     789,   639,     4,   786,   525,   110,   697,   219,   745,   487,
      12,   116,   644,    34,   647,   710,   121,   122,   260,   305,
     125,   126,    24,    34,   659,   662,    28,   257,    30,    31,
      32,    33,    34,    35,   221,    37,    38,    39,    40,   658,
      42,    43,    44,   514,   575,    47,   411,   479,   480,    51,
      52,    53,    54,    55,    56,   410,   562,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,   751,    10,    -1,    12,
      13,    -1,    -1,    16,    17,    18,    19,    20,    -1,    22,
     512,    24,    -1,    26,   516,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    -1,   119,   120,    62,
      63,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,   570,    82,
      -1,    -1,    -1,    -1,   576,    88,    -1,    -1,    91,    92,
     582,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   105,    -1,    -1,    -1,   109,    -1,   111,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   119,   120,    -1,    -1,
     123,   124,    -1,    -1,    -1,    -1,   129,   130,   131,   132,
      -1,    -1,   135,    -1,    -1,    -1,   139,   140,   630,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    16,    17,    18,
      -1,    20,    -1,    -1,    -1,    24,   658,    26,   660,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,   731,
      -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,
     109,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     119,   120,    -1,    -1,   123,   124,    -1,    -1,   760,    -1,
     129,   130,   131,   132,    -1,    -1,   135,     3,     4,     5,
     139,   140,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      16,    17,    18,    -1,    20,    -1,    -1,    -1,    24,    -1,
      26,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    -1,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,
      -1,    -1,    -1,   109,    -1,   111,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   119,   120,    -1,    -1,   123,   124,    -1,
      -1,    -1,    -1,   129,   130,   131,   132,    -1,    -1,   135,
       3,     4,     5,   139,   140,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    16,    17,    18,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   105,    -1,    -1,    -1,   109,    -1,   111,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   119,   120,    -1,    -1,
     123,   124,    -1,    -1,    -1,    -1,   129,   130,   131,   132,
      -1,     1,   135,     3,     4,     5,   139,   140,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,    -1,    -1,   109,
      -1,   111,    -1,    -1,    -1,   115,    -1,    -1,    -1,   119,
     120,    -1,    -1,   123,   124,    -1,    -1,    -1,    -1,   129,
     130,   131,   132,    -1,    -1,   135,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    -1,    91,    -1,    93,    94,    -1,    -1,
      -1,    -1,    99,    -1,    -1,    -1,    -1,    -1,   105,    -1,
      -1,    -1,   109,   110,   111,    -1,    -1,    -1,    -1,   116,
     117,    -1,   119,   120,   121,   122,   123,   124,   125,   126,
      -1,    -1,   129,   130,   131,   132,    -1,    -1,   135,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    -1,    93,
      94,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,
      -1,   105,   106,    -1,    -1,   109,    -1,   111,    -1,    -1,
      -1,    -1,    -1,   117,    -1,   119,   120,   121,   122,   123,
     124,   125,   126,    -1,    -1,   129,   130,   131,   132,    -1,
      -1,   135,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,
      91,    -1,    93,    94,    -1,    -1,    -1,    -1,    99,    -1,
      -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,
     111,    -1,    -1,    -1,    -1,    -1,   117,    -1,   119,   120,
     121,   122,   123,   124,   125,   126,    -1,    -1,   129,   130,
     131,   132,    -1,    -1,   135,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   102,    -1,    -1,   105,    -1,    -1,
      -1,   109,    -1,   111,    -1,    -1,    -1,   115,    -1,    -1,
      -1,   119,   120,    -1,    -1,   123,   124,    -1,    -1,    -1,
      -1,   129,   130,   131,   132,    -1,    -1,   135,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    46,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     105,    -1,    -1,    -1,   109,    -1,   111,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   119,   120,    -1,    -1,   123,   124,
      -1,    -1,    -1,    -1,   129,   130,   131,   132,    -1,    -1,
     135,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,   111,
      -1,    -1,    -1,   115,    -1,    -1,    -1,   119,   120,    -1,
      -1,   123,   124,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,    -1,    -1,   135,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,
     109,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     119,   120,    -1,    -1,   123,   124,    -1,    -1,    -1,    -1,
     129,   130,   131,   132,    -1,    -1,   135,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    -1,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,
      -1,    -1,    -1,   109,    -1,   111,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   119,   120,    -1,    -1,   123,   124,    -1,
      -1,    -1,    -1,   129,   130,   131,   132,    -1,    -1,   135,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   105,    -1,    -1,    -1,   109,    -1,   111,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   119,   120,    -1,    -1,
     123,   124,    -1,    -1,    -1,    -1,   129,   130,   131,   132,
      -1,    -1,   135,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,
      -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,
     120,    -1,    -1,   123,   124,    -1,    -1,    -1,    -1,   129,
     130,   131,   132,    -1,    -1,   135,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    86,
      -1,    88,    -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,
      -1,    -1,   109,    -1,   111,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   119,   120,    -1,    -1,   123,   124,    -1,    -1,
      -1,    -1,   129,   130,   131,   132,    -1,    -1,   135,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,    92,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,
     111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,   120,
      -1,    -1,   123,   124,    -1,    -1,    -1,    -1,   129,   130,
     131,   132,    -1,    -1,   135,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,    -1,    -1,    -1,    -1,   105,    -1,    -1,
      -1,   109,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   119,   120,    -1,    -1,   123,   124,    -1,    -1,    -1,
      -1,   129,   130,   131,   132,    -1,    -1,   135,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     105,    -1,    -1,    -1,   109,    -1,   111,    -1,     4,    -1,
      -1,    -1,    -1,    -1,   119,   120,    12,    -1,   123,   124,
      -1,    -1,    -1,    -1,   129,   130,   131,   132,    24,    -1,
     135,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      86,    -1,    88,    89,    -1,    91,    92,    -1,    94,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,
      -1,    -1,    -1,   109,   110,   111,    -1,    -1,    -1,     4,
     116,   117,    -1,   119,   120,   121,   122,    12,   124,   125,
     126,    -1,    -1,    -1,    19,    -1,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    91,    92,    -1,    94,
      -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,
     105,    -1,     4,    -1,   109,    -1,   111,    -1,    -1,    -1,
      12,    -1,   117,    -1,   119,   120,   121,   122,    -1,   124,
     125,   126,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,
      92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,   105,   106,    -1,     4,   109,    -1,   111,
      -1,    -1,    -1,    -1,    12,   117,    -1,   119,   120,   121,
     122,    19,   124,   125,   126,    -1,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    91,    92,    -1,    94,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,    -1,     4,
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
     122,    -1,   124,   125,   126,    24,    -1,    -1,    -1,    28,
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
      -1,    -1,    78,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    -1,    91,    92,    -1,    94,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,
      -1,     4,    -1,   109,    -1,   111,    -1,    -1,    -1,    12,
      -1,   117,    -1,   119,   120,   121,   122,    -1,   124,   125,
     126,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    78,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    92,
      -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,    -1,   105,    -1,     4,    -1,   109,    -1,   111,    -1,
      -1,    -1,    12,    -1,   117,    -1,   119,   120,   121,   122,
      -1,   124,   125,   126,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    -1,    -1,    -1,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      -1,    91,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,    -1,   105,    -1,     4,    -1,   109,
      -1,   111,    -1,    -1,    -1,    12,    -1,   117,    -1,   119,
     120,   121,   122,    -1,   124,   125,   126,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    -1,    92,    -1,    94,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,     4,
      -1,    -1,   109,    -1,   111,    -1,    -1,    12,    -1,    -1,
     117,    -1,   119,   120,    -1,   122,    -1,   124,    -1,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    -1,    92,    -1,    94,
      -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,
     105,     4,    -1,    -1,   109,    -1,   111,    -1,    -1,    12,
      -1,    -1,   117,    -1,   119,   120,    -1,   122,    -1,   124,
      -1,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,    92,
      -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,    -1,   105,     4,    -1,    -1,   109,    -1,   111,    -1,
      -1,    12,    -1,    -1,    -1,    -1,   119,   120,    -1,    -1,
      21,   124,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
       4,    42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,   109,    12,
      -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,   119,   120,
      -1,    24,   123,   124,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,     4,    42,
      43,    44,    -1,    -1,    47,   109,    12,    -1,    51,    52,
      53,    54,    55,    56,    -1,   119,   120,    -1,    24,   123,
     124,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,   109,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   119,   120,    -1,    24,
     123,   124,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,     4,    42,    43,    44,
      -1,    -1,    47,    -1,    12,    -1,    51,    52,    53,    54,
      55,    56,    -1,   119,   120,    -1,    24,    -1,   124,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,    78,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    12,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   119,    24,    -1,    -1,   123,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,     4,    42,    43,    44,    -1,    -1,    47,    -1,
      12,   109,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,   119,    24,    -1,    -1,   123,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,     4,
      42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,
     119,   120,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,    -1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,   119,    -1,    -1,
      12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    24,    -1,    99,    -1,   101,    -1,    30,    31,
      32,    33,    34,    35,   109,     4,    -1,    39,    40,    -1,
      42,    43,    44,    12,   119,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     4,    -1,    99,    -1,   101,
      -1,    -1,    -1,    12,    -1,    -1,    -1,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    24,    -1,   119,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,     4,    -1,
      39,    40,   101,    42,    43,    44,    12,    -1,    47,    -1,
     109,    -1,    51,    52,    53,    54,    55,    56,    24,    -1,
     119,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   119
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   101,   142,   143,   144,   147,   120,   124,   347,
     148,   159,     0,   148,    65,    66,   145,   102,   115,   149,
     160,   161,     1,   104,   346,   105,   130,   211,   211,   109,
     150,    14,   162,   170,   171,   130,   212,    77,    77,     4,
      12,    21,    24,    28,    30,    31,    32,    33,    34,    35,
      37,    38,    39,    40,    42,    43,    44,    47,    51,    52,
      53,    54,    55,    56,   109,   119,   120,   123,   124,   151,
     152,   153,   157,   314,   317,   318,   331,   332,   333,   339,
      27,    24,   163,   115,   158,     3,     5,     6,     7,     8,
      10,    13,    16,    17,    18,    19,    20,    22,    26,    29,
      36,    50,    57,    62,    63,    76,    82,    88,    91,    92,
     105,   109,   111,   120,   124,   129,   130,   131,   132,   135,
     139,   140,   168,   172,   173,   174,   175,   177,   192,   217,
     259,   264,   269,   270,   274,   275,   276,   277,   303,   304,
     307,   308,   330,   331,   333,   341,   342,   345,   106,   116,
     347,    79,    89,    91,    93,    94,    99,   121,   122,   125,
     126,   336,   337,   338,   340,   110,   116,   109,   154,   101,
     103,   146,   347,   115,    63,   109,   267,   268,   270,   271,
     273,    33,    34,    35,    79,    88,    91,    92,    94,   101,
     105,   109,   111,   117,   119,   121,   122,   125,   126,   197,
     219,   220,   224,   226,   228,   229,   231,   233,   235,   312,
     313,   315,   317,   319,   320,   327,   328,   329,   339,   109,
     101,   103,   296,   267,    72,    73,    74,    75,   178,   101,
     103,   208,   209,    19,    37,   176,   226,   228,   313,    14,
     296,   275,   105,   265,   266,   130,   109,   331,   105,   109,
     305,   306,   308,   342,    92,   275,   294,   295,   275,   274,
     275,    79,   106,   117,   122,   126,   267,   268,   278,   280,
     281,   310,   324,   326,   335,   336,   338,   343,   344,    91,
     110,   116,   278,   279,   336,   337,   338,   343,   348,   112,
     348,    19,   265,   265,   131,   167,   158,    71,   198,    80,
     116,    81,    84,   117,   261,   262,   263,   310,   323,   325,
     334,   336,   337,   100,   275,   101,    87,   130,   110,   110,
     110,   110,   110,   110,   153,    78,   155,   156,   157,   148,
     148,     4,   164,   119,   130,    23,    80,   323,   267,   109,
     217,   252,   253,   254,   333,    28,   106,   221,   222,   224,
     226,    86,    89,   110,   221,   239,   319,   348,   348,   317,
     329,    27,   202,   235,    90,    86,    89,   232,   235,   221,
     238,   239,    20,    46,    92,   267,   293,   297,   298,   299,
     297,   115,   272,    77,    77,    77,    77,   215,   222,   236,
     207,   259,   260,   269,   207,    15,   187,   226,   226,    80,
     116,    81,    41,    89,   131,   331,    77,    77,   130,   344,
      80,   116,   213,   275,    86,   294,   261,   332,   341,   323,
      78,    84,   116,   106,   116,   268,   110,   116,   110,   116,
     110,   110,   110,   116,   112,   236,   331,   331,   117,   169,
     309,   321,   322,   337,   344,   130,   197,   216,   222,   223,
     330,   267,   283,   284,   299,   332,    27,   210,   263,   270,
     235,    78,   300,   301,   302,   331,   275,   110,   110,   116,
     102,   346,   347,    12,   109,   165,   166,    77,    77,   101,
     103,   285,   215,   271,   337,    80,   102,   116,   240,   106,
      80,    90,   110,   110,   110,   116,   110,   110,   112,   117,
     117,   101,   103,   201,   226,   222,   228,   110,   116,   209,
     296,   275,    85,   102,   115,   346,    25,    27,   206,   102,
     115,   346,   267,    81,    80,    81,   195,   214,   215,   222,
     109,   313,   221,   130,   131,   106,    77,    77,   110,   105,
     109,   111,   311,   312,   305,    77,   267,   117,   117,   267,
     282,   299,   267,   278,   278,   278,   278,    77,    80,    80,
     333,   342,   116,    77,   130,    80,    81,   193,   247,   210,
      81,   116,   117,   209,   102,   116,    81,   157,   109,   151,
      92,   102,   115,   267,   286,   287,   288,   292,   286,   346,
     110,   222,   254,    99,   101,   109,   241,   242,   327,   222,
     243,   222,   221,     7,    26,   188,   199,   200,   260,   200,
     221,   267,   298,   267,   101,   103,   205,   260,   222,   243,
     241,    84,   181,    80,   319,   330,   106,   110,   112,   116,
      78,   215,   218,   218,   117,   117,   321,    77,   243,    28,
     248,   249,   250,    27,   244,     9,   255,   256,   257,   267,
     299,   301,   278,   151,   110,   275,   286,   102,   115,    84,
      86,   289,   290,   291,   346,   222,   327,   327,   110,    37,
     189,    19,    37,   187,   226,   102,   115,   346,   272,    26,
     191,   203,   204,   260,   204,   182,   329,    27,   184,   243,
      80,   299,   267,    77,   116,    77,   240,    84,   225,   230,
     234,   235,   251,   101,   103,   255,    22,    51,    52,   109,
     179,   258,   316,   317,   257,   110,   288,   283,   267,   210,
     291,    80,   102,    80,   226,   187,   226,    80,    81,   196,
     199,    11,    19,   190,   102,   115,   346,    86,   101,   103,
     185,   216,   215,    99,   249,    90,   117,   234,   309,   245,
     246,   272,   245,   110,   226,   227,   237,   258,    53,   180,
      86,   210,   243,   243,    80,   194,    81,   196,   243,   109,
     242,   327,   267,   187,   203,   183,   329,    78,   186,   187,
      78,   186,   230,   251,   230,   102,   115,   306,   346,   116,
     110,   226,   267,   102,   110,   243,   327,    84,   329,   102,
     102,   115,   115,   346,   346,   246,    80,   237,   182,   187,
     215
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
     172,   172,   172,   172,   172,   172,   172,   173,   174,   174,
     174,   174,   175,   176,   176,   177,   177,   178,   178,   178,
     178,   178,   179,   179,   179,   180,   181,   181,   182,   183,
     183,   184,   184,   185,   185,   185,   185,   186,   186,   186,
     186,   187,   188,   188,   188,   188,   188,   189,   189,   190,
     190,   191,   192,   192,   193,   193,   194,   194,   195,   195,
     195,   196,   196,   196,   197,   197,   198,   198,   198,   199,
     199,   200,   200,   200,   200,   201,   201,   202,   202,   203,
     203,   204,   204,   204,   204,   205,   205,   206,   206,   207,
     207,   207,   207,   208,   208,   209,   210,   210,   211,   211,
     212,   212,   212,   213,   213,   214,   214,   215,   216,   217,
     217,   218,   218,   219,   220,   220,   221,   221,   222,   222,
     222,   223,   224,   225,   226,   226,   227,   228,   229,   229,
     230,   230,   231,   231,   231,   232,   233,   233,   234,   235,
     235,   235,   235,   235,   235,   235,   235,   235,   236,   237,
     237,   238,   238,   239,   239,   240,   240,   241,   241,   241,
     242,   242,   243,   244,   244,   244,   245,   245,   246,   247,
     248,   248,   249,   249,   250,   250,   251,   251,   252,   252,
     253,   253,   254,   255,   255,   256,   256,   257,   257,   257,
     258,   258,   258,   259,   259,   259,   260,   261,   261,   262,
     262,   263,   264,   264,   264,   264,   264,   264,   264,   264,
     264,   265,   265,   266,   266,   267,   267,   268,   268,   269,
     269,   270,   270,   270,   271,   271,   272,   272,   273,   273,
     274,   274,   274,   274,   275,   275,   275,   275,   275,   275,
     275,   275,   275,   276,   276,   277,   277,   277,   277,   277,
     277,   277,   278,   278,   278,   279,   279,   280,   280,   280,
     280,   280,   280,   280,   281,   281,   282,   282,   283,   284,
     284,   285,   285,   285,   285,   286,   286,   287,   287,   287,
     288,   289,   289,   290,   290,   291,   292,   292,   293,   293,
     294,   294,   295,   295,   296,   296,   297,   297,   297,   297,
     298,   298,   299,   299,   299,   300,   300,   301,   301,   301,
     302,   302,   303,   303,   304,   304,   305,   305,   305,   306,
     306,   307,   307,   307,   307,   308,   308,   309,   309,   310,
     310,   311,   311,   311,   312,   312,   312,   312,   312,   313,
     313,   313,   314,   314,   314,   314,   314,   315,   315,   316,
     317,   317,   318,   319,   319,   319,   320,   320,   320,   320,
     321,   321,   322,   322,   323,   323,   324,   324,   325,   325,
     326,   326,   327,   328,   329,   329,   329,   329,   329,   330,
     330,   331,   331,   331,   332,   332,   333,   333,   333,   333,
     333,   333,   333,   333,   334,   334,   335,   335,   336,   337,
     337,   338,   338,   339,   339,   339,   339,   339,   339,   339,
     339,   339,   339,   339,   339,   339,   339,   339,   339,   339,
     339,   340,   340,   340,   341,   341,   342,   343,   343,   344,
     344,   345,   345,   345,   345,   345,   346,   346,   347,   347,
     348,   348
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
       1,     1,     1,     4,     7,     1,     1,     3,     4,     5,
       6,     6,     4,     3,     1,     4,     3,     2,     2,     2,
       2,     0,     1,     1,     1,     2,     0,     2,     3,     2,
       1,     0,     2,     3,     3,     3,     3,     3,     2,     1,
       0,     3,     4,     3,     4,     2,     3,     0,     1,     0,
       1,     3,     1,     1,     0,     2,     0,     2,     0,     2,
       2,     0,     2,     4,     3,     1,     4,     3,     0,     1,
       1,     3,     2,     1,     0,     3,     3,     2,     0,     1,
       1,     3,     2,     1,     0,     3,     3,     2,     0,     3,
       2,     1,     0,     3,     3,     1,     2,     0,     1,     3,
       3,     1,     0,     0,     2,     1,     3,     1,     1,     3,
       1,     1,     3,     1,     1,     1,     1,     3,     4,     3,
       1,     1,     1,     1,     1,     3,     1,     1,     1,     3,
       1,     2,     1,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     2,     3,     2,     5,     3,     3,     1,     1,
       3,     1,     0,     1,     3,     2,     0,     1,     3,     5,
       1,     5,     1,     4,     4,     0,     3,     1,     4,     2,
       3,     1,     4,     2,     3,     0,     1,     3,     0,     1,
       3,     1,     3,     0,     1,     2,     1,     2,     3,     3,
       1,     2,     3,     1,     3,     2,     1,     3,     2,     2,
       1,     4,     3,     3,     4,     4,     3,     4,     6,     6,
       4,     0,     1,     3,     4,     3,     1,     1,     3,     1,
       3,     2,     3,     1,     1,     2,     1,     0,     3,     3,
       2,     3,     2,     1,     3,     2,     4,     4,     8,     4,
       2,     2,     1,     4,     1,     1,     1,     1,     3,     3,
       3,     1,     1,     2,     2,     3,     3,     1,     1,     2,
       4,     3,     5,     3,     3,     3,     3,     1,     1,     3,
       1,     3,     3,     2,     2,     1,     2,     3,     2,     1,
       2,     3,     2,     2,     1,     4,     1,     2,     1,     2,
       1,     2,     2,     1,     3,     3,     3,     2,     1,     0,
       1,     2,     3,     1,     2,     1,     0,     3,     1,     1,
       3,     1,     1,     1,     1,     3,     1,     3,     1,     3,
       1,     2,     3,     2,     3,     1,     2,     1,     3,     1,
       3,     1,     2,     2,     1,     3,     3,     3,     2,     1,
       3,     3,     1,     3,     3,     3,     3,     1,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     1,     1,     1,     1,     1,     3,
       1,     3,     1,     3,     1,     1,     1,     1,     1,     1,
       3,     1,     3,     3,     1,     1,     1,     1,     1,     1,
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
  "cl_decl", "ty_decl", "standalone_kind_sig", "sks_vars", "inst_decl",
  "overlap_pragma", "deriv_strategy_no_via", "deriv_strategy_via",
  "opt_injective_info", "injectivity_cond", "inj_varids",
  "where_type_family", "ty_fam_inst_eqn_list", "ty_fam_inst_eqns",
  "ty_fam_inst_eqn", "at_decl_cls", "opt_family", "opt_instance",
  "at_decl_inst", "data_or_newtype", "opt_kind_sig",
  "opt_datafam_kind_sig", "opt_tyfam_kind_sig", "opt_at_kind_inj_sig",
  "tycl_hdr", "capi_ctype", "decl_cls", "decls_cls", "decllist_cls",
  "where_cls", "decl_inst", "decls_inst", "decllist_inst", "where_inst",
  "decls", "decllist", "binds", "wherebinds", "strings", "stringlist",
  "opt_tyconsig", "sigktype", "sigtype", "sigtypedoc", "sig_vars",
  "sigtypes1", "strict_mark", "strictness", "ktype", "ctype", "ctypedoc",
  "context", "context_no_ops", "type", "typedoc", "btype", "infixtype",
  "btype_no_ops", "ftype", "tyarg", "tyop", "atype_docs", "atype",
  "inst_type", "deriv_types", "comma_types0", "comma_types1", "tv_bndrs",
  "tv_bndr", "tv_bndr_no_braces", "kind", "gadt_constrlist",
  "gadt_constrs", "gadt_constr", "constrs", "constrs1", "constr", "forall",
  "constr_stuff", "fielddecls", "fielddecls1", "fielddecl",
  "maybe_derivings", "derivings", "deriving", "deriv_clause_types",
  "decl_no_th", "decl", "rhs", "gdrhs", "gdrh", "sigdecl", "activation",
  "explicit_activation", "exp", "infixexp", "infixexp_top", "exp10_top",
  "exp10", "optSemi", "scc_annot", "fexp", "aexp", "aexp1", "aexp2",
  "texp", "tup_exprs", "list", "lexps", "squals", "guardquals",
  "guardquals1", "altslist", "alts", "alts1", "alt", "alt_rhs", "gdpats",
  "gdpat", "pat", "bindpat", "apat", "apats1", "stmtlist", "stmts", "stmt",
  "qual", "fbinds", "fbinds1", "fbind", "qcon", "gen_qcon", "con",
  "con_list", "sysdcon_no_list", "sysdcon", "conop", "qconop", "gtycon",
  "ntgtycon", "oqtycon", "oqtycon_no_varcon", "qtyconop", "qtycondoc",
  "qtycon", "tycon", "qtyconsym", "tyconsym", "op", "varop", "qop", "qopm",
  "qvarop", "qvaropm", "tyvar", "tyvarop", "tyvarid", "var", "qvar",
  "qvarid", "varid", "qvarsym", "qvarsym_no_minus", "qvarsym1", "varsym",
  "varsym_no_minus", "special_id", "special_sym", "qconid", "conid",
  "qconsym", "consym", "literal", "close", "modid", "commas", YY_NULLPTR
  };
#endif


#if YYDEBUG
  const short
  parser::yyrline_[] =
  {
       0,   497,   497,   514,   515,   517,   521,   522,   523,   525,
     526,   528,   529,   532,   534,   535,   536,   544,   545,   547,
     549,   550,   552,   553,   555,   556,   557,   559,   560,   562,
     563,   565,   566,   570,   571,   573,   574,   576,   578,   579,
     581,   594,   595,   597,   598,   600,   601,   605,   606,   611,
     612,   614,   615,   616,   618,   619,   623,   625,   626,   628,
     629,   630,   631,   634,   635,   642,   644,   646,   648,   649,
     651,   652,   655,   657,   658,   661,   662,   666,   667,   668,
     669,   670,   672,   673,   674,   676,   687,   688,   690,   692,
     693,   697,   698,   700,   701,   702,   703,   705,   706,   707,
     708,   710,   713,   715,   717,   719,   720,   722,   722,   724,
     724,   728,   730,   731,   735,   736,   738,   739,   741,   742,
     743,   745,   746,   747,   751,   752,   754,   755,   756,   798,
     799,   801,   802,   803,   804,   806,   807,   809,   810,   812,
     813,   815,   816,   817,   818,   820,   821,   823,   824,   827,
     828,   829,   830,   832,   833,   835,   837,   838,   846,   847,
     849,   850,   851,   864,   865,   867,   868,   872,   874,   876,
     877,   879,   880,   884,   890,   891,   898,   899,   901,   902,
     904,   906,   915,   917,   919,   920,   922,   925,   927,   928,
     930,   931,   933,   934,   935,   937,   939,   940,   947,   954,
     955,   956,   957,   958,   959,   960,   966,   967,   970,   972,
     973,   975,   976,   978,   979,   986,   987,   989,   990,   991,
     994,   995,  1013,  1019,  1020,  1021,  1023,  1024,  1026,  1028,
    1030,  1031,  1033,  1034,  1036,  1037,  1039,  1040,  1042,  1043,
    1045,  1046,  1048,  1050,  1051,  1053,  1054,  1056,  1057,  1058,
    1060,  1061,  1062,  1067,  1069,  1070,  1072,  1076,  1077,  1079,
    1080,  1084,  1094,  1095,  1097,  1098,  1099,  1100,  1101,  1102,
    1103,  1106,  1107,  1109,  1110,  1115,  1116,  1120,  1121,  1123,
    1124,  1126,  1127,  1128,  1131,  1132,  1135,  1136,  1138,  1139,
    1144,  1145,  1146,  1147,  1150,  1151,  1152,  1153,  1155,  1157,
    1158,  1159,  1161,  1164,  1165,  1168,  1169,  1170,  1171,  1172,
    1177,  1178,  1184,  1185,  1186,  1191,  1192,  1210,  1211,  1212,
    1213,  1214,  1215,  1216,  1218,  1219,  1232,  1234,  1244,  1246,
    1247,  1250,  1251,  1252,  1253,  1255,  1256,  1258,  1259,  1260,
    1262,  1264,  1265,  1267,  1268,  1277,  1279,  1280,  1282,  1283,
    1285,  1286,  1288,  1289,  1292,  1293,  1295,  1296,  1297,  1298,
    1303,  1304,  1306,  1307,  1308,  1313,  1314,  1316,  1317,  1318,
    1320,  1321,  1353,  1354,  1356,  1357,  1359,  1360,  1361,  1363,
    1364,  1366,  1367,  1368,  1369,  1371,  1372,  1374,  1375,  1377,
    1378,  1381,  1382,  1383,  1385,  1386,  1387,  1388,  1389,  1391,
    1392,  1393,  1395,  1396,  1397,  1398,  1399,  1402,  1403,  1405,
    1407,  1408,  1412,  1414,  1415,  1416,  1418,  1419,  1420,  1421,
    1426,  1427,  1429,  1430,  1432,  1433,  1436,  1437,  1442,  1443,
    1445,  1446,  1450,  1452,  1454,  1455,  1456,  1457,  1458,  1461,
    1462,  1464,  1465,  1466,  1468,  1469,  1471,  1472,  1473,  1474,
    1475,  1476,  1477,  1478,  1480,  1481,  1483,  1484,  1486,  1488,
    1489,  1491,  1492,  1494,  1495,  1496,  1497,  1498,  1499,  1500,
    1501,  1502,  1503,  1504,  1505,  1506,  1507,  1508,  1509,  1510,
    1511,  1513,  1514,  1515,  1519,  1520,  1522,  1524,  1525,  1527,
    1528,  1532,  1533,  1534,  1535,  1536,  1541,  1544,  1548,  1549,
    1551,  1552
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
#line 6871 "parser.cc"

#line 1561 "parser.y"


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

Hs::TypeFamilyDecl make_type_family(const Located<Hs::Type>& lhs_type, const std::optional<Located<Hs::Kind>>& kind_sig)
{
    auto [head, args] = Hs::decompose_type_apps(lhs_type.value());

    // Get type con
    auto con = head.to<Hs::TypeCon>();
    if (not con)
        throw myexception()<<"Type family '"<<lhs_type.print()<<"' does not begin with a type constructor.";

    // Get args as type vars
    std::vector<Hs::TypeVar> tyvars;
    for(auto arg: args)
    {
        std::optional<Hs::Kind> kind;
        if (auto ktype = arg.to<Hs::TypeOfKind>())
        {
            arg = ktype->type;
            kind = ktype->kind;
        }

        if (auto tyvar = arg.to<Hs::TypeVar>())
        {
            auto tv = *tyvar;
            tv.kind = kind;
            tyvars.push_back(tv);
        }
        else
            throw myexception()<<"Type family '"<<lhs_type.print()<<"' argument '"<<arg.print()<<"' is not a type variable.";
    }

    std::optional<Hs::Kind> kind;
    if (kind_sig)
        kind = kind_sig->value();

    return Hs::TypeFamilyDecl(*con, tyvars, kind);
}

Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context&  context,
                                           const Hs::Type& header, const std::optional<Hs::Kind>& k, const Hs::ConstructorsDecl& constrs)
{
    auto [name, type_args] = check_type_or_class_header(header);
    if (d_or_n == Hs::DataOrNewtype::newtype and constrs.size() != 1)
        throw myexception()<<"newtype '"<<name<<"' may only have 1 constructors with 1 field";
    return {d_or_n, name, check_all_type_vars(type_args), context, k, constrs};
}

Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context&  context,
                                           const Hs::Type& header, const std::optional<Hs::Kind>& k, const std::optional<Hs::GADTConstructorsDecl>& constrs)
{
    auto [name, type_args] = check_type_or_class_header(header);
    if (d_or_n == Hs::DataOrNewtype::newtype)
    {
        if (not constrs or constrs->size() != 1 or (*constrs)[0].con_names.size() != 1)
            throw myexception()<<"newtype '"<<name<<"' may only have 1 constructors with 1 field";
    }

    if (not constrs)
        return {d_or_n, name, check_all_type_vars(type_args), context, k};
    else
        return {d_or_n, name, check_all_type_vars(type_args), context, k, *constrs};
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

Hs::ConstructorDecl make_constructor(const vector<Hs::TypeVar>& forall, const std::optional<Hs::Context>& c, const Hs::Type& typeish)
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
