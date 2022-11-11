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
#line 487 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2241 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 504 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < std::string > (),yystack_[2].value.as < std::optional<std::vector<Hs::Export>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2247 "parser.cc"
    break;

  case 4: // module: body2
#line 505 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{"Main",{},yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2253 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 507 "parser.y"
                                                                 {drv.push_module_context();}
#line 2259 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 515 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2265 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 516 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2271 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 518 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2277 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 519 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2283 "parser.cc"
    break;

  case 13: // top: semis top1
#line 522 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2289 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 524 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2295 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 525 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2301 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 526 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::ImpDecl> > (),{});}
#line 2307 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 534 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::Export>> > () = yystack_[1].value.as < std::vector<Hs::Export> > ();}
#line 2313 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 535 "parser.y"
                                      {}
#line 2319 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 537 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > () = yystack_[0].value.as < std::vector<Hs::Export> > ();}
#line 2325 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 539 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > () = yystack_[2].value.as < std::vector<Hs::Export> > (); yylhs.value.as < std::vector<Hs::Export> > ().push_back(yystack_[0].value.as < Hs::Export > ());}
#line 2331 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 540 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > ().push_back(yystack_[0].value.as < Hs::Export > ());}
#line 2337 "parser.cc"
    break;

  case 22: // export: qcname export_subspec
#line 542 "parser.y"
                                      {yylhs.value.as < Hs::Export > () = Hs::ExportSymbol{yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}; }
#line 2343 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 543 "parser.y"
                                      {yylhs.value.as < Hs::Export > () = Hs::ExportModule{{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2349 "parser.cc"
    break;

  case 24: // export_subspec: %empty
#line 545 "parser.y"
                                      {}
#line 2355 "parser.cc"
    break;

  case 25: // export_subspec: "(" qcnames ")"
#line 546 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpecSome{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2361 "parser.cc"
    break;

  case 26: // export_subspec: "(" ".." ")"
#line 547 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpecAll(); }
#line 2367 "parser.cc"
    break;

  case 27: // qcnames: %empty
#line 549 "parser.y"
                   {}
#line 2373 "parser.cc"
    break;

  case 28: // qcnames: qcnames1
#line 550 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2379 "parser.cc"
    break;

  case 29: // qcnames1: qcnames1 "," qcname
#line 552 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2385 "parser.cc"
    break;

  case 30: // qcnames1: qcname
#line 553 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2391 "parser.cc"
    break;

  case 31: // qcname: qvar
#line 555 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2397 "parser.cc"
    break;

  case 32: // qcname: oqtycon_no_varcon
#line 556 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2403 "parser.cc"
    break;

  case 37: // importdecls: importdecls_semi importdecl
#line 566 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[1].value.as < std::vector<Hs::ImpDecl> > (), yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[0].value.as < Hs::ImpDecl > ()); }
#line 2409 "parser.cc"
    break;

  case 38: // importdecls_semi: importdecls_semi importdecl semis1
#line 568 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[2].value.as < std::vector<Hs::ImpDecl> > (); yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[1].value.as < Hs::ImpDecl > ()); }
#line 2415 "parser.cc"
    break;

  case 39: // importdecls_semi: %empty
#line 569 "parser.y"
                         { }
#line 2421 "parser.cc"
    break;

  case 40: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 571 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::ImpDecl > () = Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < std::string > (),yystack_[1].value.as < std::optional<std::string> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ());
}
#line 2429 "parser.cc"
    break;

  case 41: // optqualified: "qualified"
#line 584 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2435 "parser.cc"
    break;

  case 42: // optqualified: %empty
#line 585 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2441 "parser.cc"
    break;

  case 43: // maybeas: "as" modid
#line 587 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 2447 "parser.cc"
    break;

  case 44: // maybeas: %empty
#line 588 "parser.y"
                               { }
#line 2453 "parser.cc"
    break;

  case 45: // maybeimpspec: impspec
#line 590 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2459 "parser.cc"
    break;

  case 46: // maybeimpspec: %empty
#line 591 "parser.y"
                               { }
#line 2465 "parser.cc"
    break;

  case 47: // impspec: "(" exportlist ")"
#line 595 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::Export> > ()}; }
#line 2471 "parser.cc"
    break;

  case 48: // impspec: "hiding" "(" exportlist ")"
#line 596 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::Export> > ()}; }
#line 2477 "parser.cc"
    break;

  case 49: // prec: %empty
#line 601 "parser.y"
                   { }
#line 2483 "parser.cc"
    break;

  case 50: // prec: "INTEGER"
#line 602 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < int > (); }
#line 2489 "parser.cc"
    break;

  case 51: // infix: "infix"
#line 604 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2495 "parser.cc"
    break;

  case 52: // infix: "infixl"
#line 605 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2501 "parser.cc"
    break;

  case 53: // infix: "infixr"
#line 606 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2507 "parser.cc"
    break;

  case 54: // ops: ops "," op
#line 608 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 2513 "parser.cc"
    break;

  case 55: // ops: op
#line 609 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 2519 "parser.cc"
    break;

  case 56: // topdecls: topdecls_semi topdecl
#line 613 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 2525 "parser.cc"
    break;

  case 57: // topdecls_semi: topdecls_semi topdecl semis1
#line 615 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2531 "parser.cc"
    break;

  case 58: // topdecls_semi: %empty
#line 616 "parser.y"
                                            { }
#line 2537 "parser.cc"
    break;

  case 59: // topdecl: cl_decl
#line 618 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2543 "parser.cc"
    break;

  case 60: // topdecl: ty_decl
#line 619 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2549 "parser.cc"
    break;

  case 61: // topdecl: standalone_kind_sig
#line 620 "parser.y"
                                               {}
#line 2555 "parser.cc"
    break;

  case 62: // topdecl: inst_decl
#line 621 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2561 "parser.cc"
    break;

  case 63: // topdecl: "default" "(" comma_types0 ")"
#line 624 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Hs::DefaultDecl(yystack_[1].value.as < std::vector<Hs::Type> > ()); }
#line 2567 "parser.cc"
    break;

  case 64: // topdecl: "foreign" "import" "bpcall" "STRING" var "::" sigtypedoc
#line 625 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = Hs::ForeignDecl(yystack_[3].value.as < std::string > (), yystack_[2].value.as < std::string > (), yystack_[0].value.as < Hs::Type > ());}
#line 2573 "parser.cc"
    break;

  case 65: // topdecl: decl_no_th
#line 632 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2579 "parser.cc"
    break;

  case 66: // topdecl: infixexp_top
#line 634 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < Hs::InfixExp > ();}
#line 2585 "parser.cc"
    break;

  case 67: // cl_decl: "class" tycl_hdr where_cls
#line 636 "parser.y"
                                              {yylhs.value.as < expression_ref > () = make_class_decl(yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().first,yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().second,yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 2591 "parser.cc"
    break;

  case 68: // ty_decl: "type" type "=" ctypedoc
#line 638 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_type_synonym({yystack_[2].location, yystack_[2].value.as < Hs::Type > ()},{yystack_[0].location, yystack_[0].value.as < Hs::Type > ()});}
#line 2597 "parser.cc"
    break;

  case 69: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 639 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::Type> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::Type> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > ());}
#line 2603 "parser.cc"
    break;

  case 70: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 641 "parser.y"
                                                                                          {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::Type> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::Type> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < std::optional<Hs::GADTConstructorsDecl> > ());}
#line 2609 "parser.cc"
    break;

  case 71: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 642 "parser.y"
                                                                                          {}
#line 2615 "parser.cc"
    break;

  case 75: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 651 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_instance_decl({yystack_[1].location,yystack_[1].value.as < Hs::Type > ()},yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 2621 "parser.cc"
    break;

  case 76: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 652 "parser.y"
                                                                           {}
#line 2627 "parser.cc"
    break;

  case 112: // data_or_newtype: "data"
#line 719 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 2633 "parser.cc"
    break;

  case 113: // data_or_newtype: "newtype"
#line 720 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 2639 "parser.cc"
    break;

  case 114: // opt_kind_sig: %empty
#line 724 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 2645 "parser.cc"
    break;

  case 115: // opt_kind_sig: "::" kind
#line 725 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < expression_ref > ();}
#line 2651 "parser.cc"
    break;

  case 124: // tycl_hdr: context "=>" type
#line 740 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::Type> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::Type > ()};}
#line 2657 "parser.cc"
    break;

  case 125: // tycl_hdr: type
#line 741 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::Type> > () = {{},yystack_[0].value.as < Hs::Type > ()};}
#line 2663 "parser.cc"
    break;

  case 129: // decl_cls: at_decl_cls
#line 787 "parser.y"
                        {}
#line 2669 "parser.cc"
    break;

  case 130: // decl_cls: decl
#line 788 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2675 "parser.cc"
    break;

  case 131: // decls_cls: decls_cls ";" decl_cls
#line 790 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2681 "parser.cc"
    break;

  case 132: // decls_cls: decls_cls ";"
#line 791 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 2687 "parser.cc"
    break;

  case 133: // decls_cls: decl_cls
#line 792 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2693 "parser.cc"
    break;

  case 134: // decls_cls: %empty
#line 793 "parser.y"
                                           {}
#line 2699 "parser.cc"
    break;

  case 135: // decllist_cls: "{" decls_cls "}"
#line 795 "parser.y"
                                           {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[1].location,{yystack_[1].value.as < Hs::Decls > ()}};}
#line 2705 "parser.cc"
    break;

  case 136: // decllist_cls: "vocurly" decls_cls close
#line 796 "parser.y"
                                           {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[1].location,{yystack_[1].value.as < Hs::Decls > ()}};}
#line 2711 "parser.cc"
    break;

  case 137: // where_cls: "where" decllist_cls
#line 798 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 2717 "parser.cc"
    break;

  case 138: // where_cls: %empty
#line 799 "parser.y"
                                           {}
#line 2723 "parser.cc"
    break;

  case 139: // decl_inst: at_decl_inst
#line 801 "parser.y"
                                           {}
#line 2729 "parser.cc"
    break;

  case 140: // decl_inst: decl
#line 802 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2735 "parser.cc"
    break;

  case 141: // decls_inst: decls_inst ";" decl_inst
#line 804 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2741 "parser.cc"
    break;

  case 142: // decls_inst: decls_inst ";"
#line 805 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 2747 "parser.cc"
    break;

  case 143: // decls_inst: decl_inst
#line 806 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2753 "parser.cc"
    break;

  case 144: // decls_inst: %empty
#line 807 "parser.y"
                                           {}
#line 2759 "parser.cc"
    break;

  case 145: // decllist_inst: "{" decls_inst "}"
#line 809 "parser.y"
                                           {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[1].location,{yystack_[1].value.as < Hs::Decls > ()}};}
#line 2765 "parser.cc"
    break;

  case 146: // decllist_inst: "vocurly" decls_inst close
#line 810 "parser.y"
                                           {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[1].location,{yystack_[1].value.as < Hs::Decls > ()}};}
#line 2771 "parser.cc"
    break;

  case 147: // where_inst: "where" decllist_inst
#line 812 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 2777 "parser.cc"
    break;

  case 148: // where_inst: %empty
#line 813 "parser.y"
                                           {}
#line 2783 "parser.cc"
    break;

  case 149: // decls: decls ";" decl
#line 816 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2789 "parser.cc"
    break;

  case 150: // decls: decls ";"
#line 817 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2795 "parser.cc"
    break;

  case 151: // decls: decl
#line 818 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2801 "parser.cc"
    break;

  case 152: // decls: %empty
#line 819 "parser.y"
                        {}
#line 2807 "parser.cc"
    break;

  case 153: // decllist: "{" decls "}"
#line 821 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2813 "parser.cc"
    break;

  case 154: // decllist: "vocurly" decls close
#line 822 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2819 "parser.cc"
    break;

  case 155: // binds: decllist
#line 824 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 2825 "parser.cc"
    break;

  case 156: // wherebinds: "where" binds
#line 826 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 2831 "parser.cc"
    break;

  case 157: // wherebinds: %empty
#line 827 "parser.y"
                                 {}
#line 2837 "parser.cc"
    break;

  case 163: // opt_tyconsig: %empty
#line 853 "parser.y"
                                 {}
#line 2843 "parser.cc"
    break;

  case 164: // opt_tyconsig: "::" gtycon
#line 854 "parser.y"
                                 {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 2849 "parser.cc"
    break;

  case 167: // sigtype: ctype
#line 859 "parser.y"
         { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2855 "parser.cc"
    break;

  case 168: // sigtypedoc: ctypedoc
#line 861 "parser.y"
            { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2861 "parser.cc"
    break;

  case 169: // sig_vars: sig_vars "," var
#line 863 "parser.y"
                           {yylhs.value.as < std::vector<Hs::Var> > () = yystack_[2].value.as < std::vector<Hs::Var> > (); yylhs.value.as < std::vector<Hs::Var> > ().push_back(Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}));}
#line 2867 "parser.cc"
    break;

  case 170: // sig_vars: var
#line 864 "parser.y"
                           {yylhs.value.as < std::vector<Hs::Var> > ().push_back(Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}));}
#line 2873 "parser.cc"
    break;

  case 171: // sigtypes1: sigtype
#line 866 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2879 "parser.cc"
    break;

  case 172: // sigtypes1: sigtypes1 "," sigtype
#line 867 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[2].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2885 "parser.cc"
    break;

  case 173: // strict_mark: strictness
#line 871 "parser.y"
             { yylhs.value.as < Hs::StrictLazy > () = yystack_[0].value.as < Hs::StrictLazy > (); }
#line 2891 "parser.cc"
    break;

  case 174: // strictness: PREFIX_BANG
#line 877 "parser.y"
                         {yylhs.value.as < Hs::StrictLazy > () = Hs::StrictLazy::strict;}
#line 2897 "parser.cc"
    break;

  case 175: // strictness: PREFIX_TILDE
#line 878 "parser.y"
                         {yylhs.value.as < Hs::StrictLazy > () = Hs::StrictLazy::lazy;}
#line 2903 "parser.cc"
    break;

  case 176: // ctype: "forall" tv_bndrs "." ctype
#line 885 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::ForallType(yystack_[2].value.as < std::vector<Hs::TypeVar> > (), yystack_[0].value.as < Hs::Type > ());}
#line 2909 "parser.cc"
    break;

  case 177: // ctype: context "=>" ctype
#line 886 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::Type > ());}
#line 2915 "parser.cc"
    break;

  case 178: // ctype: type
#line 888 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2921 "parser.cc"
    break;

  case 179: // ctypedoc: ctype
#line 890 "parser.y"
          { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2927 "parser.cc"
    break;

  case 180: // context: btype
#line 899 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::Type > ());}
#line 2933 "parser.cc"
    break;

  case 181: // context_no_ops: btype_no_ops
#line 901 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ()));}
#line 2939 "parser.cc"
    break;

  case 182: // type: btype
#line 903 "parser.y"
      { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2945 "parser.cc"
    break;

  case 183: // type: btype "->" ctype
#line 904 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,"->"}),yystack_[2].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()});}
#line 2951 "parser.cc"
    break;

  case 184: // typedoc: type
#line 906 "parser.y"
         { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2957 "parser.cc"
    break;

  case 185: // btype: infixtype
#line 909 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2963 "parser.cc"
    break;

  case 186: // infixtype: ftype
#line 911 "parser.y"
           { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2969 "parser.cc"
    break;

  case 187: // infixtype: btype "~" btype
#line 912 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,"~"}),yystack_[2].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()});}
#line 2975 "parser.cc"
    break;

  case 188: // btype_no_ops: atype_docs
#line 914 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2981 "parser.cc"
    break;

  case 189: // btype_no_ops: btype_no_ops atype_docs
#line 915 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[1].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 2987 "parser.cc"
    break;

  case 190: // ftype: atype
#line 917 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2993 "parser.cc"
    break;

  case 191: // ftype: tyop
#line 918 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 2999 "parser.cc"
    break;

  case 192: // ftype: ftype tyarg
#line 919 "parser.y"
                                   { yylhs.value.as < Hs::Type > () = Hs::TypeApp(yystack_[1].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()); }
#line 3005 "parser.cc"
    break;

  case 193: // tyarg: atype
#line 921 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3011 "parser.cc"
    break;

  case 194: // tyop: qtyconop
#line 923 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3017 "parser.cc"
    break;

  case 195: // tyop: tyvarop
#line 924 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3023 "parser.cc"
    break;

  case 196: // atype_docs: atype
#line 931 "parser.y"
            { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3029 "parser.cc"
    break;

  case 197: // atype: ntgtycon
#line 938 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3035 "parser.cc"
    break;

  case 198: // atype: tyvar
#line 939 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3041 "parser.cc"
    break;

  case 199: // atype: "*"
#line 940 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,"*"});}
#line 3047 "parser.cc"
    break;

  case 200: // atype: strict_mark atype
#line 941 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::StrictLazyType(yystack_[1].value.as < Hs::StrictLazy > (),yystack_[0].value.as < Hs::Type > ());}
#line 3053 "parser.cc"
    break;

  case 201: // atype: "{" fielddecls "}"
#line 942 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ());}
#line 3059 "parser.cc"
    break;

  case 202: // atype: "(" ")"
#line 943 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[1].location,"()"});}
#line 3065 "parser.cc"
    break;

  case 203: // atype: "(" comma_types1 "," ctype ")"
#line 944 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::Type> > ();ts.push_back(yystack_[1].value.as < Hs::Type > ());yylhs.value.as < Hs::Type > () = Hs::TupleType(ts);}
#line 3071 "parser.cc"
    break;

  case 204: // atype: "[" ctype "]"
#line 950 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::ListType{yystack_[1].value.as < Hs::Type > ()}; }
#line 3077 "parser.cc"
    break;

  case 205: // atype: "(" ctype ")"
#line 951 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = yystack_[1].value.as < Hs::Type > ();}
#line 3083 "parser.cc"
    break;

  case 206: // inst_type: sigtype
#line 955 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 3089 "parser.cc"
    break;

  case 209: // comma_types0: comma_types1
#line 960 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[0].value.as < std::vector<Hs::Type> > ();}
#line 3095 "parser.cc"
    break;

  case 210: // comma_types0: %empty
#line 961 "parser.y"
                                       { /* default construction OK */ }
#line 3101 "parser.cc"
    break;

  case 211: // comma_types1: ctype
#line 963 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3107 "parser.cc"
    break;

  case 212: // comma_types1: comma_types1 "," ctype
#line 964 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[2].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3113 "parser.cc"
    break;

  case 213: // tv_bndrs: tv_bndrs tv_bndr
#line 971 "parser.y"
                               {yylhs.value.as < std::vector<Hs::TypeVar> > () = yystack_[1].value.as < std::vector<Hs::TypeVar> > (); yylhs.value.as < std::vector<Hs::TypeVar> > ().push_back(yystack_[0].value.as < Hs::TypeVar > ());}
#line 3119 "parser.cc"
    break;

  case 214: // tv_bndrs: %empty
#line 972 "parser.y"
                               { /* default construction OK */}
#line 3125 "parser.cc"
    break;

  case 215: // tv_bndr: tv_bndr_no_braces
#line 974 "parser.y"
                                 {yylhs.value.as < Hs::TypeVar > () = yystack_[0].value.as < Hs::TypeVar > ();}
#line 3131 "parser.cc"
    break;

  case 216: // tv_bndr: "{" tyvar "}"
#line 975 "parser.y"
                                 {}
#line 3137 "parser.cc"
    break;

  case 217: // tv_bndr: "{" tyvar "::" kind "}"
#line 976 "parser.y"
                                 {}
#line 3143 "parser.cc"
    break;

  case 218: // tv_bndr_no_braces: tyvar
#line 979 "parser.y"
                                              {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3149 "parser.cc"
    break;

  case 219: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 980 "parser.y"
                                              {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[3].location,yystack_[3].value.as < std::string > ()},yystack_[1].value.as < expression_ref > ());}
#line 3155 "parser.cc"
    break;

  case 220: // kind: ctype
#line 998 "parser.y"
             {yylhs.value.as < expression_ref > () = type_to_kind(yystack_[0].value.as < Hs::Type > ());}
#line 3161 "parser.cc"
    break;

  case 221: // gadt_constrlist: "where" "{" gadt_constrs "}"
#line 1004 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3167 "parser.cc"
    break;

  case 222: // gadt_constrlist: "where" "vocurly" gadt_constrs close
#line 1005 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3173 "parser.cc"
    break;

  case 223: // gadt_constrlist: %empty
#line 1006 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = {};}
#line 3179 "parser.cc"
    break;

  case 224: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1008 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3185 "parser.cc"
    break;

  case 225: // gadt_constrs: gadt_constr
#line 1009 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3191 "parser.cc"
    break;

  case 226: // gadt_constr: optSemi con_list "::" sigtype
#line 1011 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),{{},yystack_[0].value.as < Hs::Type > ()});}
#line 3197 "parser.cc"
    break;

  case 227: // constrs: "=" constrs1
#line 1013 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 3203 "parser.cc"
    break;

  case 228: // constrs1: constrs1 "|" constr
#line 1015 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3209 "parser.cc"
    break;

  case 229: // constrs1: constr
#line 1016 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3215 "parser.cc"
    break;

  case 230: // constr: forall context_no_ops "=>" constr_stuff
#line 1018 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::TypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::Type > ());}
#line 3221 "parser.cc"
    break;

  case 231: // constr: forall constr_stuff
#line 1019 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::TypeVar> > (),{}, yystack_[0].value.as < Hs::Type > ());}
#line 3227 "parser.cc"
    break;

  case 232: // forall: "forall" tv_bndrs "."
#line 1021 "parser.y"
                                {yylhs.value.as < std::vector<Hs::TypeVar> > () = yystack_[1].value.as < std::vector<Hs::TypeVar> > ();}
#line 3233 "parser.cc"
    break;

  case 233: // forall: %empty
#line 1022 "parser.y"
                                {}
#line 3239 "parser.cc"
    break;

  case 234: // constr_stuff: btype_no_ops
#line 1024 "parser.y"
                                                {yylhs.value.as < Hs::Type > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ());}
#line 3245 "parser.cc"
    break;

  case 235: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1025 "parser.y"
                                                {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,yystack_[1].value.as < std::string > ()}),Hs::make_tyapps(yystack_[2].value.as < std::vector<Hs::Type> > ()),Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ())});}
#line 3251 "parser.cc"
    break;

  case 236: // fielddecls: %empty
#line 1027 "parser.y"
                                {}
#line 3257 "parser.cc"
    break;

  case 237: // fielddecls: fielddecls1
#line 1028 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 3263 "parser.cc"
    break;

  case 238: // fielddecls1: fielddecls1 "," fielddecl
#line 1030 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3269 "parser.cc"
    break;

  case 239: // fielddecls1: fielddecl
#line 1031 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3275 "parser.cc"
    break;

  case 240: // fielddecl: sig_vars "::" ctype
#line 1033 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::Var> > (),yystack_[0].value.as < Hs::Type > ());}
#line 3281 "parser.cc"
    break;

  case 251: // decl_no_th: sigdecl
#line 1052 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3287 "parser.cc"
    break;

  case 252: // decl_no_th: PREFIX_BANG aexp rhs
#line 1054 "parser.y"
                                      {yylhs.value.as < expression_ref > () = Hs::StrictValueDecl{{yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < Hs::MultiGuardedRHS > ()}; }
#line 3293 "parser.cc"
    break;

  case 253: // decl_no_th: infixexp_top rhs
#line 1055 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::ValueDecl({yystack_[1].location,yystack_[1].value.as < Hs::InfixExp > ()},yystack_[0].value.as < Hs::MultiGuardedRHS > ());}
#line 3299 "parser.cc"
    break;

  case 254: // decl: decl_no_th
#line 1057 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3305 "parser.cc"
    break;

  case 255: // rhs: "=" exp wherebinds
#line 1061 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3311 "parser.cc"
    break;

  case 256: // rhs: gdrhs wherebinds
#line 1062 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 3317 "parser.cc"
    break;

  case 257: // gdrhs: gdrhs gdrh
#line 1064 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3323 "parser.cc"
    break;

  case 258: // gdrhs: gdrh
#line 1065 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3329 "parser.cc"
    break;

  case 259: // gdrh: "|" guardquals "=" exp
#line 1069 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ()};}
#line 3335 "parser.cc"
    break;

  case 260: // sigdecl: sig_vars "::" sigtypedoc
#line 1079 "parser.y"
                                  { yylhs.value.as < expression_ref > () = Hs::SignatureDecl{yystack_[2].value.as < std::vector<Hs::Var> > (),yystack_[0].value.as < Hs::Type > ()}; }
#line 3341 "parser.cc"
    break;

  case 261: // sigdecl: infix prec ops
#line 1080 "parser.y"
                         { yylhs.value.as < expression_ref > () = Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()}; }
#line 3347 "parser.cc"
    break;

  case 262: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1082 "parser.y"
                                                    {}
#line 3353 "parser.cc"
    break;

  case 263: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1083 "parser.y"
                                            {}
#line 3359 "parser.cc"
    break;

  case 264: // sigdecl: "{-# SCC" qvar "#-}"
#line 1084 "parser.y"
                              {}
#line 3365 "parser.cc"
    break;

  case 265: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1085 "parser.y"
                                     {}
#line 3371 "parser.cc"
    break;

  case 266: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1086 "parser.y"
                                                               {}
#line 3377 "parser.cc"
    break;

  case 267: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1087 "parser.y"
                                                                      {}
#line 3383 "parser.cc"
    break;

  case 268: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1088 "parser.y"
                                                     {}
#line 3389 "parser.cc"
    break;

  case 273: // exp: infixexp "::" sigtype
#line 1100 "parser.y"
                           { yylhs.value.as < expression_ref > () = Hs::TypedExp(yystack_[2].value.as < Hs::InfixExp > (),yystack_[0].value.as < Hs::Type > ()); }
#line 3395 "parser.cc"
    break;

  case 274: // exp: infixexp
#line 1101 "parser.y"
                           { yylhs.value.as < expression_ref > () = yystack_[0].value.as < Hs::InfixExp > (); }
#line 3401 "parser.cc"
    break;

  case 275: // infixexp: exp10
#line 1105 "parser.y"
                                {yylhs.value.as < Hs::InfixExp > () = Hs::InfixExp({yystack_[0].value.as < expression_ref > ()});}
#line 3407 "parser.cc"
    break;

  case 276: // infixexp: infixexp qop exp10
#line 1106 "parser.y"
                                {yylhs.value.as < Hs::InfixExp > () = yystack_[2].value.as < Hs::InfixExp > (); yylhs.value.as < Hs::InfixExp > ().terms.push_back(yystack_[1].value.as < expression_ref > ()); yylhs.value.as < Hs::InfixExp > ().terms.push_back(yystack_[0].value.as < expression_ref > ());}
#line 3413 "parser.cc"
    break;

  case 277: // infixexp_top: exp10_top
#line 1108 "parser.y"
                                {yylhs.value.as < Hs::InfixExp > () = Hs::InfixExp({yystack_[0].value.as < expression_ref > ()});}
#line 3419 "parser.cc"
    break;

  case 278: // infixexp_top: infixexp_top qop exp10_top
#line 1109 "parser.y"
                                          {yylhs.value.as < Hs::InfixExp > () = yystack_[2].value.as < Hs::InfixExp > (); yylhs.value.as < Hs::InfixExp > ().terms.push_back(yystack_[1].value.as < expression_ref > ()); yylhs.value.as < Hs::InfixExp > ().terms.push_back(yystack_[0].value.as < expression_ref > ());}
#line 3425 "parser.cc"
    break;

  case 279: // exp10_top: "-" fexp
#line 1111 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_minus(yystack_[0].value.as < expression_ref > ());}
#line 3431 "parser.cc"
    break;

  case 280: // exp10_top: "{-# CORE" "STRING" "#-}"
#line 1112 "parser.y"
                                   {}
#line 3437 "parser.cc"
    break;

  case 281: // exp10_top: fexp
#line 1113 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3443 "parser.cc"
    break;

  case 282: // exp10: exp10_top
#line 1116 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3449 "parser.cc"
    break;

  case 283: // exp10: scc_annot exp
#line 1117 "parser.y"
                                 {}
#line 3455 "parser.cc"
    break;

  case 288: // fexp: fexp aexp
#line 1129 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_apply(yystack_[1].value.as < expression_ref > (), yystack_[0].value.as < expression_ref > ());}
#line 3461 "parser.cc"
    break;

  case 289: // fexp: fexp "TYPEAPP" atype
#line 1130 "parser.y"
                                 {}
#line 3467 "parser.cc"
    break;

  case 290: // fexp: "static" aexp
#line 1131 "parser.y"
                                 {}
#line 3473 "parser.cc"
    break;

  case 291: // fexp: aexp
#line 1132 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3479 "parser.cc"
    break;

  case 292: // aexp: qvar "@" aexp
#line 1135 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::AsPattern(Hs::Var({yystack_[2].location,yystack_[2].value.as < std::string > ()}),yystack_[0].value.as < expression_ref > ());}
#line 3485 "parser.cc"
    break;

  case 293: // aexp: PREFIX_TILDE aexp
#line 1136 "parser.y"
                                          {yylhs.value.as < expression_ref > () = Hs::LazyPattern(yystack_[0].value.as < expression_ref > ());}
#line 3491 "parser.cc"
    break;

  case 294: // aexp: "\\" apats1 "->" exp
#line 1137 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::LambdaExp(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3497 "parser.cc"
    break;

  case 295: // aexp: "let" binds "in" exp
#line 1138 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 3503 "parser.cc"
    break;

  case 296: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1140 "parser.y"
                                                       {yylhs.value.as < expression_ref > () = Hs::IfExp({yystack_[6].location,yystack_[6].value.as < expression_ref > ()},{yystack_[3].location,yystack_[3].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 3509 "parser.cc"
    break;

  case 297: // aexp: "case" exp "of" altslist
#line 1142 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::CaseExp(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < Hs::Alts > ());}
#line 3515 "parser.cc"
    break;

  case 298: // aexp: "do" stmtlist
#line 1143 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::Do(yystack_[0].value.as < Hs::Stmts > ());}
#line 3521 "parser.cc"
    break;

  case 299: // aexp: "mdo" stmtlist
#line 1144 "parser.y"
                                 {yylhs.value.as < expression_ref > () = Hs::MDo(yystack_[0].value.as < Hs::Stmts > ());}
#line 3527 "parser.cc"
    break;

  case 300: // aexp: aexp1
#line 1146 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3533 "parser.cc"
    break;

  case 301: // aexp1: aexp1 "{" fbinds "}"
#line 1149 "parser.y"
                              {}
#line 3539 "parser.cc"
    break;

  case 302: // aexp1: aexp2
#line 1150 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3545 "parser.cc"
    break;

  case 303: // aexp2: qvar
#line 1153 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3551 "parser.cc"
    break;

  case 304: // aexp2: qcon
#line 1154 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3557 "parser.cc"
    break;

  case 305: // aexp2: literal
#line 1155 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3563 "parser.cc"
    break;

  case 306: // aexp2: "(" texp ")"
#line 1156 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3569 "parser.cc"
    break;

  case 307: // aexp2: "(" tup_exprs ")"
#line 1157 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::Tuple(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3575 "parser.cc"
    break;

  case 308: // aexp2: "[" list "]"
#line 1162 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3581 "parser.cc"
    break;

  case 309: // aexp2: "_"
#line 1163 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::WildcardPattern();}
#line 3587 "parser.cc"
    break;

  case 310: // texp: exp
#line 1169 "parser.y"
                      {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3593 "parser.cc"
    break;

  case 311: // texp: infixexp qop
#line 1170 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::LeftSection ( yystack_[1].value.as < Hs::InfixExp > (), yystack_[0].value.as < expression_ref > () ); }
#line 3599 "parser.cc"
    break;

  case 312: // texp: qopm infixexp
#line 1171 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::RightSection( yystack_[1].value.as < expression_ref > (), yystack_[0].value.as < Hs::InfixExp > () ); }
#line 3605 "parser.cc"
    break;

  case 313: // tup_exprs: tup_exprs "," texp
#line 1176 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3611 "parser.cc"
    break;

  case 314: // tup_exprs: texp "," texp
#line 1177 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3617 "parser.cc"
    break;

  case 315: // list: texp
#line 1195 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List{{yystack_[0].value.as < expression_ref > ()}}; }
#line 3623 "parser.cc"
    break;

  case 316: // list: lexps
#line 1196 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List{yystack_[0].value.as < std::vector<expression_ref> > ()}; }
#line 3629 "parser.cc"
    break;

  case 317: // list: texp ".."
#line 1197 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < expression_ref > ()); }
#line 3635 "parser.cc"
    break;

  case 318: // list: texp "," exp ".."
#line 1198 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()); }
#line 3641 "parser.cc"
    break;

  case 319: // list: texp ".." exp
#line 1199 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()); }
#line 3647 "parser.cc"
    break;

  case 320: // list: texp "," exp ".." exp
#line 1200 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < expression_ref > (), yystack_[2].value.as < expression_ref > (), yystack_[0].value.as < expression_ref > ()); }
#line 3653 "parser.cc"
    break;

  case 321: // list: texp "|" squals
#line 1201 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < expression_ref > (), yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 3659 "parser.cc"
    break;

  case 322: // lexps: lexps "," texp
#line 1203 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3665 "parser.cc"
    break;

  case 323: // lexps: texp "," texp
#line 1204 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3671 "parser.cc"
    break;

  case 324: // squals: squals "," qual
#line 1217 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3677 "parser.cc"
    break;

  case 325: // squals: qual
#line 1219 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3683 "parser.cc"
    break;

  case 326: // guardquals: guardquals1
#line 1229 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3689 "parser.cc"
    break;

  case 327: // guardquals1: guardquals1 "," qual
#line 1231 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > ();yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3695 "parser.cc"
    break;

  case 328: // guardquals1: qual
#line 1232 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3701 "parser.cc"
    break;

  case 329: // altslist: "{" alts "}"
#line 1235 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 3707 "parser.cc"
    break;

  case 330: // altslist: "vocurly" alts close
#line 1236 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 3713 "parser.cc"
    break;

  case 331: // altslist: "{" "}"
#line 1237 "parser.y"
                                 {}
#line 3719 "parser.cc"
    break;

  case 332: // altslist: "vocurly" close
#line 1238 "parser.y"
                                 {}
#line 3725 "parser.cc"
    break;

  case 333: // alts: alts1
#line 1240 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 3731 "parser.cc"
    break;

  case 334: // alts: ";" alts
#line 1241 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 3737 "parser.cc"
    break;

  case 335: // alts1: alts1 ";" alt
#line 1243 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 3743 "parser.cc"
    break;

  case 336: // alts1: alts1 ";"
#line 1244 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 3749 "parser.cc"
    break;

  case 337: // alts1: alt
#line 1245 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 3755 "parser.cc"
    break;

  case 338: // alt: pat alt_rhs
#line 1247 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 3761 "parser.cc"
    break;

  case 339: // alt_rhs: "->" exp wherebinds
#line 1249 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3767 "parser.cc"
    break;

  case 340: // alt_rhs: gdpats wherebinds
#line 1250 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3773 "parser.cc"
    break;

  case 341: // gdpats: gdpats gdpat
#line 1252 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3779 "parser.cc"
    break;

  case 342: // gdpats: gdpat
#line 1253 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3785 "parser.cc"
    break;

  case 343: // gdpat: "|" guardquals "->" exp
#line 1262 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ()};}
#line 3791 "parser.cc"
    break;

  case 344: // pat: exp
#line 1264 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3797 "parser.cc"
    break;

  case 345: // pat: PREFIX_BANG aexp
#line 1265 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3803 "parser.cc"
    break;

  case 346: // bindpat: exp
#line 1267 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3809 "parser.cc"
    break;

  case 347: // bindpat: PREFIX_BANG aexp
#line 1268 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3815 "parser.cc"
    break;

  case 348: // apat: aexp
#line 1270 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3821 "parser.cc"
    break;

  case 349: // apat: PREFIX_BANG aexp
#line 1271 "parser.y"
                      {yylhs.value.as < expression_ref > () = Hs::StrictPattern(yystack_[0].value.as < expression_ref > ());}
#line 3827 "parser.cc"
    break;

  case 350: // apats1: apats1 apat
#line 1273 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3833 "parser.cc"
    break;

  case 351: // apats1: apat
#line 1274 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3839 "parser.cc"
    break;

  case 352: // stmtlist: "{" stmts "}"
#line 1277 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 3845 "parser.cc"
    break;

  case 353: // stmtlist: "vocurly" stmts close
#line 1278 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 3851 "parser.cc"
    break;

  case 354: // stmts: stmts ";" stmt
#line 1280 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3857 "parser.cc"
    break;

  case 355: // stmts: stmts ";"
#line 1281 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3863 "parser.cc"
    break;

  case 356: // stmts: stmt
#line 1282 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3869 "parser.cc"
    break;

  case 357: // stmts: %empty
#line 1283 "parser.y"
                       {}
#line 3875 "parser.cc"
    break;

  case 358: // stmt: qual
#line 1288 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3881 "parser.cc"
    break;

  case 359: // stmt: "rec" stmtlist
#line 1289 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ());}
#line 3887 "parser.cc"
    break;

  case 360: // qual: bindpat "<-" exp
#line 1291 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::PatQual(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3893 "parser.cc"
    break;

  case 361: // qual: exp
#line 1292 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::SimpleQual(yystack_[0].value.as < expression_ref > ());}
#line 3899 "parser.cc"
    break;

  case 362: // qual: "let" binds
#line 1293 "parser.y"
                        {yylhs.value.as < expression_ref > () = Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ());}
#line 3905 "parser.cc"
    break;

  case 370: // qcon: gen_qcon
#line 1338 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3911 "parser.cc"
    break;

  case 371: // qcon: sysdcon
#line 1339 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3917 "parser.cc"
    break;

  case 372: // gen_qcon: qconid
#line 1341 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3923 "parser.cc"
    break;

  case 373: // gen_qcon: "(" qconsym ")"
#line 1342 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3929 "parser.cc"
    break;

  case 374: // con: conid
#line 1344 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3935 "parser.cc"
    break;

  case 375: // con: "(" consym ")"
#line 1345 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3941 "parser.cc"
    break;

  case 376: // con: sysdcon
#line 1346 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3947 "parser.cc"
    break;

  case 377: // con_list: con_list "," con
#line 1348 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3953 "parser.cc"
    break;

  case 378: // con_list: con
#line 1349 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3959 "parser.cc"
    break;

  case 379: // sysdcon_no_list: "(" ")"
#line 1351 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 3965 "parser.cc"
    break;

  case 380: // sysdcon_no_list: "(" commas ")"
#line 1352 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3971 "parser.cc"
    break;

  case 381: // sysdcon_no_list: "(#" "#)"
#line 1353 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 3977 "parser.cc"
    break;

  case 382: // sysdcon_no_list: "(#" commas "#)"
#line 1354 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3983 "parser.cc"
    break;

  case 383: // sysdcon: sysdcon_no_list
#line 1356 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3989 "parser.cc"
    break;

  case 384: // sysdcon: "[" "]"
#line 1357 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 3995 "parser.cc"
    break;

  case 385: // conop: consym
#line 1359 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4001 "parser.cc"
    break;

  case 386: // conop: "`" conid "`"
#line 1360 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4007 "parser.cc"
    break;

  case 387: // qconop: qconsym
#line 1362 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4013 "parser.cc"
    break;

  case 388: // qconop: "`" qconid "`"
#line 1363 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4019 "parser.cc"
    break;

  case 389: // gtycon: ntgtycon
#line 1366 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4025 "parser.cc"
    break;

  case 390: // gtycon: "(" ")"
#line 1367 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 4031 "parser.cc"
    break;

  case 391: // gtycon: "(#" "#)"
#line 1368 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 4037 "parser.cc"
    break;

  case 392: // ntgtycon: oqtycon
#line 1370 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4043 "parser.cc"
    break;

  case 393: // ntgtycon: "(" commas ")"
#line 1371 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4049 "parser.cc"
    break;

  case 394: // ntgtycon: "(#" commas "#)"
#line 1372 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4055 "parser.cc"
    break;

  case 395: // ntgtycon: "(" "->" ")"
#line 1373 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 4061 "parser.cc"
    break;

  case 396: // ntgtycon: "[" "]"
#line 1374 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 4067 "parser.cc"
    break;

  case 397: // oqtycon: qtycon
#line 1376 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4073 "parser.cc"
    break;

  case 398: // oqtycon: "(" qtyconsym ")"
#line 1377 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4079 "parser.cc"
    break;

  case 399: // oqtycon: "(" "~" ")"
#line 1378 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 4085 "parser.cc"
    break;

  case 400: // oqtycon_no_varcon: qtycon
#line 1380 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4091 "parser.cc"
    break;

  case 401: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1381 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4097 "parser.cc"
    break;

  case 402: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1382 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4103 "parser.cc"
    break;

  case 403: // oqtycon_no_varcon: "(" ":" ")"
#line 1383 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 4109 "parser.cc"
    break;

  case 404: // oqtycon_no_varcon: "(" "~" ")"
#line 1384 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 4115 "parser.cc"
    break;

  case 405: // qtyconop: qtyconsym
#line 1387 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4121 "parser.cc"
    break;

  case 406: // qtyconop: "`" qtycon "`"
#line 1388 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4127 "parser.cc"
    break;

  case 407: // qtycondoc: qtycon
#line 1390 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4133 "parser.cc"
    break;

  case 408: // qtycon: "QCONID"
#line 1392 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4139 "parser.cc"
    break;

  case 409: // qtycon: tycon
#line 1393 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4145 "parser.cc"
    break;

  case 410: // tycon: "CONID"
#line 1397 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4151 "parser.cc"
    break;

  case 411: // qtyconsym: "QCONSYM"
#line 1399 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4157 "parser.cc"
    break;

  case 412: // qtyconsym: "QVARSYM"
#line 1400 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4163 "parser.cc"
    break;

  case 413: // qtyconsym: tyconsym
#line 1401 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4169 "parser.cc"
    break;

  case 414: // tyconsym: "CONSYM"
#line 1403 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4175 "parser.cc"
    break;

  case 415: // tyconsym: "VARSYM"
#line 1404 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4181 "parser.cc"
    break;

  case 416: // tyconsym: ":"
#line 1405 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4187 "parser.cc"
    break;

  case 417: // tyconsym: "-"
#line 1406 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 4193 "parser.cc"
    break;

  case 418: // op: varop
#line 1411 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4199 "parser.cc"
    break;

  case 419: // op: conop
#line 1412 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4205 "parser.cc"
    break;

  case 420: // varop: varsym
#line 1414 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4211 "parser.cc"
    break;

  case 421: // varop: "`" varid "`"
#line 1415 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4217 "parser.cc"
    break;

  case 422: // qop: qvarop
#line 1417 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 4223 "parser.cc"
    break;

  case 423: // qop: qconop
#line 1418 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 4229 "parser.cc"
    break;

  case 424: // qopm: qvaropm
#line 1421 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 4235 "parser.cc"
    break;

  case 425: // qopm: qconop
#line 1422 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 4241 "parser.cc"
    break;

  case 426: // qvarop: qvarsym
#line 1427 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4247 "parser.cc"
    break;

  case 427: // qvarop: "`" qvarid "`"
#line 1428 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4253 "parser.cc"
    break;

  case 428: // qvaropm: qvarsym_no_minus
#line 1430 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 4259 "parser.cc"
    break;

  case 429: // qvaropm: "`" qvarid "`"
#line 1431 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4265 "parser.cc"
    break;

  case 430: // tyvar: tyvarid
#line 1435 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4271 "parser.cc"
    break;

  case 431: // tyvarop: "`" tyvarid "`"
#line 1437 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4277 "parser.cc"
    break;

  case 432: // tyvarid: "VARID"
#line 1439 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4283 "parser.cc"
    break;

  case 433: // tyvarid: special_id
#line 1440 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4289 "parser.cc"
    break;

  case 434: // tyvarid: "unsafe"
#line 1441 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 4295 "parser.cc"
    break;

  case 435: // tyvarid: "safe"
#line 1442 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 4301 "parser.cc"
    break;

  case 436: // tyvarid: "interruptible"
#line 1443 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 4307 "parser.cc"
    break;

  case 437: // var: varid
#line 1446 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4313 "parser.cc"
    break;

  case 438: // var: "(" varsym ")"
#line 1447 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4319 "parser.cc"
    break;

  case 439: // qvar: qvarid
#line 1449 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4325 "parser.cc"
    break;

  case 440: // qvar: "(" varsym ")"
#line 1450 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4331 "parser.cc"
    break;

  case 441: // qvar: "(" qvarsym1 ")"
#line 1451 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4337 "parser.cc"
    break;

  case 442: // qvarid: varid
#line 1453 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4343 "parser.cc"
    break;

  case 443: // qvarid: "QVARID"
#line 1454 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4349 "parser.cc"
    break;

  case 444: // varid: "VARID"
#line 1456 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4355 "parser.cc"
    break;

  case 445: // varid: special_id
#line 1457 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4361 "parser.cc"
    break;

  case 446: // varid: "unsafe"
#line 1458 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 4367 "parser.cc"
    break;

  case 447: // varid: "safe"
#line 1459 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 4373 "parser.cc"
    break;

  case 448: // varid: "interruptible"
#line 1460 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 4379 "parser.cc"
    break;

  case 449: // varid: "forall"
#line 1461 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 4385 "parser.cc"
    break;

  case 450: // varid: "family"
#line 1462 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 4391 "parser.cc"
    break;

  case 451: // varid: "role"
#line 1463 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 4397 "parser.cc"
    break;

  case 452: // qvarsym: varsym
#line 1465 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4403 "parser.cc"
    break;

  case 453: // qvarsym: qvarsym1
#line 1466 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4409 "parser.cc"
    break;

  case 454: // qvarsym_no_minus: varsym_no_minus
#line 1468 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4415 "parser.cc"
    break;

  case 455: // qvarsym_no_minus: qvarsym1
#line 1469 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4421 "parser.cc"
    break;

  case 456: // qvarsym1: "QVARSYM"
#line 1471 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4427 "parser.cc"
    break;

  case 457: // varsym: varsym_no_minus
#line 1473 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4433 "parser.cc"
    break;

  case 458: // varsym: "-"
#line 1474 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 4439 "parser.cc"
    break;

  case 459: // varsym_no_minus: "VARSYM"
#line 1476 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4445 "parser.cc"
    break;

  case 460: // varsym_no_minus: special_sym
#line 1477 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4451 "parser.cc"
    break;

  case 461: // special_id: "as"
#line 1479 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 4457 "parser.cc"
    break;

  case 462: // special_id: "qualified"
#line 1480 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 4463 "parser.cc"
    break;

  case 463: // special_id: "hiding"
#line 1481 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 4469 "parser.cc"
    break;

  case 464: // special_id: "export"
#line 1482 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 4475 "parser.cc"
    break;

  case 465: // special_id: "label"
#line 1483 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 4481 "parser.cc"
    break;

  case 466: // special_id: "dynamic"
#line 1484 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 4487 "parser.cc"
    break;

  case 467: // special_id: "stdcall"
#line 1485 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 4493 "parser.cc"
    break;

  case 468: // special_id: "ccall"
#line 1486 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 4499 "parser.cc"
    break;

  case 469: // special_id: "capi"
#line 1487 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 4505 "parser.cc"
    break;

  case 470: // special_id: "prim"
#line 1488 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 4511 "parser.cc"
    break;

  case 471: // special_id: "javascript"
#line 1489 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 4517 "parser.cc"
    break;

  case 472: // special_id: "group"
#line 1490 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 4523 "parser.cc"
    break;

  case 473: // special_id: "stock"
#line 1491 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 4529 "parser.cc"
    break;

  case 474: // special_id: "anyclass"
#line 1492 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 4535 "parser.cc"
    break;

  case 475: // special_id: "via"
#line 1493 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 4541 "parser.cc"
    break;

  case 476: // special_id: "unit"
#line 1494 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 4547 "parser.cc"
    break;

  case 477: // special_id: "dependency"
#line 1495 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 4553 "parser.cc"
    break;

  case 478: // special_id: "signature"
#line 1496 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 4559 "parser.cc"
    break;

  case 479: // special_sym: "!"
#line 1498 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 4565 "parser.cc"
    break;

  case 480: // special_sym: "."
#line 1499 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 4571 "parser.cc"
    break;

  case 481: // special_sym: "*"
#line 1500 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 4577 "parser.cc"
    break;

  case 482: // qconid: conid
#line 1504 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4583 "parser.cc"
    break;

  case 483: // qconid: "QCONID"
#line 1505 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4589 "parser.cc"
    break;

  case 484: // conid: "CONID"
#line 1507 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4595 "parser.cc"
    break;

  case 485: // qconsym: consym
#line 1509 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4601 "parser.cc"
    break;

  case 486: // qconsym: "QCONSYM"
#line 1510 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4607 "parser.cc"
    break;

  case 487: // consym: "CONSYM"
#line 1512 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4613 "parser.cc"
    break;

  case 488: // consym: ":"
#line 1513 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4619 "parser.cc"
    break;

  case 489: // literal: "CHAR"
#line 1517 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 4625 "parser.cc"
    break;

  case 490: // literal: "STRING"
#line 1518 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 4631 "parser.cc"
    break;

  case 491: // literal: "INTEGER"
#line 1519 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < int > ()});}
#line 4637 "parser.cc"
    break;

  case 492: // literal: "RATIONAL"
#line 1520 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Double{yystack_[0].value.as < double > ()});}
#line 4643 "parser.cc"
    break;

  case 493: // literal: "PRIMINTEGER"
#line 1521 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < int > ()});}
#line 4649 "parser.cc"
    break;

  case 495: // close: error
#line 1529 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 4655 "parser.cc"
    break;

  case 496: // modid: "CONID"
#line 1533 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4661 "parser.cc"
    break;

  case 497: // modid: "QCONID"
#line 1534 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4667 "parser.cc"
    break;

  case 498: // commas: commas ","
#line 1536 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 4673 "parser.cc"
    break;

  case 499: // commas: ","
#line 1537 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 4679 "parser.cc"
    break;


#line 4683 "parser.cc"

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


  const short parser::yypact_ninf_ = -646;

  const short parser::yytable_ninf_ = -458;

  const short
  parser::yypact_[] =
  {
      23,   136,  -646,   117,  -646,  -646,  -646,  -646,  -646,   215,
      20,    46,  -646,    40,     4,     4,    22,  -646,  -646,  -646,
    -646,   165,  -646,  -646,  -646,    72,  -646,   142,   193,  4812,
     248,   304,   204,  -646,   817,  -646,    71,  -646,  -646,  -646,
    -646,   136,  -646,  -646,  -646,  -646,  -646,  -646,  -646,  -646,
    -646,  -646,  -646,  -646,  -646,  -646,  -646,  -646,  -646,  -646,
    -646,  -646,  -646,  -646,   630,  -646,  -646,  -646,  -646,   237,
     236,  -646,   277,  -646,  -646,  -646,  -646,  -646,  -646,  -646,
     -13,  -646,   136,  -646,   298,  -646,  2560,  4403,  -646,   292,
     197,  2560,  -646,  -646,  -646,   320,   217,  -646,  3578,   347,
     197,  3358,   321,   301,  5082,   205,  2959,  3358,  3092,  3358,
    1629,  1496,    93,  -646,  -646,  -646,  -646,  -646,  -646,  -646,
      24,   321,   311,   204,  -646,  -646,  -646,  -646,   375,   -18,
    -646,  -646,   286,  -646,  3225,  -646,   348,  -646,  -646,  -646,
    -646,  -646,  -646,   364,    -2,  -646,  -646,  -646,  -646,   325,
    -646,   342,   349,  -646,  -646,  -646,  -646,  -646,   354,  -646,
     355,   356,   357,  -646,  -646,  -646,  4812,  4849,  -646,  -646,
    -646,  -646,   467,  -646,   -43,  1496,   449,   623,  -646,  -646,
    2560,  -646,  -646,  -646,  -646,  -646,  -646,  -646,  -646,  5178,
    3681,  3467,   358,  5008,  -646,  -646,  -646,  -646,  -646,   446,
    4710,  -646,   385,  -646,   270,  -646,  4710,  -646,  -646,  -646,
    -646,  -646,  -646,  -646,  -646,  -646,  -646,  -646,  -646,  3888,
    2028,  2028,  -646,   365,   402,   404,   408,   409,  3888,  1230,
    1230,  -646,   473,  4403,  4403,    85,   410,   251,    87,   448,
    -646,  -646,   -19,  5082,  -646,   413,   424,     7,   386,   216,
    -646,   109,  -646,  -646,  3358,  -646,  -646,  2826,  -646,  3225,
     261,  -646,  -646,  4947,  -646,  -646,  -646,   623,     5,   387,
     379,  -646,  2560,  -646,  -646,  -646,  -646,  -646,  -646,  3092,
    -646,  -646,   -31,   -17,   356,   388,   389,   392,    84,  -646,
     246,  3888,  5082,  5082,  -646,   145,   298,   377,  4403,  3888,
    5178,  2560,  2294,  4947,  -646,    36,  -646,  -646,  2693,  -646,
    -646,  -646,  -646,  4710,  -646,  5045,  3358,  -646,  -646,  -646,
    -646,  -646,  -646,  -646,  -646,   398,   400,   405,  -646,   412,
      40,   136,    30,   445,   450,   281,  3888,  2560,  -646,   152,
     111,   423,   414,  -646,  -646,  -646,  -646,   420,   438,  -646,
     419,   426,  -646,   427,   422,   430,   149,   252,   425,   431,
     295,  -646,  -646,  4403,  3888,  4403,  -646,  -646,  -646,   434,
     435,   217,   197,  3358,   456,   462,    73,  -646,  -646,    47,
    -646,   525,  -646,  -646,  -646,  -646,  -646,  -646,   526,    95,
    -646,  -646,   286,    48,  2560,  -646,   474,   210,  3888,   108,
    3888,   429,   436,   451,   477,  -646,  -646,   483,   452,   313,
     205,   491,  -646,  2560,  -646,  -646,   453,   457,  2560,  2560,
    2294,  1762,  -646,  1762,   667,  -646,  1762,  -646,  1762,   113,
    -646,  -646,  -646,  -646,   496,   495,   498,  5141,   463,  -646,
    -646,  -646,  -646,  -646,    28,   324,  -646,  -646,  -646,  -646,
     558,   505,   471,  -646,   475,   217,  -646,  -646,  -646,  -646,
    -646,   488,  -646,   478,   515,  -646,  -646,  -646,  4910,  -646,
    -646,  -646,   489,  4812,  -646,  -646,  -646,  -646,  1895,  1363,
    -646,  -646,  -646,   487,  3888,  -646,  5178,  5215,  -646,  3888,
    -646,  -646,  -646,  3888,  -646,  -646,  -646,  -646,  -646,   964,
     964,  -646,  -646,  -646,   510,  -646,  3888,   473,  -646,  -646,
    2560,  -646,  2028,  -646,  2560,   322,  -646,  -646,  1230,  -646,
    -646,  3888,  3888,  5321,   517,  -646,  -646,   522,   163,  -646,
    -646,  5178,   499,  -646,  -646,  -646,  -646,   501,   956,   275,
    -646,  -646,  -646,  -646,  -646,  -646,  -646,  -646,   492,  -646,
     532,  -646,  -646,  -646,  -646,  -646,  3888,  3888,   497,   500,
     145,  -646,   539,  3888,   590,   593,   612,  -646,  2560,  2294,
    -646,  -646,  -646,  5045,  1762,  -646,  4812,   513,  3358,  -646,
    2161,  -646,   523,   509,  -646,   346,    40,  -646,  -646,  -646,
    -646,  3888,  5414,  5414,  -646,  -646,  -646,  -646,   516,   591,
    3785,  -646,  -646,   146,  -646,    49,  -646,  -646,  -646,   365,
    1097,  1097,  -646,  -646,  -646,  -646,  -646,  -646,  5414,   600,
    3888,   430,   549,  -646,  -646,  -646,  2294,  2560,  -646,    -5,
       0,  -646,  -646,  -646,  -646,  -646,  -646,   548,  -646,  4710,
     340,   612,    75,  -646,   612,  -646,  -646,  -646,  -646,  -646,
     524,  -646,  -646,  -646,  -646,  2427,  2294,  2560,  -646,    64,
    -646,  -646,  -646,    16,   555,  -646,  -646,  4403,  4403,   329,
    -646,   336,  -646,   964,  -646,   627,   624,  -646,  -646,   194,
    -646,    65,  -646,   556,   344,  -646,  -646,  3888,  -646,  -646,
    -646,  3888,  -646,  5288,   590,   567,  4506,  -646,  -646,  -646,
     365,   365,  -646,  -646,  -646,  -646,  3991,   144,   607,  -646,
    -646,  -646,  -646,  -646,   576,   558,  -646,  -646,  3888,  -646,
    3888,   583,  -646,  3888,  5381,  -646,  4094,  -646,  -646,  2560,
    -646,  4403,  -646,  1097,  -646,  5414,  4197,  4300,  -646,  -646,
    -646,  -646,  -646,  4710,   544,  -646,  4710,   206,  -646,   205,
      68,  -646,  -646,   551,   561,  -646,  4403,  -646,  2560,  -646,
     566,   563,  3888,  -646,  -646,   592,  3467,   594,  -646,  -646,
    -646,  5414,  -646,   578,   220,   571,    40,    81,  4608,  -646,
    4710,  -646,   365,   118,  -646,  4403,  -646,  -646,  -646,  -646,
    -646,  -646,  5414,   555,  -646,  -646,  -646,  4403,  -646,  -646,
    -646,  -646,  3888,  -646,  -646,  -646,  -646
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    36,     0,     2,    36,     4,   496,   497,     8,
       0,    39,     1,     0,     0,     0,    18,    11,    35,    13,
      16,    58,   495,   494,    12,   162,   158,     0,     0,     0,
       0,    42,    37,    15,    14,   161,     0,     6,     7,   461,
     463,     0,   462,   449,   464,   465,   466,   447,   448,   446,
     450,   451,   467,   468,   469,   470,   471,   472,   473,   474,
     475,   476,   478,   477,     0,   444,   410,   443,   408,     0,
      19,    21,    24,    32,   400,   409,    31,   439,   442,   445,
       0,    41,     0,    34,    38,   309,     0,     0,   112,     0,
       0,     0,    51,    52,    53,    81,     0,   113,     0,     0,
       0,     0,   269,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   484,   483,   489,   490,   491,   492,   493,
     269,   269,    49,    56,    59,    60,    61,    62,   128,     0,
      65,   251,    66,   277,   281,   291,   300,   302,   304,   370,
     383,   371,   170,   303,   442,   372,   482,   305,   159,     0,
      23,     0,     0,   458,   479,   481,   480,   459,     0,   456,
       0,     0,     0,   457,   460,    17,     0,    27,    22,    36,
      36,     3,    44,    33,     0,     0,     0,   274,   282,   275,
       0,   435,   436,   434,   416,   175,   417,   174,   199,   236,
       0,     0,     0,     0,   432,   415,   414,   412,   411,   138,
       0,   173,     0,   125,   182,   185,   186,   191,   190,   197,
     392,   194,   397,   405,   413,   198,   195,   430,   433,   210,
     357,   357,   298,   285,     0,     0,     0,     0,     0,   152,
     152,   155,     0,     0,     0,     0,     0,   182,   392,     0,
     299,   290,     0,     0,   270,     0,     0,     0,     0,     0,
     378,   163,   376,   374,     0,   348,   351,     0,   293,   279,
       0,   488,   384,     0,   487,   486,   310,   274,   315,     0,
     316,   425,     0,   424,   428,   455,   454,   387,   485,   458,
     379,   499,     0,     0,   455,     0,   454,   387,     0,   381,
       0,     0,     0,     0,    50,     0,    57,     0,     0,     0,
       0,     0,     0,     0,   253,   157,   258,   423,     0,   422,
     426,   453,   452,     0,   288,   364,     0,   160,   403,   404,
     402,   401,   441,   440,    20,     0,     0,    28,    30,     0,
       0,     0,    46,     0,     0,     0,     0,     0,   283,     0,
       0,     0,   237,   239,   437,   214,   396,     0,     0,   178,
       0,     0,   202,   211,     0,   405,     0,     0,     0,     0,
       0,    67,   200,     0,     0,     0,   192,   193,   211,     0,
     209,     0,     0,     0,   361,     0,     0,   356,   358,     0,
     284,     0,    78,    77,    79,    80,   206,   167,   148,     0,
     254,   151,     0,     0,     0,    76,     0,   118,     0,     0,
       0,     0,     0,     0,     0,   280,   264,     0,     0,     0,
       0,     0,   349,     0,   350,   252,     0,     0,   311,   317,
       0,     0,   308,     0,   312,   306,     0,   307,     0,   440,
     373,   380,   498,   382,     0,     0,     0,     0,   261,   419,
      55,   418,   420,   385,     0,   114,   260,   179,   168,   169,
     157,     0,   326,   328,     0,     0,   256,   257,   278,   289,
     367,     0,   363,   366,   369,   292,    26,    25,     0,     9,
      10,    43,     0,     0,    40,    45,   287,   286,     0,     0,
     297,   273,   276,     0,     0,   201,     0,     0,   204,     0,
     395,   399,   205,     0,   398,   393,   394,   406,   431,   134,
     134,   137,   124,   183,   187,    63,     0,   362,   359,   347,
       0,   352,   355,   353,     0,     0,    75,   153,   150,   154,
     295,     0,     0,     0,    86,    72,   165,   167,     0,    73,
      68,     0,     0,   271,   263,   265,   375,     0,     0,     0,
     164,   389,   377,   262,   294,   429,   388,   319,   321,   325,
     310,   323,   322,   314,   313,   268,     0,     0,     0,     0,
       0,   127,     0,     0,   233,   223,   241,   255,     0,     0,
     427,   156,   301,     0,     0,    29,     0,     0,     0,   331,
       0,   344,     0,   333,   337,     0,     0,   332,   438,   240,
     238,     0,     0,     0,   213,   215,   218,   177,   212,   107,
       0,   129,   133,     0,   130,     0,   212,   360,   354,   285,
     144,   144,   147,   149,   101,   220,   119,   120,     0,    91,
       0,     0,     0,   272,   390,   391,     0,   318,   171,     0,
       0,   421,   386,    54,   126,   115,   214,   227,   229,     0,
       0,   241,     0,    69,   242,   244,   259,   327,   365,   368,
       0,    47,   345,   334,   329,   336,     0,     0,   338,   157,
     342,   330,   176,     0,     0,   203,   108,     0,     0,   121,
     105,   121,   135,   132,   136,     0,   109,   139,   143,     0,
     140,     0,    87,     0,     0,    71,   166,     0,   324,   320,
     266,     0,   267,     0,   233,     0,   234,   188,   196,   231,
     285,   285,    70,    84,    82,    83,     0,     0,   245,   248,
     407,   243,    48,   335,     0,   157,   340,   341,     0,   216,
       0,   116,   106,     0,     0,   104,     0,   103,   131,     0,
     110,     0,   145,   142,   146,     0,   100,   100,    92,    64,
     172,   232,   228,     0,     0,   189,     0,     0,   225,     0,
       0,   249,   184,   207,     0,   246,     0,   247,     0,   339,
       0,     0,     0,   102,   122,     0,     0,   198,   296,   111,
     141,    88,    90,     0,     0,    99,     0,     0,   234,   230,
     235,   221,   285,     0,   222,     0,   250,    85,   343,   217,
     219,   117,     0,   198,    89,    95,    93,     0,    98,    96,
      94,   224,     0,   208,   123,    97,   226
  };

  const short
  parser::yypgoto_[] =
  {
    -646,  -646,  -646,  -646,  -646,  -646,  -646,    51,  -646,  -646,
    -416,  -646,   530,  -646,  -646,  -646,  -144,   559,  -646,  -646,
    -646,  -646,  -646,  -646,  -646,  -646,  -646,  -646,  -646,  -646,
    -646,  -646,  -646,  -646,  -646,  -646,  -646,  -646,  -646,  -646,
    -646,  -104,  -646,  -646,  -646,   -56,  -228,  -646,  -646,  -646,
    -646,  -646,  -646,  -646,  -646,    18,   399,  -646,    26,   200,
    -646,  -646,   -29,    96,  -646,  -646,   482,  -646,  -296,  -412,
     693,  -646,  -646,  -646,  -334,    31,  -153,   168,  -646,  -646,
      50,   326,   -48,  -646,   -84,  -646,   -91,  -646,  -377,  -646,
    -646,  -646,  -642,  -185,   437,   -58,  -646,   511,    97,   208,
    -264,  -505,  -646,    19,   -47,  -646,  -646,    38,  -646,    -7,
    -646,  -646,   253,   100,  -646,    94,    43,   713,  -221,   493,
    -646,   454,  -646,   300,  -646,   264,   -80,   720,   -22,  -278,
    -210,  -646,   -63,   -81,  -646,  -646,   -92,  -646,  -646,  -646,
    -646,   101,  -646,  -646,  -406,  -646,   107,  -646,  -646,   104,
    -646,  -646,   507,  -646,   -66,   546,   257,  -275,  -646,   192,
    -646,  -646,  -646,   361,    32,  -646,   -95,  -645,   -85,  -646,
     363,   -69,  -646,  -646,  -646,   -12,  -646,  -175,  -646,   219,
    -646,  -137,  -646,  -646,  -646,  -422,  -646,  -189,  -268,   -10,
    -196,     1,  -646,  -646,    -9,   -40,   -50,   -87,  -646,  -195,
     -83,   -65,  -243,  -646,  -293,    -8,  -111
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   171,     6,    10,    19,    30,
      69,    70,    71,   168,   326,   327,    72,    84,    11,    20,
      21,    32,    82,   332,   474,   475,   295,   122,   438,    33,
      34,   123,   124,   125,   126,   235,   127,   228,   707,   757,
     619,   682,   771,   685,   738,   774,   775,   601,   667,   731,
     677,   128,   565,   763,   524,   725,   199,   298,   602,   603,
     501,   361,   678,   679,   612,   516,   389,   231,   232,   456,
      27,    36,   411,   525,   386,   446,   129,   629,   200,   201,
     387,   448,   348,   695,   349,   753,   204,   205,   696,   206,
     366,   207,   697,   208,   388,   754,   369,   354,   487,   594,
     595,   616,   641,   747,   748,   566,   637,   638,   639,   699,
     341,   342,   343,   643,   644,   645,   708,   390,   604,   304,
     305,   306,   131,   243,   244,   374,   177,   392,   178,   179,
     749,   180,   134,   135,   136,   137,   282,   283,   269,   270,
     548,   451,   452,   480,   582,   583,   584,   658,   659,   660,
     585,   375,   256,   257,   222,   376,   377,   378,   461,   462,
     463,   138,   139,   250,   251,   140,   141,   439,   271,   540,
     209,   210,    73,   211,   709,   212,    75,   213,   214,   440,
     441,   308,   272,   309,   273,   215,   216,   217,   142,   143,
      77,    78,   310,   274,   275,   312,   163,    79,   164,   145,
     146,   277,   278,   147,    24,     9,   288
  };

  const short
  parser::yytable_[] =
  {
     218,   290,   481,   203,   359,   395,   408,   237,   391,   391,
     252,   218,   133,   381,   236,   362,   355,    74,   268,    76,
     241,   367,   253,   328,   162,   255,   258,   453,   260,   238,
     267,   267,   449,   150,   240,   144,   340,   470,   567,   202,
     337,    22,   472,   291,     1,   259,   287,   307,    22,    22,
      22,   746,   443,   314,   745,   161,    13,   577,   635,   482,
     276,   286,   299,   455,   526,   596,    22,   416,   417,    22,
     402,   285,   690,   586,   172,   507,   333,   692,  -437,   425,
     356,   357,    22,   419,   406,   426,   513,   334,   169,   420,
     170,   455,   307,   427,   247,   267,   718,   703,   300,   428,
     519,   596,   284,   218,   218,   561,   218,   454,   417,    25,
     287,   691,   403,   218,  -437,   686,   691,    12,   719,   218,
     302,   421,    17,   311,     2,   286,   704,   705,   459,   242,
     418,    29,   218,   746,    26,   162,   745,   407,   745,   473,
     482,   218,   237,   237,    23,   549,   218,   218,   656,   396,
     397,    23,    23,    23,    74,    74,    76,    76,   562,   571,
     650,    18,   512,   518,   673,   398,   284,   -74,   311,    23,
     663,   664,    23,   412,   653,   511,   255,   148,   314,    31,
     733,   358,   307,   782,   706,    23,   587,   149,   512,   409,
     344,   484,   424,  -438,   431,    66,   797,   517,   802,    68,
     432,   399,    35,   -74,   218,   289,   162,   133,   133,   281,
     518,   218,   218,   760,   203,   761,   259,   528,   764,    37,
     329,   330,   628,   628,   261,   410,   218,   300,    66,  -438,
     144,   144,    68,   404,   410,   465,   153,   161,   154,   155,
     347,   353,   184,   153,   156,   154,   155,   716,   672,   218,
     202,   156,   351,   706,   186,   442,     7,   791,   311,   495,
       8,   673,   437,   622,    66,   432,   157,   264,    68,   368,
      38,   596,   237,   157,   504,    80,   218,   218,   218,   502,
      14,    15,   435,   436,   195,   196,   458,   337,   197,   198,
     522,   523,   509,   661,   647,   261,   732,   613,   220,   483,
     221,   344,   596,   759,   767,   464,   508,   307,   781,   733,
     248,   218,   674,   218,   249,   252,   112,   443,   229,    83,
     230,   782,   796,   471,   575,   113,   280,   253,    81,   551,
     529,   552,   281,   340,   553,   797,   554,   364,   264,   307,
     365,   267,   301,   267,   793,   302,   267,   165,   267,   447,
     176,   688,   166,   621,   559,   223,   364,   740,   433,   365,
    -180,   239,   432,   621,   496,   261,   778,   301,   432,   780,
     302,   276,   670,   276,   266,   266,   276,   153,   276,   154,
     155,   453,   478,   311,   479,   156,   167,   625,   734,   680,
     680,   281,   224,   225,   226,   227,   499,   218,   500,   675,
     218,   219,   218,   303,   563,   564,   218,   157,   264,   723,
     724,   159,   265,   173,   503,   311,   723,   726,   537,   218,
     292,   293,   538,   610,   539,   611,   242,   356,   357,   683,
     656,   245,   657,    66,   218,   218,   218,    68,   558,   266,
     722,   700,   294,   701,   338,   736,   297,   737,   527,   315,
     447,   316,   318,   443,   698,   317,    74,   784,    76,   319,
     765,    74,   765,    76,   320,   321,   322,   323,   806,   218,
     218,   331,   335,   360,   281,   363,   218,   133,   133,   382,
     380,   383,   649,   799,   800,   384,   385,   344,   394,   401,
     405,   400,   262,   422,   267,   423,   133,   652,   429,  -457,
     144,   144,   430,   769,   218,   218,   218,   444,   466,   237,
     467,   698,   680,   218,   469,   153,   671,   154,   155,   144,
     442,   468,   476,   156,   276,   485,   488,   477,   489,   490,
     486,   218,   344,   218,   589,   443,   491,   492,   493,   597,
     494,  -346,   497,   598,   505,   157,   772,   510,   498,   159,
     514,   506,   218,   515,   534,   521,   606,   533,   698,   531,
     535,   698,   536,   464,    74,   450,    76,   532,   543,   805,
     545,   614,   615,   555,   546,   556,   237,   237,   557,   560,
     218,   218,   794,   721,   396,   455,   568,   569,   133,   133,
     572,   355,   570,   698,   573,   698,   574,   588,   576,   365,
     218,   618,   620,   683,   218,   623,   218,   346,   626,   218,
     627,   144,   144,   615,   631,   237,   634,   632,   636,   218,
     640,   642,   752,   651,   655,   654,   665,   684,   666,   687,
     710,   218,   694,   218,   712,   720,   218,   218,   729,   218,
     237,   662,   735,   730,   218,   237,   237,   396,   218,   218,
     218,   133,   396,   396,   252,   356,   218,   743,   520,   218,
     756,   559,   758,   762,   113,   237,   253,   785,   789,   218,
     615,   786,   787,   790,   144,   218,   792,   544,  -218,   218,
     795,   777,   296,   547,   218,   550,   798,   266,   804,   727,
     266,   218,   266,   218,   237,   710,   324,   445,   218,   728,
     605,   752,   261,   336,   770,   218,   237,   681,    28,   151,
     218,   133,   393,   396,   153,   218,   154,   155,   739,   152,
     750,   153,   156,   154,   155,   630,   530,   803,   434,   156,
     370,   617,   742,   693,   144,   801,   779,   447,   711,   590,
     303,   702,   581,   581,   157,   264,   261,   130,   159,   265,
     755,   157,   158,   415,   132,   159,   160,   714,   153,   457,
     154,   155,   713,   717,   414,   648,   156,   379,   615,   608,
     615,   542,   541,   615,   607,     0,   614,     0,   609,   633,
       0,   783,     0,     0,   303,     0,     0,     0,   157,   264,
       0,     0,   159,   265,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   615,     0,     0,     0,   353,     0,     0,     0,
      85,    39,    86,    87,    88,    89,     0,    90,     0,    40,
      91,     0,   646,    92,    93,    94,    95,    96,   266,    97,
       0,    42,     0,    98,   581,    43,    99,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,   101,    58,    59,
      60,    61,    62,    63,   102,     0,     0,     0,     0,   103,
     104,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   689,     0,   105,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,   107,     0,     0,   108,   109,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   581,
       0,   715,   110,     0,     0,     0,   111,     0,   112,     0,
       0,     0,     0,     0,     0,     0,    65,   113,     0,     0,
      67,   114,     0,     0,     0,     0,   115,   116,   117,   118,
       0,     0,   119,     0,     0,     0,   120,   121,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    85,    39,    86,
       0,   599,     0,     0,    90,     0,    40,    91,     0,     0,
      92,    93,    94,     0,    96,     0,     0,     0,    42,     0,
     600,     0,    43,   768,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,   102,   788,     0,     0,     0,   103,   104,     0,     0,
       0,     0,     0,     0,     0,   184,     0,     0,     0,     0,
     105,     0,   350,     0,     0,   351,   106,   186,     0,     0,
       0,     0,   107,     0,     0,   108,   109,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   624,     0,     0,   110,
       0,     0,   281,   111,     0,   112,     0,   195,   196,     0,
       0,   197,   198,    65,   113,     0,     0,    67,   114,     0,
       0,     0,     0,   115,   116,   117,   118,     0,     0,   119,
      85,    39,    86,   120,   121,     0,     0,    90,     0,    40,
      91,     0,     0,    92,    93,    94,     0,    96,     0,     0,
       0,    42,     0,   676,     0,    43,     0,    44,    45,    46,
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
       0,     0,   119,    85,    39,    86,   120,   121,     0,     0,
      90,     0,    40,    91,     0,     0,    92,    93,    94,     0,
      96,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
     101,    58,    59,    60,    61,    62,    63,   102,     0,     0,
       0,     0,   103,   104,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   105,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,   107,     0,
       0,   108,   109,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,   111,
       0,   112,     0,     0,     0,     0,     0,     0,     0,    65,
     113,     0,     0,    67,   114,     0,     0,     0,     0,   115,
     116,   117,   118,     0,    22,   119,    85,    39,    86,   120,
     121,     0,     0,    90,     0,    40,    91,     0,     0,     0,
       0,     0,     0,    96,     0,     0,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   100,
      50,    51,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,   101,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,   103,   174,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,   107,     0,     0,   108,   578,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    23,   110,     0,
       0,     0,   175,     0,   112,     0,     0,     0,   580,     0,
       0,     0,    65,   113,     0,     0,    67,   114,     0,     0,
       0,     0,   115,   116,   117,   118,     0,     0,   119,    85,
      39,    86,     0,     0,     0,     0,    90,     0,    40,    91,
       0,     0,     0,     0,     0,     0,    96,     0,     0,     0,
      42,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,   101,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,   103,   174,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   261,     0,     0,   106,     0,
       0,     0,     0,     0,   107,     0,     0,   279,     0,   154,
     155,     0,     0,     0,     0,   156,     0,     0,     0,     0,
       0,   110,     0,     0,     0,   175,   280,   112,     0,     0,
       0,     0,   281,   263,     0,    65,   113,   157,   264,    67,
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
       0,     0,     0,     0,   110,   262,     0,     0,   175,     0,
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
       0,   261,     0,     0,   106,     0,     0,     0,     0,     0,
     107,     0,     0,   108,     0,   154,   155,     0,     0,     0,
       0,   156,     0,     0,     0,     0,     0,   110,     0,     0,
       0,   175,     0,   112,     0,     0,     0,     0,     0,   263,
       0,    65,   113,   157,   264,    67,   114,   159,   265,     0,
       0,   115,   116,   117,   118,     0,     0,   119,    85,    39,
      86,     0,     0,     0,     0,    90,     0,    40,    91,     0,
       0,     0,     0,     0,     0,    96,     0,     0,     0,    42,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,   101,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,   103,   174,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,   107,     0,     0,   108,   578,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   579,     0,     0,
     110,     0,     0,     0,   175,     0,   112,     0,     0,     0,
     580,     0,     0,     0,    65,   113,     0,     0,    67,   114,
       0,     0,     0,     0,   115,   116,   117,   118,     0,     0,
     119,    85,    39,    86,     0,     0,     0,     0,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,   371,     0,
       0,     0,    42,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,     0,
      54,    55,    56,     0,   372,    57,     0,     0,   101,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
     103,   174,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,   107,     0,     0,   108,
     373,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,     0,   175,     0,   112,
       0,     0,     0,     0,     0,     0,     0,    65,   113,     0,
       0,    67,   114,     0,     0,     0,     0,   115,   116,   117,
     118,     0,     0,   119,    85,    39,    86,     0,     0,     0,
       0,    90,     0,    40,    91,     0,     0,     0,     0,     0,
       0,    96,     0,     0,     0,    42,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,   101,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,   103,   174,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,   107,
       0,     0,   108,   578,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
     175,     0,   112,     0,     0,     0,   580,     0,     0,     0,
      65,   113,     0,     0,    67,   114,     0,     0,     0,     0,
     115,   116,   117,   118,     0,     0,   119,    85,    39,    86,
       0,     0,     0,     0,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,   371,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,   103,   174,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,   107,     0,     0,   108,   373,     0,     0,     0,
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
       0,     0,     0,     0,     0,   107,     0,     0,   108,   578,
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
       0,     0,   103,   174,     0,     0,     0,     0,     0,     0,
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
      57,     0,     0,   101,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,   103,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,   107,     0,     0,   108,     0,     0,     0,     0,     0,
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
       0,     0,   413,     0,   107,     0,     0,     0,   254,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,     0,   175,     0,   112,     0,     0,
       0,     0,     0,     0,     0,    65,   113,     0,     0,    67,
     114,     0,     0,     0,     0,   115,   116,   117,   118,     0,
       0,   119,    85,    39,    86,     0,     0,     0,     0,    90,
       0,    40,    91,     0,     0,     0,     0,     0,     0,    96,
       0,     0,     0,    42,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,   107,     0,     0,
       0,   254,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   110,     0,     0,     0,   175,     0,
     112,     0,     0,     0,     0,     0,     0,     0,    65,   113,
       0,     0,    67,   114,     0,     0,     0,     0,   115,   116,
     117,   118,     0,     0,   119,    85,    39,    86,     0,     0,
       0,     0,    90,     0,    40,    91,     0,     0,     0,     0,
       0,     0,    96,     0,     0,     0,    42,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,   101,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   110,     0,     0,
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
       0,     0,     0,     0,     0,   313,     0,     0,     0,     0,
     110,     0,     0,     0,   175,     0,   112,     0,     0,     0,
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
     106,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,     0,   175,     0,   112,
       0,    39,     0,     0,     0,     0,     0,    65,   113,    40,
       0,    67,   114,     0,     0,     0,     0,   115,   116,   117,
     118,    42,     0,   119,     0,   345,     0,    44,    45,    46,
     181,   182,   183,     0,     0,     0,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   184,     0,     0,     0,
       0,     0,     0,   350,     0,   185,   351,     0,   186,   187,
       0,   188,     0,     0,     0,     0,     0,     0,   189,     0,
       0,     0,   190,     0,     0,     0,   191,   352,   192,     0,
       0,     0,    39,   281,   193,     0,   194,    66,   195,   196,
      40,    68,   197,   198,     0,     0,     0,   233,     0,     0,
       0,     0,    42,     0,     0,     0,     0,     0,    44,    45,
      46,   181,   182,   183,     0,   234,     0,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   184,     0,     0,
       0,     0,     0,     0,     0,     0,   185,     0,     0,   186,
     187,     0,   188,     0,     0,     0,     0,     0,     0,   189,
       0,     0,     0,   190,     0,    39,     0,   191,     0,   192,
       0,     0,     0,    40,     0,   193,     0,   194,    66,   195,
     196,     0,    68,   197,   198,    42,     0,     0,     0,   345,
       0,    44,    45,    46,   181,   182,   183,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     184,     0,     0,     0,     0,     0,     0,     0,     0,   185,
       0,     0,   186,   187,     0,   188,     0,     0,     0,     0,
       0,     0,   189,     0,     0,     0,   190,   346,     0,    39,
     191,     0,   192,     0,     0,     0,     0,    40,   193,     0,
     194,    66,   195,   196,   668,    68,   197,   198,     0,    42,
       0,     0,     0,     0,     0,    44,    45,    46,   181,   182,
     183,     0,   669,     0,    52,    53,     0,    54,    55,    56,
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
       0,     0,     0,   190,     0,    39,     0,   191,     0,   192,
       0,     0,     0,    40,     0,   193,     0,   194,    66,   195,
     196,     0,    68,   197,   198,    42,     0,     0,     0,     0,
       0,    44,    45,    46,   181,   182,   183,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     184,     0,     0,     0,     0,     0,     0,     0,     0,   185,
       0,     0,   186,   187,     0,   188,     0,     0,     0,     0,
       0,     0,   189,     0,     0,     0,   190,     0,    39,     0,
     191,   751,   192,     0,     0,     0,    40,     0,   193,     0,
     194,    66,   195,   196,     0,    68,   197,   198,    42,     0,
       0,     0,   345,     0,    44,    45,    46,   181,   182,   183,
       0,     0,     0,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   184,     0,     0,     0,     0,     0,     0,
       0,     0,   185,     0,     0,   186,   187,     0,   188,     0,
       0,     0,     0,     0,     0,   189,     0,     0,     0,   190,
       0,    39,     0,   766,     0,   192,     0,     0,     0,    40,
       0,   193,     0,   194,    66,   195,   196,     0,    68,   197,
     198,    42,     0,     0,     0,     0,     0,    44,    45,    46,
     181,   182,   183,     0,     0,     0,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   773,   184,     0,     0,     0,
       0,     0,     0,     0,     0,   185,     0,     0,   186,   187,
       0,   188,     0,     0,     0,     0,     0,     0,   189,     0,
       0,     0,   190,     0,    39,     0,   191,     0,   192,     0,
       0,     0,    40,     0,   193,     0,   194,    66,   195,   196,
       0,    68,   197,   198,    42,     0,     0,     0,     0,     0,
      44,    45,    46,   181,   182,   183,     0,     0,     0,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   776,   184,
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
       0,     0,   184,     0,     0,     0,     0,     0,     0,     0,
       0,   185,     0,     0,   186,   187,     0,   188,     0,     0,
       0,     0,     0,     0,   189,     0,     0,     0,   190,     0,
      39,     0,   191,     0,   192,     0,     0,     0,    40,     0,
     193,     0,   194,    66,   195,   196,     0,    68,   197,   198,
      42,     0,     0,     0,     0,     0,    44,    45,    46,   181,
     182,   183,     0,     0,     0,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   261,     0,     0,     0,     0,
       0,     0,     0,     0,   185,     0,  -181,     0,   187,     0,
     188,     0,     0,     0,     0,     0,     0,   189,     0,     0,
       0,   190,    39,     0,     0,   191,     0,   192,     0,     0,
      40,     0,     0,   744,     0,   194,    66,     0,   264,     0,
      68,     0,    42,     0,     0,     0,     0,     0,    44,    45,
      46,   181,   182,   183,     0,     0,     0,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   261,     0,     0,
       0,     0,     0,     0,     0,     0,   185,     0,     0,     0,
     187,     0,   188,     0,     0,     0,     0,     0,     0,   189,
       0,     0,     0,   190,    39,     0,     0,   191,     0,   192,
       0,     0,    40,     0,     0,   744,     0,   194,    66,     0,
     264,     0,    68,     0,    42,     0,     0,     0,     0,     0,
      44,    45,    46,   181,   182,   183,     0,     0,     0,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   185,     0,
       0,     0,   187,     0,   188,     0,     0,     0,     0,     0,
       0,   189,     0,     0,     0,   190,    39,     0,     0,   191,
       0,   192,     0,     0,    40,     0,     0,     0,     0,   194,
      66,     0,     0,    41,    68,     0,    42,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,     0,    50,
      51,    52,    53,    39,    54,    55,    56,     0,     0,    57,
       0,    40,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,    42,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,     0,    50,    51,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,    39,     0,     0,     0,     0,     0,
       0,    64,    40,     0,     0,     0,     0,   325,     0,     0,
       0,    65,    66,     0,    42,    67,    68,     0,    43,     0,
      44,    45,    46,    47,    48,    49,     0,    50,    51,    52,
      53,    39,    54,    55,    56,     0,     0,    57,    64,    40,
       0,    58,    59,    60,    61,    62,    63,     0,    65,    66,
       0,    42,    67,    68,     0,    43,     0,    44,    45,    46,
      47,    48,    49,     0,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,    39,     0,     0,     0,     0,     0,     0,    64,
      40,     0,     0,     0,     0,     0,     0,     0,     0,    65,
      66,     0,    42,    67,    68,     0,     0,     0,    44,    45,
      46,   181,   182,   183,     0,     0,     0,    52,    53,    39,
      54,    55,    56,     0,     0,    57,     0,    40,     0,    58,
      59,    60,    61,    62,    63,     0,    65,   113,     0,    42,
      67,   114,     0,    43,     0,    44,    45,    46,    47,    48,
      49,     0,    50,    51,    52,    53,    39,    54,    55,    56,
       0,     0,    57,     0,    40,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,    42,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,     0,    50,
      51,    52,    53,   460,    54,    55,    56,   194,    66,    57,
       0,     0,    68,    58,    59,    60,    61,    62,    63,     0,
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
       0,     0,    42,     0,   591,     0,   592,     0,    44,    45,
      46,   181,   182,   183,   593,    39,     0,    52,    53,     0,
      54,    55,    56,    40,   194,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,    42,     0,     0,     0,     0,
       0,    44,    45,    46,   181,   182,   183,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,    39,     0,   741,     0,   592,
       0,     0,     0,    40,     0,     0,     0,   593,     0,     0,
       0,     0,     0,     0,     0,    42,     0,   194,     0,     0,
       0,    44,    45,    46,   181,   182,   183,     0,    39,     0,
      52,    53,   592,    54,    55,    56,    40,     0,    57,     0,
     593,     0,    58,    59,    60,    61,    62,    63,    42,     0,
     194,     0,     0,     0,    44,    45,    46,   181,   182,   183,
       0,     0,     0,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     593,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     194,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   194
  };

  const short
  parser::yycheck_[] =
  {
      87,   112,   336,    87,   193,   233,   249,    98,   229,   230,
     105,    98,    34,   223,    98,   200,   191,    29,   110,    29,
     101,   206,   105,   167,    64,   106,   107,   302,   109,    98,
     110,   111,   300,    41,   100,    34,   189,   330,   450,    87,
     177,     1,    12,    19,    21,   108,   111,   132,     1,     1,
       1,   696,   295,   134,   696,    64,     5,   473,   563,   337,
     110,   111,    80,    27,   398,   487,     1,   263,   263,     1,
      89,   111,    77,   479,    82,   371,   119,    77,    80,   110,
     191,   192,     1,    78,    77,   116,   379,   130,   101,    84,
     103,    27,   177,   110,   104,   175,    80,    22,   116,   116,
     393,   523,   111,   190,   191,    77,   193,   303,   303,   105,
     175,   116,   131,   200,   116,   620,   116,     0,   102,   206,
      84,   116,   102,   132,   101,   175,    51,    52,   313,   105,
     267,   109,   219,   778,   130,   175,   778,   130,   780,   109,
     418,   228,   233,   234,   104,   420,   233,   234,    84,   233,
     234,   104,   104,   104,   166,   167,   166,   167,   130,   455,
     576,   115,   115,   115,   115,    80,   175,    80,   177,   104,
     592,   593,   104,   254,   580,   102,   257,   106,   259,    14,
     115,   193,   267,   115,   109,   104,   479,   116,   115,    80,
     189,    80,   272,    80,   110,   120,   115,   102,    80,   124,
     116,   116,   130,   116,   291,   112,   246,   229,   230,   116,
     115,   298,   299,   718,   298,   720,   279,   109,   723,    77,
     169,   170,   556,   557,    79,   116,   313,   116,   120,   116,
     229,   230,   124,   243,   116,   316,    91,   246,    93,    94,
     190,   191,    79,    91,    99,    93,    94,   659,   102,   336,
     298,    99,    89,   109,    91,   295,   120,   762,   267,   110,
     124,   115,   117,   531,   120,   116,   121,   122,   124,   219,
      77,   693,   363,   121,   365,    27,   363,   364,   365,   363,
      65,    66,   292,   293,   121,   122,   308,   424,   125,   126,
      80,    81,   373,   586,   569,    79,   102,   518,   101,   339,
     103,   300,   724,   715,   726,   315,   372,   392,   102,   115,
     105,   398,   605,   400,   109,   410,   111,   560,   101,   115,
     103,   115,   102,   331,   468,   120,   110,   410,    24,   421,
     399,   423,   116,   486,   426,   115,   428,    86,   122,   424,
      89,   421,    81,   423,   766,    84,   426,   110,   428,   299,
      86,   626,   116,   528,   437,    91,    86,   691,   112,    89,
      90,    14,   116,   538,   112,    79,   743,    81,   116,   746,
      84,   421,   600,   423,   110,   111,   426,    91,   428,    93,
      94,   656,   101,   392,   103,    99,   109,   112,   681,   610,
     611,   116,    72,    73,    74,    75,   101,   484,   103,   609,
     487,   109,   489,   117,    80,    81,   493,   121,   122,    80,
      81,   125,   126,   115,   364,   424,    80,    81,   105,   506,
     120,   121,   109,   101,   111,   103,   105,   538,   539,   618,
      84,   130,    86,   120,   521,   522,   523,   124,   437,   175,
     668,   101,   131,   103,   180,   101,    71,   103,   398,   101,
     400,    87,   110,   696,   639,   130,   468,   750,   468,   110,
     724,   473,   726,   473,   110,   110,   110,   110,   802,   556,
     557,     4,    23,    27,   116,    90,   563,   499,   500,    77,
     115,    77,   574,   776,   777,    77,    77,   486,    15,    41,
      77,    81,   106,   106,   574,   116,   518,   578,   110,   110,
     499,   500,   110,   731,   591,   592,   593,   130,   110,   600,
     110,   696,   733,   600,   102,    91,   600,    93,    94,   518,
     560,   116,    77,    99,   574,   102,   106,    77,    90,   110,
     116,   618,   531,   620,   484,   778,   110,   110,   116,   489,
     110,    85,   117,   493,   110,   121,   735,    85,   117,   125,
      25,   116,   639,    27,    77,    81,   506,   106,   743,   130,
      77,   746,   110,   573,   576,   301,   576,   131,    77,   797,
     117,   521,   522,    77,   117,    80,   667,   668,    80,   116,
     667,   668,   771,   667,   668,    27,    81,   116,   610,   611,
     102,   766,   117,   778,   116,   780,    81,   110,   109,    89,
     687,    84,    80,   792,   691,   106,   693,   106,   116,   696,
      78,   610,   611,   563,   117,   706,    77,   117,    28,   706,
      27,     9,   706,   110,   115,   102,   110,    27,    37,    80,
     642,   718,    84,   720,   110,    80,   723,   724,    11,   726,
     731,   591,    86,    19,   731,   736,   737,   731,   735,   736,
     737,   673,   736,   737,   749,   766,   743,    90,   394,   746,
      53,   744,    86,    80,   120,   756,   749,   116,   102,   756,
     620,   110,   756,   110,   673,   762,    84,   413,    84,   766,
     102,   737,   123,   419,   771,   421,   115,   423,   792,   671,
     426,   778,   428,   780,   785,   707,   166,   298,   785,   673,
     500,   785,    79,    80,   733,   792,   797,   611,    15,    79,
     797,   733,   230,   797,    91,   802,    93,    94,   687,    89,
     701,    91,    99,    93,    94,   557,   400,   785,   291,    99,
     219,   523,   694,   636,   733,   782,   743,   687,   644,   486,
     117,   641,   478,   479,   121,   122,    79,    34,   125,   126,
     707,   121,   122,   260,    34,   125,   126,   656,    91,   305,
      93,    94,   655,   659,   257,   573,    99,   221,   718,   512,
     720,   410,   409,   723,   510,    -1,   726,    -1,   514,   560,
      -1,   749,    -1,    -1,   117,    -1,    -1,    -1,   121,   122,
      -1,    -1,   125,   126,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   762,    -1,    -1,    -1,   766,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,    -1,    10,    -1,    12,
      13,    -1,   568,    16,    17,    18,    19,    20,   574,    22,
      -1,    24,    -1,    26,   580,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   627,    -1,    76,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   655,
      -1,   657,   105,    -1,    -1,    -1,   109,    -1,   111,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   119,   120,    -1,    -1,
     123,   124,    -1,    -1,    -1,    -1,   129,   130,   131,   132,
      -1,    -1,   135,    -1,    -1,    -1,   139,   140,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
      -1,     7,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      16,    17,    18,    -1,    20,    -1,    -1,    -1,    24,    -1,
      26,    -1,    28,   729,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    57,   758,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      76,    -1,    86,    -1,    -1,    89,    82,    91,    -1,    -1,
      -1,    -1,    88,    -1,    -1,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   110,    -1,    -1,   105,
      -1,    -1,   116,   109,    -1,   111,    -1,   121,   122,    -1,
      -1,   125,   126,   119,   120,    -1,    -1,   123,   124,    -1,
      -1,    -1,    -1,   129,   130,   131,   132,    -1,    -1,   135,
       3,     4,     5,   139,   140,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    16,    17,    18,    -1,    20,    -1,    -1,
      -1,    24,    -1,    26,    -1,    28,    -1,    30,    31,    32,
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
      -1,    -1,   135,     3,     4,     5,   139,   140,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    16,    17,    18,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,
      -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,
     120,    -1,    -1,   123,   124,    -1,    -1,    -1,    -1,   129,
     130,   131,   132,    -1,     1,   135,     3,     4,     5,   139,
     140,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,
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
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    -1,    93,
      94,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,
      -1,   105,    -1,    -1,    -1,   109,   110,   111,    -1,    -1,
      -1,    -1,   116,   117,    -1,   119,   120,   121,   122,   123,
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
      -1,    -1,    -1,    -1,   105,   106,    -1,    -1,   109,    -1,
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
      -1,    79,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    91,    -1,    93,    94,    -1,    -1,    -1,
      -1,    99,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,
      -1,   109,    -1,   111,    -1,    -1,    -1,    -1,    -1,   117,
      -1,   119,   120,   121,   122,   123,   124,   125,   126,    -1,
      -1,   129,   130,   131,   132,    -1,    -1,   135,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,
     105,    -1,    -1,    -1,   109,    -1,   111,    -1,    -1,    -1,
     115,    -1,    -1,    -1,   119,   120,    -1,    -1,   123,   124,
      -1,    -1,    -1,    -1,   129,   130,   131,   132,    -1,    -1,
     135,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    46,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,   111,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,   120,    -1,
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
     109,    -1,   111,    -1,    -1,    -1,   115,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    92,
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
      -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
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
      47,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    86,    -1,    88,    -1,    -1,    -1,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   105,    -1,    -1,    -1,   109,    -1,   111,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   119,   120,    -1,    -1,   123,
     124,    -1,    -1,    -1,    -1,   129,   130,   131,   132,    -1,
      -1,   135,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,
      -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,
     111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,   120,
      -1,    -1,   123,   124,    -1,    -1,    -1,    -1,   129,   130,
     131,   132,    -1,    -1,   135,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,    -1,
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
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,   111,
      -1,     4,    -1,    -1,    -1,    -1,    -1,   119,   120,    12,
      -1,   123,   124,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,    24,    -1,   135,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    86,    -1,    88,    89,    -1,    91,    92,
      -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,    -1,   105,    -1,    -1,    -1,   109,   110,   111,    -1,
      -1,    -1,     4,   116,   117,    -1,   119,   120,   121,   122,
      12,   124,   125,   126,    -1,    -1,    -1,    19,    -1,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    -1,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,
      92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,   105,    -1,     4,    -1,   109,    -1,   111,
      -1,    -1,    -1,    12,    -1,   117,    -1,   119,   120,   121,
     122,    -1,   124,   125,   126,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    -1,    91,    92,    -1,    94,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,   105,   106,    -1,     4,
     109,    -1,   111,    -1,    -1,    -1,    -1,    12,   117,    -1,
     119,   120,   121,   122,    19,   124,   125,   126,    -1,    24,
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
      -1,    -1,    -1,   105,    -1,     4,    -1,   109,    -1,   111,
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
     109,   110,   111,    -1,    -1,    -1,    12,    -1,   117,    -1,
     119,   120,   121,   122,    -1,   124,   125,   126,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    79,
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
      -1,    88,    -1,    -1,    91,    92,    -1,    94,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,    -1,
       4,    -1,   109,    -1,   111,    -1,    -1,    -1,    12,    -1,
     117,    -1,   119,   120,   121,   122,    -1,   124,   125,   126,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    -1,    92,    -1,
      94,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,   105,     4,    -1,    -1,   109,    -1,   111,    -1,    -1,
      12,    -1,    -1,   117,    -1,   119,   120,    -1,   122,    -1,
     124,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,
      92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,   105,     4,    -1,    -1,   109,    -1,   111,
      -1,    -1,    12,    -1,    -1,   117,    -1,   119,   120,    -1,
     122,    -1,   124,    -1,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    -1,    -1,    -1,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      -1,    -1,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,    -1,   105,     4,    -1,    -1,   109,
      -1,   111,    -1,    -1,    12,    -1,    -1,    -1,    -1,   119,
     120,    -1,    -1,    21,   124,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,     4,    42,    43,    44,    -1,    -1,    47,
      -1,    12,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
      -1,   109,    12,    -1,    -1,    -1,    -1,    78,    -1,    -1,
      -1,   119,   120,    -1,    24,   123,   124,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,     4,    42,    43,    44,    -1,    -1,    47,   109,    12,
      -1,    51,    52,    53,    54,    55,    56,    -1,   119,   120,
      -1,    24,   123,   124,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,   109,
      12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,
     120,    -1,    24,   123,   124,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,     4,
      42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,    51,
      52,    53,    54,    55,    56,    -1,   119,   120,    -1,    24,
     123,   124,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,     4,    42,    43,    44,
      -1,    -1,    47,    -1,    12,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,    78,    42,    43,    44,   119,   120,    47,
      -1,    -1,   124,    51,    52,    53,    54,    55,    56,    -1,
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
       0,    21,   101,   142,   143,   144,   147,   120,   124,   346,
     148,   159,     0,   148,    65,    66,   145,   102,   115,   149,
     160,   161,     1,   104,   345,   105,   130,   211,   211,   109,
     150,    14,   162,   170,   171,   130,   212,    77,    77,     4,
      12,    21,    24,    28,    30,    31,    32,    33,    34,    35,
      37,    38,    39,    40,    42,    43,    44,    47,    51,    52,
      53,    54,    55,    56,   109,   119,   120,   123,   124,   151,
     152,   153,   157,   313,   316,   317,   330,   331,   332,   338,
      27,    24,   163,   115,   158,     3,     5,     6,     7,     8,
      10,    13,    16,    17,    18,    19,    20,    22,    26,    29,
      36,    50,    57,    62,    63,    76,    82,    88,    91,    92,
     105,   109,   111,   120,   124,   129,   130,   131,   132,   135,
     139,   140,   168,   172,   173,   174,   175,   177,   192,   217,
     258,   263,   268,   269,   273,   274,   275,   276,   302,   303,
     306,   307,   329,   330,   332,   340,   341,   344,   106,   116,
     346,    79,    89,    91,    93,    94,    99,   121,   122,   125,
     126,   335,   336,   337,   339,   110,   116,   109,   154,   101,
     103,   146,   346,   115,    63,   109,   266,   267,   269,   270,
     272,    33,    34,    35,    79,    88,    91,    92,    94,   101,
     105,   109,   111,   117,   119,   121,   122,   125,   126,   197,
     219,   220,   223,   225,   227,   228,   230,   232,   234,   311,
     312,   314,   316,   318,   319,   326,   327,   328,   338,   109,
     101,   103,   295,   266,    72,    73,    74,    75,   178,   101,
     103,   208,   209,    19,    37,   176,   225,   227,   312,    14,
     295,   274,   105,   264,   265,   130,   109,   330,   105,   109,
     304,   305,   307,   341,    92,   274,   293,   294,   274,   273,
     274,    79,   106,   117,   122,   126,   266,   267,   277,   279,
     280,   309,   323,   325,   334,   335,   337,   342,   343,    91,
     110,   116,   277,   278,   335,   336,   337,   342,   347,   112,
     347,    19,   264,   264,   131,   167,   158,    71,   198,    80,
     116,    81,    84,   117,   260,   261,   262,   309,   322,   324,
     333,   335,   336,   100,   274,   101,    87,   130,   110,   110,
     110,   110,   110,   110,   153,    78,   155,   156,   157,   148,
     148,     4,   164,   119,   130,    23,    80,   322,   266,   109,
     217,   251,   252,   253,   332,    28,   106,   221,   223,   225,
      86,    89,   110,   221,   238,   318,   347,   347,   316,   328,
      27,   202,   234,    90,    86,    89,   231,   234,   221,   237,
     238,    20,    46,    92,   266,   292,   296,   297,   298,   296,
     115,   271,    77,    77,    77,    77,   215,   221,   235,   207,
     258,   259,   268,   207,    15,   187,   225,   225,    80,   116,
      81,    41,    89,   131,   330,    77,    77,   130,   343,    80,
     116,   213,   274,    86,   293,   260,   331,   340,   322,    78,
      84,   116,   106,   116,   267,   110,   116,   110,   116,   110,
     110,   110,   116,   112,   235,   330,   330,   117,   169,   308,
     320,   321,   336,   343,   130,   197,   216,   221,   222,   329,
     266,   282,   283,   298,   331,    27,   210,   262,   269,   234,
      78,   299,   300,   301,   330,   274,   110,   110,   116,   102,
     345,   346,    12,   109,   165,   166,    77,    77,   101,   103,
     284,   215,   270,   336,    80,   102,   116,   239,   106,    90,
     110,   110,   110,   116,   110,   110,   112,   117,   117,   101,
     103,   201,   225,   221,   227,   110,   116,   209,   295,   274,
      85,   102,   115,   345,    25,    27,   206,   102,   115,   345,
     266,    81,    80,    81,   195,   214,   215,   221,   109,   312,
     222,   130,   131,   106,    77,    77,   110,   105,   109,   111,
     310,   311,   304,    77,   266,   117,   117,   266,   281,   298,
     266,   277,   277,   277,   277,    77,    80,    80,   332,   341,
     116,    77,   130,    80,    81,   193,   246,   210,    81,   116,
     117,   209,   102,   116,    81,   157,   109,   151,    92,   102,
     115,   266,   285,   286,   287,   291,   285,   345,   110,   221,
     253,    99,   101,   109,   240,   241,   326,   221,   221,     7,
      26,   188,   199,   200,   259,   200,   221,   266,   297,   266,
     101,   103,   205,   259,   221,   221,   242,   240,    84,   181,
      80,   318,   329,   106,   110,   112,   116,    78,   215,   218,
     218,   117,   117,   320,    77,   242,    28,   247,   248,   249,
      27,   243,     9,   254,   255,   256,   266,   298,   300,   277,
     151,   110,   274,   285,   102,   115,    84,    86,   288,   289,
     290,   345,   221,   326,   326,   110,    37,   189,    19,    37,
     187,   225,   102,   115,   345,   271,    26,   191,   203,   204,
     259,   204,   182,   328,    27,   184,   242,    80,   298,   266,
      77,   116,    77,   239,    84,   224,   229,   233,   234,   250,
     101,   103,   254,    22,    51,    52,   109,   179,   257,   315,
     316,   256,   110,   287,   282,   266,   210,   290,    80,   102,
      80,   225,   187,    80,    81,   196,    81,   196,   199,    11,
      19,   190,   102,   115,   345,    86,   101,   103,   185,   216,
     215,    99,   248,    90,   117,   233,   308,   244,   245,   271,
     244,   110,   225,   226,   236,   257,    53,   180,    86,   210,
     242,   242,    80,   194,   242,   241,   109,   326,   266,   187,
     203,   183,   328,    78,   186,   187,    78,   186,   229,   250,
     229,   102,   115,   305,   345,   116,   110,   225,   266,   102,
     110,   242,    84,   326,   328,   102,   102,   115,   115,   345,
     345,   245,    80,   236,   182,   187,   215
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
     217,   218,   218,   219,   220,   220,   221,   221,   221,   222,
     223,   224,   225,   225,   226,   227,   228,   228,   229,   229,
     230,   230,   230,   231,   232,   232,   233,   234,   234,   234,
     234,   234,   234,   234,   234,   234,   235,   236,   236,   237,
     237,   238,   238,   239,   239,   240,   240,   240,   241,   241,
     242,   243,   243,   243,   244,   244,   245,   246,   247,   247,
     248,   248,   249,   249,   250,   250,   251,   251,   252,   252,
     253,   254,   254,   255,   255,   256,   256,   256,   257,   257,
     257,   258,   258,   258,   259,   260,   260,   261,   261,   262,
     263,   263,   263,   263,   263,   263,   263,   263,   263,   264,
     264,   265,   265,   266,   266,   267,   267,   268,   268,   269,
     269,   269,   270,   270,   271,   271,   272,   272,   273,   273,
     273,   273,   274,   274,   274,   274,   274,   274,   274,   274,
     274,   275,   275,   276,   276,   276,   276,   276,   276,   276,
     277,   277,   277,   278,   278,   279,   279,   279,   279,   279,
     279,   279,   280,   280,   281,   281,   282,   283,   283,   284,
     284,   284,   284,   285,   285,   286,   286,   286,   287,   288,
     288,   289,   289,   290,   291,   291,   292,   292,   293,   293,
     294,   294,   295,   295,   296,   296,   296,   296,   297,   297,
     298,   298,   298,   299,   299,   300,   300,   300,   301,   301,
     302,   302,   303,   303,   304,   304,   304,   305,   305,   306,
     306,   306,   306,   307,   307,   308,   308,   309,   309,   310,
     310,   310,   311,   311,   311,   311,   311,   312,   312,   312,
     313,   313,   313,   313,   313,   314,   314,   315,   316,   316,
     317,   318,   318,   318,   319,   319,   319,   319,   320,   320,
     321,   321,   322,   322,   323,   323,   324,   324,   325,   325,
     326,   327,   328,   328,   328,   328,   328,   329,   329,   330,
     330,   330,   331,   331,   332,   332,   332,   332,   332,   332,
     332,   332,   333,   333,   334,   334,   335,   336,   336,   337,
     337,   338,   338,   338,   338,   338,   338,   338,   338,   338,
     338,   338,   338,   338,   338,   338,   338,   338,   338,   339,
     339,   339,   340,   340,   341,   342,   342,   343,   343,   344,
     344,   344,   344,   344,   345,   345,   346,   346,   347,   347
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
       0,     3,     4,     3,     3,     2,     3,     0,     1,     0,
       1,     3,     1,     1,     0,     2,     0,     2,     0,     2,
       2,     0,     2,     4,     3,     1,     4,     3,     0,     1,
       1,     3,     2,     1,     0,     3,     3,     2,     0,     1,
       1,     3,     2,     1,     0,     3,     3,     2,     0,     3,
       2,     1,     0,     3,     3,     1,     2,     0,     1,     3,
       3,     1,     0,     0,     2,     1,     3,     1,     1,     3,
       1,     1,     3,     1,     1,     1,     4,     3,     1,     1,
       1,     1,     1,     3,     1,     1,     1,     3,     1,     2,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       2,     3,     2,     5,     3,     3,     1,     1,     3,     1,
       0,     1,     3,     2,     0,     1,     3,     5,     1,     5,
       1,     4,     4,     0,     3,     1,     4,     2,     3,     1,
       4,     2,     3,     0,     1,     3,     0,     1,     3,     1,
       3,     0,     1,     2,     1,     2,     3,     3,     1,     2,
       3,     1,     3,     2,     1,     3,     2,     2,     1,     4,
       3,     3,     4,     4,     3,     4,     6,     6,     4,     0,
       1,     3,     4,     3,     1,     1,     3,     1,     3,     2,
       3,     1,     1,     2,     1,     0,     3,     3,     2,     3,
       2,     1,     3,     2,     4,     4,     8,     4,     2,     2,
       1,     4,     1,     1,     1,     1,     3,     3,     3,     1,
       1,     2,     2,     3,     3,     1,     1,     2,     4,     3,
       5,     3,     3,     3,     3,     1,     1,     3,     1,     3,
       3,     2,     2,     1,     2,     3,     2,     1,     2,     3,
       2,     2,     1,     4,     1,     2,     1,     2,     1,     2,
       2,     1,     3,     3,     3,     2,     1,     0,     1,     2,
       3,     1,     2,     1,     0,     3,     1,     1,     3,     1,
       1,     1,     1,     3,     1,     3,     1,     3,     1,     2,
       3,     2,     3,     1,     2,     1,     3,     1,     3,     1,
       2,     2,     1,     3,     3,     3,     2,     1,     3,     3,
       1,     3,     3,     3,     3,     1,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     1,     1,     1,     1,     1,     3,     1,     3,
       1,     3,     1,     1,     1,     1,     1,     1,     3,     1,
       3,     3,     1,     1,     1,     1,     1,     1,     1,     1,
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
  "sigtypes1", "strict_mark", "strictness", "ctype", "ctypedoc", "context",
  "context_no_ops", "type", "typedoc", "btype", "infixtype",
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
       0,   487,   487,   504,   505,   507,   511,   512,   513,   515,
     516,   518,   519,   522,   524,   525,   526,   534,   535,   537,
     539,   540,   542,   543,   545,   546,   547,   549,   550,   552,
     553,   555,   556,   560,   561,   563,   564,   566,   568,   569,
     571,   584,   585,   587,   588,   590,   591,   595,   596,   601,
     602,   604,   605,   606,   608,   609,   613,   615,   616,   618,
     619,   620,   621,   624,   625,   632,   634,   636,   638,   639,
     641,   642,   645,   647,   648,   651,   652,   656,   657,   658,
     659,   660,   662,   663,   664,   666,   677,   678,   680,   682,
     683,   687,   688,   690,   691,   692,   693,   695,   696,   697,
     698,   700,   703,   705,   706,   708,   709,   711,   711,   713,
     713,   717,   719,   720,   724,   725,   727,   728,   730,   731,
     732,   734,   735,   736,   740,   741,   743,   744,   745,   787,
     788,   790,   791,   792,   793,   795,   796,   798,   799,   801,
     802,   804,   805,   806,   807,   809,   810,   812,   813,   816,
     817,   818,   819,   821,   822,   824,   826,   827,   835,   836,
     838,   839,   840,   853,   854,   856,   857,   859,   861,   863,
     864,   866,   867,   871,   877,   878,   885,   886,   888,   890,
     899,   901,   903,   904,   906,   909,   911,   912,   914,   915,
     917,   918,   919,   921,   923,   924,   931,   938,   939,   940,
     941,   942,   943,   944,   950,   951,   955,   957,   958,   960,
     961,   963,   964,   971,   972,   974,   975,   976,   979,   980,
     998,  1004,  1005,  1006,  1008,  1009,  1011,  1013,  1015,  1016,
    1018,  1019,  1021,  1022,  1024,  1025,  1027,  1028,  1030,  1031,
    1033,  1035,  1036,  1038,  1039,  1041,  1042,  1043,  1045,  1046,
    1047,  1052,  1054,  1055,  1057,  1061,  1062,  1064,  1065,  1069,
    1079,  1080,  1082,  1083,  1084,  1085,  1086,  1087,  1088,  1091,
    1092,  1094,  1095,  1100,  1101,  1105,  1106,  1108,  1109,  1111,
    1112,  1113,  1116,  1117,  1120,  1121,  1123,  1124,  1129,  1130,
    1131,  1132,  1135,  1136,  1137,  1138,  1140,  1142,  1143,  1144,
    1146,  1149,  1150,  1153,  1154,  1155,  1156,  1157,  1162,  1163,
    1169,  1170,  1171,  1176,  1177,  1195,  1196,  1197,  1198,  1199,
    1200,  1201,  1203,  1204,  1217,  1219,  1229,  1231,  1232,  1235,
    1236,  1237,  1238,  1240,  1241,  1243,  1244,  1245,  1247,  1249,
    1250,  1252,  1253,  1262,  1264,  1265,  1267,  1268,  1270,  1271,
    1273,  1274,  1277,  1278,  1280,  1281,  1282,  1283,  1288,  1289,
    1291,  1292,  1293,  1298,  1299,  1301,  1302,  1303,  1305,  1306,
    1338,  1339,  1341,  1342,  1344,  1345,  1346,  1348,  1349,  1351,
    1352,  1353,  1354,  1356,  1357,  1359,  1360,  1362,  1363,  1366,
    1367,  1368,  1370,  1371,  1372,  1373,  1374,  1376,  1377,  1378,
    1380,  1381,  1382,  1383,  1384,  1387,  1388,  1390,  1392,  1393,
    1397,  1399,  1400,  1401,  1403,  1404,  1405,  1406,  1411,  1412,
    1414,  1415,  1417,  1418,  1421,  1422,  1427,  1428,  1430,  1431,
    1435,  1437,  1439,  1440,  1441,  1442,  1443,  1446,  1447,  1449,
    1450,  1451,  1453,  1454,  1456,  1457,  1458,  1459,  1460,  1461,
    1462,  1463,  1465,  1466,  1468,  1469,  1471,  1473,  1474,  1476,
    1477,  1479,  1480,  1481,  1482,  1483,  1484,  1485,  1486,  1487,
    1488,  1489,  1490,  1491,  1492,  1493,  1494,  1495,  1496,  1498,
    1499,  1500,  1504,  1505,  1507,  1509,  1510,  1512,  1513,  1517,
    1518,  1519,  1520,  1521,  1526,  1529,  1533,  1534,  1536,  1537
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
#line 6741 "parser.cc"

#line 1546 "parser.y"


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
