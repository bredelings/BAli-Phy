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

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        value.YY_MOVE_OR_COPY< Hs::TypeFamilyInstanceEqn > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        value.YY_MOVE_OR_COPY< Hs::TypeVar > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alt: // alt
        value.YY_MOVE_OR_COPY< Located<Hs::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_binds: // binds
        value.YY_MOVE_OR_COPY< Located<Hs::Binds> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decllist_cls: // decllist_cls
      case symbol_kind::S_decllist_inst: // decllist_inst
        value.YY_MOVE_OR_COPY< Located<Hs::Decls> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
        value.YY_MOVE_OR_COPY< Located<Hs::InfixExp> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
        value.YY_MOVE_OR_COPY< Located<expression_ref> > (YY_MOVE (that.value));
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
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_list: // list
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_literal: // literal
        value.YY_MOVE_OR_COPY< expression_ref > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        value.YY_MOVE_OR_COPY< float > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        value.YY_MOVE_OR_COPY< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
        value.YY_MOVE_OR_COPY< integer > (YY_MOVE (that.value));
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

      case symbol_kind::S_wherebinds: // wherebinds
        value.YY_MOVE_OR_COPY< std::optional<Located<Hs::Binds>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
        value.YY_MOVE_OR_COPY< std::optional<Located<Hs::Decls>> > (YY_MOVE (that.value));
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

      case symbol_kind::S_where_type_family: // where_type_family
        value.YY_MOVE_OR_COPY< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > (YY_MOVE (that.value));
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

      case symbol_kind::S_sks_vars: // sks_vars
        value.YY_MOVE_OR_COPY< std::vector<Hs::TypeCon> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ty_fam_inst_eqn_list: // ty_fam_inst_eqn_list
      case symbol_kind::S_ty_fam_inst_eqns: // ty_fam_inst_eqns
        value.YY_MOVE_OR_COPY< std::vector<Hs::TypeFamilyInstanceEqn> > (YY_MOVE (that.value));
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

      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        value.YY_MOVE_OR_COPY< std::vector<Located<expression_ref>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_con_list: // con_list
        value.YY_MOVE_OR_COPY< std::vector<Located<std::string>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decls: // decls
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

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        value.move< Hs::TypeFamilyInstanceEqn > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        value.move< Hs::TypeVar > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alt: // alt
        value.move< Located<Hs::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_binds: // binds
        value.move< Located<Hs::Binds> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decllist_cls: // decllist_cls
      case symbol_kind::S_decllist_inst: // decllist_inst
        value.move< Located<Hs::Decls> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
        value.move< Located<Hs::InfixExp> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
        value.move< Located<expression_ref> > (YY_MOVE (that.value));
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
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_list: // list
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_literal: // literal
        value.move< expression_ref > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        value.move< float > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        value.move< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
        value.move< integer > (YY_MOVE (that.value));
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

      case symbol_kind::S_wherebinds: // wherebinds
        value.move< std::optional<Located<Hs::Binds>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
        value.move< std::optional<Located<Hs::Decls>> > (YY_MOVE (that.value));
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

      case symbol_kind::S_where_type_family: // where_type_family
        value.move< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > (YY_MOVE (that.value));
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

      case symbol_kind::S_sks_vars: // sks_vars
        value.move< std::vector<Hs::TypeCon> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ty_fam_inst_eqn_list: // ty_fam_inst_eqn_list
      case symbol_kind::S_ty_fam_inst_eqns: // ty_fam_inst_eqns
        value.move< std::vector<Hs::TypeFamilyInstanceEqn> > (YY_MOVE (that.value));
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

      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        value.move< std::vector<Located<expression_ref>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_con_list: // con_list
        value.move< std::vector<Located<std::string>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decls: // decls
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

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        value.copy< Hs::TypeFamilyInstanceEqn > (that.value);
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        value.copy< Hs::TypeVar > (that.value);
        break;

      case symbol_kind::S_alt: // alt
        value.copy< Located<Hs::Alt> > (that.value);
        break;

      case symbol_kind::S_binds: // binds
        value.copy< Located<Hs::Binds> > (that.value);
        break;

      case symbol_kind::S_decllist_cls: // decllist_cls
      case symbol_kind::S_decllist_inst: // decllist_inst
        value.copy< Located<Hs::Decls> > (that.value);
        break;

      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
        value.copy< Located<Hs::InfixExp> > (that.value);
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
        value.copy< Located<expression_ref> > (that.value);
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
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_list: // list
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_literal: // literal
        value.copy< expression_ref > (that.value);
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        value.copy< float > (that.value);
        break;

      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        value.copy< int > (that.value);
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
        value.copy< integer > (that.value);
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

      case symbol_kind::S_wherebinds: // wherebinds
        value.copy< std::optional<Located<Hs::Binds>> > (that.value);
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
        value.copy< std::optional<Located<Hs::Decls>> > (that.value);
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

      case symbol_kind::S_where_type_family: // where_type_family
        value.copy< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > (that.value);
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

      case symbol_kind::S_sks_vars: // sks_vars
        value.copy< std::vector<Hs::TypeCon> > (that.value);
        break;

      case symbol_kind::S_ty_fam_inst_eqn_list: // ty_fam_inst_eqn_list
      case symbol_kind::S_ty_fam_inst_eqns: // ty_fam_inst_eqns
        value.copy< std::vector<Hs::TypeFamilyInstanceEqn> > (that.value);
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

      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        value.copy< std::vector<Located<expression_ref>> > (that.value);
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_con_list: // con_list
        value.copy< std::vector<Located<std::string>> > (that.value);
        break;

      case symbol_kind::S_decls: // decls
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

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        value.move< Hs::TypeFamilyInstanceEqn > (that.value);
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        value.move< Hs::TypeVar > (that.value);
        break;

      case symbol_kind::S_alt: // alt
        value.move< Located<Hs::Alt> > (that.value);
        break;

      case symbol_kind::S_binds: // binds
        value.move< Located<Hs::Binds> > (that.value);
        break;

      case symbol_kind::S_decllist_cls: // decllist_cls
      case symbol_kind::S_decllist_inst: // decllist_inst
        value.move< Located<Hs::Decls> > (that.value);
        break;

      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
        value.move< Located<Hs::InfixExp> > (that.value);
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
        value.move< Located<expression_ref> > (that.value);
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
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_list: // list
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_literal: // literal
        value.move< expression_ref > (that.value);
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        value.move< float > (that.value);
        break;

      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        value.move< int > (that.value);
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
        value.move< integer > (that.value);
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

      case symbol_kind::S_wherebinds: // wherebinds
        value.move< std::optional<Located<Hs::Binds>> > (that.value);
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
        value.move< std::optional<Located<Hs::Decls>> > (that.value);
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

      case symbol_kind::S_where_type_family: // where_type_family
        value.move< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > (that.value);
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

      case symbol_kind::S_sks_vars: // sks_vars
        value.move< std::vector<Hs::TypeCon> > (that.value);
        break;

      case symbol_kind::S_ty_fam_inst_eqn_list: // ty_fam_inst_eqn_list
      case symbol_kind::S_ty_fam_inst_eqns: // ty_fam_inst_eqns
        value.move< std::vector<Hs::TypeFamilyInstanceEqn> > (that.value);
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

      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        value.move< std::vector<Located<expression_ref>> > (that.value);
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_con_list: // con_list
        value.move< std::vector<Located<std::string>> > (that.value);
        break;

      case symbol_kind::S_decls: // decls
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

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        yylhs.value.emplace< Hs::TypeFamilyInstanceEqn > ();
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        yylhs.value.emplace< Hs::TypeVar > ();
        break;

      case symbol_kind::S_alt: // alt
        yylhs.value.emplace< Located<Hs::Alt> > ();
        break;

      case symbol_kind::S_binds: // binds
        yylhs.value.emplace< Located<Hs::Binds> > ();
        break;

      case symbol_kind::S_decllist_cls: // decllist_cls
      case symbol_kind::S_decllist_inst: // decllist_inst
        yylhs.value.emplace< Located<Hs::Decls> > ();
        break;

      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
        yylhs.value.emplace< Located<Hs::InfixExp> > ();
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
        yylhs.value.emplace< Located<expression_ref> > ();
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
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_list: // list
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_literal: // literal
        yylhs.value.emplace< expression_ref > ();
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        yylhs.value.emplace< float > ();
        break;

      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        yylhs.value.emplace< int > ();
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
        yylhs.value.emplace< integer > ();
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

      case symbol_kind::S_wherebinds: // wherebinds
        yylhs.value.emplace< std::optional<Located<Hs::Binds>> > ();
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
        yylhs.value.emplace< std::optional<Located<Hs::Decls>> > ();
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

      case symbol_kind::S_where_type_family: // where_type_family
        yylhs.value.emplace< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ();
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

      case symbol_kind::S_sks_vars: // sks_vars
        yylhs.value.emplace< std::vector<Hs::TypeCon> > ();
        break;

      case symbol_kind::S_ty_fam_inst_eqn_list: // ty_fam_inst_eqn_list
      case symbol_kind::S_ty_fam_inst_eqns: // ty_fam_inst_eqns
        yylhs.value.emplace< std::vector<Hs::TypeFamilyInstanceEqn> > ();
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

      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        yylhs.value.emplace< std::vector<Located<expression_ref>> > ();
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_con_list: // con_list
        yylhs.value.emplace< std::vector<Located<std::string>> > ();
        break;

      case symbol_kind::S_decls: // decls
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
#line 507 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2446 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 524 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < std::string > (),yystack_[2].value.as < std::optional<std::vector<Hs::Export>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2452 "parser.cc"
    break;

  case 4: // module: body2
#line 525 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{"Main",{},yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2458 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 527 "parser.y"
                                                                 {drv.push_module_context();}
#line 2464 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 535 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2470 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 536 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2476 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 538 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2482 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 539 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2488 "parser.cc"
    break;

  case 13: // top: semis top1
#line 542 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2494 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 544 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2500 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 545 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2506 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 546 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::ImpDecl> > (),{});}
#line 2512 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 554 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::Export>> > () = yystack_[1].value.as < std::vector<Hs::Export> > ();}
#line 2518 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 555 "parser.y"
                                      {}
#line 2524 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 557 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > () = yystack_[0].value.as < std::vector<Hs::Export> > ();}
#line 2530 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 559 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > () = yystack_[2].value.as < std::vector<Hs::Export> > (); yylhs.value.as < std::vector<Hs::Export> > ().push_back(yystack_[0].value.as < Hs::Export > ());}
#line 2536 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 560 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > ().push_back(yystack_[0].value.as < Hs::Export > ());}
#line 2542 "parser.cc"
    break;

  case 22: // export: qcname export_subspec
#line 562 "parser.y"
                                      {yylhs.value.as < Hs::Export > () = Hs::ExportSymbol{yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}; }
#line 2548 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 563 "parser.y"
                                      {yylhs.value.as < Hs::Export > () = Hs::ExportModule{{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2554 "parser.cc"
    break;

  case 24: // export_subspec: %empty
#line 565 "parser.y"
                                      {}
#line 2560 "parser.cc"
    break;

  case 25: // export_subspec: "(" qcnames ")"
#line 566 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpecSome{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2566 "parser.cc"
    break;

  case 26: // export_subspec: "(" ".." ")"
#line 567 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpecAll(); }
#line 2572 "parser.cc"
    break;

  case 27: // qcnames: %empty
#line 569 "parser.y"
                   {}
#line 2578 "parser.cc"
    break;

  case 28: // qcnames: qcnames1
#line 570 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2584 "parser.cc"
    break;

  case 29: // qcnames1: qcnames1 "," qcname
#line 572 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2590 "parser.cc"
    break;

  case 30: // qcnames1: qcname
#line 573 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2596 "parser.cc"
    break;

  case 31: // qcname: qvar
#line 575 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2602 "parser.cc"
    break;

  case 32: // qcname: oqtycon_no_varcon
#line 576 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2608 "parser.cc"
    break;

  case 37: // importdecls: importdecls_semi importdecl
#line 586 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[1].value.as < std::vector<Hs::ImpDecl> > (), yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[0].value.as < Hs::ImpDecl > ()); }
#line 2614 "parser.cc"
    break;

  case 38: // importdecls_semi: importdecls_semi importdecl semis1
#line 588 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[2].value.as < std::vector<Hs::ImpDecl> > (); yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[1].value.as < Hs::ImpDecl > ()); }
#line 2620 "parser.cc"
    break;

  case 39: // importdecls_semi: %empty
#line 589 "parser.y"
                         { }
#line 2626 "parser.cc"
    break;

  case 40: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 591 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::ImpDecl > () = Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < std::string > (),yystack_[1].value.as < std::optional<std::string> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ());
}
#line 2634 "parser.cc"
    break;

  case 41: // optqualified: "qualified"
#line 604 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2640 "parser.cc"
    break;

  case 42: // optqualified: %empty
#line 605 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2646 "parser.cc"
    break;

  case 43: // maybeas: "as" modid
#line 607 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 2652 "parser.cc"
    break;

  case 44: // maybeas: %empty
#line 608 "parser.y"
                               { }
#line 2658 "parser.cc"
    break;

  case 45: // maybeimpspec: impspec
#line 610 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2664 "parser.cc"
    break;

  case 46: // maybeimpspec: %empty
#line 611 "parser.y"
                               { }
#line 2670 "parser.cc"
    break;

  case 47: // impspec: "(" exportlist ")"
#line 615 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::Export> > ()}; }
#line 2676 "parser.cc"
    break;

  case 48: // impspec: "hiding" "(" exportlist ")"
#line 616 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::Export> > ()}; }
#line 2682 "parser.cc"
    break;

  case 49: // prec: %empty
#line 621 "parser.y"
                   { }
#line 2688 "parser.cc"
    break;

  case 50: // prec: "INTEGER"
#line 622 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < integer > ().convert_to<int>(); }
#line 2694 "parser.cc"
    break;

  case 51: // infix: "infix"
#line 624 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2700 "parser.cc"
    break;

  case 52: // infix: "infixl"
#line 625 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2706 "parser.cc"
    break;

  case 53: // infix: "infixr"
#line 626 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2712 "parser.cc"
    break;

  case 54: // ops: ops "," op
#line 628 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 2718 "parser.cc"
    break;

  case 55: // ops: op
#line 629 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 2724 "parser.cc"
    break;

  case 56: // topdecls: topdecls_semi topdecl
#line 633 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 2730 "parser.cc"
    break;

  case 57: // topdecls_semi: topdecls_semi topdecl semis1
#line 635 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2736 "parser.cc"
    break;

  case 58: // topdecls_semi: %empty
#line 636 "parser.y"
                                            { }
#line 2742 "parser.cc"
    break;

  case 59: // topdecl: cl_decl
#line 638 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2748 "parser.cc"
    break;

  case 60: // topdecl: ty_decl
#line 639 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2754 "parser.cc"
    break;

  case 61: // topdecl: standalone_kind_sig
#line 640 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2760 "parser.cc"
    break;

  case 62: // topdecl: inst_decl
#line 641 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2766 "parser.cc"
    break;

  case 63: // topdecl: "default" "(" comma_types0 ")"
#line 644 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Hs::DefaultDecl(yystack_[1].value.as < std::vector<Hs::Type> > ()); }
#line 2772 "parser.cc"
    break;

  case 64: // topdecl: "foreign" "import" "bpcall" "STRING" var "::" sigtypedoc
#line 645 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = Hs::ForeignDecl(yystack_[3].value.as < std::string > (), yystack_[2].value.as < std::string > (), yystack_[0].value.as < Hs::Type > ());}
#line 2778 "parser.cc"
    break;

  case 65: // topdecl: decl_no_th
#line 652 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2784 "parser.cc"
    break;

  case 66: // topdecl: infixexp_top
#line 654 "parser.y"
                                               {yylhs.value.as < expression_ref > () = unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ());}
#line 2790 "parser.cc"
    break;

  case 67: // cl_decl: "class" tycl_hdr where_cls
#line 656 "parser.y"
                                              {yylhs.value.as < expression_ref > () = make_class_decl(yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().first,yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().second,yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ());}
#line 2796 "parser.cc"
    break;

  case 68: // ty_decl: "type" type "=" ktype
#line 658 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_type_synonym({yystack_[2].location, yystack_[2].value.as < Hs::Type > ()},{yystack_[0].location, yystack_[0].value.as < Hs::Type > ()});}
#line 2802 "parser.cc"
    break;

  case 69: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 659 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::Type> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::Type> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > ());}
#line 2808 "parser.cc"
    break;

  case 70: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 661 "parser.y"
                                                                                          {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::Type> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::Type> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < std::optional<Hs::GADTConstructorsDecl> > ());}
#line 2814 "parser.cc"
    break;

  case 71: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 662 "parser.y"
                                                                                          {yylhs.value.as < expression_ref > () = make_type_family({yystack_[3].location,yystack_[3].value.as < Hs::Type > ()}, yystack_[2].value.as < std::optional<Located<Hs::Kind>> > (), yystack_[0].value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ());}
#line 2820 "parser.cc"
    break;

  case 72: // standalone_kind_sig: "type" sks_vars "::" kind
#line 665 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = Hs::KindSigDecl(yystack_[2].value.as < std::vector<Hs::TypeCon> > (),yystack_[0].value.as < expression_ref > ());}
#line 2826 "parser.cc"
    break;

  case 73: // sks_vars: sks_vars "," oqtycon
#line 667 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::TypeCon> > () = yystack_[2].value.as < std::vector<Hs::TypeCon> > (); yylhs.value.as < std::vector<Hs::TypeCon> > ().push_back(Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()})); }
#line 2832 "parser.cc"
    break;

  case 74: // sks_vars: oqtycon
#line 668 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::TypeCon> > () = {Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()})}; }
#line 2838 "parser.cc"
    break;

  case 75: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 671 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_instance_decl({yystack_[1].location,yystack_[1].value.as < Hs::Type > ()},yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ());}
#line 2844 "parser.cc"
    break;

  case 76: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 672 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 2850 "parser.cc"
    break;

  case 91: // where_type_family: %empty
#line 707 "parser.y"
                                                           {}
#line 2856 "parser.cc"
    break;

  case 92: // where_type_family: "where" ty_fam_inst_eqn_list
#line 708 "parser.y"
                                                           {yylhs.value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > () = yystack_[0].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2862 "parser.cc"
    break;

  case 93: // ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
#line 710 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2868 "parser.cc"
    break;

  case 94: // ty_fam_inst_eqn_list: "vocurly" ty_fam_inst_eqns close
#line 711 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2874 "parser.cc"
    break;

  case 95: // ty_fam_inst_eqn_list: "{" ".." "}"
#line 712 "parser.y"
                                                           {}
#line 2880 "parser.cc"
    break;

  case 96: // ty_fam_inst_eqn_list: "vocurly" ".." close
#line 713 "parser.y"
                                                           {}
#line 2886 "parser.cc"
    break;

  case 97: // ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
#line 715 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[2].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > (); yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ().push_back(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 2892 "parser.cc"
    break;

  case 98: // ty_fam_inst_eqns: ty_fam_inst_eqns ";"
#line 716 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2898 "parser.cc"
    break;

  case 99: // ty_fam_inst_eqns: ty_fam_inst_eqn
#line 717 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = {yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ()};}
#line 2904 "parser.cc"
    break;

  case 100: // ty_fam_inst_eqns: %empty
#line 718 "parser.y"
                                                           {}
#line 2910 "parser.cc"
    break;

  case 101: // ty_fam_inst_eqn: type "=" ctype
#line 720 "parser.y"
                                                           {yylhs.value.as < Hs::TypeFamilyInstanceEqn > () = make_type_family_instance_eqn({yystack_[2].location,yystack_[2].value.as < Hs::Type > ()},{yystack_[0].location,yystack_[0].value.as < Hs::Type > ()});}
#line 2916 "parser.cc"
    break;

  case 102: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 723 "parser.y"
                                                               {}
#line 2922 "parser.cc"
    break;

  case 103: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 725 "parser.y"
                                                               { yylhs.value.as < expression_ref > () = make_type_family({yystack_[1].location,yystack_[1].value.as < Hs::Type > ()}, yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {}); }
#line 2928 "parser.cc"
    break;

  case 104: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 727 "parser.y"
                                                               { yylhs.value.as < expression_ref > () = make_type_family({yystack_[1].location,yystack_[1].value.as < Hs::Type > ()}, yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {}); }
#line 2934 "parser.cc"
    break;

  case 105: // at_decl_cls: "type" ty_fam_inst_eqn
#line 729 "parser.y"
                                                               { yylhs.value.as < expression_ref > () = Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());    }
#line 2940 "parser.cc"
    break;

  case 106: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 730 "parser.y"
                                                               { yylhs.value.as < expression_ref > () = Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());    }
#line 2946 "parser.cc"
    break;

  case 111: // at_decl_inst: "type" opt_instance ty_fam_inst_eqn
#line 738 "parser.y"
                                                              { yylhs.value.as < expression_ref > () = Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());    }
#line 2952 "parser.cc"
    break;

  case 112: // data_or_newtype: "data"
#line 740 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 2958 "parser.cc"
    break;

  case 113: // data_or_newtype: "newtype"
#line 741 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 2964 "parser.cc"
    break;

  case 114: // opt_kind_sig: %empty
#line 745 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 2970 "parser.cc"
    break;

  case 115: // opt_kind_sig: "::" kind
#line 746 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < expression_ref > ();}
#line 2976 "parser.cc"
    break;

  case 118: // opt_tyfam_kind_sig: %empty
#line 751 "parser.y"
                                      {}
#line 2982 "parser.cc"
    break;

  case 119: // opt_tyfam_kind_sig: "::" kind
#line 752 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < expression_ref > ()}};}
#line 2988 "parser.cc"
    break;

  case 120: // opt_tyfam_kind_sig: "=" tv_bndr
#line 753 "parser.y"
                                      {}
#line 2994 "parser.cc"
    break;

  case 121: // opt_at_kind_inj_sig: %empty
#line 755 "parser.y"
                                      {}
#line 3000 "parser.cc"
    break;

  case 122: // opt_at_kind_inj_sig: "::" kind
#line 756 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < expression_ref > ()}};}
#line 3006 "parser.cc"
    break;

  case 123: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 757 "parser.y"
                                                                  {}
#line 3012 "parser.cc"
    break;

  case 124: // tycl_hdr: context "=>" type
#line 761 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::Type> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::Type > ()};}
#line 3018 "parser.cc"
    break;

  case 125: // tycl_hdr: type
#line 762 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::Type> > () = {{},yystack_[0].value.as < Hs::Type > ()};}
#line 3024 "parser.cc"
    break;

  case 129: // decl_cls: at_decl_cls
#line 808 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3030 "parser.cc"
    break;

  case 130: // decl_cls: decl
#line 809 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3036 "parser.cc"
    break;

  case 131: // decls_cls: decls_cls ";" decl_cls
#line 811 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3042 "parser.cc"
    break;

  case 132: // decls_cls: decls_cls ";"
#line 812 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3048 "parser.cc"
    break;

  case 133: // decls_cls: decl_cls
#line 813 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3054 "parser.cc"
    break;

  case 134: // decls_cls: %empty
#line 814 "parser.y"
                                           {}
#line 3060 "parser.cc"
    break;

  case 135: // decllist_cls: "{" decls_cls "}"
#line 816 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3066 "parser.cc"
    break;

  case 136: // decllist_cls: "vocurly" decls_cls close
#line 817 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3072 "parser.cc"
    break;

  case 137: // where_cls: "where" decllist_cls
#line 819 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3078 "parser.cc"
    break;

  case 138: // where_cls: %empty
#line 820 "parser.y"
                                           {}
#line 3084 "parser.cc"
    break;

  case 139: // decl_inst: at_decl_inst
#line 822 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3090 "parser.cc"
    break;

  case 140: // decl_inst: decl
#line 823 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3096 "parser.cc"
    break;

  case 141: // decls_inst: decls_inst ";" decl_inst
#line 825 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3102 "parser.cc"
    break;

  case 142: // decls_inst: decls_inst ";"
#line 826 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3108 "parser.cc"
    break;

  case 143: // decls_inst: decl_inst
#line 827 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3114 "parser.cc"
    break;

  case 144: // decls_inst: %empty
#line 828 "parser.y"
                                           {}
#line 3120 "parser.cc"
    break;

  case 145: // decllist_inst: "{" decls_inst "}"
#line 830 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3126 "parser.cc"
    break;

  case 146: // decllist_inst: "vocurly" decls_inst close
#line 831 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3132 "parser.cc"
    break;

  case 147: // where_inst: "where" decllist_inst
#line 833 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3138 "parser.cc"
    break;

  case 148: // where_inst: %empty
#line 834 "parser.y"
                                           {}
#line 3144 "parser.cc"
    break;

  case 149: // decls: decls ";" decl
#line 837 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3150 "parser.cc"
    break;

  case 150: // decls: decls ";"
#line 838 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3156 "parser.cc"
    break;

  case 151: // decls: decl
#line 839 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3162 "parser.cc"
    break;

  case 152: // decls: %empty
#line 840 "parser.y"
                        {}
#line 3168 "parser.cc"
    break;

  case 153: // decllist: "{" decls "}"
#line 842 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3174 "parser.cc"
    break;

  case 154: // decllist: "vocurly" decls close
#line 843 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3180 "parser.cc"
    break;

  case 155: // binds: decllist
#line 845 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 3186 "parser.cc"
    break;

  case 156: // wherebinds: "where" binds
#line 847 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 3192 "parser.cc"
    break;

  case 157: // wherebinds: %empty
#line 848 "parser.y"
                                 {}
#line 3198 "parser.cc"
    break;

  case 163: // opt_tyconsig: %empty
#line 874 "parser.y"
                                 {}
#line 3204 "parser.cc"
    break;

  case 164: // opt_tyconsig: "::" gtycon
#line 875 "parser.y"
                                 {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3210 "parser.cc"
    break;

  case 165: // sigtype: ctype
#line 884 "parser.y"
         { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3216 "parser.cc"
    break;

  case 166: // sigtypedoc: ctypedoc
#line 886 "parser.y"
            { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3222 "parser.cc"
    break;

  case 167: // sig_vars: sig_vars "," var
#line 888 "parser.y"
                           {yylhs.value.as < std::vector<Hs::Var> > () = yystack_[2].value.as < std::vector<Hs::Var> > (); yylhs.value.as < std::vector<Hs::Var> > ().push_back(Hs::Var(yystack_[0].value.as < std::string > ()));}
#line 3228 "parser.cc"
    break;

  case 168: // sig_vars: var
#line 889 "parser.y"
                           {yylhs.value.as < std::vector<Hs::Var> > ().push_back(Hs::Var(yystack_[0].value.as < std::string > ()));}
#line 3234 "parser.cc"
    break;

  case 169: // sigtypes1: sigtype
#line 891 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3240 "parser.cc"
    break;

  case 170: // sigtypes1: sigtypes1 "," sigtype
#line 892 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[2].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3246 "parser.cc"
    break;

  case 171: // strict_mark: strictness
#line 896 "parser.y"
             { yylhs.value.as < Hs::StrictLazy > () = yystack_[0].value.as < Hs::StrictLazy > (); }
#line 3252 "parser.cc"
    break;

  case 172: // strictness: PREFIX_BANG
#line 902 "parser.y"
                         {yylhs.value.as < Hs::StrictLazy > () = Hs::StrictLazy::strict;}
#line 3258 "parser.cc"
    break;

  case 173: // strictness: PREFIX_TILDE
#line 903 "parser.y"
                         {yylhs.value.as < Hs::StrictLazy > () = Hs::StrictLazy::lazy;}
#line 3264 "parser.cc"
    break;

  case 174: // ktype: ctype
#line 910 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 3270 "parser.cc"
    break;

  case 175: // ktype: ctype "::" kind
#line 911 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeOfKind(yystack_[2].value.as < Hs::Type > (), yystack_[0].value.as < expression_ref > ());}
#line 3276 "parser.cc"
    break;

  case 176: // ctype: "forall" tv_bndrs "." ctype
#line 913 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::ForallType(yystack_[2].value.as < std::vector<Hs::TypeVar> > (), yystack_[0].value.as < Hs::Type > ());}
#line 3282 "parser.cc"
    break;

  case 177: // ctype: context "=>" ctype
#line 914 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::Type > ());}
#line 3288 "parser.cc"
    break;

  case 178: // ctype: type
#line 916 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3294 "parser.cc"
    break;

  case 179: // ctypedoc: ctype
#line 918 "parser.y"
          { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3300 "parser.cc"
    break;

  case 180: // context: btype
#line 927 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::Type > ());}
#line 3306 "parser.cc"
    break;

  case 181: // context_no_ops: btype_no_ops
#line 929 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ()));}
#line 3312 "parser.cc"
    break;

  case 182: // type: btype
#line 931 "parser.y"
      { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3318 "parser.cc"
    break;

  case 183: // type: btype "->" ctype
#line 932 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,"->"}),yystack_[2].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()});}
#line 3324 "parser.cc"
    break;

  case 184: // typedoc: type
#line 934 "parser.y"
         { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3330 "parser.cc"
    break;

  case 185: // btype: infixtype
#line 937 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3336 "parser.cc"
    break;

  case 186: // infixtype: ftype
#line 939 "parser.y"
           { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3342 "parser.cc"
    break;

  case 187: // infixtype: btype "~" btype
#line 940 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,"~"}),yystack_[2].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()});}
#line 3348 "parser.cc"
    break;

  case 188: // btype_no_ops: atype_docs
#line 942 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3354 "parser.cc"
    break;

  case 189: // btype_no_ops: btype_no_ops atype_docs
#line 943 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[1].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3360 "parser.cc"
    break;

  case 190: // ftype: atype
#line 945 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3366 "parser.cc"
    break;

  case 191: // ftype: tyop
#line 946 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3372 "parser.cc"
    break;

  case 192: // ftype: ftype tyarg
#line 947 "parser.y"
                                   { yylhs.value.as < Hs::Type > () = Hs::TypeApp(yystack_[1].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()); }
#line 3378 "parser.cc"
    break;

  case 193: // tyarg: atype
#line 949 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3384 "parser.cc"
    break;

  case 194: // tyop: qtyconop
#line 951 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3390 "parser.cc"
    break;

  case 195: // tyop: tyvarop
#line 952 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3396 "parser.cc"
    break;

  case 196: // atype_docs: atype
#line 959 "parser.y"
            { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3402 "parser.cc"
    break;

  case 197: // atype: ntgtycon
#line 966 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3408 "parser.cc"
    break;

  case 198: // atype: tyvar
#line 967 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3414 "parser.cc"
    break;

  case 199: // atype: "*"
#line 968 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,"*"});}
#line 3420 "parser.cc"
    break;

  case 200: // atype: strict_mark atype
#line 969 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::StrictLazyType(yystack_[1].value.as < Hs::StrictLazy > (),yystack_[0].value.as < Hs::Type > ());}
#line 3426 "parser.cc"
    break;

  case 201: // atype: "{" fielddecls "}"
#line 970 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ());}
#line 3432 "parser.cc"
    break;

  case 202: // atype: "(" ")"
#line 971 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[1].location,"()"});}
#line 3438 "parser.cc"
    break;

  case 203: // atype: "(" comma_types1 "," ktype ")"
#line 972 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::Type> > ();ts.push_back(yystack_[1].value.as < Hs::Type > ());yylhs.value.as < Hs::Type > () = Hs::TupleType(ts);}
#line 3444 "parser.cc"
    break;

  case 204: // atype: "[" ktype "]"
#line 978 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::ListType{yystack_[1].value.as < Hs::Type > ()}; }
#line 3450 "parser.cc"
    break;

  case 205: // atype: "(" ktype ")"
#line 979 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = yystack_[1].value.as < Hs::Type > ();}
#line 3456 "parser.cc"
    break;

  case 206: // inst_type: sigtype
#line 982 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 3462 "parser.cc"
    break;

  case 209: // comma_types0: comma_types1
#line 987 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[0].value.as < std::vector<Hs::Type> > ();}
#line 3468 "parser.cc"
    break;

  case 210: // comma_types0: %empty
#line 988 "parser.y"
                                       { /* default construction OK */ }
#line 3474 "parser.cc"
    break;

  case 211: // comma_types1: ktype
#line 990 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3480 "parser.cc"
    break;

  case 212: // comma_types1: comma_types1 "," ktype
#line 991 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[2].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3486 "parser.cc"
    break;

  case 213: // tv_bndrs: tv_bndrs tv_bndr
#line 998 "parser.y"
                               {yylhs.value.as < std::vector<Hs::TypeVar> > () = yystack_[1].value.as < std::vector<Hs::TypeVar> > (); yylhs.value.as < std::vector<Hs::TypeVar> > ().push_back(yystack_[0].value.as < Hs::TypeVar > ());}
#line 3492 "parser.cc"
    break;

  case 214: // tv_bndrs: %empty
#line 999 "parser.y"
                               { /* default construction OK */}
#line 3498 "parser.cc"
    break;

  case 215: // tv_bndr: tv_bndr_no_braces
#line 1001 "parser.y"
                                 {yylhs.value.as < Hs::TypeVar > () = yystack_[0].value.as < Hs::TypeVar > ();}
#line 3504 "parser.cc"
    break;

  case 216: // tv_bndr: "{" tyvar "}"
#line 1002 "parser.y"
                                 {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[1].location,yystack_[1].value.as < std::string > ()});}
#line 3510 "parser.cc"
    break;

  case 217: // tv_bndr: "{" tyvar "::" kind "}"
#line 1003 "parser.y"
                                 {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[3].location,yystack_[3].value.as < std::string > ()});}
#line 3516 "parser.cc"
    break;

  case 218: // tv_bndr_no_braces: tyvar
#line 1006 "parser.y"
                                              {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3522 "parser.cc"
    break;

  case 219: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 1007 "parser.y"
                                              {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[3].location,yystack_[3].value.as < std::string > ()},yystack_[1].value.as < expression_ref > ());}
#line 3528 "parser.cc"
    break;

  case 220: // kind: ctype
#line 1025 "parser.y"
             {yylhs.value.as < expression_ref > () = type_to_kind(yystack_[0].value.as < Hs::Type > ());}
#line 3534 "parser.cc"
    break;

  case 221: // gadt_constrlist: "where" "{" gadt_constrs "}"
#line 1031 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3540 "parser.cc"
    break;

  case 222: // gadt_constrlist: "where" "vocurly" gadt_constrs close
#line 1032 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3546 "parser.cc"
    break;

  case 223: // gadt_constrlist: %empty
#line 1033 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = {};}
#line 3552 "parser.cc"
    break;

  case 224: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1035 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3558 "parser.cc"
    break;

  case 225: // gadt_constrs: gadt_constr
#line 1036 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3564 "parser.cc"
    break;

  case 226: // gadt_constr: optSemi con_list "::" sigtype
#line 1038 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),{{},yystack_[0].value.as < Hs::Type > ()});}
#line 3570 "parser.cc"
    break;

  case 227: // constrs: "=" constrs1
#line 1040 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 3576 "parser.cc"
    break;

  case 228: // constrs1: constrs1 "|" constr
#line 1042 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3582 "parser.cc"
    break;

  case 229: // constrs1: constr
#line 1043 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3588 "parser.cc"
    break;

  case 230: // constr: forall context_no_ops "=>" constr_stuff
#line 1045 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::TypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::Type > ());}
#line 3594 "parser.cc"
    break;

  case 231: // constr: forall constr_stuff
#line 1046 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::TypeVar> > (),{}, yystack_[0].value.as < Hs::Type > ());}
#line 3600 "parser.cc"
    break;

  case 232: // forall: "forall" tv_bndrs "."
#line 1048 "parser.y"
                                {yylhs.value.as < std::vector<Hs::TypeVar> > () = yystack_[1].value.as < std::vector<Hs::TypeVar> > ();}
#line 3606 "parser.cc"
    break;

  case 233: // forall: %empty
#line 1049 "parser.y"
                                {}
#line 3612 "parser.cc"
    break;

  case 234: // constr_stuff: btype_no_ops
#line 1051 "parser.y"
                                                {yylhs.value.as < Hs::Type > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ());}
#line 3618 "parser.cc"
    break;

  case 235: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1052 "parser.y"
                                                {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,yystack_[1].value.as < std::string > ()}),Hs::make_tyapps(yystack_[2].value.as < std::vector<Hs::Type> > ()),Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ())});}
#line 3624 "parser.cc"
    break;

  case 236: // fielddecls: %empty
#line 1054 "parser.y"
                                {}
#line 3630 "parser.cc"
    break;

  case 237: // fielddecls: fielddecls1
#line 1055 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 3636 "parser.cc"
    break;

  case 238: // fielddecls1: fielddecls1 "," fielddecl
#line 1057 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3642 "parser.cc"
    break;

  case 239: // fielddecls1: fielddecl
#line 1058 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3648 "parser.cc"
    break;

  case 240: // fielddecl: sig_vars "::" ctype
#line 1060 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::Var> > (),yystack_[0].value.as < Hs::Type > ());}
#line 3654 "parser.cc"
    break;

  case 251: // decl_no_th: sigdecl
#line 1079 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3660 "parser.cc"
    break;

  case 252: // decl_no_th: PREFIX_BANG aexp rhs
#line 1081 "parser.y"
                                      {yylhs.value.as < expression_ref > () = Hs::StrictValueDecl{yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}; }
#line 3666 "parser.cc"
    break;

  case 253: // decl_no_th: infixexp_top rhs
#line 1082 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::ValueDecl({yystack_[1].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ());}
#line 3672 "parser.cc"
    break;

  case 254: // decl: decl_no_th
#line 1084 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3678 "parser.cc"
    break;

  case 255: // rhs: "=" exp wherebinds
#line 1088 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3684 "parser.cc"
    break;

  case 256: // rhs: gdrhs wherebinds
#line 1089 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 3690 "parser.cc"
    break;

  case 257: // gdrhs: gdrhs gdrh
#line 1091 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3696 "parser.cc"
    break;

  case 258: // gdrhs: gdrh
#line 1092 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3702 "parser.cc"
    break;

  case 259: // gdrh: "|" guardquals "=" exp
#line 1096 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 3708 "parser.cc"
    break;

  case 260: // sigdecl: sig_vars "::" sigtypedoc
#line 1106 "parser.y"
                                  { yylhs.value.as < expression_ref > () = Hs::SignatureDecl{yystack_[2].value.as < std::vector<Hs::Var> > (),yystack_[0].value.as < Hs::Type > ()}; }
#line 3714 "parser.cc"
    break;

  case 261: // sigdecl: infix prec ops
#line 1107 "parser.y"
                         { yylhs.value.as < expression_ref > () = Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()}; }
#line 3720 "parser.cc"
    break;

  case 262: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1109 "parser.y"
                                                    {}
#line 3726 "parser.cc"
    break;

  case 263: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1110 "parser.y"
                                            {}
#line 3732 "parser.cc"
    break;

  case 264: // sigdecl: "{-# SCC" qvar "#-}"
#line 1111 "parser.y"
                              {}
#line 3738 "parser.cc"
    break;

  case 265: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1112 "parser.y"
                                     {}
#line 3744 "parser.cc"
    break;

  case 266: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1113 "parser.y"
                                                               {}
#line 3750 "parser.cc"
    break;

  case 267: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1114 "parser.y"
                                                                      {}
#line 3756 "parser.cc"
    break;

  case 268: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1115 "parser.y"
                                                     {}
#line 3762 "parser.cc"
    break;

  case 273: // exp: infixexp "::" sigtype
#line 1127 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[2].location+yystack_[0].location, Hs::TypedExp({yystack_[2].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[2].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::Type > ())}; }
#line 3768 "parser.cc"
    break;

  case 274: // exp: infixexp
#line 1128 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}; }
#line 3774 "parser.cc"
    break;

  case 275: // infixexp: exp10
#line 1132 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location,Hs::InfixExp({yystack_[0].value.as < Located<expression_ref> > ()})};}
#line 3780 "parser.cc"
    break;

  case 276: // infixexp: infixexp qop exp10
#line 1133 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < expression_ref > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3786 "parser.cc"
    break;

  case 277: // infixexp_top: exp10_top
#line 1135 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location, Hs::InfixExp({yystack_[0].value.as < Located<expression_ref> > ()})};}
#line 3792 "parser.cc"
    break;

  case 278: // infixexp_top: infixexp_top qop exp10_top
#line 1136 "parser.y"
                                          {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < expression_ref > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3798 "parser.cc"
    break;

  case 279: // exp10_top: "-" fexp
#line 1138 "parser.y"
                                   {yylhs.value.as < Located<expression_ref> > () = {yystack_[1].location+yystack_[0].location,Hs::InfixExp( { {yystack_[1].location,Hs::Neg()}, yystack_[0].value.as < Located<expression_ref> > ()} )};}
#line 3804 "parser.cc"
    break;

  case 280: // exp10_top: "{-# CORE" "STRING" "#-}"
#line 1139 "parser.y"
                                   {}
#line 3810 "parser.cc"
    break;

  case 281: // exp10_top: fexp
#line 1140 "parser.y"
                                   {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3816 "parser.cc"
    break;

  case 282: // exp10: exp10_top
#line 1143 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3822 "parser.cc"
    break;

  case 283: // exp10: scc_annot exp
#line 1144 "parser.y"
                                 {}
#line 3828 "parser.cc"
    break;

  case 288: // fexp: fexp aexp
#line 1156 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ApplyExp(yystack_[1].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ())};}
#line 3834 "parser.cc"
    break;

  case 289: // fexp: fexp "TYPEAPP" atype
#line 1157 "parser.y"
                                 {}
#line 3840 "parser.cc"
    break;

  case 290: // fexp: "static" aexp
#line 1158 "parser.y"
                                 {}
#line 3846 "parser.cc"
    break;

  case 291: // fexp: aexp
#line 1159 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3852 "parser.cc"
    break;

  case 292: // aexp: qvar "@" aexp
#line 1162 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::AsPattern(Hs::Var(yystack_[2].value.as < std::string > ()),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3858 "parser.cc"
    break;

  case 293: // aexp: PREFIX_TILDE aexp
#line 1163 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LazyPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3864 "parser.cc"
    break;

  case 294: // aexp: "\\" apats1 "->" exp
#line 1164 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LambdaExp(yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3870 "parser.cc"
    break;

  case 295: // aexp: "let" binds "in" exp
#line 1165 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3876 "parser.cc"
    break;

  case 296: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1167 "parser.y"
                                                       {yylhs.value.as < Located<expression_ref> > () = {yystack_[7].location+yystack_[0].location,Hs::IfExp(yystack_[6].value.as < Located<expression_ref> > (),yystack_[3].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3882 "parser.cc"
    break;

  case 297: // aexp: "case" exp "of" altslist
#line 1169 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::CaseExp(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::Alts > ())}; }
#line 3888 "parser.cc"
    break;

  case 298: // aexp: "do" stmtlist
#line 1170 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Do(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 3894 "parser.cc"
    break;

  case 299: // aexp: "mdo" stmtlist
#line 1171 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::MDo(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 3900 "parser.cc"
    break;

  case 300: // aexp: aexp1
#line 1173 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3906 "parser.cc"
    break;

  case 301: // aexp1: aexp1 "{" fbinds "}"
#line 1176 "parser.y"
                              {}
#line 3912 "parser.cc"
    break;

  case 302: // aexp1: aexp2
#line 1177 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3918 "parser.cc"
    break;

  case 303: // aexp2: qvar
#line 1180 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Var(yystack_[0].value.as < std::string > ())};}
#line 3924 "parser.cc"
    break;

  case 304: // aexp2: qcon
#line 1181 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Con(yystack_[0].value.as < std::string > ())};}
#line 3930 "parser.cc"
    break;

  case 305: // aexp2: literal
#line 1182 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[0].value.as < expression_ref > ()};}
#line 3936 "parser.cc"
    break;

  case 306: // aexp2: "(" texp ")"
#line 1183 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, unloc(yystack_[1].value.as < Located<expression_ref> > ())};}
#line 3942 "parser.cc"
    break;

  case 307: // aexp2: "(" tup_exprs ")"
#line 1184 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Tuple(yystack_[1].value.as < std::vector<Located<expression_ref>> > ())};}
#line 3948 "parser.cc"
    break;

  case 308: // aexp2: "[" list "]"
#line 1189 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[1].value.as < expression_ref > ()};}
#line 3954 "parser.cc"
    break;

  case 309: // aexp2: "_"
#line 1190 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::WildcardPattern()};}
#line 3960 "parser.cc"
    break;

  case 310: // texp: exp
#line 1196 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3966 "parser.cc"
    break;

  case 311: // texp: infixexp qop
#line 1197 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LeftSection ( {yystack_[1].value.as < Located<Hs::InfixExp> > ().loc, unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())}, {yystack_[0].location,yystack_[0].value.as < expression_ref > ()} )}; }
#line 3972 "parser.cc"
    break;

  case 312: // texp: qopm infixexp
#line 1198 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RightSection( {yystack_[1].location,yystack_[1].value.as < expression_ref > ()}, {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())} )}; }
#line 3978 "parser.cc"
    break;

  case 313: // tup_exprs: tup_exprs "," texp
#line 1203 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3984 "parser.cc"
    break;

  case 314: // tup_exprs: texp "," texp
#line 1204 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3990 "parser.cc"
    break;

  case 315: // list: texp
#line 1222 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List({yystack_[0].value.as < Located<expression_ref> > ()}); }
#line 3996 "parser.cc"
    break;

  case 316: // list: lexps
#line 1223 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List(yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4002 "parser.cc"
    break;

  case 317: // list: texp ".."
#line 1224 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4008 "parser.cc"
    break;

  case 318: // list: texp "," exp ".."
#line 1225 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4014 "parser.cc"
    break;

  case 319: // list: texp ".." exp
#line 1226 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4020 "parser.cc"
    break;

  case 320: // list: texp "," exp ".." exp
#line 1227 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < Located<expression_ref> > (), yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4026 "parser.cc"
    break;

  case 321: // list: texp "|" squals
#line 1228 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4032 "parser.cc"
    break;

  case 322: // lexps: lexps "," texp
#line 1230 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4038 "parser.cc"
    break;

  case 323: // lexps: texp "," texp
#line 1231 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4044 "parser.cc"
    break;

  case 324: // squals: squals "," qual
#line 1244 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4050 "parser.cc"
    break;

  case 325: // squals: qual
#line 1246 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4056 "parser.cc"
    break;

  case 326: // guardquals: guardquals1
#line 1256 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[0].value.as < std::vector<Located<expression_ref>> > ();}
#line 4062 "parser.cc"
    break;

  case 327: // guardquals1: guardquals1 "," qual
#line 1258 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > ();yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4068 "parser.cc"
    break;

  case 328: // guardquals1: qual
#line 1259 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4074 "parser.cc"
    break;

  case 329: // altslist: "{" alts "}"
#line 1262 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4080 "parser.cc"
    break;

  case 330: // altslist: "vocurly" alts close
#line 1263 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4086 "parser.cc"
    break;

  case 331: // altslist: "{" "}"
#line 1264 "parser.y"
                                 {}
#line 4092 "parser.cc"
    break;

  case 332: // altslist: "vocurly" close
#line 1265 "parser.y"
                                 {}
#line 4098 "parser.cc"
    break;

  case 333: // alts: alts1
#line 1267 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4104 "parser.cc"
    break;

  case 334: // alts: ";" alts
#line 1268 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4110 "parser.cc"
    break;

  case 335: // alts1: alts1 ";" alt
#line 1270 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4116 "parser.cc"
    break;

  case 336: // alts1: alts1 ";"
#line 1271 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4122 "parser.cc"
    break;

  case 337: // alts1: alt
#line 1272 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4128 "parser.cc"
    break;

  case 338: // alt: pat alt_rhs
#line 1274 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 4134 "parser.cc"
    break;

  case 339: // alt_rhs: "->" exp wherebinds
#line 1276 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4140 "parser.cc"
    break;

  case 340: // alt_rhs: gdpats wherebinds
#line 1277 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4146 "parser.cc"
    break;

  case 341: // gdpats: gdpats gdpat
#line 1279 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4152 "parser.cc"
    break;

  case 342: // gdpats: gdpat
#line 1280 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4158 "parser.cc"
    break;

  case 343: // gdpat: "|" guardquals "->" exp
#line 1289 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4164 "parser.cc"
    break;

  case 344: // pat: exp
#line 1291 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4170 "parser.cc"
    break;

  case 345: // pat: PREFIX_BANG aexp
#line 1292 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4176 "parser.cc"
    break;

  case 346: // bindpat: exp
#line 1294 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4182 "parser.cc"
    break;

  case 347: // bindpat: PREFIX_BANG aexp
#line 1295 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4188 "parser.cc"
    break;

  case 348: // apat: aexp
#line 1297 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4194 "parser.cc"
    break;

  case 349: // apat: PREFIX_BANG aexp
#line 1298 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4200 "parser.cc"
    break;

  case 350: // apats1: apats1 apat
#line 1300 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4206 "parser.cc"
    break;

  case 351: // apats1: apat
#line 1301 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4212 "parser.cc"
    break;

  case 352: // stmtlist: "{" stmts "}"
#line 1304 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4218 "parser.cc"
    break;

  case 353: // stmtlist: "vocurly" stmts close
#line 1305 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4224 "parser.cc"
    break;

  case 354: // stmts: stmts ";" stmt
#line 1307 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4230 "parser.cc"
    break;

  case 355: // stmts: stmts ";"
#line 1308 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 4236 "parser.cc"
    break;

  case 356: // stmts: stmt
#line 1309 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4242 "parser.cc"
    break;

  case 357: // stmts: %empty
#line 1310 "parser.y"
                       {}
#line 4248 "parser.cc"
    break;

  case 358: // stmt: qual
#line 1315 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4254 "parser.cc"
    break;

  case 359: // stmt: "rec" stmtlist
#line 1316 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ())};}
#line 4260 "parser.cc"
    break;

  case 360: // qual: bindpat "<-" exp
#line 1318 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::PatQual(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4266 "parser.cc"
    break;

  case 361: // qual: exp
#line 1319 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::SimpleQual(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4272 "parser.cc"
    break;

  case 362: // qual: "let" binds
#line 1320 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ())};}
#line 4278 "parser.cc"
    break;

  case 370: // qcon: gen_qcon
#line 1365 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4284 "parser.cc"
    break;

  case 371: // qcon: sysdcon
#line 1366 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4290 "parser.cc"
    break;

  case 372: // gen_qcon: qconid
#line 1368 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4296 "parser.cc"
    break;

  case 373: // gen_qcon: "(" qconsym ")"
#line 1369 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4302 "parser.cc"
    break;

  case 374: // con: conid
#line 1371 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4308 "parser.cc"
    break;

  case 375: // con: "(" consym ")"
#line 1372 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4314 "parser.cc"
    break;

  case 376: // con: sysdcon
#line 1373 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4320 "parser.cc"
    break;

  case 377: // con_list: con_list "," con
#line 1375 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4326 "parser.cc"
    break;

  case 378: // con_list: con
#line 1376 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4332 "parser.cc"
    break;

  case 379: // sysdcon_no_list: "(" ")"
#line 1378 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 4338 "parser.cc"
    break;

  case 380: // sysdcon_no_list: "(" commas ")"
#line 1379 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4344 "parser.cc"
    break;

  case 381: // sysdcon_no_list: "(#" "#)"
#line 1380 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 4350 "parser.cc"
    break;

  case 382: // sysdcon_no_list: "(#" commas "#)"
#line 1381 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4356 "parser.cc"
    break;

  case 383: // sysdcon: sysdcon_no_list
#line 1383 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4362 "parser.cc"
    break;

  case 384: // sysdcon: "[" "]"
#line 1384 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 4368 "parser.cc"
    break;

  case 385: // conop: consym
#line 1386 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4374 "parser.cc"
    break;

  case 386: // conop: "`" conid "`"
#line 1387 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4380 "parser.cc"
    break;

  case 387: // qconop: qconsym
#line 1389 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4386 "parser.cc"
    break;

  case 388: // qconop: "`" qconid "`"
#line 1390 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4392 "parser.cc"
    break;

  case 389: // gtycon: ntgtycon
#line 1393 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4398 "parser.cc"
    break;

  case 390: // gtycon: "(" ")"
#line 1394 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 4404 "parser.cc"
    break;

  case 391: // gtycon: "(#" "#)"
#line 1395 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 4410 "parser.cc"
    break;

  case 392: // ntgtycon: oqtycon
#line 1397 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4416 "parser.cc"
    break;

  case 393: // ntgtycon: "(" commas ")"
#line 1398 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4422 "parser.cc"
    break;

  case 394: // ntgtycon: "(#" commas "#)"
#line 1399 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4428 "parser.cc"
    break;

  case 395: // ntgtycon: "(" "->" ")"
#line 1400 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 4434 "parser.cc"
    break;

  case 396: // ntgtycon: "[" "]"
#line 1401 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 4440 "parser.cc"
    break;

  case 397: // oqtycon: qtycon
#line 1403 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4446 "parser.cc"
    break;

  case 398: // oqtycon: "(" qtyconsym ")"
#line 1404 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4452 "parser.cc"
    break;

  case 399: // oqtycon: "(" "~" ")"
#line 1405 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 4458 "parser.cc"
    break;

  case 400: // oqtycon_no_varcon: qtycon
#line 1407 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4464 "parser.cc"
    break;

  case 401: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1408 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4470 "parser.cc"
    break;

  case 402: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1409 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4476 "parser.cc"
    break;

  case 403: // oqtycon_no_varcon: "(" ":" ")"
#line 1410 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 4482 "parser.cc"
    break;

  case 404: // oqtycon_no_varcon: "(" "~" ")"
#line 1411 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 4488 "parser.cc"
    break;

  case 405: // qtyconop: qtyconsym
#line 1414 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4494 "parser.cc"
    break;

  case 406: // qtyconop: "`" qtycon "`"
#line 1415 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4500 "parser.cc"
    break;

  case 407: // qtycondoc: qtycon
#line 1417 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4506 "parser.cc"
    break;

  case 408: // qtycon: "QCONID"
#line 1419 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4512 "parser.cc"
    break;

  case 409: // qtycon: tycon
#line 1420 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4518 "parser.cc"
    break;

  case 410: // tycon: "CONID"
#line 1424 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4524 "parser.cc"
    break;

  case 411: // qtyconsym: "QCONSYM"
#line 1426 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4530 "parser.cc"
    break;

  case 412: // qtyconsym: "QVARSYM"
#line 1427 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4536 "parser.cc"
    break;

  case 413: // qtyconsym: tyconsym
#line 1428 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4542 "parser.cc"
    break;

  case 414: // tyconsym: "CONSYM"
#line 1430 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4548 "parser.cc"
    break;

  case 415: // tyconsym: "VARSYM"
#line 1431 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4554 "parser.cc"
    break;

  case 416: // tyconsym: ":"
#line 1432 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4560 "parser.cc"
    break;

  case 417: // tyconsym: "-"
#line 1433 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 4566 "parser.cc"
    break;

  case 418: // op: varop
#line 1438 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4572 "parser.cc"
    break;

  case 419: // op: conop
#line 1439 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4578 "parser.cc"
    break;

  case 420: // varop: varsym
#line 1441 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4584 "parser.cc"
    break;

  case 421: // varop: "`" varid "`"
#line 1442 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4590 "parser.cc"
    break;

  case 422: // qop: qvarop
#line 1444 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4596 "parser.cc"
    break;

  case 423: // qop: qconop
#line 1445 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4602 "parser.cc"
    break;

  case 424: // qopm: qvaropm
#line 1448 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4608 "parser.cc"
    break;

  case 425: // qopm: qconop
#line 1449 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4614 "parser.cc"
    break;

  case 426: // qvarop: qvarsym
#line 1454 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4620 "parser.cc"
    break;

  case 427: // qvarop: "`" qvarid "`"
#line 1455 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4626 "parser.cc"
    break;

  case 428: // qvaropm: qvarsym_no_minus
#line 1457 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 4632 "parser.cc"
    break;

  case 429: // qvaropm: "`" qvarid "`"
#line 1458 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4638 "parser.cc"
    break;

  case 430: // tyvar: tyvarid
#line 1462 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4644 "parser.cc"
    break;

  case 431: // tyvarop: "`" tyvarid "`"
#line 1464 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4650 "parser.cc"
    break;

  case 432: // tyvarid: "VARID"
#line 1466 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4656 "parser.cc"
    break;

  case 433: // tyvarid: special_id
#line 1467 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4662 "parser.cc"
    break;

  case 434: // tyvarid: "unsafe"
#line 1468 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 4668 "parser.cc"
    break;

  case 435: // tyvarid: "safe"
#line 1469 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 4674 "parser.cc"
    break;

  case 436: // tyvarid: "interruptible"
#line 1470 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 4680 "parser.cc"
    break;

  case 437: // var: varid
#line 1473 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4686 "parser.cc"
    break;

  case 438: // var: "(" varsym ")"
#line 1474 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4692 "parser.cc"
    break;

  case 439: // qvar: qvarid
#line 1476 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4698 "parser.cc"
    break;

  case 440: // qvar: "(" varsym ")"
#line 1477 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4704 "parser.cc"
    break;

  case 441: // qvar: "(" qvarsym1 ")"
#line 1478 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4710 "parser.cc"
    break;

  case 442: // qvarid: varid
#line 1480 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4716 "parser.cc"
    break;

  case 443: // qvarid: "QVARID"
#line 1481 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4722 "parser.cc"
    break;

  case 444: // varid: "VARID"
#line 1483 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4728 "parser.cc"
    break;

  case 445: // varid: special_id
#line 1484 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4734 "parser.cc"
    break;

  case 446: // varid: "unsafe"
#line 1485 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 4740 "parser.cc"
    break;

  case 447: // varid: "safe"
#line 1486 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 4746 "parser.cc"
    break;

  case 448: // varid: "interruptible"
#line 1487 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 4752 "parser.cc"
    break;

  case 449: // varid: "forall"
#line 1488 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 4758 "parser.cc"
    break;

  case 450: // varid: "family"
#line 1489 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 4764 "parser.cc"
    break;

  case 451: // varid: "role"
#line 1490 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 4770 "parser.cc"
    break;

  case 452: // qvarsym: varsym
#line 1492 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4776 "parser.cc"
    break;

  case 453: // qvarsym: qvarsym1
#line 1493 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4782 "parser.cc"
    break;

  case 454: // qvarsym_no_minus: varsym_no_minus
#line 1495 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4788 "parser.cc"
    break;

  case 455: // qvarsym_no_minus: qvarsym1
#line 1496 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4794 "parser.cc"
    break;

  case 456: // qvarsym1: "QVARSYM"
#line 1498 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4800 "parser.cc"
    break;

  case 457: // varsym: varsym_no_minus
#line 1500 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4806 "parser.cc"
    break;

  case 458: // varsym: "-"
#line 1501 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 4812 "parser.cc"
    break;

  case 459: // varsym_no_minus: "VARSYM"
#line 1503 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4818 "parser.cc"
    break;

  case 460: // varsym_no_minus: special_sym
#line 1504 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4824 "parser.cc"
    break;

  case 461: // special_id: "as"
#line 1506 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 4830 "parser.cc"
    break;

  case 462: // special_id: "qualified"
#line 1507 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 4836 "parser.cc"
    break;

  case 463: // special_id: "hiding"
#line 1508 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 4842 "parser.cc"
    break;

  case 464: // special_id: "export"
#line 1509 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 4848 "parser.cc"
    break;

  case 465: // special_id: "label"
#line 1510 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 4854 "parser.cc"
    break;

  case 466: // special_id: "dynamic"
#line 1511 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 4860 "parser.cc"
    break;

  case 467: // special_id: "stdcall"
#line 1512 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 4866 "parser.cc"
    break;

  case 468: // special_id: "ccall"
#line 1513 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 4872 "parser.cc"
    break;

  case 469: // special_id: "capi"
#line 1514 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 4878 "parser.cc"
    break;

  case 470: // special_id: "prim"
#line 1515 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 4884 "parser.cc"
    break;

  case 471: // special_id: "javascript"
#line 1516 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 4890 "parser.cc"
    break;

  case 472: // special_id: "group"
#line 1517 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 4896 "parser.cc"
    break;

  case 473: // special_id: "stock"
#line 1518 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 4902 "parser.cc"
    break;

  case 474: // special_id: "anyclass"
#line 1519 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 4908 "parser.cc"
    break;

  case 475: // special_id: "via"
#line 1520 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 4914 "parser.cc"
    break;

  case 476: // special_id: "unit"
#line 1521 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 4920 "parser.cc"
    break;

  case 477: // special_id: "dependency"
#line 1522 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 4926 "parser.cc"
    break;

  case 478: // special_id: "signature"
#line 1523 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 4932 "parser.cc"
    break;

  case 479: // special_sym: "!"
#line 1525 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 4938 "parser.cc"
    break;

  case 480: // special_sym: "."
#line 1526 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 4944 "parser.cc"
    break;

  case 481: // special_sym: "*"
#line 1527 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 4950 "parser.cc"
    break;

  case 482: // qconid: conid
#line 1531 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4956 "parser.cc"
    break;

  case 483: // qconid: "QCONID"
#line 1532 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4962 "parser.cc"
    break;

  case 484: // conid: "CONID"
#line 1534 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4968 "parser.cc"
    break;

  case 485: // qconsym: consym
#line 1536 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4974 "parser.cc"
    break;

  case 486: // qconsym: "QCONSYM"
#line 1537 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4980 "parser.cc"
    break;

  case 487: // consym: "CONSYM"
#line 1539 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4986 "parser.cc"
    break;

  case 488: // consym: ":"
#line 1540 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4992 "parser.cc"
    break;

  case 489: // literal: "CHAR"
#line 1544 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 4998 "parser.cc"
    break;

  case 490: // literal: "STRING"
#line 1545 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 5004 "parser.cc"
    break;

  case 491: // literal: "INTEGER"
#line 1546 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < integer > ()});}
#line 5010 "parser.cc"
    break;

  case 492: // literal: "RATIONAL"
#line 1547 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Double{yystack_[0].value.as < double > ()});}
#line 5016 "parser.cc"
    break;

  case 493: // literal: "PRIMINTEGER"
#line 1548 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < integer > ()});}
#line 5022 "parser.cc"
    break;

  case 495: // close: error
#line 1556 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 5028 "parser.cc"
    break;

  case 496: // modid: "CONID"
#line 1560 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5034 "parser.cc"
    break;

  case 497: // modid: "QCONID"
#line 1561 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5040 "parser.cc"
    break;

  case 498: // commas: commas ","
#line 1563 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 5046 "parser.cc"
    break;

  case 499: // commas: ","
#line 1564 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 5052 "parser.cc"
    break;


#line 5056 "parser.cc"

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


  const short parser::yypact_ninf_ = -645;

  const short parser::yytable_ninf_ = -458;

  const short
  parser::yypact_[] =
  {
      79,   111,  -645,    93,  -645,  -645,  -645,  -645,  -645,   210,
     -23,   -17,  -645,    44,   -28,   -28,     0,  -645,  -645,  -645,
    -645,    96,  -645,  -645,  -645,    14,  -645,    75,   131,  4722,
     166,   192,   140,  -645,   733,  -645,    91,  -645,  -645,  -645,
    -645,   111,  -645,  -645,  -645,  -645,  -645,  -645,  -645,  -645,
    -645,  -645,  -645,  -645,  -645,  -645,  -645,  -645,  -645,  -645,
    -645,  -645,  -645,  -645,   294,  -645,  -645,  -645,  -645,   189,
     105,  -645,   243,  -645,  -645,  -645,  -645,  -645,  -645,  -645,
      32,  -645,   111,  -645,   190,  -645,  2470,  4313,  -645,   263,
     234,  2470,  -645,  -645,  -645,   356,   296,  -645,  3488,   305,
     234,  3268,   239,   231,  4992,   136,  2869,  3268,  3002,  3268,
    1539,  1406,   235,  -645,  -645,  -645,  -645,  -645,  -645,  -645,
      20,   239,   245,   140,  -645,  -645,  -645,  -645,   311,    73,
    -645,  -645,   432,  -645,  3135,  -645,   288,  -645,  -645,  -645,
    -645,  -645,  -645,   308,    97,  -645,  -645,  -645,  -645,   272,
    -645,   313,   329,  -645,  -645,  -645,  -645,  -645,   330,  -645,
     339,   344,   347,  -645,  -645,  -645,  4722,  4759,  -645,  -645,
    -645,  -645,   458,  -645,   -43,  1406,   440,   714,  -645,  -645,
    2470,  -645,  -645,  -645,  -645,  -645,  -645,  -645,  -645,  5088,
    3591,  3377,   365,  4918,  -645,  -645,  -645,  -645,  -645,   462,
    4620,  -645,   395,  -645,   267,  -645,  4620,  -645,  -645,  -645,
    -645,  -645,  -645,  -645,  -645,  -645,  -645,  -645,  -645,  3798,
    1938,  1938,  -645,   384,   415,   425,   426,   431,  3798,  1140,
    1140,  -645,   497,  4313,  4313,   101,   436,    -8,   106,   473,
    -645,  -645,   -26,  4992,  -645,   452,   373,   -22,   424,     1,
    -645,   107,  -645,  -645,  3268,  -645,  -645,  2736,  -645,  3135,
     297,  -645,  -645,  4857,  -645,  -645,  -645,   714,   104,   427,
     421,  -645,  2470,  -645,  -645,  -645,  -645,  -645,  -645,  3002,
    -645,  -645,    80,   114,   344,   428,   429,   430,   154,  -645,
     242,  3798,  4992,  4992,  -645,   879,   190,   413,  4313,  3798,
    5088,  2470,  2204,  4857,  -645,    31,  -645,  -645,  2603,  -645,
    -645,  -645,  -645,  4620,  -645,  4955,  3268,  -645,  -645,  -645,
    -645,  -645,  -645,  -645,  -645,   435,   437,   439,  -645,   444,
      44,   111,    34,   464,   471,   343,  3798,  2470,  -645,   583,
     123,   449,   446,  -645,  -645,  -645,  -645,   450,   480,   474,
    -645,   453,   456,  -645,   467,   463,   468,   181,   252,   469,
     472,   372,  -645,  -645,  4313,  3798,  4313,  -645,  -645,  -645,
     478,   476,   296,   234,  3268,   505,   511,   148,  -645,  -645,
      47,  -645,   573,  -645,  -645,  -645,  -645,  -645,  -645,   577,
     170,  -645,  -645,   432,    52,  2470,  -645,   519,   213,  3798,
      82,  3798,   483,   475,   504,   537,  -645,  -645,   539,   507,
     260,   136,   543,  -645,  2470,  -645,  -645,   506,   512,  2470,
    2470,  2204,  1672,  -645,  1672,   362,  -645,  1672,  -645,  1672,
     130,  -645,  -645,  -645,  -645,   545,   544,   548,  5051,   517,
    -645,  -645,  -645,  -645,  -645,    -3,   325,  -645,  -645,  -645,
    -645,   607,   554,   522,  -645,   523,   296,  -645,  -645,  -645,
    -645,  -645,   541,  -645,   525,   558,  -645,  -645,  -645,  4820,
    -645,  -645,  -645,   547,  4722,  -645,  -645,  -645,  -645,  1805,
    1273,  -645,  -645,  -645,   555,  3798,  -645,  5088,  5125,  -645,
    3798,  3798,  -645,  -645,  -645,  3798,  -645,  -645,  -645,  -645,
    -645,   874,   874,  -645,  -645,  -645,   564,  -645,  3798,   497,
    -645,  -645,  2470,  -645,  1938,  -645,  2470,   377,  -645,  -645,
    1140,  -645,  -645,  3798,  3798,  5231,   580,  -645,  -645,   241,
    -645,  -645,  5088,   562,  -645,  -645,  -645,  -645,   563,   866,
     258,  -645,  -645,  -645,  -645,  -645,  -645,  -645,  -645,   565,
    -645,   593,  -645,  -645,  -645,  -645,  -645,  3798,  3798,   566,
     568,   879,  -645,   609,  3798,   659,   661,   680,  -645,  2470,
    2204,  -645,  -645,  -645,  4955,  1672,  -645,  4722,   582,  3268,
    -645,  2071,  -645,   588,   579,  -645,   411,    44,  -645,  -645,
    -645,  -645,  3798,  5324,  5324,  -645,  -645,  -645,  -645,  -645,
     585,   660,  3695,  -645,  -645,   171,  -645,    60,  -645,  -645,
    -645,   384,  1007,  1007,  -645,  -645,  -645,  -645,  -645,  5324,
     669,   468,   619,  -645,  -645,  -645,  2204,  2470,  -645,    -2,
       6,  -645,  -645,  -645,  -645,  -645,  -645,   616,  -645,  4620,
     419,   680,    85,  -645,   680,  -645,  -645,  -645,  -645,  -645,
     595,  -645,  -645,  -645,  -645,  2337,  2204,  2470,  -645,    46,
    -645,  -645,  -645,   -13,   623,  -645,  -645,  4313,  4313,  4313,
    -645,   333,  -645,   874,  -645,   696,   689,  -645,  -645,   180,
    -645,    61,  -645,   626,   433,  -645,  3798,  -645,  -645,  -645,
    3798,  -645,  5198,   659,   625,  4416,  -645,  -645,  -645,   384,
     384,  -645,  -645,  -645,  -645,  3901,   116,   656,  -645,  -645,
    -645,  -645,  -645,   631,   607,  -645,  -645,  3798,  -645,  3798,
     638,  -645,   337,  3798,  4004,  -645,  -645,  2470,  -645,  4313,
    -645,  1007,  -645,  5324,  4107,  4210,  -645,  -645,  -645,  -645,
    -645,  4620,   599,  -645,  4620,   194,  -645,   136,    68,  -645,
    -645,   605,   614,  -645,  4313,  -645,  2470,  -645,   624,   615,
    3798,  -645,  5291,  -645,  -645,  3377,   643,   645,  -645,  -645,
    -645,  5324,  -645,   629,   196,  -645,    44,    70,  4518,  -645,
    4620,  -645,   384,   135,  -645,  4313,  -645,  -645,  -645,  -645,
    -645,  -645,   623,  5324,  -645,  -645,  -645,  4313,  -645,  -645,
    -645,  3798,  -645,  -645,  -645,  -645
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
     383,   371,   168,   303,   442,   372,   482,   305,   159,     0,
      23,     0,     0,   458,   479,   481,   480,   459,     0,   456,
       0,     0,     0,   457,   460,    17,     0,    27,    22,    36,
      36,     3,    44,    33,     0,     0,     0,   274,   282,   275,
       0,   435,   436,   434,   416,   173,   417,   172,   199,   236,
       0,     0,     0,     0,   432,   415,   414,   412,   411,   138,
       0,   171,     0,   125,   182,   185,   186,   191,   190,   197,
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
       0,     0,   237,   239,   437,   214,   396,     0,   174,     0,
     178,     0,     0,   202,   211,     0,   405,     0,     0,     0,
       0,     0,    67,   200,     0,     0,     0,   192,   193,   211,
       0,   209,     0,     0,     0,   361,     0,     0,   356,   358,
       0,   284,     0,    78,    77,    79,    80,   206,   165,   148,
       0,   254,   151,     0,     0,     0,    76,     0,   118,     0,
       0,     0,     0,     0,     0,     0,   280,   264,     0,     0,
       0,     0,     0,   349,     0,   350,   252,     0,     0,   311,
     317,     0,     0,   308,     0,   312,   306,     0,   307,     0,
     440,   373,   380,   498,   382,     0,     0,     0,     0,   261,
     419,    55,   418,   420,   385,     0,   114,   260,   179,   166,
     167,   157,     0,   326,   328,     0,     0,   256,   257,   278,
     289,   367,     0,   363,   366,   369,   292,    26,    25,     0,
       9,    10,    43,     0,     0,    40,    45,   287,   286,     0,
       0,   297,   273,   276,     0,     0,   201,     0,     0,   204,
       0,     0,   395,   399,   205,     0,   398,   393,   394,   406,
     431,   134,   134,   137,   124,   183,   187,    63,     0,   362,
     359,   347,     0,   352,   355,   353,     0,     0,    75,   153,
     150,   154,   295,     0,     0,     0,    86,   220,    72,     0,
      73,    68,     0,     0,   271,   263,   265,   375,     0,     0,
       0,   164,   389,   377,   262,   294,   429,   388,   319,   321,
     325,   310,   323,   322,   314,   313,   268,     0,     0,     0,
       0,     0,   127,     0,     0,   233,   223,   241,   255,     0,
       0,   427,   156,   301,     0,     0,    29,     0,     0,     0,
     331,     0,   344,     0,   333,   337,     0,     0,   332,   438,
     240,   238,     0,     0,     0,   213,   215,   218,   175,   177,
     212,   107,     0,   129,   133,     0,   130,     0,   212,   360,
     354,   285,   144,   144,   147,   149,   101,   119,   120,     0,
      91,     0,     0,   272,   390,   391,     0,   318,   169,     0,
       0,   421,   386,    54,   126,   115,   214,   227,   229,     0,
       0,   241,     0,    69,   242,   244,   259,   327,   365,   368,
       0,    47,   345,   334,   329,   336,     0,     0,   338,   157,
     342,   330,   176,     0,     0,   203,   108,     0,     0,     0,
     105,   121,   135,   132,   136,     0,   109,   139,   143,     0,
     140,     0,    87,     0,     0,    71,     0,   324,   320,   266,
       0,   267,     0,   233,     0,   234,   188,   196,   231,   285,
     285,    70,    84,    82,    83,     0,     0,   245,   248,   407,
     243,    48,   335,     0,   157,   340,   341,     0,   216,     0,
     116,   106,   121,     0,     0,   103,   131,     0,   110,     0,
     145,   142,   146,     0,   100,   100,    92,    64,   170,   232,
     228,     0,     0,   189,     0,     0,   225,     0,     0,   249,
     184,   207,     0,   246,     0,   247,     0,   339,     0,     0,
       0,   102,     0,   104,   122,     0,     0,   198,   296,   111,
     141,    88,    90,     0,     0,    99,     0,     0,   234,   230,
     235,   221,   285,     0,   222,     0,   250,    85,   343,   217,
     219,   117,   198,     0,    89,    95,    93,    98,    96,    94,
     224,     0,   208,   123,    97,   226
  };

  const short
  parser::yypgoto_[] =
  {
    -645,  -645,  -645,  -645,  -645,  -645,  -645,    29,  -645,  -645,
    -409,  -645,   567,  -645,  -645,  -645,  -152,   611,  -645,  -645,
    -645,  -645,  -645,  -645,  -645,  -645,  -645,  -645,  -645,  -645,
    -645,  -645,  -645,  -645,  -645,  -645,  -645,  -645,  -645,  -645,
    -645,   -58,  -645,  -645,  -645,     7,  -225,  -645,  -645,  -645,
    -645,  -645,  -645,  -645,  -645,    22,   460,  -645,    74,   246,
    -645,  -645,    23,   143,  -645,  -645,   530,  -645,  -325,  -411,
     763,  -645,  -645,  -315,    95,  -162,   221,  -645,  -645,  -149,
     -99,  -645,   -71,  -645,   -84,  -645,   -74,  -645,  -350,  -645,
    -645,  -645,  -607,  -171,   491,    12,  -645,   572,   156,   273,
    -634,  -452,  -645,    99,    18,  -645,  -645,   108,  -645,    62,
    -645,  -645,   319,   169,  -645,   167,   110,   778,  -187,   557,
    -645,   509,  -645,   314,  -645,   147,   -93,   784,    -1,  -281,
    -211,  -645,   -78,   153,  -645,  -645,  -101,  -645,  -645,  -645,
    -645,   163,  -645,  -645,  -436,  -645,   165,  -645,  -645,   164,
    -645,  -645,   569,  -645,   -72,   601,   315,  -266,  -645,   253,
    -645,  -645,  -645,   417,    83,  -645,   -95,  -644,   -83,  -645,
     422,   -76,  -645,  -645,  -645,   -27,  -645,  -190,  -645,   276,
    -645,  -151,  -645,  -645,  -645,  -424,  -645,  -174,  -263,    -9,
    -204,   -11,  -645,  -645,   -14,   -51,   -79,   -87,  -645,  -177,
     -98,   -54,  -243,  -645,  -326,   -16,  -107
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   171,     6,    10,    19,    30,
      69,    70,    71,   168,   326,   327,    72,    84,    11,    20,
      21,    32,    82,   332,   475,   476,   295,   122,   439,    33,
      34,   123,   124,   125,   126,   235,   127,   228,   706,   755,
     620,   682,   771,   685,   736,   774,   775,   603,   667,   729,
     677,   128,   566,   761,   526,   725,   199,   298,   604,   605,
     503,   362,   678,   679,   614,   518,   390,   231,   232,   457,
      27,    36,   412,   387,   447,   129,   629,   200,   201,   354,
     527,   449,   349,   694,   350,   751,   204,   205,   695,   206,
     367,   207,   696,   208,   389,   752,   370,   355,   488,   595,
     596,   528,   641,   745,   746,   567,   637,   638,   639,   698,
     341,   342,   343,   643,   644,   645,   707,   391,   606,   304,
     305,   306,   131,   243,   244,   375,   177,   393,   178,   179,
     747,   180,   134,   135,   136,   137,   282,   283,   269,   270,
     549,   452,   453,   481,   583,   584,   585,   658,   659,   660,
     586,   376,   256,   257,   222,   377,   378,   379,   462,   463,
     464,   138,   139,   250,   251,   140,   141,   440,   271,   541,
     209,   210,    73,   211,   708,   212,    75,   213,   214,   441,
     442,   308,   272,   309,   273,   215,   216,   217,   142,   143,
      77,    78,   310,   274,   275,   312,   163,    79,   164,   145,
     146,   277,   278,   147,    24,     9,   288
  };

  const short
  parser::yytable_[] =
  {
     218,   356,    74,   203,   471,   290,   409,   253,   396,   268,
     252,   218,   382,   162,   236,   328,   202,   267,   267,   360,
      76,   482,   238,   144,   237,   150,   337,   340,   240,   363,
     259,   276,   286,   133,    13,   368,   454,   450,   598,   291,
     568,   347,   392,   392,   587,    22,   473,   509,    22,   307,
     161,   744,   444,    22,   515,   407,   483,   287,   456,   417,
     285,    22,    22,   403,   597,   578,   172,   717,   521,    22,
     369,    22,   617,   456,   562,   689,   333,    25,   365,    17,
     261,   366,   267,   691,   357,   358,   418,   334,   743,   718,
     766,   348,   348,    12,   307,   247,   286,   284,    18,   455,
       1,   597,    26,   218,   218,   404,   218,   702,   408,    29,
      31,   280,   635,   218,   690,   302,   419,   281,   311,   218,
     348,   287,   690,   264,   162,   242,   418,   563,   766,   388,
     656,   572,   218,   169,   744,   170,   703,   704,   483,    74,
      74,   218,   460,   474,    35,   653,   218,   218,    23,   397,
     398,    23,    37,   299,   588,   550,    23,    76,    76,   237,
     237,   284,   514,   311,    23,    23,   359,   520,   650,   663,
     664,   743,    23,   743,    23,   673,   731,  -437,   344,   425,
       2,   399,   420,   782,   307,   797,   -74,   410,   421,   300,
     426,   529,   388,    80,   705,   162,   427,   148,   329,   330,
     448,   259,    66,   485,   218,    66,    68,   149,    38,    68,
    -438,   218,   218,  -437,   203,   801,    81,   400,   144,   144,
     422,   166,   -74,   411,   428,   705,   218,   202,   133,   133,
     429,     7,   161,   176,   405,     8,    66,   388,   223,   300,
      68,   248,   628,   628,   443,   249,  -438,   112,   715,   218,
     513,   411,   531,   311,   241,    83,   113,   266,   266,   255,
     258,   661,   260,   514,   432,   758,   505,   759,   597,   622,
     433,   764,   519,   672,   337,    14,    15,   218,   218,   218,
     504,   674,   730,   436,   437,   520,   673,   314,   484,   344,
     237,   497,   506,   524,   525,   731,   781,   433,   796,   165,
     767,   510,   348,   757,   647,   173,   465,   459,   791,   782,
     307,   797,   218,   253,   218,   472,   252,   576,   444,   239,
     184,   552,   266,   553,   530,   340,   554,   338,   555,   267,
     352,   267,   186,   615,   267,   220,   267,   221,   597,   621,
     560,   792,   307,   276,   242,   276,   600,   289,   276,   621,
     276,   281,   167,   365,   434,   732,   366,  -180,   433,   608,
     687,   245,   195,   196,   498,   538,   197,   198,   433,   539,
     625,   540,   219,   151,   281,   738,   294,   670,   301,   311,
      66,   302,   297,   152,    68,   153,   590,   154,   155,   315,
     454,   778,   599,   156,   780,   316,   348,   229,   218,   230,
     675,   218,   317,   218,   218,   564,   565,   413,   218,   348,
     255,   311,   314,   723,   724,   157,   158,   723,   762,   159,
     160,   218,   784,   318,   616,   680,   680,   559,   224,   225,
     226,   227,   357,   358,   292,   293,   218,   218,   218,   319,
     320,   261,    74,   721,   479,   683,   480,    74,   451,   321,
     798,   799,   444,   153,   322,   154,   155,   323,   388,   388,
      76,   156,   331,   335,   153,    76,   154,   155,   697,   466,
     218,   218,   156,   501,   649,   502,   344,   218,   612,   303,
     613,   281,   267,   157,   264,   364,   805,   159,   265,   361,
     144,   144,   383,   662,   157,   656,   276,   657,   159,   381,
     133,   133,   384,   385,   769,   218,   218,   218,   386,   144,
     443,   261,   395,   301,   402,   218,   302,   401,   671,   133,
     699,   344,   700,   153,   697,   154,   155,   511,   237,   406,
     262,   156,   218,   423,   734,   444,   735,   424,   430,  -457,
     431,   477,   522,   445,   680,   467,   470,   468,   478,   303,
      74,   486,   218,   157,   264,   469,   489,   159,   265,   772,
     490,   545,   487,   492,   491,   465,   493,   548,    76,   551,
     697,   266,   804,   697,   266,   356,   266,   494,   496,   495,
     218,   218,   218,   720,   397,   722,   499,   448,   507,   500,
    -346,   388,   508,   237,   237,   237,   512,   794,   516,   218,
     523,   144,   144,   218,   517,   218,   533,   697,   218,   697,
     534,   133,   133,   532,   535,   709,   536,   537,   218,   683,
     544,   750,   556,   546,   557,   616,   582,   582,   558,   547,
     218,   237,   218,   561,   456,   569,   218,   218,   570,   575,
     571,   574,   218,   573,   560,   397,   218,   218,   218,   253,
     397,   397,   252,   366,   218,   237,   577,   218,   357,   609,
     237,   237,   144,   611,   619,   589,   348,   218,   623,   346,
     787,   627,   133,   218,   153,   218,   154,   155,   218,   709,
     237,   626,   156,   631,   218,   632,   634,   636,   640,   642,
     654,   218,   651,   218,   655,   665,   684,   666,   218,   686,
     693,   750,   388,   719,   157,   711,   218,   727,   728,   754,
     218,   237,   733,   397,   218,   741,   646,   756,   760,   113,
     144,   785,   266,   237,   786,   790,   789,   793,   582,  -218,
     133,   795,   652,   324,   296,   803,    85,    39,    86,    87,
      88,    89,   777,    90,   763,    40,    91,   726,   607,    92,
      93,    94,    95,    96,   770,    97,   681,    42,   446,    98,
     394,    43,    99,    44,    45,    46,    47,    48,    49,   100,
      50,    51,    52,    53,   688,    54,    55,    56,    28,   630,
      57,   737,   435,   101,    58,    59,    60,    61,    62,    63,
     102,   371,   692,   261,   336,   103,   104,   802,   618,   748,
     800,   740,   582,   779,   714,   153,   591,   154,   155,   105,
     701,   710,   130,   156,   458,   106,   753,   416,   132,   713,
     712,   107,   380,   716,   108,   109,   415,   648,   543,   610,
     783,   303,   542,     0,     0,   157,   264,   633,   110,   159,
     265,     0,   111,     0,   112,     0,     0,     0,     0,     0,
       0,     0,    65,   113,     0,     0,    67,   114,     0,     0,
       0,     0,   115,   116,   117,   118,     0,     0,   119,     0,
       0,     0,   120,   121,   768,     0,     0,    85,    39,    86,
       0,   601,     0,     0,    90,     0,    40,    91,     0,     0,
      92,    93,    94,     0,    96,     0,     0,     0,    42,     0,
     602,     0,    43,   788,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,   102,     0,     0,     0,     0,   103,   104,     0,     0,
       0,     0,     0,     0,     0,   184,     0,     0,     0,     0,
     105,     0,   351,     0,     0,   352,   106,   186,   261,     0,
       0,     0,   107,     0,     0,   108,   109,     0,     0,     0,
     153,     0,   154,   155,     0,     0,   624,     0,   156,   110,
       0,     0,   281,   111,     0,   112,     0,   195,   196,     0,
       0,   197,   198,    65,   113,     0,   438,    67,   114,     0,
     157,   264,     0,   115,   116,   117,   118,     0,     0,   119,
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
       0,   107,     0,     0,   108,   579,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    23,   110,     0,
       0,     0,   175,     0,   112,     0,     0,     0,   581,     0,
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
       0,     0,     0,   107,     0,     0,   108,   579,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   580,     0,     0,
     110,     0,     0,     0,   175,     0,   112,     0,     0,     0,
     581,     0,     0,     0,    65,   113,     0,     0,    67,   114,
       0,     0,     0,     0,   115,   116,   117,   118,     0,     0,
     119,    85,    39,    86,     0,     0,     0,     0,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,   372,     0,
       0,     0,    42,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,     0,
      54,    55,    56,     0,   373,    57,     0,     0,   101,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
     103,   174,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,   107,     0,     0,   108,
     374,     0,     0,     0,     0,     0,     0,     0,     0,     0,
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
       0,     0,   108,   579,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
     175,     0,   112,     0,     0,     0,   581,     0,     0,     0,
      65,   113,     0,     0,    67,   114,     0,     0,     0,     0,
     115,   116,   117,   118,     0,     0,   119,    85,    39,    86,
       0,     0,     0,     0,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,   372,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,   103,   174,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,   107,     0,     0,   108,   374,     0,     0,     0,
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
       0,     0,     0,     0,     0,   107,     0,     0,   108,   579,
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
       0,     0,   414,     0,   107,     0,     0,     0,   254,     0,
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
       0,     0,     0,   351,     0,   185,   352,     0,   186,   187,
       0,   188,     0,     0,     0,     0,     0,     0,   189,     0,
       0,     0,   190,     0,     0,     0,   191,   353,   192,     0,
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
     191,   749,   192,     0,     0,     0,    40,     0,   193,     0,
     194,    66,   195,   196,     0,    68,   197,   198,    42,     0,
       0,     0,   345,     0,    44,    45,    46,   181,   182,   183,
       0,     0,     0,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   184,     0,     0,     0,     0,     0,     0,
       0,     0,   185,     0,     0,   186,   187,     0,   188,     0,
       0,     0,     0,     0,     0,   189,     0,     0,     0,   190,
       0,    39,     0,   765,     0,   192,     0,     0,     0,    40,
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
      40,     0,     0,   742,     0,   194,    66,     0,   264,     0,
      68,     0,    42,     0,     0,     0,     0,     0,    44,    45,
      46,   181,   182,   183,     0,     0,     0,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   261,     0,     0,
       0,     0,     0,     0,     0,     0,   185,     0,     0,     0,
     187,     0,   188,     0,     0,     0,     0,     0,     0,   189,
       0,     0,     0,   190,    39,     0,     0,   191,     0,   192,
       0,     0,    40,     0,     0,   742,     0,   194,    66,     0,
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
      51,    52,    53,   461,    54,    55,    56,   194,    66,    57,
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
       0,     0,    42,     0,   592,     0,   593,     0,    44,    45,
      46,   181,   182,   183,   594,    39,     0,    52,    53,     0,
      54,    55,    56,    40,   194,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,    42,     0,     0,     0,     0,
       0,    44,    45,    46,   181,   182,   183,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,    39,     0,   739,     0,   593,
       0,     0,     0,    40,     0,     0,     0,   594,     0,     0,
       0,     0,     0,     0,     0,    42,     0,   194,     0,     0,
       0,    44,    45,    46,   181,   182,   183,     0,    39,     0,
      52,    53,   593,    54,    55,    56,    40,     0,    57,     0,
     594,     0,    58,    59,    60,    61,    62,    63,    42,     0,
     194,     0,     0,     0,    44,    45,    46,   181,   182,   183,
       0,     0,     0,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     594,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     194,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   194
  };

  const short
  parser::yycheck_[] =
  {
      87,   191,    29,    87,   330,   112,   249,   105,   233,   110,
     105,    98,   223,    64,    98,   167,    87,   110,   111,   193,
      29,   336,    98,    34,    98,    41,   177,   189,   100,   200,
     108,   110,   111,    34,     5,   206,   302,   300,   490,    19,
     451,   190,   229,   230,   480,     1,    12,   372,     1,   132,
      64,   695,   295,     1,   380,    77,   337,   111,    27,   263,
     111,     1,     1,    89,   488,   474,    82,    80,   394,     1,
     219,     1,   524,    27,    77,    77,   119,   105,    86,   102,
      79,    89,   175,    77,   191,   192,   263,   130,   695,   102,
     724,   190,   191,     0,   177,   104,   175,   111,   115,   303,
      21,   525,   130,   190,   191,   131,   193,    22,   130,   109,
      14,   110,   564,   200,   116,    84,   267,   116,   132,   206,
     219,   175,   116,   122,   175,   105,   303,   130,   762,   228,
      84,   456,   219,   101,   778,   103,    51,    52,   419,   166,
     167,   228,   313,   109,   130,   581,   233,   234,   104,   233,
     234,   104,    77,    80,   480,   421,   104,   166,   167,   233,
     234,   175,   115,   177,   104,   104,   193,   115,   577,   593,
     594,   778,   104,   780,   104,   115,   115,    80,   189,   272,
     101,    80,    78,   115,   267,   115,    80,    80,    84,   116,
     110,   109,   291,    27,   109,   246,   116,   106,   169,   170,
     299,   279,   120,    80,   291,   120,   124,   116,    77,   124,
      80,   298,   299,   116,   298,    80,    24,   116,   229,   230,
     116,   116,   116,   116,   110,   109,   313,   298,   229,   230,
     116,   120,   246,    86,   243,   124,   120,   336,    91,   116,
     124,   105,   557,   558,   295,   109,   116,   111,   659,   336,
     102,   116,   401,   267,   101,   115,   120,   110,   111,   106,
     107,   587,   109,   115,   110,   717,   365,   719,   692,   532,
     116,   723,   102,   102,   425,    65,    66,   364,   365,   366,
     364,   607,   102,   292,   293,   115,   115,   134,   339,   300,
     364,   110,   366,    80,    81,   115,   102,   116,   102,   110,
     724,   373,   401,   714,   570,   115,   315,   308,   760,   115,
     393,   115,   399,   411,   401,   331,   411,   469,   561,    14,
      79,   422,   175,   424,   400,   487,   427,   180,   429,   422,
      89,   424,    91,   520,   427,   101,   429,   103,   762,   529,
     438,   765,   425,   422,   105,   424,   495,   112,   427,   539,
     429,   116,   109,    86,   112,   681,    89,    90,   116,   508,
     626,   130,   121,   122,   112,   105,   125,   126,   116,   109,
     112,   111,   109,    79,   116,   690,   131,   602,    81,   393,
     120,    84,    71,    89,   124,    91,   485,    93,    94,   101,
     656,   741,   491,    99,   744,    87,   495,   101,   485,   103,
     611,   488,   130,   490,   491,    80,    81,   254,   495,   508,
     257,   425,   259,    80,    81,   121,   122,    80,    81,   125,
     126,   508,   748,   110,   523,   612,   613,   438,    72,    73,
      74,    75,   539,   540,   120,   121,   523,   524,   525,   110,
     110,    79,   469,   668,   101,   619,   103,   474,   301,   110,
     776,   777,   695,    91,   110,    93,    94,   110,   557,   558,
     469,    99,     4,    23,    91,   474,    93,    94,   639,   316,
     557,   558,    99,   101,   575,   103,   487,   564,   101,   117,
     103,   116,   575,   121,   122,    90,   801,   125,   126,    27,
     501,   502,    77,   592,   121,    84,   575,    86,   125,   115,
     501,   502,    77,    77,   729,   592,   593,   594,    77,   520,
     561,    79,    15,    81,    41,   602,    84,    81,   602,   520,
     101,   532,   103,    91,   695,    93,    94,   374,   602,    77,
     106,    99,   619,   106,   101,   778,   103,   116,   110,   110,
     110,    77,   395,   130,   731,   110,   102,   110,    77,   117,
     577,   102,   639,   121,   122,   116,   106,   125,   126,   733,
      80,   414,   116,   110,    90,   574,   110,   420,   577,   422,
     741,   424,   797,   744,   427,   765,   429,   110,   110,   116,
     667,   668,   669,   667,   668,   669,   117,   686,   110,   117,
      85,   690,   116,   667,   668,   669,    85,   771,    25,   686,
      81,   612,   613,   690,    27,   692,   131,   778,   695,   780,
     106,   612,   613,   130,    77,   642,    77,   110,   705,   793,
      77,   705,    77,   117,    80,   724,   479,   480,    80,   117,
     717,   705,   719,   116,    27,    81,   723,   724,   116,    81,
     117,   116,   729,   102,   742,   729,   733,   734,   735,   747,
     734,   735,   747,    89,   741,   729,   109,   744,   765,   512,
     734,   735,   673,   516,    84,   110,   765,   754,   106,   106,
     754,    78,   673,   760,    91,   762,    93,    94,   765,   706,
     754,   116,    99,   117,   771,   117,    77,    28,    27,     9,
     102,   778,   110,   780,   115,   110,    27,    37,   785,    80,
      84,   785,   801,    80,   121,   110,   793,    11,    19,    53,
     797,   785,    86,   797,   801,    90,   569,    86,    80,   120,
     731,   116,   575,   797,   110,   110,   102,    84,   581,    84,
     731,   102,   579,   166,   123,   793,     3,     4,     5,     6,
       7,     8,   735,    10,   722,    12,    13,   673,   502,    16,
      17,    18,    19,    20,   731,    22,   613,    24,   298,    26,
     230,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,   627,    42,    43,    44,    15,   558,
      47,   686,   291,    50,    51,    52,    53,    54,    55,    56,
      57,   219,   636,    79,    80,    62,    63,   785,   525,   700,
     782,   693,   655,   741,   657,    91,   487,    93,    94,    76,
     641,   644,    34,    99,   305,    82,   706,   260,    34,   656,
     655,    88,   221,   659,    91,    92,   257,   574,   411,   514,
     747,   117,   410,    -1,    -1,   121,   122,   561,   105,   125,
     126,    -1,   109,    -1,   111,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   119,   120,    -1,    -1,   123,   124,    -1,    -1,
      -1,    -1,   129,   130,   131,   132,    -1,    -1,   135,    -1,
      -1,    -1,   139,   140,   727,    -1,    -1,     3,     4,     5,
      -1,     7,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      16,    17,    18,    -1,    20,    -1,    -1,    -1,    24,    -1,
      26,    -1,    28,   756,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      76,    -1,    86,    -1,    -1,    89,    82,    91,    79,    -1,
      -1,    -1,    88,    -1,    -1,    91,    92,    -1,    -1,    -1,
      91,    -1,    93,    94,    -1,    -1,   110,    -1,    99,   105,
      -1,    -1,   116,   109,    -1,   111,    -1,   121,   122,    -1,
      -1,   125,   126,   119,   120,    -1,   117,   123,   124,    -1,
     121,   122,    -1,   129,   130,   131,   132,    -1,    -1,   135,
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
     139,   140,   168,   172,   173,   174,   175,   177,   192,   216,
     258,   263,   268,   269,   273,   274,   275,   276,   302,   303,
     306,   307,   329,   330,   332,   340,   341,   344,   106,   116,
     346,    79,    89,    91,    93,    94,    99,   121,   122,   125,
     126,   335,   336,   337,   339,   110,   116,   109,   154,   101,
     103,   146,   346,   115,    63,   109,   266,   267,   269,   270,
     272,    33,    34,    35,    79,    88,    91,    92,    94,   101,
     105,   109,   111,   117,   119,   121,   122,   125,   126,   197,
     218,   219,   223,   225,   227,   228,   230,   232,   234,   311,
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
     216,   251,   252,   253,   332,    28,   106,   220,   221,   223,
     225,    86,    89,   110,   220,   238,   318,   347,   347,   316,
     328,    27,   202,   234,    90,    86,    89,   231,   234,   220,
     237,   238,    20,    46,    92,   266,   292,   296,   297,   298,
     296,   115,   271,    77,    77,    77,    77,   214,   221,   235,
     207,   258,   259,   268,   207,    15,   187,   225,   225,    80,
     116,    81,    41,    89,   131,   330,    77,    77,   130,   343,
      80,   116,   213,   274,    86,   293,   260,   331,   340,   322,
      78,    84,   116,   106,   116,   267,   110,   116,   110,   116,
     110,   110,   110,   116,   112,   235,   330,   330,   117,   169,
     308,   320,   321,   336,   343,   130,   197,   215,   221,   222,
     329,   266,   282,   283,   298,   331,    27,   210,   262,   269,
     234,    78,   299,   300,   301,   330,   274,   110,   110,   116,
     102,   345,   346,    12,   109,   165,   166,    77,    77,   101,
     103,   284,   214,   270,   336,    80,   102,   116,   239,   106,
      80,    90,   110,   110,   110,   116,   110,   110,   112,   117,
     117,   101,   103,   201,   225,   221,   227,   110,   116,   209,
     295,   274,    85,   102,   115,   345,    25,    27,   206,   102,
     115,   345,   266,    81,    80,    81,   195,   221,   242,   109,
     312,   220,   130,   131,   106,    77,    77,   110,   105,   109,
     111,   310,   311,   304,    77,   266,   117,   117,   266,   281,
     298,   266,   277,   277,   277,   277,    77,    80,    80,   332,
     341,   116,    77,   130,    80,    81,   193,   246,   210,    81,
     116,   117,   209,   102,   116,    81,   157,   109,   151,    92,
     102,   115,   266,   285,   286,   287,   291,   285,   345,   110,
     221,   253,    99,   101,   109,   240,   241,   326,   242,   221,
     220,     7,    26,   188,   199,   200,   259,   200,   220,   266,
     297,   266,   101,   103,   205,   259,   221,   242,   240,    84,
     181,   318,   329,   106,   110,   112,   116,    78,   214,   217,
     217,   117,   117,   320,    77,   242,    28,   247,   248,   249,
      27,   243,     9,   254,   255,   256,   266,   298,   300,   277,
     151,   110,   274,   285,   102,   115,    84,    86,   288,   289,
     290,   345,   221,   326,   326,   110,    37,   189,    19,    37,
     187,   225,   102,   115,   345,   271,    26,   191,   203,   204,
     259,   204,   182,   328,    27,   184,    80,   298,   266,    77,
     116,    77,   239,    84,   224,   229,   233,   234,   250,   101,
     103,   254,    22,    51,    52,   109,   179,   257,   315,   316,
     256,   110,   287,   282,   266,   210,   290,    80,   102,    80,
     225,   187,   225,    80,    81,   196,   199,    11,    19,   190,
     102,   115,   345,    86,   101,   103,   185,   215,   214,    99,
     248,    90,   117,   233,   308,   244,   245,   271,   244,   110,
     225,   226,   236,   257,    53,   180,    86,   210,   242,   242,
      80,   194,    81,   196,   242,   109,   241,   326,   266,   187,
     203,   183,   328,    78,   186,   187,    78,   186,   229,   250,
     229,   102,   115,   305,   345,   116,   110,   225,   266,   102,
     110,   242,   326,    84,   328,   102,   102,   115,   345,   345,
     245,    80,   236,   182,   187,   214
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
     212,   212,   212,   213,   213,   214,   215,   216,   216,   217,
     217,   218,   219,   219,   220,   220,   221,   221,   221,   222,
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
       0,     3,     4,     3,     4,     2,     3,     0,     1,     0,
       1,     3,     1,     1,     0,     2,     0,     2,     0,     2,
       2,     0,     2,     4,     3,     1,     4,     3,     0,     1,
       1,     3,     2,     1,     0,     3,     3,     2,     0,     1,
       1,     3,     2,     1,     0,     3,     3,     2,     0,     3,
       2,     1,     0,     3,     3,     1,     2,     0,     1,     3,
       3,     1,     0,     0,     2,     1,     1,     3,     1,     1,
       3,     1,     1,     1,     1,     3,     4,     3,     1,     1,
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
  "opt_tyconsig", "sigtype", "sigtypedoc", "sig_vars", "sigtypes1",
  "strict_mark", "strictness", "ktype", "ctype", "ctypedoc", "context",
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
       0,   507,   507,   524,   525,   527,   531,   532,   533,   535,
     536,   538,   539,   542,   544,   545,   546,   554,   555,   557,
     559,   560,   562,   563,   565,   566,   567,   569,   570,   572,
     573,   575,   576,   580,   581,   583,   584,   586,   588,   589,
     591,   604,   605,   607,   608,   610,   611,   615,   616,   621,
     622,   624,   625,   626,   628,   629,   633,   635,   636,   638,
     639,   640,   641,   644,   645,   652,   654,   656,   658,   659,
     661,   662,   665,   667,   668,   671,   672,   676,   677,   678,
     679,   680,   682,   683,   684,   686,   697,   698,   700,   702,
     703,   707,   708,   710,   711,   712,   713,   715,   716,   717,
     718,   720,   723,   725,   727,   729,   730,   732,   732,   734,
     734,   738,   740,   741,   745,   746,   748,   749,   751,   752,
     753,   755,   756,   757,   761,   762,   764,   765,   766,   808,
     809,   811,   812,   813,   814,   816,   817,   819,   820,   822,
     823,   825,   826,   827,   828,   830,   831,   833,   834,   837,
     838,   839,   840,   842,   843,   845,   847,   848,   856,   857,
     859,   860,   861,   874,   875,   884,   886,   888,   889,   891,
     892,   896,   902,   903,   910,   911,   913,   914,   916,   918,
     927,   929,   931,   932,   934,   937,   939,   940,   942,   943,
     945,   946,   947,   949,   951,   952,   959,   966,   967,   968,
     969,   970,   971,   972,   978,   979,   982,   984,   985,   987,
     988,   990,   991,   998,   999,  1001,  1002,  1003,  1006,  1007,
    1025,  1031,  1032,  1033,  1035,  1036,  1038,  1040,  1042,  1043,
    1045,  1046,  1048,  1049,  1051,  1052,  1054,  1055,  1057,  1058,
    1060,  1062,  1063,  1065,  1066,  1068,  1069,  1070,  1072,  1073,
    1074,  1079,  1081,  1082,  1084,  1088,  1089,  1091,  1092,  1096,
    1106,  1107,  1109,  1110,  1111,  1112,  1113,  1114,  1115,  1118,
    1119,  1121,  1122,  1127,  1128,  1132,  1133,  1135,  1136,  1138,
    1139,  1140,  1143,  1144,  1147,  1148,  1150,  1151,  1156,  1157,
    1158,  1159,  1162,  1163,  1164,  1165,  1167,  1169,  1170,  1171,
    1173,  1176,  1177,  1180,  1181,  1182,  1183,  1184,  1189,  1190,
    1196,  1197,  1198,  1203,  1204,  1222,  1223,  1224,  1225,  1226,
    1227,  1228,  1230,  1231,  1244,  1246,  1256,  1258,  1259,  1262,
    1263,  1264,  1265,  1267,  1268,  1270,  1271,  1272,  1274,  1276,
    1277,  1279,  1280,  1289,  1291,  1292,  1294,  1295,  1297,  1298,
    1300,  1301,  1304,  1305,  1307,  1308,  1309,  1310,  1315,  1316,
    1318,  1319,  1320,  1325,  1326,  1328,  1329,  1330,  1332,  1333,
    1365,  1366,  1368,  1369,  1371,  1372,  1373,  1375,  1376,  1378,
    1379,  1380,  1381,  1383,  1384,  1386,  1387,  1389,  1390,  1393,
    1394,  1395,  1397,  1398,  1399,  1400,  1401,  1403,  1404,  1405,
    1407,  1408,  1409,  1410,  1411,  1414,  1415,  1417,  1419,  1420,
    1424,  1426,  1427,  1428,  1430,  1431,  1432,  1433,  1438,  1439,
    1441,  1442,  1444,  1445,  1448,  1449,  1454,  1455,  1457,  1458,
    1462,  1464,  1466,  1467,  1468,  1469,  1470,  1473,  1474,  1476,
    1477,  1478,  1480,  1481,  1483,  1484,  1485,  1486,  1487,  1488,
    1489,  1490,  1492,  1493,  1495,  1496,  1498,  1500,  1501,  1503,
    1504,  1506,  1507,  1508,  1509,  1510,  1511,  1512,  1513,  1514,
    1515,  1516,  1517,  1518,  1519,  1520,  1521,  1522,  1523,  1525,
    1526,  1527,  1531,  1532,  1534,  1536,  1537,  1539,  1540,  1544,
    1545,  1546,  1547,  1548,  1553,  1556,  1560,  1561,  1563,  1564
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
#line 7096 "parser.cc"

#line 1573 "parser.y"


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

Hs::TypeFamilyDecl make_type_family(const Located<Hs::Type>& lhs_type, const std::optional<Located<Hs::Kind>>& kind_sig,
                                    const std::optional<std::vector<Hs::TypeFamilyInstanceEqn>>& eqns)
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

    return Hs::TypeFamilyDecl(*con, tyvars, kind, eqns);
}

Hs::TypeFamilyInstanceEqn make_type_family_instance_eqn(const Located<Hs::Type>& lhs_type, const Located<Hs::Type>& rhs_type)
{
    auto [head, args] = Hs::decompose_type_apps(lhs_type.value());

    // Get type con
    auto con = head.to<Hs::TypeCon>();
    if (not con)
        throw myexception()<<"Type family instance '"<<lhs_type.print()<<"' does not begin with a type constructor.";

    return Hs::TypeFamilyInstanceEqn(*con, args, rhs_type.value());
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

Hs::InstanceDecl make_instance_decl(const Located<Hs::Type>& ltype, const optional<Located<Hs::Decls>>& decls)
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

    std::vector<Hs::TypeFamilyInstanceDecl> type_inst_decls;
    Hs::Decls method_decls;
    if (decls)
        for(auto& decl: unloc(*decls))
        {
            if (auto TI = decl.to<Hs::TypeFamilyInstanceDecl>())
                type_inst_decls.push_back(*TI);
            else if (auto V = decl.to<Hs::ValueDecl>())
                method_decls.push_back(*V);
            else
                throw myexception()<<"In declaration of instance "<<unloc(ltype).print()<<", I don't recognize declaration:\n   "<<decl.print();
        }
    return {context, type, type_inst_decls, method_decls};
}

Hs::ClassDecl make_class_decl(const Hs::Context& context, const Hs::Type& header, const optional<Located<Hs::Decls>>& decls)
{
    auto [name, type_args] = check_type_or_class_header(header);

    std::vector<Hs::FixityDecl> fixity_decls;
    std::vector<Hs::TypeFamilyDecl> type_fam_decls;
    std::vector<Hs::TypeFamilyInstanceDecl> default_type_inst_decls;
    std::vector<Hs::SignatureDecl> sig_decls;
    Hs::Decls default_method_decls;

    if (decls)
        for(auto& decl: unloc(*decls))
        {
            if (auto F = decl.to<Hs::FixityDecl>())
                fixity_decls.push_back(*F);
            else if (auto TF = decl.to<Hs::TypeFamilyDecl>())
                type_fam_decls.push_back(*TF);
            else if (auto TI = decl.to<Hs::TypeFamilyInstanceDecl>())
                default_type_inst_decls.push_back(*TI);
            else if (auto S = decl.to<Hs::SignatureDecl>())
                sig_decls.push_back(*S);
            else if (auto V = decl.to<Hs::ValueDecl>())
                default_method_decls.push_back(*V);
            else
                throw myexception()<<"In declaration of class "<<name<<", I don't recognize declaration:\n   "<<decl.print();
        }

    return {context, name, check_all_type_vars(type_args), fixity_decls, type_fam_decls, default_type_inst_decls, sig_decls, default_method_decls};
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
        if (head_name == "*" or head_name == "Type")
            return kind_type();
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

Hs::ConstructorDecl make_constructor(const vector<Hs::TypeVar>& forall, const std::optional<Hs::Context>& c, const Hs::Type& typeish)
{
    // 1. Split into head and arguments
    auto [head,args] = Hs::decompose_type_apps(typeish);

    // 2. Get the constructor name.
    auto tc = head.to<Hs::TypeCon>();
    if (not tc)
        throw myexception()<<"In constructor `"<<typeish<<"`:\n    `"<<head<<"` is not a data constructor!";
    auto name = unloc(tc->name);

    // 3. If we have 1 arg and its a FieldDecls, then make a record constructor.
    if (args.size() == 1)
    {
        if (auto fd = args[0].to<Hs::FieldDecls>())
        {
            return {forall, c, name, *fd};
        }
    }

    // 4. Otherwise make a normal constructor.
    return {forall, c, name, args};
}

