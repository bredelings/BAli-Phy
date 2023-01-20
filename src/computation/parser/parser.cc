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
        value.YY_MOVE_OR_COPY< Located<Hs::InfixExp> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exp: // exp
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

      case symbol_kind::S_sig_vars: // sig_vars
        value.YY_MOVE_OR_COPY< std::vector<Hs::LVar> > (YY_MOVE (that.value));
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
        value.move< Located<Hs::InfixExp> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exp: // exp
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

      case symbol_kind::S_sig_vars: // sig_vars
        value.move< std::vector<Hs::LVar> > (YY_MOVE (that.value));
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
        value.copy< Located<Hs::InfixExp> > (that.value);
        break;

      case symbol_kind::S_exp: // exp
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

      case symbol_kind::S_sig_vars: // sig_vars
        value.copy< std::vector<Hs::LVar> > (that.value);
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
        value.move< Located<Hs::InfixExp> > (that.value);
        break;

      case symbol_kind::S_exp: // exp
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

      case symbol_kind::S_sig_vars: // sig_vars
        value.move< std::vector<Hs::LVar> > (that.value);
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
        yylhs.value.emplace< Located<Hs::InfixExp> > ();
        break;

      case symbol_kind::S_exp: // exp
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

      case symbol_kind::S_sig_vars: // sig_vars
        yylhs.value.emplace< std::vector<Hs::LVar> > ();
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
#line 505 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2436 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 522 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < std::string > (),yystack_[2].value.as < std::optional<std::vector<Hs::Export>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2442 "parser.cc"
    break;

  case 4: // module: body2
#line 523 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{"Main",{},yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2448 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 525 "parser.y"
                                                                 {drv.push_module_context();}
#line 2454 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 533 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2460 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 534 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2466 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 536 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2472 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 537 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2478 "parser.cc"
    break;

  case 13: // top: semis top1
#line 540 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2484 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 542 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2490 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 543 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2496 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 544 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::ImpDecl> > (),{});}
#line 2502 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 552 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::Export>> > () = yystack_[1].value.as < std::vector<Hs::Export> > ();}
#line 2508 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 553 "parser.y"
                                      {}
#line 2514 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 555 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > () = yystack_[0].value.as < std::vector<Hs::Export> > ();}
#line 2520 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 557 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > () = yystack_[2].value.as < std::vector<Hs::Export> > (); yylhs.value.as < std::vector<Hs::Export> > ().push_back(yystack_[0].value.as < Hs::Export > ());}
#line 2526 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 558 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::Export> > ().push_back(yystack_[0].value.as < Hs::Export > ());}
#line 2532 "parser.cc"
    break;

  case 22: // export: qcname export_subspec
#line 560 "parser.y"
                                      {yylhs.value.as < Hs::Export > () = Hs::ExportSymbol{yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}; }
#line 2538 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 561 "parser.y"
                                      {yylhs.value.as < Hs::Export > () = Hs::ExportModule{{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2544 "parser.cc"
    break;

  case 24: // export_subspec: %empty
#line 563 "parser.y"
                                      {}
#line 2550 "parser.cc"
    break;

  case 25: // export_subspec: "(" qcnames ")"
#line 564 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpecSome{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2556 "parser.cc"
    break;

  case 26: // export_subspec: "(" ".." ")"
#line 565 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpecAll(); }
#line 2562 "parser.cc"
    break;

  case 27: // qcnames: %empty
#line 567 "parser.y"
                   {}
#line 2568 "parser.cc"
    break;

  case 28: // qcnames: qcnames1
#line 568 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2574 "parser.cc"
    break;

  case 29: // qcnames1: qcnames1 "," qcname
#line 570 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2580 "parser.cc"
    break;

  case 30: // qcnames1: qcname
#line 571 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2586 "parser.cc"
    break;

  case 31: // qcname: qvar
#line 573 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2592 "parser.cc"
    break;

  case 32: // qcname: oqtycon_no_varcon
#line 574 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2598 "parser.cc"
    break;

  case 37: // importdecls: importdecls_semi importdecl
#line 584 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[1].value.as < std::vector<Hs::ImpDecl> > (), yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[0].value.as < Hs::ImpDecl > ()); }
#line 2604 "parser.cc"
    break;

  case 38: // importdecls_semi: importdecls_semi importdecl semis1
#line 586 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[2].value.as < std::vector<Hs::ImpDecl> > (); yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[1].value.as < Hs::ImpDecl > ()); }
#line 2610 "parser.cc"
    break;

  case 39: // importdecls_semi: %empty
#line 587 "parser.y"
                         { }
#line 2616 "parser.cc"
    break;

  case 40: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 589 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::ImpDecl > () = Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < std::string > (),yystack_[1].value.as < std::optional<std::string> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ());
}
#line 2624 "parser.cc"
    break;

  case 41: // optqualified: "qualified"
#line 602 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2630 "parser.cc"
    break;

  case 42: // optqualified: %empty
#line 603 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2636 "parser.cc"
    break;

  case 43: // maybeas: "as" modid
#line 605 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 2642 "parser.cc"
    break;

  case 44: // maybeas: %empty
#line 606 "parser.y"
                               { }
#line 2648 "parser.cc"
    break;

  case 45: // maybeimpspec: impspec
#line 608 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2654 "parser.cc"
    break;

  case 46: // maybeimpspec: %empty
#line 609 "parser.y"
                               { }
#line 2660 "parser.cc"
    break;

  case 47: // impspec: "(" exportlist ")"
#line 613 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::Export> > ()}; }
#line 2666 "parser.cc"
    break;

  case 48: // impspec: "hiding" "(" exportlist ")"
#line 614 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::Export> > ()}; }
#line 2672 "parser.cc"
    break;

  case 49: // prec: %empty
#line 619 "parser.y"
                   { }
#line 2678 "parser.cc"
    break;

  case 50: // prec: "INTEGER"
#line 620 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < integer > ().convert_to<int>(); }
#line 2684 "parser.cc"
    break;

  case 51: // infix: "infix"
#line 622 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2690 "parser.cc"
    break;

  case 52: // infix: "infixl"
#line 623 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2696 "parser.cc"
    break;

  case 53: // infix: "infixr"
#line 624 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2702 "parser.cc"
    break;

  case 54: // ops: ops "," op
#line 626 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 2708 "parser.cc"
    break;

  case 55: // ops: op
#line 627 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 2714 "parser.cc"
    break;

  case 56: // topdecls: topdecls_semi topdecl
#line 631 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 2720 "parser.cc"
    break;

  case 57: // topdecls_semi: topdecls_semi topdecl semis1
#line 633 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2726 "parser.cc"
    break;

  case 58: // topdecls_semi: %empty
#line 634 "parser.y"
                                            { }
#line 2732 "parser.cc"
    break;

  case 59: // topdecl: cl_decl
#line 636 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2738 "parser.cc"
    break;

  case 60: // topdecl: ty_decl
#line 637 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2744 "parser.cc"
    break;

  case 61: // topdecl: standalone_kind_sig
#line 638 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2750 "parser.cc"
    break;

  case 62: // topdecl: inst_decl
#line 639 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2756 "parser.cc"
    break;

  case 63: // topdecl: "default" "(" comma_types0 ")"
#line 642 "parser.y"
                                               {yylhs.value.as < expression_ref > () = Hs::DefaultDecl(yystack_[1].value.as < std::vector<Hs::Type> > ()); }
#line 2762 "parser.cc"
    break;

  case 64: // topdecl: "foreign" "import" "bpcall" "STRING" var "::" sigtypedoc
#line 643 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = Hs::ForeignDecl(yystack_[3].value.as < std::string > (), yystack_[2].value.as < std::string > (), yystack_[0].value.as < Hs::Type > ());}
#line 2768 "parser.cc"
    break;

  case 65: // topdecl: decl_no_th
#line 650 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2774 "parser.cc"
    break;

  case 66: // topdecl: infixexp
#line 652 "parser.y"
                                               {yylhs.value.as < expression_ref > () = unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ());}
#line 2780 "parser.cc"
    break;

  case 67: // cl_decl: "class" tycl_hdr where_cls
#line 654 "parser.y"
                                              {yylhs.value.as < expression_ref > () = make_class_decl(yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().first,yystack_[1].value.as < std::pair<Hs::Context,Hs::Type> > ().second,yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ());}
#line 2786 "parser.cc"
    break;

  case 68: // ty_decl: "type" type "=" ktype
#line 656 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_type_synonym({yystack_[2].location, yystack_[2].value.as < Hs::Type > ()},{yystack_[0].location, yystack_[0].value.as < Hs::Type > ()});}
#line 2792 "parser.cc"
    break;

  case 69: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 657 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::Type> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::Type> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > ());}
#line 2798 "parser.cc"
    break;

  case 70: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 659 "parser.y"
                                                                                          {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::Type> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::Type> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < std::optional<Hs::GADTConstructorsDecl> > ());}
#line 2804 "parser.cc"
    break;

  case 71: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 660 "parser.y"
                                                                                          {yylhs.value.as < expression_ref > () = make_type_family({yystack_[3].location,yystack_[3].value.as < Hs::Type > ()}, yystack_[2].value.as < std::optional<Located<Hs::Kind>> > (), yystack_[0].value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ());}
#line 2810 "parser.cc"
    break;

  case 72: // standalone_kind_sig: "type" sks_vars "::" kind
#line 663 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = Hs::KindSigDecl(yystack_[2].value.as < std::vector<Hs::TypeCon> > (),yystack_[0].value.as < expression_ref > ());}
#line 2816 "parser.cc"
    break;

  case 73: // sks_vars: sks_vars "," oqtycon
#line 665 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::TypeCon> > () = yystack_[2].value.as < std::vector<Hs::TypeCon> > (); yylhs.value.as < std::vector<Hs::TypeCon> > ().push_back(Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()})); }
#line 2822 "parser.cc"
    break;

  case 74: // sks_vars: oqtycon
#line 666 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::TypeCon> > () = {Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()})}; }
#line 2828 "parser.cc"
    break;

  case 75: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 669 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_instance_decl({yystack_[1].location,yystack_[1].value.as < Hs::Type > ()},yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ());}
#line 2834 "parser.cc"
    break;

  case 76: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 670 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 2840 "parser.cc"
    break;

  case 91: // where_type_family: %empty
#line 705 "parser.y"
                                                           {}
#line 2846 "parser.cc"
    break;

  case 92: // where_type_family: "where" ty_fam_inst_eqn_list
#line 706 "parser.y"
                                                           {yylhs.value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > () = yystack_[0].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2852 "parser.cc"
    break;

  case 93: // ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
#line 708 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2858 "parser.cc"
    break;

  case 94: // ty_fam_inst_eqn_list: "vocurly" ty_fam_inst_eqns close
#line 709 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2864 "parser.cc"
    break;

  case 95: // ty_fam_inst_eqn_list: "{" ".." "}"
#line 710 "parser.y"
                                                           {}
#line 2870 "parser.cc"
    break;

  case 96: // ty_fam_inst_eqn_list: "vocurly" ".." close
#line 711 "parser.y"
                                                           {}
#line 2876 "parser.cc"
    break;

  case 97: // ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
#line 713 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[2].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > (); yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ().push_back(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 2882 "parser.cc"
    break;

  case 98: // ty_fam_inst_eqns: ty_fam_inst_eqns ";"
#line 714 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2888 "parser.cc"
    break;

  case 99: // ty_fam_inst_eqns: ty_fam_inst_eqn
#line 715 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = {yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ()};}
#line 2894 "parser.cc"
    break;

  case 100: // ty_fam_inst_eqns: %empty
#line 716 "parser.y"
                                                           {}
#line 2900 "parser.cc"
    break;

  case 101: // ty_fam_inst_eqn: type "=" ctype
#line 718 "parser.y"
                                                           {yylhs.value.as < Hs::TypeFamilyInstanceEqn > () = make_type_family_instance_eqn({yystack_[2].location,yystack_[2].value.as < Hs::Type > ()},{yystack_[0].location,yystack_[0].value.as < Hs::Type > ()});}
#line 2906 "parser.cc"
    break;

  case 102: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 721 "parser.y"
                                                               {}
#line 2912 "parser.cc"
    break;

  case 103: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 723 "parser.y"
                                                               { yylhs.value.as < expression_ref > () = make_type_family({yystack_[1].location,yystack_[1].value.as < Hs::Type > ()}, yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {}); }
#line 2918 "parser.cc"
    break;

  case 104: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 725 "parser.y"
                                                               { yylhs.value.as < expression_ref > () = make_type_family({yystack_[1].location,yystack_[1].value.as < Hs::Type > ()}, yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {}); }
#line 2924 "parser.cc"
    break;

  case 105: // at_decl_cls: "type" ty_fam_inst_eqn
#line 727 "parser.y"
                                                               { yylhs.value.as < expression_ref > () = Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());    }
#line 2930 "parser.cc"
    break;

  case 106: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 728 "parser.y"
                                                               { yylhs.value.as < expression_ref > () = Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());    }
#line 2936 "parser.cc"
    break;

  case 111: // at_decl_inst: "type" opt_instance ty_fam_inst_eqn
#line 736 "parser.y"
                                                              { yylhs.value.as < expression_ref > () = Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());    }
#line 2942 "parser.cc"
    break;

  case 112: // data_or_newtype: "data"
#line 738 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 2948 "parser.cc"
    break;

  case 113: // data_or_newtype: "newtype"
#line 739 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 2954 "parser.cc"
    break;

  case 114: // opt_kind_sig: %empty
#line 743 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 2960 "parser.cc"
    break;

  case 115: // opt_kind_sig: "::" kind
#line 744 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < expression_ref > ();}
#line 2966 "parser.cc"
    break;

  case 118: // opt_tyfam_kind_sig: %empty
#line 749 "parser.y"
                                      {}
#line 2972 "parser.cc"
    break;

  case 119: // opt_tyfam_kind_sig: "::" kind
#line 750 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < expression_ref > ()}};}
#line 2978 "parser.cc"
    break;

  case 120: // opt_tyfam_kind_sig: "=" tv_bndr
#line 751 "parser.y"
                                      {}
#line 2984 "parser.cc"
    break;

  case 121: // opt_at_kind_inj_sig: %empty
#line 753 "parser.y"
                                      {}
#line 2990 "parser.cc"
    break;

  case 122: // opt_at_kind_inj_sig: "::" kind
#line 754 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < expression_ref > ()}};}
#line 2996 "parser.cc"
    break;

  case 123: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 755 "parser.y"
                                                                  {}
#line 3002 "parser.cc"
    break;

  case 124: // tycl_hdr: context "=>" type
#line 759 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::Type> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::Type > ()};}
#line 3008 "parser.cc"
    break;

  case 125: // tycl_hdr: type
#line 760 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::Type> > () = {{},yystack_[0].value.as < Hs::Type > ()};}
#line 3014 "parser.cc"
    break;

  case 129: // decl_cls: at_decl_cls
#line 806 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3020 "parser.cc"
    break;

  case 130: // decl_cls: decl
#line 807 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3026 "parser.cc"
    break;

  case 131: // decls_cls: decls_cls ";" decl_cls
#line 809 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3032 "parser.cc"
    break;

  case 132: // decls_cls: decls_cls ";"
#line 810 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3038 "parser.cc"
    break;

  case 133: // decls_cls: decl_cls
#line 811 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3044 "parser.cc"
    break;

  case 134: // decls_cls: %empty
#line 812 "parser.y"
                                           {}
#line 3050 "parser.cc"
    break;

  case 135: // decllist_cls: "{" decls_cls "}"
#line 814 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3056 "parser.cc"
    break;

  case 136: // decllist_cls: "vocurly" decls_cls close
#line 815 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3062 "parser.cc"
    break;

  case 137: // where_cls: "where" decllist_cls
#line 817 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3068 "parser.cc"
    break;

  case 138: // where_cls: %empty
#line 818 "parser.y"
                                           {}
#line 3074 "parser.cc"
    break;

  case 139: // decl_inst: at_decl_inst
#line 820 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3080 "parser.cc"
    break;

  case 140: // decl_inst: decl
#line 821 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3086 "parser.cc"
    break;

  case 141: // decls_inst: decls_inst ";" decl_inst
#line 823 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3092 "parser.cc"
    break;

  case 142: // decls_inst: decls_inst ";"
#line 824 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3098 "parser.cc"
    break;

  case 143: // decls_inst: decl_inst
#line 825 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3104 "parser.cc"
    break;

  case 144: // decls_inst: %empty
#line 826 "parser.y"
                                           {}
#line 3110 "parser.cc"
    break;

  case 145: // decllist_inst: "{" decls_inst "}"
#line 828 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3116 "parser.cc"
    break;

  case 146: // decllist_inst: "vocurly" decls_inst close
#line 829 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3122 "parser.cc"
    break;

  case 147: // where_inst: "where" decllist_inst
#line 831 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3128 "parser.cc"
    break;

  case 148: // where_inst: %empty
#line 832 "parser.y"
                                           {}
#line 3134 "parser.cc"
    break;

  case 149: // decls: decls ";" decl
#line 835 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3140 "parser.cc"
    break;

  case 150: // decls: decls ";"
#line 836 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3146 "parser.cc"
    break;

  case 151: // decls: decl
#line 837 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3152 "parser.cc"
    break;

  case 152: // decls: %empty
#line 838 "parser.y"
                        {}
#line 3158 "parser.cc"
    break;

  case 153: // decllist: "{" decls "}"
#line 840 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3164 "parser.cc"
    break;

  case 154: // decllist: "vocurly" decls close
#line 841 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3170 "parser.cc"
    break;

  case 155: // binds: decllist
#line 843 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 3176 "parser.cc"
    break;

  case 156: // wherebinds: "where" binds
#line 845 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 3182 "parser.cc"
    break;

  case 157: // wherebinds: %empty
#line 846 "parser.y"
                                 {}
#line 3188 "parser.cc"
    break;

  case 163: // opt_tyconsig: %empty
#line 872 "parser.y"
                                 {}
#line 3194 "parser.cc"
    break;

  case 164: // opt_tyconsig: "::" gtycon
#line 873 "parser.y"
                                 {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3200 "parser.cc"
    break;

  case 165: // sigtype: ctype
#line 882 "parser.y"
         { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3206 "parser.cc"
    break;

  case 166: // sigtypedoc: ctypedoc
#line 884 "parser.y"
            { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3212 "parser.cc"
    break;

  case 167: // sig_vars: sig_vars "," var
#line 886 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > () = yystack_[2].value.as < std::vector<Hs::LVar> > (); yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3218 "parser.cc"
    break;

  case 168: // sig_vars: var
#line 887 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3224 "parser.cc"
    break;

  case 169: // sigtypes1: sigtype
#line 889 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3230 "parser.cc"
    break;

  case 170: // sigtypes1: sigtypes1 "," sigtype
#line 890 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[2].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3236 "parser.cc"
    break;

  case 171: // strict_mark: strictness
#line 894 "parser.y"
             { yylhs.value.as < Hs::StrictLazy > () = yystack_[0].value.as < Hs::StrictLazy > (); }
#line 3242 "parser.cc"
    break;

  case 172: // strictness: PREFIX_BANG
#line 900 "parser.y"
                         {yylhs.value.as < Hs::StrictLazy > () = Hs::StrictLazy::strict;}
#line 3248 "parser.cc"
    break;

  case 173: // strictness: PREFIX_TILDE
#line 901 "parser.y"
                         {yylhs.value.as < Hs::StrictLazy > () = Hs::StrictLazy::lazy;}
#line 3254 "parser.cc"
    break;

  case 174: // ktype: ctype
#line 908 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 3260 "parser.cc"
    break;

  case 175: // ktype: ctype "::" kind
#line 909 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeOfKind(yystack_[2].value.as < Hs::Type > (), yystack_[0].value.as < expression_ref > ());}
#line 3266 "parser.cc"
    break;

  case 176: // ctype: "forall" tv_bndrs "." ctype
#line 911 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::ForallType(yystack_[2].value.as < std::vector<Hs::TypeVar> > (), yystack_[0].value.as < Hs::Type > ());}
#line 3272 "parser.cc"
    break;

  case 177: // ctype: context "=>" ctype
#line 912 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::Type > ());}
#line 3278 "parser.cc"
    break;

  case 178: // ctype: type
#line 914 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3284 "parser.cc"
    break;

  case 179: // ctypedoc: ctype
#line 916 "parser.y"
          { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3290 "parser.cc"
    break;

  case 180: // context: btype
#line 925 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::Type > ());}
#line 3296 "parser.cc"
    break;

  case 181: // context_no_ops: btype_no_ops
#line 927 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ()));}
#line 3302 "parser.cc"
    break;

  case 182: // type: btype
#line 929 "parser.y"
      { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3308 "parser.cc"
    break;

  case 183: // type: btype "->" ctype
#line 930 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,"->"}),yystack_[2].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()});}
#line 3314 "parser.cc"
    break;

  case 184: // typedoc: type
#line 932 "parser.y"
         { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3320 "parser.cc"
    break;

  case 185: // btype: infixtype
#line 935 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3326 "parser.cc"
    break;

  case 186: // infixtype: ftype
#line 937 "parser.y"
           { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3332 "parser.cc"
    break;

  case 187: // infixtype: btype "~" btype
#line 938 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,"~"}),yystack_[2].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()});}
#line 3338 "parser.cc"
    break;

  case 188: // btype_no_ops: atype_docs
#line 940 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3344 "parser.cc"
    break;

  case 189: // btype_no_ops: btype_no_ops atype_docs
#line 941 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[1].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3350 "parser.cc"
    break;

  case 190: // ftype: atype
#line 943 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3356 "parser.cc"
    break;

  case 191: // ftype: tyop
#line 944 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3362 "parser.cc"
    break;

  case 192: // ftype: ftype tyarg
#line 945 "parser.y"
                                   { yylhs.value.as < Hs::Type > () = Hs::TypeApp(yystack_[1].value.as < Hs::Type > (),yystack_[0].value.as < Hs::Type > ()); }
#line 3368 "parser.cc"
    break;

  case 193: // tyarg: atype
#line 947 "parser.y"
       { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3374 "parser.cc"
    break;

  case 194: // tyop: qtyconop
#line 949 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3380 "parser.cc"
    break;

  case 195: // tyop: tyvarop
#line 950 "parser.y"
                                   {yylhs.value.as < Hs::Type > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3386 "parser.cc"
    break;

  case 196: // atype_docs: atype
#line 957 "parser.y"
            { yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > (); }
#line 3392 "parser.cc"
    break;

  case 197: // atype: ntgtycon
#line 964 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3398 "parser.cc"
    break;

  case 198: // atype: tyvar
#line 965 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3404 "parser.cc"
    break;

  case 199: // atype: "*"
#line 966 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[0].location,"*"});}
#line 3410 "parser.cc"
    break;

  case 200: // atype: strict_mark atype
#line 967 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::StrictLazyType(yystack_[1].value.as < Hs::StrictLazy > (),yystack_[0].value.as < Hs::Type > ());}
#line 3416 "parser.cc"
    break;

  case 201: // atype: "{" fielddecls "}"
#line 968 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ());}
#line 3422 "parser.cc"
    break;

  case 202: // atype: "(" ")"
#line 969 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::TypeCon({yystack_[1].location,"()"});}
#line 3428 "parser.cc"
    break;

  case 203: // atype: "(" comma_types1 "," ktype ")"
#line 970 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::Type> > ();ts.push_back(yystack_[1].value.as < Hs::Type > ());yylhs.value.as < Hs::Type > () = Hs::TupleType(ts);}
#line 3434 "parser.cc"
    break;

  case 204: // atype: "[" ktype "]"
#line 976 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = Hs::ListType{yystack_[1].value.as < Hs::Type > ()}; }
#line 3440 "parser.cc"
    break;

  case 205: // atype: "(" ktype ")"
#line 977 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = yystack_[1].value.as < Hs::Type > ();}
#line 3446 "parser.cc"
    break;

  case 206: // inst_type: sigtype
#line 980 "parser.y"
                                       {yylhs.value.as < Hs::Type > () = yystack_[0].value.as < Hs::Type > ();}
#line 3452 "parser.cc"
    break;

  case 209: // comma_types0: comma_types1
#line 985 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[0].value.as < std::vector<Hs::Type> > ();}
#line 3458 "parser.cc"
    break;

  case 210: // comma_types0: %empty
#line 986 "parser.y"
                                       { /* default construction OK */ }
#line 3464 "parser.cc"
    break;

  case 211: // comma_types1: ktype
#line 988 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3470 "parser.cc"
    break;

  case 212: // comma_types1: comma_types1 "," ktype
#line 989 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::Type> > () = yystack_[2].value.as < std::vector<Hs::Type> > (); yylhs.value.as < std::vector<Hs::Type> > ().push_back(yystack_[0].value.as < Hs::Type > ());}
#line 3476 "parser.cc"
    break;

  case 213: // tv_bndrs: tv_bndrs tv_bndr
#line 996 "parser.y"
                               {yylhs.value.as < std::vector<Hs::TypeVar> > () = yystack_[1].value.as < std::vector<Hs::TypeVar> > (); yylhs.value.as < std::vector<Hs::TypeVar> > ().push_back(yystack_[0].value.as < Hs::TypeVar > ());}
#line 3482 "parser.cc"
    break;

  case 214: // tv_bndrs: %empty
#line 997 "parser.y"
                               { /* default construction OK */}
#line 3488 "parser.cc"
    break;

  case 215: // tv_bndr: tv_bndr_no_braces
#line 999 "parser.y"
                                 {yylhs.value.as < Hs::TypeVar > () = yystack_[0].value.as < Hs::TypeVar > ();}
#line 3494 "parser.cc"
    break;

  case 216: // tv_bndr: "{" tyvar "}"
#line 1000 "parser.y"
                                 {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[1].location,yystack_[1].value.as < std::string > ()});}
#line 3500 "parser.cc"
    break;

  case 217: // tv_bndr: "{" tyvar "::" kind "}"
#line 1001 "parser.y"
                                 {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[3].location,yystack_[3].value.as < std::string > ()});}
#line 3506 "parser.cc"
    break;

  case 218: // tv_bndr_no_braces: tyvar
#line 1004 "parser.y"
                                              {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3512 "parser.cc"
    break;

  case 219: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 1005 "parser.y"
                                              {yylhs.value.as < Hs::TypeVar > () = Hs::TypeVar({yystack_[3].location,yystack_[3].value.as < std::string > ()},yystack_[1].value.as < expression_ref > ());}
#line 3518 "parser.cc"
    break;

  case 220: // kind: ctype
#line 1023 "parser.y"
             {yylhs.value.as < expression_ref > () = type_to_kind(yystack_[0].value.as < Hs::Type > ());}
#line 3524 "parser.cc"
    break;

  case 221: // gadt_constrlist: "where" "{" gadt_constrs "}"
#line 1029 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3530 "parser.cc"
    break;

  case 222: // gadt_constrlist: "where" "vocurly" gadt_constrs close
#line 1030 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3536 "parser.cc"
    break;

  case 223: // gadt_constrlist: %empty
#line 1031 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = {};}
#line 3542 "parser.cc"
    break;

  case 224: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1033 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3548 "parser.cc"
    break;

  case 225: // gadt_constrs: gadt_constr
#line 1034 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3554 "parser.cc"
    break;

  case 226: // gadt_constr: optSemi con_list "::" sigtype
#line 1036 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),{{},yystack_[0].value.as < Hs::Type > ()});}
#line 3560 "parser.cc"
    break;

  case 227: // constrs: "=" constrs1
#line 1038 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 3566 "parser.cc"
    break;

  case 228: // constrs1: constrs1 "|" constr
#line 1040 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3572 "parser.cc"
    break;

  case 229: // constrs1: constr
#line 1041 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3578 "parser.cc"
    break;

  case 230: // constr: forall context_no_ops "=>" constr_stuff
#line 1043 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::TypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::Type > ());}
#line 3584 "parser.cc"
    break;

  case 231: // constr: forall constr_stuff
#line 1044 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::TypeVar> > (),{}, yystack_[0].value.as < Hs::Type > ());}
#line 3590 "parser.cc"
    break;

  case 232: // forall: "forall" tv_bndrs "."
#line 1046 "parser.y"
                                {yylhs.value.as < std::vector<Hs::TypeVar> > () = yystack_[1].value.as < std::vector<Hs::TypeVar> > ();}
#line 3596 "parser.cc"
    break;

  case 233: // forall: %empty
#line 1047 "parser.y"
                                {}
#line 3602 "parser.cc"
    break;

  case 234: // constr_stuff: btype_no_ops
#line 1049 "parser.y"
                                                {yylhs.value.as < Hs::Type > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ());}
#line 3608 "parser.cc"
    break;

  case 235: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1050 "parser.y"
                                                {yylhs.value.as < Hs::Type > () = Hs::make_tyapps({Hs::TypeCon({yystack_[1].location,yystack_[1].value.as < std::string > ()}),Hs::make_tyapps(yystack_[2].value.as < std::vector<Hs::Type> > ()),Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::Type> > ())});}
#line 3614 "parser.cc"
    break;

  case 236: // fielddecls: %empty
#line 1052 "parser.y"
                                {}
#line 3620 "parser.cc"
    break;

  case 237: // fielddecls: fielddecls1
#line 1053 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 3626 "parser.cc"
    break;

  case 238: // fielddecls1: fielddecls1 "," fielddecl
#line 1055 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3632 "parser.cc"
    break;

  case 239: // fielddecls1: fielddecl
#line 1056 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3638 "parser.cc"
    break;

  case 240: // fielddecl: sig_vars "::" ctype
#line 1058 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::Type > ());}
#line 3644 "parser.cc"
    break;

  case 251: // decl_no_th: sigdecl
#line 1077 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3650 "parser.cc"
    break;

  case 252: // decl_no_th: PREFIX_BANG aexp rhs
#line 1079 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::StrictValueDecl{yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}; }
#line 3656 "parser.cc"
    break;

  case 253: // decl_no_th: infixexp rhs
#line 1080 "parser.y"
                              {yylhs.value.as < expression_ref > () = Hs::ValueDecl({yystack_[1].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ());}
#line 3662 "parser.cc"
    break;

  case 254: // decl: decl_no_th
#line 1082 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3668 "parser.cc"
    break;

  case 255: // rhs: "=" exp wherebinds
#line 1086 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3674 "parser.cc"
    break;

  case 256: // rhs: gdrhs wherebinds
#line 1087 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 3680 "parser.cc"
    break;

  case 257: // gdrhs: gdrhs gdrh
#line 1089 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3686 "parser.cc"
    break;

  case 258: // gdrhs: gdrh
#line 1090 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3692 "parser.cc"
    break;

  case 259: // gdrh: "|" guardquals "=" exp
#line 1094 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 3698 "parser.cc"
    break;

  case 260: // sigdecl: sig_vars "::" sigtypedoc
#line 1104 "parser.y"
                                  { yylhs.value.as < expression_ref > () = Hs::SignatureDecl{yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::Type > ()}; }
#line 3704 "parser.cc"
    break;

  case 261: // sigdecl: infix prec ops
#line 1105 "parser.y"
                         { yylhs.value.as < expression_ref > () = Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()}; }
#line 3710 "parser.cc"
    break;

  case 262: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1107 "parser.y"
                                                    {}
#line 3716 "parser.cc"
    break;

  case 263: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1108 "parser.y"
                                            {}
#line 3722 "parser.cc"
    break;

  case 264: // sigdecl: "{-# SCC" qvar "#-}"
#line 1109 "parser.y"
                              {}
#line 3728 "parser.cc"
    break;

  case 265: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1110 "parser.y"
                                     {}
#line 3734 "parser.cc"
    break;

  case 266: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1111 "parser.y"
                                                               {}
#line 3740 "parser.cc"
    break;

  case 267: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1112 "parser.y"
                                                                      {}
#line 3746 "parser.cc"
    break;

  case 268: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1113 "parser.y"
                                                     {}
#line 3752 "parser.cc"
    break;

  case 273: // exp: infixexp "::" sigtype
#line 1125 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[2].location+yystack_[0].location, Hs::TypedExp({yystack_[2].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[2].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::Type > ())}; }
#line 3758 "parser.cc"
    break;

  case 274: // exp: infixexp
#line 1126 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}; }
#line 3764 "parser.cc"
    break;

  case 275: // infixexp: exp10
#line 1130 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location,Hs::InfixExp({yystack_[0].value.as < Located<expression_ref> > ()})};}
#line 3770 "parser.cc"
    break;

  case 276: // infixexp: infixexp qop exp10
#line 1131 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < expression_ref > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3776 "parser.cc"
    break;

  case 277: // exp10: "-" fexp
#line 1133 "parser.y"
                               {yylhs.value.as < Located<expression_ref> > () = {yystack_[1].location+yystack_[0].location,Hs::InfixExp( { {yystack_[1].location,Hs::Neg()}, yystack_[0].value.as < Located<expression_ref> > ()} )};}
#line 3782 "parser.cc"
    break;

  case 278: // exp10: fexp
#line 1134 "parser.y"
                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3788 "parser.cc"
    break;

  case 281: // fexp: fexp aexp
#line 1142 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ApplyExp(yystack_[1].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ())};}
#line 3794 "parser.cc"
    break;

  case 282: // fexp: fexp "TYPEAPP" atype
#line 1143 "parser.y"
                                 {}
#line 3800 "parser.cc"
    break;

  case 283: // fexp: "static" aexp
#line 1144 "parser.y"
                                 {}
#line 3806 "parser.cc"
    break;

  case 284: // fexp: aexp
#line 1145 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3812 "parser.cc"
    break;

  case 285: // aexp: qvar "@" aexp
#line 1148 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::AsPattern(Hs::Var(yystack_[2].value.as < std::string > ()),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3818 "parser.cc"
    break;

  case 286: // aexp: PREFIX_TILDE aexp
#line 1149 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LazyPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3824 "parser.cc"
    break;

  case 287: // aexp: "\\" apats1 "->" exp
#line 1150 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LambdaExp(yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3830 "parser.cc"
    break;

  case 288: // aexp: "let" binds "in" exp
#line 1151 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3836 "parser.cc"
    break;

  case 289: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1153 "parser.y"
                                                       {yylhs.value.as < Located<expression_ref> > () = {yystack_[7].location+yystack_[0].location,Hs::IfExp(yystack_[6].value.as < Located<expression_ref> > (),yystack_[3].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3842 "parser.cc"
    break;

  case 290: // aexp: "case" exp "of" altslist
#line 1155 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::CaseExp(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::Alts > ())}; }
#line 3848 "parser.cc"
    break;

  case 291: // aexp: "do" stmtlist
#line 1156 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Do(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 3854 "parser.cc"
    break;

  case 292: // aexp: "mdo" stmtlist
#line 1157 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::MDo(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 3860 "parser.cc"
    break;

  case 293: // aexp: aexp1
#line 1159 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3866 "parser.cc"
    break;

  case 294: // aexp1: aexp1 "{" fbinds "}"
#line 1162 "parser.y"
                              {}
#line 3872 "parser.cc"
    break;

  case 295: // aexp1: aexp2
#line 1163 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3878 "parser.cc"
    break;

  case 296: // aexp2: qvar
#line 1166 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Var(yystack_[0].value.as < std::string > ())};}
#line 3884 "parser.cc"
    break;

  case 297: // aexp2: qcon
#line 1167 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Con(yystack_[0].value.as < std::string > ())};}
#line 3890 "parser.cc"
    break;

  case 298: // aexp2: literal
#line 1168 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[0].value.as < expression_ref > ()};}
#line 3896 "parser.cc"
    break;

  case 299: // aexp2: "(" texp ")"
#line 1169 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, unloc(yystack_[1].value.as < Located<expression_ref> > ())};}
#line 3902 "parser.cc"
    break;

  case 300: // aexp2: "(" tup_exprs ")"
#line 1170 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Tuple(yystack_[1].value.as < std::vector<Located<expression_ref>> > ())};}
#line 3908 "parser.cc"
    break;

  case 301: // aexp2: "[" list "]"
#line 1175 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[1].value.as < expression_ref > ()};}
#line 3914 "parser.cc"
    break;

  case 302: // aexp2: "_"
#line 1176 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::WildcardPattern()};}
#line 3920 "parser.cc"
    break;

  case 303: // texp: exp
#line 1182 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3926 "parser.cc"
    break;

  case 304: // texp: infixexp qop
#line 1183 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LeftSection ( {yystack_[1].value.as < Located<Hs::InfixExp> > ().loc, unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())}, {yystack_[0].location,yystack_[0].value.as < expression_ref > ()} )}; }
#line 3932 "parser.cc"
    break;

  case 305: // texp: qopm infixexp
#line 1184 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RightSection( {yystack_[1].location,yystack_[1].value.as < expression_ref > ()}, {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())} )}; }
#line 3938 "parser.cc"
    break;

  case 306: // tup_exprs: tup_exprs "," texp
#line 1189 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3944 "parser.cc"
    break;

  case 307: // tup_exprs: texp "," texp
#line 1190 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3950 "parser.cc"
    break;

  case 308: // list: texp
#line 1208 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List({yystack_[0].value.as < Located<expression_ref> > ()}); }
#line 3956 "parser.cc"
    break;

  case 309: // list: lexps
#line 1209 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List(yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 3962 "parser.cc"
    break;

  case 310: // list: texp ".."
#line 1210 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 3968 "parser.cc"
    break;

  case 311: // list: texp "," exp ".."
#line 1211 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<expression_ref> > ()); }
#line 3974 "parser.cc"
    break;

  case 312: // list: texp ".." exp
#line 1212 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ()); }
#line 3980 "parser.cc"
    break;

  case 313: // list: texp "," exp ".." exp
#line 1213 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < Located<expression_ref> > (), yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ()); }
#line 3986 "parser.cc"
    break;

  case 314: // list: texp "|" squals
#line 1214 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 3992 "parser.cc"
    break;

  case 315: // lexps: lexps "," texp
#line 1216 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3998 "parser.cc"
    break;

  case 316: // lexps: texp "," texp
#line 1217 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4004 "parser.cc"
    break;

  case 317: // squals: squals "," qual
#line 1230 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4010 "parser.cc"
    break;

  case 318: // squals: qual
#line 1232 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4016 "parser.cc"
    break;

  case 319: // guardquals: guardquals1
#line 1242 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[0].value.as < std::vector<Located<expression_ref>> > ();}
#line 4022 "parser.cc"
    break;

  case 320: // guardquals1: guardquals1 "," qual
#line 1244 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > ();yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4028 "parser.cc"
    break;

  case 321: // guardquals1: qual
#line 1245 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4034 "parser.cc"
    break;

  case 322: // altslist: "{" alts "}"
#line 1248 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4040 "parser.cc"
    break;

  case 323: // altslist: "vocurly" alts close
#line 1249 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4046 "parser.cc"
    break;

  case 324: // altslist: "{" "}"
#line 1250 "parser.y"
                                 {}
#line 4052 "parser.cc"
    break;

  case 325: // altslist: "vocurly" close
#line 1251 "parser.y"
                                 {}
#line 4058 "parser.cc"
    break;

  case 326: // alts: alts1
#line 1253 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4064 "parser.cc"
    break;

  case 327: // alts: ";" alts
#line 1254 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4070 "parser.cc"
    break;

  case 328: // alts1: alts1 ";" alt
#line 1256 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4076 "parser.cc"
    break;

  case 329: // alts1: alts1 ";"
#line 1257 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4082 "parser.cc"
    break;

  case 330: // alts1: alt
#line 1258 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4088 "parser.cc"
    break;

  case 331: // alt: pat alt_rhs
#line 1260 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 4094 "parser.cc"
    break;

  case 332: // alt_rhs: "->" exp wherebinds
#line 1262 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4100 "parser.cc"
    break;

  case 333: // alt_rhs: gdpats wherebinds
#line 1263 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4106 "parser.cc"
    break;

  case 334: // gdpats: gdpats gdpat
#line 1265 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4112 "parser.cc"
    break;

  case 335: // gdpats: gdpat
#line 1266 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4118 "parser.cc"
    break;

  case 336: // gdpat: "|" guardquals "->" exp
#line 1275 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4124 "parser.cc"
    break;

  case 337: // pat: exp
#line 1277 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4130 "parser.cc"
    break;

  case 338: // pat: PREFIX_BANG aexp
#line 1278 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4136 "parser.cc"
    break;

  case 339: // bindpat: exp
#line 1280 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4142 "parser.cc"
    break;

  case 340: // bindpat: PREFIX_BANG aexp
#line 1281 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4148 "parser.cc"
    break;

  case 341: // apat: aexp
#line 1283 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4154 "parser.cc"
    break;

  case 342: // apat: PREFIX_BANG aexp
#line 1284 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4160 "parser.cc"
    break;

  case 343: // apats1: apats1 apat
#line 1286 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4166 "parser.cc"
    break;

  case 344: // apats1: apat
#line 1287 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4172 "parser.cc"
    break;

  case 345: // stmtlist: "{" stmts "}"
#line 1290 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4178 "parser.cc"
    break;

  case 346: // stmtlist: "vocurly" stmts close
#line 1291 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4184 "parser.cc"
    break;

  case 347: // stmts: stmts ";" stmt
#line 1293 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4190 "parser.cc"
    break;

  case 348: // stmts: stmts ";"
#line 1294 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 4196 "parser.cc"
    break;

  case 349: // stmts: stmt
#line 1295 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4202 "parser.cc"
    break;

  case 350: // stmts: %empty
#line 1296 "parser.y"
                       {}
#line 4208 "parser.cc"
    break;

  case 351: // stmt: qual
#line 1301 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4214 "parser.cc"
    break;

  case 352: // stmt: "rec" stmtlist
#line 1302 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ())};}
#line 4220 "parser.cc"
    break;

  case 353: // qual: bindpat "<-" exp
#line 1304 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::PatQual(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4226 "parser.cc"
    break;

  case 354: // qual: exp
#line 1305 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::SimpleQual(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4232 "parser.cc"
    break;

  case 355: // qual: "let" binds
#line 1306 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ())};}
#line 4238 "parser.cc"
    break;

  case 363: // qcon: gen_qcon
#line 1351 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4244 "parser.cc"
    break;

  case 364: // qcon: sysdcon
#line 1352 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4250 "parser.cc"
    break;

  case 365: // gen_qcon: qconid
#line 1354 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4256 "parser.cc"
    break;

  case 366: // gen_qcon: "(" qconsym ")"
#line 1355 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4262 "parser.cc"
    break;

  case 367: // con: conid
#line 1357 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4268 "parser.cc"
    break;

  case 368: // con: "(" consym ")"
#line 1358 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4274 "parser.cc"
    break;

  case 369: // con: sysdcon
#line 1359 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4280 "parser.cc"
    break;

  case 370: // con_list: con_list "," con
#line 1361 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4286 "parser.cc"
    break;

  case 371: // con_list: con
#line 1362 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4292 "parser.cc"
    break;

  case 372: // sysdcon_no_list: "(" ")"
#line 1364 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 4298 "parser.cc"
    break;

  case 373: // sysdcon_no_list: "(" commas ")"
#line 1365 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4304 "parser.cc"
    break;

  case 374: // sysdcon_no_list: "(#" "#)"
#line 1366 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 4310 "parser.cc"
    break;

  case 375: // sysdcon_no_list: "(#" commas "#)"
#line 1367 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4316 "parser.cc"
    break;

  case 376: // sysdcon: sysdcon_no_list
#line 1369 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4322 "parser.cc"
    break;

  case 377: // sysdcon: "[" "]"
#line 1370 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 4328 "parser.cc"
    break;

  case 378: // conop: consym
#line 1372 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4334 "parser.cc"
    break;

  case 379: // conop: "`" conid "`"
#line 1373 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4340 "parser.cc"
    break;

  case 380: // qconop: qconsym
#line 1375 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4346 "parser.cc"
    break;

  case 381: // qconop: "`" qconid "`"
#line 1376 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4352 "parser.cc"
    break;

  case 382: // gtycon: ntgtycon
#line 1379 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4358 "parser.cc"
    break;

  case 383: // gtycon: "(" ")"
#line 1380 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 4364 "parser.cc"
    break;

  case 384: // gtycon: "(#" "#)"
#line 1381 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 4370 "parser.cc"
    break;

  case 385: // ntgtycon: oqtycon
#line 1383 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4376 "parser.cc"
    break;

  case 386: // ntgtycon: "(" commas ")"
#line 1384 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4382 "parser.cc"
    break;

  case 387: // ntgtycon: "(#" commas "#)"
#line 1385 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4388 "parser.cc"
    break;

  case 388: // ntgtycon: "(" "->" ")"
#line 1386 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 4394 "parser.cc"
    break;

  case 389: // ntgtycon: "[" "]"
#line 1387 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 4400 "parser.cc"
    break;

  case 390: // oqtycon: qtycon
#line 1389 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4406 "parser.cc"
    break;

  case 391: // oqtycon: "(" qtyconsym ")"
#line 1390 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4412 "parser.cc"
    break;

  case 392: // oqtycon: "(" "~" ")"
#line 1391 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 4418 "parser.cc"
    break;

  case 393: // oqtycon_no_varcon: qtycon
#line 1393 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4424 "parser.cc"
    break;

  case 394: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1394 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4430 "parser.cc"
    break;

  case 395: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1395 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4436 "parser.cc"
    break;

  case 396: // oqtycon_no_varcon: "(" ":" ")"
#line 1396 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 4442 "parser.cc"
    break;

  case 397: // oqtycon_no_varcon: "(" "~" ")"
#line 1397 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 4448 "parser.cc"
    break;

  case 398: // qtyconop: qtyconsym
#line 1400 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4454 "parser.cc"
    break;

  case 399: // qtyconop: "`" qtycon "`"
#line 1401 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4460 "parser.cc"
    break;

  case 400: // qtycondoc: qtycon
#line 1403 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4466 "parser.cc"
    break;

  case 401: // qtycon: "QCONID"
#line 1405 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4472 "parser.cc"
    break;

  case 402: // qtycon: tycon
#line 1406 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4478 "parser.cc"
    break;

  case 403: // tycon: "CONID"
#line 1410 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4484 "parser.cc"
    break;

  case 404: // qtyconsym: "QCONSYM"
#line 1412 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4490 "parser.cc"
    break;

  case 405: // qtyconsym: "QVARSYM"
#line 1413 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4496 "parser.cc"
    break;

  case 406: // qtyconsym: tyconsym
#line 1414 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4502 "parser.cc"
    break;

  case 407: // tyconsym: "CONSYM"
#line 1416 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4508 "parser.cc"
    break;

  case 408: // tyconsym: "VARSYM"
#line 1417 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4514 "parser.cc"
    break;

  case 409: // tyconsym: ":"
#line 1418 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4520 "parser.cc"
    break;

  case 410: // tyconsym: "-"
#line 1419 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 4526 "parser.cc"
    break;

  case 411: // op: varop
#line 1424 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4532 "parser.cc"
    break;

  case 412: // op: conop
#line 1425 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4538 "parser.cc"
    break;

  case 413: // varop: varsym
#line 1427 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4544 "parser.cc"
    break;

  case 414: // varop: "`" varid "`"
#line 1428 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4550 "parser.cc"
    break;

  case 415: // qop: qvarop
#line 1430 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4556 "parser.cc"
    break;

  case 416: // qop: qconop
#line 1431 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4562 "parser.cc"
    break;

  case 417: // qopm: qvaropm
#line 1434 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4568 "parser.cc"
    break;

  case 418: // qopm: qconop
#line 1435 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4574 "parser.cc"
    break;

  case 419: // qvarop: qvarsym
#line 1440 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4580 "parser.cc"
    break;

  case 420: // qvarop: "`" qvarid "`"
#line 1441 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4586 "parser.cc"
    break;

  case 421: // qvaropm: qvarsym_no_minus
#line 1443 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 4592 "parser.cc"
    break;

  case 422: // qvaropm: "`" qvarid "`"
#line 1444 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4598 "parser.cc"
    break;

  case 423: // tyvar: tyvarid
#line 1448 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4604 "parser.cc"
    break;

  case 424: // tyvarop: "`" tyvarid "`"
#line 1450 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4610 "parser.cc"
    break;

  case 425: // tyvarid: "VARID"
#line 1452 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4616 "parser.cc"
    break;

  case 426: // tyvarid: special_id
#line 1453 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4622 "parser.cc"
    break;

  case 427: // tyvarid: "unsafe"
#line 1454 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 4628 "parser.cc"
    break;

  case 428: // tyvarid: "safe"
#line 1455 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 4634 "parser.cc"
    break;

  case 429: // tyvarid: "interruptible"
#line 1456 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 4640 "parser.cc"
    break;

  case 430: // var: varid
#line 1459 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4646 "parser.cc"
    break;

  case 431: // var: "(" varsym ")"
#line 1460 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4652 "parser.cc"
    break;

  case 432: // qvar: qvarid
#line 1462 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4658 "parser.cc"
    break;

  case 433: // qvar: "(" varsym ")"
#line 1463 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4664 "parser.cc"
    break;

  case 434: // qvar: "(" qvarsym1 ")"
#line 1464 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4670 "parser.cc"
    break;

  case 435: // qvarid: varid
#line 1466 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4676 "parser.cc"
    break;

  case 436: // qvarid: "QVARID"
#line 1467 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4682 "parser.cc"
    break;

  case 437: // varid: "VARID"
#line 1469 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4688 "parser.cc"
    break;

  case 438: // varid: special_id
#line 1470 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4694 "parser.cc"
    break;

  case 439: // varid: "unsafe"
#line 1471 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 4700 "parser.cc"
    break;

  case 440: // varid: "safe"
#line 1472 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 4706 "parser.cc"
    break;

  case 441: // varid: "interruptible"
#line 1473 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 4712 "parser.cc"
    break;

  case 442: // varid: "forall"
#line 1474 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 4718 "parser.cc"
    break;

  case 443: // varid: "family"
#line 1475 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 4724 "parser.cc"
    break;

  case 444: // varid: "role"
#line 1476 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 4730 "parser.cc"
    break;

  case 445: // qvarsym: varsym
#line 1478 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4736 "parser.cc"
    break;

  case 446: // qvarsym: qvarsym1
#line 1479 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4742 "parser.cc"
    break;

  case 447: // qvarsym_no_minus: varsym_no_minus
#line 1481 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4748 "parser.cc"
    break;

  case 448: // qvarsym_no_minus: qvarsym1
#line 1482 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4754 "parser.cc"
    break;

  case 449: // qvarsym1: "QVARSYM"
#line 1484 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4760 "parser.cc"
    break;

  case 450: // varsym: varsym_no_minus
#line 1486 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4766 "parser.cc"
    break;

  case 451: // varsym: "-"
#line 1487 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 4772 "parser.cc"
    break;

  case 452: // varsym_no_minus: "VARSYM"
#line 1489 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4778 "parser.cc"
    break;

  case 453: // varsym_no_minus: special_sym
#line 1490 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4784 "parser.cc"
    break;

  case 454: // special_id: "as"
#line 1492 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 4790 "parser.cc"
    break;

  case 455: // special_id: "qualified"
#line 1493 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 4796 "parser.cc"
    break;

  case 456: // special_id: "hiding"
#line 1494 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 4802 "parser.cc"
    break;

  case 457: // special_id: "export"
#line 1495 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 4808 "parser.cc"
    break;

  case 458: // special_id: "label"
#line 1496 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 4814 "parser.cc"
    break;

  case 459: // special_id: "dynamic"
#line 1497 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 4820 "parser.cc"
    break;

  case 460: // special_id: "stdcall"
#line 1498 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 4826 "parser.cc"
    break;

  case 461: // special_id: "ccall"
#line 1499 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 4832 "parser.cc"
    break;

  case 462: // special_id: "capi"
#line 1500 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 4838 "parser.cc"
    break;

  case 463: // special_id: "prim"
#line 1501 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 4844 "parser.cc"
    break;

  case 464: // special_id: "javascript"
#line 1502 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 4850 "parser.cc"
    break;

  case 465: // special_id: "group"
#line 1503 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 4856 "parser.cc"
    break;

  case 466: // special_id: "stock"
#line 1504 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 4862 "parser.cc"
    break;

  case 467: // special_id: "anyclass"
#line 1505 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 4868 "parser.cc"
    break;

  case 468: // special_id: "via"
#line 1506 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 4874 "parser.cc"
    break;

  case 469: // special_id: "unit"
#line 1507 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 4880 "parser.cc"
    break;

  case 470: // special_id: "dependency"
#line 1508 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 4886 "parser.cc"
    break;

  case 471: // special_id: "signature"
#line 1509 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 4892 "parser.cc"
    break;

  case 472: // special_sym: "!"
#line 1511 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 4898 "parser.cc"
    break;

  case 473: // special_sym: "."
#line 1512 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 4904 "parser.cc"
    break;

  case 474: // special_sym: "*"
#line 1513 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 4910 "parser.cc"
    break;

  case 475: // qconid: conid
#line 1517 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4916 "parser.cc"
    break;

  case 476: // qconid: "QCONID"
#line 1518 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4922 "parser.cc"
    break;

  case 477: // conid: "CONID"
#line 1520 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4928 "parser.cc"
    break;

  case 478: // qconsym: consym
#line 1522 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4934 "parser.cc"
    break;

  case 479: // qconsym: "QCONSYM"
#line 1523 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4940 "parser.cc"
    break;

  case 480: // consym: "CONSYM"
#line 1525 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4946 "parser.cc"
    break;

  case 481: // consym: ":"
#line 1526 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4952 "parser.cc"
    break;

  case 482: // literal: "CHAR"
#line 1530 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 4958 "parser.cc"
    break;

  case 483: // literal: "STRING"
#line 1531 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 4964 "parser.cc"
    break;

  case 484: // literal: "INTEGER"
#line 1532 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < integer > ()});}
#line 4970 "parser.cc"
    break;

  case 485: // literal: "RATIONAL"
#line 1533 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Double{yystack_[0].value.as < double > ()});}
#line 4976 "parser.cc"
    break;

  case 486: // literal: "PRIMINTEGER"
#line 1534 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < integer > ()});}
#line 4982 "parser.cc"
    break;

  case 488: // close: error
#line 1542 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 4988 "parser.cc"
    break;

  case 489: // modid: "CONID"
#line 1546 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4994 "parser.cc"
    break;

  case 490: // modid: "QCONID"
#line 1547 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5000 "parser.cc"
    break;

  case 491: // commas: commas ","
#line 1549 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 5006 "parser.cc"
    break;

  case 492: // commas: ","
#line 1550 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 5012 "parser.cc"
    break;


#line 5016 "parser.cc"

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


  const short parser::yypact_ninf_ = -619;

  const short parser::yytable_ninf_ = -451;

  const short
  parser::yypact_[] =
  {
      32,   -15,  -619,    77,  -619,  -619,  -619,  -619,  -619,   287,
      50,   -22,  -619,    46,   -46,   -46,    22,  -619,  -619,  -619,
    -619,   163,  -619,  -619,  -619,    13,  -619,    95,   107,  4593,
     206,   225,   148,  -619,   845,  -619,   -28,  -619,  -619,  -619,
    -619,   -15,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,
    -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,
    -619,  -619,  -619,  -619,   284,  -619,  -619,  -619,  -619,   162,
     171,  -619,   185,  -619,  -619,  -619,  -619,  -619,  -619,  -619,
     -31,  -619,   -15,  -619,   186,  -619,  2585,  4184,  -619,   203,
     230,  2585,  -619,  -619,  -619,   362,   296,  -619,  3359,   324,
     230,  3250,   240,  4863,    84,  2851,  3250,  2984,  3250,  1654,
    1521,   255,  -619,  -619,  -619,  -619,  -619,  -619,  -619,    24,
     240,   228,   148,  -619,  -619,  -619,  -619,   248,     2,  -619,
    -619,   431,  -619,  3117,  -619,   279,  -619,  -619,  -619,  -619,
    -619,  -619,   298,     7,  -619,  -619,  -619,  -619,   263,  -619,
     288,   307,  -619,  -619,  -619,  -619,  -619,   328,  -619,   331,
     337,   342,  -619,  -619,  -619,  4593,  4630,  -619,  -619,  -619,
    -619,   438,  -619,  1521,   441,   688,  -619,  -619,  -619,  -619,
    -619,  -619,  -619,  -619,  4959,  3462,   707,   350,  4789,  -619,
    -619,  -619,  -619,  -619,   440,  4491,  -619,   378,  -619,   268,
    -619,  4491,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,
    -619,  -619,  -619,  -619,  3669,  2053,  2053,  -619,   354,   393,
     395,   397,   398,  3669,  1255,  1255,  -619,   464,  4184,  4184,
      78,   399,   146,   127,   442,  -619,  -619,   -25,  4863,  -619,
     241,   -33,   375,    -6,  -619,   129,  -619,  -619,  3250,  -619,
    -619,  2718,  -619,  3117,   280,  -619,  -619,  4728,  -619,  -619,
    -619,   688,   160,   376,   368,  -619,  2585,  -619,  -619,  -619,
    -619,  -619,  -619,  2984,  -619,  -619,   176,   231,   337,   377,
     380,   384,   232,  -619,   258,  3669,  4863,  4863,  -619,   131,
     186,   355,  4184,  3669,  4959,  2585,  2319,  4728,  -619,    18,
    -619,  -619,  2585,  -619,  -619,  -619,  -619,  4491,  -619,  4826,
    3250,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,   386,
     388,   372,  -619,   403,    46,   -15,    29,   326,  3669,    41,
     135,   406,   383,  -619,  -619,  -619,  -619,   394,   429,   421,
    -619,   417,   419,  -619,   422,   415,   423,   239,   270,   418,
     420,   329,  -619,  -619,  4184,  3669,  4184,  -619,  -619,  -619,
     424,   425,   296,   230,  3250,   451,   455,    76,  -619,  -619,
      56,  -619,   517,  -619,  -619,  -619,  -619,  -619,  -619,   516,
     119,  -619,  -619,   431,    59,  2585,  -619,   466,   122,  3669,
     137,  3669,   430,   433,   444,   478,  -619,   484,   463,   155,
      84,   488,  -619,  2585,  -619,  -619,   461,   465,  2585,  2585,
    2319,  1787,  -619,  1787,   631,  -619,  1787,  -619,  1787,   138,
    -619,  -619,  -619,  -619,   506,   504,   510,  4922,   470,  -619,
    -619,  -619,  -619,  -619,    -8,   323,  -619,  -619,  -619,  -619,
     565,   512,   479,  -619,   480,   296,  -619,  -619,  -619,  -619,
    -619,   494,  -619,   482,   520,  -619,  -619,  -619,  4691,  -619,
    -619,  -619,   493,  4593,  -619,  -619,  1920,  1388,  -619,  -619,
     495,  3669,  -619,  4959,  4996,  -619,  3669,  3669,  -619,  -619,
    -619,  3669,  -619,  -619,  -619,  -619,  -619,   989,   989,  -619,
    -619,  -619,   514,  -619,  3669,   464,  -619,  -619,  2585,  -619,
    2053,  -619,  2585,   345,  -619,  -619,  1255,  -619,  -619,  3669,
    3669,  5102,   522,  -619,  -619,   437,  -619,  -619,  4959,   502,
    -619,  -619,  -619,  -619,   505,   837,   276,  -619,  -619,  -619,
    -619,  -619,  -619,  -619,  -619,   496,  -619,   532,  -619,  -619,
    -619,  -619,  -619,  3669,  3669,   498,   503,   131,  -619,   542,
    3669,   598,   600,   633,  -619,  2585,  2319,  -619,  -619,  -619,
    4826,  1787,  -619,  4593,   535,  3250,  -619,  2186,  -619,   547,
     536,  -619,   369,    46,  -619,  -619,  -619,  -619,  3669,  5195,
    5195,  -619,  -619,  -619,  -619,  -619,   540,   617,  3566,  -619,
    -619,   188,  -619,    60,  -619,  -619,  -619,   354,  1122,  1122,
    -619,  -619,  -619,  -619,  -619,  5195,   628,   423,   577,  -619,
    -619,  -619,  2319,  2585,  -619,    -3,     4,  -619,  -619,  -619,
    -619,  -619,  -619,   578,  -619,  4491,   357,   633,   165,  -619,
     633,  -619,  -619,  -619,  -619,  -619,   553,  -619,  -619,  -619,
    -619,  2452,  2319,  2585,  -619,    28,  -619,  -619,  -619,   -12,
     585,  -619,  -619,  4184,  4184,  4184,  -619,   332,  -619,   989,
    -619,   657,   650,  -619,  -619,   196,  -619,    61,  -619,   586,
     358,  -619,  3669,  -619,  -619,  -619,  3669,  -619,  5069,   598,
     581,  4287,  -619,  -619,  -619,   354,   354,  -619,  -619,  -619,
    -619,  3772,   164,   622,  -619,  -619,  -619,  -619,  -619,   590,
     565,  -619,  -619,  3669,  -619,  3669,   601,  -619,   340,  3669,
    3875,  -619,  -619,  2585,  -619,  4184,  -619,  1122,  -619,  5195,
    3978,  4081,  -619,  -619,  -619,  -619,  -619,  4491,   558,  -619,
    4491,   214,  -619,    84,    66,  -619,  -619,   564,   572,  -619,
    4184,  -619,  2585,  -619,   583,   573,  3669,  -619,  5162,  -619,
    -619,   707,   602,   604,  -619,  -619,  -619,  5195,  -619,   587,
     235,  -619,    46,    75,  4389,  -619,  4491,  -619,   354,   139,
    -619,  4184,  -619,  -619,  -619,  -619,  -619,  -619,   585,  5195,
    -619,  -619,  -619,  4184,  -619,  -619,  -619,  3669,  -619,  -619,
    -619,  -619
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    36,     0,     2,    36,     4,   489,   490,     8,
       0,    39,     1,     0,     0,     0,    18,    11,    35,    13,
      16,    58,   488,   487,    12,   162,   158,     0,     0,     0,
       0,    42,    37,    15,    14,   161,     0,     6,     7,   454,
     456,     0,   455,   442,   457,   458,   459,   440,   441,   439,
     443,   444,   460,   461,   462,   463,   464,   465,   466,   467,
     468,   469,   471,   470,     0,   437,   403,   436,   401,     0,
      19,    21,    24,    32,   393,   402,    31,   432,   435,   438,
       0,    41,     0,    34,    38,   302,     0,     0,   112,     0,
       0,     0,    51,    52,    53,    81,     0,   113,     0,     0,
       0,     0,   269,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   477,   476,   482,   483,   484,   485,   486,   269,
     269,    49,    56,    59,    60,    61,    62,   128,     0,    65,
     251,    66,   275,   278,   284,   293,   295,   297,   363,   376,
     364,   168,   296,   435,   365,   475,   298,   159,     0,    23,
       0,     0,   451,   472,   474,   473,   452,     0,   449,     0,
       0,     0,   450,   453,    17,     0,    27,    22,    36,    36,
       3,    44,    33,     0,     0,   274,   428,   429,   427,   409,
     173,   410,   172,   199,   236,     0,     0,     0,     0,   425,
     408,   407,   405,   404,   138,     0,   171,     0,   125,   182,
     185,   186,   191,   190,   197,   385,   194,   390,   398,   406,
     198,   195,   423,   426,   210,   350,   350,   291,   280,     0,
       0,     0,     0,     0,   152,   152,   155,     0,     0,     0,
       0,     0,   182,   385,     0,   292,   283,     0,     0,   270,
       0,     0,     0,     0,   371,   163,   369,   367,     0,   341,
     344,     0,   286,   277,     0,   481,   377,     0,   480,   479,
     303,   274,   308,     0,   309,   418,     0,   417,   421,   448,
     447,   380,   478,   451,   372,   492,     0,     0,   448,     0,
     447,   380,     0,   374,     0,     0,     0,     0,    50,     0,
      57,     0,     0,     0,     0,     0,     0,     0,   253,   157,
     258,   416,     0,   415,   419,   446,   445,     0,   281,   357,
       0,   160,   396,   397,   395,   394,   434,   433,    20,     0,
       0,    28,    30,     0,     0,     0,    46,     0,     0,     0,
       0,     0,   237,   239,   430,   214,   389,     0,   174,     0,
     178,     0,     0,   202,   211,     0,   398,     0,     0,     0,
       0,     0,    67,   200,     0,     0,     0,   192,   193,   211,
       0,   209,     0,     0,     0,   354,     0,     0,   349,   351,
       0,   279,     0,    78,    77,    79,    80,   206,   165,   148,
       0,   254,   151,     0,     0,     0,    76,     0,   118,     0,
       0,     0,     0,     0,     0,     0,   264,     0,     0,     0,
       0,     0,   342,     0,   343,   252,     0,     0,   304,   310,
       0,     0,   301,     0,   305,   299,     0,   300,     0,   433,
     366,   373,   491,   375,     0,     0,     0,     0,   261,   412,
      55,   411,   413,   378,     0,   114,   260,   179,   166,   167,
     157,     0,   319,   321,     0,     0,   256,   257,   276,   282,
     360,     0,   356,   359,   362,   285,    26,    25,     0,     9,
      10,    43,     0,     0,    40,    45,     0,     0,   290,   273,
       0,     0,   201,     0,     0,   204,     0,     0,   388,   392,
     205,     0,   391,   386,   387,   399,   424,   134,   134,   137,
     124,   183,   187,    63,     0,   355,   352,   340,     0,   345,
     348,   346,     0,     0,    75,   153,   150,   154,   288,     0,
       0,     0,    86,   220,    72,     0,    73,    68,     0,     0,
     271,   263,   265,   368,     0,     0,     0,   164,   382,   370,
     262,   287,   422,   381,   312,   314,   318,   303,   316,   315,
     307,   306,   268,     0,     0,     0,     0,     0,   127,     0,
       0,   233,   223,   241,   255,     0,     0,   420,   156,   294,
       0,     0,    29,     0,     0,     0,   324,     0,   337,     0,
     326,   330,     0,     0,   325,   431,   240,   238,     0,     0,
       0,   213,   215,   218,   175,   177,   212,   107,     0,   129,
     133,     0,   130,     0,   212,   353,   347,   280,   144,   144,
     147,   149,   101,   119,   120,     0,    91,     0,     0,   272,
     383,   384,     0,   311,   169,     0,     0,   414,   379,    54,
     126,   115,   214,   227,   229,     0,     0,   241,     0,    69,
     242,   244,   259,   320,   358,   361,     0,    47,   338,   327,
     322,   329,     0,     0,   331,   157,   335,   323,   176,     0,
       0,   203,   108,     0,     0,     0,   105,   121,   135,   132,
     136,     0,   109,   139,   143,     0,   140,     0,    87,     0,
       0,    71,     0,   317,   313,   266,     0,   267,     0,   233,
       0,   234,   188,   196,   231,   280,   280,    70,    84,    82,
      83,     0,     0,   245,   248,   400,   243,    48,   328,     0,
     157,   333,   334,     0,   216,     0,   116,   106,   121,     0,
       0,   103,   131,     0,   110,     0,   145,   142,   146,     0,
     100,   100,    92,    64,   170,   232,   228,     0,     0,   189,
       0,     0,   225,     0,     0,   249,   184,   207,     0,   246,
       0,   247,     0,   332,     0,     0,     0,   102,     0,   104,
     122,     0,     0,   198,   289,   111,   141,    88,    90,     0,
       0,    99,     0,     0,   234,   230,   235,   221,   280,     0,
     222,     0,   250,    85,   336,   217,   219,   117,   198,     0,
      89,    95,    93,    98,    96,    94,   224,     0,   208,   123,
      97,   226
  };

  const short
  parser::yypgoto_[] =
  {
    -619,  -619,  -619,  -619,  -619,  -619,  -619,    31,  -619,  -619,
    -424,  -619,   526,  -619,  -619,  -619,  -151,   571,  -619,  -619,
    -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,
    -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,  -619,
    -619,   -85,  -619,  -619,  -619,   -26,  -209,  -619,  -619,  -619,
    -619,  -619,  -619,  -619,  -619,   -10,   411,  -619,    45,   217,
    -619,  -619,    -5,   108,  -619,  -619,   490,  -619,  -308,  -409,
     698,  -619,  -619,  -316,    48,  -159,   173,  -619,  -619,  -168,
     158,  -619,   -55,  -619,   -84,  -619,   -74,  -619,  -319,  -619,
    -619,  -619,  -598,  -106,   443,   -45,  -619,   513,   111,   218,
    -618,  -447,  -619,    57,   -34,  -619,  -619,    65,  -619,    37,
    -619,  -619,   272,   128,  -619,   136,    73,   735,  -197,   519,
    -619,   471,  -619,    92,  -619,   305,    15,  -260,  -202,   -81,
     -68,  -619,  -619,   -96,  -619,  -619,  -619,  -619,   132,  -619,
    -619,  -416,  -619,   134,  -619,  -619,   133,  -619,  -619,   525,
    -619,   -70,   561,   283,  -291,  -619,   220,  -619,  -619,  -619,
     385,    51,  -619,   -95,  -615,   -75,  -619,   389,   -80,  -619,
    -619,  -619,   -19,  -619,  -179,  -619,   242,  -619,   529,  -619,
    -619,  -619,  -383,  -619,  -180,  -259,    -9,  -186,   -11,  -619,
    -619,   -14,   -58,   -88,   -87,  -619,  -171,  -103,   -52,  -241,
    -619,  -217,    -7,  -107
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   170,     6,    10,    19,    30,
      69,    70,    71,   167,   320,   321,    72,    84,    11,    20,
      21,    32,    82,   326,   464,   465,   289,   121,   428,    33,
      34,   122,   123,   124,   125,   230,   126,   223,   692,   741,
     606,   668,   757,   671,   722,   760,   761,   589,   653,   715,
     663,   127,   552,   747,   512,   711,   194,   292,   590,   591,
     489,   352,   664,   665,   600,   504,   380,   226,   227,   446,
      27,    36,   401,   377,   436,   128,   615,   195,   196,   344,
     513,   438,   339,   680,   340,   737,   199,   200,   681,   201,
     357,   202,   682,   203,   379,   738,   360,   345,   474,   581,
     582,   514,   627,   731,   732,   553,   623,   624,   625,   684,
     331,   332,   333,   629,   630,   631,   693,   381,   592,   298,
     299,   300,   130,   238,   239,   365,   175,   132,   733,   133,
     134,   135,   136,   276,   277,   263,   264,   535,   441,   442,
     468,   569,   570,   571,   644,   645,   646,   572,   366,   250,
     251,   217,   367,   368,   369,   451,   452,   453,   137,   138,
     244,   245,   139,   140,   429,   265,   527,   204,   205,    73,
     206,   694,   207,    75,   208,   209,   430,   431,   302,   266,
     303,   267,   210,   211,   212,   141,   142,    77,    78,   304,
     268,   269,   306,   162,    79,   163,   144,   145,   271,   272,
     146,    24,     9,   282
  };

  const short
  parser::yytable_[] =
  {
     213,   247,   398,   198,   284,   443,   161,   346,   350,   246,
      74,   213,   469,   262,   231,   322,   372,   337,   233,   386,
      76,   270,   280,   143,   232,   330,   253,   382,   382,   584,
     235,   554,   197,   236,   149,   439,    13,   249,   252,   564,
     254,   462,   448,   285,   396,   445,   359,    22,   433,   131,
     160,   573,   279,     1,   495,   445,   301,    22,   281,    25,
      22,    22,    22,   603,   393,   308,   730,    22,   703,   548,
     168,   406,   169,   255,   675,   171,    22,    12,   147,   347,
     348,   677,   293,   729,    26,   280,   407,  -430,   148,   353,
     704,   583,   752,    18,   241,   358,   278,   397,   213,   213,
     301,   213,   296,   621,   274,     7,   394,   460,   213,     8,
     275,   444,   642,   676,   213,   161,   258,   305,   294,   536,
     676,   281,   549,  -430,   261,   261,   407,   213,   583,   237,
     752,    29,   152,     2,   153,   154,   213,   558,   463,   636,
     155,   213,   213,    35,   387,   388,    74,    74,   448,   730,
      23,   639,    17,   501,   232,   232,    76,    76,   389,   278,
      23,   305,   156,    23,    23,    23,   729,   507,   729,   349,
      23,   500,    37,   334,   506,   659,   717,    31,   499,    23,
     402,   768,   161,   249,    38,   308,   301,   688,   261,   242,
     783,   500,   253,   243,   390,   111,   649,   650,   213,   323,
     324,   449,   510,   511,   112,   213,   213,   -74,   198,   399,
     255,   286,   287,   143,   143,   471,   689,   690,  -431,   787,
     213,   505,   152,   517,   153,   154,   160,   614,   614,   395,
     155,   432,   355,    80,   506,   356,   701,   197,   409,   383,
     383,   213,   455,   -74,   410,   400,   515,   305,   427,    81,
     574,   294,   156,   258,  -431,   400,   744,    66,   745,   608,
     524,    68,   750,    83,   525,   633,   526,   213,   213,   213,
     490,   470,   164,   691,   691,    66,   411,   425,   426,    68,
     232,   414,   492,   334,    66,    66,   415,   165,    68,    68,
     658,   743,   416,   496,   166,   583,   497,   247,   716,   777,
     454,   172,   213,   659,   213,   246,   433,   562,   301,   601,
     516,   717,   214,   586,   330,   538,   767,   539,   461,   291,
     540,   673,   541,   270,   546,   270,   594,   753,   270,   768,
     270,   215,   152,   216,   153,   154,   607,   782,   234,   301,
     155,   417,   421,   338,   338,   237,   607,   418,   422,   483,
     783,   443,    14,    15,   355,   422,   647,   356,  -180,   288,
     724,   295,   156,   150,   296,   583,   158,   283,   778,   305,
     423,   275,   338,   151,   422,   152,   660,   153,   154,   656,
     309,   378,   484,   155,   213,   310,   422,   213,   611,   213,
     213,   174,   275,   311,   213,   661,   218,   224,   312,   225,
     305,   666,   666,   550,   551,   156,   157,   213,   764,   158,
     159,   766,   709,   710,   260,   260,   545,   313,   347,   348,
     709,   748,   213,   213,   213,   669,   261,   466,   261,   467,
     487,   261,   488,   261,   219,   220,   221,   222,   314,    74,
     433,   315,   325,   378,    74,   707,   598,   316,   599,    76,
     718,   437,   317,   642,    76,   643,   213,   213,   685,   720,
     686,   721,   334,   213,   327,   635,   275,   351,   354,   371,
     373,   791,   374,   270,   375,   376,   143,   143,   260,   385,
     391,   256,   412,   392,   413,   434,   378,   419,   458,   432,
    -450,   213,   213,   213,   420,   143,   456,   638,   457,   473,
     475,   213,   383,   383,   657,   459,   755,   334,   472,   476,
     255,   477,   295,   491,   232,   296,   179,   770,   213,   683,
     666,   383,   152,   433,   153,   154,   342,   478,   181,   479,
     155,   481,   480,   482,   493,   485,  -339,   486,   213,   758,
     498,   494,   502,   503,    74,   784,   785,   509,   297,   338,
     520,   454,   156,   258,    76,   521,   158,   259,   190,   191,
     518,   522,   192,   193,   519,   530,   213,   213,   213,   706,
     387,   708,   346,   523,   790,   683,   261,   780,   532,   232,
     232,   232,   533,   542,   543,   213,   547,   143,   143,   213,
     544,   213,   445,   555,   213,   556,   559,   557,   560,   669,
     440,   561,   563,   356,   213,   575,   605,   736,   609,   695,
     613,   336,   612,   383,   383,   617,   213,   232,   213,   620,
     618,   683,   213,   213,   683,   546,   622,   626,   213,   576,
     247,   387,   213,   213,   213,   585,   387,   387,   246,   338,
     213,   232,   628,   213,   347,   637,   232,   232,   143,   640,
     651,   641,   338,   213,   652,   670,   773,   672,   683,   213,
     683,   213,   679,   697,   213,   705,   232,   602,   713,   714,
     213,   727,   719,   695,   383,   740,   742,   213,   112,   213,
     771,   746,   772,   776,   213,   775,   779,   736,  -218,   781,
     508,   318,   213,   290,   789,   763,   213,   232,   749,   387,
     213,   378,   378,   435,   712,   593,   143,   667,   531,   232,
     255,    39,   756,    28,   534,   384,   537,   616,   260,    40,
     723,   260,   152,   260,   153,   154,   788,   361,   424,   604,
     155,    42,   383,   678,   786,   335,   648,    44,    45,    46,
     176,   177,   178,   734,   726,   577,    52,    53,   297,    54,
      55,    56,   156,   258,    57,   687,   158,   259,    58,    59,
      60,    61,    62,    63,   765,   739,   696,   255,   328,   129,
     447,   568,   568,   405,   699,   698,   404,   370,   702,   152,
     634,   153,   154,   596,   769,   529,   179,   155,   528,   619,
     408,     0,     0,   341,     0,   180,   342,     0,   181,   182,
       0,   183,     0,   595,     0,   297,     0,   597,   184,   156,
     258,     0,   185,   158,   259,     0,   186,   343,   187,     0,
       0,     0,     0,   275,   188,     0,   189,    66,   190,   191,
     437,    68,   192,   193,   378,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    85,    39,
      86,    87,    88,    89,     0,    90,     0,    40,    91,     0,
     632,    92,    93,    94,    95,    96,   260,    97,   602,    42,
       0,    98,   568,    43,    99,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,   101,    58,    59,    60,    61,
      62,    63,   102,     0,     0,     0,     0,     0,   103,   338,
       0,     0,     0,     0,     0,     0,   179,     0,   674,     0,
       0,   104,     0,   341,     0,     0,   342,   105,   181,     0,
       0,     0,     0,   106,     0,     0,   107,   108,     0,     0,
       0,     0,     0,     0,     0,   378,   568,   610,   700,     0,
     109,     0,     0,   275,   110,     0,   111,     0,   190,   191,
       0,     0,   192,   193,    65,   112,     0,     0,    67,   113,
       0,     0,     0,     0,   114,   115,   116,   117,     0,     0,
     118,     0,     0,     0,   119,   120,     0,     0,     0,     0,
       0,     0,    85,    39,    86,     0,   587,     0,     0,    90,
       0,    40,    91,     0,     0,    92,    93,    94,     0,    96,
       0,     0,     0,    42,     0,   588,     0,    43,   754,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,   101,
      58,    59,    60,    61,    62,    63,   102,   774,     0,     0,
       0,     0,   103,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   104,     0,     0,     0,     0,
       0,   105,     0,     0,     0,     0,     0,   106,     0,     0,
     107,   108,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,     0,     0,     0,   110,     0,
     111,     0,     0,     0,     0,     0,     0,     0,    65,   112,
       0,     0,    67,   113,     0,     0,     0,     0,   114,   115,
     116,   117,     0,     0,   118,    85,    39,    86,   119,   120,
       0,     0,    90,     0,    40,    91,     0,     0,    92,    93,
      94,     0,    96,     0,     0,     0,    42,     0,   662,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,   101,    58,    59,    60,    61,    62,    63,   102,
       0,     0,     0,     0,     0,   103,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   104,     0,
       0,     0,     0,     0,   105,     0,     0,     0,     0,     0,
     106,     0,     0,   107,   108,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   109,     0,     0,
       0,   110,     0,   111,     0,     0,     0,     0,     0,     0,
       0,    65,   112,     0,     0,    67,   113,     0,     0,     0,
       0,   114,   115,   116,   117,     0,     0,   118,    85,    39,
      86,   119,   120,     0,     0,    90,     0,    40,    91,     0,
       0,    92,    93,    94,     0,    96,     0,     0,     0,    42,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,   101,    58,    59,    60,    61,
      62,    63,   102,     0,     0,     0,     0,     0,   103,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   104,     0,     0,     0,     0,     0,   105,     0,     0,
       0,     0,     0,   106,     0,     0,   107,   108,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,     0,     0,     0,   110,     0,   111,     0,     0,     0,
       0,     0,     0,     0,    65,   112,     0,     0,    67,   113,
       0,     0,     0,     0,   114,   115,   116,   117,     0,    22,
     118,    85,    39,    86,   119,   120,     0,     0,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,    42,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,   101,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     105,     0,     0,     0,     0,     0,   106,     0,     0,   107,
     565,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    23,   109,     0,     0,     0,   173,     0,   111,
       0,     0,     0,   567,     0,     0,     0,    65,   112,     0,
       0,    67,   113,     0,     0,     0,     0,   114,   115,   116,
     117,     0,     0,   118,    85,    39,    86,     0,     0,     0,
       0,    90,     0,    40,    91,     0,     0,     0,     0,     0,
       0,    96,     0,     0,     0,    42,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,   101,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     255,     0,     0,   105,     0,     0,     0,     0,     0,   106,
       0,     0,   273,     0,   153,   154,     0,     0,     0,     0,
     155,     0,     0,     0,     0,     0,   109,     0,     0,     0,
     173,   274,   111,     0,     0,     0,     0,   275,   257,     0,
      65,   112,   156,   258,    67,   113,   158,   259,     0,     0,
     114,   115,   116,   117,     0,     0,   118,    85,    39,    86,
       0,     0,     0,     0,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   255,     0,     0,   105,     0,     0,     0,
       0,     0,   106,     0,     0,   107,     0,   153,   154,     0,
       0,     0,     0,   155,     0,     0,     0,     0,     0,   109,
     256,     0,     0,   173,     0,   111,     0,     0,     0,     0,
       0,   257,     0,    65,   112,   156,   258,    67,   113,   158,
     259,     0,     0,   114,   115,   116,   117,     0,     0,   118,
      85,    39,    86,     0,     0,     0,     0,    90,     0,    40,
      91,     0,     0,     0,     0,     0,     0,    96,     0,     0,
       0,    42,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,   101,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   255,     0,     0,   105,
       0,     0,     0,     0,     0,   106,     0,     0,   107,     0,
     153,   154,     0,     0,     0,     0,   155,     0,     0,     0,
       0,     0,   109,     0,     0,     0,   173,     0,   111,     0,
       0,     0,     0,     0,   257,     0,    65,   112,   156,   258,
      67,   113,   158,   259,     0,     0,   114,   115,   116,   117,
       0,     0,   118,    85,    39,    86,     0,     0,     0,     0,
      90,     0,    40,    91,     0,     0,     0,     0,     0,     0,
      96,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
     101,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   105,     0,     0,     0,     0,     0,   106,     0,
       0,   107,   565,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   566,     0,     0,   109,     0,     0,     0,   173,
       0,   111,     0,     0,     0,   567,     0,     0,     0,    65,
     112,     0,     0,    67,   113,     0,     0,     0,     0,   114,
     115,   116,   117,     0,     0,   118,    85,    39,    86,     0,
       0,     0,     0,    90,     0,    40,    91,     0,     0,     0,
       0,     0,     0,   362,     0,     0,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   100,
      50,    51,    52,    53,     0,    54,    55,    56,     0,   363,
      57,     0,     0,   101,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   105,     0,     0,     0,     0,
       0,   106,     0,     0,   107,   364,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   109,     0,
       0,     0,   173,     0,   111,     0,     0,     0,     0,     0,
       0,     0,    65,   112,     0,     0,    67,   113,     0,     0,
       0,     0,   114,   115,   116,   117,     0,     0,   118,    85,
      39,    86,     0,     0,     0,     0,    90,     0,    40,    91,
       0,     0,     0,     0,     0,     0,    96,     0,     0,     0,
      42,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,   101,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   105,     0,
       0,     0,     0,     0,   106,     0,     0,   107,   565,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,     0,     0,     0,   173,     0,   111,     0,     0,
       0,   567,     0,     0,     0,    65,   112,     0,     0,    67,
     113,     0,     0,     0,     0,   114,   115,   116,   117,     0,
       0,   118,    85,    39,    86,     0,     0,     0,     0,    90,
       0,    40,    91,     0,     0,     0,     0,     0,     0,   362,
       0,     0,     0,    42,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,   101,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   105,     0,     0,     0,     0,     0,   106,     0,     0,
     107,   364,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,     0,     0,     0,   173,     0,
     111,     0,     0,     0,     0,     0,     0,     0,    65,   112,
       0,     0,    67,   113,     0,     0,     0,     0,   114,   115,
     116,   117,     0,     0,   118,    85,    39,    86,     0,     0,
       0,     0,    90,     0,    40,    91,     0,     0,     0,     0,
       0,     0,    96,     0,     0,     0,    42,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,   101,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   105,     0,     0,     0,     0,     0,
     106,     0,     0,   107,   565,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   109,     0,     0,
       0,   173,     0,   111,     0,     0,     0,     0,     0,     0,
       0,    65,   112,     0,     0,    67,   113,     0,     0,     0,
       0,   114,   115,   116,   117,     0,     0,   118,    85,    39,
      86,     0,     0,     0,     0,    90,     0,    40,    91,     0,
       0,     0,     0,     0,     0,    96,     0,     0,     0,    42,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,   101,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   105,     0,     0,
       0,     0,     0,   106,     0,     0,   107,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,     0,     0,     0,   173,     0,   111,     0,     0,     0,
       0,     0,     0,     0,    65,   112,     0,     0,    67,   113,
       0,     0,     0,     0,   114,   115,   116,   117,     0,     0,
     118,    85,    39,    86,     0,     0,     0,     0,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,    42,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     105,     0,     0,     0,   403,     0,   106,     0,     0,     0,
     248,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,     0,     0,     0,   173,     0,   111,
       0,     0,     0,     0,     0,     0,     0,    65,   112,     0,
       0,    67,   113,     0,     0,     0,     0,   114,   115,   116,
     117,     0,     0,   118,    85,    39,    86,     0,     0,     0,
       0,    90,     0,    40,    91,     0,     0,     0,     0,     0,
       0,    96,     0,     0,     0,    42,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   105,     0,     0,     0,     0,     0,   106,
       0,     0,     0,   248,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   109,     0,     0,     0,
     173,     0,   111,     0,     0,     0,     0,     0,     0,     0,
      65,   112,     0,     0,    67,   113,     0,     0,     0,     0,
     114,   115,   116,   117,     0,     0,   118,    85,    39,    86,
       0,     0,     0,     0,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   105,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   109,
       0,     0,     0,   173,     0,   111,     0,     0,     0,     0,
       0,     0,     0,    65,   112,     0,     0,    67,   113,     0,
       0,     0,     0,   114,   115,   116,   117,     0,     0,   118,
      85,    39,    86,     0,     0,     0,     0,    90,     0,    40,
      91,     0,     0,     0,     0,     0,     0,    96,     0,     0,
       0,    42,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   105,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   307,     0,     0,
       0,     0,   109,     0,     0,     0,   173,     0,   111,     0,
       0,     0,     0,     0,     0,     0,    65,   112,     0,     0,
      67,   113,     0,     0,     0,     0,   114,   115,   116,   117,
       0,     0,   118,    85,    39,    86,     0,     0,     0,     0,
      90,     0,    40,    91,     0,     0,     0,     0,     0,     0,
      96,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   105,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,     0,     0,     0,   173,
       0,   111,     0,    39,     0,     0,     0,     0,     0,    65,
     112,    40,     0,    67,   113,     0,     0,     0,   228,   114,
     115,   116,   117,    42,     0,   118,     0,     0,     0,    44,
      45,    46,   176,   177,   178,     0,   229,     0,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   179,     0,
       0,     0,     0,     0,     0,     0,     0,   180,     0,     0,
     181,   182,     0,   183,     0,     0,     0,     0,     0,     0,
     184,     0,     0,     0,   185,     0,    39,     0,   186,     0,
     187,     0,     0,     0,    40,     0,   188,     0,   189,    66,
     190,   191,     0,    68,   192,   193,    42,     0,     0,     0,
     335,     0,    44,    45,    46,   176,   177,   178,     0,     0,
       0,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   179,     0,     0,     0,     0,     0,     0,     0,     0,
     180,     0,     0,   181,   182,     0,   183,     0,     0,     0,
       0,     0,     0,   184,     0,     0,     0,   185,   336,     0,
      39,   186,     0,   187,     0,     0,     0,     0,    40,   188,
       0,   189,    66,   190,   191,   654,    68,   192,   193,     0,
      42,     0,     0,     0,     0,     0,    44,    45,    46,   176,
     177,   178,     0,   655,     0,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   179,     0,     0,     0,     0,
       0,     0,     0,     0,   180,     0,     0,   181,   182,     0,
     183,     0,     0,     0,     0,     0,     0,   184,     0,     0,
       0,   185,     0,    39,     0,   186,     0,   187,     0,     0,
       0,    40,     0,   188,     0,   189,    66,   190,   191,     0,
      68,   192,   193,    42,     0,     0,     0,   335,     0,    44,
      45,    46,   176,   177,   178,     0,     0,     0,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   179,     0,
       0,     0,     0,     0,     0,     0,     0,   180,     0,     0,
     181,   182,     0,   183,     0,     0,     0,     0,     0,     0,
     184,     0,     0,     0,   185,     0,    39,     0,   186,     0,
     187,     0,     0,     0,    40,     0,   188,     0,   189,    66,
     190,   191,     0,    68,   192,   193,    42,     0,     0,     0,
       0,     0,    44,    45,    46,   176,   177,   178,     0,     0,
       0,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   179,     0,     0,     0,     0,     0,     0,     0,     0,
     180,     0,     0,   181,   182,     0,   183,     0,     0,     0,
       0,     0,     0,   184,     0,     0,     0,   185,     0,    39,
       0,   186,   735,   187,     0,     0,     0,    40,     0,   188,
       0,   189,    66,   190,   191,     0,    68,   192,   193,    42,
       0,     0,     0,   335,     0,    44,    45,    46,   176,   177,
     178,     0,     0,     0,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   179,     0,     0,     0,     0,     0,
       0,     0,     0,   180,     0,     0,   181,   182,     0,   183,
       0,     0,     0,     0,     0,     0,   184,     0,     0,     0,
     185,     0,    39,     0,   751,     0,   187,     0,     0,     0,
      40,     0,   188,     0,   189,    66,   190,   191,     0,    68,
     192,   193,    42,     0,     0,     0,     0,     0,    44,    45,
      46,   176,   177,   178,     0,     0,     0,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   759,   179,     0,     0,
       0,     0,     0,     0,     0,     0,   180,     0,     0,   181,
     182,     0,   183,     0,     0,     0,     0,     0,     0,   184,
       0,     0,     0,   185,     0,    39,     0,   186,     0,   187,
       0,     0,     0,    40,     0,   188,     0,   189,    66,   190,
     191,     0,    68,   192,   193,    42,     0,     0,     0,     0,
       0,    44,    45,    46,   176,   177,   178,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   762,
     179,     0,     0,     0,     0,     0,     0,     0,     0,   180,
       0,     0,   181,   182,     0,   183,     0,     0,     0,     0,
       0,     0,   184,     0,     0,     0,   185,     0,    39,     0,
     186,     0,   187,     0,     0,     0,    40,     0,   188,     0,
     189,    66,   190,   191,     0,    68,   192,   193,    42,     0,
       0,     0,     0,     0,    44,    45,    46,   176,   177,   178,
       0,     0,     0,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   179,     0,     0,     0,     0,     0,     0,
       0,     0,   180,     0,     0,   181,   182,     0,   183,     0,
       0,     0,     0,     0,     0,   184,     0,     0,     0,   185,
       0,    39,     0,   186,     0,   187,     0,     0,     0,    40,
       0,   188,     0,   189,    66,   190,   191,     0,    68,   192,
     193,    42,     0,     0,     0,     0,     0,    44,    45,    46,
     176,   177,   178,     0,     0,     0,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   255,     0,     0,     0,
       0,     0,     0,     0,     0,   180,     0,  -181,     0,   182,
       0,   183,     0,     0,     0,     0,     0,     0,   184,     0,
       0,     0,   185,    39,     0,     0,   186,     0,   187,     0,
       0,    40,     0,     0,   728,     0,   189,    66,     0,   258,
       0,    68,     0,    42,     0,     0,     0,     0,     0,    44,
      45,    46,   176,   177,   178,     0,     0,     0,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   255,     0,
       0,     0,     0,     0,     0,     0,     0,   180,     0,     0,
       0,   182,     0,   183,     0,     0,     0,     0,     0,     0,
     184,     0,     0,     0,   185,    39,     0,     0,   186,     0,
     187,     0,     0,    40,     0,     0,   728,     0,   189,    66,
       0,   258,     0,    68,     0,    42,     0,     0,     0,     0,
       0,    44,    45,    46,   176,   177,   178,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   180,
       0,     0,     0,   182,     0,   183,     0,     0,     0,     0,
       0,     0,   184,     0,     0,     0,   185,    39,     0,     0,
     186,     0,   187,     0,     0,    40,     0,     0,     0,     0,
     189,    66,     0,     0,    41,    68,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,     0,
      50,    51,    52,    53,    39,    54,    55,    56,     0,     0,
      57,     0,    40,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,     0,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,    39,     0,     0,     0,     0,
       0,     0,    64,    40,     0,     0,     0,     0,   319,     0,
       0,     0,    65,    66,     0,    42,    67,    68,     0,    43,
       0,    44,    45,    46,    47,    48,    49,     0,    50,    51,
      52,    53,    39,    54,    55,    56,     0,     0,    57,    64,
      40,     0,    58,    59,    60,    61,    62,    63,     0,    65,
      66,     0,    42,    67,    68,     0,    43,     0,    44,    45,
      46,    47,    48,    49,     0,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,    39,     0,     0,     0,     0,     0,     0,
      64,    40,     0,     0,     0,     0,     0,     0,     0,     0,
      65,    66,     0,    42,    67,    68,     0,     0,     0,    44,
      45,    46,   176,   177,   178,     0,     0,     0,    52,    53,
      39,    54,    55,    56,     0,     0,    57,     0,    40,     0,
      58,    59,    60,    61,    62,    63,     0,    65,   112,     0,
      42,    67,   113,     0,    43,     0,    44,    45,    46,    47,
      48,    49,     0,    50,    51,    52,    53,    39,    54,    55,
      56,     0,     0,    57,     0,    40,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,     0,
      50,    51,    52,    53,   450,    54,    55,    56,   189,    66,
      57,     0,     0,    68,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,    39,     0,     0,     0,
       0,     0,     0,     0,    40,   240,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    65,    42,     0,     0,    67,
      43,     0,    44,    45,    46,    47,    48,    49,     0,    50,
      51,    52,    53,    39,    54,    55,    56,     0,     0,    57,
       0,    40,   240,    58,    59,    60,    61,    62,    63,     0,
       0,     0,    65,    42,     0,     0,    67,    43,     0,    44,
      45,    46,    47,    48,    49,     0,    50,    51,    52,    53,
      39,    54,    55,    56,     0,     0,    57,     0,    40,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
      42,     0,     0,     0,     0,     0,    44,    45,    46,   176,
     177,   178,     0,     0,     0,    52,    53,     0,    54,    55,
      56,    65,   112,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   329,     0,
       0,     0,     0,    39,     0,     0,     0,     0,    65,     0,
       0,    40,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    42,     0,   578,     0,   579,     0,    44,
      45,    46,   176,   177,   178,   580,    39,     0,    52,    53,
       0,    54,    55,    56,    40,   189,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,    42,     0,     0,     0,
       0,     0,    44,    45,    46,   176,   177,   178,     0,     0,
       0,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,    39,     0,   725,     0,
     579,     0,     0,     0,    40,     0,     0,     0,   580,     0,
       0,     0,     0,     0,     0,     0,    42,     0,   189,     0,
       0,     0,    44,    45,    46,   176,   177,   178,     0,    39,
       0,    52,    53,   579,    54,    55,    56,    40,     0,    57,
       0,   580,     0,    58,    59,    60,    61,    62,    63,    42,
       0,   189,     0,     0,     0,    44,    45,    46,   176,   177,
     178,     0,     0,     0,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   580,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   189,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   189
  };

  const short
  parser::yycheck_[] =
  {
      87,   104,   243,    87,   111,   296,    64,   186,   188,   104,
      29,    98,   328,   109,    98,   166,   218,   185,    98,   228,
      29,   109,   110,    34,    98,   184,   107,   224,   225,   476,
     100,   440,    87,   101,    41,   294,     5,   105,   106,   463,
     108,    12,   302,    19,    77,    27,   214,     1,   289,    34,
      64,   467,   110,    21,   362,    27,   131,     1,   110,   105,
       1,     1,     1,   510,    89,   133,   681,     1,    80,    77,
     101,   257,   103,    79,    77,    82,     1,     0,   106,   186,
     187,    77,    80,   681,   130,   173,   257,    80,   116,   195,
     102,   474,   710,   115,   103,   201,   110,   130,   185,   186,
     175,   188,    84,   550,   110,   120,   131,   324,   195,   124,
     116,   297,    84,   116,   201,   173,   122,   131,   116,   410,
     116,   173,   130,   116,   109,   110,   297,   214,   511,   105,
     748,   109,    91,   101,    93,    94,   223,   445,   109,   563,
      99,   228,   229,   130,   228,   229,   165,   166,   408,   764,
     104,   567,   102,   370,   228,   229,   165,   166,    80,   173,
     104,   175,   121,   104,   104,   104,   764,   384,   766,   188,
     104,   115,    77,   184,   115,   115,   115,    14,   102,   104,
     248,   115,   240,   251,    77,   253,   261,    22,   173,   105,
     115,   115,   273,   109,   116,   111,   579,   580,   285,   168,
     169,   307,    80,    81,   120,   292,   293,    80,   292,    80,
      79,   119,   120,   224,   225,    80,    51,    52,    80,    80,
     307,   102,    91,   391,    93,    94,   240,   543,   544,   238,
      99,   289,    86,    27,   115,    89,   645,   292,    78,   224,
     225,   328,   310,   116,    84,   116,   109,   261,   117,    24,
     467,   116,   121,   122,   116,   116,   703,   120,   705,   518,
     105,   124,   709,   115,   109,   556,   111,   354,   355,   356,
     354,   329,   110,   109,   109,   120,   116,   286,   287,   124,
     354,   266,   356,   294,   120,   120,   110,   116,   124,   124,
     102,   700,   116,   363,   109,   678,   364,   400,   102,   746,
     309,   115,   389,   115,   391,   400,   547,   458,   383,   506,
     390,   115,   109,   481,   473,   411,   102,   413,   325,    71,
     416,   612,   418,   411,   427,   413,   494,   710,   416,   115,
     418,   101,    91,   103,    93,    94,   515,   102,    14,   414,
      99,   110,   110,   185,   186,   105,   525,   116,   116,   110,
     115,   642,    65,    66,    86,   116,   573,    89,    90,   131,
     676,    81,   121,    79,    84,   748,   125,   112,   751,   383,
     112,   116,   214,    89,   116,    91,   593,    93,    94,   588,
     101,   223,   112,    99,   471,    87,   116,   474,   112,   476,
     477,    86,   116,   130,   481,   597,    91,   101,   110,   103,
     414,   598,   599,    80,    81,   121,   122,   494,   727,   125,
     126,   730,    80,    81,   109,   110,   427,   110,   525,   526,
      80,    81,   509,   510,   511,   605,   411,   101,   413,   103,
     101,   416,   103,   418,    72,    73,    74,    75,   110,   458,
     681,   110,     4,   285,   463,   654,   101,   110,   103,   458,
     667,   293,   110,    84,   463,    86,   543,   544,   101,   101,
     103,   103,   473,   550,    23,   561,   116,    27,    90,   115,
      77,   787,    77,   561,    77,    77,   487,   488,   173,    15,
      81,   106,   106,    41,   116,   130,   328,   110,   116,   547,
     110,   578,   579,   580,   110,   506,   110,   565,   110,   116,
     106,   588,   487,   488,   588,   102,   715,   518,   102,    80,
      79,    90,    81,   355,   588,    84,    79,   734,   605,   625,
     717,   506,    91,   764,    93,    94,    89,   110,    91,   110,
      99,   116,   110,   110,   110,   117,    85,   117,   625,   719,
      85,   116,    25,    27,   563,   762,   763,    81,   117,   391,
     106,   560,   121,   122,   563,    77,   125,   126,   121,   122,
     130,    77,   125,   126,   131,    77,   653,   654,   655,   653,
     654,   655,   751,   110,   783,   681,   561,   757,   117,   653,
     654,   655,   117,    77,    80,   672,   116,   598,   599,   676,
      80,   678,    27,    81,   681,   116,   102,   117,   116,   779,
     295,    81,   109,    89,   691,   110,    84,   691,   106,   628,
      78,   106,   116,   598,   599,   117,   703,   691,   705,    77,
     117,   727,   709,   710,   730,   728,    28,    27,   715,   471,
     733,   715,   719,   720,   721,   477,   720,   721,   733,   481,
     727,   715,     9,   730,   751,   110,   720,   721,   659,   102,
     110,   115,   494,   740,    37,    27,   740,    80,   764,   746,
     766,   748,    84,   110,   751,    80,   740,   509,    11,    19,
     757,    90,    86,   692,   659,    53,    86,   764,   120,   766,
     116,    80,   110,   110,   771,   102,    84,   771,    84,   102,
     385,   165,   779,   122,   779,   721,   783,   771,   708,   783,
     787,   543,   544,   292,   659,   488,   717,   599,   403,   783,
      79,     4,   717,    15,   409,   225,   411,   544,   413,    12,
     672,   416,    91,   418,    93,    94,   771,   214,   285,   511,
      99,    24,   717,   622,   768,    28,   578,    30,    31,    32,
      33,    34,    35,   686,   679,   473,    39,    40,   117,    42,
      43,    44,   121,   122,    47,   627,   125,   126,    51,    52,
      53,    54,    55,    56,   727,   692,   630,    79,    80,    34,
     299,   466,   467,   254,   642,   641,   251,   216,   645,    91,
     560,    93,    94,   500,   733,   400,    79,    99,   399,   547,
     261,    -1,    -1,    86,    -1,    88,    89,    -1,    91,    92,
      -1,    94,    -1,   498,    -1,   117,    -1,   502,   101,   121,
     122,    -1,   105,   125,   126,    -1,   109,   110,   111,    -1,
      -1,    -1,    -1,   116,   117,    -1,   119,   120,   121,   122,
     672,   124,   125,   126,   676,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,    -1,    10,    -1,    12,    13,    -1,
     555,    16,    17,    18,    19,    20,   561,    22,   710,    24,
      -1,    26,   567,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,   751,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,   613,    -1,
      -1,    76,    -1,    86,    -1,    -1,    89,    82,    91,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   787,   641,   110,   643,    -1,
     105,    -1,    -1,   116,   109,    -1,   111,    -1,   121,   122,
      -1,    -1,   125,   126,   119,   120,    -1,    -1,   123,   124,
      -1,    -1,    -1,    -1,   129,   130,   131,   132,    -1,    -1,
     135,    -1,    -1,    -1,   139,   140,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,    -1,     7,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    16,    17,    18,    -1,    20,
      -1,    -1,    -1,    24,    -1,    26,    -1,    28,   713,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    57,   742,    -1,    -1,
      -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,
      91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,
     111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,   120,
      -1,    -1,   123,   124,    -1,    -1,    -1,    -1,   129,   130,
     131,   132,    -1,    -1,   135,     3,     4,     5,   139,   140,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    16,    17,
      18,    -1,    20,    -1,    -1,    -1,    24,    -1,    26,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,
      -1,   109,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   119,   120,    -1,    -1,   123,   124,    -1,    -1,    -1,
      -1,   129,   130,   131,   132,    -1,    -1,   135,     3,     4,
       5,   139,   140,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    16,    17,    18,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    76,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     105,    -1,    -1,    -1,   109,    -1,   111,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   119,   120,    -1,    -1,   123,   124,
      -1,    -1,    -1,    -1,   129,   130,   131,   132,    -1,     1,
     135,     3,     4,     5,   139,   140,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,    -1,    -1,   109,    -1,   111,
      -1,    -1,    -1,   115,    -1,    -1,    -1,   119,   120,    -1,
      -1,   123,   124,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,    -1,    -1,   135,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    -1,    91,    -1,    93,    94,    -1,    -1,    -1,    -1,
      99,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,
     109,   110,   111,    -1,    -1,    -1,    -1,   116,   117,    -1,
     119,   120,   121,   122,   123,   124,   125,   126,    -1,    -1,
     129,   130,   131,   132,    -1,    -1,   135,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    -1,    91,    -1,    93,    94,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,    -1,   105,
     106,    -1,    -1,   109,    -1,   111,    -1,    -1,    -1,    -1,
      -1,   117,    -1,   119,   120,   121,   122,   123,   124,   125,
     126,    -1,    -1,   129,   130,   131,   132,    -1,    -1,   135,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    -1,
      93,    94,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,
      -1,    -1,   105,    -1,    -1,    -1,   109,    -1,   111,    -1,
      -1,    -1,    -1,    -1,   117,    -1,   119,   120,   121,   122,
     123,   124,   125,   126,    -1,    -1,   129,   130,   131,   132,
      -1,    -1,   135,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   102,    -1,    -1,   105,    -1,    -1,    -1,   109,
      -1,   111,    -1,    -1,    -1,   115,    -1,    -1,    -1,   119,
     120,    -1,    -1,   123,   124,    -1,    -1,    -1,    -1,   129,
     130,   131,   132,    -1,    -1,   135,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    46,
      47,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,
      -1,    -1,   109,    -1,   111,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   119,   120,    -1,    -1,   123,   124,    -1,    -1,
      -1,    -1,   129,   130,   131,   132,    -1,    -1,   135,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   105,    -1,    -1,    -1,   109,    -1,   111,    -1,    -1,
      -1,   115,    -1,    -1,    -1,   119,   120,    -1,    -1,   123,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,
      -1,   109,    -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   119,   120,    -1,    -1,   123,   124,    -1,    -1,    -1,
      -1,   129,   130,   131,   132,    -1,    -1,   135,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
     120,    12,    -1,   123,   124,    -1,    -1,    -1,    19,   129,
     130,   131,   132,    24,    -1,   135,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,
      91,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,    -1,   105,    -1,     4,    -1,   109,    -1,
     111,    -1,    -1,    -1,    12,    -1,   117,    -1,   119,   120,
     121,   122,    -1,   124,   125,   126,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    91,    92,    -1,    94,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,   106,    -1,
       4,   109,    -1,   111,    -1,    -1,    -1,    -1,    12,   117,
      -1,   119,   120,   121,   122,    19,   124,   125,   126,    -1,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    -1,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    92,    -1,
      94,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,   105,    -1,     4,    -1,   109,    -1,   111,    -1,    -1,
      -1,    12,    -1,   117,    -1,   119,   120,   121,   122,    -1,
     124,   125,   126,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,
      91,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,    -1,   105,    -1,     4,    -1,   109,    -1,
     111,    -1,    -1,    -1,    12,    -1,   117,    -1,   119,   120,
     121,   122,    -1,   124,   125,   126,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    91,    92,    -1,    94,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,    -1,     4,
      -1,   109,   110,   111,    -1,    -1,    -1,    12,    -1,   117,
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
      -1,    -1,    -1,    -1,    -1,    -1,    78,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,
      92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,   105,    -1,     4,    -1,   109,    -1,   111,
      -1,    -1,    -1,    12,    -1,   117,    -1,   119,   120,   121,
     122,    -1,   124,   125,   126,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,
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
      -1,    -1,    88,    -1,    -1,    91,    92,    -1,    94,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,
      -1,     4,    -1,   109,    -1,   111,    -1,    -1,    -1,    12,
      -1,   117,    -1,   119,   120,   121,   122,    -1,   124,   125,
     126,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    -1,    92,
      -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,    -1,   105,     4,    -1,    -1,   109,    -1,   111,    -1,
      -1,    12,    -1,    -1,   117,    -1,   119,   120,    -1,   122,
      -1,   124,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,
      -1,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,    -1,   105,     4,    -1,    -1,   109,    -1,
     111,    -1,    -1,    12,    -1,    -1,   117,    -1,   119,   120,
      -1,   122,    -1,   124,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    -1,    -1,    92,    -1,    94,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,   105,     4,    -1,    -1,
     109,    -1,   111,    -1,    -1,    12,    -1,    -1,    -1,    -1,
     119,   120,    -1,    -1,    21,   124,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,     4,    42,    43,    44,    -1,    -1,
      47,    -1,    12,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,   109,    12,    -1,    -1,    -1,    -1,    78,    -1,
      -1,    -1,   119,   120,    -1,    24,   123,   124,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,     4,    42,    43,    44,    -1,    -1,    47,   109,
      12,    -1,    51,    52,    53,    54,    55,    56,    -1,   119,
     120,    -1,    24,   123,   124,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,
     109,    12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     119,   120,    -1,    24,   123,   124,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
       4,    42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,
      51,    52,    53,    54,    55,    56,    -1,   119,   120,    -1,
      24,   123,   124,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,     4,    42,    43,
      44,    -1,    -1,    47,    -1,    12,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,    78,    42,    43,    44,   119,   120,
      47,    -1,    -1,   124,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    12,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   119,    24,    -1,    -1,   123,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,     4,    42,    43,    44,    -1,    -1,    47,
      -1,    12,   109,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,   119,    24,    -1,    -1,   123,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
       4,    42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,
      44,   119,   120,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,
      -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,   119,    -1,
      -1,    12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    24,    -1,    99,    -1,   101,    -1,    30,
      31,    32,    33,    34,    35,   109,     4,    -1,    39,    40,
      -1,    42,    43,    44,    12,   119,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    99,    -1,
     101,    -1,    -1,    -1,    12,    -1,    -1,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,   119,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,     4,
      -1,    39,    40,   101,    42,    43,    44,    12,    -1,    47,
      -1,   109,    -1,    51,    52,    53,    54,    55,    56,    24,
      -1,   119,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   119
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   101,   142,   143,   144,   147,   120,   124,   343,
     148,   159,     0,   148,    65,    66,   145,   102,   115,   149,
     160,   161,     1,   104,   342,   105,   130,   211,   211,   109,
     150,    14,   162,   170,   171,   130,   212,    77,    77,     4,
      12,    21,    24,    28,    30,    31,    32,    33,    34,    35,
      37,    38,    39,    40,    42,    43,    44,    47,    51,    52,
      53,    54,    55,    56,   109,   119,   120,   123,   124,   151,
     152,   153,   157,   310,   313,   314,   327,   328,   329,   335,
      27,    24,   163,   115,   158,     3,     5,     6,     7,     8,
      10,    13,    16,    17,    18,    19,    20,    22,    26,    29,
      36,    50,    57,    63,    76,    82,    88,    91,    92,   105,
     109,   111,   120,   124,   129,   130,   131,   132,   135,   139,
     140,   168,   172,   173,   174,   175,   177,   192,   216,   258,
     263,   267,   268,   270,   271,   272,   273,   299,   300,   303,
     304,   326,   327,   329,   337,   338,   341,   106,   116,   343,
      79,    89,    91,    93,    94,    99,   121,   122,   125,   126,
     332,   333,   334,   336,   110,   116,   109,   154,   101,   103,
     146,   343,   115,   109,   266,   267,    33,    34,    35,    79,
      88,    91,    92,    94,   101,   105,   109,   111,   117,   119,
     121,   122,   125,   126,   197,   218,   219,   223,   225,   227,
     228,   230,   232,   234,   308,   309,   311,   313,   315,   316,
     323,   324,   325,   335,   109,   101,   103,   292,   266,    72,
      73,    74,    75,   178,   101,   103,   208,   209,    19,    37,
     176,   225,   227,   309,    14,   292,   271,   105,   264,   265,
     109,   327,   105,   109,   301,   302,   304,   338,    92,   271,
     290,   291,   271,   270,   271,    79,   106,   117,   122,   126,
     266,   267,   274,   276,   277,   306,   320,   322,   331,   332,
     334,   339,   340,    91,   110,   116,   274,   275,   332,   333,
     334,   339,   344,   112,   344,    19,   264,   264,   131,   167,
     158,    71,   198,    80,   116,    81,    84,   117,   260,   261,
     262,   306,   319,   321,   330,   332,   333,   100,   271,   101,
      87,   130,   110,   110,   110,   110,   110,   110,   153,    78,
     155,   156,   157,   148,   148,     4,   164,    23,    80,   109,
     216,   251,   252,   253,   329,    28,   106,   220,   221,   223,
     225,    86,    89,   110,   220,   238,   315,   344,   344,   313,
     325,    27,   202,   234,    90,    86,    89,   231,   234,   220,
     237,   238,    20,    46,    92,   266,   289,   293,   294,   295,
     293,   115,   269,    77,    77,    77,    77,   214,   221,   235,
     207,   258,   259,   267,   207,    15,   187,   225,   225,    80,
     116,    81,    41,    89,   131,   327,    77,   130,   340,    80,
     116,   213,   271,    86,   290,   260,   328,   337,   319,    78,
      84,   116,   106,   116,   267,   110,   116,   110,   116,   110,
     110,   110,   116,   112,   235,   327,   327,   117,   169,   305,
     317,   318,   333,   340,   130,   197,   215,   221,   222,   326,
     266,   279,   280,   295,   328,    27,   210,   262,   268,   234,
      78,   296,   297,   298,   327,   271,   110,   110,   116,   102,
     342,   343,    12,   109,   165,   166,   101,   103,   281,   214,
     333,    80,   102,   116,   239,   106,    80,    90,   110,   110,
     110,   116,   110,   110,   112,   117,   117,   101,   103,   201,
     225,   221,   227,   110,   116,   209,   292,   271,    85,   102,
     115,   342,    25,    27,   206,   102,   115,   342,   266,    81,
      80,    81,   195,   221,   242,   109,   309,   220,   130,   131,
     106,    77,    77,   110,   105,   109,   111,   307,   308,   301,
      77,   266,   117,   117,   266,   278,   295,   266,   274,   274,
     274,   274,    77,    80,    80,   329,   338,   116,    77,   130,
      80,    81,   193,   246,   210,    81,   116,   117,   209,   102,
     116,    81,   157,   109,   151,    92,   102,   115,   266,   282,
     283,   284,   288,   282,   342,   110,   221,   253,    99,   101,
     109,   240,   241,   323,   242,   221,   220,     7,    26,   188,
     199,   200,   259,   200,   220,   266,   294,   266,   101,   103,
     205,   259,   221,   242,   240,    84,   181,   315,   326,   106,
     110,   112,   116,    78,   214,   217,   217,   117,   117,   317,
      77,   242,    28,   247,   248,   249,    27,   243,     9,   254,
     255,   256,   266,   295,   297,   274,   151,   110,   271,   282,
     102,   115,    84,    86,   285,   286,   287,   342,   221,   323,
     323,   110,    37,   189,    19,    37,   187,   225,   102,   115,
     342,   269,    26,   191,   203,   204,   259,   204,   182,   325,
      27,   184,    80,   295,   266,    77,   116,    77,   239,    84,
     224,   229,   233,   234,   250,   101,   103,   254,    22,    51,
      52,   109,   179,   257,   312,   313,   256,   110,   284,   279,
     266,   210,   287,    80,   102,    80,   225,   187,   225,    80,
      81,   196,   199,    11,    19,   190,   102,   115,   342,    86,
     101,   103,   185,   215,   214,    99,   248,    90,   117,   233,
     305,   244,   245,   269,   244,   110,   225,   226,   236,   257,
      53,   180,    86,   210,   242,   242,    80,   194,    81,   196,
     242,   109,   241,   323,   266,   187,   203,   183,   325,    78,
     186,   187,    78,   186,   229,   250,   229,   102,   115,   302,
     342,   116,   110,   225,   266,   102,   110,   242,   323,    84,
     325,   102,   102,   115,   342,   342,   245,    80,   236,   182,
     187,   214
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
     269,   270,   270,   270,   270,   271,   271,   271,   271,   271,
     271,   271,   271,   271,   272,   272,   273,   273,   273,   273,
     273,   273,   273,   274,   274,   274,   275,   275,   276,   276,
     276,   276,   276,   276,   276,   277,   277,   278,   278,   279,
     280,   280,   281,   281,   281,   281,   282,   282,   283,   283,
     283,   284,   285,   285,   286,   286,   287,   288,   288,   289,
     289,   290,   290,   291,   291,   292,   292,   293,   293,   293,
     293,   294,   294,   295,   295,   295,   296,   296,   297,   297,
     297,   298,   298,   299,   299,   300,   300,   301,   301,   301,
     302,   302,   303,   303,   303,   303,   304,   304,   305,   305,
     306,   306,   307,   307,   307,   308,   308,   308,   308,   308,
     309,   309,   309,   310,   310,   310,   310,   310,   311,   311,
     312,   313,   313,   314,   315,   315,   315,   316,   316,   316,
     316,   317,   317,   318,   318,   319,   319,   320,   320,   321,
     321,   322,   322,   323,   324,   325,   325,   325,   325,   325,
     326,   326,   327,   327,   327,   328,   328,   329,   329,   329,
     329,   329,   329,   329,   329,   330,   330,   331,   331,   332,
     333,   333,   334,   334,   335,   335,   335,   335,   335,   335,
     335,   335,   335,   335,   335,   335,   335,   335,   335,   335,
     335,   335,   336,   336,   336,   337,   337,   338,   339,   339,
     340,   340,   341,   341,   341,   341,   341,   342,   342,   343,
     343,   344,   344
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
       1,     3,     4,     3,     1,     1,     3,     2,     1,     1,
       0,     2,     3,     2,     1,     3,     2,     4,     4,     8,
       4,     2,     2,     1,     4,     1,     1,     1,     1,     3,
       3,     3,     1,     1,     2,     2,     3,     3,     1,     1,
       2,     4,     3,     5,     3,     3,     3,     3,     1,     1,
       3,     1,     3,     3,     2,     2,     1,     2,     3,     2,
       1,     2,     3,     2,     2,     1,     4,     1,     2,     1,
       2,     1,     2,     2,     1,     3,     3,     3,     2,     1,
       0,     1,     2,     3,     1,     2,     1,     0,     3,     1,
       1,     3,     1,     1,     1,     1,     3,     1,     3,     1,
       3,     1,     2,     3,     2,     3,     1,     2,     1,     3,
       1,     3,     1,     2,     2,     1,     3,     3,     3,     2,
       1,     3,     3,     1,     3,     3,     3,     3,     1,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     1,     1,     1,     1,     1,
       3,     1,     3,     1,     3,     1,     1,     1,     1,     1,
       1,     3,     1,     3,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     1
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
  "explicit_activation", "exp", "infixexp", "exp10", "optSemi", "fexp",
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
       0,   505,   505,   522,   523,   525,   529,   530,   531,   533,
     534,   536,   537,   540,   542,   543,   544,   552,   553,   555,
     557,   558,   560,   561,   563,   564,   565,   567,   568,   570,
     571,   573,   574,   578,   579,   581,   582,   584,   586,   587,
     589,   602,   603,   605,   606,   608,   609,   613,   614,   619,
     620,   622,   623,   624,   626,   627,   631,   633,   634,   636,
     637,   638,   639,   642,   643,   650,   652,   654,   656,   657,
     659,   660,   663,   665,   666,   669,   670,   674,   675,   676,
     677,   678,   680,   681,   682,   684,   695,   696,   698,   700,
     701,   705,   706,   708,   709,   710,   711,   713,   714,   715,
     716,   718,   721,   723,   725,   727,   728,   730,   730,   732,
     732,   736,   738,   739,   743,   744,   746,   747,   749,   750,
     751,   753,   754,   755,   759,   760,   762,   763,   764,   806,
     807,   809,   810,   811,   812,   814,   815,   817,   818,   820,
     821,   823,   824,   825,   826,   828,   829,   831,   832,   835,
     836,   837,   838,   840,   841,   843,   845,   846,   854,   855,
     857,   858,   859,   872,   873,   882,   884,   886,   887,   889,
     890,   894,   900,   901,   908,   909,   911,   912,   914,   916,
     925,   927,   929,   930,   932,   935,   937,   938,   940,   941,
     943,   944,   945,   947,   949,   950,   957,   964,   965,   966,
     967,   968,   969,   970,   976,   977,   980,   982,   983,   985,
     986,   988,   989,   996,   997,   999,  1000,  1001,  1004,  1005,
    1023,  1029,  1030,  1031,  1033,  1034,  1036,  1038,  1040,  1041,
    1043,  1044,  1046,  1047,  1049,  1050,  1052,  1053,  1055,  1056,
    1058,  1060,  1061,  1063,  1064,  1066,  1067,  1068,  1070,  1071,
    1072,  1077,  1079,  1080,  1082,  1086,  1087,  1089,  1090,  1094,
    1104,  1105,  1107,  1108,  1109,  1110,  1111,  1112,  1113,  1116,
    1117,  1119,  1120,  1125,  1126,  1130,  1131,  1133,  1134,  1136,
    1137,  1142,  1143,  1144,  1145,  1148,  1149,  1150,  1151,  1153,
    1155,  1156,  1157,  1159,  1162,  1163,  1166,  1167,  1168,  1169,
    1170,  1175,  1176,  1182,  1183,  1184,  1189,  1190,  1208,  1209,
    1210,  1211,  1212,  1213,  1214,  1216,  1217,  1230,  1232,  1242,
    1244,  1245,  1248,  1249,  1250,  1251,  1253,  1254,  1256,  1257,
    1258,  1260,  1262,  1263,  1265,  1266,  1275,  1277,  1278,  1280,
    1281,  1283,  1284,  1286,  1287,  1290,  1291,  1293,  1294,  1295,
    1296,  1301,  1302,  1304,  1305,  1306,  1311,  1312,  1314,  1315,
    1316,  1318,  1319,  1351,  1352,  1354,  1355,  1357,  1358,  1359,
    1361,  1362,  1364,  1365,  1366,  1367,  1369,  1370,  1372,  1373,
    1375,  1376,  1379,  1380,  1381,  1383,  1384,  1385,  1386,  1387,
    1389,  1390,  1391,  1393,  1394,  1395,  1396,  1397,  1400,  1401,
    1403,  1405,  1406,  1410,  1412,  1413,  1414,  1416,  1417,  1418,
    1419,  1424,  1425,  1427,  1428,  1430,  1431,  1434,  1435,  1440,
    1441,  1443,  1444,  1448,  1450,  1452,  1453,  1454,  1455,  1456,
    1459,  1460,  1462,  1463,  1464,  1466,  1467,  1469,  1470,  1471,
    1472,  1473,  1474,  1475,  1476,  1478,  1479,  1481,  1482,  1484,
    1486,  1487,  1489,  1490,  1492,  1493,  1494,  1495,  1496,  1497,
    1498,  1499,  1500,  1501,  1502,  1503,  1504,  1505,  1506,  1507,
    1508,  1509,  1511,  1512,  1513,  1517,  1518,  1520,  1522,  1523,
    1525,  1526,  1530,  1531,  1532,  1533,  1534,  1539,  1542,  1546,
    1547,  1549,  1550
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
#line 7027 "parser.cc"

#line 1559 "parser.y"


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

