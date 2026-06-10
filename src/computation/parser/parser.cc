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
#line 56 "parser.y"

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

      case symbol_kind::S_deriv_strategy_no_via: // deriv_strategy_no_via
        value.YY_MOVE_OR_COPY< Hs::DerivingStrategy > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.YY_MOVE_OR_COPY< Hs::FieldDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infix: // infix
        value.YY_MOVE_OR_COPY< Hs::Fixity > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fd: // fd
        value.YY_MOVE_OR_COPY< Hs::FunDep > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gadt_constr: // gadt_constr
        value.YY_MOVE_OR_COPY< Hs::GADTConstructorDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gadt_constrlist: // gadt_constrlist
      case symbol_kind::S_gadt_constrs: // gadt_constrs
        value.YY_MOVE_OR_COPY< Hs::GADTConstructorsDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.YY_MOVE_OR_COPY< Hs::GuardedRHS > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_impspec: // impspec
        value.YY_MOVE_OR_COPY< Hs::ImpSpec > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_kind: // kind
        value.YY_MOVE_OR_COPY< Hs::Kind > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_export: // export
      case symbol_kind::S_import: // import
        value.YY_MOVE_OR_COPY< Hs::LExport > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.YY_MOVE_OR_COPY< Hs::LImpDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_deriv_strategy_via: // deriv_strategy_via
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
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_constr_stuff: // constr_stuff
        value.YY_MOVE_OR_COPY< Hs::LType > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        value.YY_MOVE_OR_COPY< Hs::LTypeVar > (YY_MOVE (that.value));
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

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        value.YY_MOVE_OR_COPY< Hs::TypeFamilyInstanceEqn > (YY_MOVE (that.value));
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

      case symbol_kind::S_fbinds: // fbinds
      case symbol_kind::S_fbinds1: // fbinds1
        value.YY_MOVE_OR_COPY< Located<Hs::FieldBindings> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infixexp: // infixexp
        value.YY_MOVE_OR_COPY< Located<Hs::InfixExp> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_stand_alone_deriving: // stand_alone_deriving
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
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
      case symbol_kind::S_call_conv: // call_conv
      case symbol_kind::S_modid: // modid
        value.YY_MOVE_OR_COPY< Located<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_optqualified: // optqualified
        value.YY_MOVE_OR_COPY< bool > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_CHAR: // "CHAR"
      case symbol_kind::S_PRIMCHAR: // "PRIMCHAR"
        value.YY_MOVE_OR_COPY< char > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.YY_MOVE_OR_COPY< double > (YY_MOVE (that.value));
        break;

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

      case symbol_kind::S_RATIONAL: // "RATIONAL"
        value.YY_MOVE_OR_COPY< rational > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_export_subspec: // export_subspec
        value.YY_MOVE_OR_COPY< std::optional<Hs::ExportSubSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.YY_MOVE_OR_COPY< std::optional<Hs::ImpSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_opt_kind_sig: // opt_kind_sig
        value.YY_MOVE_OR_COPY< std::optional<Hs::Kind> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
        value.YY_MOVE_OR_COPY< std::optional<Hs::LType> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.YY_MOVE_OR_COPY< std::optional<Located<Hs::Binds>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
        value.YY_MOVE_OR_COPY< std::optional<Located<Hs::Decls>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fbind: // fbind
        value.YY_MOVE_OR_COPY< std::optional<Located<Hs::FieldBinding>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_opt_datafam_kind_sig: // opt_datafam_kind_sig
      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        value.YY_MOVE_OR_COPY< std::optional<Located<Hs::Kind>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeas: // maybeas
      case symbol_kind::S_opt_class: // opt_class
        value.YY_MOVE_OR_COPY< std::optional<Located<std::string>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_prec: // prec
        value.YY_MOVE_OR_COPY< std::optional<int> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_overlap_pragma: // overlap_pragma
        value.YY_MOVE_OR_COPY< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.YY_MOVE_OR_COPY< std::optional<std::vector<Hs::LExport>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_where_type_family: // where_type_family
        value.YY_MOVE_OR_COPY< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.YY_MOVE_OR_COPY< std::pair<Hs::Context,Hs::LType> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.YY_MOVE_OR_COPY< std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_INLINE_PRAG: // "{-# INLINE"
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
      case symbol_kind::S_tyop: // tyop
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
      case symbol_kind::S_field: // field
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
        value.YY_MOVE_OR_COPY< std::string > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_datafam_inst_hdr: // datafam_inst_hdr
        value.YY_MOVE_OR_COPY< std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybe_derivings: // maybe_derivings
      case symbol_kind::S_derivings: // derivings
      case symbol_kind::S_deriving: // deriving
        value.YY_MOVE_OR_COPY< std::vector<Hs::Deriving> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.YY_MOVE_OR_COPY< std::vector<Hs::FieldDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fds: // fds
      case symbol_kind::S_fds1: // fds1
        value.YY_MOVE_OR_COPY< std::vector<Hs::FunDep> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.YY_MOVE_OR_COPY< std::vector<Hs::GuardedRHS> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_importlist: // importlist
      case symbol_kind::S_importlist1: // importlist1
        value.YY_MOVE_OR_COPY< std::vector<Hs::LExport> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.YY_MOVE_OR_COPY< std::vector<Hs::LImpDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_deriv_types: // deriv_types
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_deriv_clause_types: // deriv_clause_types
        value.YY_MOVE_OR_COPY< std::vector<Hs::LType> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sks_vars: // sks_vars
        value.YY_MOVE_OR_COPY< std::vector<Hs::LTypeCon> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_varids0: // varids0
      case symbol_kind::S_forall: // forall
        value.YY_MOVE_OR_COPY< std::vector<Hs::LTypeVar> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.YY_MOVE_OR_COPY< std::vector<Hs::LVar> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ty_fam_inst_eqn_list: // ty_fam_inst_eqn_list
      case symbol_kind::S_ty_fam_inst_eqns: // ty_fam_inst_eqns
        value.YY_MOVE_OR_COPY< std::vector<Hs::TypeFamilyInstanceEqn> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.YY_MOVE_OR_COPY< std::vector<Located<Hs::Alt>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decls: // decls
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
      case symbol_kind::S_ops: // ops
      case symbol_kind::S_con_list: // con_list
        value.YY_MOVE_OR_COPY< std::vector<Located<std::string>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_projection: // projection
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

      case symbol_kind::S_deriv_strategy_no_via: // deriv_strategy_no_via
        value.move< Hs::DerivingStrategy > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.move< Hs::FieldDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infix: // infix
        value.move< Hs::Fixity > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fd: // fd
        value.move< Hs::FunDep > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gadt_constr: // gadt_constr
        value.move< Hs::GADTConstructorDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gadt_constrlist: // gadt_constrlist
      case symbol_kind::S_gadt_constrs: // gadt_constrs
        value.move< Hs::GADTConstructorsDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.move< Hs::GuardedRHS > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_impspec: // impspec
        value.move< Hs::ImpSpec > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_kind: // kind
        value.move< Hs::Kind > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_export: // export
      case symbol_kind::S_import: // import
        value.move< Hs::LExport > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.move< Hs::LImpDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_deriv_strategy_via: // deriv_strategy_via
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
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_constr_stuff: // constr_stuff
        value.move< Hs::LType > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        value.move< Hs::LTypeVar > (YY_MOVE (that.value));
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

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        value.move< Hs::TypeFamilyInstanceEqn > (YY_MOVE (that.value));
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

      case symbol_kind::S_fbinds: // fbinds
      case symbol_kind::S_fbinds1: // fbinds1
        value.move< Located<Hs::FieldBindings> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infixexp: // infixexp
        value.move< Located<Hs::InfixExp> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_stand_alone_deriving: // stand_alone_deriving
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
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
      case symbol_kind::S_call_conv: // call_conv
      case symbol_kind::S_modid: // modid
        value.move< Located<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_optqualified: // optqualified
        value.move< bool > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_CHAR: // "CHAR"
      case symbol_kind::S_PRIMCHAR: // "PRIMCHAR"
        value.move< char > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.move< double > (YY_MOVE (that.value));
        break;

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

      case symbol_kind::S_RATIONAL: // "RATIONAL"
        value.move< rational > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_export_subspec: // export_subspec
        value.move< std::optional<Hs::ExportSubSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.move< std::optional<Hs::ImpSpec> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_opt_kind_sig: // opt_kind_sig
        value.move< std::optional<Hs::Kind> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
        value.move< std::optional<Hs::LType> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.move< std::optional<Located<Hs::Binds>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
        value.move< std::optional<Located<Hs::Decls>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fbind: // fbind
        value.move< std::optional<Located<Hs::FieldBinding>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_opt_datafam_kind_sig: // opt_datafam_kind_sig
      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        value.move< std::optional<Located<Hs::Kind>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeas: // maybeas
      case symbol_kind::S_opt_class: // opt_class
        value.move< std::optional<Located<std::string>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_prec: // prec
        value.move< std::optional<int> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_overlap_pragma: // overlap_pragma
        value.move< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.move< std::optional<std::vector<Hs::LExport>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_where_type_family: // where_type_family
        value.move< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.move< std::pair<Hs::Context,Hs::LType> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.move< std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_INLINE_PRAG: // "{-# INLINE"
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
      case symbol_kind::S_tyop: // tyop
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
      case symbol_kind::S_field: // field
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
        value.move< std::string > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_datafam_inst_hdr: // datafam_inst_hdr
        value.move< std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybe_derivings: // maybe_derivings
      case symbol_kind::S_derivings: // derivings
      case symbol_kind::S_deriving: // deriving
        value.move< std::vector<Hs::Deriving> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.move< std::vector<Hs::FieldDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fds: // fds
      case symbol_kind::S_fds1: // fds1
        value.move< std::vector<Hs::FunDep> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.move< std::vector<Hs::GuardedRHS> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_importlist: // importlist
      case symbol_kind::S_importlist1: // importlist1
        value.move< std::vector<Hs::LExport> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.move< std::vector<Hs::LImpDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_deriv_types: // deriv_types
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_deriv_clause_types: // deriv_clause_types
        value.move< std::vector<Hs::LType> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sks_vars: // sks_vars
        value.move< std::vector<Hs::LTypeCon> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_varids0: // varids0
      case symbol_kind::S_forall: // forall
        value.move< std::vector<Hs::LTypeVar> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.move< std::vector<Hs::LVar> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ty_fam_inst_eqn_list: // ty_fam_inst_eqn_list
      case symbol_kind::S_ty_fam_inst_eqns: // ty_fam_inst_eqns
        value.move< std::vector<Hs::TypeFamilyInstanceEqn> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Located<Hs::Alt>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decls: // decls
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
      case symbol_kind::S_ops: // ops
      case symbol_kind::S_con_list: // con_list
        value.move< std::vector<Located<std::string>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_projection: // projection
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

      case symbol_kind::S_deriv_strategy_no_via: // deriv_strategy_no_via
        value.copy< Hs::DerivingStrategy > (that.value);
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.copy< Hs::FieldDecl > (that.value);
        break;

      case symbol_kind::S_infix: // infix
        value.copy< Hs::Fixity > (that.value);
        break;

      case symbol_kind::S_fd: // fd
        value.copy< Hs::FunDep > (that.value);
        break;

      case symbol_kind::S_gadt_constr: // gadt_constr
        value.copy< Hs::GADTConstructorDecl > (that.value);
        break;

      case symbol_kind::S_gadt_constrlist: // gadt_constrlist
      case symbol_kind::S_gadt_constrs: // gadt_constrs
        value.copy< Hs::GADTConstructorsDecl > (that.value);
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.copy< Hs::GuardedRHS > (that.value);
        break;

      case symbol_kind::S_impspec: // impspec
        value.copy< Hs::ImpSpec > (that.value);
        break;

      case symbol_kind::S_kind: // kind
        value.copy< Hs::Kind > (that.value);
        break;

      case symbol_kind::S_export: // export
      case symbol_kind::S_import: // import
        value.copy< Hs::LExport > (that.value);
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.copy< Hs::LImpDecl > (that.value);
        break;

      case symbol_kind::S_deriv_strategy_via: // deriv_strategy_via
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
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_constr_stuff: // constr_stuff
        value.copy< Hs::LType > (that.value);
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        value.copy< Hs::LTypeVar > (that.value);
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

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        value.copy< Hs::TypeFamilyInstanceEqn > (that.value);
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

      case symbol_kind::S_fbinds: // fbinds
      case symbol_kind::S_fbinds1: // fbinds1
        value.copy< Located<Hs::FieldBindings> > (that.value);
        break;

      case symbol_kind::S_infixexp: // infixexp
        value.copy< Located<Hs::InfixExp> > (that.value);
        break;

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_stand_alone_deriving: // stand_alone_deriving
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
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
      case symbol_kind::S_call_conv: // call_conv
      case symbol_kind::S_modid: // modid
        value.copy< Located<std::string> > (that.value);
        break;

      case symbol_kind::S_optqualified: // optqualified
        value.copy< bool > (that.value);
        break;

      case symbol_kind::S_CHAR: // "CHAR"
      case symbol_kind::S_PRIMCHAR: // "PRIMCHAR"
        value.copy< char > (that.value);
        break;

      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.copy< double > (that.value);
        break;

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

      case symbol_kind::S_RATIONAL: // "RATIONAL"
        value.copy< rational > (that.value);
        break;

      case symbol_kind::S_export_subspec: // export_subspec
        value.copy< std::optional<Hs::ExportSubSpec> > (that.value);
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.copy< std::optional<Hs::ImpSpec> > (that.value);
        break;

      case symbol_kind::S_opt_kind_sig: // opt_kind_sig
        value.copy< std::optional<Hs::Kind> > (that.value);
        break;

      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
        value.copy< std::optional<Hs::LType> > (that.value);
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.copy< std::optional<Located<Hs::Binds>> > (that.value);
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
        value.copy< std::optional<Located<Hs::Decls>> > (that.value);
        break;

      case symbol_kind::S_fbind: // fbind
        value.copy< std::optional<Located<Hs::FieldBinding>> > (that.value);
        break;

      case symbol_kind::S_opt_datafam_kind_sig: // opt_datafam_kind_sig
      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        value.copy< std::optional<Located<Hs::Kind>> > (that.value);
        break;

      case symbol_kind::S_maybeas: // maybeas
      case symbol_kind::S_opt_class: // opt_class
        value.copy< std::optional<Located<std::string>> > (that.value);
        break;

      case symbol_kind::S_prec: // prec
        value.copy< std::optional<int> > (that.value);
        break;

      case symbol_kind::S_overlap_pragma: // overlap_pragma
        value.copy< std::optional<std::string> > (that.value);
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.copy< std::optional<std::vector<Hs::LExport>> > (that.value);
        break;

      case symbol_kind::S_where_type_family: // where_type_family
        value.copy< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > (that.value);
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.copy< std::pair<Hs::Context,Hs::LType> > (that.value);
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.copy< std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > (that.value);
        break;

      case symbol_kind::S_INLINE_PRAG: // "{-# INLINE"
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
      case symbol_kind::S_tyop: // tyop
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
      case symbol_kind::S_field: // field
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
        value.copy< std::string > (that.value);
        break;

      case symbol_kind::S_datafam_inst_hdr: // datafam_inst_hdr
        value.copy< std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > (that.value);
        break;

      case symbol_kind::S_maybe_derivings: // maybe_derivings
      case symbol_kind::S_derivings: // derivings
      case symbol_kind::S_deriving: // deriving
        value.copy< std::vector<Hs::Deriving> > (that.value);
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.copy< std::vector<Hs::FieldDecl> > (that.value);
        break;

      case symbol_kind::S_fds: // fds
      case symbol_kind::S_fds1: // fds1
        value.copy< std::vector<Hs::FunDep> > (that.value);
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.copy< std::vector<Hs::GuardedRHS> > (that.value);
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_importlist: // importlist
      case symbol_kind::S_importlist1: // importlist1
        value.copy< std::vector<Hs::LExport> > (that.value);
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.copy< std::vector<Hs::LImpDecl> > (that.value);
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_deriv_types: // deriv_types
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_deriv_clause_types: // deriv_clause_types
        value.copy< std::vector<Hs::LType> > (that.value);
        break;

      case symbol_kind::S_sks_vars: // sks_vars
        value.copy< std::vector<Hs::LTypeCon> > (that.value);
        break;

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_varids0: // varids0
      case symbol_kind::S_forall: // forall
        value.copy< std::vector<Hs::LTypeVar> > (that.value);
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.copy< std::vector<Hs::LVar> > (that.value);
        break;

      case symbol_kind::S_ty_fam_inst_eqn_list: // ty_fam_inst_eqn_list
      case symbol_kind::S_ty_fam_inst_eqns: // ty_fam_inst_eqns
        value.copy< std::vector<Hs::TypeFamilyInstanceEqn> > (that.value);
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.copy< std::vector<Located<Hs::Alt>> > (that.value);
        break;

      case symbol_kind::S_decls: // decls
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
      case symbol_kind::S_ops: // ops
      case symbol_kind::S_con_list: // con_list
        value.copy< std::vector<Located<std::string>> > (that.value);
        break;

      case symbol_kind::S_projection: // projection
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

      case symbol_kind::S_deriv_strategy_no_via: // deriv_strategy_no_via
        value.move< Hs::DerivingStrategy > (that.value);
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.move< Hs::FieldDecl > (that.value);
        break;

      case symbol_kind::S_infix: // infix
        value.move< Hs::Fixity > (that.value);
        break;

      case symbol_kind::S_fd: // fd
        value.move< Hs::FunDep > (that.value);
        break;

      case symbol_kind::S_gadt_constr: // gadt_constr
        value.move< Hs::GADTConstructorDecl > (that.value);
        break;

      case symbol_kind::S_gadt_constrlist: // gadt_constrlist
      case symbol_kind::S_gadt_constrs: // gadt_constrs
        value.move< Hs::GADTConstructorsDecl > (that.value);
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.move< Hs::GuardedRHS > (that.value);
        break;

      case symbol_kind::S_impspec: // impspec
        value.move< Hs::ImpSpec > (that.value);
        break;

      case symbol_kind::S_kind: // kind
        value.move< Hs::Kind > (that.value);
        break;

      case symbol_kind::S_export: // export
      case symbol_kind::S_import: // import
        value.move< Hs::LExport > (that.value);
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.move< Hs::LImpDecl > (that.value);
        break;

      case symbol_kind::S_deriv_strategy_via: // deriv_strategy_via
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
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_constr_stuff: // constr_stuff
        value.move< Hs::LType > (that.value);
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        value.move< Hs::LTypeVar > (that.value);
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

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        value.move< Hs::TypeFamilyInstanceEqn > (that.value);
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

      case symbol_kind::S_fbinds: // fbinds
      case symbol_kind::S_fbinds1: // fbinds1
        value.move< Located<Hs::FieldBindings> > (that.value);
        break;

      case symbol_kind::S_infixexp: // infixexp
        value.move< Located<Hs::InfixExp> > (that.value);
        break;

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_stand_alone_deriving: // stand_alone_deriving
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
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
      case symbol_kind::S_call_conv: // call_conv
      case symbol_kind::S_modid: // modid
        value.move< Located<std::string> > (that.value);
        break;

      case symbol_kind::S_optqualified: // optqualified
        value.move< bool > (that.value);
        break;

      case symbol_kind::S_CHAR: // "CHAR"
      case symbol_kind::S_PRIMCHAR: // "PRIMCHAR"
        value.move< char > (that.value);
        break;

      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.move< double > (that.value);
        break;

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

      case symbol_kind::S_RATIONAL: // "RATIONAL"
        value.move< rational > (that.value);
        break;

      case symbol_kind::S_export_subspec: // export_subspec
        value.move< std::optional<Hs::ExportSubSpec> > (that.value);
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        value.move< std::optional<Hs::ImpSpec> > (that.value);
        break;

      case symbol_kind::S_opt_kind_sig: // opt_kind_sig
        value.move< std::optional<Hs::Kind> > (that.value);
        break;

      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
        value.move< std::optional<Hs::LType> > (that.value);
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.move< std::optional<Located<Hs::Binds>> > (that.value);
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
        value.move< std::optional<Located<Hs::Decls>> > (that.value);
        break;

      case symbol_kind::S_fbind: // fbind
        value.move< std::optional<Located<Hs::FieldBinding>> > (that.value);
        break;

      case symbol_kind::S_opt_datafam_kind_sig: // opt_datafam_kind_sig
      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        value.move< std::optional<Located<Hs::Kind>> > (that.value);
        break;

      case symbol_kind::S_maybeas: // maybeas
      case symbol_kind::S_opt_class: // opt_class
        value.move< std::optional<Located<std::string>> > (that.value);
        break;

      case symbol_kind::S_prec: // prec
        value.move< std::optional<int> > (that.value);
        break;

      case symbol_kind::S_overlap_pragma: // overlap_pragma
        value.move< std::optional<std::string> > (that.value);
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.move< std::optional<std::vector<Hs::LExport>> > (that.value);
        break;

      case symbol_kind::S_where_type_family: // where_type_family
        value.move< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > (that.value);
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.move< std::pair<Hs::Context,Hs::LType> > (that.value);
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.move< std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > (that.value);
        break;

      case symbol_kind::S_INLINE_PRAG: // "{-# INLINE"
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
      case symbol_kind::S_tyop: // tyop
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
      case symbol_kind::S_field: // field
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
        value.move< std::string > (that.value);
        break;

      case symbol_kind::S_datafam_inst_hdr: // datafam_inst_hdr
        value.move< std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > (that.value);
        break;

      case symbol_kind::S_maybe_derivings: // maybe_derivings
      case symbol_kind::S_derivings: // derivings
      case symbol_kind::S_deriving: // deriving
        value.move< std::vector<Hs::Deriving> > (that.value);
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.move< std::vector<Hs::FieldDecl> > (that.value);
        break;

      case symbol_kind::S_fds: // fds
      case symbol_kind::S_fds1: // fds1
        value.move< std::vector<Hs::FunDep> > (that.value);
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.move< std::vector<Hs::GuardedRHS> > (that.value);
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_importlist: // importlist
      case symbol_kind::S_importlist1: // importlist1
        value.move< std::vector<Hs::LExport> > (that.value);
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.move< std::vector<Hs::LImpDecl> > (that.value);
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_deriv_types: // deriv_types
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_deriv_clause_types: // deriv_clause_types
        value.move< std::vector<Hs::LType> > (that.value);
        break;

      case symbol_kind::S_sks_vars: // sks_vars
        value.move< std::vector<Hs::LTypeCon> > (that.value);
        break;

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_varids0: // varids0
      case symbol_kind::S_forall: // forall
        value.move< std::vector<Hs::LTypeVar> > (that.value);
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        value.move< std::vector<Hs::LVar> > (that.value);
        break;

      case symbol_kind::S_ty_fam_inst_eqn_list: // ty_fam_inst_eqn_list
      case symbol_kind::S_ty_fam_inst_eqns: // ty_fam_inst_eqns
        value.move< std::vector<Hs::TypeFamilyInstanceEqn> > (that.value);
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Located<Hs::Alt>> > (that.value);
        break;

      case symbol_kind::S_decls: // decls
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
      case symbol_kind::S_ops: // ops
      case symbol_kind::S_con_list: // con_list
        value.move< std::vector<Located<std::string>> > (that.value);
        break;

      case symbol_kind::S_projection: // projection
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

      case symbol_kind::S_deriv_strategy_no_via: // deriv_strategy_no_via
        yylhs.value.emplace< Hs::DerivingStrategy > ();
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        yylhs.value.emplace< Hs::FieldDecl > ();
        break;

      case symbol_kind::S_infix: // infix
        yylhs.value.emplace< Hs::Fixity > ();
        break;

      case symbol_kind::S_fd: // fd
        yylhs.value.emplace< Hs::FunDep > ();
        break;

      case symbol_kind::S_gadt_constr: // gadt_constr
        yylhs.value.emplace< Hs::GADTConstructorDecl > ();
        break;

      case symbol_kind::S_gadt_constrlist: // gadt_constrlist
      case symbol_kind::S_gadt_constrs: // gadt_constrs
        yylhs.value.emplace< Hs::GADTConstructorsDecl > ();
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        yylhs.value.emplace< Hs::GuardedRHS > ();
        break;

      case symbol_kind::S_impspec: // impspec
        yylhs.value.emplace< Hs::ImpSpec > ();
        break;

      case symbol_kind::S_kind: // kind
        yylhs.value.emplace< Hs::Kind > ();
        break;

      case symbol_kind::S_export: // export
      case symbol_kind::S_import: // import
        yylhs.value.emplace< Hs::LExport > ();
        break;

      case symbol_kind::S_importdecl: // importdecl
        yylhs.value.emplace< Hs::LImpDecl > ();
        break;

      case symbol_kind::S_deriv_strategy_via: // deriv_strategy_via
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
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_constr_stuff: // constr_stuff
        yylhs.value.emplace< Hs::LType > ();
        break;

      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_tv_bndr_no_braces: // tv_bndr_no_braces
        yylhs.value.emplace< Hs::LTypeVar > ();
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

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        yylhs.value.emplace< Hs::TypeFamilyInstanceEqn > ();
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

      case symbol_kind::S_fbinds: // fbinds
      case symbol_kind::S_fbinds1: // fbinds1
        yylhs.value.emplace< Located<Hs::FieldBindings> > ();
        break;

      case symbol_kind::S_infixexp: // infixexp
        yylhs.value.emplace< Located<Hs::InfixExp> > ();
        break;

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_stand_alone_deriving: // stand_alone_deriving
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
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
      case symbol_kind::S_call_conv: // call_conv
      case symbol_kind::S_modid: // modid
        yylhs.value.emplace< Located<std::string> > ();
        break;

      case symbol_kind::S_optqualified: // optqualified
        yylhs.value.emplace< bool > ();
        break;

      case symbol_kind::S_CHAR: // "CHAR"
      case symbol_kind::S_PRIMCHAR: // "PRIMCHAR"
        yylhs.value.emplace< char > ();
        break;

      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        yylhs.value.emplace< double > ();
        break;

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

      case symbol_kind::S_RATIONAL: // "RATIONAL"
        yylhs.value.emplace< rational > ();
        break;

      case symbol_kind::S_export_subspec: // export_subspec
        yylhs.value.emplace< std::optional<Hs::ExportSubSpec> > ();
        break;

      case symbol_kind::S_maybeimpspec: // maybeimpspec
        yylhs.value.emplace< std::optional<Hs::ImpSpec> > ();
        break;

      case symbol_kind::S_opt_kind_sig: // opt_kind_sig
        yylhs.value.emplace< std::optional<Hs::Kind> > ();
        break;

      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
        yylhs.value.emplace< std::optional<Hs::LType> > ();
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        yylhs.value.emplace< std::optional<Located<Hs::Binds>> > ();
        break;

      case symbol_kind::S_where_cls: // where_cls
      case symbol_kind::S_where_inst: // where_inst
        yylhs.value.emplace< std::optional<Located<Hs::Decls>> > ();
        break;

      case symbol_kind::S_fbind: // fbind
        yylhs.value.emplace< std::optional<Located<Hs::FieldBinding>> > ();
        break;

      case symbol_kind::S_opt_datafam_kind_sig: // opt_datafam_kind_sig
      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        yylhs.value.emplace< std::optional<Located<Hs::Kind>> > ();
        break;

      case symbol_kind::S_maybeas: // maybeas
      case symbol_kind::S_opt_class: // opt_class
        yylhs.value.emplace< std::optional<Located<std::string>> > ();
        break;

      case symbol_kind::S_prec: // prec
        yylhs.value.emplace< std::optional<int> > ();
        break;

      case symbol_kind::S_overlap_pragma: // overlap_pragma
        yylhs.value.emplace< std::optional<std::string> > ();
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        yylhs.value.emplace< std::optional<std::vector<Hs::LExport>> > ();
        break;

      case symbol_kind::S_where_type_family: // where_type_family
        yylhs.value.emplace< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ();
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        yylhs.value.emplace< std::pair<Hs::Context,Hs::LType> > ();
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        yylhs.value.emplace< std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();
        break;

      case symbol_kind::S_INLINE_PRAG: // "{-# INLINE"
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
      case symbol_kind::S_tyop: // tyop
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
      case symbol_kind::S_field: // field
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
        yylhs.value.emplace< std::string > ();
        break;

      case symbol_kind::S_datafam_inst_hdr: // datafam_inst_hdr
        yylhs.value.emplace< std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
        break;

      case symbol_kind::S_maybe_derivings: // maybe_derivings
      case symbol_kind::S_derivings: // derivings
      case symbol_kind::S_deriving: // deriving
        yylhs.value.emplace< std::vector<Hs::Deriving> > ();
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        yylhs.value.emplace< std::vector<Hs::FieldDecl> > ();
        break;

      case symbol_kind::S_fds: // fds
      case symbol_kind::S_fds1: // fds1
        yylhs.value.emplace< std::vector<Hs::FunDep> > ();
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        yylhs.value.emplace< std::vector<Hs::GuardedRHS> > ();
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_importlist: // importlist
      case symbol_kind::S_importlist1: // importlist1
        yylhs.value.emplace< std::vector<Hs::LExport> > ();
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        yylhs.value.emplace< std::vector<Hs::LImpDecl> > ();
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_deriv_types: // deriv_types
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_deriv_clause_types: // deriv_clause_types
        yylhs.value.emplace< std::vector<Hs::LType> > ();
        break;

      case symbol_kind::S_sks_vars: // sks_vars
        yylhs.value.emplace< std::vector<Hs::LTypeCon> > ();
        break;

      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_varids0: // varids0
      case symbol_kind::S_forall: // forall
        yylhs.value.emplace< std::vector<Hs::LTypeVar> > ();
        break;

      case symbol_kind::S_sig_vars: // sig_vars
        yylhs.value.emplace< std::vector<Hs::LVar> > ();
        break;

      case symbol_kind::S_ty_fam_inst_eqn_list: // ty_fam_inst_eqn_list
      case symbol_kind::S_ty_fam_inst_eqns: // ty_fam_inst_eqns
        yylhs.value.emplace< std::vector<Hs::TypeFamilyInstanceEqn> > ();
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        yylhs.value.emplace< std::vector<Located<Hs::Alt>> > ();
        break;

      case symbol_kind::S_decls: // decls
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
      case symbol_kind::S_ops: // ops
      case symbol_kind::S_con_list: // con_list
        yylhs.value.emplace< std::vector<Located<std::string>> > ();
        break;

      case symbol_kind::S_projection: // projection
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
#line 519 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2676 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 536 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < Located<std::string> > (),yystack_[2].value.as < std::optional<std::vector<Hs::LExport>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2682 "parser.cc"
    break;

  case 4: // module: body2
#line 537 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{{noloc,"Main"},{},yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2688 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 539 "parser.y"
                                                                 {drv.push_module_context();}
#line 2694 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 547 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2700 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 548 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2706 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 550 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2712 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 551 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2718 "parser.cc"
    break;

  case 13: // top: semis top1
#line 554 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2724 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 556 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2730 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 557 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2736 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 558 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::LImpDecl> > (),{});}
#line 2742 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 566 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::LExport>> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2748 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 567 "parser.y"
                                      {}
#line 2754 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 569 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2760 "parser.cc"
    break;

  case 20: // exportlist: %empty
#line 570 "parser.y"
                                      {}
#line 2766 "parser.cc"
    break;

  case 21: // exportlist: exportlist1 ','
#line 571 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2772 "parser.cc"
    break;

  case 22: // exportlist: ','
#line 572 "parser.y"
                                      {}
#line 2778 "parser.cc"
    break;

  case 23: // exportlist1: exportlist1 "," export
#line 574 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2784 "parser.cc"
    break;

  case 24: // exportlist1: export
#line 575 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2790 "parser.cc"
    break;

  case 25: // export: qcname export_subspec
#line 577 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2796 "parser.cc"
    break;

  case 26: // export: "module" modid
#line 578 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2802 "parser.cc"
    break;

  case 27: // export: "default" modid
#line 579 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::default_}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2808 "parser.cc"
    break;

  case 28: // export_subspec: %empty
#line 581 "parser.y"
                                      {}
#line 2814 "parser.cc"
    break;

  case 29: // export_subspec: "(" qcnames ")"
#line 582 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2820 "parser.cc"
    break;

  case 30: // export_subspec: "(" ".." ")"
#line 583 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec(); }
#line 2826 "parser.cc"
    break;

  case 31: // qcnames: %empty
#line 585 "parser.y"
                   {}
#line 2832 "parser.cc"
    break;

  case 32: // qcnames: qcnames1
#line 586 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2838 "parser.cc"
    break;

  case 33: // qcnames1: qcnames1 "," qcname
#line 588 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2844 "parser.cc"
    break;

  case 34: // qcnames1: qcname
#line 589 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2850 "parser.cc"
    break;

  case 35: // qcname: qvar
#line 591 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2856 "parser.cc"
    break;

  case 36: // qcname: oqtycon_no_varcon
#line 592 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2862 "parser.cc"
    break;

  case 41: // importdecls: importdecls_semi importdecl
#line 602 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[1].value.as < std::vector<Hs::LImpDecl> > (), yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[0].value.as < Hs::LImpDecl > ()); }
#line 2868 "parser.cc"
    break;

  case 42: // importdecls_semi: importdecls_semi importdecl semis1
#line 604 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[2].value.as < std::vector<Hs::LImpDecl> > (); yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[1].value.as < Hs::LImpDecl > ()); }
#line 2874 "parser.cc"
    break;

  case 43: // importdecls_semi: %empty
#line 605 "parser.y"
                         { }
#line 2880 "parser.cc"
    break;

  case 44: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 607 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::LImpDecl > () = {yylhs.location, Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < Located<std::string> > (),yystack_[1].value.as < std::optional<Located<std::string>> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ())};
}
#line 2888 "parser.cc"
    break;

  case 45: // optqualified: "qualified"
#line 620 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2894 "parser.cc"
    break;

  case 46: // optqualified: %empty
#line 621 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2900 "parser.cc"
    break;

  case 47: // maybeas: "as" modid
#line 623 "parser.y"
                               { yylhs.value.as < std::optional<Located<std::string>> > () = yystack_[0].value.as < Located<std::string> > (); }
#line 2906 "parser.cc"
    break;

  case 48: // maybeas: %empty
#line 624 "parser.y"
                               { }
#line 2912 "parser.cc"
    break;

  case 49: // maybeimpspec: impspec
#line 626 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2918 "parser.cc"
    break;

  case 50: // maybeimpspec: %empty
#line 627 "parser.y"
                               { }
#line 2924 "parser.cc"
    break;

  case 51: // impspec: "(" importlist ")"
#line 631 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2930 "parser.cc"
    break;

  case 52: // impspec: "hiding" "(" importlist ")"
#line 632 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2936 "parser.cc"
    break;

  case 53: // importlist: importlist1
#line 634 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2942 "parser.cc"
    break;

  case 54: // importlist: %empty
#line 635 "parser.y"
                                      {}
#line 2948 "parser.cc"
    break;

  case 55: // importlist: importlist1 ','
#line 636 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2954 "parser.cc"
    break;

  case 56: // importlist: ','
#line 637 "parser.y"
                                      {}
#line 2960 "parser.cc"
    break;

  case 57: // importlist1: importlist1 "," import
#line 639 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2966 "parser.cc"
    break;

  case 58: // importlist1: import
#line 640 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2972 "parser.cc"
    break;

  case 59: // import: qcname export_subspec
#line 642 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2978 "parser.cc"
    break;

  case 60: // import: "module" modid
#line 643 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2984 "parser.cc"
    break;

  case 61: // prec: %empty
#line 648 "parser.y"
                   { }
#line 2990 "parser.cc"
    break;

  case 62: // prec: "INTEGER"
#line 649 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < integer > ().convert_to<int>(); }
#line 2996 "parser.cc"
    break;

  case 63: // infix: "infix"
#line 651 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 3002 "parser.cc"
    break;

  case 64: // infix: "infixl"
#line 652 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 3008 "parser.cc"
    break;

  case 65: // infix: "infixr"
#line 653 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 3014 "parser.cc"
    break;

  case 66: // ops: ops "," op
#line 655 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3020 "parser.cc"
    break;

  case 67: // ops: op
#line 656 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = {{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 3026 "parser.cc"
    break;

  case 68: // topdecls: topdecls_semi topdecl
#line 660 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ()); }
#line 3032 "parser.cc"
    break;

  case 69: // topdecls_semi: topdecls_semi topdecl semis1
#line 662 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 3038 "parser.cc"
    break;

  case 70: // topdecls_semi: %empty
#line 663 "parser.y"
                                            { }
#line 3044 "parser.cc"
    break;

  case 71: // topdecl: cl_decl
#line 665 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3050 "parser.cc"
    break;

  case 72: // topdecl: ty_decl
#line 666 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3056 "parser.cc"
    break;

  case 73: // topdecl: standalone_kind_sig
#line 667 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3062 "parser.cc"
    break;

  case 74: // topdecl: inst_decl
#line 668 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3068 "parser.cc"
    break;

  case 75: // topdecl: stand_alone_deriving
#line 669 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3074 "parser.cc"
    break;

  case 76: // topdecl: "default" opt_class "(" comma_types0 ")"
#line 671 "parser.y"
                                                         {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::DefaultDecl(yystack_[3].value.as < std::optional<Located<std::string>> > (),yystack_[1].value.as < std::vector<Hs::LType> > ())}; }
#line 3080 "parser.cc"
    break;

  case 77: // topdecl: "foreign" "import" call_conv "STRING" var "::" sigtypedoc
#line 672 "parser.y"
                                                                  {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ForeignDecl(yystack_[4].value.as < Located<std::string> > (), yystack_[3].value.as < std::string > (), {yystack_[2].location,yystack_[2].value.as < std::string > ()}, yystack_[0].value.as < Hs::LType > ())};}
#line 3086 "parser.cc"
    break;

  case 78: // topdecl: decl_no_th
#line 678 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3092 "parser.cc"
    break;

  case 79: // topdecl: infixexp
#line 680 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<Hs::InfixExp> > ();}
#line 3098 "parser.cc"
    break;

  case 80: // call_conv: "bpcall"
#line 683 "parser.y"
                    {yylhs.value.as < Located<std::string> > () = {yylhs.location,"bpcall"};}
#line 3104 "parser.cc"
    break;

  case 81: // call_conv: "trcall"
#line 684 "parser.y"
                    {yylhs.value.as < Located<std::string> > () = {yylhs.location,"trcall"};}
#line 3110 "parser.cc"
    break;

  case 82: // call_conv: "ecall"
#line 685 "parser.y"
                    {yylhs.value.as < Located<std::string> > () = {yylhs.location,"ecall"};}
#line 3116 "parser.cc"
    break;

  case 83: // cl_decl: "class" tycl_hdr fds where_cls
#line 687 "parser.y"
                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_class_decl(yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first,yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,yystack_[1].value.as < std::vector<Hs::FunDep> > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 3122 "parser.cc"
    break;

  case 84: // ty_decl: "type" type "=" ktype
#line 690 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_type_synonym(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3128 "parser.cc"
    break;

  case 85: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 691 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > (),yystack_[0].value.as < std::vector<Hs::Deriving> > ())};}
#line 3134 "parser.cc"
    break;

  case 86: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 693 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ())};}
#line 3140 "parser.cc"
    break;

  case 87: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 694 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[3].value.as < Hs::LType > (), yystack_[2].value.as < std::optional<Located<Hs::Kind>> > (), yystack_[0].value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ())};}
#line 3146 "parser.cc"
    break;

  case 88: // ty_decl: "data" "family" type opt_datafam_kind_sig
#line 695 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})};}
#line 3152 "parser.cc"
    break;

  case 89: // standalone_kind_sig: "type" sks_vars "::" kind
#line 697 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::KindSigDecl(yystack_[2].value.as < std::vector<Hs::LTypeCon> > (),yystack_[0].value.as < Hs::Kind > ())};}
#line 3158 "parser.cc"
    break;

  case 90: // sks_vars: sks_vars "," oqtycon
#line 699 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = yystack_[2].value.as < std::vector<Hs::LTypeCon> > (); yylhs.value.as < std::vector<Hs::LTypeCon> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}); }
#line 3164 "parser.cc"
    break;

  case 91: // sks_vars: oqtycon
#line 700 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}}; }
#line 3170 "parser.cc"
    break;

  case 92: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 703 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_instance_decl(yystack_[2].value.as < std::optional<std::string> > (),yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 3176 "parser.cc"
    break;

  case 93: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 704 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};}
#line 3182 "parser.cc"
    break;

  case 94: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 706 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(tvs, con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), context ? unloc(*context) : Hs::Context(), {}, yystack_[1].value.as < Hs::ConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ()))};
	   }
#line 3192 "parser.cc"
    break;

  case 95: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 712 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(tvs, con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), context ? unloc(*context) : Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ()))};
	   }
#line 3202 "parser.cc"
    break;

  case 96: // overlap_pragma: "{-# OVERLAPPABLE" "#-}"
#line 718 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPABLE"; }
#line 3208 "parser.cc"
    break;

  case 97: // overlap_pragma: "{-# OVERLAPPING" "#-}"
#line 719 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPING"; }
#line 3214 "parser.cc"
    break;

  case 98: // overlap_pragma: "{-# OVERLAPS" "#-}"
#line 720 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPS"; }
#line 3220 "parser.cc"
    break;

  case 99: // overlap_pragma: "{-# INCOHERENT" "#-}"
#line 721 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "INCOHERENT"; }
#line 3226 "parser.cc"
    break;

  case 100: // overlap_pragma: %empty
#line 722 "parser.y"
                                               {}
#line 3232 "parser.cc"
    break;

  case 101: // deriv_strategy_no_via: "stock"
#line 724 "parser.y"
                                    {yylhs.value.as < Hs::DerivingStrategy > () = Hs::DerivingStrategy::stock;}
#line 3238 "parser.cc"
    break;

  case 102: // deriv_strategy_no_via: "anyclass"
#line 725 "parser.y"
                                    {yylhs.value.as < Hs::DerivingStrategy > () = Hs::DerivingStrategy::anyclass;}
#line 3244 "parser.cc"
    break;

  case 103: // deriv_strategy_no_via: "newtype"
#line 726 "parser.y"
                                    {yylhs.value.as < Hs::DerivingStrategy > () = Hs::DerivingStrategy::newtype;}
#line 3250 "parser.cc"
    break;

  case 104: // deriv_strategy_via: "via" type
#line 728 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3256 "parser.cc"
    break;

  case 105: // stand_alone_deriving: "deriving" "instance" inst_type
#line 731 "parser.y"
                             {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StandaloneDerivingDecl({}, yystack_[0].value.as < Hs::LType > ())};}
#line 3262 "parser.cc"
    break;

  case 106: // stand_alone_deriving: "deriving" deriv_strategy_no_via "instance" inst_type
#line 733 "parser.y"
                             {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StandaloneDerivingDecl(yystack_[2].value.as < Hs::DerivingStrategy > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3268 "parser.cc"
    break;

  case 107: // stand_alone_deriving: "deriving" deriv_strategy_via "instance" inst_type
#line 735 "parser.y"
                             {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StandaloneDerivingDecl(Hs::DerivingStrategy::via, yystack_[0].value.as < Hs::LType > (), yystack_[2].value.as < Hs::LType > ())};}
#line 3274 "parser.cc"
    break;

  case 113: // where_type_family: %empty
#line 749 "parser.y"
                                                           {}
#line 3280 "parser.cc"
    break;

  case 114: // where_type_family: "where" ty_fam_inst_eqn_list
#line 750 "parser.y"
                                                           {yylhs.value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > () = yystack_[0].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3286 "parser.cc"
    break;

  case 115: // ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
#line 752 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3292 "parser.cc"
    break;

  case 116: // ty_fam_inst_eqn_list: "vocurly" ty_fam_inst_eqns close
#line 753 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3298 "parser.cc"
    break;

  case 117: // ty_fam_inst_eqn_list: "{" ".." "}"
#line 754 "parser.y"
                                                           {}
#line 3304 "parser.cc"
    break;

  case 118: // ty_fam_inst_eqn_list: "vocurly" ".." close
#line 755 "parser.y"
                                                           {}
#line 3310 "parser.cc"
    break;

  case 119: // ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
#line 757 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[2].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > (); yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ().push_back(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 3316 "parser.cc"
    break;

  case 120: // ty_fam_inst_eqns: ty_fam_inst_eqns ";"
#line 758 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3322 "parser.cc"
    break;

  case 121: // ty_fam_inst_eqns: ty_fam_inst_eqn
#line 759 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = {yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ()};}
#line 3328 "parser.cc"
    break;

  case 122: // ty_fam_inst_eqns: %empty
#line 760 "parser.y"
                                                           {}
#line 3334 "parser.cc"
    break;

  case 123: // ty_fam_inst_eqn: type "=" ctype
#line 762 "parser.y"
                                                           {yylhs.value.as < Hs::TypeFamilyInstanceEqn > () = make_type_family_instance_eqn(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ());}
#line 3340 "parser.cc"
    break;

  case 124: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 765 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3346 "parser.cc"
    break;

  case 125: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 767 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3352 "parser.cc"
    break;

  case 126: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 769 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3358 "parser.cc"
    break;

  case 127: // at_decl_cls: "type" ty_fam_inst_eqn
#line 771 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3364 "parser.cc"
    break;

  case 128: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 772 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3370 "parser.cc"
    break;

  case 133: // at_decl_inst: "type" opt_instance ty_fam_inst_eqn
#line 780 "parser.y"
                                                              { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3376 "parser.cc"
    break;

  case 134: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 783 "parser.y"
              {
		  auto& [tvs, context, type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(tvs, con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), context ? unloc(*context) : Hs::Context(), {}, yystack_[1].value.as < Hs::ConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ()))};
	      }
#line 3386 "parser.cc"
    break;

  case 135: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 790 "parser.y"
              {
		  auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(tvs, con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), context ? unloc(*context) : Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ()))};
	      }
#line 3396 "parser.cc"
    break;

  case 136: // data_or_newtype: "data"
#line 796 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 3402 "parser.cc"
    break;

  case 137: // data_or_newtype: "newtype"
#line 797 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 3408 "parser.cc"
    break;

  case 138: // opt_class: %empty
#line 800 "parser.y"
                  {yylhs.value.as < std::optional<Located<std::string>> > () = {};}
#line 3414 "parser.cc"
    break;

  case 139: // opt_class: qtycon
#line 801 "parser.y"
                  {yylhs.value.as < std::optional<Located<std::string>> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()};}
#line 3420 "parser.cc"
    break;

  case 140: // opt_kind_sig: %empty
#line 805 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 3426 "parser.cc"
    break;

  case 141: // opt_kind_sig: "::" kind
#line 806 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < Hs::Kind > ();}
#line 3432 "parser.cc"
    break;

  case 142: // opt_datafam_kind_sig: %empty
#line 808 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {};}
#line 3438 "parser.cc"
    break;

  case 143: // opt_datafam_kind_sig: "::" kind
#line 809 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3444 "parser.cc"
    break;

  case 144: // opt_tyfam_kind_sig: %empty
#line 811 "parser.y"
                                      {}
#line 3450 "parser.cc"
    break;

  case 145: // opt_tyfam_kind_sig: "::" kind
#line 812 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3456 "parser.cc"
    break;

  case 146: // opt_tyfam_kind_sig: "=" tv_bndr
#line 813 "parser.y"
                                      {}
#line 3462 "parser.cc"
    break;

  case 147: // opt_at_kind_inj_sig: %empty
#line 815 "parser.y"
                                      {}
#line 3468 "parser.cc"
    break;

  case 148: // opt_at_kind_inj_sig: "::" kind
#line 816 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3474 "parser.cc"
    break;

  case 149: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 817 "parser.y"
                                                                  {}
#line 3480 "parser.cc"
    break;

  case 150: // tycl_hdr: context "=>" type
#line 820 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ()};}
#line 3486 "parser.cc"
    break;

  case 151: // tycl_hdr: type
#line 821 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {{},yystack_[0].value.as < Hs::LType > ()};}
#line 3492 "parser.cc"
    break;

  case 152: // datafam_inst_hdr: "forall" tv_bndrs "." context "=>" type
#line 824 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[4].value.as < std::vector<Hs::LTypeVar> > ()}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3498 "parser.cc"
    break;

  case 153: // datafam_inst_hdr: "forall" tv_bndrs "=>" type
#line 825 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[2].value.as < std::vector<Hs::LTypeVar> > ()}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3504 "parser.cc"
    break;

  case 154: // datafam_inst_hdr: context "=>" type
#line 826 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3510 "parser.cc"
    break;

  case 155: // datafam_inst_hdr: type
#line 827 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3516 "parser.cc"
    break;

  case 159: // decl_cls: at_decl_cls
#line 873 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3522 "parser.cc"
    break;

  case 160: // decl_cls: decl
#line 874 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3528 "parser.cc"
    break;

  case 161: // decls_cls: decls_cls ";" decl_cls
#line 876 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3534 "parser.cc"
    break;

  case 162: // decls_cls: decls_cls ";"
#line 877 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3540 "parser.cc"
    break;

  case 163: // decls_cls: decl_cls
#line 878 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3546 "parser.cc"
    break;

  case 164: // decls_cls: %empty
#line 879 "parser.y"
                                           {}
#line 3552 "parser.cc"
    break;

  case 165: // decllist_cls: "{" decls_cls "}"
#line 881 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3558 "parser.cc"
    break;

  case 166: // decllist_cls: "vocurly" decls_cls close
#line 882 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3564 "parser.cc"
    break;

  case 167: // where_cls: "where" decllist_cls
#line 884 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3570 "parser.cc"
    break;

  case 168: // where_cls: %empty
#line 885 "parser.y"
                                           {}
#line 3576 "parser.cc"
    break;

  case 169: // decl_inst: at_decl_inst
#line 887 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3582 "parser.cc"
    break;

  case 170: // decl_inst: decl
#line 888 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3588 "parser.cc"
    break;

  case 171: // decls_inst: decls_inst ";" decl_inst
#line 890 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3594 "parser.cc"
    break;

  case 172: // decls_inst: decls_inst ";"
#line 891 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3600 "parser.cc"
    break;

  case 173: // decls_inst: decl_inst
#line 892 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3606 "parser.cc"
    break;

  case 174: // decls_inst: %empty
#line 893 "parser.y"
                                           {}
#line 3612 "parser.cc"
    break;

  case 175: // decllist_inst: "{" decls_inst "}"
#line 895 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3618 "parser.cc"
    break;

  case 176: // decllist_inst: "vocurly" decls_inst close
#line 896 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3624 "parser.cc"
    break;

  case 177: // where_inst: "where" decllist_inst
#line 898 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3630 "parser.cc"
    break;

  case 178: // where_inst: %empty
#line 899 "parser.y"
                                           {}
#line 3636 "parser.cc"
    break;

  case 179: // decls: decls ";" decl
#line 902 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3642 "parser.cc"
    break;

  case 180: // decls: decls ";"
#line 903 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3648 "parser.cc"
    break;

  case 181: // decls: decl
#line 904 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3654 "parser.cc"
    break;

  case 182: // decls: %empty
#line 905 "parser.y"
                        {}
#line 3660 "parser.cc"
    break;

  case 183: // decllist: "{" decls "}"
#line 907 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3666 "parser.cc"
    break;

  case 184: // decllist: "vocurly" decls close
#line 908 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3672 "parser.cc"
    break;

  case 185: // binds: decllist
#line 910 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 3678 "parser.cc"
    break;

  case 186: // wherebinds: "where" binds
#line 912 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 3684 "parser.cc"
    break;

  case 187: // wherebinds: %empty
#line 913 "parser.y"
                                 {}
#line 3690 "parser.cc"
    break;

  case 193: // opt_tyconsig: %empty
#line 939 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {};}
#line 3696 "parser.cc"
    break;

  case 194: // opt_tyconsig: "::" gtycon
#line 940 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}};}
#line 3702 "parser.cc"
    break;

  case 195: // sigtype: ctype
#line 949 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3708 "parser.cc"
    break;

  case 196: // sigtypedoc: ctypedoc
#line 951 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3714 "parser.cc"
    break;

  case 197: // sig_vars: sig_vars "," var
#line 953 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > () = yystack_[2].value.as < std::vector<Hs::LVar> > (); yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3720 "parser.cc"
    break;

  case 198: // sig_vars: var
#line 954 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3726 "parser.cc"
    break;

  case 199: // sigtypes1: sigtype
#line 956 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3732 "parser.cc"
    break;

  case 200: // sigtypes1: sigtypes1 "," sigtype
#line 957 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3738 "parser.cc"
    break;

  case 201: // ktype: ctype
#line 966 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3744 "parser.cc"
    break;

  case 202: // ktype: ctype "::" kind
#line 967 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeOfKind(yystack_[2].value.as < Hs::LType > (), yystack_[0].value.as < Hs::Kind > ())};}
#line 3750 "parser.cc"
    break;

  case 203: // ctype: "forall" tv_bndrs "." ctype
#line 969 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ForallType(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3756 "parser.cc"
    break;

  case 204: // ctype: context "=>" ctype
#line 970 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3762 "parser.cc"
    break;

  case 205: // ctype: type
#line 972 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3768 "parser.cc"
    break;

  case 206: // ctypedoc: ctype
#line 974 "parser.y"
          { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3774 "parser.cc"
    break;

  case 207: // context: btype
#line 983 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::LType > ());}
#line 3780 "parser.cc"
    break;

  case 208: // context_no_ops: btype_no_ops
#line 985 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ()));}
#line 3786 "parser.cc"
    break;

  case 209: // type: btype
#line 987 "parser.y"
      { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3792 "parser.cc"
    break;

  case 210: // type: btype "->" ctype
#line 988 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = Hs::type_apply({{yystack_[1].location,Hs::TypeCon("->")},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3798 "parser.cc"
    break;

  case 211: // typedoc: type
#line 990 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3804 "parser.cc"
    break;

  case 212: // btype: infixtype
#line 993 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3810 "parser.cc"
    break;

  case 213: // infixtype: ftype
#line 995 "parser.y"
           { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3816 "parser.cc"
    break;

  case 214: // infixtype: btype tyop btype
#line 996 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = Hs::type_apply({{yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3822 "parser.cc"
    break;

  case 215: // btype_no_ops: atype_docs
#line 998 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3828 "parser.cc"
    break;

  case 216: // btype_no_ops: btype_no_ops atype_docs
#line 999 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3834 "parser.cc"
    break;

  case 217: // ftype: atype
#line 1001 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3840 "parser.cc"
    break;

  case 218: // ftype: ftype tyarg
#line 1003 "parser.y"
                                   { yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeApp(yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())}; }
#line 3846 "parser.cc"
    break;

  case 219: // ftype: ftype "@" atype
#line 1004 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[2].value.as < Hs::LType > (); }
#line 3852 "parser.cc"
    break;

  case 220: // tyarg: atype
#line 1006 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3858 "parser.cc"
    break;

  case 221: // tyop: qtyconop
#line 1008 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3864 "parser.cc"
    break;

  case 222: // tyop: tyvarop
#line 1009 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3870 "parser.cc"
    break;

  case 223: // atype_docs: atype
#line 1016 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3876 "parser.cc"
    break;

  case 224: // atype: ntgtycon
#line 1023 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon(yystack_[0].value.as < std::string > ())};}
#line 3882 "parser.cc"
    break;

  case 225: // atype: tyvar
#line 1024 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3888 "parser.cc"
    break;

  case 226: // atype: "*"
#line 1025 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("*")};}
#line 3894 "parser.cc"
    break;

  case 227: // atype: PREFIX_BANG atype
#line 1026 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::StrictType(yystack_[0].value.as < Hs::LType > ())};}
#line 3900 "parser.cc"
    break;

  case 228: // atype: PREFIX_TILDE atype
#line 1027 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::LazyType(yystack_[0].value.as < Hs::LType > ())};}
#line 3906 "parser.cc"
    break;

  case 229: // atype: "{" fielddecls "}"
#line 1028 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ())};}
#line 3912 "parser.cc"
    break;

  case 230: // atype: "(" ")"
#line 1029 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("()")};}
#line 3918 "parser.cc"
    break;

  case 231: // atype: "(" comma_types1 "," ktype ")"
#line 1030 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::LType> > ();ts.push_back(yystack_[1].value.as < Hs::LType > ());yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TupleType(ts)};}
#line 3924 "parser.cc"
    break;

  case 232: // atype: "[" ktype "]"
#line 1036 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ListType{yystack_[1].value.as < Hs::LType > ()}}; }
#line 3930 "parser.cc"
    break;

  case 233: // atype: "(" ktype ")"
#line 1037 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[1].value.as < Hs::LType > ();}
#line 3936 "parser.cc"
    break;

  case 234: // inst_type: sigtype
#line 1040 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3942 "parser.cc"
    break;

  case 235: // deriv_types: typedoc
#line 1042 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3948 "parser.cc"
    break;

  case 236: // deriv_types: typedoc "," deriv_types
#line 1043 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().insert(yylhs.value.as < std::vector<Hs::LType> > ().begin(), yystack_[2].value.as < Hs::LType > ());}
#line 3954 "parser.cc"
    break;

  case 237: // comma_types0: comma_types1
#line 1045 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > ();}
#line 3960 "parser.cc"
    break;

  case 238: // comma_types0: %empty
#line 1046 "parser.y"
                                       { /* default construction OK */ }
#line 3966 "parser.cc"
    break;

  case 239: // comma_types1: ktype
#line 1048 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3972 "parser.cc"
    break;

  case 240: // comma_types1: comma_types1 "," ktype
#line 1049 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3978 "parser.cc"
    break;

  case 241: // tv_bndrs: tv_bndrs tv_bndr
#line 1056 "parser.y"
                               {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back(yystack_[0].value.as < Hs::LTypeVar > ());}
#line 3984 "parser.cc"
    break;

  case 242: // tv_bndrs: %empty
#line 1057 "parser.y"
                               { /* default construction OK */}
#line 3990 "parser.cc"
    break;

  case 243: // tv_bndr: tv_bndr_no_braces
#line 1059 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = yystack_[0].value.as < Hs::LTypeVar > ();}
#line 3996 "parser.cc"
    break;

  case 244: // tv_bndr: "{" tyvar "}"
#line 1060 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[1].location,Hs::TypeVar(yystack_[1].value.as < std::string > ())};}
#line 4002 "parser.cc"
    break;

  case 245: // tv_bndr: "{" tyvar "::" kind "}"
#line 1061 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[3].location,Hs::TypeVar(yystack_[3].value.as < std::string > ())};}
#line 4008 "parser.cc"
    break;

  case 246: // tv_bndr_no_braces: tyvar
#line 1064 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 4014 "parser.cc"
    break;

  case 247: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 1065 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[3].value.as < std::string > (),yystack_[1].value.as < Hs::Kind > ())};}
#line 4020 "parser.cc"
    break;

  case 248: // fds: %empty
#line 1069 "parser.y"
                                    { /* default to empty */ }
#line 4026 "parser.cc"
    break;

  case 249: // fds: "|" fds1
#line 1070 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > () = yystack_[0].value.as < std::vector<Hs::FunDep> > (); }
#line 4032 "parser.cc"
    break;

  case 250: // fds1: fds1 "," fd
#line 1072 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > () = yystack_[2].value.as < std::vector<Hs::FunDep> > (); yylhs.value.as < std::vector<Hs::FunDep> > ().push_back(yystack_[0].value.as < Hs::FunDep > ()); }
#line 4038 "parser.cc"
    break;

  case 251: // fds1: fd
#line 1073 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > ().push_back(yystack_[0].value.as < Hs::FunDep > ()); }
#line 4044 "parser.cc"
    break;

  case 252: // fd: varids0 "->" varids0
#line 1076 "parser.y"
                                    { yylhs.value.as < Hs::FunDep > () = Hs::FunDep(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < std::vector<Hs::LTypeVar> > ()); }
#line 4050 "parser.cc"
    break;

  case 253: // varids0: varids0 tyvar
#line 1078 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4056 "parser.cc"
    break;

  case 254: // varids0: %empty
#line 1079 "parser.y"
                                    { /* default to empty */}
#line 4062 "parser.cc"
    break;

  case 255: // kind: ctype
#line 1084 "parser.y"
             {yylhs.value.as < Hs::Kind > () = type_to_kind(yystack_[0].value.as < Hs::LType > ());}
#line 4068 "parser.cc"
    break;

  case 256: // gadt_constrlist: "where" "{" gadt_constrs "}"
#line 1090 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 4074 "parser.cc"
    break;

  case 257: // gadt_constrlist: "where" "vocurly" gadt_constrs close
#line 1091 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 4080 "parser.cc"
    break;

  case 258: // gadt_constrlist: %empty
#line 1092 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = {};}
#line 4086 "parser.cc"
    break;

  case 259: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1094 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 4092 "parser.cc"
    break;

  case 260: // gadt_constrs: gadt_constr
#line 1095 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 4098 "parser.cc"
    break;

  case 261: // gadt_constr: optSemi con_list "::" sigtype
#line 1097 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),yystack_[0].value.as < Hs::LType > ());}
#line 4104 "parser.cc"
    break;

  case 262: // constrs: "=" constrs1
#line 1099 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 4110 "parser.cc"
    break;

  case 263: // constrs1: constrs1 "|" constr
#line 1101 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 4116 "parser.cc"
    break;

  case 264: // constrs1: constr
#line 1102 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 4122 "parser.cc"
    break;

  case 265: // constr: forall context_no_ops "=>" constr_stuff
#line 1104 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::LTypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::LType > ());}
#line 4128 "parser.cc"
    break;

  case 266: // constr: forall constr_stuff
#line 1105 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::LTypeVar> > (),{}, yystack_[0].value.as < Hs::LType > ());}
#line 4134 "parser.cc"
    break;

  case 267: // forall: "forall" tv_bndrs "."
#line 1107 "parser.y"
                                {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > ();}
#line 4140 "parser.cc"
    break;

  case 268: // forall: %empty
#line 1108 "parser.y"
                                {}
#line 4146 "parser.cc"
    break;

  case 269: // constr_stuff: btype_no_ops
#line 1110 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ());}
#line 4152 "parser.cc"
    break;

  case 270: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1111 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::type_apply({
                                                                          {yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},
                                                                          Hs::type_apply(yystack_[2].value.as < std::vector<Hs::LType> > ()),
                                                                          Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ())
                                                                       });}
#line 4162 "parser.cc"
    break;

  case 271: // fielddecls: %empty
#line 1117 "parser.y"
                                {}
#line 4168 "parser.cc"
    break;

  case 272: // fielddecls: fielddecls1
#line 1118 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 4174 "parser.cc"
    break;

  case 273: // fielddecls1: fielddecls1 "," fielddecl
#line 1120 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 4180 "parser.cc"
    break;

  case 274: // fielddecls1: fielddecl
#line 1121 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 4186 "parser.cc"
    break;

  case 275: // fielddecl: sig_vars "::" ctype
#line 1123 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ());}
#line 4192 "parser.cc"
    break;

  case 276: // maybe_derivings: %empty
#line 1125 "parser.y"
                            {}
#line 4198 "parser.cc"
    break;

  case 277: // maybe_derivings: derivings
#line 1126 "parser.y"
                            {yylhs.value.as < std::vector<Hs::Deriving> > () = yystack_[0].value.as < std::vector<Hs::Deriving> > ();}
#line 4204 "parser.cc"
    break;

  case 278: // derivings: derivings deriving
#line 1128 "parser.y"
                                    {yylhs.value.as < std::vector<Hs::Deriving> > () = yystack_[1].value.as < std::vector<Hs::Deriving> > (); yylhs.value.as < std::vector<Hs::Deriving> > ().insert(yylhs.value.as < std::vector<Hs::Deriving> > ().end(), yystack_[0].value.as < std::vector<Hs::Deriving> > ().begin(), yystack_[0].value.as < std::vector<Hs::Deriving> > ().end());}
#line 4210 "parser.cc"
    break;

  case 279: // derivings: deriving
#line 1129 "parser.y"
                                    {yylhs.value.as < std::vector<Hs::Deriving> > () = yystack_[0].value.as < std::vector<Hs::Deriving> > ();}
#line 4216 "parser.cc"
    break;

  case 280: // deriving: "deriving" deriv_clause_types
#line 1132 "parser.y"
          {
              for(auto& type: yystack_[0].value.as < std::vector<Hs::LType> > ())
                  yylhs.value.as < std::vector<Hs::Deriving> > ().push_back(Hs::Deriving({}, type));
          }
#line 4225 "parser.cc"
    break;

  case 281: // deriving: "deriving" deriv_strategy_no_via deriv_clause_types
#line 1137 "parser.y"
          {
              for(auto& type: yystack_[0].value.as < std::vector<Hs::LType> > ())
                  yylhs.value.as < std::vector<Hs::Deriving> > ().push_back(Hs::Deriving(yystack_[1].value.as < Hs::DerivingStrategy > (), type));
          }
#line 4234 "parser.cc"
    break;

  case 282: // deriving: "deriving" deriv_clause_types deriv_strategy_via
#line 1142 "parser.y"
          {
              for(auto& type: yystack_[1].value.as < std::vector<Hs::LType> > ())
                  yylhs.value.as < std::vector<Hs::Deriving> > ().push_back(Hs::Deriving(Hs::DerivingStrategy::via, type, yystack_[0].value.as < Hs::LType > ()));
          }
#line 4243 "parser.cc"
    break;

  case 283: // deriv_clause_types: qtycondoc
#line 1147 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::LType> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())});}
#line 4249 "parser.cc"
    break;

  case 284: // deriv_clause_types: "(" ")"
#line 1148 "parser.y"
                                        {}
#line 4255 "parser.cc"
    break;

  case 285: // deriv_clause_types: "(" deriv_types ")"
#line 1149 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > ();}
#line 4261 "parser.cc"
    break;

  case 286: // decl_no_th: sigdecl
#line 1154 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4267 "parser.cc"
    break;

  case 287: // decl_no_th: infixexp rhs
#line 1156 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ValueDecl({yystack_[1].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ())};}
#line 4273 "parser.cc"
    break;

  case 288: // decl: decl_no_th
#line 1158 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4279 "parser.cc"
    break;

  case 289: // rhs: "=" exp wherebinds
#line 1162 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4285 "parser.cc"
    break;

  case 290: // rhs: gdrhs wherebinds
#line 1163 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 4291 "parser.cc"
    break;

  case 291: // gdrhs: gdrhs gdrh
#line 1165 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4297 "parser.cc"
    break;

  case 292: // gdrhs: gdrh
#line 1166 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4303 "parser.cc"
    break;

  case 293: // gdrh: "|" guardquals "=" exp
#line 1170 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4309 "parser.cc"
    break;

  case 294: // sigdecl: sig_vars "::" sigtypedoc
#line 1180 "parser.y"
                                  { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeSigDecl{yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ()}}; }
#line 4315 "parser.cc"
    break;

  case 295: // sigdecl: infix prec ops
#line 1181 "parser.y"
                         { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<Located<std::string>> > ()}}; }
#line 4321 "parser.cc"
    break;

  case 296: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1183 "parser.y"
                                                    {}
#line 4327 "parser.cc"
    break;

  case 297: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1184 "parser.y"
                                            { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::InlinePragma({yystack_[3].location,yystack_[3].value.as < std::string > ()},{yystack_[1].location,yystack_[1].value.as < std::string > ()})}; }
#line 4333 "parser.cc"
    break;

  case 298: // sigdecl: "{-# SCC" qvar "#-}"
#line 1185 "parser.y"
                              {}
#line 4339 "parser.cc"
    break;

  case 299: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1186 "parser.y"
                                     {}
#line 4345 "parser.cc"
    break;

  case 300: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1187 "parser.y"
                                                               {}
#line 4351 "parser.cc"
    break;

  case 301: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1188 "parser.y"
                                                                      {}
#line 4357 "parser.cc"
    break;

  case 302: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1189 "parser.y"
                                                     {}
#line 4363 "parser.cc"
    break;

  case 307: // exp: infixexp "::" sigtype
#line 1201 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[2].location+yystack_[0].location, Hs::TypedExp({yystack_[2].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[2].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::LType > ())}; }
#line 4369 "parser.cc"
    break;

  case 308: // exp: infixexp
#line 1202 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}; }
#line 4375 "parser.cc"
    break;

  case 309: // infixexp: exp10
#line 1206 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location,Hs::InfixExp({yystack_[0].value.as < Located<expression_ref> > ()})};}
#line 4381 "parser.cc"
    break;

  case 310: // infixexp: infixexp qop exp10
#line 1207 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < expression_ref > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4387 "parser.cc"
    break;

  case 311: // exp10: PREFIX_MINUS fexp
#line 1209 "parser.y"
                                        {yylhs.value.as < Located<expression_ref> > () = {yystack_[1].location+yystack_[0].location,Hs::InfixExp( { {yystack_[1].location,Hs::Neg()}, yystack_[0].value.as < Located<expression_ref> > ()} )};}
#line 4393 "parser.cc"
    break;

  case 312: // exp10: fexp
#line 1210 "parser.y"
                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4399 "parser.cc"
    break;

  case 315: // fexp: fexp aexp
#line 1218 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ApplyExp(yystack_[1].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4405 "parser.cc"
    break;

  case 316: // fexp: fexp "@" atype
#line 1219 "parser.y"
                                 {}
#line 4411 "parser.cc"
    break;

  case 317: // fexp: "static" aexp
#line 1220 "parser.y"
                                 {}
#line 4417 "parser.cc"
    break;

  case 318: // fexp: aexp
#line 1221 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4423 "parser.cc"
    break;

  case 319: // aexp: qvar TIGHT_INFIX_AT aexp
#line 1224 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::AsPattern({yystack_[2].location,Hs::Var(yystack_[2].value.as < std::string > ())},yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4429 "parser.cc"
    break;

  case 320: // aexp: PREFIX_TILDE aexp
#line 1225 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LazyPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4435 "parser.cc"
    break;

  case 321: // aexp: PREFIX_BANG aexp
#line 1226 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4441 "parser.cc"
    break;

  case 322: // aexp: "\\" apats1 "->" exp
#line 1227 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LambdaExp(yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4447 "parser.cc"
    break;

  case 323: // aexp: "let" binds "in" exp
#line 1228 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4453 "parser.cc"
    break;

  case 324: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1230 "parser.y"
                                                       {yylhs.value.as < Located<expression_ref> > () = {yystack_[7].location+yystack_[0].location,Hs::IfExp(yystack_[6].value.as < Located<expression_ref> > (),yystack_[3].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4459 "parser.cc"
    break;

  case 325: // aexp: "case" exp "of" altslist
#line 1232 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::CaseExp(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::Alts > ())}; }
#line 4465 "parser.cc"
    break;

  case 326: // aexp: "do" stmtlist
#line 1233 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Do(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4471 "parser.cc"
    break;

  case 327: // aexp: "mdo" stmtlist
#line 1234 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::MDo(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4477 "parser.cc"
    break;

  case 328: // aexp: aexp1
#line 1236 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4483 "parser.cc"
    break;

  case 329: // aexp1: aexp1 "{" fbinds "}"
#line 1239 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::RecordExp{yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<Hs::FieldBindings> > ()}}; }
#line 4489 "parser.cc"
    break;

  case 330: // aexp1: aexp1 TIGHT_INFIX_DOT field
#line 1240 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = make_record_field_selection(yylhs.location, yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < std::string > ()); }
#line 4495 "parser.cc"
    break;

  case 331: // aexp1: aexp2
#line 1241 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > (); }
#line 4501 "parser.cc"
    break;

  case 332: // aexp2: qvar
#line 1244 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Var(yystack_[0].value.as < std::string > ())};}
#line 4507 "parser.cc"
    break;

  case 333: // aexp2: qcon
#line 1245 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Con(yystack_[0].value.as < std::string > ())};}
#line 4513 "parser.cc"
    break;

  case 334: // aexp2: literal
#line 1246 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[0].value.as < expression_ref > ()};}
#line 4519 "parser.cc"
    break;

  case 335: // aexp2: "(" texp ")"
#line 1247 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, unloc(yystack_[1].value.as < Located<expression_ref> > ())};}
#line 4525 "parser.cc"
    break;

  case 336: // aexp2: "(" tup_exprs ")"
#line 1248 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Tuple(yystack_[1].value.as < std::vector<Located<expression_ref>> > ())};}
#line 4531 "parser.cc"
    break;

  case 337: // aexp2: "(" projection ")"
#line 1249 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = make_record_projection(yylhs.location, yystack_[1].value.as < std::vector<std::string> > ());}
#line 4537 "parser.cc"
    break;

  case 338: // aexp2: "[" list "]"
#line 1254 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[1].value.as < expression_ref > ()};}
#line 4543 "parser.cc"
    break;

  case 339: // aexp2: "_"
#line 1255 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::WildcardPattern()};}
#line 4549 "parser.cc"
    break;

  case 340: // projection: projection TIGHT_INFIX_DOT field
#line 1258 "parser.y"
                                              {yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 4555 "parser.cc"
    break;

  case 341: // projection: PREFIX_DOT field
#line 1259 "parser.y"
                                              {yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 4561 "parser.cc"
    break;

  case 342: // texp: exp
#line 1264 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4567 "parser.cc"
    break;

  case 343: // texp: infixexp qop
#line 1265 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LeftSection ( {yystack_[1].value.as < Located<Hs::InfixExp> > ().loc, unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())}, {yystack_[0].location,yystack_[0].value.as < expression_ref > ()} )}; }
#line 4573 "parser.cc"
    break;

  case 344: // texp: qopm infixexp
#line 1266 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RightSection( {yystack_[1].location,yystack_[1].value.as < expression_ref > ()}, {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())} )}; }
#line 4579 "parser.cc"
    break;

  case 345: // tup_exprs: tup_exprs "," texp
#line 1271 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4585 "parser.cc"
    break;

  case 346: // tup_exprs: texp "," texp
#line 1272 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4591 "parser.cc"
    break;

  case 347: // list: texp
#line 1290 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List({yystack_[0].value.as < Located<expression_ref> > ()}); }
#line 4597 "parser.cc"
    break;

  case 348: // list: lexps
#line 1291 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List(yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4603 "parser.cc"
    break;

  case 349: // list: texp ".."
#line 1292 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4609 "parser.cc"
    break;

  case 350: // list: texp "," exp ".."
#line 1293 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4615 "parser.cc"
    break;

  case 351: // list: texp ".." exp
#line 1294 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4621 "parser.cc"
    break;

  case 352: // list: texp "," exp ".." exp
#line 1295 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < Located<expression_ref> > (), yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4627 "parser.cc"
    break;

  case 353: // list: texp "|" squals
#line 1296 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4633 "parser.cc"
    break;

  case 354: // lexps: lexps "," texp
#line 1298 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4639 "parser.cc"
    break;

  case 355: // lexps: texp "," texp
#line 1299 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4645 "parser.cc"
    break;

  case 356: // squals: squals "," qual
#line 1312 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4651 "parser.cc"
    break;

  case 357: // squals: qual
#line 1314 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4657 "parser.cc"
    break;

  case 358: // guardquals: guardquals1
#line 1324 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[0].value.as < std::vector<Located<expression_ref>> > ();}
#line 4663 "parser.cc"
    break;

  case 359: // guardquals1: guardquals1 "," qual
#line 1326 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > ();yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4669 "parser.cc"
    break;

  case 360: // guardquals1: qual
#line 1327 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4675 "parser.cc"
    break;

  case 361: // altslist: "{" alts "}"
#line 1330 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4681 "parser.cc"
    break;

  case 362: // altslist: "vocurly" alts close
#line 1331 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4687 "parser.cc"
    break;

  case 363: // altslist: "{" "}"
#line 1332 "parser.y"
                                 {}
#line 4693 "parser.cc"
    break;

  case 364: // altslist: "vocurly" close
#line 1333 "parser.y"
                                 {}
#line 4699 "parser.cc"
    break;

  case 365: // alts: alts1
#line 1335 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4705 "parser.cc"
    break;

  case 366: // alts: ";" alts
#line 1336 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4711 "parser.cc"
    break;

  case 367: // alts1: alts1 ";" alt
#line 1338 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4717 "parser.cc"
    break;

  case 368: // alts1: alts1 ";"
#line 1339 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4723 "parser.cc"
    break;

  case 369: // alts1: alt
#line 1340 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4729 "parser.cc"
    break;

  case 370: // alt: pat alt_rhs
#line 1342 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 4735 "parser.cc"
    break;

  case 371: // alt_rhs: "->" exp wherebinds
#line 1344 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4741 "parser.cc"
    break;

  case 372: // alt_rhs: gdpats wherebinds
#line 1345 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4747 "parser.cc"
    break;

  case 373: // gdpats: gdpats gdpat
#line 1347 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4753 "parser.cc"
    break;

  case 374: // gdpats: gdpat
#line 1348 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4759 "parser.cc"
    break;

  case 375: // gdpat: "|" guardquals "->" exp
#line 1357 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4765 "parser.cc"
    break;

  case 376: // pat: exp
#line 1359 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4771 "parser.cc"
    break;

  case 377: // bindpat: exp
#line 1361 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4777 "parser.cc"
    break;

  case 378: // apat: aexp
#line 1363 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4783 "parser.cc"
    break;

  case 379: // apats1: apats1 apat
#line 1365 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4789 "parser.cc"
    break;

  case 380: // apats1: apat
#line 1366 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4795 "parser.cc"
    break;

  case 381: // stmtlist: "{" stmts "}"
#line 1369 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4801 "parser.cc"
    break;

  case 382: // stmtlist: "vocurly" stmts close
#line 1370 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4807 "parser.cc"
    break;

  case 383: // stmts: stmts ";" stmt
#line 1372 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4813 "parser.cc"
    break;

  case 384: // stmts: stmts ";"
#line 1373 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 4819 "parser.cc"
    break;

  case 385: // stmts: stmt
#line 1374 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4825 "parser.cc"
    break;

  case 386: // stmts: %empty
#line 1375 "parser.y"
                       {}
#line 4831 "parser.cc"
    break;

  case 387: // stmt: qual
#line 1380 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4837 "parser.cc"
    break;

  case 388: // stmt: "rec" stmtlist
#line 1381 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ())};}
#line 4843 "parser.cc"
    break;

  case 389: // qual: bindpat "<-" exp
#line 1383 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::PatQual(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4849 "parser.cc"
    break;

  case 390: // qual: exp
#line 1384 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::SimpleQual(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4855 "parser.cc"
    break;

  case 391: // qual: "let" binds
#line 1385 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ())};}
#line 4861 "parser.cc"
    break;

  case 392: // fbinds: fbinds1
#line 1390 "parser.y"
                        {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > ();}
#line 4867 "parser.cc"
    break;

  case 393: // fbinds: %empty
#line 1391 "parser.y"
                        {}
#line 4873 "parser.cc"
    break;

  case 394: // fbinds1: fbind "," fbinds1
#line 1393 "parser.y"
                            {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > (); unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).insert(unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).begin(), *yystack_[2].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4879 "parser.cc"
    break;

  case 395: // fbinds1: fbind
#line 1394 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).push_back(*yystack_[0].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4885 "parser.cc"
    break;

  case 396: // fbinds1: ".."
#line 1395 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).dotdot = true; yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4891 "parser.cc"
    break;

  case 397: // fbind: qvar "=" texp
#line 1397 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[2].value.as < std::string > ())}, yystack_[0].value.as < Located<expression_ref> > ())}};}
#line 4897 "parser.cc"
    break;

  case 398: // fbind: qvar
#line 1398 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[0].value.as < std::string > ())})}};}
#line 4903 "parser.cc"
    break;

  case 399: // fbind: field TIGHT_INFIX_DOT fieldToUpdate "=" texp
#line 1399 "parser.y"
                                                      {}
#line 4909 "parser.cc"
    break;

  case 400: // fbind: field TIGHT_INFIX_DOT fieldToUpdate
#line 1400 "parser.y"
                                                      {}
#line 4915 "parser.cc"
    break;

  case 403: // qcon: gen_qcon
#line 1436 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4921 "parser.cc"
    break;

  case 404: // qcon: sysdcon
#line 1437 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4927 "parser.cc"
    break;

  case 405: // gen_qcon: qconid
#line 1439 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4933 "parser.cc"
    break;

  case 406: // gen_qcon: "(" qconsym ")"
#line 1440 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4939 "parser.cc"
    break;

  case 407: // con: conid
#line 1442 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4945 "parser.cc"
    break;

  case 408: // con: "(" consym ")"
#line 1443 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4951 "parser.cc"
    break;

  case 409: // con: sysdcon
#line 1444 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4957 "parser.cc"
    break;

  case 410: // con_list: con_list "," con
#line 1446 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4963 "parser.cc"
    break;

  case 411: // con_list: con
#line 1447 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4969 "parser.cc"
    break;

  case 412: // sysdcon_no_list: "(" ")"
#line 1449 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 4975 "parser.cc"
    break;

  case 413: // sysdcon_no_list: "(" commas ")"
#line 1450 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4981 "parser.cc"
    break;

  case 414: // sysdcon_no_list: "(#" "#)"
#line 1451 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 4987 "parser.cc"
    break;

  case 415: // sysdcon_no_list: "(#" commas "#)"
#line 1452 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4993 "parser.cc"
    break;

  case 416: // sysdcon: sysdcon_no_list
#line 1454 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4999 "parser.cc"
    break;

  case 417: // sysdcon: "[" "]"
#line 1455 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 5005 "parser.cc"
    break;

  case 418: // conop: consym
#line 1457 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5011 "parser.cc"
    break;

  case 419: // conop: "`" conid "`"
#line 1458 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5017 "parser.cc"
    break;

  case 420: // qconop: qconsym
#line 1460 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5023 "parser.cc"
    break;

  case 421: // qconop: "`" qconid "`"
#line 1461 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5029 "parser.cc"
    break;

  case 422: // gtycon: ntgtycon
#line 1464 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5035 "parser.cc"
    break;

  case 423: // gtycon: "(" ")"
#line 1465 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 5041 "parser.cc"
    break;

  case 424: // gtycon: "(#" "#)"
#line 1466 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 5047 "parser.cc"
    break;

  case 425: // ntgtycon: oqtycon
#line 1468 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5053 "parser.cc"
    break;

  case 426: // ntgtycon: "(" commas ")"
#line 1469 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 5059 "parser.cc"
    break;

  case 427: // ntgtycon: "(#" commas "#)"
#line 1470 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 5065 "parser.cc"
    break;

  case 428: // ntgtycon: "(" "->" ")"
#line 1471 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 5071 "parser.cc"
    break;

  case 429: // ntgtycon: "[" "]"
#line 1472 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 5077 "parser.cc"
    break;

  case 430: // oqtycon: qtycon
#line 1474 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5083 "parser.cc"
    break;

  case 431: // oqtycon: "(" qtyconsym ")"
#line 1475 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5089 "parser.cc"
    break;

  case 432: // oqtycon_no_varcon: qtycon
#line 1477 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5095 "parser.cc"
    break;

  case 433: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1478 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5101 "parser.cc"
    break;

  case 434: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1479 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5107 "parser.cc"
    break;

  case 435: // oqtycon_no_varcon: "(" ":" ")"
#line 1480 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 5113 "parser.cc"
    break;

  case 436: // qtyconop: qtyconsym
#line 1483 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5119 "parser.cc"
    break;

  case 437: // qtyconop: "`" qtycon "`"
#line 1484 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5125 "parser.cc"
    break;

  case 438: // qtycondoc: qtycon
#line 1486 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5131 "parser.cc"
    break;

  case 439: // qtycon: "QCONID"
#line 1488 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5137 "parser.cc"
    break;

  case 440: // qtycon: tycon
#line 1489 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5143 "parser.cc"
    break;

  case 441: // tycon: "CONID"
#line 1493 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5149 "parser.cc"
    break;

  case 442: // qtyconsym: "QCONSYM"
#line 1495 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5155 "parser.cc"
    break;

  case 443: // qtyconsym: "QVARSYM"
#line 1496 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5161 "parser.cc"
    break;

  case 444: // qtyconsym: tyconsym
#line 1497 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5167 "parser.cc"
    break;

  case 445: // tyconsym: "CONSYM"
#line 1499 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5173 "parser.cc"
    break;

  case 446: // tyconsym: "VARSYM"
#line 1500 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5179 "parser.cc"
    break;

  case 447: // tyconsym: ":"
#line 1501 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 5185 "parser.cc"
    break;

  case 448: // tyconsym: "-"
#line 1502 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 5191 "parser.cc"
    break;

  case 449: // op: varop
#line 1507 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5197 "parser.cc"
    break;

  case 450: // op: conop
#line 1508 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5203 "parser.cc"
    break;

  case 451: // varop: varsym
#line 1510 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5209 "parser.cc"
    break;

  case 452: // varop: "`" varid "`"
#line 1511 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5215 "parser.cc"
    break;

  case 453: // qop: qvarop
#line 1513 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 5221 "parser.cc"
    break;

  case 454: // qop: qconop
#line 1514 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 5227 "parser.cc"
    break;

  case 455: // qopm: qvaropm
#line 1517 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 5233 "parser.cc"
    break;

  case 456: // qopm: qconop
#line 1518 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 5239 "parser.cc"
    break;

  case 457: // qvarop: qvarsym
#line 1523 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5245 "parser.cc"
    break;

  case 458: // qvarop: "`" qvarid "`"
#line 1524 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5251 "parser.cc"
    break;

  case 459: // qvaropm: qvarsym_no_minus
#line 1526 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 5257 "parser.cc"
    break;

  case 460: // qvaropm: "`" qvarid "`"
#line 1527 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5263 "parser.cc"
    break;

  case 461: // tyvar: tyvarid
#line 1531 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5269 "parser.cc"
    break;

  case 462: // tyvarop: "`" tyvarid "`"
#line 1533 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5275 "parser.cc"
    break;

  case 463: // tyvarid: "VARID"
#line 1535 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5281 "parser.cc"
    break;

  case 464: // tyvarid: special_id
#line 1536 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5287 "parser.cc"
    break;

  case 465: // tyvarid: "unsafe"
#line 1537 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 5293 "parser.cc"
    break;

  case 466: // tyvarid: "safe"
#line 1538 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 5299 "parser.cc"
    break;

  case 467: // tyvarid: "interruptible"
#line 1539 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 5305 "parser.cc"
    break;

  case 468: // var: varid
#line 1542 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5311 "parser.cc"
    break;

  case 469: // var: "(" varsym ")"
#line 1543 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5317 "parser.cc"
    break;

  case 470: // qvar: qvarid
#line 1545 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5323 "parser.cc"
    break;

  case 471: // qvar: "(" varsym ")"
#line 1546 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5329 "parser.cc"
    break;

  case 472: // qvar: "(" qvarsym1 ")"
#line 1547 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5335 "parser.cc"
    break;

  case 473: // field: varid
#line 1549 "parser.y"
       { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5341 "parser.cc"
    break;

  case 474: // qvarid: varid
#line 1551 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5347 "parser.cc"
    break;

  case 475: // qvarid: "QVARID"
#line 1552 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5353 "parser.cc"
    break;

  case 476: // varid: "VARID"
#line 1554 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5359 "parser.cc"
    break;

  case 477: // varid: special_id
#line 1555 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5365 "parser.cc"
    break;

  case 478: // varid: "unsafe"
#line 1556 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 5371 "parser.cc"
    break;

  case 479: // varid: "safe"
#line 1557 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 5377 "parser.cc"
    break;

  case 480: // varid: "interruptible"
#line 1558 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 5383 "parser.cc"
    break;

  case 481: // varid: "forall"
#line 1559 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 5389 "parser.cc"
    break;

  case 482: // varid: "family"
#line 1560 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 5395 "parser.cc"
    break;

  case 483: // varid: "role"
#line 1561 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 5401 "parser.cc"
    break;

  case 484: // qvarsym: varsym
#line 1563 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5407 "parser.cc"
    break;

  case 485: // qvarsym: qvarsym1
#line 1564 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5413 "parser.cc"
    break;

  case 486: // qvarsym_no_minus: varsym_no_minus
#line 1566 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5419 "parser.cc"
    break;

  case 487: // qvarsym_no_minus: qvarsym1
#line 1567 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5425 "parser.cc"
    break;

  case 488: // qvarsym1: "QVARSYM"
#line 1569 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5431 "parser.cc"
    break;

  case 489: // varsym: varsym_no_minus
#line 1571 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5437 "parser.cc"
    break;

  case 490: // varsym: "-"
#line 1572 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 5443 "parser.cc"
    break;

  case 491: // varsym_no_minus: "VARSYM"
#line 1574 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5449 "parser.cc"
    break;

  case 492: // varsym_no_minus: special_sym
#line 1575 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5455 "parser.cc"
    break;

  case 493: // special_id: "as"
#line 1577 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 5461 "parser.cc"
    break;

  case 494: // special_id: "qualified"
#line 1578 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 5467 "parser.cc"
    break;

  case 495: // special_id: "hiding"
#line 1579 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 5473 "parser.cc"
    break;

  case 496: // special_id: "export"
#line 1580 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 5479 "parser.cc"
    break;

  case 497: // special_id: "label"
#line 1581 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 5485 "parser.cc"
    break;

  case 498: // special_id: "dynamic"
#line 1582 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 5491 "parser.cc"
    break;

  case 499: // special_id: "stdcall"
#line 1583 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 5497 "parser.cc"
    break;

  case 500: // special_id: "ccall"
#line 1584 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 5503 "parser.cc"
    break;

  case 501: // special_id: "capi"
#line 1585 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 5509 "parser.cc"
    break;

  case 502: // special_id: "prim"
#line 1586 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 5515 "parser.cc"
    break;

  case 503: // special_id: "javascript"
#line 1587 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 5521 "parser.cc"
    break;

  case 504: // special_id: "group"
#line 1588 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 5527 "parser.cc"
    break;

  case 505: // special_id: "stock"
#line 1589 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 5533 "parser.cc"
    break;

  case 506: // special_id: "anyclass"
#line 1590 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 5539 "parser.cc"
    break;

  case 507: // special_id: "via"
#line 1591 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 5545 "parser.cc"
    break;

  case 508: // special_id: "unit"
#line 1592 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 5551 "parser.cc"
    break;

  case 509: // special_id: "dependency"
#line 1593 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 5557 "parser.cc"
    break;

  case 510: // special_id: "signature"
#line 1594 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 5563 "parser.cc"
    break;

  case 511: // special_sym: "."
#line 1596 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 5569 "parser.cc"
    break;

  case 512: // special_sym: "*"
#line 1597 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 5575 "parser.cc"
    break;

  case 513: // qconid: conid
#line 1601 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5581 "parser.cc"
    break;

  case 514: // qconid: "QCONID"
#line 1602 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5587 "parser.cc"
    break;

  case 515: // conid: "CONID"
#line 1604 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5593 "parser.cc"
    break;

  case 516: // qconsym: consym
#line 1606 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5599 "parser.cc"
    break;

  case 517: // qconsym: "QCONSYM"
#line 1607 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5605 "parser.cc"
    break;

  case 518: // consym: "CONSYM"
#line 1609 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5611 "parser.cc"
    break;

  case 519: // consym: ":"
#line 1610 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 5617 "parser.cc"
    break;

  case 520: // literal: "CHAR"
#line 1614 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 5623 "parser.cc"
    break;

  case 521: // literal: "STRING"
#line 1615 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 5629 "parser.cc"
    break;

  case 522: // literal: "INTEGER"
#line 1616 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < integer > ()});}
#line 5635 "parser.cc"
    break;

  case 523: // literal: "RATIONAL"
#line 1617 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Floating{yystack_[0].value.as < rational > ()});}
#line 5641 "parser.cc"
    break;

  case 524: // literal: "PRIMINTEGER"
#line 1618 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < integer > ()});}
#line 5647 "parser.cc"
    break;

  case 526: // close: error
#line 1626 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 5653 "parser.cc"
    break;

  case 527: // modid: "CONID"
#line 1630 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5659 "parser.cc"
    break;

  case 528: // modid: "QCONID"
#line 1631 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5665 "parser.cc"
    break;

  case 529: // commas: commas ","
#line 1633 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 5671 "parser.cc"
    break;

  case 530: // commas: ","
#line 1634 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 5677 "parser.cc"
    break;


#line 5681 "parser.cc"

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


  const short parser::yypact_ninf_ = -723;

  const short parser::yytable_ninf_ = -490;

  const short
  parser::yypact_[] =
  {
      39,   236,  -723,   172,  -723,  -723,  -723,  -723,  -723,    69,
     -11,   -10,  -723,    65,    -3,    -3,     5,  -723,  -723,  -723,
    -723,   188,  -723,  -723,  -723,    78,  -723,   164,   198,  1442,
     258,   275,   103,  -723,   881,  -723,   191,  -723,  -723,  -723,
     236,  -723,   236,  -723,  -723,  -723,  -723,  -723,  -723,  -723,
    -723,  -723,  -723,  -723,  -723,  -723,  -723,  -723,  -723,  -723,
    -723,  -723,  -723,  -723,  -723,   744,  -723,  -723,  -723,  -723,
    -723,   192,   -15,  -723,   223,  -723,  -723,  -723,  -723,  -723,
    -723,  -723,   359,  -723,   236,  -723,   228,  -723,  2578,  4586,
     319,   257,   438,   375,  2578,  -723,  -723,  -723,   395,   413,
    -723,  3692,   363,   375,  3268,  4994,   245,  3268,  3268,  2854,
    3268,  1888,  1750,   289,   284,  -723,  -723,  -723,  -723,  -723,
    -723,  -723,    36,   284,   248,   103,  -723,  -723,  -723,  -723,
    -723,   105,   109,  -723,  -723,   679,  -723,  2992,  -723,   325,
    -723,  -723,  -723,  -723,  -723,  -723,   312,   125,  -723,  -723,
    -723,  -723,   276,  -723,  -723,   302,  -723,  -723,  -723,  -723,
     311,  -723,   315,   339,   349,  -723,  -723,  -723,  4688,  -723,
    4725,  -723,  -723,  -723,  -723,   435,  -723,  1750,   461,   690,
    -723,  -723,  -723,  4586,  4586,  -723,  5093,  3780,  3382,   368,
    -723,   409,   450,  -723,   360,  -723,  3970,  -723,  -723,  -723,
    -723,  -723,  -723,  -723,  4586,   402,  -723,  4058,  -723,  -723,
    -723,  4586,   486,   533,  2302,  2302,  -723,   437,   475,   476,
     479,   483,  4058,  1307,  1307,  -723,   553,  4586,  4586,   144,
     488,   499,   150,    58,  -723,  -723,   381,    -4,   463,   121,
    -723,   163,  -723,  -723,  -723,  -723,  3130,  -723,  2992,  -723,
    -723,  -723,  4892,  -723,  -723,  -723,   690,   155,   471,   451,
    -723,  2578,  -723,  -723,  -723,  -723,  -723,  -723,  5225,  -723,
    -723,   194,   129,   267,   339,   468,   469,   470,   282,  -723,
     298,    90,  4994,  -723,  4058,  4994,  4994,  -723,   990,   228,
     513,   453,  4586,  4058,  5093,  2578,  2716,  4892,  -723,    45,
    -723,  -723,  2578,  -723,  -723,  -723,  -723,  4586,  -723,  5225,
    4929,  3268,  -723,  -723,  -723,  -723,  -723,  -723,  -723,   477,
     480,   481,  -723,   494,    65,   236,    38,   420,  4058,  -723,
    -723,   193,   173,   502,   485,  -723,  -723,  -723,  -723,   506,
     508,   525,  -723,  -723,   509,  -723,  -723,  -723,  -723,  -723,
    -723,   514,   515,   519,  -723,   290,   301,  -723,   609,  4586,
    4058,   825,  4586,  -723,  -723,  -723,  4586,  -723,  -723,   556,
    4058,  -723,  -723,  -723,  -723,  4058,  4058,   413,   375,   552,
     555,    -6,  -723,  -723,    47,  -723,   618,  -723,  -723,  -723,
    -723,   620,   197,  -723,  -723,   679,    48,  2578,  -723,   568,
     353,  4058,   242,  4058,  -723,  -723,  -723,   520,  -723,   579,
     550,   244,   245,   589,  2578,  -723,   547,   549,  2578,  2578,
    2716,  2026,  -723,  2026,   862,  -723,  -723,  5225,  -723,  -723,
    2026,  -723,  2026,   178,  -723,  -723,  -723,  -723,   535,   566,
     603,   604,   606,   616,  5031,   565,  -723,  -723,  -723,  -723,
    -723,  4146,    15,   367,  -723,  -723,  -723,  -723,   673,   621,
     582,  -723,   585,   413,  -723,  -723,  -723,  -723,  -723,  -723,
     601,  -723,   590,   631,   613,   614,  -723,  -723,  -723,  4827,
    -723,  -723,  -723,   605,  1483,  -723,  -723,  2164,  1612,  -723,
    -723,   610,  4058,  -723,  5093,  5262,  -723,  4058,  4058,  -723,
    -723,  4058,  -723,  -723,  -723,   597,  -723,  5375,   421,  -723,
    -723,  -723,   602,   608,   403,  -723,  4058,  -723,  -723,   611,
     607,  -723,  -723,   553,  -723,  2578,  -723,  2302,  -723,  2578,
     434,  -723,  -723,  1307,  -723,  -723,  4058,  4058,  1548,   645,
    -723,  -723,   692,  -723,  -723,  5093,  -723,  -723,   622,   578,
     306,  -723,  -723,  -723,  -723,  -723,  -723,  -723,  -723,   615,
    -723,   654,  -723,  -723,  -723,  -723,  -723,   626,  -723,  -723,
    -723,  4058,  4058,   627,   628,   990,  -723,   367,   647,  -723,
    -723,   672,  4058,   729,   731,   751,  -723,  2578,  2716,  -723,
    -723,  -723,  4929,  2026,  5225,  -723,  1483,   236,  -723,   223,
     649,   151,  -723,  -723,  2440,  -723,   659,   650,  -723,   423,
      65,  -723,  -723,  -723,  -723,  4058,  5449,  5449,  -723,  -723,
    -723,  -723,  -723,   652,  -723,  -723,  -723,  1169,  1169,  -723,
    -723,  -723,  -723,  -723,  4058,  -723,  -723,   437,  1025,  1025,
    -723,  -723,  -723,  -723,  -723,  5449,   741,   697,  -723,  -723,
    2716,  2578,  -723,  -723,    -8,   138,  -723,  -723,  -723,  5130,
     731,   751,  4586,  -723,  -723,  -723,   694,  -723,  4586,   441,
     751,    95,  -723,   751,  -723,  -723,  -723,  -723,  -723,    20,
    -723,   666,  -723,  -723,  -723,  4790,  -723,  -723,  -723,  2578,
    2716,  2578,  -723,    89,  -723,  -723,  -723,    17,   700,  -723,
    -723,  5449,   745,  3882,  -723,  -723,   199,  -723,    51,  -723,
     777,  -723,   771,  -723,   771,  -723,   208,  -723,    52,  -723,
     704,   442,  -723,  4058,  -723,  -723,  -723,  4058,  -723,  4586,
    4586,   751,  -723,  -723,  5299,   729,   701,  3486,  -723,  -723,
    -723,   437,   437,  -723,  4234,   251,   737,  -723,  -723,  -723,
    2026,  5225,  -723,  -723,  -723,   708,   673,  -723,  -723,  4058,
    -723,  4058,  -723,  4586,  4586,  4586,  -723,   392,  -723,  1169,
    -723,  2578,  -723,  4586,   513,  -723,  1025,  -723,  5449,  4322,
    4410,  -723,  -723,  -723,  -723,   705,   403,  -723,  -723,  -723,
    4586,   671,  -723,  4586,   229,  -723,   245,    56,  -723,  -723,
     681,   685,  -723,  -723,  -723,  -723,  2578,  -723,   696,   693,
     556,  -723,   454,  4058,  4498,  -723,  -723,  -723,  -723,  4146,
    -723,  5449,  -723,   698,   256,  -723,    65,    57,  4586,  3588,
    -723,  4586,  -723,   437,   182,  -723,  4586,  -723,  -723,  -723,
    -723,  -723,  5412,  -723,  -723,  3382,   723,   727,   367,  -723,
    -723,  -723,  4586,  -723,  -723,  -723,  -723,  4058,  -723,   700,
    5449,   731,   751,  -723,  -723,  -723,   751,  -723,  -723
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    40,     0,     2,    40,     4,   527,   528,     8,
       0,    43,     1,     0,     0,     0,    18,    11,    39,    13,
      16,    70,   526,   525,    12,   192,   188,     0,     0,    20,
       0,    46,    41,    15,    14,   191,     0,     6,     7,   493,
       0,   495,     0,   494,   481,   496,   497,   498,   479,   480,
     478,   482,   483,   499,   500,   501,   502,   503,   504,   505,
     506,   507,   508,   510,   509,     0,   476,   441,   475,   439,
      22,     0,    19,    24,    28,    36,   432,   440,    35,   470,
     474,   477,     0,    45,     0,    38,    42,   339,     0,     0,
     136,   138,     0,     0,     0,    63,    64,    65,   100,     0,
     137,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   303,   515,   514,   520,   521,   522,
     523,   524,   303,   303,    61,    68,    71,    72,    73,    74,
      75,   158,     0,    78,   286,    79,   309,   312,   318,   328,
     331,   333,   403,   416,   404,   198,   332,   474,   405,   513,
     334,   189,     0,    27,    26,     0,   490,   512,   511,   491,
       0,   488,     0,     0,     0,   489,   492,    17,     0,    21,
      31,    25,    40,    40,     3,    48,    37,     0,     0,   308,
     466,   467,   465,     0,     0,   226,   271,     0,     0,     0,
     463,   248,     0,   151,   209,   212,   213,   217,   224,   425,
     430,   225,   461,   464,     0,     0,   139,     0,   103,   101,
     102,     0,     0,     0,   386,   386,   326,   314,     0,     0,
       0,     0,     0,   182,   182,   185,     0,     0,     0,     0,
       0,   209,   425,     0,   327,   317,     0,     0,     0,     0,
     411,   193,   409,   407,   378,   380,     0,   320,   311,   321,
     519,   417,     0,   518,   517,   342,   308,   347,     0,   348,
     456,     0,   455,   459,   487,   486,   420,   516,     0,   412,
     530,     0,     0,     0,   487,     0,   486,   420,     0,   414,
       0,     0,     0,   304,     0,     0,     0,    62,     0,    69,
     158,     0,     0,     0,     0,     0,     0,     0,   287,   187,
     292,   454,     0,   453,   457,   485,   484,     0,   315,     0,
     393,     0,   190,   435,   434,   433,   472,   471,    23,     0,
       0,    32,    34,     0,     0,     0,    50,     0,     0,   228,
     227,     0,     0,     0,   272,   274,   468,   242,   429,     0,
     201,     0,   205,   447,     0,   448,   230,   446,   445,   443,
     442,   239,     0,     0,   444,     0,     0,   254,   168,     0,
       0,     0,     0,   221,   436,   222,     0,   218,   220,   142,
     238,   234,   195,   105,   104,     0,     0,     0,     0,   390,
       0,     0,   385,   387,     0,   313,     0,    97,    96,    98,
      99,   178,     0,   288,   181,     0,     0,     0,    93,     0,
     144,     0,     0,     0,    80,    81,    82,     0,   298,     0,
       0,     0,     0,     0,     0,   379,     0,     0,   343,   349,
       0,     0,   338,     0,   344,   341,   473,     0,   337,   335,
       0,   336,     0,   471,   406,   413,   529,   415,     0,     0,
       0,     0,     0,     0,     0,   295,   450,    67,   449,   451,
     418,     0,     0,   140,   294,   206,   196,   197,   187,     0,
     358,   360,     0,     0,   290,   291,   310,   316,   330,   396,
       0,   392,   395,   398,     0,   474,   319,    30,    29,     0,
       9,    10,    47,     0,    54,    44,    49,     0,     0,   325,
     307,     0,     0,   229,     0,     0,   232,     0,     0,   428,
     233,     0,   431,   426,   427,   249,   251,     0,     0,    83,
     150,   210,     0,     0,   214,   219,     0,    88,   239,     0,
     237,   106,   107,   391,   388,     0,   381,   384,   382,     0,
       0,    92,   183,   180,   184,   323,     0,     0,     0,   108,
     255,    89,     0,    90,    84,     0,   299,   408,     0,     0,
       0,   194,   422,   410,   296,   322,   460,   421,   351,   353,
     357,   342,   355,   354,   340,   346,   345,     0,   305,   297,
     302,     0,     0,     0,     0,     0,   242,   140,     0,   155,
     157,     0,     0,   268,   258,   276,   289,     0,     0,   458,
     186,   329,     0,     0,     0,    33,    54,     0,    56,    28,
       0,    53,    58,   363,     0,   376,     0,   365,   369,     0,
       0,   364,   469,   275,   273,     0,     0,     0,   241,   243,
     246,   202,   204,   240,   254,   254,   253,   164,   164,   167,
     437,   462,   143,    76,     0,   389,   383,   314,   174,   174,
     177,   179,   123,   145,   146,     0,   113,     0,   423,   424,
       0,   350,   306,   199,     0,     0,   452,   419,    66,     0,
     258,   276,     0,   156,   141,   242,   262,   264,     0,     0,
     276,     0,    85,   277,   279,   293,   359,   394,   397,   400,
     402,     0,    60,    59,    51,     0,    55,   366,   361,   368,
       0,     0,   370,   187,   374,   362,   203,     0,     0,   231,
     250,   252,   129,     0,   159,   163,     0,   160,     0,   240,
       0,   136,   131,   169,   131,   173,     0,   170,     0,   109,
       0,     0,    87,     0,   356,   352,   300,     0,   301,     0,
       0,   276,    94,   154,     0,   268,     0,   269,   215,   223,
     266,   314,   314,    86,     0,     0,   280,   283,   438,   278,
       0,     0,    52,    57,   367,     0,   187,   372,   373,     0,
     244,     0,   130,     0,     0,     0,   127,   147,   165,   162,
     166,     0,   132,     0,   158,   175,   172,   176,     0,   122,
     122,   114,    77,   200,   153,     0,   207,    95,   267,   263,
       0,     0,   216,     0,     0,   260,     0,     0,   284,   211,
     235,     0,   281,   282,   399,   401,     0,   371,     0,     0,
     142,   128,   147,     0,     0,   125,   161,   324,   133,     0,
     171,   110,   112,     0,     0,   121,     0,     0,     0,   269,
     265,   270,   256,   314,     0,   257,     0,   285,   375,   245,
     247,   124,     0,   126,   148,     0,     0,   225,   140,   111,
     117,   115,   120,   118,   116,   152,   259,     0,   236,   225,
       0,   258,   276,   119,   261,   149,   276,   134,   135
  };

  const short
  parser::yypgoto_[] =
  {
    -723,  -723,  -723,  -723,  -723,  -723,  -723,    42,  -723,  -723,
    -723,  -723,   657,   227,  -723,  -723,    -7,   702,  -723,  -723,
    -723,  -723,  -723,  -723,  -723,  -723,   232,  -723,   145,  -723,
    -723,  -723,  -723,  -723,  -723,  -723,  -723,  -723,  -723,  -723,
    -723,  -723,   160,    86,  -723,  -723,   -27,  -723,  -723,  -723,
      54,  -186,  -723,  -723,   122,  -723,   801,  -723,  -559,    29,
    -723,    30,   551,    25,  -275,    76,   218,  -723,  -723,    71,
     211,  -723,  -723,   629,  -723,  -283,  -432,   836,  -723,  -723,
    -320,   131,  -161,   280,  -173,   -60,  -723,   -88,  -723,   -85,
    -723,   -90,  -723,  -357,  -723,  -723,  -723,  -633,  -145,  -187,
      26,  -723,   491,  -540,   328,  -722,  -723,  -723,   253,   271,
    -430,  -623,   136,    59,  -554,  -723,   167,  -723,   114,  -723,
    -723,   412,  -616,  -723,   235,   177,   892,  -196,  -723,  -723,
     630,  -723,   428,  -723,   124,   -24,  -240,  -193,   818,   -64,
    -723,  -723,  -723,   -98,  -723,  -723,  -723,  -723,   243,  -723,
    -723,  -417,  -723,   252,  -723,  -723,   241,  -723,  -723,   706,
    -723,   -86,   725,   416,  -262,  -723,   352,  -723,  -723,  -723,
    -723,   534,   152,  -723,  -103,  -660,   -45,  -723,   536,   -89,
    -723,  -723,  -723,   118,  -723,  -162,  -723,   376,  -723,   699,
    -723,  -723,  -723,   139,  -723,  -354,  -261,    35,  -248,   -69,
     -32,  -723,  -723,   195,    -9,   -81,   -29,  -723,   -42,  -100,
     -44,  -197,  -723,  -295,   -21,  -104
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   174,     6,    10,    19,    30,
      71,    72,    73,   171,   320,   321,   599,    86,    11,    20,
      21,    32,    84,   326,   485,   486,   600,   601,   602,   288,
     124,   445,    33,    34,   125,   407,   126,   127,   128,   229,
     129,   222,   212,   213,   130,   646,   719,   821,   722,   781,
     824,   825,   704,   763,   773,   713,   714,   205,   584,   517,
     539,   815,   191,   577,   292,   705,   706,   629,   509,   715,
     716,   640,   531,   392,   225,   226,   464,    27,    36,   413,
     371,   454,   132,   654,   351,   372,   456,   341,   736,   342,
     800,   194,   195,   737,   196,   367,   362,   738,   197,   373,
     801,   519,   352,   495,   618,   619,   358,   505,   506,   507,
     541,   670,   794,   795,   585,   666,   667,   668,   740,   333,
     334,   335,   672,   673,   674,   746,   393,   707,   298,   299,
     300,   134,   282,   283,   255,   179,   136,   796,   137,   138,
     139,   140,   271,   272,   273,   258,   259,   559,   459,   460,
     489,   606,   607,   608,   692,   693,   694,   609,   380,   245,
     246,   216,   381,   382,   383,   470,   471,   472,   679,   141,
     142,   240,   241,   143,   144,   446,   260,   551,   198,   199,
      75,   363,   747,   200,    77,   353,   354,   447,   448,   302,
     261,   303,   262,   201,   365,   202,   145,   146,   474,    79,
      80,   304,   263,   264,   306,   165,   203,   166,   148,   149,
     266,   267,   150,    24,     9,   278
  };

  const short
  parser::yytable_[] =
  {
      81,   192,   147,   242,   193,    81,   243,   513,   490,   280,
     135,   231,   232,   257,   339,   451,   230,   234,   660,   153,
     425,   154,    74,   661,   386,   332,   586,   394,   394,   481,
     265,   276,   364,   457,   461,   391,   659,   731,   329,   330,
     235,   398,   410,   244,   247,   732,   249,    13,    22,    22,
     483,   368,    22,    22,   743,   284,   164,    22,    22,    81,
       1,   468,   466,   175,    78,    81,    22,   621,   277,   364,
     726,   610,   463,   308,   408,    81,    81,   793,    81,    81,
      81,    81,    81,    81,   355,   356,   632,   256,   256,   528,
     301,   450,   846,   580,   523,    17,   276,   441,   759,   404,
     526,   534,   750,   275,   792,   168,    25,   643,    81,    18,
     405,   406,   727,   527,   231,   787,   463,   208,    29,   369,
     846,   231,   751,   760,   290,   734,   374,   340,   340,   169,
     296,   409,    26,   277,   301,    14,    15,   231,   231,    81,
     237,    81,   399,   400,     2,   281,   209,    76,    81,   210,
     581,   484,   664,   256,   336,    23,    23,    81,   560,    23,
      23,    74,   467,   322,    23,    23,   527,   533,   164,   793,
     769,   776,    12,    23,   690,   833,   852,   291,   466,   564,
     590,   438,   244,   416,   308,    81,    81,   687,   521,   522,
     293,   147,   147,   611,    81,    81,   792,   518,   792,   395,
     395,   250,    31,    78,   192,    78,  -468,   193,   744,   206,
     417,   301,   178,    35,   323,   324,   728,    81,   217,    81,
      67,   515,    85,    81,    69,   401,   439,   164,   462,   294,
     544,   -91,    81,   455,   419,   269,   426,   424,   866,    81,
     420,   270,    37,   429,   411,  -468,   867,   476,   253,   430,
     868,   653,   653,    81,   492,   417,    81,    81,   727,  -469,
     163,   757,   336,   857,   402,    81,    81,    81,    81,   231,
     -91,   685,   514,    81,   510,   421,    38,   426,   475,   449,
      81,    81,    81,   412,   647,    82,    76,   156,    76,   861,
     157,   720,   524,   294,   862,   686,   427,   158,  -469,    83,
     511,   151,   412,   532,   482,   768,   167,   274,   428,   242,
     340,   152,   243,   543,   775,   695,   533,   440,   769,   159,
     442,   443,   491,   562,   807,   563,   676,   776,   623,   808,
     305,   809,   565,   332,   566,   832,   170,   641,   379,   379,
     265,   540,   265,   340,   574,   473,   680,   176,   833,   265,
     301,   265,   364,   548,   238,   542,   204,   549,   239,   550,
     113,     7,   851,   578,   744,     8,   579,    67,    81,    67,
     115,    69,   274,    69,   305,   852,    67,   233,   450,   301,
      69,   431,    67,   844,   287,    81,    69,   432,   724,    81,
      81,    81,    81,   281,    81,   426,   435,   256,    81,   256,
     311,    81,   436,    81,   503,   279,   256,   783,   256,   270,
     436,   312,   573,   770,   437,    81,   313,   504,   436,   458,
     379,   436,   649,   777,   822,   314,   270,   309,   461,   315,
     310,   163,   613,   829,   537,   538,   831,   540,   622,   325,
     343,   340,   717,   717,   710,   355,   356,   360,   582,   583,
      81,   305,  -207,   316,   345,    81,   540,   207,    81,    81,
     208,   709,   336,   317,   172,    81,   173,   849,   218,   219,
     220,   221,   595,   813,   814,   156,   642,   540,   157,   512,
     214,   361,   215,   343,   327,   158,   347,   348,   270,   209,
     349,   350,   210,   211,   357,   678,    81,   345,    81,   819,
      81,   147,   835,   805,    81,   375,   720,   159,   690,   395,
     691,   161,   265,   336,    78,   370,    81,   766,   223,    78,
     224,   535,   540,   739,   361,   487,   627,   488,   628,   347,
     348,   853,   854,   349,   350,   813,   842,   864,   555,   638,
     450,   639,   359,   558,   379,   561,   741,   779,   742,   780,
     285,   286,   376,   387,   388,   696,   385,   389,    81,    81,
     475,   390,   426,    81,    81,    81,   449,    81,   397,   256,
     403,   423,   231,   251,   340,    81,   682,   733,   811,   343,
     717,   422,   433,  -489,   434,   291,   360,   818,   452,   497,
     305,   477,   739,   345,   478,   147,   147,    76,    81,    81,
     480,   479,    76,   395,   395,   494,   147,   147,   493,    81,
      81,   605,   605,   231,   395,   395,   496,   498,   767,   305,
     361,    81,    81,   499,   364,   347,   348,   473,   500,   349,
     350,    78,   450,   502,   620,   501,   508,   516,  -377,   231,
     786,   525,   785,   529,   784,   739,   626,   530,   739,   635,
     536,   379,   804,   637,   231,   545,    81,   546,   343,   799,
      81,    81,    81,   455,   547,   344,   863,   554,   556,   265,
     557,   567,   345,   231,   231,   231,   568,   620,   810,   399,
     812,   569,   570,   231,   739,   575,   739,   571,   399,   231,
     231,   574,   648,   242,   399,   399,   243,   572,   270,   540,
     463,   540,   588,   587,   347,   348,   589,   591,   349,   350,
     592,   675,   379,   593,    76,   594,  -473,   624,   596,   426,
      78,    81,    81,   630,   612,   633,   256,   634,   605,   631,
     645,   578,   338,   651,   579,   650,   652,   147,   231,   662,
      81,   355,    81,   855,   147,   395,   231,    81,   656,   657,
     663,   799,   395,   540,   642,   697,   698,   665,   669,   250,
     671,   295,   231,   684,   296,   688,   699,   399,   721,   689,
     250,   328,   343,   156,   379,   725,   157,    81,   723,   735,
     752,   761,   762,   158,   156,   340,   345,   157,   771,   748,
     772,   778,   211,   790,   158,   806,   115,   828,   620,   837,
     297,   836,   839,    76,   850,   159,   253,   840,   860,   161,
     254,   297,  -246,   605,   379,   756,   159,   253,   347,   348,
     161,   254,   349,   350,   155,   318,   683,   289,   681,    39,
     753,   745,   803,   865,   827,   131,   774,    41,   156,   841,
     626,   157,   843,   453,   848,   816,   708,   820,   158,    43,
     718,    28,   655,   396,   782,    45,    46,    47,   180,   181,
     182,   520,   858,   748,    53,    54,   644,    55,    56,    57,
     159,   160,    58,   620,   161,   162,    59,   700,   797,    60,
      61,    62,    63,    64,    87,    39,    88,    89,    90,    91,
      92,    93,   856,    41,    94,   817,   701,    95,    96,    97,
      98,    99,   789,   100,   830,    43,   614,   101,   749,    44,
     102,    45,    46,    47,    48,    49,    50,   103,    51,    52,
      53,    54,   802,    55,    56,    57,   133,   248,    58,   465,
     838,   104,    59,   755,   758,    60,    61,    62,    63,    64,
     384,   754,   250,   636,   677,   105,   553,   552,   834,   190,
      67,   658,   415,   847,    69,   418,   156,     0,   106,   157,
       0,     0,     0,     0,   107,     0,   158,     0,     0,     0,
       0,   108,     0,     0,   109,     0,   110,     0,     0,     0,
       0,   620,     0,   297,   859,     0,     0,     0,   159,   253,
     111,     0,   161,   254,   112,     0,   113,     0,     0,     0,
       0,     0,     0,     0,   114,    66,   115,     0,     0,    68,
     116,     0,     0,     0,     0,   117,   118,   119,   120,     0,
       0,   121,     0,     0,     0,     0,   122,   123,    87,    39,
      88,     0,   711,     0,     0,    93,     0,    41,    94,     0,
       0,    95,    96,    97,     0,    99,     0,   100,     0,    43,
       0,   712,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   103,    51,    52,    53,    54,     0,    55,    56,    57,
     250,     0,    58,     0,     0,   104,    59,     0,     0,    60,
      61,    62,    63,    64,   156,     0,     0,   157,     0,   105,
       0,     0,     0,     0,   158,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,   107,     0,
       0,   444,     0,     0,     0,   108,   159,   253,   109,     0,
     110,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   111,     0,     0,     0,   112,     0,
     113,     0,     0,     0,     0,     0,     0,     0,   114,    66,
     115,     0,     0,    68,   116,     0,     0,     0,     0,   117,
     118,   119,   120,     0,     0,   121,     0,     0,     0,     0,
     122,   123,    87,    39,    88,     0,   702,     0,     0,    93,
       0,    41,    94,     0,     0,    95,    96,    97,     0,    99,
       0,     0,     0,    43,     0,   703,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   103,    51,    52,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,   104,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,   105,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,     0,   108,
       0,     0,   109,     0,   110,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   111,     0,
       0,     0,   112,     0,   113,     0,     0,     0,     0,     0,
       0,     0,   114,    66,   115,     0,     0,    68,   116,     0,
       0,     0,     0,   117,   118,   119,   120,     0,     0,   121,
      87,    39,    88,     0,   122,   123,     0,    93,     0,    41,
      94,     0,     0,    95,    96,    97,     0,    99,     0,     0,
       0,    43,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   103,    51,    52,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,   104,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,   105,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,     0,   108,     0,     0,
     109,     0,   110,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,     0,     0,     0,
     112,     0,   113,     0,     0,     0,     0,     0,     0,     0,
     114,    66,   115,     0,     0,    68,   116,     0,     0,     0,
       0,   117,   118,   119,   120,     0,    39,   121,     0,     0,
      40,     0,   122,   123,    41,     0,     0,     0,     0,     0,
       0,     0,     0,    42,     0,     0,    43,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,     0,    55,    56,    57,    39,     0,    58,
       0,     0,     0,    59,     0,    41,    60,    61,    62,    63,
      64,     0,     0,     0,   597,     0,     0,    43,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,     0,     0,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    39,     0,     0,    65,     0,     0,     0,     0,
      41,     0,     0,     0,     0,     0,    66,    67,     0,     0,
      68,    69,    43,     0,     0,     0,     0,     0,    45,    46,
      47,   180,   181,   182,     0,     0,    70,    53,    54,     0,
      55,    56,    57,     0,     0,    58,    65,     0,     0,    59,
       0,     0,    60,    61,    62,    63,    64,    66,    67,     0,
       0,    68,    69,    22,     0,    87,    39,    88,     0,     0,
       0,     0,    93,     0,    41,    94,     0,   598,     0,     0,
       0,     0,    99,     0,     0,     0,    43,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   103,    51,
      52,    53,    54,   616,    55,    56,    57,     0,     0,    58,
       0,   617,   104,    59,     0,     0,    60,    61,    62,    63,
      64,     0,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
       0,     0,   108,     0,     0,   109,     0,   110,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      23,   111,     0,     0,     0,   177,     0,   113,     0,     0,
       0,   604,     0,     0,     0,     0,    66,   115,     0,     0,
      68,   116,     0,     0,     0,     0,   117,   118,   119,   120,
       0,     0,   121,    87,    39,    88,     0,     0,     0,     0,
      93,     0,    41,    94,     0,     0,     0,     0,     0,     0,
      99,     0,     0,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   103,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
     104,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     250,     0,     0,   107,     0,     0,     0,     0,     0,     0,
     108,     0,     0,   109,   156,   110,     0,   157,     0,     0,
       0,     0,     0,   268,   158,     0,     0,     0,     0,   111,
       0,     0,     0,   177,   269,   113,     0,     0,     0,     0,
     270,   252,     0,     0,    66,   115,   159,   253,    68,   116,
     161,   254,     0,     0,   117,   118,   119,   120,     0,     0,
     121,    87,    39,    88,     0,     0,     0,     0,    93,     0,
      41,    94,     0,     0,     0,     0,     0,     0,    99,     0,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   103,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,   104,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   250,     0,
       0,   107,     0,     0,     0,     0,     0,     0,   108,     0,
       0,   109,     0,   110,     0,   157,     0,     0,     0,     0,
       0,     0,   158,     0,     0,     0,     0,   111,   251,     0,
       0,   177,     0,   113,     0,     0,     0,     0,     0,   252,
       0,     0,    66,   115,   159,   253,    68,   116,   161,   254,
       0,     0,   117,   118,   119,   120,     0,     0,   121,    87,
      39,    88,     0,     0,     0,     0,    93,     0,    41,    94,
       0,     0,     0,     0,     0,     0,    99,     0,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   103,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,   104,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   250,     0,     0,   107,
       0,     0,     0,     0,     0,     0,   108,     0,     0,   109,
       0,   110,     0,   157,     0,     0,     0,     0,     0,     0,
     158,     0,     0,     0,     0,   111,     0,     0,     0,   177,
       0,   113,     0,     0,     0,     0,     0,   252,     0,     0,
      66,   115,   159,   253,    68,   116,   161,   254,     0,     0,
     117,   118,   119,   120,     0,     0,   121,    87,    39,    88,
       0,     0,     0,     0,    93,     0,    41,    94,     0,     0,
       0,     0,     0,     0,    99,     0,     0,     0,    43,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     103,    51,    52,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,   104,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   107,     0,     0,
       0,     0,     0,     0,   108,     0,     0,   109,     0,   110,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     603,     0,     0,   111,     0,     0,     0,   177,     0,   113,
       0,     0,     0,   604,     0,     0,     0,     0,    66,   115,
       0,     0,    68,   116,     0,     0,     0,     0,   117,   118,
     119,   120,     0,     0,   121,    87,    39,    88,     0,     0,
       0,     0,    93,     0,    41,    94,     0,     0,     0,     0,
       0,     0,   377,     0,     0,     0,    43,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   103,    51,
      52,    53,    54,     0,    55,    56,    57,     0,   378,    58,
       0,     0,   104,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
       0,     0,   108,     0,     0,   109,     0,   110,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   111,     0,     0,     0,   177,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    66,   115,     0,     0,
      68,   116,     0,     0,     0,     0,   117,   118,   119,   120,
       0,     0,   121,    87,    39,    88,     0,     0,     0,     0,
      93,     0,    41,    94,     0,     0,     0,     0,     0,     0,
      99,     0,     0,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   103,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
     104,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,     0,     0,     0,     0,     0,     0,
     108,     0,     0,   109,     0,   110,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   111,
       0,     0,     0,   177,     0,   113,     0,     0,     0,   604,
       0,     0,     0,     0,    66,   115,     0,     0,    68,   116,
       0,     0,     0,     0,   117,   118,   119,   120,     0,     0,
     121,    87,    39,    88,     0,     0,     0,     0,    93,     0,
      41,    94,     0,     0,     0,     0,     0,     0,    99,     0,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   103,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,   104,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   107,     0,     0,     0,     0,     0,     0,   108,     0,
       0,   109,     0,   110,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   111,     0,     0,
       0,   177,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    66,   115,     0,     0,    68,   116,     0,     0,
       0,     0,   117,   118,   119,   120,     0,     0,   121,    87,
      39,    88,     0,     0,     0,     0,    93,     0,    41,    94,
       0,     0,     0,     0,     0,     0,   377,     0,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   103,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,   104,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   107,
       0,     0,     0,     0,     0,     0,   108,     0,     0,   109,
       0,   110,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   111,     0,     0,     0,   177,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      66,   115,     0,     0,    68,   116,     0,     0,     0,     0,
     117,   118,   119,   120,     0,     0,   121,    87,    39,    88,
       0,     0,     0,     0,    93,     0,    41,    94,     0,     0,
       0,     0,     0,     0,    99,     0,     0,     0,    43,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     103,    51,    52,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,   104,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   107,     0,     0,
       0,     0,     0,     0,   108,     0,     0,     0,     0,   110,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   111,     0,     0,     0,   177,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    66,   115,
       0,     0,    68,   116,     0,     0,     0,     0,   117,   118,
     119,   120,     0,     0,   121,    87,    39,    88,     0,     0,
       0,     0,    93,     0,    41,    94,     0,     0,     0,     0,
       0,     0,    99,     0,     0,     0,    43,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   103,    51,
      52,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
       0,   307,   108,     0,     0,     0,     0,   110,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   111,     0,     0,     0,   177,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    66,   115,     0,     0,
      68,   116,     0,     0,     0,     0,   117,   118,   119,   120,
       0,     0,   121,    87,    39,    88,     0,     0,     0,     0,
      93,     0,    41,    94,     0,     0,     0,     0,     0,     0,
      99,     0,     0,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   103,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,     0,     0,     0,   414,     0,     0,
     108,     0,     0,     0,     0,   110,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   111,
       0,     0,     0,   177,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    66,   115,     0,     0,    68,   116,
       0,     0,     0,     0,   117,   118,   119,   120,     0,     0,
     121,    87,    39,    88,     0,     0,     0,     0,    93,     0,
      41,    94,     0,     0,     0,     0,     0,     0,    99,     0,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   103,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   107,     0,     0,     0,     0,     0,     0,   108,     0,
       0,     0,     0,   110,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   111,     0,     0,
       0,   177,     0,   113,     0,     0,    39,     0,     0,     0,
       0,     0,    66,   115,    41,     0,    68,   116,     0,     0,
       0,     0,   117,   118,   119,   120,    43,     0,   121,     0,
     337,     0,    45,    46,    47,   180,   181,   182,     0,     0,
       0,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   343,     0,     0,     0,     0,     0,     0,   344,
       0,     0,   183,     0,     0,     0,   345,   184,     0,   185,
       0,     0,     0,     0,     0,     0,     0,   186,     0,     0,
      39,   187,     0,     0,     0,   188,   346,   189,    41,     0,
       0,     0,   270,     0,     0,     0,   190,    67,   347,   348,
      43,    69,   349,   350,     0,     0,    45,    46,    47,   180,
     181,   182,     0,     0,     0,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   250,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   183,     0,  -208,     0,
       0,   184,     0,   185,     0,     0,     0,     0,     0,     0,
       0,   186,    39,     0,     0,   187,     0,     0,     0,   188,
      41,   189,     0,     0,     0,     0,     0,   791,     0,     0,
     190,    67,    43,   253,     0,    69,     0,     0,    45,    46,
      47,   180,   181,   182,     0,     0,     0,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   250,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   183,     0,
       0,     0,     0,   184,     0,   185,     0,     0,     0,     0,
       0,     0,     0,   186,     0,     0,    39,   187,     0,     0,
       0,   188,     0,   189,    41,     0,     0,     0,     0,   791,
       0,   227,   190,    67,     0,   253,    43,    69,     0,     0,
       0,     0,    45,    46,    47,   180,   181,   182,     0,   228,
       0,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   183,     0,    39,     0,     0,   184,     0,   185,
       0,     0,    41,     0,     0,     0,     0,   186,     0,     0,
       0,   187,     0,     0,    43,   188,     0,   189,   337,     0,
      45,    46,    47,   180,   181,   182,   190,    67,     0,    53,
      54,    69,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     183,     0,     0,     0,     0,   184,     0,   185,     0,     0,
       0,     0,     0,     0,     0,   186,    39,     0,     0,   187,
     338,     0,     0,   188,    41,   189,     0,     0,     0,     0,
       0,   764,     0,     0,   190,    67,    43,     0,     0,    69,
       0,     0,    45,    46,    47,   180,   181,   182,     0,   765,
       0,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   183,     0,    39,     0,     0,   184,     0,   185,
       0,     0,    41,     0,     0,     0,     0,   186,     0,     0,
       0,   187,     0,     0,    43,   188,     0,   189,     0,     0,
      45,    46,    47,   180,   181,   182,   190,    67,     0,    53,
      54,    69,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   366,
     183,     0,    39,     0,     0,   184,     0,   185,     0,     0,
      41,     0,     0,     0,     0,   186,     0,     0,     0,   187,
       0,     0,    43,   188,     0,   189,   337,     0,    45,    46,
      47,   180,   181,   182,   190,    67,     0,    53,    54,    69,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   183,     0,
      39,     0,     0,   184,     0,   185,     0,     0,    41,     0,
       0,     0,     0,   186,     0,     0,     0,   187,     0,     0,
      43,   188,     0,   189,   576,     0,    45,    46,    47,   180,
     181,   182,   190,    67,     0,    53,    54,    69,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   183,     0,    39,     0,
       0,   184,     0,   185,     0,     0,    41,     0,     0,     0,
       0,   186,     0,     0,     0,   187,     0,     0,    43,   188,
       0,   189,     0,     0,    45,    46,    47,   180,   181,   182,
     190,    67,     0,    53,    54,    69,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   183,     0,    39,     0,     0,   184,
       0,   185,     0,     0,    41,     0,     0,     0,     0,   186,
       0,     0,     0,   187,     0,     0,    43,   188,   798,   189,
       0,     0,    45,    46,    47,   180,   181,   182,   190,    67,
       0,    53,    54,    69,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   823,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   183,     0,    39,     0,     0,   184,     0,   185,
       0,     0,    41,     0,     0,     0,     0,   186,     0,     0,
       0,   187,     0,     0,    43,   188,     0,   189,     0,     0,
      45,    46,    47,   180,   181,   182,   190,    67,     0,    53,
      54,    69,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   826,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     183,     0,    39,     0,     0,   184,     0,   185,     0,     0,
      41,     0,     0,     0,     0,   186,     0,     0,     0,   187,
       0,     0,    43,   188,     0,   189,   337,     0,    45,    46,
      47,   180,   181,   182,   190,    67,     0,    53,    54,    69,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   183,     0,
      39,     0,     0,   184,     0,   185,     0,     0,    41,     0,
       0,     0,     0,   186,     0,     0,     0,   187,     0,     0,
      43,   845,     0,   189,     0,     0,    45,    46,    47,   180,
     181,   182,   190,    67,     0,    53,    54,    69,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   183,     0,     0,     0,
       0,   184,     0,   185,     0,     0,     0,     0,     0,     0,
       0,   186,    39,     0,     0,   187,    40,     0,     0,   188,
      41,   189,     0,     0,     0,     0,     0,     0,     0,    42,
     190,    67,    43,     0,     0,    69,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    39,
      55,    56,    57,     0,     0,    58,     0,    41,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,    43,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    39,     0,     0,     0,     0,     0,
       0,    65,    41,     0,   319,     0,     0,     0,     0,     0,
       0,   597,    66,    67,    43,     0,    68,    69,    44,     0,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,    39,    55,    56,    57,     0,     0,    58,    65,    41,
       0,    59,     0,     0,    60,    61,    62,    63,    64,    66,
      67,    43,     0,    68,    69,    44,     0,    45,    46,    47,
      48,    49,    50,     0,    51,    52,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    39,     0,     0,     0,
       0,     0,     0,    65,    41,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    66,    67,    43,     0,    68,    69,
      44,     0,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,    39,    55,    56,    57,     0,     0,    58,
      65,    41,     0,    59,     0,     0,    60,    61,    62,    63,
      64,    66,    67,    43,     0,    68,    69,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    39,     0,
       0,     0,     0,     0,     0,     0,    41,     0,   469,     0,
       0,     0,     0,     0,     0,     0,    66,   115,    43,     0,
      68,   116,    44,     0,    45,    46,    47,    48,    49,    50,
       0,    51,    52,    53,    54,    39,    55,    56,    57,     0,
       0,    58,   236,    41,     0,    59,     0,     0,    60,    61,
      62,    63,    64,    66,     0,    43,     0,    68,     0,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,    39,     0,     0,
       0,     0,     0,     0,     0,    41,     0,   236,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    43,    66,     0,
       0,    44,    68,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,    39,    55,    56,    57,     0,     0,
      58,     0,    41,     0,    59,     0,     0,    60,    61,    62,
      63,    64,     0,     0,    43,    66,   115,     0,     0,     0,
      45,    46,    47,   180,   181,   182,     0,     0,     0,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   331,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    66,     0,     0,
       0,     0,   729,     0,     0,     0,     0,     0,     0,    39,
       0,     0,     0,     0,   730,   616,     0,    41,     0,     0,
       0,     0,     0,   617,     0,     0,     0,     0,     0,    43,
       0,     0,     0,    44,   190,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    39,    55,    56,    57,
       0,     0,    58,     0,    41,     0,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,    43,     0,     0,     0,
       0,     0,    45,    46,    47,   180,   181,   182,     0,     0,
       0,    53,    54,    39,    55,    56,    57,     0,     0,    58,
       0,    41,     0,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,    43,     0,     0,     0,     0,     0,    45,
      46,    47,   180,   181,   182,     0,     0,     0,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,    66,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,   615,   616,     0,     0,
       0,     0,     0,     0,     0,   617,     0,     0,     0,    39,
       0,     0,     0,     0,     0,     0,   190,    41,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    43,
       0,     0,     0,   788,   616,    45,    46,    47,   180,   181,
     182,     0,   617,     0,    53,    54,    39,    55,    56,    57,
       0,     0,    58,   190,    41,     0,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,    43,     0,     0,     0,
       0,     0,    45,    46,    47,   180,   181,   182,     0,     0,
       0,    53,    54,    39,    55,    56,    57,     0,     0,    58,
       0,    41,   625,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,    43,     0,     0,     0,     0,     0,    45,
      46,    47,   180,   181,   182,     0,     0,     0,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,   190,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   617,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   190,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   190
  };

  const short
  parser::yycheck_[] =
  {
      29,    89,    34,   106,    89,    34,   106,   361,   328,   113,
      34,   101,   101,   111,   187,   290,   101,   103,   577,    40,
     268,    42,    29,   577,   217,   186,   458,   223,   224,   324,
     111,   112,   194,   294,   296,   222,   576,   660,   183,   184,
     104,   227,   239,   107,   108,   661,   110,     5,     1,     1,
      12,   196,     1,     1,   670,    19,    65,     1,     1,    88,
      21,   309,   302,    84,    29,    94,     1,   497,   112,   231,
      78,   488,    27,   137,    78,   104,   105,   737,   107,   108,
     109,   110,   111,   112,   188,   189,   516,   111,   112,   384,
     135,   288,   814,    78,   377,   106,   177,   284,    81,    41,
     106,   396,    82,   112,   737,   120,   109,   537,   137,   119,
      52,    53,   120,   119,   204,   731,    27,    22,   113,   204,
     842,   211,   102,   106,    19,   665,   211,   187,   188,   144,
      85,   135,   135,   177,   179,    66,    67,   227,   228,   168,
     105,   170,   227,   228,   105,   109,    51,    29,   177,    54,
     135,   113,   582,   177,   186,   108,   108,   186,   420,   108,
     108,   168,   307,   170,   108,   108,   119,   119,   177,   829,
     119,   119,     0,   108,    85,   119,   119,    72,   418,   427,
     463,    91,   246,   252,   248,   214,   215,   604,   375,   376,
      81,   223,   224,   488,   223,   224,   829,   370,   831,   223,
     224,    80,    14,   168,   292,   170,    81,   292,   113,    91,
     252,   256,    88,   135,   172,   173,    78,   246,    94,   248,
     125,   366,   119,   252,   129,    81,   136,   236,   297,   120,
     403,    81,   261,   293,    79,   114,   268,   261,   861,   268,
      85,   120,    78,   114,    81,   120,   862,   311,   127,   120,
     866,   571,   572,   282,    81,   297,   285,   286,   120,    81,
      65,   693,   294,    81,   120,   294,   295,   296,   297,   359,
     120,   120,   362,   302,   359,   120,    78,   309,   310,   288,
     309,   310,   311,   120,   545,    27,   168,    94,   170,   848,
      97,   645,   378,   120,   848,   144,   102,   104,   120,    24,
     360,   110,   120,   106,   325,   106,   114,   112,   114,   412,
     370,   120,   412,   402,   106,   610,   119,   282,   119,   126,
     285,   286,   331,   421,   756,   423,   588,   119,   501,   759,
     135,   761,   430,   494,   432,   106,   113,   533,   214,   215,
     421,   401,   423,   403,   444,   310,   594,   119,   119,   430,
     395,   432,   514,   109,   109,   113,    37,   113,   113,   115,
     115,   125,   106,   451,   113,   129,   451,   125,   397,   125,
     125,   129,   177,   129,   179,   119,   125,    14,   575,   424,
     129,   114,   125,   813,   136,   414,   129,   120,   650,   418,
     419,   420,   421,   109,   423,   427,   114,   421,   427,   423,
      88,   430,   120,   432,   114,   116,   430,   727,   432,   120,
     120,   135,   444,   708,   116,   444,   114,   116,   120,   295,
     296,   120,   116,   718,   778,   114,   120,   102,   690,   114,
     105,   236,   492,   790,    81,    82,   793,   497,   498,     4,
      80,   501,   638,   639,   637,   549,   550,    87,    81,    82,
     479,   256,    92,   114,    94,   484,   516,    19,   487,   488,
      22,   634,   494,   114,   105,   494,   107,   821,    73,    74,
      75,    76,   479,    81,    82,    94,   536,   537,    97,   361,
     105,   121,   107,    80,    23,   104,   126,   127,   120,    51,
     130,   131,    54,    55,    85,   593,   525,    94,   527,   774,
     529,   533,   797,   751,   533,    19,   860,   126,    85,   533,
      87,   130,   593,   545,   479,   113,   545,   703,   105,   484,
     107,   397,   582,   668,   121,   105,   105,   107,   107,   126,
     127,   826,   827,   130,   131,    81,    82,   857,   414,   105,
     737,   107,    92,   419,   420,   421,   105,   105,   107,   107,
     122,   123,    19,    78,    78,   615,   119,    78,   587,   588,
     592,    78,   594,   592,   593,   594,   575,   596,    15,   593,
      82,   120,   662,   110,   634,   604,   597,   662,   764,    80,
     776,   110,   114,   114,   114,    72,    87,   773,   135,    81,
     395,   114,   737,    94,   114,   627,   628,   479,   627,   628,
     106,   120,   484,   627,   628,   120,   638,   639,   106,   638,
     639,   487,   488,   703,   638,   639,   110,    92,   703,   424,
     121,   650,   651,   114,   786,   126,   127,   592,   114,   130,
     131,   596,   829,   114,   495,   120,    27,    81,    86,   729,
     730,    86,   730,    25,   729,   790,   507,    27,   793,   525,
      82,   527,   750,   529,   744,   135,   685,    78,    80,   744,
     689,   690,   691,   723,   114,    87,   852,    78,   121,   750,
     121,   136,    94,   763,   764,   765,   110,   538,   763,   764,
     765,    78,    78,   773,   829,   120,   831,    81,   773,   779,
     780,   791,   114,   796,   779,   780,   796,    81,   120,   759,
      27,   761,   120,    82,   126,   127,   121,   106,   130,   131,
     120,   587,   588,    82,   596,   102,   102,   120,   113,   751,
     685,   750,   751,   121,   114,   114,   750,   120,   604,   121,
      85,   819,   110,    79,   819,   120,   110,   769,   828,    92,
     769,   845,   771,   828,   776,   769,   836,   776,   121,   121,
      78,   836,   776,   813,   814,   616,   617,    28,    27,    80,
       9,    82,   852,   114,    85,   106,   114,   852,    27,   119,
      80,    81,    80,    94,   650,   651,    97,   806,    81,    85,
     114,    81,    37,   104,    94,   845,    94,    97,    11,   671,
      19,    87,    55,    92,   104,    87,   125,    92,   659,   114,
     121,   120,   106,   685,   106,   126,   127,   114,    85,   130,
     131,   121,    85,   689,   690,   691,   126,   127,   126,   127,
     130,   131,   130,   131,    80,   168,   599,   125,   596,     4,
     685,   671,   746,   860,   780,    34,   714,    12,    94,   810,
     701,    97,   812,   292,   819,   769,   628,   776,   104,    24,
     639,    15,   572,   224,   723,    30,    31,    32,    33,    34,
      35,   370,   836,   745,    39,    40,   538,    42,    43,    44,
     126,   127,    47,   734,   130,   131,    51,   624,   742,    54,
      55,    56,    57,    58,     3,     4,     5,     6,     7,     8,
       9,    10,   833,    12,    13,   771,   625,    16,    17,    18,
      19,    20,   735,    22,   790,    24,   494,    26,   673,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,   745,    42,    43,    44,    34,   109,    47,   299,
     806,    50,    51,   690,   693,    54,    55,    56,    57,    58,
     215,   689,    80,   527,   592,    64,   412,   411,   796,   124,
     125,   575,   246,   814,   129,   256,    94,    -1,    77,    97,
      -1,    -1,    -1,    -1,    83,    -1,   104,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    -1,    95,    -1,    -1,    -1,
      -1,   842,    -1,   121,   845,    -1,    -1,    -1,   126,   127,
     109,    -1,   130,   131,   113,    -1,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   123,   124,   125,    -1,    -1,   128,
     129,    -1,    -1,    -1,    -1,   134,   135,   136,   137,    -1,
      -1,   140,    -1,    -1,    -1,    -1,   145,   146,     3,     4,
       5,    -1,     7,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    16,    17,    18,    -1,    20,    -1,    22,    -1,    24,
      -1,    26,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      80,    -1,    47,    -1,    -1,    50,    51,    -1,    -1,    54,
      55,    56,    57,    58,    94,    -1,    -1,    97,    -1,    64,
      -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    77,    -1,    -1,    -1,    -1,    -1,    83,    -1,
      -1,   121,    -1,    -1,    -1,    90,   126,   127,    93,    -1,
      95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,    -1,    -1,    -1,   113,    -1,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   123,   124,
     125,    -1,    -1,   128,   129,    -1,    -1,    -1,    -1,   134,
     135,   136,   137,    -1,    -1,   140,    -1,    -1,    -1,    -1,
     145,   146,     3,     4,     5,    -1,     7,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    16,    17,    18,    -1,    20,
      -1,    -1,    -1,    24,    -1,    26,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    77,    -1,    -1,    -1,
      -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    93,    -1,    95,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,
      -1,    -1,   113,    -1,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   123,   124,   125,    -1,    -1,   128,   129,    -1,
      -1,    -1,    -1,   134,   135,   136,   137,    -1,    -1,   140,
       3,     4,     5,    -1,   145,   146,    -1,    10,    -1,    12,
      13,    -1,    -1,    16,    17,    18,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    77,    -1,    -1,    -1,    -1,    -1,
      83,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      93,    -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,    -1,    -1,
     113,    -1,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     123,   124,   125,    -1,    -1,   128,   129,    -1,    -1,    -1,
      -1,   134,   135,   136,   137,    -1,     4,   140,    -1,    -1,
       8,    -1,   145,   146,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    21,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,    -1,    42,    43,    44,     4,    -1,    47,
      -1,    -1,    -1,    51,    -1,    12,    54,    55,    56,    57,
      58,    -1,    -1,    -1,    21,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     4,    -1,    -1,   113,    -1,    -1,    -1,    -1,
      12,    -1,    -1,    -1,    -1,    -1,   124,   125,    -1,    -1,
     128,   129,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,   144,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,   113,    -1,    -1,    51,
      -1,    -1,    54,    55,    56,    57,    58,   124,   125,    -1,
      -1,   128,   129,     1,    -1,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,   144,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,   105,    42,    43,    44,    -1,    -1,    47,
      -1,   113,    50,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,   124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    -1,    93,    -1,    95,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     108,   109,    -1,    -1,    -1,   113,    -1,   115,    -1,    -1,
      -1,   119,    -1,    -1,    -1,    -1,   124,   125,    -1,    -1,
     128,   129,    -1,    -1,    -1,    -1,   134,   135,   136,   137,
      -1,    -1,   140,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    -1,    -1,    54,    55,    56,    57,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    94,    95,    -1,    97,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,    -1,    -1,    -1,   109,
      -1,    -1,    -1,   113,   114,   115,    -1,    -1,    -1,    -1,
     120,   121,    -1,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,    -1,    -1,   134,   135,   136,   137,    -1,    -1,
     140,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    -1,    95,    -1,    97,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,    -1,    -1,   109,   110,    -1,
      -1,   113,    -1,   115,    -1,    -1,    -1,    -1,    -1,   121,
      -1,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
      -1,    -1,   134,   135,   136,   137,    -1,    -1,   140,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    -1,    -1,
      54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    83,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,
      -1,    95,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,    -1,    -1,    -1,   109,    -1,    -1,    -1,   113,
      -1,   115,    -1,    -1,    -1,    -1,    -1,   121,    -1,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,    -1,    -1,
     134,   135,   136,   137,    -1,    -1,   140,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    -1,    95,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,    -1,    -1,   109,    -1,    -1,    -1,   113,    -1,   115,
      -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,   124,   125,
      -1,    -1,   128,   129,    -1,    -1,    -1,    -1,   134,   135,
     136,   137,    -1,    -1,   140,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    46,    47,
      -1,    -1,    50,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    -1,    93,    -1,    95,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,    -1,    -1,    -1,   113,    -1,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,    -1,    -1,
     128,   129,    -1,    -1,    -1,    -1,   134,   135,   136,   137,
      -1,    -1,   140,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    -1,    -1,    54,    55,    56,    57,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    -1,    95,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
      -1,    -1,    -1,   113,    -1,   115,    -1,    -1,    -1,   119,
      -1,    -1,    -1,    -1,   124,   125,    -1,    -1,   128,   129,
      -1,    -1,    -1,    -1,   134,   135,   136,   137,    -1,    -1,
     140,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,    -1,
      -1,   113,    -1,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,    -1,    -1,   128,   129,    -1,    -1,
      -1,    -1,   134,   135,   136,   137,    -1,    -1,   140,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    -1,    -1,
      54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,
      -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   109,    -1,    -1,    -1,   113,
      -1,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,    -1,    -1,   128,   129,    -1,    -1,    -1,    -1,
     134,   135,   136,   137,    -1,    -1,   140,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    -1,    -1,    95,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,    -1,    -1,    -1,   113,    -1,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
      -1,    -1,   128,   129,    -1,    -1,    -1,    -1,   134,   135,
     136,   137,    -1,    -1,   140,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,
      -1,    89,    90,    -1,    -1,    -1,    -1,    95,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,    -1,    -1,    -1,   113,    -1,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,    -1,    -1,
     128,   129,    -1,    -1,    -1,    -1,   134,   135,   136,   137,
      -1,    -1,   140,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    -1,    -1,    54,    55,    56,    57,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    83,    -1,    -1,    -1,    87,    -1,    -1,
      90,    -1,    -1,    -1,    -1,    95,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
      -1,    -1,    -1,   113,    -1,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,    -1,    -1,   128,   129,
      -1,    -1,    -1,    -1,   134,   135,   136,   137,    -1,    -1,
     140,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    -1,    -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,    -1,
      -1,   113,    -1,   115,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,   124,   125,    12,    -1,   128,   129,    -1,    -1,
      -1,    -1,   134,   135,   136,   137,    24,    -1,   140,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    -1,    90,    -1,    -1,    -1,    94,    95,    -1,    97,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,
       4,   109,    -1,    -1,    -1,   113,   114,   115,    12,    -1,
      -1,    -1,   120,    -1,    -1,    -1,   124,   125,   126,   127,
      24,   129,   130,   131,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,
      54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    92,    -1,
      -1,    95,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   105,     4,    -1,    -1,   109,    -1,    -1,    -1,   113,
      12,   115,    -1,    -1,    -1,    -1,    -1,   121,    -1,    -1,
     124,   125,    24,   127,    -1,   129,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    -1,    -1,    95,    -1,    97,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,    -1,    -1,     4,   109,    -1,    -1,
      -1,   113,    -1,   115,    12,    -1,    -1,    -1,    -1,   121,
      -1,    19,   124,   125,    -1,   127,    24,   129,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,     4,    -1,    -1,    95,    -1,    97,
      -1,    -1,    12,    -1,    -1,    -1,    -1,   105,    -1,    -1,
      -1,   109,    -1,    -1,    24,   113,    -1,   115,    28,    -1,
      30,    31,    32,    33,    34,    35,   124,   125,    -1,    39,
      40,   129,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    -1,    -1,    54,    55,    56,    57,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    -1,    -1,    95,    -1,    97,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   105,     4,    -1,    -1,   109,
     110,    -1,    -1,   113,    12,   115,    -1,    -1,    -1,    -1,
      -1,    19,    -1,    -1,   124,   125,    24,    -1,    -1,   129,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,     4,    -1,    -1,    95,    -1,    97,
      -1,    -1,    12,    -1,    -1,    -1,    -1,   105,    -1,    -1,
      -1,   109,    -1,    -1,    24,   113,    -1,   115,    -1,    -1,
      30,    31,    32,    33,    34,    35,   124,   125,    -1,    39,
      40,   129,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    -1,    -1,    54,    55,    56,    57,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      90,    -1,     4,    -1,    -1,    95,    -1,    97,    -1,    -1,
      12,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,
      -1,    -1,    24,   113,    -1,   115,    28,    -1,    30,    31,
      32,    33,    34,    35,   124,   125,    -1,    39,    40,   129,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
       4,    -1,    -1,    95,    -1,    97,    -1,    -1,    12,    -1,
      -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,    -1,
      24,   113,    -1,   115,    28,    -1,    30,    31,    32,    33,
      34,    35,   124,   125,    -1,    39,    40,   129,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,
      54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,     4,    -1,
      -1,    95,    -1,    97,    -1,    -1,    12,    -1,    -1,    -1,
      -1,   105,    -1,    -1,    -1,   109,    -1,    -1,    24,   113,
      -1,   115,    -1,    -1,    30,    31,    32,    33,    34,    35,
     124,   125,    -1,    39,    40,   129,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,     4,    -1,    -1,    95,
      -1,    97,    -1,    -1,    12,    -1,    -1,    -1,    -1,   105,
      -1,    -1,    -1,   109,    -1,    -1,    24,   113,   114,   115,
      -1,    -1,    30,    31,    32,    33,    34,    35,   124,   125,
      -1,    39,    40,   129,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,     4,    -1,    -1,    95,    -1,    97,
      -1,    -1,    12,    -1,    -1,    -1,    -1,   105,    -1,    -1,
      -1,   109,    -1,    -1,    24,   113,    -1,   115,    -1,    -1,
      30,    31,    32,    33,    34,    35,   124,   125,    -1,    39,
      40,   129,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    -1,    -1,    54,    55,    56,    57,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,     4,    -1,    -1,    95,    -1,    97,    -1,    -1,
      12,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,
      -1,    -1,    24,   113,    -1,   115,    28,    -1,    30,    31,
      32,    33,    34,    35,   124,   125,    -1,    39,    40,   129,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
       4,    -1,    -1,    95,    -1,    97,    -1,    -1,    12,    -1,
      -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,    -1,
      24,   113,    -1,   115,    -1,    -1,    30,    31,    32,    33,
      34,    35,   124,   125,    -1,    39,    40,   129,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,
      54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    -1,
      -1,    95,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   105,     4,    -1,    -1,   109,     8,    -1,    -1,   113,
      12,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    21,
     124,   125,    24,    -1,    -1,   129,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,     4,
      42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
      -1,   113,    12,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    21,   124,   125,    24,    -1,   128,   129,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,     4,    42,    43,    44,    -1,    -1,    47,   113,    12,
      -1,    51,    -1,    -1,    54,    55,    56,    57,    58,   124,
     125,    24,    -1,   128,   129,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,   113,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,    24,    -1,   128,   129,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,     4,    42,    43,    44,    -1,    -1,    47,
     113,    12,    -1,    51,    -1,    -1,    54,    55,    56,    57,
      58,   124,   125,    24,    -1,   128,   129,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,    24,    -1,
     128,   129,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,     4,    42,    43,    44,    -1,
      -1,    47,   113,    12,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,   124,    -1,    24,    -1,   128,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    -1,   113,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,   124,    -1,
      -1,    28,   128,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,     4,    42,    43,    44,    -1,    -1,
      47,    -1,    12,    -1,    51,    -1,    -1,    54,    55,    56,
      57,    58,    -1,    -1,    24,   124,   125,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    -1,    -1,    -1,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    -1,    -1,    54,    55,    56,    57,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,    -1,    -1,
      -1,    -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,   104,   105,    -1,    12,    -1,    -1,
      -1,    -1,    -1,   113,    -1,    -1,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,   124,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,     4,    42,    43,    44,
      -1,    -1,    47,    -1,    12,    -1,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,     4,    42,    43,    44,    -1,    -1,    47,
      -1,    12,    -1,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,   124,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   113,    -1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,   124,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,
      -1,    -1,    -1,   104,   105,    30,    31,    32,    33,    34,
      35,    -1,   113,    -1,    39,    40,     4,    42,    43,    44,
      -1,    -1,    47,   124,    12,    -1,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,     4,    42,    43,    44,    -1,    -1,    47,
      -1,    12,    87,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,   124,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   113,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   105,   148,   149,   150,   153,   125,   129,   361,
     154,   165,     0,   154,    66,    67,   151,   106,   119,   155,
     166,   167,     1,   108,   360,   109,   135,   224,   224,   113,
     156,    14,   168,   179,   180,   135,   225,    78,    78,     4,
       8,    12,    21,    24,    28,    30,    31,    32,    33,    34,
      35,    37,    38,    39,    40,    42,    43,    44,    47,    51,
      54,    55,    56,    57,    58,   113,   124,   125,   128,   129,
     144,   157,   158,   159,   163,   327,   330,   331,   344,   346,
     347,   353,    27,    24,   169,   119,   164,     3,     5,     6,
       7,     8,     9,    10,    13,    16,    17,    18,    19,    20,
      22,    26,    29,    36,    50,    64,    77,    83,    90,    93,
      95,   109,   113,   115,   123,   125,   129,   134,   135,   136,
     137,   140,   145,   146,   177,   181,   183,   184,   185,   187,
     191,   203,   229,   273,   278,   282,   283,   285,   286,   287,
     288,   316,   317,   320,   321,   343,   344,   347,   355,   356,
     359,   110,   120,   361,   361,    80,    94,    97,   104,   126,
     127,   130,   131,   350,   351,   352,   354,   114,   120,   144,
     113,   160,   105,   107,   152,   361,   119,   113,   281,   282,
      33,    34,    35,    90,    95,    97,   105,   109,   113,   115,
     124,   209,   234,   236,   238,   239,   241,   245,   325,   326,
     330,   340,   342,   353,    37,   204,   330,    19,    22,    51,
      54,    55,   189,   190,   105,   107,   308,   281,    73,    74,
      75,    76,   188,   105,   107,   221,   222,    19,    37,   186,
     236,   238,   326,    14,   308,   286,   113,   344,   109,   113,
     318,   319,   321,   356,   286,   306,   307,   286,   285,   286,
      80,   110,   121,   127,   131,   281,   282,   290,   292,   293,
     323,   337,   339,   349,   350,   352,   357,   358,   103,   114,
     120,   289,   290,   291,   350,   351,   352,   357,   362,   116,
     362,   109,   279,   280,    19,   279,   279,   136,   176,   164,
      19,    72,   211,    81,   120,    82,    85,   121,   275,   276,
     277,   323,   336,   338,   348,   350,   351,    89,   286,   102,
     105,    88,   135,   114,   114,   114,   114,   114,   159,    79,
     161,   162,   163,   154,   154,     4,   170,    23,    81,   245,
     245,   113,   229,   266,   267,   268,   347,    28,   110,   231,
     232,   234,   236,    80,    87,    94,   114,   126,   127,   130,
     131,   231,   249,   332,   333,   362,   362,    85,   253,    92,
      87,   121,   243,   328,   332,   341,    89,   242,   245,   236,
     113,   227,   232,   246,   236,    19,    19,    20,    46,   281,
     305,   309,   310,   311,   309,   119,   284,    78,    78,    78,
      78,   246,   220,   273,   274,   282,   220,    15,   198,   236,
     236,    81,   120,    82,    41,    52,    53,   182,    78,   135,
     358,    81,   120,   226,    87,   306,   346,   355,   336,    79,
      85,   120,   110,   120,   282,   345,   347,   102,   114,   114,
     120,   114,   120,   114,   114,   114,   120,   116,    91,   136,
     344,   246,   344,   344,   121,   178,   322,   334,   335,   351,
     358,   211,   135,   209,   228,   232,   233,   343,   281,   295,
     296,   311,   346,    27,   223,   277,   283,   245,   345,    79,
     312,   313,   314,   344,   345,   347,   286,   114,   114,   120,
     106,   360,   361,    12,   113,   171,   172,   105,   107,   297,
     227,   351,    81,   106,   120,   250,   110,    81,    92,   114,
     114,   120,   114,   114,   116,   254,   255,   256,    27,   215,
     236,   232,   330,   342,   238,   245,    81,   206,   231,   248,
     249,   246,   246,   222,   308,    86,   106,   119,   360,    25,
      27,   219,   106,   119,   360,   281,    82,    81,    82,   207,
     232,   257,   113,   326,   231,   135,    78,   114,   109,   113,
     115,   324,   325,   318,    78,   281,   121,   121,   281,   294,
     311,   281,   290,   290,   345,   290,   290,   136,   110,    78,
      78,    81,    81,   347,   356,   120,    28,   210,   234,   236,
      78,   135,    81,    82,   205,   261,   223,    82,   120,   121,
     222,   106,   120,    82,   102,   163,   113,    21,   144,   163,
     173,   174,   175,   106,   119,   281,   298,   299,   300,   304,
     298,   360,   114,   232,   268,   104,   105,   113,   251,   252,
     340,   257,   232,   231,   120,    87,   340,   105,   107,   214,
     121,   121,   257,   114,   120,   281,   310,   281,   105,   107,
     218,   274,   232,   257,   251,    85,   192,   343,   114,   116,
     120,    79,   110,   227,   230,   230,   121,   121,   334,   250,
     205,   261,    92,    78,   257,    28,   262,   263,   264,    27,
     258,     9,   269,   270,   271,   281,   311,   313,   290,   315,
     345,   173,   361,   160,   114,   120,   144,   298,   106,   119,
      85,    87,   301,   302,   303,   360,   232,   340,   340,   114,
     255,   256,     7,    26,   199,   212,   213,   274,   213,   231,
     284,     7,    26,   202,   203,   216,   217,   274,   217,   193,
     342,    27,   195,    81,   311,   281,    78,   120,    78,    92,
     104,   258,   269,   236,   250,    85,   235,   240,   244,   245,
     265,   105,   107,   269,   113,   189,   272,   329,   330,   271,
      82,   102,   114,   175,   300,   295,   281,   223,   303,    81,
     106,    81,    37,   200,    19,    37,   198,   236,   106,   119,
     360,    11,    19,   201,   201,   106,   119,   360,    87,   105,
     107,   196,   228,   227,   236,   234,   238,   269,   104,   263,
      92,   121,   244,   322,   259,   260,   284,   259,   114,   236,
     237,   247,   272,   190,   290,   345,    87,   223,   257,   257,
     236,   198,   236,    81,    82,   208,   212,   281,   198,   211,
     216,   194,   342,    79,   197,   198,    79,   197,    92,   240,
     265,   240,   106,   119,   319,   360,   120,   114,   281,   106,
     114,   206,    82,   208,   257,   113,   252,   340,   210,   342,
     106,   106,   119,   360,   360,   236,   260,    81,   247,   340,
      85,   205,   261,   198,   227,   193,   258,   269,   269
  };

  const short
  parser::yyr1_[] =
  {
       0,   147,   148,   149,   149,   150,   151,   151,   151,   152,
     152,   153,   153,   154,   155,   155,   155,   156,   156,   157,
     157,   157,   157,   158,   158,   159,   159,   159,   160,   160,
     160,   161,   161,   162,   162,   163,   163,   164,   164,   165,
     165,   166,   167,   167,   168,   169,   169,   170,   170,   171,
     171,   172,   172,   173,   173,   173,   173,   174,   174,   175,
     175,   176,   176,   177,   177,   177,   178,   178,   179,   180,
     180,   181,   181,   181,   181,   181,   181,   181,   181,   181,
     182,   182,   182,   183,   184,   184,   184,   184,   184,   185,
     186,   186,   187,   187,   187,   187,   188,   188,   188,   188,
     188,   189,   189,   189,   190,   191,   191,   191,   192,   192,
     193,   194,   194,   195,   195,   196,   196,   196,   196,   197,
     197,   197,   197,   198,   199,   199,   199,   199,   199,   200,
     200,   201,   201,   202,   202,   202,   203,   203,   204,   204,
     205,   205,   206,   206,   207,   207,   207,   208,   208,   208,
     209,   209,   210,   210,   210,   210,   211,   211,   211,   212,
     212,   213,   213,   213,   213,   214,   214,   215,   215,   216,
     216,   217,   217,   217,   217,   218,   218,   219,   219,   220,
     220,   220,   220,   221,   221,   222,   223,   223,   224,   224,
     225,   225,   225,   226,   226,   227,   228,   229,   229,   230,
     230,   231,   231,   232,   232,   232,   233,   234,   235,   236,
     236,   237,   238,   239,   239,   240,   240,   241,   241,   241,
     242,   243,   243,   244,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   246,   247,   247,   248,   248,   249,
     249,   250,   250,   251,   251,   251,   252,   252,   253,   253,
     254,   254,   255,   256,   256,   257,   258,   258,   258,   259,
     259,   260,   261,   262,   262,   263,   263,   264,   264,   265,
     265,   266,   266,   267,   267,   268,   269,   269,   270,   270,
     271,   271,   271,   272,   272,   272,   273,   273,   274,   275,
     275,   276,   276,   277,   278,   278,   278,   278,   278,   278,
     278,   278,   278,   279,   279,   280,   280,   281,   281,   282,
     282,   283,   283,   284,   284,   285,   285,   285,   285,   286,
     286,   286,   286,   286,   286,   286,   286,   286,   286,   287,
     287,   287,   288,   288,   288,   288,   288,   288,   288,   288,
     289,   289,   290,   290,   290,   291,   291,   292,   292,   292,
     292,   292,   292,   292,   293,   293,   294,   294,   295,   296,
     296,   297,   297,   297,   297,   298,   298,   299,   299,   299,
     300,   301,   301,   302,   302,   303,   304,   305,   306,   307,
     307,   308,   308,   309,   309,   309,   309,   310,   310,   311,
     311,   311,   312,   312,   313,   313,   313,   314,   314,   314,
     314,   315,   315,   316,   316,   317,   317,   318,   318,   318,
     319,   319,   320,   320,   320,   320,   321,   321,   322,   322,
     323,   323,   324,   324,   324,   325,   325,   325,   325,   325,
     326,   326,   327,   327,   327,   327,   328,   328,   329,   330,
     330,   331,   332,   332,   332,   333,   333,   333,   333,   334,
     334,   335,   335,   336,   336,   337,   337,   338,   338,   339,
     339,   340,   341,   342,   342,   342,   342,   342,   343,   343,
     344,   344,   344,   345,   346,   346,   347,   347,   347,   347,
     347,   347,   347,   347,   348,   348,   349,   349,   350,   351,
     351,   352,   352,   353,   353,   353,   353,   353,   353,   353,
     353,   353,   353,   353,   353,   353,   353,   353,   353,   353,
     353,   354,   354,   355,   355,   356,   357,   357,   358,   358,
     359,   359,   359,   359,   359,   360,   360,   361,   361,   362,
     362
  };

  const signed char
  parser::yyr2_[] =
  {
       0,     2,     1,     6,     1,     0,     3,     3,     0,     3,
       3,     3,     3,     2,     2,     2,     1,     3,     0,     1,
       0,     2,     1,     3,     1,     2,     2,     2,     0,     3,
       3,     0,     1,     3,     1,     1,     1,     2,     1,     2,
       0,     2,     3,     0,     5,     1,     0,     2,     0,     1,
       0,     3,     4,     1,     0,     2,     1,     3,     1,     2,
       2,     0,     1,     1,     1,     1,     3,     1,     2,     3,
       0,     1,     1,     1,     1,     1,     5,     7,     1,     1,
       1,     1,     1,     4,     4,     5,     6,     6,     4,     4,
       3,     1,     4,     3,     6,     7,     2,     2,     2,     2,
       0,     1,     1,     1,     2,     3,     4,     4,     0,     2,
       3,     2,     1,     0,     2,     3,     3,     3,     3,     3,
       2,     1,     0,     3,     4,     3,     4,     2,     3,     0,
       1,     0,     1,     3,     6,     7,     1,     1,     0,     1,
       0,     2,     0,     2,     0,     2,     2,     0,     2,     4,
       3,     1,     6,     4,     3,     1,     4,     3,     0,     1,
       1,     3,     2,     1,     0,     3,     3,     2,     0,     1,
       1,     3,     2,     1,     0,     3,     3,     2,     0,     3,
       2,     1,     0,     3,     3,     1,     2,     0,     1,     3,
       3,     1,     0,     0,     2,     1,     1,     3,     1,     1,
       3,     1,     3,     4,     3,     1,     1,     1,     1,     1,
       3,     1,     1,     1,     3,     1,     2,     1,     2,     3,
       1,     1,     1,     1,     1,     1,     1,     2,     2,     3,
       2,     5,     3,     3,     1,     1,     3,     1,     0,     1,
       3,     2,     0,     1,     3,     5,     1,     5,     0,     2,
       3,     1,     3,     2,     0,     1,     4,     4,     0,     3,
       1,     4,     2,     3,     1,     4,     2,     3,     0,     1,
       3,     0,     1,     3,     1,     3,     0,     1,     2,     1,
       2,     3,     3,     1,     2,     3,     1,     2,     1,     3,
       2,     2,     1,     4,     3,     3,     4,     4,     3,     4,
       6,     6,     4,     0,     1,     3,     4,     3,     1,     1,
       3,     2,     1,     1,     0,     2,     3,     2,     1,     3,
       2,     2,     4,     4,     8,     4,     2,     2,     1,     4,
       3,     1,     1,     1,     1,     3,     3,     3,     3,     1,
       3,     2,     1,     2,     2,     3,     3,     1,     1,     2,
       4,     3,     5,     3,     3,     3,     3,     1,     1,     3,
       1,     3,     3,     2,     2,     1,     2,     3,     2,     1,
       2,     3,     2,     2,     1,     4,     1,     1,     1,     2,
       1,     3,     3,     3,     2,     1,     0,     1,     2,     3,
       1,     2,     1,     0,     3,     1,     1,     3,     1,     5,
       3,     3,     1,     1,     1,     1,     3,     1,     3,     1,
       3,     1,     2,     3,     2,     3,     1,     2,     1,     3,
       1,     3,     1,     2,     2,     1,     3,     3,     3,     2,
       1,     3,     1,     3,     3,     3,     1,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     1,     1,     1,     1,     1,     3,     1,
       3,     1,     3,     1,     1,     1,     1,     1,     1,     3,
       1,     3,     3,     1,     1,     1,     1,     1,     1,     1,
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
  "\"type\"", "\"where\"", "\"forall\"", "\"foreign\"", "\"export\"",
  "\"label\"", "\"dynamic\"", "\"safe\"", "\"interruptible\"",
  "\"unsafe\"", "\"mdo\"", "\"family\"", "\"role\"", "\"stdcall\"",
  "\"ccall\"", "\"bpcall\"", "\"capi\"", "\"prim\"", "\"javascript\"",
  "\"proc\"", "\"rec\"", "\"group\"", "\"by\"", "\"using\"", "\"static\"",
  "\"stock\"", "\"trcall\"", "\"ecall\"", "\"anyclass\"", "\"via\"",
  "\"unit\"", "\"signature\"", "\"dependency\"", "\"{-# SPECIALIZE\"",
  "\"{-# SPECIALIZE_INLINE\"", "\"{-# SOURCE\"", "\"{-# RULES\"",
  "\"{-# CORE\"", "\"{-# SCC\"", "\"{-# GENERATED\"", "\"{-# DEPRECATED\"",
  "\"{-# WARNING\"", "\"{-# UNPACK\"", "\"{-# NOUNPACK\"", "\"{-# ANN\"",
  "\"{-# MINIMAL\"", "\"{-# CTYPE\"", "\"{-# OVERLAPPING\"",
  "\"{-# OVERLAPPABLE\"", "\"{-# OVERLAPS\"", "\"{-# INCOHERENT\"",
  "\"{-# COMPLETE\"", "\"#-}\"", "\"..\"", "\":\"", "\"::\"", "\"=\"",
  "\"\\\\\"", "\"lcase\"", "\"|\"", "\"<-\"", "\"->\"", "TIGHT_INFIX_AT",
  "\"@\"", "PREFIX_TILDE", "\"~\"", "\"=>\"", "PREFIX_MINUS", "\"-\"",
  "PREFIX_BANG", "\"!\"", "\"*\"", "\"-<\"", "\">-\"", "\"-<<\"",
  "\">>-\"", "TIGHT_INFIX_DOT", "PREFIX_DOT", "\".\"", "\"{\"", "\"}\"",
  "\"vocurly\"", "\"vccurly\"", "\"[\"", "\"]\"", "\"[:\"", "\":]\"",
  "\"(\"", "\")\"", "\"(#\"", "\"#)\"", "\"(|\"", "\"|)\"", "\";\"",
  "\",\"", "\"`\"", "\"'\"", "\"{-# INLINE\"", "\"VARID\"", "\"CONID\"",
  "\"VARSYM\"", "\"CONSYM\"", "\"QVARID\"", "\"QCONID\"", "\"QVARSYM\"",
  "\"QCONSYM\"", "\"IPDUPVARID\"", "\"LABELVARID\"", "\"CHAR\"",
  "\"STRING\"", "\"INTEGER\"", "\"RATIONAL\"", "\"PRIMCHAR\"",
  "\"PRIMSTRING\"", "\"PRIMINTEGER\"", "\"PRIMWORD\"", "\"PRIMFLOAT\"",
  "\"PRIMDOUBLE\"", "','", "\"{-# SPECIALISE\"",
  "\"{-# SPECIALISE_INLINE\"", "$accept", "unit", "module",
  "missing_module_keyword", "maybemodwarning", "body", "body2", "top",
  "top1", "maybeexports", "exportlist", "exportlist1", "export",
  "export_subspec", "qcnames", "qcnames1", "qcname", "semis1", "semis",
  "importdecls", "importdecls_semi", "importdecl", "optqualified",
  "maybeas", "maybeimpspec", "impspec", "importlist", "importlist1",
  "import", "prec", "infix", "ops", "topdecls", "topdecls_semi", "topdecl",
  "call_conv", "cl_decl", "ty_decl", "standalone_kind_sig", "sks_vars",
  "inst_decl", "overlap_pragma", "deriv_strategy_no_via",
  "deriv_strategy_via", "stand_alone_deriving", "opt_injective_info",
  "injectivity_cond", "inj_varids", "where_type_family",
  "ty_fam_inst_eqn_list", "ty_fam_inst_eqns", "ty_fam_inst_eqn",
  "at_decl_cls", "opt_family", "opt_instance", "at_decl_inst",
  "data_or_newtype", "opt_class", "opt_kind_sig", "opt_datafam_kind_sig",
  "opt_tyfam_kind_sig", "opt_at_kind_inj_sig", "tycl_hdr",
  "datafam_inst_hdr", "capi_ctype", "decl_cls", "decls_cls",
  "decllist_cls", "where_cls", "decl_inst", "decls_inst", "decllist_inst",
  "where_inst", "decls", "decllist", "binds", "wherebinds", "strings",
  "stringlist", "opt_tyconsig", "sigtype", "sigtypedoc", "sig_vars",
  "sigtypes1", "ktype", "ctype", "ctypedoc", "context", "context_no_ops",
  "type", "typedoc", "btype", "infixtype", "btype_no_ops", "ftype",
  "tyarg", "tyop", "atype_docs", "atype", "inst_type", "deriv_types",
  "comma_types0", "comma_types1", "tv_bndrs", "tv_bndr",
  "tv_bndr_no_braces", "fds", "fds1", "fd", "varids0", "kind",
  "gadt_constrlist", "gadt_constrs", "gadt_constr", "constrs", "constrs1",
  "constr", "forall", "constr_stuff", "fielddecls", "fielddecls1",
  "fielddecl", "maybe_derivings", "derivings", "deriving",
  "deriv_clause_types", "decl_no_th", "decl", "rhs", "gdrhs", "gdrh",
  "sigdecl", "activation", "explicit_activation", "exp", "infixexp",
  "exp10", "optSemi", "fexp", "aexp", "aexp1", "aexp2", "projection",
  "texp", "tup_exprs", "list", "lexps", "squals", "guardquals",
  "guardquals1", "altslist", "alts", "alts1", "alt", "alt_rhs", "gdpats",
  "gdpat", "pat", "bindpat", "apat", "apats1", "stmtlist", "stmts", "stmt",
  "qual", "fbinds", "fbinds1", "fbind", "fieldToUpdate", "qcon",
  "gen_qcon", "con", "con_list", "sysdcon_no_list", "sysdcon", "conop",
  "qconop", "gtycon", "ntgtycon", "oqtycon", "oqtycon_no_varcon",
  "qtyconop", "qtycondoc", "qtycon", "tycon", "qtyconsym", "tyconsym",
  "op", "varop", "qop", "qopm", "qvarop", "qvaropm", "tyvar", "tyvarop",
  "tyvarid", "var", "qvar", "field", "qvarid", "varid", "qvarsym",
  "qvarsym_no_minus", "qvarsym1", "varsym", "varsym_no_minus",
  "special_id", "special_sym", "qconid", "conid", "qconsym", "consym",
  "literal", "close", "modid", "commas", YY_NULLPTR
  };
#endif


#if YYDEBUG
  const short
  parser::yyrline_[] =
  {
       0,   519,   519,   536,   537,   539,   543,   544,   545,   547,
     548,   550,   551,   554,   556,   557,   558,   566,   567,   569,
     570,   571,   572,   574,   575,   577,   578,   579,   581,   582,
     583,   585,   586,   588,   589,   591,   592,   596,   597,   599,
     600,   602,   604,   605,   607,   620,   621,   623,   624,   626,
     627,   631,   632,   634,   635,   636,   637,   639,   640,   642,
     643,   648,   649,   651,   652,   653,   655,   656,   660,   662,
     663,   665,   666,   667,   668,   669,   671,   672,   678,   680,
     683,   684,   685,   687,   690,   691,   693,   694,   695,   697,
     699,   700,   703,   704,   705,   711,   718,   719,   720,   721,
     722,   724,   725,   726,   728,   730,   732,   734,   739,   740,
     742,   744,   745,   749,   750,   752,   753,   754,   755,   757,
     758,   759,   760,   762,   765,   767,   769,   771,   772,   774,
     774,   776,   776,   780,   782,   789,   796,   797,   800,   801,
     805,   806,   808,   809,   811,   812,   813,   815,   816,   817,
     820,   821,   824,   825,   826,   827,   829,   830,   831,   873,
     874,   876,   877,   878,   879,   881,   882,   884,   885,   887,
     888,   890,   891,   892,   893,   895,   896,   898,   899,   902,
     903,   904,   905,   907,   908,   910,   912,   913,   921,   922,
     924,   925,   926,   939,   940,   949,   951,   953,   954,   956,
     957,   966,   967,   969,   970,   972,   974,   983,   985,   987,
     988,   990,   993,   995,   996,   998,   999,  1001,  1003,  1004,
    1006,  1008,  1009,  1016,  1023,  1024,  1025,  1026,  1027,  1028,
    1029,  1030,  1036,  1037,  1040,  1042,  1043,  1045,  1046,  1048,
    1049,  1056,  1057,  1059,  1060,  1061,  1064,  1065,  1069,  1070,
    1072,  1073,  1076,  1078,  1079,  1084,  1090,  1091,  1092,  1094,
    1095,  1097,  1099,  1101,  1102,  1104,  1105,  1107,  1108,  1110,
    1111,  1117,  1118,  1120,  1121,  1123,  1125,  1126,  1128,  1129,
    1131,  1136,  1141,  1147,  1148,  1149,  1154,  1156,  1158,  1162,
    1163,  1165,  1166,  1170,  1180,  1181,  1183,  1184,  1185,  1186,
    1187,  1188,  1189,  1192,  1193,  1195,  1196,  1201,  1202,  1206,
    1207,  1209,  1210,  1212,  1213,  1218,  1219,  1220,  1221,  1224,
    1225,  1226,  1227,  1228,  1230,  1232,  1233,  1234,  1236,  1239,
    1240,  1241,  1244,  1245,  1246,  1247,  1248,  1249,  1254,  1255,
    1258,  1259,  1264,  1265,  1266,  1271,  1272,  1290,  1291,  1292,
    1293,  1294,  1295,  1296,  1298,  1299,  1312,  1314,  1324,  1326,
    1327,  1330,  1331,  1332,  1333,  1335,  1336,  1338,  1339,  1340,
    1342,  1344,  1345,  1347,  1348,  1357,  1359,  1361,  1363,  1365,
    1366,  1369,  1370,  1372,  1373,  1374,  1375,  1380,  1381,  1383,
    1384,  1385,  1390,  1391,  1393,  1394,  1395,  1397,  1398,  1399,
    1400,  1403,  1404,  1436,  1437,  1439,  1440,  1442,  1443,  1444,
    1446,  1447,  1449,  1450,  1451,  1452,  1454,  1455,  1457,  1458,
    1460,  1461,  1464,  1465,  1466,  1468,  1469,  1470,  1471,  1472,
    1474,  1475,  1477,  1478,  1479,  1480,  1483,  1484,  1486,  1488,
    1489,  1493,  1495,  1496,  1497,  1499,  1500,  1501,  1502,  1507,
    1508,  1510,  1511,  1513,  1514,  1517,  1518,  1523,  1524,  1526,
    1527,  1531,  1533,  1535,  1536,  1537,  1538,  1539,  1542,  1543,
    1545,  1546,  1547,  1549,  1551,  1552,  1554,  1555,  1556,  1557,
    1558,  1559,  1560,  1561,  1563,  1564,  1566,  1567,  1569,  1571,
    1572,  1574,  1575,  1577,  1578,  1579,  1580,  1581,  1582,  1583,
    1584,  1585,  1586,  1587,  1588,  1589,  1590,  1591,  1592,  1593,
    1594,  1596,  1597,  1601,  1602,  1604,  1606,  1607,  1609,  1610,
    1614,  1615,  1616,  1617,  1618,  1623,  1626,  1630,  1631,  1633,
    1634
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
#line 7783 "parser.cc"

#line 1643 "parser.y"


using std::optional;
using std::string;
using std::vector;
using std::pair;

void
yy::parser::error (const location_type& l, const std::string& m)
{
    drv.push_error_message(l,m);
}

pair<vector<Hs::LImpDecl>, optional<Hs::Decls>> make_body(const std::vector<Hs::LImpDecl>& imports, const std::optional<Hs::Decls>& topdecls)
{
    if (topdecls)
    {
        auto topdecls2 = Hs::Decls(*topdecls);
        return {imports, topdecls2};
    }
    else
        return {imports, {}};
}

Hs::LExp make_record_field_selection(const yy::location& loc, const Hs::LExp& object, const std::string& field)
{
    Hs::LExp selector = {loc, Hs::Var(field)};
    return {loc, Hs::ApplyExp(selector, object)};
}

Hs::LExp make_record_projection(const yy::location& loc, const std::vector<std::string>& fields)
{
    Hs::LVar arg = {loc, Hs::Var("v$record_dot")};
    Hs::LExp body = {loc, unloc(arg)};

    for(const auto& field: fields)
        body = make_record_field_selection(loc, body, field);

    return {loc, Hs::LambdaExp({{loc, unloc(arg)}}, body)};
}

// See PostProcess.hs:checkTyClHdr
std::tuple<Located<Hs::TypeCon>, vector<Hs::LType>>
check_type_or_class_header2(const Hs::LType& type)
{
    auto [type_head, type_args] = Hs::decompose_type_apps(type);

    // FIXME -- add location!
    auto tc = unloc(type_head).to<Hs::TypeCon>();

    // Convert to error message!
    if (not tc)
        throw myexception()<<"Malformed type or class header '"<<type<<"'";

    auto name = tc->name;

    return {{type_head.loc, *tc}, type_args};
}

vector<Hs::LTypeVar> check_all_type_vars(const vector<Hs::LType>& ltypes)
{
    vector<Hs::LTypeVar> ltype_vars;
    for(auto& ltype: ltypes)
    {
        auto& [loc,type] = ltype;
        if (auto tv = type.to<Hs::TypeVar>())
        {
            ltype_vars.push_back({loc,*tv});
        }
        else if (auto tok = type.to<Hs::TypeOfKind>())
        {
            if (auto tv = unloc(tok->type).to<Hs::TypeVar>())
            {
                auto tvok = *tv;
                tvok.kind = tok->kind;
                ltype_vars.push_back({loc,tvok});
            }
            else
                throw myexception()<<"Type '"<<type<<"' is not a type variable";
        }
        else
        {
            throw myexception()<<"Type '"<<type<<"' is not a type variable";
        }
    }
    return ltype_vars;
}

Hs::TypeSynonymDecl make_type_synonym(const Hs::LType& lhs_type, const Hs::LType& rhs_type)
{
    auto [con, type_args] = check_type_or_class_header2(lhs_type);
    return {con, check_all_type_vars(type_args), rhs_type};
}

Hs::FamilyDecl make_family_decl(Hs::FamilyInfo info, const Hs::LType& lhs_type, const std::optional<Located<Hs::Kind>>& kind_sig,
				const std::optional<std::vector<Hs::TypeFamilyInstanceEqn>>& eqns)
{
    auto [lhead, largs] = Hs::decompose_type_apps(lhs_type);

    // Get type con
    auto con = unloc(lhead).to<Hs::TypeCon>();
    if (not con)
        throw myexception()<<"Type family '"<<lhs_type.print()<<"' does not begin with a type constructor.";
    auto lcon = Hs::LTypeCon{lhead.loc, *con};
    
    // Get args as type vars
    std::vector<Hs::LTypeVar> tyvars;
    for(auto larg: largs)
    {
        std::optional<Hs::Kind> kind;
        if (auto ktype = unloc(larg).to<Hs::TypeOfKind>())
        {
            larg = ktype->type;
            kind = ktype->kind;
        }

        if (auto tyvar = unloc(larg).to<Hs::TypeVar>())
        {
            auto tv = Hs::LTypeVar{larg.loc, *tyvar};
            unloc(tv).kind = kind;
            tyvars.push_back(tv);
        }
        else
            throw myexception()<<"Type family '"<<lhs_type.print()<<"' argument '"<<larg.print()<<"' is not a type variable.";
    }

    std::optional<Hs::Kind> kind;
    if (kind_sig)
        kind = kind_sig->value();

    return Hs::FamilyDecl(info, lcon, tyvars, kind, eqns);
}

Hs::TypeFamilyInstanceEqn make_type_family_instance_eqn(const Hs::LType& lhs_type, const Hs::LType& rhs_type)
{
    auto [head, args] = Hs::decompose_type_apps(lhs_type);

    // Get type con
    auto con = unloc(head).to<Hs::TypeCon>();
    if (not con)
        throw myexception()<<"Type family instance '"<<lhs_type.print()<<"' does not begin with a type constructor.";
    
    return Hs::TypeFamilyInstanceEqn({head.loc, *con}, args, rhs_type);
}

Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context&  context,
                                           const Hs::LType& header, const std::optional<Hs::Kind>& k,
                                           const Hs::ConstructorsDecl& constrs,
                                           const std::vector<Hs::Deriving>& derivings)
{
    auto [con, type_args] = check_type_or_class_header2(header);
    return {con, check_all_type_vars(type_args), Hs::DataDefn(d_or_n, context, k, constrs, derivings)};
}

Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context&  context,
                                           const Hs::LType& header, const std::optional<Hs::Kind>& k,
                                           const Hs::GADTConstructorsDecl& constrs,
                                           const std::vector<Hs::Deriving>& derivings)
{
    auto [con, type_args] = check_type_or_class_header2(header);
    return {con, check_all_type_vars(type_args), Hs::DataDefn(d_or_n, context, k, constrs, derivings)};
}

Hs::InstanceDecl make_instance_decl(const std::optional<std::string>& oprag, const Hs::LType& polytype, const optional<Located<Hs::Decls>>& decls)
{
    std::vector<Hs::TypeFamilyInstanceDecl> type_inst_decls;
    std::vector<Hs::DataFamilyInstanceDecl> data_inst_decls;
    Hs::Decls method_decls;
    if (decls)
        for(auto& [loc,decl]: unloc(*decls))
        {
            if (auto TI = decl.to<Hs::TypeFamilyInstanceDecl>())
                type_inst_decls.push_back(*TI);
            else if (auto DI = decl.to<Hs::DataFamilyInstanceDecl>())
                data_inst_decls.push_back(*DI);
            else if (auto V = decl.to<Hs::ValueDecl>())
                method_decls.push_back({loc,*V});
            else
                throw myexception()<<"In declaration of instance "<<unloc(polytype).print()<<", I don't recognize declaration:\n   "<<decl.print();
        }
    return {oprag, polytype, type_inst_decls, data_inst_decls, method_decls};
}

Hs::ClassDecl make_class_decl(const Hs::Context& context, const Hs::LType& header,
                              const std::vector<Hs::FunDep>& fds, const optional<Located<Hs::Decls>>& decls)
{
    auto [con, type_args] = check_type_or_class_header2(header);

    std::vector<Hs::FixityDecl> fixity_decls;
    std::vector<Hs::FamilyDecl> fam_decls;
    std::vector<Hs::TypeFamilyInstanceDecl> default_type_inst_decls;
    std::vector<Hs::TypeSigDecl> sig_decls;
    Hs::Decls default_method_decls;

    if (decls)
        for(auto& [loc,decl]: unloc(*decls))
        {
            if (auto F = decl.to<Hs::FixityDecl>())
                fixity_decls.push_back(*F);
            else if (auto TF = decl.to<Hs::FamilyDecl>())
                fam_decls.push_back(*TF);
            else if (auto TI = decl.to<Hs::TypeFamilyInstanceDecl>())
                default_type_inst_decls.push_back(*TI);
            else if (auto S = decl.to<Hs::TypeSigDecl>())
                sig_decls.push_back(*S);
            else if (auto V = decl.to<Hs::ValueDecl>())
                default_method_decls.push_back({loc,*V});
            else
                throw myexception()<<"In declaration of class "<<con<<", I don't recognize declaration:\n   "<<decl.print();
        }

    return {context, con, check_all_type_vars(type_args), fixity_decls, fds, fam_decls, default_type_inst_decls, sig_decls, default_method_decls};
}

// Can we change the context parsing rule to expect:
// nothing
// | ctype => header
// | ( ctypes2 ) => header
Hs::Context make_context(const Hs::LType& context)
{
    vector<Hs::LType> constraints;
    if (auto tuple = unloc(context).to<Hs::TupleType>())
        constraints = tuple->element_types;
    else
        constraints.push_back(context);

    return {constraints};
}

std::optional<Hs::Kind> type_to_kind_(const Hs::LType& kind)
{
    auto [kind_head, kind_args] = Hs::decompose_type_apps(kind);

    if (not unloc(kind_head).is_a<Hs::TypeCon>()) return {};
    auto V = unloc(kind_head).as_<Hs::TypeCon>();
    auto head_name = V.name;

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

Hs::Kind type_to_kind(const Hs::LType& kind)
{
    auto maybe_kind = type_to_kind_(kind);

    if (not maybe_kind)
        throw myexception()<<"Kind '"<<kind<<"' is malformed";

    return *maybe_kind;
}

Hs::ConstructorDecl make_constructor(const vector<Hs::LTypeVar>& qtvs, const Hs::Context& context, const Hs::LType& typeish)
{
    // 1. Split into head and arguments
    auto [head,args] = Hs::decompose_type_apps(typeish);

    // 2. Get the constructor name.
    auto tc = unloc(head).to<Hs::TypeCon>();
    if (not tc)
        throw myexception()<<"In constructor `"<<typeish<<"`:\n    `"<<head<<"` is not a data constructor!";
    auto con = Hs::LCon{head.loc, Hs::Con(tc->name)};

    // 3. If we have 1 arg and its a FieldDecls, then make a record constructor.
    if (args.size() == 1)
    {
        if (auto fd = unloc(args[0]).to<Hs::FieldDecls>())
        {
            return {qtvs, context, con, *fd};
        }
    }

    // 4. Otherwise make a normal constructor.
    return {qtvs, context, con, args};
}
