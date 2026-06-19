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
#line 58 "parser.y"

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
      case symbol_kind::S_gadt_constrs0: // gadt_constrs0
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

      case symbol_kind::S_role: // role
        value.YY_MOVE_OR_COPY< Located<std::optional<Role>> > (YY_MOVE (that.value));
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

      case symbol_kind::S_maybe_roles: // maybe_roles
      case symbol_kind::S_roles: // roles
        value.YY_MOVE_OR_COPY< std::vector<Located<std::optional<Role>>> > (YY_MOVE (that.value));
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
      case symbol_kind::S_gadt_constrs0: // gadt_constrs0
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

      case symbol_kind::S_role: // role
        value.move< Located<std::optional<Role>> > (YY_MOVE (that.value));
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

      case symbol_kind::S_maybe_roles: // maybe_roles
      case symbol_kind::S_roles: // roles
        value.move< std::vector<Located<std::optional<Role>>> > (YY_MOVE (that.value));
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
      case symbol_kind::S_gadt_constrs0: // gadt_constrs0
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

      case symbol_kind::S_role: // role
        value.copy< Located<std::optional<Role>> > (that.value);
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

      case symbol_kind::S_maybe_roles: // maybe_roles
      case symbol_kind::S_roles: // roles
        value.copy< std::vector<Located<std::optional<Role>>> > (that.value);
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
      case symbol_kind::S_gadt_constrs0: // gadt_constrs0
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

      case symbol_kind::S_role: // role
        value.move< Located<std::optional<Role>> > (that.value);
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

      case symbol_kind::S_maybe_roles: // maybe_roles
      case symbol_kind::S_roles: // roles
        value.move< std::vector<Located<std::optional<Role>>> > (that.value);
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
      case symbol_kind::S_gadt_constrs0: // gadt_constrs0
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

      case symbol_kind::S_role: // role
        yylhs.value.emplace< Located<std::optional<Role>> > ();
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

      case symbol_kind::S_maybe_roles: // maybe_roles
      case symbol_kind::S_roles: // roles
        yylhs.value.emplace< std::vector<Located<std::optional<Role>>> > ();
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
#line 524 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2726 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 541 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < Located<std::string> > (),yystack_[2].value.as < std::optional<std::vector<Hs::LExport>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2732 "parser.cc"
    break;

  case 4: // module: body2
#line 542 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{{noloc,"Main"},{},yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2738 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 544 "parser.y"
                                                                 {drv.push_module_context();}
#line 2744 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 552 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2750 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 553 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2756 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 555 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2762 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 556 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2768 "parser.cc"
    break;

  case 13: // top: semis top1
#line 559 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2774 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 561 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2780 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 562 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2786 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 563 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::LImpDecl> > (),{});}
#line 2792 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 571 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::LExport>> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2798 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 572 "parser.y"
                                      {}
#line 2804 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 574 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2810 "parser.cc"
    break;

  case 20: // exportlist: %empty
#line 575 "parser.y"
                                      {}
#line 2816 "parser.cc"
    break;

  case 21: // exportlist: exportlist1 ','
#line 576 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2822 "parser.cc"
    break;

  case 22: // exportlist: ','
#line 577 "parser.y"
                                      {}
#line 2828 "parser.cc"
    break;

  case 23: // exportlist1: exportlist1 "," export
#line 579 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2834 "parser.cc"
    break;

  case 24: // exportlist1: export
#line 580 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2840 "parser.cc"
    break;

  case 25: // export: qcname export_subspec
#line 582 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2846 "parser.cc"
    break;

  case 26: // export: "module" modid
#line 583 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2852 "parser.cc"
    break;

  case 27: // export: "default" modid
#line 584 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::default_}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2858 "parser.cc"
    break;

  case 28: // export_subspec: %empty
#line 586 "parser.y"
                                      {}
#line 2864 "parser.cc"
    break;

  case 29: // export_subspec: "(" qcnames ")"
#line 587 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2870 "parser.cc"
    break;

  case 30: // export_subspec: "(" ".." ")"
#line 588 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec(); }
#line 2876 "parser.cc"
    break;

  case 31: // qcnames: %empty
#line 590 "parser.y"
                   {}
#line 2882 "parser.cc"
    break;

  case 32: // qcnames: qcnames1
#line 591 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2888 "parser.cc"
    break;

  case 33: // qcnames1: qcnames1 "," qcname
#line 593 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2894 "parser.cc"
    break;

  case 34: // qcnames1: qcname
#line 594 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2900 "parser.cc"
    break;

  case 35: // qcname: qvar
#line 596 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2906 "parser.cc"
    break;

  case 36: // qcname: oqtycon_no_varcon
#line 597 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2912 "parser.cc"
    break;

  case 41: // importdecls: importdecls_semi importdecl
#line 607 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[1].value.as < std::vector<Hs::LImpDecl> > (), yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[0].value.as < Hs::LImpDecl > ()); }
#line 2918 "parser.cc"
    break;

  case 42: // importdecls_semi: importdecls_semi importdecl semis1
#line 609 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[2].value.as < std::vector<Hs::LImpDecl> > (); yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[1].value.as < Hs::LImpDecl > ()); }
#line 2924 "parser.cc"
    break;

  case 43: // importdecls_semi: %empty
#line 610 "parser.y"
                         { }
#line 2930 "parser.cc"
    break;

  case 44: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 612 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::LImpDecl > () = {yylhs.location, Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < Located<std::string> > (),yystack_[1].value.as < std::optional<Located<std::string>> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ())};
}
#line 2938 "parser.cc"
    break;

  case 45: // optqualified: "qualified"
#line 625 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2944 "parser.cc"
    break;

  case 46: // optqualified: %empty
#line 626 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2950 "parser.cc"
    break;

  case 47: // maybeas: "as" modid
#line 628 "parser.y"
                               { yylhs.value.as < std::optional<Located<std::string>> > () = yystack_[0].value.as < Located<std::string> > (); }
#line 2956 "parser.cc"
    break;

  case 48: // maybeas: %empty
#line 629 "parser.y"
                               { }
#line 2962 "parser.cc"
    break;

  case 49: // maybeimpspec: impspec
#line 631 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2968 "parser.cc"
    break;

  case 50: // maybeimpspec: %empty
#line 632 "parser.y"
                               { }
#line 2974 "parser.cc"
    break;

  case 51: // impspec: "(" importlist ")"
#line 636 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2980 "parser.cc"
    break;

  case 52: // impspec: "hiding" "(" importlist ")"
#line 637 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2986 "parser.cc"
    break;

  case 53: // importlist: importlist1
#line 639 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2992 "parser.cc"
    break;

  case 54: // importlist: %empty
#line 640 "parser.y"
                                      {}
#line 2998 "parser.cc"
    break;

  case 55: // importlist: importlist1 ','
#line 641 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 3004 "parser.cc"
    break;

  case 56: // importlist: ','
#line 642 "parser.y"
                                      {}
#line 3010 "parser.cc"
    break;

  case 57: // importlist1: importlist1 "," import
#line 644 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 3016 "parser.cc"
    break;

  case 58: // importlist1: import
#line 645 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 3022 "parser.cc"
    break;

  case 59: // import: qcname export_subspec
#line 647 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 3028 "parser.cc"
    break;

  case 60: // import: "module" modid
#line 648 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 3034 "parser.cc"
    break;

  case 61: // prec: %empty
#line 653 "parser.y"
                   { }
#line 3040 "parser.cc"
    break;

  case 62: // prec: "INTEGER"
#line 654 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < integer > ().convert_to<int>(); }
#line 3046 "parser.cc"
    break;

  case 63: // infix: "infix"
#line 656 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 3052 "parser.cc"
    break;

  case 64: // infix: "infixl"
#line 657 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 3058 "parser.cc"
    break;

  case 65: // infix: "infixr"
#line 658 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 3064 "parser.cc"
    break;

  case 66: // ops: ops "," op
#line 660 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3070 "parser.cc"
    break;

  case 67: // ops: op
#line 661 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = {{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 3076 "parser.cc"
    break;

  case 68: // topdecls: topdecls_semi topdecl
#line 665 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ()); }
#line 3082 "parser.cc"
    break;

  case 69: // topdecls_semi: topdecls_semi topdecl semis1
#line 667 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 3088 "parser.cc"
    break;

  case 70: // topdecls_semi: %empty
#line 668 "parser.y"
                                            { }
#line 3094 "parser.cc"
    break;

  case 71: // topdecl: cl_decl
#line 670 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3100 "parser.cc"
    break;

  case 72: // topdecl: ty_decl
#line 671 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3106 "parser.cc"
    break;

  case 73: // topdecl: standalone_kind_sig
#line 672 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3112 "parser.cc"
    break;

  case 74: // topdecl: inst_decl
#line 673 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3118 "parser.cc"
    break;

  case 75: // topdecl: stand_alone_deriving
#line 674 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3124 "parser.cc"
    break;

  case 76: // topdecl: "default" opt_class "(" comma_types0 ")"
#line 675 "parser.y"
                                                         {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::DefaultDecl(yystack_[3].value.as < std::optional<Located<std::string>> > (),yystack_[1].value.as < std::vector<Hs::LType> > ())}; }
#line 3130 "parser.cc"
    break;

  case 77: // topdecl: "foreign" "import" call_conv "STRING" var "::" sigtypedoc
#line 676 "parser.y"
                                                                  {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ForeignDecl(yystack_[4].value.as < Located<std::string> > (), yystack_[3].value.as < std::string > (), {yystack_[2].location,yystack_[2].value.as < std::string > ()}, yystack_[0].value.as < Hs::LType > ())};}
#line 3136 "parser.cc"
    break;

  case 78: // topdecl: decl_no_th
#line 682 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3142 "parser.cc"
    break;

  case 79: // topdecl: infixexp
#line 684 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<Hs::InfixExp> > ();}
#line 3148 "parser.cc"
    break;

  case 80: // call_conv: "bpcall"
#line 687 "parser.y"
                    {yylhs.value.as < Located<std::string> > () = {yylhs.location,"bpcall"};}
#line 3154 "parser.cc"
    break;

  case 81: // call_conv: "trcall"
#line 688 "parser.y"
                    {yylhs.value.as < Located<std::string> > () = {yylhs.location,"trcall"};}
#line 3160 "parser.cc"
    break;

  case 82: // call_conv: "ecall"
#line 689 "parser.y"
                    {yylhs.value.as < Located<std::string> > () = {yylhs.location,"ecall"};}
#line 3166 "parser.cc"
    break;

  case 83: // cl_decl: "class" tycl_hdr fds where_cls
#line 691 "parser.y"
                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_class_decl(yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first,yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,yystack_[1].value.as < std::vector<Hs::FunDep> > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 3172 "parser.cc"
    break;

  case 84: // ty_decl: "type" "role" oqtycon maybe_roles
#line 694 "parser.y"
                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RoleAnnotationDecl({yystack_[1].location, Hs::TypeCon(yystack_[1].value.as < std::string > ())}, yystack_[0].value.as < std::vector<Located<std::optional<Role>>> > ())};}
#line 3178 "parser.cc"
    break;

  case 85: // ty_decl: "type" type "=" ktype
#line 695 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_type_synonym(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3184 "parser.cc"
    break;

  case 86: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 696 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > (),yystack_[0].value.as < std::vector<Hs::Deriving> > ())};}
#line 3190 "parser.cc"
    break;

  case 87: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 698 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ())};}
#line 3196 "parser.cc"
    break;

  case 88: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 699 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[3].value.as < Hs::LType > (), yystack_[2].value.as < std::optional<Located<Hs::Kind>> > (), yystack_[0].value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ())};}
#line 3202 "parser.cc"
    break;

  case 89: // ty_decl: "data" "family" type opt_datafam_kind_sig
#line 700 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})};}
#line 3208 "parser.cc"
    break;

  case 90: // standalone_kind_sig: "type" sks_vars "::" kind
#line 702 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::KindSigDecl(yystack_[2].value.as < std::vector<Hs::LTypeCon> > (),yystack_[0].value.as < Hs::Kind > ())};}
#line 3214 "parser.cc"
    break;

  case 91: // sks_vars: sks_vars "," oqtycon
#line 704 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = yystack_[2].value.as < std::vector<Hs::LTypeCon> > (); yylhs.value.as < std::vector<Hs::LTypeCon> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}); }
#line 3220 "parser.cc"
    break;

  case 92: // sks_vars: oqtycon
#line 705 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}}; }
#line 3226 "parser.cc"
    break;

  case 93: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 708 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_instance_decl(yystack_[2].value.as < std::optional<std::string> > (),yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 3232 "parser.cc"
    break;

  case 94: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 709 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};}
#line 3238 "parser.cc"
    break;

  case 95: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 711 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(tvs, con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), context ? unloc(*context) : Hs::Context(), {}, yystack_[1].value.as < Hs::ConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ()))};
	   }
#line 3248 "parser.cc"
    break;

  case 96: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 717 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(tvs, con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), context ? unloc(*context) : Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ()))};
	   }
#line 3258 "parser.cc"
    break;

  case 97: // overlap_pragma: "{-# OVERLAPPABLE" "#-}"
#line 723 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPABLE"; }
#line 3264 "parser.cc"
    break;

  case 98: // overlap_pragma: "{-# OVERLAPPING" "#-}"
#line 724 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPING"; }
#line 3270 "parser.cc"
    break;

  case 99: // overlap_pragma: "{-# OVERLAPS" "#-}"
#line 725 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPS"; }
#line 3276 "parser.cc"
    break;

  case 100: // overlap_pragma: "{-# INCOHERENT" "#-}"
#line 726 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "INCOHERENT"; }
#line 3282 "parser.cc"
    break;

  case 101: // overlap_pragma: %empty
#line 727 "parser.y"
                                               {}
#line 3288 "parser.cc"
    break;

  case 102: // deriv_strategy_no_via: "stock"
#line 729 "parser.y"
                                    {yylhs.value.as < Hs::DerivingStrategy > () = Hs::DerivingStrategy::stock;}
#line 3294 "parser.cc"
    break;

  case 103: // deriv_strategy_no_via: "anyclass"
#line 730 "parser.y"
                                    {yylhs.value.as < Hs::DerivingStrategy > () = Hs::DerivingStrategy::anyclass;}
#line 3300 "parser.cc"
    break;

  case 104: // deriv_strategy_no_via: "newtype"
#line 731 "parser.y"
                                    {yylhs.value.as < Hs::DerivingStrategy > () = Hs::DerivingStrategy::newtype;}
#line 3306 "parser.cc"
    break;

  case 105: // deriv_strategy_via: "via" type
#line 733 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3312 "parser.cc"
    break;

  case 106: // stand_alone_deriving: "deriving" "instance" inst_type
#line 736 "parser.y"
                             {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StandaloneDerivingDecl({}, yystack_[0].value.as < Hs::LType > ())};}
#line 3318 "parser.cc"
    break;

  case 107: // stand_alone_deriving: "deriving" deriv_strategy_no_via "instance" inst_type
#line 738 "parser.y"
                             {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StandaloneDerivingDecl(yystack_[2].value.as < Hs::DerivingStrategy > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3324 "parser.cc"
    break;

  case 108: // stand_alone_deriving: "deriving" deriv_strategy_via "instance" inst_type
#line 740 "parser.y"
                             {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StandaloneDerivingDecl(Hs::DerivingStrategy::via, yystack_[0].value.as < Hs::LType > (), yystack_[2].value.as < Hs::LType > ())};}
#line 3330 "parser.cc"
    break;

  case 114: // where_type_family: %empty
#line 754 "parser.y"
                                                           {}
#line 3336 "parser.cc"
    break;

  case 115: // where_type_family: "where" ty_fam_inst_eqn_list
#line 755 "parser.y"
                                                           {yylhs.value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > () = yystack_[0].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3342 "parser.cc"
    break;

  case 116: // ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
#line 757 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3348 "parser.cc"
    break;

  case 117: // ty_fam_inst_eqn_list: "vocurly" ty_fam_inst_eqns close
#line 758 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3354 "parser.cc"
    break;

  case 118: // ty_fam_inst_eqn_list: "{" ".." "}"
#line 759 "parser.y"
                                                           {}
#line 3360 "parser.cc"
    break;

  case 119: // ty_fam_inst_eqn_list: "vocurly" ".." close
#line 760 "parser.y"
                                                           {}
#line 3366 "parser.cc"
    break;

  case 120: // ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
#line 762 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[2].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > (); yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ().push_back(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 3372 "parser.cc"
    break;

  case 121: // ty_fam_inst_eqns: ty_fam_inst_eqns ";"
#line 763 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3378 "parser.cc"
    break;

  case 122: // ty_fam_inst_eqns: ty_fam_inst_eqn
#line 764 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = {yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ()};}
#line 3384 "parser.cc"
    break;

  case 123: // ty_fam_inst_eqns: %empty
#line 765 "parser.y"
                                                           {}
#line 3390 "parser.cc"
    break;

  case 124: // ty_fam_inst_eqn: type "=" ctype
#line 767 "parser.y"
                                                           {yylhs.value.as < Hs::TypeFamilyInstanceEqn > () = make_type_family_instance_eqn(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ());}
#line 3396 "parser.cc"
    break;

  case 125: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 770 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3402 "parser.cc"
    break;

  case 126: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 772 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3408 "parser.cc"
    break;

  case 127: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 774 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3414 "parser.cc"
    break;

  case 128: // at_decl_cls: "type" ty_fam_inst_eqn
#line 776 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3420 "parser.cc"
    break;

  case 129: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 777 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3426 "parser.cc"
    break;

  case 134: // at_decl_inst: "type" opt_instance ty_fam_inst_eqn
#line 785 "parser.y"
                                                              { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3432 "parser.cc"
    break;

  case 135: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 788 "parser.y"
              {
		  auto& [tvs, context, type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(tvs, con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), context ? unloc(*context) : Hs::Context(), {}, yystack_[1].value.as < Hs::ConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ()))};
	      }
#line 3442 "parser.cc"
    break;

  case 136: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 795 "parser.y"
              {
		  auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(tvs, con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), context ? unloc(*context) : Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ()))};
	      }
#line 3452 "parser.cc"
    break;

  case 137: // data_or_newtype: "data"
#line 801 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 3458 "parser.cc"
    break;

  case 138: // data_or_newtype: "newtype"
#line 802 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 3464 "parser.cc"
    break;

  case 139: // opt_class: %empty
#line 805 "parser.y"
                  {yylhs.value.as < std::optional<Located<std::string>> > () = {};}
#line 3470 "parser.cc"
    break;

  case 140: // opt_class: qtycon
#line 806 "parser.y"
                  {yylhs.value.as < std::optional<Located<std::string>> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()};}
#line 3476 "parser.cc"
    break;

  case 141: // opt_kind_sig: %empty
#line 810 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 3482 "parser.cc"
    break;

  case 142: // opt_kind_sig: "::" kind
#line 811 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < Hs::Kind > ();}
#line 3488 "parser.cc"
    break;

  case 143: // opt_datafam_kind_sig: %empty
#line 813 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {};}
#line 3494 "parser.cc"
    break;

  case 144: // opt_datafam_kind_sig: "::" kind
#line 814 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3500 "parser.cc"
    break;

  case 145: // opt_tyfam_kind_sig: %empty
#line 816 "parser.y"
                                      {}
#line 3506 "parser.cc"
    break;

  case 146: // opt_tyfam_kind_sig: "::" kind
#line 817 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3512 "parser.cc"
    break;

  case 147: // opt_tyfam_kind_sig: "=" tv_bndr
#line 818 "parser.y"
                                      {}
#line 3518 "parser.cc"
    break;

  case 148: // opt_at_kind_inj_sig: %empty
#line 820 "parser.y"
                                      {}
#line 3524 "parser.cc"
    break;

  case 149: // opt_at_kind_inj_sig: "::" kind
#line 821 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3530 "parser.cc"
    break;

  case 150: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 822 "parser.y"
                                                                  {}
#line 3536 "parser.cc"
    break;

  case 151: // tycl_hdr: context "=>" type
#line 825 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ()};}
#line 3542 "parser.cc"
    break;

  case 152: // tycl_hdr: type
#line 826 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {{},yystack_[0].value.as < Hs::LType > ()};}
#line 3548 "parser.cc"
    break;

  case 153: // datafam_inst_hdr: "forall" tv_bndrs "." context "=>" type
#line 829 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[4].value.as < std::vector<Hs::LTypeVar> > ()}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3554 "parser.cc"
    break;

  case 154: // datafam_inst_hdr: "forall" tv_bndrs "=>" type
#line 830 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[2].value.as < std::vector<Hs::LTypeVar> > ()}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3560 "parser.cc"
    break;

  case 155: // datafam_inst_hdr: context "=>" type
#line 831 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3566 "parser.cc"
    break;

  case 156: // datafam_inst_hdr: type
#line 832 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3572 "parser.cc"
    break;

  case 160: // maybe_roles: %empty
#line 841 "parser.y"
                                               {yylhs.value.as < std::vector<Located<std::optional<Role>>> > () = {};}
#line 3578 "parser.cc"
    break;

  case 161: // maybe_roles: roles
#line 842 "parser.y"
                                               {yylhs.value.as < std::vector<Located<std::optional<Role>>> > () = yystack_[0].value.as < std::vector<Located<std::optional<Role>>> > ();}
#line 3584 "parser.cc"
    break;

  case 162: // roles: role
#line 844 "parser.y"
                                               {yylhs.value.as < std::vector<Located<std::optional<Role>>> > () = {yystack_[0].value.as < Located<std::optional<Role>> > ()};}
#line 3590 "parser.cc"
    break;

  case 163: // roles: roles role
#line 845 "parser.y"
                                               {yylhs.value.as < std::vector<Located<std::optional<Role>>> > () = yystack_[1].value.as < std::vector<Located<std::optional<Role>>> > (); yylhs.value.as < std::vector<Located<std::optional<Role>>> > ().push_back(yystack_[0].value.as < Located<std::optional<Role>> > ());}
#line 3596 "parser.cc"
    break;

  case 164: // role: "VARID"
#line 848 "parser.y"
              {
                  if (yystack_[0].value.as < std::string > () == "nominal")
                      yylhs.value.as < Located<std::optional<Role>> > () = {yylhs.location, Role::Nominal};
                  else if (yystack_[0].value.as < std::string > () == "representational")
                      yylhs.value.as < Located<std::optional<Role>> > () = {yylhs.location, Role::Representational};
                  else if (yystack_[0].value.as < std::string > () == "phantom")
                      yylhs.value.as < Located<std::optional<Role>> > () = {yylhs.location, Role::Phantom};
                  else
                  {
                      drv.push_error_message(yystack_[0].location, "Unknown role `" + yystack_[0].value.as < std::string > () + "`");
                      YYERROR;
                  }
              }
#line 3614 "parser.cc"
    break;

  case 165: // role: "_"
#line 861 "parser.y"
                                                {yylhs.value.as < Located<std::optional<Role>> > () = {yylhs.location, std::optional<Role>{}};}
#line 3620 "parser.cc"
    break;

  case 166: // decl_cls: at_decl_cls
#line 889 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3626 "parser.cc"
    break;

  case 167: // decl_cls: decl
#line 890 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3632 "parser.cc"
    break;

  case 168: // decls_cls: decls_cls ";" decl_cls
#line 892 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3638 "parser.cc"
    break;

  case 169: // decls_cls: decls_cls ";"
#line 893 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3644 "parser.cc"
    break;

  case 170: // decls_cls: decl_cls
#line 894 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3650 "parser.cc"
    break;

  case 171: // decls_cls: %empty
#line 895 "parser.y"
                                           {}
#line 3656 "parser.cc"
    break;

  case 172: // decllist_cls: "{" decls_cls "}"
#line 897 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3662 "parser.cc"
    break;

  case 173: // decllist_cls: "vocurly" decls_cls close
#line 898 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3668 "parser.cc"
    break;

  case 174: // where_cls: "where" decllist_cls
#line 900 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3674 "parser.cc"
    break;

  case 175: // where_cls: %empty
#line 901 "parser.y"
                                           {}
#line 3680 "parser.cc"
    break;

  case 176: // decl_inst: at_decl_inst
#line 903 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3686 "parser.cc"
    break;

  case 177: // decl_inst: decl
#line 904 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3692 "parser.cc"
    break;

  case 178: // decls_inst: decls_inst ";" decl_inst
#line 906 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3698 "parser.cc"
    break;

  case 179: // decls_inst: decls_inst ";"
#line 907 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3704 "parser.cc"
    break;

  case 180: // decls_inst: decl_inst
#line 908 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3710 "parser.cc"
    break;

  case 181: // decls_inst: %empty
#line 909 "parser.y"
                                           {}
#line 3716 "parser.cc"
    break;

  case 182: // decllist_inst: "{" decls_inst "}"
#line 911 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3722 "parser.cc"
    break;

  case 183: // decllist_inst: "vocurly" decls_inst close
#line 912 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3728 "parser.cc"
    break;

  case 184: // where_inst: "where" decllist_inst
#line 914 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3734 "parser.cc"
    break;

  case 185: // where_inst: %empty
#line 915 "parser.y"
                                           {}
#line 3740 "parser.cc"
    break;

  case 186: // decls: decls ";" decl
#line 918 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3746 "parser.cc"
    break;

  case 187: // decls: decls ";"
#line 919 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3752 "parser.cc"
    break;

  case 188: // decls: decl
#line 920 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3758 "parser.cc"
    break;

  case 189: // decls: %empty
#line 921 "parser.y"
                        {}
#line 3764 "parser.cc"
    break;

  case 190: // decllist: "{" decls "}"
#line 923 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3770 "parser.cc"
    break;

  case 191: // decllist: "vocurly" decls close
#line 924 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3776 "parser.cc"
    break;

  case 192: // binds: decllist
#line 926 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 3782 "parser.cc"
    break;

  case 193: // wherebinds: "where" binds
#line 928 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 3788 "parser.cc"
    break;

  case 194: // wherebinds: %empty
#line 929 "parser.y"
                                 {}
#line 3794 "parser.cc"
    break;

  case 200: // opt_tyconsig: %empty
#line 955 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {};}
#line 3800 "parser.cc"
    break;

  case 201: // opt_tyconsig: "::" gtycon
#line 956 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}};}
#line 3806 "parser.cc"
    break;

  case 202: // sigtype: ctype
#line 965 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3812 "parser.cc"
    break;

  case 203: // sigtypedoc: ctypedoc
#line 967 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3818 "parser.cc"
    break;

  case 204: // sig_vars: sig_vars "," var
#line 969 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > () = yystack_[2].value.as < std::vector<Hs::LVar> > (); yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3824 "parser.cc"
    break;

  case 205: // sig_vars: var
#line 970 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3830 "parser.cc"
    break;

  case 206: // sigtypes1: sigtype
#line 972 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3836 "parser.cc"
    break;

  case 207: // sigtypes1: sigtypes1 "," sigtype
#line 973 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3842 "parser.cc"
    break;

  case 208: // ktype: ctype
#line 982 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3848 "parser.cc"
    break;

  case 209: // ktype: ctype "::" kind
#line 983 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeOfKind(yystack_[2].value.as < Hs::LType > (), yystack_[0].value.as < Hs::Kind > ())};}
#line 3854 "parser.cc"
    break;

  case 210: // ctype: "forall" tv_bndrs "." ctype
#line 985 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ForallType(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3860 "parser.cc"
    break;

  case 211: // ctype: context "=>" ctype
#line 986 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3866 "parser.cc"
    break;

  case 212: // ctype: type
#line 988 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3872 "parser.cc"
    break;

  case 213: // ctypedoc: ctype
#line 990 "parser.y"
          { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3878 "parser.cc"
    break;

  case 214: // context: btype
#line 999 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::LType > ());}
#line 3884 "parser.cc"
    break;

  case 215: // context_no_ops: btype_no_ops
#line 1001 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ()));}
#line 3890 "parser.cc"
    break;

  case 216: // type: btype
#line 1003 "parser.y"
      { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3896 "parser.cc"
    break;

  case 217: // type: btype "->" ctype
#line 1004 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = Hs::type_apply({{yystack_[1].location,Hs::TypeCon("->")},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3902 "parser.cc"
    break;

  case 218: // typedoc: type
#line 1006 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3908 "parser.cc"
    break;

  case 219: // btype: infixtype
#line 1009 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3914 "parser.cc"
    break;

  case 220: // infixtype: ftype
#line 1011 "parser.y"
           { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3920 "parser.cc"
    break;

  case 221: // infixtype: btype tyop btype
#line 1012 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = Hs::type_apply({{yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3926 "parser.cc"
    break;

  case 222: // btype_no_ops: atype_docs
#line 1014 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3932 "parser.cc"
    break;

  case 223: // btype_no_ops: btype_no_ops atype_docs
#line 1015 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3938 "parser.cc"
    break;

  case 224: // ftype: atype
#line 1017 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3944 "parser.cc"
    break;

  case 225: // ftype: ftype tyarg
#line 1019 "parser.y"
                                   { yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeApp(yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())}; }
#line 3950 "parser.cc"
    break;

  case 226: // ftype: ftype "@" atype
#line 1020 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[2].value.as < Hs::LType > (); }
#line 3956 "parser.cc"
    break;

  case 227: // tyarg: atype
#line 1022 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3962 "parser.cc"
    break;

  case 228: // tyop: qtyconop
#line 1024 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3968 "parser.cc"
    break;

  case 229: // tyop: tyvarop
#line 1025 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3974 "parser.cc"
    break;

  case 230: // atype_docs: atype
#line 1032 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3980 "parser.cc"
    break;

  case 231: // atype: ntgtycon
#line 1039 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon(yystack_[0].value.as < std::string > ())};}
#line 3986 "parser.cc"
    break;

  case 232: // atype: tyvar
#line 1040 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3992 "parser.cc"
    break;

  case 233: // atype: "*"
#line 1041 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("*")};}
#line 3998 "parser.cc"
    break;

  case 234: // atype: PREFIX_BANG atype
#line 1042 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::StrictType(yystack_[0].value.as < Hs::LType > ())};}
#line 4004 "parser.cc"
    break;

  case 235: // atype: PREFIX_TILDE atype
#line 1043 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::LazyType(yystack_[0].value.as < Hs::LType > ())};}
#line 4010 "parser.cc"
    break;

  case 236: // atype: "{" fielddecls "}"
#line 1044 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ())};}
#line 4016 "parser.cc"
    break;

  case 237: // atype: "(" ")"
#line 1045 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("()")};}
#line 4022 "parser.cc"
    break;

  case 238: // atype: "(" comma_types1 "," ktype ")"
#line 1046 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::LType> > ();ts.push_back(yystack_[1].value.as < Hs::LType > ());yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TupleType(ts)};}
#line 4028 "parser.cc"
    break;

  case 239: // atype: "[" ktype "]"
#line 1052 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ListType{yystack_[1].value.as < Hs::LType > ()}}; }
#line 4034 "parser.cc"
    break;

  case 240: // atype: "(" ktype ")"
#line 1053 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[1].value.as < Hs::LType > ();}
#line 4040 "parser.cc"
    break;

  case 241: // inst_type: sigtype
#line 1056 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 4046 "parser.cc"
    break;

  case 242: // deriv_types: typedoc
#line 1058 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 4052 "parser.cc"
    break;

  case 243: // deriv_types: typedoc "," deriv_types
#line 1059 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().insert(yylhs.value.as < std::vector<Hs::LType> > ().begin(), yystack_[2].value.as < Hs::LType > ());}
#line 4058 "parser.cc"
    break;

  case 244: // comma_types0: comma_types1
#line 1061 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > ();}
#line 4064 "parser.cc"
    break;

  case 245: // comma_types0: %empty
#line 1062 "parser.y"
                                       { /* default construction OK */ }
#line 4070 "parser.cc"
    break;

  case 246: // comma_types1: ktype
#line 1064 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 4076 "parser.cc"
    break;

  case 247: // comma_types1: comma_types1 "," ktype
#line 1065 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 4082 "parser.cc"
    break;

  case 248: // tv_bndrs: tv_bndrs tv_bndr
#line 1072 "parser.y"
                               {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back(yystack_[0].value.as < Hs::LTypeVar > ());}
#line 4088 "parser.cc"
    break;

  case 249: // tv_bndrs: %empty
#line 1073 "parser.y"
                               { /* default construction OK */}
#line 4094 "parser.cc"
    break;

  case 250: // tv_bndr: tv_bndr_no_braces
#line 1075 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = yystack_[0].value.as < Hs::LTypeVar > ();}
#line 4100 "parser.cc"
    break;

  case 251: // tv_bndr: "{" tyvar "}"
#line 1076 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[1].location,Hs::TypeVar(yystack_[1].value.as < std::string > ())};}
#line 4106 "parser.cc"
    break;

  case 252: // tv_bndr: "{" tyvar "::" kind "}"
#line 1077 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[3].location,Hs::TypeVar(yystack_[3].value.as < std::string > ())};}
#line 4112 "parser.cc"
    break;

  case 253: // tv_bndr_no_braces: tyvar
#line 1080 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 4118 "parser.cc"
    break;

  case 254: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 1081 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[3].value.as < std::string > (),yystack_[1].value.as < Hs::Kind > ())};}
#line 4124 "parser.cc"
    break;

  case 255: // fds: %empty
#line 1085 "parser.y"
                                    { /* default to empty */ }
#line 4130 "parser.cc"
    break;

  case 256: // fds: "|" fds1
#line 1086 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > () = yystack_[0].value.as < std::vector<Hs::FunDep> > (); }
#line 4136 "parser.cc"
    break;

  case 257: // fds1: fds1 "," fd
#line 1088 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > () = yystack_[2].value.as < std::vector<Hs::FunDep> > (); yylhs.value.as < std::vector<Hs::FunDep> > ().push_back(yystack_[0].value.as < Hs::FunDep > ()); }
#line 4142 "parser.cc"
    break;

  case 258: // fds1: fd
#line 1089 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > ().push_back(yystack_[0].value.as < Hs::FunDep > ()); }
#line 4148 "parser.cc"
    break;

  case 259: // fd: varids0 "->" varids0
#line 1092 "parser.y"
                                    { yylhs.value.as < Hs::FunDep > () = Hs::FunDep(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < std::vector<Hs::LTypeVar> > ()); }
#line 4154 "parser.cc"
    break;

  case 260: // varids0: varids0 tyvar
#line 1094 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4160 "parser.cc"
    break;

  case 261: // varids0: %empty
#line 1095 "parser.y"
                                    { /* default to empty */}
#line 4166 "parser.cc"
    break;

  case 262: // kind: ctype
#line 1100 "parser.y"
             {yylhs.value.as < Hs::Kind > () = type_to_kind(yystack_[0].value.as < Hs::LType > ());}
#line 4172 "parser.cc"
    break;

  case 263: // gadt_constrlist: "where" "{" gadt_constrs0 "}"
#line 1106 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 4178 "parser.cc"
    break;

  case 264: // gadt_constrlist: "where" "vocurly" gadt_constrs0 close
#line 1107 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 4184 "parser.cc"
    break;

  case 265: // gadt_constrlist: %empty
#line 1108 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = {};}
#line 4190 "parser.cc"
    break;

  case 266: // gadt_constrs0: gadt_constrs
#line 1110 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[0].value.as < Hs::GADTConstructorsDecl > ();}
#line 4196 "parser.cc"
    break;

  case 267: // gadt_constrs0: %empty
#line 1111 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()={};}
#line 4202 "parser.cc"
    break;

  case 268: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1113 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 4208 "parser.cc"
    break;

  case 269: // gadt_constrs: gadt_constr
#line 1114 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 4214 "parser.cc"
    break;

  case 270: // gadt_constr: optSemi con_list "::" sigtype
#line 1116 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),yystack_[0].value.as < Hs::LType > ());}
#line 4220 "parser.cc"
    break;

  case 271: // constrs: "=" constrs1
#line 1118 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 4226 "parser.cc"
    break;

  case 272: // constrs1: constrs1 "|" constr
#line 1120 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 4232 "parser.cc"
    break;

  case 273: // constrs1: constr
#line 1121 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 4238 "parser.cc"
    break;

  case 274: // constr: forall context_no_ops "=>" constr_stuff
#line 1123 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::LTypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::LType > ());}
#line 4244 "parser.cc"
    break;

  case 275: // constr: forall constr_stuff
#line 1124 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::LTypeVar> > (),{}, yystack_[0].value.as < Hs::LType > ());}
#line 4250 "parser.cc"
    break;

  case 276: // forall: "forall" tv_bndrs "."
#line 1126 "parser.y"
                                {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > ();}
#line 4256 "parser.cc"
    break;

  case 277: // forall: %empty
#line 1127 "parser.y"
                                {}
#line 4262 "parser.cc"
    break;

  case 278: // constr_stuff: btype_no_ops
#line 1129 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ());}
#line 4268 "parser.cc"
    break;

  case 279: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1130 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::type_apply({
                                                                          {yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},
                                                                          Hs::type_apply(yystack_[2].value.as < std::vector<Hs::LType> > ()),
                                                                          Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ())
                                                                       });}
#line 4278 "parser.cc"
    break;

  case 280: // fielddecls: %empty
#line 1136 "parser.y"
                                {}
#line 4284 "parser.cc"
    break;

  case 281: // fielddecls: fielddecls1
#line 1137 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 4290 "parser.cc"
    break;

  case 282: // fielddecls1: fielddecls1 "," fielddecl
#line 1139 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 4296 "parser.cc"
    break;

  case 283: // fielddecls1: fielddecl
#line 1140 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 4302 "parser.cc"
    break;

  case 284: // fielddecl: sig_vars "::" ctype
#line 1142 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ());}
#line 4308 "parser.cc"
    break;

  case 285: // maybe_derivings: %empty
#line 1144 "parser.y"
                            {}
#line 4314 "parser.cc"
    break;

  case 286: // maybe_derivings: derivings
#line 1145 "parser.y"
                            {yylhs.value.as < std::vector<Hs::Deriving> > () = yystack_[0].value.as < std::vector<Hs::Deriving> > ();}
#line 4320 "parser.cc"
    break;

  case 287: // derivings: derivings deriving
#line 1147 "parser.y"
                                    {yylhs.value.as < std::vector<Hs::Deriving> > () = yystack_[1].value.as < std::vector<Hs::Deriving> > (); yylhs.value.as < std::vector<Hs::Deriving> > ().insert(yylhs.value.as < std::vector<Hs::Deriving> > ().end(), yystack_[0].value.as < std::vector<Hs::Deriving> > ().begin(), yystack_[0].value.as < std::vector<Hs::Deriving> > ().end());}
#line 4326 "parser.cc"
    break;

  case 288: // derivings: deriving
#line 1148 "parser.y"
                                    {yylhs.value.as < std::vector<Hs::Deriving> > () = yystack_[0].value.as < std::vector<Hs::Deriving> > ();}
#line 4332 "parser.cc"
    break;

  case 289: // deriving: "deriving" deriv_clause_types
#line 1151 "parser.y"
          {
              for(auto& type: yystack_[0].value.as < std::vector<Hs::LType> > ())
                  yylhs.value.as < std::vector<Hs::Deriving> > ().push_back(Hs::Deriving({}, type));
          }
#line 4341 "parser.cc"
    break;

  case 290: // deriving: "deriving" deriv_strategy_no_via deriv_clause_types
#line 1156 "parser.y"
          {
              for(auto& type: yystack_[0].value.as < std::vector<Hs::LType> > ())
                  yylhs.value.as < std::vector<Hs::Deriving> > ().push_back(Hs::Deriving(yystack_[1].value.as < Hs::DerivingStrategy > (), type));
          }
#line 4350 "parser.cc"
    break;

  case 291: // deriving: "deriving" deriv_clause_types deriv_strategy_via
#line 1161 "parser.y"
          {
              for(auto& type: yystack_[1].value.as < std::vector<Hs::LType> > ())
                  yylhs.value.as < std::vector<Hs::Deriving> > ().push_back(Hs::Deriving(Hs::DerivingStrategy::via, type, yystack_[0].value.as < Hs::LType > ()));
          }
#line 4359 "parser.cc"
    break;

  case 292: // deriv_clause_types: qtycondoc
#line 1166 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::LType> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())});}
#line 4365 "parser.cc"
    break;

  case 293: // deriv_clause_types: "(" ")"
#line 1167 "parser.y"
                                        {}
#line 4371 "parser.cc"
    break;

  case 294: // deriv_clause_types: "(" deriv_types ")"
#line 1168 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > ();}
#line 4377 "parser.cc"
    break;

  case 295: // decl_no_th: sigdecl
#line 1173 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4383 "parser.cc"
    break;

  case 296: // decl_no_th: infixexp rhs
#line 1175 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ValueDecl({yystack_[1].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ())};}
#line 4389 "parser.cc"
    break;

  case 297: // decl: decl_no_th
#line 1177 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4395 "parser.cc"
    break;

  case 298: // rhs: "=" exp wherebinds
#line 1181 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4401 "parser.cc"
    break;

  case 299: // rhs: gdrhs wherebinds
#line 1182 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 4407 "parser.cc"
    break;

  case 300: // gdrhs: gdrhs gdrh
#line 1184 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4413 "parser.cc"
    break;

  case 301: // gdrhs: gdrh
#line 1185 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4419 "parser.cc"
    break;

  case 302: // gdrh: "|" guardquals "=" exp
#line 1189 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4425 "parser.cc"
    break;

  case 303: // sigdecl: sig_vars "::" sigtypedoc
#line 1199 "parser.y"
                                  { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeSigDecl{yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ()}}; }
#line 4431 "parser.cc"
    break;

  case 304: // sigdecl: infix prec ops
#line 1200 "parser.y"
                         { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<Located<std::string>> > ()}}; }
#line 4437 "parser.cc"
    break;

  case 305: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1202 "parser.y"
                                                    {}
#line 4443 "parser.cc"
    break;

  case 306: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1203 "parser.y"
                                            { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::InlinePragma({yystack_[3].location,yystack_[3].value.as < std::string > ()},{yystack_[1].location,yystack_[1].value.as < std::string > ()})}; }
#line 4449 "parser.cc"
    break;

  case 307: // sigdecl: "{-# SCC" qvar "#-}"
#line 1204 "parser.y"
                              {}
#line 4455 "parser.cc"
    break;

  case 308: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1205 "parser.y"
                                     {}
#line 4461 "parser.cc"
    break;

  case 309: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1206 "parser.y"
                                                               {}
#line 4467 "parser.cc"
    break;

  case 310: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1207 "parser.y"
                                                                      {}
#line 4473 "parser.cc"
    break;

  case 311: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1208 "parser.y"
                                                     {}
#line 4479 "parser.cc"
    break;

  case 316: // exp: infixexp "::" sigtype
#line 1220 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[2].location+yystack_[0].location, Hs::TypedExp({yystack_[2].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[2].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::LType > ())}; }
#line 4485 "parser.cc"
    break;

  case 317: // exp: infixexp
#line 1221 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}; }
#line 4491 "parser.cc"
    break;

  case 318: // infixexp: exp10
#line 1225 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location,Hs::InfixExp({yystack_[0].value.as < Located<expression_ref> > ()})};}
#line 4497 "parser.cc"
    break;

  case 319: // infixexp: infixexp qop exp10
#line 1226 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < expression_ref > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4503 "parser.cc"
    break;

  case 320: // exp10: PREFIX_MINUS fexp
#line 1228 "parser.y"
                                        {yylhs.value.as < Located<expression_ref> > () = {yystack_[1].location+yystack_[0].location,Hs::InfixExp( { {yystack_[1].location,Hs::Neg()}, yystack_[0].value.as < Located<expression_ref> > ()} )};}
#line 4509 "parser.cc"
    break;

  case 321: // exp10: fexp
#line 1229 "parser.y"
                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4515 "parser.cc"
    break;

  case 324: // fexp: fexp aexp
#line 1237 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = make_parsed_app(yylhs.location, yystack_[1].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ());}
#line 4521 "parser.cc"
    break;

  case 325: // fexp: fexp "@" atype
#line 1238 "parser.y"
                                 {}
#line 4527 "parser.cc"
    break;

  case 326: // fexp: "static" aexp
#line 1239 "parser.y"
                                 {}
#line 4533 "parser.cc"
    break;

  case 327: // fexp: aexp
#line 1240 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4539 "parser.cc"
    break;

  case 328: // aexp: qvar TIGHT_INFIX_AT aexp
#line 1243 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::AsPattern({yystack_[2].location,Hs::Var(yystack_[2].value.as < std::string > ())},yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4545 "parser.cc"
    break;

  case 329: // aexp: PREFIX_TILDE aexp
#line 1244 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LazyPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4551 "parser.cc"
    break;

  case 330: // aexp: PREFIX_BANG aexp
#line 1245 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4557 "parser.cc"
    break;

  case 331: // aexp: "\\" apats1 "->" exp
#line 1246 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LambdaExp(yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4563 "parser.cc"
    break;

  case 332: // aexp: "let" binds "in" exp
#line 1247 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4569 "parser.cc"
    break;

  case 333: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1249 "parser.y"
                                                       {yylhs.value.as < Located<expression_ref> > () = {yystack_[7].location+yystack_[0].location,Hs::IfExp(yystack_[6].value.as < Located<expression_ref> > (),yystack_[3].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4575 "parser.cc"
    break;

  case 334: // aexp: "case" exp "of" altslist
#line 1251 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::CaseExp(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::Alts > ())}; }
#line 4581 "parser.cc"
    break;

  case 335: // aexp: "do" stmtlist
#line 1252 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Do(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4587 "parser.cc"
    break;

  case 336: // aexp: "mdo" stmtlist
#line 1253 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::MDo(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4593 "parser.cc"
    break;

  case 337: // aexp: aexp1
#line 1255 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4599 "parser.cc"
    break;

  case 338: // aexp1: aexp1 "{" fbinds "}"
#line 1258 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = make_record_expression(yylhs.location, yystack_[3].value.as < Located<expression_ref> > (), yystack_[1].value.as < Located<Hs::FieldBindings> > ()); }
#line 4605 "parser.cc"
    break;

  case 339: // aexp1: aexp1 TIGHT_INFIX_DOT field
#line 1259 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = make_record_field_selection(yylhs.location, yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < std::string > ()); }
#line 4611 "parser.cc"
    break;

  case 340: // aexp1: aexp2
#line 1260 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > (); }
#line 4617 "parser.cc"
    break;

  case 341: // aexp2: qvar
#line 1263 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Var(yystack_[0].value.as < std::string > ())};}
#line 4623 "parser.cc"
    break;

  case 342: // aexp2: qcon
#line 1264 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Con(yystack_[0].value.as < std::string > ())};}
#line 4629 "parser.cc"
    break;

  case 343: // aexp2: literal
#line 1265 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[0].value.as < expression_ref > ()};}
#line 4635 "parser.cc"
    break;

  case 344: // aexp2: "(" texp ")"
#line 1266 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, unloc(yystack_[1].value.as < Located<expression_ref> > ())};}
#line 4641 "parser.cc"
    break;

  case 345: // aexp2: "(" tup_exprs ")"
#line 1267 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Tuple(yystack_[1].value.as < std::vector<Located<expression_ref>> > ())};}
#line 4647 "parser.cc"
    break;

  case 346: // aexp2: "(" projection ")"
#line 1268 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = make_record_projection(yylhs.location, yystack_[1].value.as < std::vector<std::string> > ());}
#line 4653 "parser.cc"
    break;

  case 347: // aexp2: "[" list "]"
#line 1273 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[1].value.as < expression_ref > ()};}
#line 4659 "parser.cc"
    break;

  case 348: // aexp2: "_"
#line 1274 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::WildcardPattern()};}
#line 4665 "parser.cc"
    break;

  case 349: // projection: projection TIGHT_INFIX_DOT field
#line 1277 "parser.y"
                                              {yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 4671 "parser.cc"
    break;

  case 350: // projection: PREFIX_DOT field
#line 1278 "parser.y"
                                              {yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 4677 "parser.cc"
    break;

  case 351: // texp: exp
#line 1283 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4683 "parser.cc"
    break;

  case 352: // texp: infixexp qop
#line 1284 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LeftSection ( {yystack_[1].value.as < Located<Hs::InfixExp> > ().loc, unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())}, {yystack_[0].location,yystack_[0].value.as < expression_ref > ()} )}; }
#line 4689 "parser.cc"
    break;

  case 353: // texp: qopm infixexp
#line 1285 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RightSection( {yystack_[1].location,yystack_[1].value.as < expression_ref > ()}, {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())} )}; }
#line 4695 "parser.cc"
    break;

  case 354: // tup_exprs: tup_exprs "," texp
#line 1290 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4701 "parser.cc"
    break;

  case 355: // tup_exprs: texp "," texp
#line 1291 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4707 "parser.cc"
    break;

  case 356: // list: texp
#line 1309 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List({yystack_[0].value.as < Located<expression_ref> > ()}); }
#line 4713 "parser.cc"
    break;

  case 357: // list: lexps
#line 1310 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List(yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4719 "parser.cc"
    break;

  case 358: // list: texp ".."
#line 1311 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4725 "parser.cc"
    break;

  case 359: // list: texp "," exp ".."
#line 1312 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4731 "parser.cc"
    break;

  case 360: // list: texp ".." exp
#line 1313 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4737 "parser.cc"
    break;

  case 361: // list: texp "," exp ".." exp
#line 1314 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < Located<expression_ref> > (), yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4743 "parser.cc"
    break;

  case 362: // list: texp "|" squals
#line 1315 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4749 "parser.cc"
    break;

  case 363: // lexps: lexps "," texp
#line 1317 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4755 "parser.cc"
    break;

  case 364: // lexps: texp "," texp
#line 1318 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4761 "parser.cc"
    break;

  case 365: // squals: squals "," qual
#line 1331 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4767 "parser.cc"
    break;

  case 366: // squals: qual
#line 1333 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4773 "parser.cc"
    break;

  case 367: // guardquals: guardquals1
#line 1343 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[0].value.as < std::vector<Located<expression_ref>> > ();}
#line 4779 "parser.cc"
    break;

  case 368: // guardquals1: guardquals1 "," qual
#line 1345 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > ();yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4785 "parser.cc"
    break;

  case 369: // guardquals1: qual
#line 1346 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4791 "parser.cc"
    break;

  case 370: // altslist: "{" alts "}"
#line 1349 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4797 "parser.cc"
    break;

  case 371: // altslist: "vocurly" alts close
#line 1350 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4803 "parser.cc"
    break;

  case 372: // altslist: "{" "}"
#line 1351 "parser.y"
                                 {}
#line 4809 "parser.cc"
    break;

  case 373: // altslist: "vocurly" close
#line 1352 "parser.y"
                                 {}
#line 4815 "parser.cc"
    break;

  case 374: // alts: alts1
#line 1354 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4821 "parser.cc"
    break;

  case 375: // alts: ";" alts
#line 1355 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4827 "parser.cc"
    break;

  case 376: // alts1: alts1 ";" alt
#line 1357 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4833 "parser.cc"
    break;

  case 377: // alts1: alts1 ";"
#line 1358 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4839 "parser.cc"
    break;

  case 378: // alts1: alt
#line 1359 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4845 "parser.cc"
    break;

  case 379: // alt: pat alt_rhs
#line 1361 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 4851 "parser.cc"
    break;

  case 380: // alt_rhs: "->" exp wherebinds
#line 1363 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4857 "parser.cc"
    break;

  case 381: // alt_rhs: gdpats wherebinds
#line 1364 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4863 "parser.cc"
    break;

  case 382: // gdpats: gdpats gdpat
#line 1366 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4869 "parser.cc"
    break;

  case 383: // gdpats: gdpat
#line 1367 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4875 "parser.cc"
    break;

  case 384: // gdpat: "|" guardquals "->" exp
#line 1376 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4881 "parser.cc"
    break;

  case 385: // pat: exp
#line 1378 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4887 "parser.cc"
    break;

  case 386: // bindpat: exp
#line 1380 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4893 "parser.cc"
    break;

  case 387: // apat: aexp
#line 1382 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4899 "parser.cc"
    break;

  case 388: // apats1: apats1 apat
#line 1384 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4905 "parser.cc"
    break;

  case 389: // apats1: apat
#line 1385 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4911 "parser.cc"
    break;

  case 390: // stmtlist: "{" stmts "}"
#line 1388 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4917 "parser.cc"
    break;

  case 391: // stmtlist: "vocurly" stmts close
#line 1389 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4923 "parser.cc"
    break;

  case 392: // stmts: stmts ";" stmt
#line 1391 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4929 "parser.cc"
    break;

  case 393: // stmts: stmts ";"
#line 1392 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 4935 "parser.cc"
    break;

  case 394: // stmts: stmt
#line 1393 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4941 "parser.cc"
    break;

  case 395: // stmts: %empty
#line 1394 "parser.y"
                       {}
#line 4947 "parser.cc"
    break;

  case 396: // stmt: qual
#line 1399 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4953 "parser.cc"
    break;

  case 397: // stmt: "rec" stmtlist
#line 1400 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ())};}
#line 4959 "parser.cc"
    break;

  case 398: // qual: bindpat "<-" exp
#line 1402 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::PatQual(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4965 "parser.cc"
    break;

  case 399: // qual: exp
#line 1403 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::SimpleQual(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4971 "parser.cc"
    break;

  case 400: // qual: "let" binds
#line 1404 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ())};}
#line 4977 "parser.cc"
    break;

  case 401: // fbinds: fbinds1
#line 1409 "parser.y"
                        {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > ();}
#line 4983 "parser.cc"
    break;

  case 402: // fbinds: %empty
#line 1410 "parser.y"
                        {}
#line 4989 "parser.cc"
    break;

  case 403: // fbinds1: fbind "," fbinds1
#line 1412 "parser.y"
                            {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > (); unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).fields.insert(unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).fields.begin(), *yystack_[2].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4995 "parser.cc"
    break;

  case 404: // fbinds1: fbind
#line 1413 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).fields.push_back(*yystack_[0].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 5001 "parser.cc"
    break;

  case 405: // fbinds1: ".."
#line 1414 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).dotdot = yylhs.location; yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 5007 "parser.cc"
    break;

  case 406: // fbind: qvar "=" texp
#line 1416 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[2].value.as < std::string > ())}, yystack_[0].value.as < Located<expression_ref> > ())}};}
#line 5013 "parser.cc"
    break;

  case 407: // fbind: qvar
#line 1417 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[0].value.as < std::string > ())})}};}
#line 5019 "parser.cc"
    break;

  case 408: // fbind: field TIGHT_INFIX_DOT fieldToUpdate "=" texp
#line 1418 "parser.y"
                                                      {}
#line 5025 "parser.cc"
    break;

  case 409: // fbind: field TIGHT_INFIX_DOT fieldToUpdate
#line 1419 "parser.y"
                                                      {}
#line 5031 "parser.cc"
    break;

  case 412: // qcon: gen_qcon
#line 1455 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5037 "parser.cc"
    break;

  case 413: // qcon: sysdcon
#line 1456 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5043 "parser.cc"
    break;

  case 414: // gen_qcon: qconid
#line 1458 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5049 "parser.cc"
    break;

  case 415: // gen_qcon: "(" qconsym ")"
#line 1459 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5055 "parser.cc"
    break;

  case 416: // con: conid
#line 1461 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5061 "parser.cc"
    break;

  case 417: // con: "(" consym ")"
#line 1462 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5067 "parser.cc"
    break;

  case 418: // con: sysdcon
#line 1463 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5073 "parser.cc"
    break;

  case 419: // con_list: con_list "," con
#line 1465 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 5079 "parser.cc"
    break;

  case 420: // con_list: con
#line 1466 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 5085 "parser.cc"
    break;

  case 421: // sysdcon_no_list: "(" ")"
#line 1468 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 5091 "parser.cc"
    break;

  case 422: // sysdcon_no_list: "(" commas ")"
#line 1469 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 5097 "parser.cc"
    break;

  case 423: // sysdcon_no_list: "(#" "#)"
#line 1470 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 5103 "parser.cc"
    break;

  case 424: // sysdcon_no_list: "(#" commas "#)"
#line 1471 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 5109 "parser.cc"
    break;

  case 425: // sysdcon: sysdcon_no_list
#line 1473 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5115 "parser.cc"
    break;

  case 426: // sysdcon: "[" "]"
#line 1474 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 5121 "parser.cc"
    break;

  case 427: // conop: consym
#line 1476 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5127 "parser.cc"
    break;

  case 428: // conop: "`" conid "`"
#line 1477 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5133 "parser.cc"
    break;

  case 429: // qconop: qconsym
#line 1479 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5139 "parser.cc"
    break;

  case 430: // qconop: "`" qconid "`"
#line 1480 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5145 "parser.cc"
    break;

  case 431: // gtycon: ntgtycon
#line 1483 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5151 "parser.cc"
    break;

  case 432: // gtycon: "(" ")"
#line 1484 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 5157 "parser.cc"
    break;

  case 433: // gtycon: "(#" "#)"
#line 1485 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 5163 "parser.cc"
    break;

  case 434: // ntgtycon: oqtycon
#line 1487 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5169 "parser.cc"
    break;

  case 435: // ntgtycon: "(" commas ")"
#line 1488 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 5175 "parser.cc"
    break;

  case 436: // ntgtycon: "(#" commas "#)"
#line 1489 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 5181 "parser.cc"
    break;

  case 437: // ntgtycon: "(" "->" ")"
#line 1490 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 5187 "parser.cc"
    break;

  case 438: // ntgtycon: "[" "]"
#line 1491 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 5193 "parser.cc"
    break;

  case 439: // oqtycon: qtycon
#line 1493 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5199 "parser.cc"
    break;

  case 440: // oqtycon: "(" qtyconsym ")"
#line 1494 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5205 "parser.cc"
    break;

  case 441: // oqtycon_no_varcon: qtycon
#line 1496 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5211 "parser.cc"
    break;

  case 442: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1497 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5217 "parser.cc"
    break;

  case 443: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1498 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5223 "parser.cc"
    break;

  case 444: // oqtycon_no_varcon: "(" ":" ")"
#line 1499 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 5229 "parser.cc"
    break;

  case 445: // qtyconop: qtyconsym
#line 1502 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5235 "parser.cc"
    break;

  case 446: // qtyconop: "`" qtycon "`"
#line 1503 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5241 "parser.cc"
    break;

  case 447: // qtycondoc: qtycon
#line 1505 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5247 "parser.cc"
    break;

  case 448: // qtycon: "QCONID"
#line 1507 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5253 "parser.cc"
    break;

  case 449: // qtycon: tycon
#line 1508 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5259 "parser.cc"
    break;

  case 450: // tycon: "CONID"
#line 1512 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5265 "parser.cc"
    break;

  case 451: // qtyconsym: "QCONSYM"
#line 1514 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5271 "parser.cc"
    break;

  case 452: // qtyconsym: "QVARSYM"
#line 1515 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5277 "parser.cc"
    break;

  case 453: // qtyconsym: tyconsym
#line 1516 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5283 "parser.cc"
    break;

  case 454: // tyconsym: "CONSYM"
#line 1518 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5289 "parser.cc"
    break;

  case 455: // tyconsym: "VARSYM"
#line 1519 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5295 "parser.cc"
    break;

  case 456: // tyconsym: ":"
#line 1520 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 5301 "parser.cc"
    break;

  case 457: // tyconsym: "-"
#line 1521 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 5307 "parser.cc"
    break;

  case 458: // op: varop
#line 1526 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5313 "parser.cc"
    break;

  case 459: // op: conop
#line 1527 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5319 "parser.cc"
    break;

  case 460: // varop: varsym
#line 1529 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5325 "parser.cc"
    break;

  case 461: // varop: "`" varid "`"
#line 1530 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5331 "parser.cc"
    break;

  case 462: // qop: qvarop
#line 1532 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 5337 "parser.cc"
    break;

  case 463: // qop: qconop
#line 1533 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 5343 "parser.cc"
    break;

  case 464: // qopm: qvaropm
#line 1536 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 5349 "parser.cc"
    break;

  case 465: // qopm: qconop
#line 1537 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 5355 "parser.cc"
    break;

  case 466: // qvarop: qvarsym
#line 1542 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5361 "parser.cc"
    break;

  case 467: // qvarop: "`" qvarid "`"
#line 1543 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5367 "parser.cc"
    break;

  case 468: // qvaropm: qvarsym_no_minus
#line 1545 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 5373 "parser.cc"
    break;

  case 469: // qvaropm: "`" qvarid "`"
#line 1546 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5379 "parser.cc"
    break;

  case 470: // tyvar: tyvarid
#line 1550 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5385 "parser.cc"
    break;

  case 471: // tyvarop: "`" tyvarid "`"
#line 1552 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5391 "parser.cc"
    break;

  case 472: // tyvarid: "VARID"
#line 1554 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5397 "parser.cc"
    break;

  case 473: // tyvarid: special_id
#line 1555 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5403 "parser.cc"
    break;

  case 474: // tyvarid: "unsafe"
#line 1556 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 5409 "parser.cc"
    break;

  case 475: // tyvarid: "safe"
#line 1557 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 5415 "parser.cc"
    break;

  case 476: // tyvarid: "interruptible"
#line 1558 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 5421 "parser.cc"
    break;

  case 477: // var: varid
#line 1561 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5427 "parser.cc"
    break;

  case 478: // var: "(" varsym ")"
#line 1562 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5433 "parser.cc"
    break;

  case 479: // qvar: qvarid
#line 1564 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5439 "parser.cc"
    break;

  case 480: // qvar: "(" varsym ")"
#line 1565 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5445 "parser.cc"
    break;

  case 481: // qvar: "(" qvarsym1 ")"
#line 1566 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5451 "parser.cc"
    break;

  case 482: // field: varid
#line 1568 "parser.y"
       { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5457 "parser.cc"
    break;

  case 483: // qvarid: varid
#line 1570 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5463 "parser.cc"
    break;

  case 484: // qvarid: "QVARID"
#line 1571 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5469 "parser.cc"
    break;

  case 485: // varid: "VARID"
#line 1573 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5475 "parser.cc"
    break;

  case 486: // varid: special_id
#line 1574 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5481 "parser.cc"
    break;

  case 487: // varid: "unsafe"
#line 1575 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 5487 "parser.cc"
    break;

  case 488: // varid: "safe"
#line 1576 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 5493 "parser.cc"
    break;

  case 489: // varid: "interruptible"
#line 1577 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 5499 "parser.cc"
    break;

  case 490: // varid: "forall"
#line 1578 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 5505 "parser.cc"
    break;

  case 491: // varid: "family"
#line 1579 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 5511 "parser.cc"
    break;

  case 492: // varid: "role"
#line 1580 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 5517 "parser.cc"
    break;

  case 493: // qvarsym: varsym
#line 1582 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5523 "parser.cc"
    break;

  case 494: // qvarsym: qvarsym1
#line 1583 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5529 "parser.cc"
    break;

  case 495: // qvarsym_no_minus: varsym_no_minus
#line 1585 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5535 "parser.cc"
    break;

  case 496: // qvarsym_no_minus: qvarsym1
#line 1586 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5541 "parser.cc"
    break;

  case 497: // qvarsym1: "QVARSYM"
#line 1588 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5547 "parser.cc"
    break;

  case 498: // varsym: varsym_no_minus
#line 1590 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5553 "parser.cc"
    break;

  case 499: // varsym: "-"
#line 1591 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 5559 "parser.cc"
    break;

  case 500: // varsym_no_minus: "VARSYM"
#line 1593 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5565 "parser.cc"
    break;

  case 501: // varsym_no_minus: special_sym
#line 1594 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5571 "parser.cc"
    break;

  case 502: // special_id: "as"
#line 1596 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 5577 "parser.cc"
    break;

  case 503: // special_id: "qualified"
#line 1597 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 5583 "parser.cc"
    break;

  case 504: // special_id: "hiding"
#line 1598 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 5589 "parser.cc"
    break;

  case 505: // special_id: "export"
#line 1599 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 5595 "parser.cc"
    break;

  case 506: // special_id: "label"
#line 1600 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 5601 "parser.cc"
    break;

  case 507: // special_id: "dynamic"
#line 1601 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 5607 "parser.cc"
    break;

  case 508: // special_id: "stdcall"
#line 1602 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 5613 "parser.cc"
    break;

  case 509: // special_id: "ccall"
#line 1603 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 5619 "parser.cc"
    break;

  case 510: // special_id: "capi"
#line 1604 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 5625 "parser.cc"
    break;

  case 511: // special_id: "prim"
#line 1605 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 5631 "parser.cc"
    break;

  case 512: // special_id: "javascript"
#line 1606 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 5637 "parser.cc"
    break;

  case 513: // special_id: "group"
#line 1607 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 5643 "parser.cc"
    break;

  case 514: // special_id: "stock"
#line 1608 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 5649 "parser.cc"
    break;

  case 515: // special_id: "anyclass"
#line 1609 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 5655 "parser.cc"
    break;

  case 516: // special_id: "via"
#line 1610 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 5661 "parser.cc"
    break;

  case 517: // special_id: "unit"
#line 1611 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 5667 "parser.cc"
    break;

  case 518: // special_id: "dependency"
#line 1612 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 5673 "parser.cc"
    break;

  case 519: // special_id: "signature"
#line 1613 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 5679 "parser.cc"
    break;

  case 520: // special_sym: "."
#line 1615 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 5685 "parser.cc"
    break;

  case 521: // special_sym: "*"
#line 1616 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 5691 "parser.cc"
    break;

  case 522: // qconid: conid
#line 1620 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5697 "parser.cc"
    break;

  case 523: // qconid: "QCONID"
#line 1621 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5703 "parser.cc"
    break;

  case 524: // conid: "CONID"
#line 1623 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5709 "parser.cc"
    break;

  case 525: // qconsym: consym
#line 1625 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5715 "parser.cc"
    break;

  case 526: // qconsym: "QCONSYM"
#line 1626 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5721 "parser.cc"
    break;

  case 527: // consym: "CONSYM"
#line 1628 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5727 "parser.cc"
    break;

  case 528: // consym: ":"
#line 1629 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 5733 "parser.cc"
    break;

  case 529: // literal: "CHAR"
#line 1633 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 5739 "parser.cc"
    break;

  case 530: // literal: "STRING"
#line 1634 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 5745 "parser.cc"
    break;

  case 531: // literal: "INTEGER"
#line 1635 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < integer > ()});}
#line 5751 "parser.cc"
    break;

  case 532: // literal: "RATIONAL"
#line 1636 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Floating{yystack_[0].value.as < rational > ()});}
#line 5757 "parser.cc"
    break;

  case 533: // literal: "PRIMINTEGER"
#line 1637 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < integer > ()});}
#line 5763 "parser.cc"
    break;

  case 535: // close: error
#line 1645 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 5769 "parser.cc"
    break;

  case 536: // modid: "CONID"
#line 1649 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5775 "parser.cc"
    break;

  case 537: // modid: "QCONID"
#line 1650 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5781 "parser.cc"
    break;

  case 538: // commas: commas ","
#line 1652 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 5787 "parser.cc"
    break;

  case 539: // commas: ","
#line 1653 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 5793 "parser.cc"
    break;


#line 5797 "parser.cc"

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


  const short parser::yypact_ninf_ = -720;

  const short parser::yytable_ninf_ = -499;

  const short
  parser::yypact_[] =
  {
      40,   232,  -720,   120,  -720,  -720,  -720,  -720,  -720,   444,
       6,    82,  -720,    38,    80,    80,    97,  -720,  -720,  -720,
    -720,   213,  -720,  -720,  -720,   118,  -720,   235,   276,  1535,
     253,   314,   244,  -720,   957,  -720,     3,  -720,  -720,  -720,
     232,  -720,   232,  -720,  -720,  -720,  -720,  -720,  -720,  -720,
    -720,  -720,  -720,  -720,  -720,  -720,  -720,  -720,  -720,  -720,
    -720,  -720,  -720,  -720,  -720,   428,  -720,  -720,  -720,  -720,
    -720,   267,   117,  -720,   285,  -720,  -720,  -720,  -720,  -720,
    -720,  -720,   215,  -720,   232,  -720,   291,  -720,  2671,  4679,
     366,   257,   386,   284,  2671,  -720,  -720,  -720,   503,   344,
    -720,  3683,   408,   284,  3361,  5087,     9,  3361,  3361,  2947,
    3361,  1981,  1843,   299,   316,  -720,  -720,  -720,  -720,  -720,
    -720,  -720,    28,   316,   292,   244,  -720,  -720,  -720,  -720,
    -720,    53,    22,  -720,  -720,   305,  -720,  3085,  -720,   260,
    -720,  -720,  -720,  -720,  -720,  -720,   345,   112,  -720,  -720,
    -720,  -720,   303,  -720,  -720,   329,  -720,  -720,  -720,  -720,
     349,  -720,   361,   375,   381,  -720,  -720,  -720,  4781,  -720,
    4818,  -720,  -720,  -720,  -720,   442,  -720,  1843,   492,   700,
    -720,  -720,  -720,  4679,  4679,  -720,  5186,  3873,  3475,   418,
    -720,   460,   465,  -720,   249,  -720,  4063,  -720,  -720,  -720,
    -720,  -720,  -720,  -720,  4679,   448,  -720,  4151,  -720,  -720,
    -720,  4679,   533,   550,  2395,  2395,  -720,   468,   506,   512,
     514,   519,  4151,  1400,  1400,  -720,   584,  4679,  4679,   174,
     132,   526,   713,   135,   498,  -720,  -720,   270,    -4,   490,
      63,  -720,   150,  -720,  -720,  -720,  -720,  3223,  -720,  3085,
    -720,  -720,  -720,  4985,  -720,  -720,  -720,   700,   144,   500,
     491,  -720,  2671,  -720,  -720,  -720,  -720,  -720,  -720,  5318,
    -720,  -720,   184,    -5,   263,   375,   499,   501,   505,   293,
    -720,   332,   109,  5087,  -720,  4151,  5087,  5087,  -720,   379,
     291,   542,   483,  4679,  4151,  5186,  2671,  2809,  4985,  -720,
      36,  -720,  -720,  2671,  -720,  -720,  -720,  -720,  4679,  -720,
    5318,  5022,  3361,  -720,  -720,  -720,  -720,  -720,  -720,  -720,
     510,   511,   507,  -720,   520,    38,   232,    41,   383,  4151,
    -720,  -720,   353,   155,   525,   516,  -720,  -720,  -720,  -720,
     522,   539,   545,  -720,  -720,   527,  -720,  -720,  -720,  -720,
    -720,  -720,   529,   537,   532,  -720,   297,   362,  -720,   631,
    4679,  4151,  1641,  4679,  -720,  -720,  -720,  4679,  -720,  -720,
     578,  4151,  -720,  -720,  -720,  -720,  4151,  4151,   344,   284,
     574,   577,     0,  -720,  -720,    45,  -720,   640,  -720,  -720,
    -720,  -720,   639,   137,  -720,  -720,   305,    65,  2671,  -720,
     590,   439,   437,    47,  4151,   174,  4151,  -720,  -720,  -720,
     540,  -720,   601,   566,   431,     9,   604,  2671,  -720,   565,
     569,  2671,  2671,  2809,  2119,  -720,  2119,   738,  -720,  -720,
    5318,  -720,  -720,  2119,  -720,  2119,   158,  -720,  -720,  -720,
    -720,   551,   582,   615,   616,   617,   620,  5124,   587,  -720,
    -720,  -720,  -720,  -720,  4239,    15,   447,  -720,  -720,  -720,
    -720,   677,   626,   589,  -720,   592,   344,  -720,  -720,  -720,
    -720,  -720,  -720,   609,  -720,   597,   628,   623,   624,  -720,
    -720,  -720,  4920,  -720,  -720,  -720,   605,  1576,  -720,  -720,
    2257,  1705,  -720,  -720,   613,  4151,  -720,  5186,  5355,  -720,
    4151,  4151,  -720,  -720,  4151,  -720,  -720,  -720,   599,  -720,
    5509,   389,  -720,  -720,  -720,   607,   612,   518,  -720,  4151,
    -720,  -720,   621,   622,  -720,  -720,   584,  -720,  2671,  -720,
    2395,  -720,  2671,   411,  -720,  -720,  1400,  -720,  -720,  4151,
    4151,  5468,   651,  -720,  -720,  -720,    47,  -720,  -720,  -720,
    -720,  -720,  5186,  -720,  -720,   627,   340,   371,  -720,  -720,
    -720,  -720,  -720,  -720,  -720,  -720,   625,  -720,   664,  -720,
    -720,  -720,  -720,  -720,   634,  -720,  -720,  -720,  4151,  4151,
     633,   635,   379,  -720,   447,   655,  -720,  -720,   671,  4151,
     729,   732,   754,  -720,  2671,  2809,  -720,  -720,  -720,  5022,
    2119,  5318,  -720,  1576,   232,  -720,   285,   650,   127,  -720,
    -720,  2533,  -720,   660,   648,  -720,   157,    38,  -720,  -720,
    -720,  -720,  4151,  5604,  5604,  -720,  -720,  -720,  -720,  -720,
     654,  -720,  -720,  -720,  1262,  1262,  -720,  -720,  -720,  -720,
    -720,  4151,  -720,  -720,   468,  1118,  1118,  -720,  -720,  -720,
    -720,  -720,  5604,   743,  -720,   694,  -720,  -720,  2809,  2671,
    -720,  -720,    13,   110,  -720,  -720,  -720,  5223,   732,   754,
    4679,  -720,  -720,  -720,   691,  -720,  4679,   430,   754,   243,
    -720,   754,  -720,  -720,  -720,  -720,  -720,   124,  -720,   663,
    -720,  -720,  -720,  4883,  -720,  -720,  -720,  2671,  2809,  2671,
    -720,    74,  -720,  -720,  -720,    24,   697,  -720,  -720,  5604,
     742,  3975,  -720,  -720,   173,  -720,    68,  -720,   771,  -720,
     764,  -720,   764,  -720,   204,  -720,    83,  -720,   702,   476,
    -720,  4151,  -720,  -720,  -720,  4151,  -720,  4679,  4679,   754,
    -720,  -720,  5392,   729,   692,  3579,  -720,  -720,  -720,   226,
      84,  -720,  4327,   202,   735,  -720,  -720,  -720,  2119,  5318,
    -720,  -720,  -720,   704,   677,  -720,  -720,  4151,  -720,  4151,
    -720,  4679,  4679,  4679,  -720,   452,  -720,  1262,  -720,  2671,
    -720,  4679,   542,  -720,  1118,  -720,  5604,  4415,  4503,  -720,
    -720,  -720,  -720,   695,   518,  -720,  -720,  -720,  4679,   667,
    -720,  4679,   689,   680,  -720,     9,    38,  -720,  -720,   681,
     688,  -720,  -720,  -720,  -720,  2671,  -720,   703,   699,   578,
    -720,   504,  4151,  4591,  -720,  -720,  -720,  -720,  4239,  -720,
    5604,  -720,   709,   252,  -720,    38,    89,  4679,  3771,  -720,
    4679,  -720,   468,   165,  -720,  4679,  -720,  -720,  -720,  -720,
    -720,  5567,  -720,  -720,  3475,   718,   721,   447,  -720,  -720,
    -720,  4679,  -720,  -720,  -720,  -720,  4151,  -720,   697,  5604,
     732,   754,  -720,  -720,  -720,   754,  -720,  -720
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    40,     0,     2,    40,     4,   536,   537,     8,
       0,    43,     1,     0,     0,     0,    18,    11,    39,    13,
      16,    70,   535,   534,    12,   199,   195,     0,     0,    20,
       0,    46,    41,    15,    14,   198,     0,     6,     7,   502,
       0,   504,     0,   503,   490,   505,   506,   507,   488,   489,
     487,   491,   492,   508,   509,   510,   511,   512,   513,   514,
     515,   516,   517,   519,   518,     0,   485,   450,   484,   448,
      22,     0,    19,    24,    28,    36,   441,   449,    35,   479,
     483,   486,     0,    45,     0,    38,    42,   348,     0,     0,
     137,   139,     0,     0,     0,    63,    64,    65,   101,     0,
     138,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   312,   524,   523,   529,   530,   531,
     532,   533,   312,   312,    61,    68,    71,    72,    73,    74,
      75,   159,     0,    78,   295,    79,   318,   321,   327,   337,
     340,   342,   412,   425,   413,   205,   341,   483,   414,   522,
     343,   196,     0,    27,    26,     0,   499,   521,   520,   500,
       0,   497,     0,     0,     0,   498,   501,    17,     0,    21,
      31,    25,    40,    40,     3,    48,    37,     0,     0,   317,
     475,   476,   474,     0,     0,   233,   280,     0,     0,     0,
     472,   255,     0,   152,   216,   219,   220,   224,   231,   434,
     439,   232,   470,   473,     0,     0,   140,     0,   104,   102,
     103,     0,     0,     0,   395,   395,   335,   323,     0,     0,
       0,     0,     0,   189,   189,   192,     0,     0,     0,     0,
       0,     0,   216,   434,     0,   336,   326,     0,     0,     0,
       0,   420,   200,   418,   416,   387,   389,     0,   329,   320,
     330,   528,   426,     0,   527,   526,   351,   317,   356,     0,
     357,   465,     0,   464,   468,   496,   495,   429,   525,     0,
     421,   539,     0,     0,     0,   496,     0,   495,   429,     0,
     423,     0,     0,     0,   313,     0,     0,     0,    62,     0,
      69,   159,     0,     0,     0,     0,     0,     0,     0,   296,
     194,   301,   463,     0,   462,   466,   494,   493,     0,   324,
       0,   402,     0,   197,   444,   443,   442,   481,   480,    23,
       0,     0,    32,    34,     0,     0,     0,    50,     0,     0,
     235,   234,     0,     0,     0,   281,   283,   477,   249,   438,
       0,   208,     0,   212,   456,     0,   457,   237,   455,   454,
     452,   451,   246,     0,     0,   453,     0,     0,   261,   175,
       0,     0,     0,     0,   228,   445,   229,     0,   225,   227,
     143,   245,   241,   202,   106,   105,     0,     0,     0,     0,
     399,     0,     0,   394,   396,     0,   322,     0,    98,    97,
      99,   100,   185,     0,   297,   188,     0,     0,     0,    94,
       0,   145,     0,   160,     0,     0,     0,    80,    81,    82,
       0,   307,     0,     0,     0,     0,     0,     0,   388,     0,
       0,   352,   358,     0,     0,   347,     0,   353,   350,   482,
       0,   346,   344,     0,   345,     0,   480,   415,   422,   538,
     424,     0,     0,     0,     0,     0,     0,     0,   304,   459,
      67,   458,   460,   427,     0,     0,   141,   303,   213,   203,
     204,   194,     0,   367,   369,     0,     0,   299,   300,   319,
     325,   339,   405,     0,   401,   404,   407,     0,   483,   328,
      30,    29,     0,     9,    10,    47,     0,    54,    44,    49,
       0,     0,   334,   316,     0,     0,   236,     0,     0,   239,
       0,     0,   437,   240,     0,   440,   435,   436,   256,   258,
       0,     0,    83,   151,   217,     0,     0,   221,   226,     0,
      89,   246,     0,   244,   107,   108,   400,   397,     0,   390,
     393,   391,     0,     0,    93,   190,   187,   191,   332,     0,
       0,     0,   109,   165,   164,    84,   161,   162,   262,    90,
      91,    85,     0,   308,   417,     0,     0,     0,   201,   431,
     419,   305,   331,   469,   430,   360,   362,   366,   351,   364,
     363,   349,   355,   354,     0,   314,   306,   311,     0,     0,
       0,     0,     0,   249,   141,     0,   156,   158,     0,     0,
     277,   265,   285,   298,     0,     0,   467,   193,   338,     0,
       0,     0,    33,    54,     0,    56,    28,     0,    53,    58,
     372,     0,   385,     0,   374,   378,     0,     0,   373,   478,
     284,   282,     0,     0,     0,   248,   250,   253,   209,   211,
     247,   261,   261,   260,   171,   171,   174,   446,   471,   144,
      76,     0,   398,   392,   323,   181,   181,   184,   186,   124,
     146,   147,     0,   114,   163,     0,   432,   433,     0,   359,
     315,   206,     0,     0,   461,   428,    66,     0,   265,   285,
       0,   157,   142,   249,   271,   273,     0,     0,   285,     0,
      86,   286,   288,   302,   368,   403,   406,   409,   411,     0,
      60,    59,    51,     0,    55,   375,   370,   377,     0,     0,
     379,   194,   383,   371,   210,     0,     0,   238,   257,   259,
     130,     0,   166,   170,     0,   167,     0,   247,     0,   137,
     132,   176,   132,   180,     0,   177,     0,   110,     0,     0,
      88,     0,   365,   361,   309,     0,   310,     0,     0,   285,
      95,   155,     0,   277,     0,   278,   222,   230,   275,   323,
     323,    87,     0,     0,   289,   292,   447,   287,     0,     0,
      52,    57,   376,     0,   194,   381,   382,     0,   251,     0,
     131,     0,     0,     0,   128,   148,   172,   169,   173,     0,
     133,     0,   159,   182,   179,   183,     0,   123,   123,   115,
      77,   207,   154,     0,   214,    96,   276,   272,     0,     0,
     223,     0,     0,   266,   269,     0,     0,   293,   218,   242,
       0,   290,   291,   408,   410,     0,   380,     0,     0,   143,
     129,   148,     0,     0,   126,   168,   333,   134,     0,   178,
     111,   113,     0,     0,   122,     0,     0,     0,   278,   274,
     279,   263,   323,     0,   264,     0,   294,   384,   252,   254,
     125,     0,   127,   149,     0,     0,   232,   141,   112,   118,
     116,   121,   119,   117,   153,   268,     0,   243,   232,     0,
     265,   285,   120,   270,   150,   285,   135,   136
  };

  const short
  parser::yypgoto_[] =
  {
    -720,  -720,  -720,  -720,  -720,  -720,  -720,    49,  -720,  -720,
    -720,  -720,   649,   214,  -720,  -720,    -1,   683,  -720,  -720,
    -720,  -720,  -720,  -720,  -720,  -720,   216,  -720,   129,  -720,
    -720,  -720,  -720,  -720,  -720,  -720,  -720,  -720,  -720,  -720,
    -720,  -720,   149,    79,  -720,  -720,   -33,  -720,  -720,  -720,
      50,  -219,  -720,  -720,   115,  -720,   811,  -720,  -567,    27,
    -720,    26,   555,    23,  -280,  -720,  -720,   306,    76,   219,
    -720,  -720,    72,   211,  -720,  -720,   636,  -720,  -284,  -412,
     843,  -720,  -720,  -319,   130,  -160,   283,  -157,   310,  -720,
     -88,  -720,   -76,  -720,   -49,  -720,  -687,  -720,  -720,  -720,
    -700,   -67,  -178,    18,  -720,   495,  -526,   326,  -719,  -720,
    -720,   239,   240,  -463,  -635,   121,  -720,    31,  -550,  -720,
     131,  -720,    77,  -720,  -720,   380,  -637,  -720,   195,   125,
     845,  -181,  -720,  -720,   580,  -720,   473,  -720,   230,   -12,
    -241,  -202,   772,   -40,  -720,  -720,  -720,  -107,  -720,  -720,
    -720,  -720,   186,  -720,  -720,  -455,  -720,   185,  -720,  -720,
     189,  -720,  -720,   638,  -720,   -74,   672,   363,  -274,  -720,
     295,  -720,  -720,  -720,  -720,   477,    86,  -720,  -103,  -694,
     -87,  -720,   481,   -66,  -720,  -720,  -720,    -2,  -720,  -192,
    -720,   315,  -720,   641,  -720,  -720,  -720,    32,  -720,  -356,
    -264,   -10,  -255,  -126,    39,  -720,  -720,   116,   -41,   -91,
     -29,  -720,   -79,   -99,   -16,  -231,  -720,  -287,   -24,  -101
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   174,     6,    10,    19,    30,
      71,    72,    73,   171,   321,   322,   606,    86,    11,    20,
      21,    32,    84,   327,   488,   489,   607,   608,   609,   289,
     124,   448,    33,    34,   125,   410,   126,   127,   128,   230,
     129,   222,   212,   213,   130,   653,   727,   830,   730,   789,
     833,   834,   712,   771,   781,   721,   722,   205,   591,   520,
     542,   824,   191,   584,   293,   545,   546,   547,   713,   714,
     636,   512,   723,   724,   647,   534,   393,   225,   226,   467,
      27,    36,   416,   372,   457,   132,   662,   352,   373,   459,
     342,   744,   343,   809,   194,   195,   745,   196,   368,   363,
     746,   197,   374,   810,   522,   353,   498,   625,   626,   359,
     508,   509,   510,   549,   678,   802,   803,   804,   592,   674,
     675,   676,   748,   334,   335,   336,   680,   681,   682,   754,
     394,   715,   299,   300,   301,   134,   283,   284,   256,   179,
     136,   805,   137,   138,   139,   140,   272,   273,   274,   259,
     260,   566,   462,   463,   492,   613,   614,   615,   700,   701,
     702,   616,   381,   246,   247,   216,   382,   383,   384,   473,
     474,   475,   687,   141,   142,   241,   242,   143,   144,   449,
     261,   558,   198,   199,    75,   364,   755,   200,    77,   354,
     355,   450,   451,   303,   262,   304,   263,   201,   366,   202,
     145,   146,   477,    79,    80,   305,   264,   265,   307,   165,
     203,   166,   148,   149,   267,   268,   150,    24,     9,   279
  };

  const short
  parser::yytable_[] =
  {
      81,   192,   365,   243,   258,    81,   516,   244,   399,   413,
     493,   454,   281,   193,   428,   387,   153,   668,   154,    78,
     266,   277,   135,   464,   164,   231,   333,    76,    74,   235,
     340,   460,   740,   739,   669,   233,   617,   628,   484,    22,
     365,   751,   395,   395,   392,   800,    22,   285,   302,   593,
     543,   801,   232,   486,    13,   471,   639,   667,   453,    81,
     175,     1,   469,   466,   236,    81,    22,   245,   248,    22,
     250,   276,   291,   147,   411,    81,    81,   650,    81,    81,
      81,    81,    81,    81,    22,  -267,   277,   356,   357,   206,
      22,   734,   302,   587,   526,   238,   278,   309,   531,   257,
     257,   466,   795,   294,   855,   767,   529,   444,    81,   432,
     537,   838,    17,   151,   840,   433,   330,   331,   239,   530,
      12,   297,   240,   152,   113,   292,   672,   419,   370,   369,
     768,   412,   855,   735,   115,   375,   164,   282,   800,    81,
     800,    81,   295,   251,   801,     2,    23,   742,    81,   567,
     588,   400,   401,    23,   487,   232,   695,    81,    78,   698,
      78,   278,   232,   403,   530,   257,    76,    74,    76,   323,
     302,   544,   465,    23,   420,   571,    23,   270,   232,   232,
     469,   163,   597,   271,   536,    81,    81,   777,   736,    25,
     254,    23,  -267,  -477,    81,    81,   164,    23,   524,   525,
     441,    18,   784,   386,   618,   192,   758,   245,   861,   309,
      29,   396,   396,   404,   521,    26,   -92,   193,    81,   420,
      81,   324,   325,   422,    81,   337,   759,    31,   275,   423,
     735,   414,  -477,    81,   876,   875,   495,   168,   877,  -478,
      81,   470,   698,   535,   699,   442,   866,   693,   452,   551,
     427,   306,   405,    35,    81,   -92,   536,    81,    81,   661,
     661,   169,   147,   147,   424,   208,    81,    81,    81,    81,
     415,   694,   479,   443,    81,   295,   445,   446,  -478,   776,
      82,    81,    81,    81,   513,   415,   430,   402,   655,   765,
     870,   494,   777,   275,   209,   306,   728,   210,   431,    67,
     518,   476,   485,    69,   817,   527,   818,   871,   429,   302,
     783,   232,   243,    37,   517,   752,   244,   569,   178,   570,
     172,   684,   173,   784,   217,   365,   572,    67,   573,   344,
     703,    69,  -267,   266,   337,   266,   361,   333,    83,   550,
     302,  -214,   266,   346,   266,   386,   688,   630,   581,   429,
     478,   453,   816,   163,    38,   648,   752,     7,   860,   853,
     515,     8,   310,    85,   156,   311,   585,   157,    67,    81,
     362,   861,    69,   306,   158,   348,   349,   434,   586,   350,
     351,   167,    67,   435,   732,   251,    69,   296,    81,   214,
     297,   215,    81,    81,    81,    81,   159,    81,   170,   156,
     161,    81,   157,   204,    81,   207,    81,   438,   208,   158,
     176,   506,   257,   439,   257,   280,   791,   439,    81,   271,
     344,   257,   234,   257,   464,   282,   298,   345,   288,   778,
     831,   159,   254,   312,   346,   161,   255,   209,   313,   785,
     210,   211,   718,   314,   380,   380,   326,   156,   440,   223,
     157,   224,   439,    81,   656,   356,   357,   158,    81,   251,
     271,    81,    81,   315,   725,   725,   348,   349,    81,   429,
     350,   351,    78,   156,   858,   316,   157,    78,   507,   159,
      76,   602,   439,   158,   717,    76,   580,   657,   490,   317,
     491,   271,   774,   686,   634,   318,   635,   341,   341,    81,
     447,    81,   828,    81,   814,   159,   254,    81,   155,   266,
      14,    15,   306,   728,   453,   328,   645,   344,   646,   844,
     540,   541,   156,    81,   396,   157,   461,   380,   589,   590,
     627,   346,   158,   822,   823,   749,   337,   750,   271,   407,
     555,   452,   633,   306,   556,   358,   557,   873,   862,   863,
     408,   409,   376,   820,   159,   160,    67,   360,   161,   162,
      69,   371,   827,   348,   349,    81,    81,   350,   351,   377,
      81,    81,    81,   627,    81,   147,   218,   219,   220,   221,
     690,   787,    81,   788,   388,   822,   851,   386,   257,   476,
     389,   337,   390,    78,   741,   286,   287,   391,   344,   398,
     252,    76,   365,   725,   458,    81,    81,   453,   406,   747,
     425,   426,   346,   436,   292,  -498,    81,    81,   455,   437,
     500,   232,   396,   396,   480,   481,   483,   482,   538,    81,
      81,   496,   499,   396,   396,   775,   497,   501,   478,   362,
     429,   502,   872,   503,   348,   349,   505,   562,   350,   351,
     793,   813,   565,   380,   568,   705,   706,   504,   511,   519,
    -386,   792,   232,   528,    81,   532,   533,   266,    81,    81,
      81,   514,   539,   147,   147,   552,   808,   756,   747,   553,
     554,   341,   561,    78,   147,   147,   563,   574,   232,   794,
     564,    76,   575,   576,   577,   819,   400,   821,   578,   627,
     581,   579,   243,   232,   466,   400,   244,   582,   594,   595,
     600,   400,   400,   596,   548,   598,   341,   599,   603,   631,
     612,   612,   232,   232,   232,   601,  -482,   619,   637,    81,
      81,   747,   232,   638,   747,   640,   652,   339,   232,   232,
     585,   633,   641,   659,   660,   658,   257,   670,    81,   671,
      81,   756,   586,   356,   664,    81,   665,   673,   642,   677,
     380,   864,   644,   679,   692,   396,   696,   697,   707,   808,
     729,   747,   396,   747,   627,   731,   743,   760,   769,   770,
     251,   329,   779,   780,   798,   400,    81,   837,   232,   786,
     211,   815,   115,   344,   156,   841,   232,   157,   429,   842,
     361,   845,   846,   869,   158,   620,  -253,   346,   290,   848,
     548,   629,   232,   849,   341,   859,   147,   319,   251,   689,
     691,   298,   761,   147,   683,   380,   159,   254,   753,   548,
     161,   255,   156,   812,   362,   157,   874,   782,   836,   348,
     349,   612,   158,   350,   351,   131,   850,   852,   456,   649,
     548,   857,   654,   825,   716,   856,   829,   726,    28,   298,
     397,   790,   663,   867,   159,   254,   523,   651,   161,   255,
     708,   806,   709,   865,   797,   839,   757,   621,   811,   133,
     468,   249,   762,   627,   763,   418,   868,   385,   380,   733,
     766,   843,   560,   643,   685,   559,     0,   666,   421,   548,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   612,   380,   764,
       0,     0,   704,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   341,     0,     0,     0,     0,     0,     0,     0,     0,
      87,    39,    88,    89,    90,    91,    92,    93,     0,    41,
      94,     0,     0,    95,    96,    97,    98,    99,     0,   100,
       0,    43,     0,   101,     0,    44,   102,    45,    46,    47,
      48,    49,    50,   103,    51,    52,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,   104,    59,   826,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,   105,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
     107,   458,     0,     0,     0,   847,     0,   108,     0,     0,
     109,     0,   110,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,     0,     0,     0,
     112,     0,   113,     0,     0,     0,     0,   548,     0,   548,
     114,    66,   115,     0,     0,    68,   116,     0,     0,     0,
       0,   117,   118,   119,   120,     0,     0,   121,     0,     0,
       0,     0,   122,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    87,    39,    88,     0,   719,     0,     0,    93,     0,
      41,    94,   548,   649,    95,    96,    97,     0,    99,     0,
     100,     0,    43,     0,   720,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   103,    51,    52,    53,    54,     0,
      55,    56,    57,     0,   341,    58,     0,     0,   104,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,   105,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,   107,     0,     0,     0,     0,     0,     0,   108,     0,
       0,   109,     0,   110,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   111,     0,     0,
       0,   112,     0,   113,     0,     0,     0,     0,     0,     0,
       0,   114,    66,   115,     0,     0,    68,   116,     0,     0,
       0,     0,   117,   118,   119,   120,     0,     0,   121,     0,
       0,     0,     0,   122,   123,    87,    39,    88,     0,   710,
       0,     0,    93,     0,    41,    94,     0,     0,    95,    96,
      97,     0,    99,     0,     0,     0,    43,     0,   711,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   103,    51,
      52,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,   104,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,   105,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
       0,     0,   108,     0,     0,   109,     0,   110,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   111,     0,     0,     0,   112,     0,   113,     0,     0,
       0,     0,     0,     0,     0,   114,    66,   115,     0,     0,
      68,   116,     0,     0,     0,     0,   117,   118,   119,   120,
       0,     0,   121,    87,    39,    88,     0,   122,   123,     0,
      93,     0,    41,    94,     0,     0,    95,    96,    97,     0,
      99,     0,     0,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   103,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
     104,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,   105,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,   107,     0,     0,     0,     0,     0,     0,
     108,     0,     0,   109,     0,   110,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   111,
       0,     0,     0,   112,     0,   113,     0,     0,     0,     0,
       0,     0,     0,   114,    66,   115,     0,     0,    68,   116,
       0,     0,     0,     0,   117,   118,   119,   120,     0,    39,
     121,     0,     0,    40,     0,   122,   123,    41,     0,     0,
       0,     0,     0,     0,     0,     0,    42,     0,     0,    43,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,     0,    55,    56,    57,
      39,     0,    58,     0,     0,     0,    59,     0,    41,    60,
      61,    62,    63,    64,     0,     0,     0,   604,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,     0,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    39,     0,     0,    65,     0,
       0,     0,     0,    41,     0,     0,     0,     0,     0,    66,
      67,     0,     0,    68,    69,    43,     0,     0,     0,     0,
       0,    45,    46,    47,   180,   181,   182,     0,     0,    70,
      53,    54,     0,    55,    56,    57,     0,     0,    58,    65,
       0,     0,    59,     0,     0,    60,    61,    62,    63,    64,
      66,    67,     0,     0,    68,    69,    22,     0,    87,    39,
      88,     0,     0,     0,     0,    93,     0,    41,    94,     0,
     605,     0,     0,     0,     0,    99,     0,     0,     0,    43,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   103,    51,    52,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,   104,    59,     0,     0,    60,
      61,    62,    63,    64,     0,   190,    67,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,     0,   108,     0,     0,   109,     0,
     110,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    23,   111,     0,     0,     0,   177,     0,
     113,     0,     0,     0,   611,     0,     0,     0,     0,    66,
     115,     0,     0,    68,   116,     0,     0,     0,     0,   117,
     118,   119,   120,     0,     0,   121,    87,    39,    88,     0,
       0,     0,     0,    93,     0,    41,    94,     0,     0,     0,
       0,     0,     0,    99,     0,     0,     0,    43,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   103,
      51,    52,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,   104,    59,     0,     0,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   251,     0,     0,   107,     0,     0,     0,
       0,     0,     0,   108,     0,     0,   109,   156,   110,     0,
     157,     0,     0,     0,     0,     0,   269,   158,     0,     0,
       0,     0,   111,     0,     0,     0,   177,   270,   113,     0,
       0,     0,     0,   271,   253,     0,     0,    66,   115,   159,
     254,    68,   116,   161,   255,     0,     0,   117,   118,   119,
     120,     0,     0,   121,    87,    39,    88,     0,     0,     0,
       0,    93,     0,    41,    94,     0,     0,     0,     0,     0,
       0,    99,     0,     0,     0,    43,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   103,    51,    52,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,   104,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   251,     0,     0,   107,     0,     0,     0,     0,     0,
       0,   108,     0,     0,   109,     0,   110,     0,   157,     0,
       0,     0,     0,     0,     0,   158,     0,     0,     0,     0,
     111,   252,     0,     0,   177,     0,   113,     0,     0,     0,
       0,     0,   253,     0,     0,    66,   115,   159,   254,    68,
     116,   161,   255,     0,     0,   117,   118,   119,   120,     0,
       0,   121,    87,    39,    88,     0,     0,     0,     0,    93,
       0,    41,    94,     0,     0,     0,     0,     0,     0,    99,
       0,     0,     0,    43,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   103,    51,    52,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,   104,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   251,
       0,     0,   107,     0,     0,     0,     0,     0,     0,   108,
       0,     0,   109,     0,   110,     0,   157,     0,     0,     0,
       0,     0,     0,   158,     0,     0,     0,     0,   111,     0,
       0,     0,   177,     0,   113,     0,     0,     0,     0,     0,
     253,     0,     0,    66,   115,   159,   254,    68,   116,   161,
     255,     0,     0,   117,   118,   119,   120,     0,     0,   121,
      87,    39,    88,     0,     0,     0,     0,    93,     0,    41,
      94,     0,     0,     0,     0,     0,     0,    99,     0,     0,
       0,    43,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   103,    51,    52,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,   104,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,     0,   108,     0,     0,
     109,     0,   110,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   610,     0,     0,   111,     0,     0,     0,
     177,     0,   113,     0,     0,     0,   611,     0,     0,     0,
       0,    66,   115,     0,     0,    68,   116,     0,     0,     0,
       0,   117,   118,   119,   120,     0,     0,   121,    87,    39,
      88,     0,     0,     0,     0,    93,     0,    41,    94,     0,
       0,     0,     0,     0,     0,   378,     0,     0,     0,    43,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   103,    51,    52,    53,    54,     0,    55,    56,    57,
       0,   379,    58,     0,     0,   104,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,     0,   108,     0,     0,   109,     0,
     110,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   111,     0,     0,     0,   177,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    66,
     115,     0,     0,    68,   116,     0,     0,     0,     0,   117,
     118,   119,   120,     0,     0,   121,    87,    39,    88,     0,
       0,     0,     0,    93,     0,    41,    94,     0,     0,     0,
       0,     0,     0,    99,     0,     0,     0,    43,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   103,
      51,    52,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,   104,    59,     0,     0,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,     0,   108,     0,     0,   109,     0,   110,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   111,     0,     0,     0,   177,     0,   113,     0,
       0,     0,   611,     0,     0,     0,     0,    66,   115,     0,
       0,    68,   116,     0,     0,     0,     0,   117,   118,   119,
     120,     0,     0,   121,    87,    39,    88,     0,     0,     0,
       0,    93,     0,    41,    94,     0,     0,     0,     0,     0,
       0,    99,     0,     0,     0,    43,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   103,    51,    52,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,   104,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,     0,
       0,   108,     0,     0,   109,     0,   110,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     111,     0,     0,     0,   177,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    66,   115,     0,     0,    68,
     116,     0,     0,     0,     0,   117,   118,   119,   120,     0,
       0,   121,    87,    39,    88,     0,     0,     0,     0,    93,
       0,    41,    94,     0,     0,     0,     0,     0,     0,   378,
       0,     0,     0,    43,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   103,    51,    52,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,   104,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,     0,   108,
       0,     0,   109,     0,   110,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   111,     0,
       0,     0,   177,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    66,   115,     0,     0,    68,   116,     0,
       0,     0,     0,   117,   118,   119,   120,     0,     0,   121,
      87,    39,    88,     0,     0,     0,     0,    93,     0,    41,
      94,     0,     0,     0,     0,     0,     0,    99,     0,     0,
       0,    43,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   103,    51,    52,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,   104,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,     0,   108,     0,     0,
       0,     0,   110,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,     0,     0,     0,
     177,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,    66,   115,     0,     0,    68,   116,     0,     0,     0,
       0,   117,   118,   119,   120,     0,     0,   121,    87,    39,
      88,     0,     0,     0,     0,    93,     0,    41,    94,     0,
       0,     0,     0,     0,     0,    99,     0,     0,     0,    43,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   103,    51,    52,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,   308,   108,     0,     0,     0,     0,
     110,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   111,     0,     0,     0,   177,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    66,
     115,     0,     0,    68,   116,     0,     0,     0,     0,   117,
     118,   119,   120,     0,     0,   121,    87,    39,    88,     0,
       0,     0,     0,    93,     0,    41,    94,     0,     0,     0,
       0,     0,     0,    99,     0,     0,     0,    43,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   103,
      51,    52,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,     0,     0,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,     0,     0,     0,
     417,     0,     0,   108,     0,     0,     0,     0,   110,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   111,     0,     0,     0,   177,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    66,   115,     0,
       0,    68,   116,     0,     0,     0,     0,   117,   118,   119,
     120,     0,     0,   121,    87,    39,    88,     0,     0,     0,
       0,    93,     0,    41,    94,     0,     0,     0,     0,     0,
       0,    99,     0,     0,     0,    43,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   103,    51,    52,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,     0,
       0,   108,     0,     0,     0,     0,   110,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     111,     0,     0,     0,   177,     0,   113,     0,     0,    39,
       0,     0,     0,     0,     0,    66,   115,    41,     0,    68,
     116,     0,     0,     0,     0,   117,   118,   119,   120,    43,
       0,   121,     0,   338,     0,    45,    46,    47,   180,   181,
     182,     0,     0,     0,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   344,     0,     0,     0,     0,
       0,     0,   345,     0,     0,   183,     0,     0,     0,   346,
     184,     0,   185,     0,     0,     0,     0,     0,     0,     0,
     186,     0,     0,    39,   187,     0,     0,     0,   188,   347,
     189,    41,     0,     0,     0,   271,     0,     0,     0,   190,
      67,   348,   349,    43,    69,   350,   351,     0,     0,    45,
      46,    47,   180,   181,   182,     0,     0,     0,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   251,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   183,
       0,  -215,     0,     0,   184,     0,   185,     0,     0,     0,
       0,     0,     0,     0,   186,     0,     0,    39,   187,     0,
       0,     0,   188,     0,   189,    41,     0,     0,     0,     0,
     799,     0,   227,   190,    67,     0,   254,    43,    69,     0,
       0,     0,     0,    45,    46,    47,   180,   181,   182,     0,
     228,   229,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,     0,     0,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   183,     0,    39,     0,     0,   184,     0,
     185,     0,     0,    41,     0,     0,     0,     0,   186,     0,
       0,     0,   187,     0,     0,    43,   188,     0,   189,     0,
       0,    45,    46,    47,   180,   181,   182,   190,    67,     0,
      53,    54,    69,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   251,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   183,     0,     0,     0,     0,   184,     0,   185,     0,
       0,     0,     0,     0,     0,     0,   186,    39,     0,     0,
     187,     0,     0,     0,   188,    41,   189,     0,     0,     0,
       0,     0,   799,     0,     0,   190,    67,    43,   254,     0,
      69,   338,     0,    45,    46,    47,   180,   181,   182,     0,
       0,     0,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,     0,     0,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   183,     0,     0,     0,     0,   184,     0,
     185,     0,     0,     0,     0,     0,     0,     0,   186,    39,
       0,     0,   187,   339,     0,     0,   188,    41,   189,     0,
       0,     0,     0,     0,   772,     0,     0,   190,    67,    43,
       0,     0,    69,     0,     0,    45,    46,    47,   180,   181,
     182,     0,   773,     0,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   183,     0,    39,     0,     0,
     184,     0,   185,     0,     0,    41,     0,     0,     0,     0,
     186,     0,     0,     0,   187,     0,     0,    43,   188,     0,
     189,     0,     0,    45,    46,    47,   180,   181,   182,   190,
      67,     0,    53,    54,    69,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,     0,     0,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   367,   183,     0,    39,     0,     0,   184,     0,
     185,     0,     0,    41,     0,     0,     0,     0,   186,     0,
       0,     0,   187,     0,     0,    43,   188,     0,   189,   338,
       0,    45,    46,    47,   180,   181,   182,   190,    67,     0,
      53,    54,    69,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   183,     0,    39,     0,     0,   184,     0,   185,     0,
       0,    41,     0,     0,     0,     0,   186,     0,     0,     0,
     187,     0,     0,    43,   188,     0,   189,   583,     0,    45,
      46,    47,   180,   181,   182,   190,    67,     0,    53,    54,
      69,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   183,
       0,    39,     0,     0,   184,     0,   185,     0,     0,    41,
       0,     0,     0,     0,   186,     0,     0,     0,   187,     0,
       0,    43,   188,     0,   189,     0,     0,    45,    46,    47,
     180,   181,   182,   190,    67,     0,    53,    54,    69,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   183,     0,    39,
       0,     0,   184,     0,   185,     0,     0,    41,     0,     0,
       0,     0,   186,     0,     0,     0,   187,     0,     0,    43,
     188,   807,   189,     0,     0,    45,    46,    47,   180,   181,
     182,   190,    67,     0,    53,    54,    69,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   832,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   183,     0,    39,     0,     0,
     184,     0,   185,     0,     0,    41,     0,     0,     0,     0,
     186,     0,     0,     0,   187,     0,     0,    43,   188,     0,
     189,     0,     0,    45,    46,    47,   180,   181,   182,   190,
      67,     0,    53,    54,    69,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,     0,     0,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   835,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   183,     0,    39,     0,     0,   184,     0,
     185,     0,     0,    41,     0,     0,     0,     0,   186,     0,
       0,     0,   187,     0,     0,    43,   188,     0,   189,   338,
       0,    45,    46,    47,   180,   181,   182,   190,    67,     0,
      53,    54,    69,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   183,     0,    39,     0,     0,   184,     0,   185,     0,
       0,    41,     0,     0,     0,     0,   186,     0,     0,     0,
     187,     0,     0,    43,   854,     0,   189,     0,     0,    45,
      46,    47,   180,   181,   182,   190,    67,     0,    53,    54,
      69,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   183,
       0,     0,     0,     0,   184,     0,   185,     0,     0,     0,
       0,     0,     0,     0,   186,    39,     0,     0,   187,    40,
       0,     0,   188,    41,   189,     0,     0,     0,     0,     0,
       0,     0,    42,   190,    67,    43,     0,     0,    69,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    39,    55,    56,    57,     0,     0,    58,     0,
      41,     0,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    39,     0,     0,
       0,     0,     0,     0,    65,    41,     0,   320,     0,     0,
       0,     0,     0,     0,   604,    66,    67,    43,     0,    68,
      69,    44,     0,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,    39,    55,    56,    57,     0,     0,
      58,    65,    41,     0,    59,     0,     0,    60,    61,    62,
      63,    64,    66,    67,    43,     0,    68,    69,    44,     0,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    39,
       0,     0,     0,     0,     0,     0,    65,    41,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    66,    67,    43,
       0,    68,    69,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    39,    55,    56,    57,
       0,     0,    58,    65,    41,     0,    59,     0,     0,    60,
      61,    62,    63,    64,    66,    67,    43,     0,    68,    69,
      44,     0,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    39,     0,     0,     0,     0,     0,     0,     0,    41,
       0,   472,     0,     0,     0,     0,     0,     0,     0,    66,
     115,    43,     0,    68,   116,    44,     0,    45,    46,    47,
      48,    49,    50,     0,    51,    52,    53,    54,    39,    55,
      56,    57,     0,     0,    58,   237,    41,     0,    59,     0,
       0,    60,    61,    62,    63,    64,    66,     0,    43,     0,
      68,     0,    44,     0,    45,    46,    47,    48,    49,    50,
       0,    51,    52,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
      39,     0,     0,     0,     0,     0,     0,     0,    41,     0,
     237,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      43,    66,     0,     0,    44,    68,    45,    46,    47,    48,
      49,    50,     0,    51,    52,    53,    54,    39,    55,    56,
      57,     0,     0,    58,     0,    41,     0,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,    43,    66,   115,
       0,     0,     0,    45,    46,    47,   180,   181,   182,     0,
       0,     0,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,     0,     0,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   332,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      66,     0,     0,     0,     0,   737,     0,     0,     0,     0,
       0,     0,    39,     0,     0,     0,     0,   738,   623,     0,
      41,     0,     0,     0,     0,     0,   624,     0,     0,     0,
       0,     0,    43,     0,     0,     0,    44,   190,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    39,
      55,    56,    57,     0,     0,    58,     0,    41,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,    43,
       0,     0,     0,     0,     0,    45,    46,    47,   180,   181,
     182,     0,     0,     0,    53,    54,    39,    55,    56,    57,
       0,     0,    58,     0,    41,     0,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,    43,     0,     0,     0,
       0,     0,    45,    46,    47,   180,   181,   182,     0,     0,
       0,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,    66,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,   622,
     623,     0,     0,     0,     0,     0,     0,     0,   624,     0,
       0,     0,    39,     0,     0,     0,     0,     0,     0,   190,
      41,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    43,     0,     0,     0,   796,   623,    45,    46,
      47,   180,   181,   182,     0,   624,     0,    53,    54,     0,
      55,    56,    57,    39,     0,    58,   190,     0,     0,    59,
       0,    41,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,    43,     0,     0,     0,     0,     0,    45,
      46,    47,   180,   181,   182,     0,     0,     0,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,    39,     0,   623,     0,     0,     0,     0,     0,    41,
       0,   624,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    43,   190,     0,     0,     0,   632,    45,    46,    47,
     180,   181,   182,     0,     0,     0,    53,    54,    39,    55,
      56,    57,     0,     0,    58,     0,    41,     0,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,    43,     0,
       0,     0,     0,   190,    45,    46,    47,   180,   181,   182,
       0,     0,     0,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     624,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   190
  };

  const short
  parser::yycheck_[] =
  {
      29,    89,   194,   106,   111,    34,   362,   106,   227,   240,
     329,   291,   113,    89,   269,   217,    40,   584,    42,    29,
     111,   112,    34,   297,    65,   101,   186,    29,    29,   103,
     187,   295,   669,   668,   584,   101,   491,   500,   325,     1,
     232,   678,   223,   224,   222,   745,     1,    19,   135,   461,
       3,   745,   101,    12,     5,   310,   519,   583,   289,    88,
      84,    21,   303,    27,   104,    94,     1,   107,   108,     1,
     110,   112,    19,    34,    78,   104,   105,   540,   107,   108,
     109,   110,   111,   112,     1,     1,   177,   188,   189,    91,
       1,    78,   179,    78,   378,   105,   112,   137,   385,   111,
     112,    27,   739,    81,   823,    81,   106,   285,   137,   114,
     397,   798,   106,   110,   801,   120,   183,   184,   109,   119,
       0,    85,   113,   120,   115,    72,   589,   253,   204,   196,
     106,   135,   851,   120,   125,   211,   177,   109,   838,   168,
     840,   170,   120,    80,   838,   105,   108,   673,   177,   423,
     135,   227,   228,   108,   113,   204,   611,   186,   168,    85,
     170,   177,   211,   229,   119,   177,   168,   168,   170,   170,
     257,   124,   298,   108,   253,   430,   108,   114,   227,   228,
     421,    65,   466,   120,   119,   214,   215,   119,    78,   109,
     127,   108,   108,    81,   223,   224,   237,   108,   376,   377,
      91,   119,   119,   119,   491,   293,    82,   247,   119,   249,
     113,   223,   224,    81,   371,   135,    81,   293,   247,   298,
     249,   172,   173,    79,   253,   186,   102,    14,   112,    85,
     120,    81,   120,   262,   871,   870,    81,   120,   875,    81,
     269,   308,    85,   106,    87,   136,    81,   120,   289,   406,
     262,   135,   120,   135,   283,   120,   119,   286,   287,   578,
     579,   144,   223,   224,   120,    22,   295,   296,   297,   298,
     120,   144,   312,   283,   303,   120,   286,   287,   120,   106,
      27,   310,   311,   312,   360,   120,   102,   113,   552,   701,
     857,   332,   119,   177,    51,   179,   652,    54,   114,   125,
     367,   311,   326,   129,   767,   379,   769,   857,   269,   396,
     106,   360,   415,    78,   363,   113,   415,   424,    88,   426,
     105,   595,   107,   119,    94,   517,   433,   125,   435,    80,
     617,   129,   106,   424,   295,   426,    87,   497,    24,   405,
     427,    92,   433,    94,   435,   119,   601,   504,   447,   310,
     311,   582,   764,   237,    78,   536,   113,   125,   106,   822,
     362,   129,   102,   119,    94,   105,   454,    97,   125,   398,
     121,   119,   129,   257,   104,   126,   127,   114,   454,   130,
     131,   114,   125,   120,   658,    80,   129,    82,   417,   105,
      85,   107,   421,   422,   423,   424,   126,   426,   113,    94,
     130,   430,    97,    37,   433,    19,   435,   114,    22,   104,
     119,   114,   424,   120,   426,   116,   735,   120,   447,   120,
      80,   433,    14,   435,   698,   109,   121,    87,   136,   716,
     786,   126,   127,    88,    94,   130,   131,    51,   135,   726,
      54,    55,   644,   114,   214,   215,     4,    94,   116,   105,
      97,   107,   120,   482,   114,   556,   557,   104,   487,    80,
     120,   490,   491,   114,   645,   646,   126,   127,   497,   430,
     130,   131,   482,    94,   830,   114,    97,   487,   116,   126,
     482,   482,   120,   104,   641,   487,   447,   116,   105,   114,
     107,   120,   711,   600,   105,   114,   107,   187,   188,   528,
     121,   530,   782,   532,   759,   126,   127,   536,    80,   600,
      66,    67,   396,   869,   745,    23,   105,    80,   107,   806,
      81,    82,    94,   552,   536,    97,   296,   297,    81,    82,
     498,    94,   104,    81,    82,   105,   497,   107,   120,    41,
     109,   582,   510,   427,   113,    85,   115,   866,   835,   836,
      52,    53,    19,   772,   126,   127,   125,    92,   130,   131,
     129,   113,   781,   126,   127,   594,   595,   130,   131,    19,
     599,   600,   601,   541,   603,   536,    73,    74,    75,    76,
     604,   105,   611,   107,    78,    81,    82,   119,   600,   599,
      78,   552,    78,   603,   670,   122,   123,    78,    80,    15,
     110,   603,   794,   784,   294,   634,   635,   838,    82,   676,
     110,   120,    94,   114,    72,   114,   645,   646,   135,   114,
      81,   670,   634,   635,   114,   114,   106,   120,   398,   658,
     659,   106,   110,   645,   646,   711,   120,    92,   599,   121,
     601,   114,   861,   114,   126,   127,   114,   417,   130,   131,
     738,   758,   422,   423,   424,   623,   624,   120,    27,    81,
      86,   737,   711,    86,   693,    25,    27,   758,   697,   698,
     699,   361,    82,   634,   635,   135,   752,   679,   745,    78,
     114,   371,    78,   693,   645,   646,   121,   136,   737,   738,
     121,   693,   110,    78,    78,   771,   772,   773,    81,   667,
     799,    81,   805,   752,    27,   781,   805,   120,    82,   120,
      82,   787,   788,   121,   404,   106,   406,   120,   113,   120,
     490,   491,   771,   772,   773,   102,   102,   114,   121,   758,
     759,   798,   781,   121,   801,   114,    85,   110,   787,   788,
     828,   709,   120,    79,   110,   120,   758,    92,   777,    78,
     779,   753,   828,   854,   121,   784,   121,    28,   528,    27,
     530,   837,   532,     9,   114,   777,   106,   119,   114,   845,
      27,   838,   784,   840,   742,    81,    85,   114,    81,    37,
      80,    81,    11,    19,    92,   861,   815,    92,   837,    87,
      55,    87,   125,    80,    94,   106,   845,    97,   759,   119,
      87,   120,   114,    85,   104,   495,    85,    94,   125,   106,
     500,   501,   861,   114,   504,   106,   777,   168,    80,   603,
     606,   121,   693,   784,   594,   595,   126,   127,   679,   519,
     130,   131,    94,   754,   121,    97,   869,   722,   788,   126,
     127,   611,   104,   130,   131,    34,   819,   821,   293,   539,
     540,   828,   546,   777,   635,   823,   784,   646,    15,   121,
     224,   731,   579,   845,   126,   127,   371,   541,   130,   131,
     631,   750,   632,   842,   743,   798,   681,   497,   753,    34,
     300,   109,   697,   851,   698,   247,   854,   215,   658,   659,
     701,   805,   415,   530,   599,   414,    -1,   582,   257,   589,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   697,   698,   699,
      -1,    -1,   622,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   641,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    -1,    12,
      13,    -1,    -1,    16,    17,    18,    19,    20,    -1,    22,
      -1,    24,    -1,    26,    -1,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,   779,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    77,    -1,    -1,    -1,    -1,    -1,
      83,   731,    -1,    -1,    -1,   815,    -1,    90,    -1,    -1,
      93,    -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,    -1,    -1,
     113,    -1,   115,    -1,    -1,    -1,    -1,   767,    -1,   769,
     123,   124,   125,    -1,    -1,   128,   129,    -1,    -1,    -1,
      -1,   134,   135,   136,   137,    -1,    -1,   140,    -1,    -1,
      -1,    -1,   145,   146,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    10,    -1,
      12,    13,   822,   823,    16,    17,    18,    -1,    20,    -1,
      22,    -1,    24,    -1,    26,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,   854,    47,    -1,    -1,    50,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    77,    -1,    -1,    -1,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,    -1,
      -1,   113,    -1,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   123,   124,   125,    -1,    -1,   128,   129,    -1,    -1,
      -1,    -1,   134,   135,   136,   137,    -1,    -1,   140,    -1,
      -1,    -1,    -1,   145,   146,     3,     4,     5,    -1,     7,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    16,    17,
      18,    -1,    20,    -1,    -1,    -1,    24,    -1,    26,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,
      -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    -1,    93,    -1,    95,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,    -1,    -1,    -1,   113,    -1,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   123,   124,   125,    -1,    -1,
     128,   129,    -1,    -1,    -1,    -1,   134,   135,   136,   137,
      -1,    -1,   140,     3,     4,     5,    -1,   145,   146,    -1,
      10,    -1,    12,    13,    -1,    -1,    16,    17,    18,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    -1,    -1,    54,    55,    56,    57,    58,    -1,
      -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,    -1,    -1,
      -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    -1,    95,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
      -1,    -1,    -1,   113,    -1,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   123,   124,   125,    -1,    -1,   128,   129,
      -1,    -1,    -1,    -1,   134,   135,   136,   137,    -1,     4,
     140,    -1,    -1,     8,    -1,   145,   146,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    21,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    -1,    42,    43,    44,
       4,    -1,    47,    -1,    -1,    -1,    51,    -1,    12,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    21,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,
      54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,   113,    -1,
      -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,    -1,   124,
     125,    -1,    -1,   128,   129,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,   144,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,   113,
      -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,    58,
     124,   125,    -1,    -1,   128,   129,     1,    -1,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
     144,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,   124,   125,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    -1,
      95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   108,   109,    -1,    -1,    -1,   113,    -1,
     115,    -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,   124,
     125,    -1,    -1,   128,   129,    -1,    -1,    -1,    -1,   134,
     135,   136,   137,    -1,    -1,   140,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    -1,    -1,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    83,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    95,    -1,
      97,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,    -1,
      -1,    -1,   109,    -1,    -1,    -1,   113,   114,   115,    -1,
      -1,    -1,    -1,   120,   121,    -1,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,    -1,    -1,   134,   135,   136,
     137,    -1,    -1,   140,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    -1,    95,    -1,    97,    -1,
      -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,    -1,
     109,   110,    -1,    -1,   113,    -1,   115,    -1,    -1,    -1,
      -1,    -1,   121,    -1,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,    -1,    -1,   134,   135,   136,   137,    -1,
      -1,   140,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    93,    -1,    95,    -1,    97,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,    -1,    -1,    -1,   109,    -1,
      -1,    -1,   113,    -1,   115,    -1,    -1,    -1,    -1,    -1,
     121,    -1,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,    -1,    -1,   134,   135,   136,   137,    -1,    -1,   140,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      83,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      93,    -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,    -1,    -1,   109,    -1,    -1,    -1,
     113,    -1,   115,    -1,    -1,    -1,   119,    -1,    -1,    -1,
      -1,   124,   125,    -1,    -1,   128,   129,    -1,    -1,    -1,
      -1,   134,   135,   136,   137,    -1,    -1,   140,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    46,    47,    -1,    -1,    50,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    -1,
      95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,    -1,    -1,    -1,   113,    -1,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,    -1,    -1,   128,   129,    -1,    -1,    -1,    -1,   134,
     135,   136,   137,    -1,    -1,   140,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    -1,    -1,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    -1,    95,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,    -1,    -1,    -1,   113,    -1,   115,    -1,
      -1,    -1,   119,    -1,    -1,    -1,    -1,   124,   125,    -1,
      -1,   128,   129,    -1,    -1,    -1,    -1,   134,   135,   136,
     137,    -1,    -1,   140,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    -1,    95,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,    -1,    -1,    -1,   113,    -1,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,    -1,    -1,   128,
     129,    -1,    -1,    -1,    -1,   134,   135,   136,   137,    -1,
      -1,   140,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    93,    -1,    95,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,
      -1,    -1,   113,    -1,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,    -1,    -1,   128,   129,    -1,
      -1,    -1,    -1,   134,   135,   136,   137,    -1,    -1,   140,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      83,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      -1,    -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,    -1,    -1,
     113,    -1,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,    -1,    -1,   128,   129,    -1,    -1,    -1,
      -1,   134,   135,   136,   137,    -1,    -1,   140,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,
      -1,    -1,    -1,    -1,    89,    90,    -1,    -1,    -1,    -1,
      95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,    -1,    -1,    -1,   113,    -1,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,    -1,    -1,   128,   129,    -1,    -1,    -1,    -1,   134,
     135,   136,   137,    -1,    -1,   140,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,
      87,    -1,    -1,    90,    -1,    -1,    -1,    -1,    95,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,    -1,    -1,    -1,   113,    -1,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,    -1,
      -1,   128,   129,    -1,    -1,    -1,    -1,   134,   135,   136,
     137,    -1,    -1,   140,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    -1,    -1,    95,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,    -1,    -1,    -1,   113,    -1,   115,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,   124,   125,    12,    -1,   128,
     129,    -1,    -1,    -1,    -1,   134,   135,   136,   137,    24,
      -1,   140,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    87,    -1,    -1,    90,    -1,    -1,    -1,    94,
      95,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     105,    -1,    -1,     4,   109,    -1,    -1,    -1,   113,   114,
     115,    12,    -1,    -1,    -1,   120,    -1,    -1,    -1,   124,
     125,   126,   127,    24,   129,   130,   131,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    92,    -1,    -1,    95,    -1,    97,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   105,    -1,    -1,     4,   109,    -1,
      -1,    -1,   113,    -1,   115,    12,    -1,    -1,    -1,    -1,
     121,    -1,    19,   124,   125,    -1,   127,    24,   129,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,     4,    -1,    -1,    95,    -1,
      97,    -1,    -1,    12,    -1,    -1,    -1,    -1,   105,    -1,
      -1,    -1,   109,    -1,    -1,    24,   113,    -1,   115,    -1,
      -1,    30,    31,    32,    33,    34,    35,   124,   125,    -1,
      39,    40,   129,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    -1,    -1,    95,    -1,    97,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   105,     4,    -1,    -1,
     109,    -1,    -1,    -1,   113,    12,   115,    -1,    -1,    -1,
      -1,    -1,   121,    -1,    -1,   124,   125,    24,   127,    -1,
     129,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    -1,    -1,    95,    -1,
      97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,     4,
      -1,    -1,   109,   110,    -1,    -1,   113,    12,   115,    -1,
      -1,    -1,    -1,    -1,    19,    -1,    -1,   124,   125,    24,
      -1,    -1,   129,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,     4,    -1,    -1,
      95,    -1,    97,    -1,    -1,    12,    -1,    -1,    -1,    -1,
     105,    -1,    -1,    -1,   109,    -1,    -1,    24,   113,    -1,
     115,    -1,    -1,    30,    31,    32,    33,    34,    35,   124,
     125,    -1,    39,    40,   129,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    90,    -1,     4,    -1,    -1,    95,    -1,
      97,    -1,    -1,    12,    -1,    -1,    -1,    -1,   105,    -1,
      -1,    -1,   109,    -1,    -1,    24,   113,    -1,   115,    28,
      -1,    30,    31,    32,    33,    34,    35,   124,   125,    -1,
      39,    40,   129,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,     4,    -1,    -1,    95,    -1,    97,    -1,
      -1,    12,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,
     109,    -1,    -1,    24,   113,    -1,   115,    28,    -1,    30,
      31,    32,    33,    34,    35,   124,   125,    -1,    39,    40,
     129,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,     4,    -1,    -1,    95,    -1,    97,    -1,    -1,    12,
      -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,
      -1,    24,   113,    -1,   115,    -1,    -1,    30,    31,    32,
      33,    34,    35,   124,   125,    -1,    39,    40,   129,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,     4,
      -1,    -1,    95,    -1,    97,    -1,    -1,    12,    -1,    -1,
      -1,    -1,   105,    -1,    -1,    -1,   109,    -1,    -1,    24,
     113,   114,   115,    -1,    -1,    30,    31,    32,    33,    34,
      35,   124,   125,    -1,    39,    40,   129,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,     4,    -1,    -1,
      95,    -1,    97,    -1,    -1,    12,    -1,    -1,    -1,    -1,
     105,    -1,    -1,    -1,   109,    -1,    -1,    24,   113,    -1,
     115,    -1,    -1,    30,    31,    32,    33,    34,    35,   124,
     125,    -1,    39,    40,   129,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,     4,    -1,    -1,    95,    -1,
      97,    -1,    -1,    12,    -1,    -1,    -1,    -1,   105,    -1,
      -1,    -1,   109,    -1,    -1,    24,   113,    -1,   115,    28,
      -1,    30,    31,    32,    33,    34,    35,   124,   125,    -1,
      39,    40,   129,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,     4,    -1,    -1,    95,    -1,    97,    -1,
      -1,    12,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,
     109,    -1,    -1,    24,   113,    -1,   115,    -1,    -1,    30,
      31,    32,    33,    34,    35,   124,   125,    -1,    39,    40,
     129,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    -1,    -1,    95,    -1,    97,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   105,     4,    -1,    -1,   109,     8,
      -1,    -1,   113,    12,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    21,   124,   125,    24,    -1,    -1,   129,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,     4,    42,    43,    44,    -1,    -1,    47,    -1,
      12,    -1,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,   113,    12,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    21,   124,   125,    24,    -1,   128,
     129,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,     4,    42,    43,    44,    -1,    -1,
      47,   113,    12,    -1,    51,    -1,    -1,    54,    55,    56,
      57,    58,   124,   125,    24,    -1,   128,   129,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    -1,    -1,    54,    55,    56,    57,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,   113,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,    24,
      -1,   128,   129,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,     4,    42,    43,    44,
      -1,    -1,    47,   113,    12,    -1,    51,    -1,    -1,    54,
      55,    56,    57,    58,   124,   125,    24,    -1,   128,   129,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,    24,    -1,   128,   129,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,     4,    42,
      43,    44,    -1,    -1,    47,   113,    12,    -1,    51,    -1,
      -1,    54,    55,    56,    57,    58,   124,    -1,    24,    -1,
     128,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,
     113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      24,   124,    -1,    -1,    28,   128,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,     4,    42,    43,
      44,    -1,    -1,    47,    -1,    12,    -1,    51,    -1,    -1,
      54,    55,    56,    57,    58,    -1,    -1,    24,   124,   125,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,    -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,   104,   105,    -1,
      12,    -1,    -1,    -1,    -1,    -1,   113,    -1,    -1,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,   124,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,     4,
      42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,     4,    42,    43,    44,
      -1,    -1,    47,    -1,    12,    -1,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,   124,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,   124,
      12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    24,    -1,    -1,    -1,   104,   105,    30,    31,
      32,    33,    34,    35,    -1,   113,    -1,    39,    40,    -1,
      42,    43,    44,     4,    -1,    47,   124,    -1,    -1,    51,
      -1,    12,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,     4,    -1,   105,    -1,    -1,    -1,    -1,    -1,    12,
      -1,   113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    24,   124,    -1,    -1,    -1,    87,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,     4,    42,
      43,    44,    -1,    -1,    47,    -1,    12,    -1,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    24,    -1,
      -1,    -1,    -1,   124,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   105,   148,   149,   150,   153,   125,   129,   365,
     154,   165,     0,   154,    66,    67,   151,   106,   119,   155,
     166,   167,     1,   108,   364,   109,   135,   227,   227,   113,
     156,    14,   168,   179,   180,   135,   228,    78,    78,     4,
       8,    12,    21,    24,    28,    30,    31,    32,    33,    34,
      35,    37,    38,    39,    40,    42,    43,    44,    47,    51,
      54,    55,    56,    57,    58,   113,   124,   125,   128,   129,
     144,   157,   158,   159,   163,   331,   334,   335,   348,   350,
     351,   357,    27,    24,   169,   119,   164,     3,     5,     6,
       7,     8,     9,    10,    13,    16,    17,    18,    19,    20,
      22,    26,    29,    36,    50,    64,    77,    83,    90,    93,
      95,   109,   113,   115,   123,   125,   129,   134,   135,   136,
     137,   140,   145,   146,   177,   181,   183,   184,   185,   187,
     191,   203,   232,   277,   282,   286,   287,   289,   290,   291,
     292,   320,   321,   324,   325,   347,   348,   351,   359,   360,
     363,   110,   120,   365,   365,    80,    94,    97,   104,   126,
     127,   130,   131,   354,   355,   356,   358,   114,   120,   144,
     113,   160,   105,   107,   152,   365,   119,   113,   285,   286,
      33,    34,    35,    90,    95,    97,   105,   109,   113,   115,
     124,   209,   237,   239,   241,   242,   244,   248,   329,   330,
     334,   344,   346,   357,    37,   204,   334,    19,    22,    51,
      54,    55,   189,   190,   105,   107,   312,   285,    73,    74,
      75,    76,   188,   105,   107,   224,   225,    19,    37,    38,
     186,   239,   241,   330,    14,   312,   290,   113,   348,   109,
     113,   322,   323,   325,   360,   290,   310,   311,   290,   289,
     290,    80,   110,   121,   127,   131,   285,   286,   294,   296,
     297,   327,   341,   343,   353,   354,   356,   361,   362,   103,
     114,   120,   293,   294,   295,   354,   355,   356,   361,   366,
     116,   366,   109,   283,   284,    19,   283,   283,   136,   176,
     164,    19,    72,   211,    81,   120,    82,    85,   121,   279,
     280,   281,   327,   340,   342,   352,   354,   355,    89,   290,
     102,   105,    88,   135,   114,   114,   114,   114,   114,   159,
      79,   161,   162,   163,   154,   154,     4,   170,    23,    81,
     248,   248,   113,   232,   270,   271,   272,   351,    28,   110,
     234,   235,   237,   239,    80,    87,    94,   114,   126,   127,
     130,   131,   234,   252,   336,   337,   366,   366,    85,   256,
      92,    87,   121,   246,   332,   336,   345,    89,   245,   248,
     239,   113,   230,   235,   249,   239,    19,    19,    20,    46,
     285,   309,   313,   314,   315,   313,   119,   288,    78,    78,
      78,    78,   249,   223,   277,   278,   286,   223,    15,   198,
     239,   239,   113,   330,    81,   120,    82,    41,    52,    53,
     182,    78,   135,   362,    81,   120,   229,    87,   310,   350,
     359,   340,    79,    85,   120,   110,   120,   286,   349,   351,
     102,   114,   114,   120,   114,   120,   114,   114,   114,   120,
     116,    91,   136,   348,   249,   348,   348,   121,   178,   326,
     338,   339,   355,   362,   211,   135,   209,   231,   235,   236,
     347,   285,   299,   300,   315,   350,    27,   226,   281,   287,
     248,   349,    79,   316,   317,   318,   348,   349,   351,   290,
     114,   114,   120,   106,   364,   365,    12,   113,   171,   172,
     105,   107,   301,   230,   355,    81,   106,   120,   253,   110,
      81,    92,   114,   114,   120,   114,   114,   116,   257,   258,
     259,    27,   218,   239,   235,   334,   346,   241,   248,    81,
     206,   234,   251,   252,   249,   249,   225,   312,    86,   106,
     119,   364,    25,    27,   222,   106,   119,   364,   285,    82,
      81,    82,   207,     3,   124,   212,   213,   214,   235,   260,
     330,   234,   135,    78,   114,   109,   113,   115,   328,   329,
     322,    78,   285,   121,   121,   285,   298,   315,   285,   294,
     294,   349,   294,   294,   136,   110,    78,    78,    81,    81,
     351,   360,   120,    28,   210,   237,   239,    78,   135,    81,
      82,   205,   265,   226,    82,   120,   121,   225,   106,   120,
      82,   102,   163,   113,    21,   144,   163,   173,   174,   175,
     106,   119,   285,   302,   303,   304,   308,   302,   364,   114,
     235,   272,   104,   105,   113,   254,   255,   344,   260,   235,
     234,   120,    87,   344,   105,   107,   217,   121,   121,   260,
     114,   120,   285,   314,   285,   105,   107,   221,   278,   235,
     260,   254,    85,   192,   214,   347,   114,   116,   120,    79,
     110,   230,   233,   233,   121,   121,   338,   253,   205,   265,
      92,    78,   260,    28,   266,   267,   268,    27,   261,     9,
     273,   274,   275,   285,   315,   317,   294,   319,   349,   173,
     365,   160,   114,   120,   144,   302,   106,   119,    85,    87,
     305,   306,   307,   364,   235,   344,   344,   114,   258,   259,
       7,    26,   199,   215,   216,   278,   216,   234,   288,     7,
      26,   202,   203,   219,   220,   278,   220,   193,   346,    27,
     195,    81,   315,   285,    78,   120,    78,    92,   104,   261,
     273,   239,   253,    85,   238,   243,   247,   248,   269,   105,
     107,   273,   113,   189,   276,   333,   334,   275,    82,   102,
     114,   175,   304,   299,   285,   226,   307,    81,   106,    81,
      37,   200,    19,    37,   198,   239,   106,   119,   364,    11,
      19,   201,   201,   106,   119,   364,    87,   105,   107,   196,
     231,   230,   239,   237,   241,   273,   104,   267,    92,   121,
     247,   326,   262,   263,   264,   288,   262,   114,   239,   240,
     250,   276,   190,   294,   349,    87,   226,   260,   260,   239,
     198,   239,    81,    82,   208,   215,   285,   198,   211,   219,
     194,   346,    79,   197,   198,    79,   197,    92,   243,   269,
     243,   106,   119,   323,   364,   120,   114,   285,   106,   114,
     206,    82,   208,   260,   113,   255,   344,   210,   346,   106,
     106,   119,   364,   364,   239,   264,    81,   250,   344,    85,
     205,   265,   198,   230,   193,   261,   273,   273
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
     182,   182,   182,   183,   184,   184,   184,   184,   184,   184,
     185,   186,   186,   187,   187,   187,   187,   188,   188,   188,
     188,   188,   189,   189,   189,   190,   191,   191,   191,   192,
     192,   193,   194,   194,   195,   195,   196,   196,   196,   196,
     197,   197,   197,   197,   198,   199,   199,   199,   199,   199,
     200,   200,   201,   201,   202,   202,   202,   203,   203,   204,
     204,   205,   205,   206,   206,   207,   207,   207,   208,   208,
     208,   209,   209,   210,   210,   210,   210,   211,   211,   211,
     212,   212,   213,   213,   214,   214,   215,   215,   216,   216,
     216,   216,   217,   217,   218,   218,   219,   219,   220,   220,
     220,   220,   221,   221,   222,   222,   223,   223,   223,   223,
     224,   224,   225,   226,   226,   227,   227,   228,   228,   228,
     229,   229,   230,   231,   232,   232,   233,   233,   234,   234,
     235,   235,   235,   236,   237,   238,   239,   239,   240,   241,
     242,   242,   243,   243,   244,   244,   244,   245,   246,   246,
     247,   248,   248,   248,   248,   248,   248,   248,   248,   248,
     248,   249,   250,   250,   251,   251,   252,   252,   253,   253,
     254,   254,   254,   255,   255,   256,   256,   257,   257,   258,
     259,   259,   260,   261,   261,   261,   262,   262,   263,   263,
     264,   265,   266,   266,   267,   267,   268,   268,   269,   269,
     270,   270,   271,   271,   272,   273,   273,   274,   274,   275,
     275,   275,   276,   276,   276,   277,   277,   278,   279,   279,
     280,   280,   281,   282,   282,   282,   282,   282,   282,   282,
     282,   282,   283,   283,   284,   284,   285,   285,   286,   286,
     287,   287,   288,   288,   289,   289,   289,   289,   290,   290,
     290,   290,   290,   290,   290,   290,   290,   290,   291,   291,
     291,   292,   292,   292,   292,   292,   292,   292,   292,   293,
     293,   294,   294,   294,   295,   295,   296,   296,   296,   296,
     296,   296,   296,   297,   297,   298,   298,   299,   300,   300,
     301,   301,   301,   301,   302,   302,   303,   303,   303,   304,
     305,   305,   306,   306,   307,   308,   309,   310,   311,   311,
     312,   312,   313,   313,   313,   313,   314,   314,   315,   315,
     315,   316,   316,   317,   317,   317,   318,   318,   318,   318,
     319,   319,   320,   320,   321,   321,   322,   322,   322,   323,
     323,   324,   324,   324,   324,   325,   325,   326,   326,   327,
     327,   328,   328,   328,   329,   329,   329,   329,   329,   330,
     330,   331,   331,   331,   331,   332,   332,   333,   334,   334,
     335,   336,   336,   336,   337,   337,   337,   337,   338,   338,
     339,   339,   340,   340,   341,   341,   342,   342,   343,   343,
     344,   345,   346,   346,   346,   346,   346,   347,   347,   348,
     348,   348,   349,   350,   350,   351,   351,   351,   351,   351,
     351,   351,   351,   352,   352,   353,   353,   354,   355,   355,
     356,   356,   357,   357,   357,   357,   357,   357,   357,   357,
     357,   357,   357,   357,   357,   357,   357,   357,   357,   357,
     358,   358,   359,   359,   360,   361,   361,   362,   362,   363,
     363,   363,   363,   363,   364,   364,   365,   365,   366,   366
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
       1,     1,     1,     4,     4,     4,     5,     6,     6,     4,
       4,     3,     1,     4,     3,     6,     7,     2,     2,     2,
       2,     0,     1,     1,     1,     2,     3,     4,     4,     0,
       2,     3,     2,     1,     0,     2,     3,     3,     3,     3,
       3,     2,     1,     0,     3,     4,     3,     4,     2,     3,
       0,     1,     0,     1,     3,     6,     7,     1,     1,     0,
       1,     0,     2,     0,     2,     0,     2,     2,     0,     2,
       4,     3,     1,     6,     4,     3,     1,     4,     3,     0,
       0,     1,     1,     2,     1,     1,     1,     1,     3,     2,
       1,     0,     3,     3,     2,     0,     1,     1,     3,     2,
       1,     0,     3,     3,     2,     0,     3,     2,     1,     0,
       3,     3,     1,     2,     0,     1,     3,     3,     1,     0,
       0,     2,     1,     1,     3,     1,     1,     3,     1,     3,
       4,     3,     1,     1,     1,     1,     1,     3,     1,     1,
       1,     3,     1,     2,     1,     2,     3,     1,     1,     1,
       1,     1,     1,     1,     2,     2,     3,     2,     5,     3,
       3,     1,     1,     3,     1,     0,     1,     3,     2,     0,
       1,     3,     5,     1,     5,     0,     2,     3,     1,     3,
       2,     0,     1,     4,     4,     0,     1,     0,     3,     1,
       4,     2,     3,     1,     4,     2,     3,     0,     1,     3,
       0,     1,     3,     1,     3,     0,     1,     2,     1,     2,
       3,     3,     1,     2,     3,     1,     2,     1,     3,     2,
       2,     1,     4,     3,     3,     4,     4,     3,     4,     6,
       6,     4,     0,     1,     3,     4,     3,     1,     1,     3,
       2,     1,     1,     0,     2,     3,     2,     1,     3,     2,
       2,     4,     4,     8,     4,     2,     2,     1,     4,     3,
       1,     1,     1,     1,     3,     3,     3,     3,     1,     3,
       2,     1,     2,     2,     3,     3,     1,     1,     2,     4,
       3,     5,     3,     3,     3,     3,     1,     1,     3,     1,
       3,     3,     2,     2,     1,     2,     3,     2,     1,     2,
       3,     2,     2,     1,     4,     1,     1,     1,     2,     1,
       3,     3,     3,     2,     1,     0,     1,     2,     3,     1,
       2,     1,     0,     3,     1,     1,     3,     1,     5,     3,
       3,     1,     1,     1,     1,     3,     1,     3,     1,     3,
       1,     2,     3,     2,     3,     1,     2,     1,     3,     1,
       3,     1,     2,     2,     1,     3,     3,     3,     2,     1,
       3,     1,     3,     3,     3,     1,     3,     1,     1,     1,
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
  "datafam_inst_hdr", "capi_ctype", "maybe_roles", "roles", "role",
  "decl_cls", "decls_cls", "decllist_cls", "where_cls", "decl_inst",
  "decls_inst", "decllist_inst", "where_inst", "decls", "decllist",
  "binds", "wherebinds", "strings", "stringlist", "opt_tyconsig",
  "sigtype", "sigtypedoc", "sig_vars", "sigtypes1", "ktype", "ctype",
  "ctypedoc", "context", "context_no_ops", "type", "typedoc", "btype",
  "infixtype", "btype_no_ops", "ftype", "tyarg", "tyop", "atype_docs",
  "atype", "inst_type", "deriv_types", "comma_types0", "comma_types1",
  "tv_bndrs", "tv_bndr", "tv_bndr_no_braces", "fds", "fds1", "fd",
  "varids0", "kind", "gadt_constrlist", "gadt_constrs0", "gadt_constrs",
  "gadt_constr", "constrs", "constrs1", "constr", "forall", "constr_stuff",
  "fielddecls", "fielddecls1", "fielddecl", "maybe_derivings", "derivings",
  "deriving", "deriv_clause_types", "decl_no_th", "decl", "rhs", "gdrhs",
  "gdrh", "sigdecl", "activation", "explicit_activation", "exp",
  "infixexp", "exp10", "optSemi", "fexp", "aexp", "aexp1", "aexp2",
  "projection", "texp", "tup_exprs", "list", "lexps", "squals",
  "guardquals", "guardquals1", "altslist", "alts", "alts1", "alt",
  "alt_rhs", "gdpats", "gdpat", "pat", "bindpat", "apat", "apats1",
  "stmtlist", "stmts", "stmt", "qual", "fbinds", "fbinds1", "fbind",
  "fieldToUpdate", "qcon", "gen_qcon", "con", "con_list",
  "sysdcon_no_list", "sysdcon", "conop", "qconop", "gtycon", "ntgtycon",
  "oqtycon", "oqtycon_no_varcon", "qtyconop", "qtycondoc", "qtycon",
  "tycon", "qtyconsym", "tyconsym", "op", "varop", "qop", "qopm", "qvarop",
  "qvaropm", "tyvar", "tyvarop", "tyvarid", "var", "qvar", "field",
  "qvarid", "varid", "qvarsym", "qvarsym_no_minus", "qvarsym1", "varsym",
  "varsym_no_minus", "special_id", "special_sym", "qconid", "conid",
  "qconsym", "consym", "literal", "close", "modid", "commas", YY_NULLPTR
  };
#endif


#if YYDEBUG
  const short
  parser::yyrline_[] =
  {
       0,   524,   524,   541,   542,   544,   548,   549,   550,   552,
     553,   555,   556,   559,   561,   562,   563,   571,   572,   574,
     575,   576,   577,   579,   580,   582,   583,   584,   586,   587,
     588,   590,   591,   593,   594,   596,   597,   601,   602,   604,
     605,   607,   609,   610,   612,   625,   626,   628,   629,   631,
     632,   636,   637,   639,   640,   641,   642,   644,   645,   647,
     648,   653,   654,   656,   657,   658,   660,   661,   665,   667,
     668,   670,   671,   672,   673,   674,   675,   676,   682,   684,
     687,   688,   689,   691,   694,   695,   696,   698,   699,   700,
     702,   704,   705,   708,   709,   710,   716,   723,   724,   725,
     726,   727,   729,   730,   731,   733,   735,   737,   739,   744,
     745,   747,   749,   750,   754,   755,   757,   758,   759,   760,
     762,   763,   764,   765,   767,   770,   772,   774,   776,   777,
     779,   779,   781,   781,   785,   787,   794,   801,   802,   805,
     806,   810,   811,   813,   814,   816,   817,   818,   820,   821,
     822,   825,   826,   829,   830,   831,   832,   834,   835,   836,
     841,   842,   844,   845,   847,   861,   889,   890,   892,   893,
     894,   895,   897,   898,   900,   901,   903,   904,   906,   907,
     908,   909,   911,   912,   914,   915,   918,   919,   920,   921,
     923,   924,   926,   928,   929,   937,   938,   940,   941,   942,
     955,   956,   965,   967,   969,   970,   972,   973,   982,   983,
     985,   986,   988,   990,   999,  1001,  1003,  1004,  1006,  1009,
    1011,  1012,  1014,  1015,  1017,  1019,  1020,  1022,  1024,  1025,
    1032,  1039,  1040,  1041,  1042,  1043,  1044,  1045,  1046,  1052,
    1053,  1056,  1058,  1059,  1061,  1062,  1064,  1065,  1072,  1073,
    1075,  1076,  1077,  1080,  1081,  1085,  1086,  1088,  1089,  1092,
    1094,  1095,  1100,  1106,  1107,  1108,  1110,  1111,  1113,  1114,
    1116,  1118,  1120,  1121,  1123,  1124,  1126,  1127,  1129,  1130,
    1136,  1137,  1139,  1140,  1142,  1144,  1145,  1147,  1148,  1150,
    1155,  1160,  1166,  1167,  1168,  1173,  1175,  1177,  1181,  1182,
    1184,  1185,  1189,  1199,  1200,  1202,  1203,  1204,  1205,  1206,
    1207,  1208,  1211,  1212,  1214,  1215,  1220,  1221,  1225,  1226,
    1228,  1229,  1231,  1232,  1237,  1238,  1239,  1240,  1243,  1244,
    1245,  1246,  1247,  1249,  1251,  1252,  1253,  1255,  1258,  1259,
    1260,  1263,  1264,  1265,  1266,  1267,  1268,  1273,  1274,  1277,
    1278,  1283,  1284,  1285,  1290,  1291,  1309,  1310,  1311,  1312,
    1313,  1314,  1315,  1317,  1318,  1331,  1333,  1343,  1345,  1346,
    1349,  1350,  1351,  1352,  1354,  1355,  1357,  1358,  1359,  1361,
    1363,  1364,  1366,  1367,  1376,  1378,  1380,  1382,  1384,  1385,
    1388,  1389,  1391,  1392,  1393,  1394,  1399,  1400,  1402,  1403,
    1404,  1409,  1410,  1412,  1413,  1414,  1416,  1417,  1418,  1419,
    1422,  1423,  1455,  1456,  1458,  1459,  1461,  1462,  1463,  1465,
    1466,  1468,  1469,  1470,  1471,  1473,  1474,  1476,  1477,  1479,
    1480,  1483,  1484,  1485,  1487,  1488,  1489,  1490,  1491,  1493,
    1494,  1496,  1497,  1498,  1499,  1502,  1503,  1505,  1507,  1508,
    1512,  1514,  1515,  1516,  1518,  1519,  1520,  1521,  1526,  1527,
    1529,  1530,  1532,  1533,  1536,  1537,  1542,  1543,  1545,  1546,
    1550,  1552,  1554,  1555,  1556,  1557,  1558,  1561,  1562,  1564,
    1565,  1566,  1568,  1570,  1571,  1573,  1574,  1575,  1576,  1577,
    1578,  1579,  1580,  1582,  1583,  1585,  1586,  1588,  1590,  1591,
    1593,  1594,  1596,  1597,  1598,  1599,  1600,  1601,  1602,  1603,
    1604,  1605,  1606,  1607,  1608,  1609,  1610,  1611,  1612,  1613,
    1615,  1616,  1620,  1621,  1623,  1625,  1626,  1628,  1629,  1633,
    1634,  1635,  1636,  1637,  1642,  1645,  1649,  1650,  1652,  1653
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
#line 7932 "parser.cc"

#line 1662 "parser.y"


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
    return make_parsed_app(loc, selector, object);
}

Hs::LExp make_parsed_app(const yy::location& loc, const Hs::LExp& head, const Hs::LExp& arg)
{
    // Accumulate parser application syntax into one flat source spine.
    std::vector<Hs::LExp> terms;
    if (auto app = unloc(head).to<Hs::ParsedApp>())
        terms = app->terms;
    else
        terms.push_back(head);
    terms.push_back(arg);
    return {loc, Hs::ParsedApp(terms)};
}

Hs::LExp make_record_expression(const yy::location& loc, const Hs::LExp& head, const Located<Hs::FieldBindings>& fbinds)
{
    return {loc, Hs::RecordSyntax(head, fbinds)};
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
