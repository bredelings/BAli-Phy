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
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
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
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
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
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
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
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
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
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
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
#line 513 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2586 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 530 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < Located<std::string> > (),yystack_[2].value.as < std::optional<std::vector<Hs::LExport>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2592 "parser.cc"
    break;

  case 4: // module: body2
#line 531 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{{noloc,"Main"},{},yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2598 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 533 "parser.y"
                                                                 {drv.push_module_context();}
#line 2604 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 541 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2610 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 542 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2616 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 544 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2622 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 545 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2628 "parser.cc"
    break;

  case 13: // top: semis top1
#line 548 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2634 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 550 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2640 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 551 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2646 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 552 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::LImpDecl> > (),{});}
#line 2652 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 560 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::LExport>> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2658 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 561 "parser.y"
                                      {}
#line 2664 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 563 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2670 "parser.cc"
    break;

  case 20: // exportlist: %empty
#line 564 "parser.y"
                                      {}
#line 2676 "parser.cc"
    break;

  case 21: // exportlist: exportlist1 ','
#line 565 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2682 "parser.cc"
    break;

  case 22: // exportlist: ','
#line 566 "parser.y"
                                      {}
#line 2688 "parser.cc"
    break;

  case 23: // exportlist1: exportlist1 "," export
#line 568 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2694 "parser.cc"
    break;

  case 24: // exportlist1: export
#line 569 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2700 "parser.cc"
    break;

  case 25: // export: qcname export_subspec
#line 571 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2706 "parser.cc"
    break;

  case 26: // export: "module" modid
#line 572 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2712 "parser.cc"
    break;

  case 27: // export: "default" modid
#line 573 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::default_}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2718 "parser.cc"
    break;

  case 28: // export_subspec: %empty
#line 575 "parser.y"
                                      {}
#line 2724 "parser.cc"
    break;

  case 29: // export_subspec: "(" qcnames ")"
#line 576 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2730 "parser.cc"
    break;

  case 30: // export_subspec: "(" ".." ")"
#line 577 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec(); }
#line 2736 "parser.cc"
    break;

  case 31: // qcnames: %empty
#line 579 "parser.y"
                   {}
#line 2742 "parser.cc"
    break;

  case 32: // qcnames: qcnames1
#line 580 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2748 "parser.cc"
    break;

  case 33: // qcnames1: qcnames1 "," qcname
#line 582 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2754 "parser.cc"
    break;

  case 34: // qcnames1: qcname
#line 583 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2760 "parser.cc"
    break;

  case 35: // qcname: qvar
#line 585 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2766 "parser.cc"
    break;

  case 36: // qcname: oqtycon_no_varcon
#line 586 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2772 "parser.cc"
    break;

  case 41: // importdecls: importdecls_semi importdecl
#line 596 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[1].value.as < std::vector<Hs::LImpDecl> > (), yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[0].value.as < Hs::LImpDecl > ()); }
#line 2778 "parser.cc"
    break;

  case 42: // importdecls_semi: importdecls_semi importdecl semis1
#line 598 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[2].value.as < std::vector<Hs::LImpDecl> > (); yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[1].value.as < Hs::LImpDecl > ()); }
#line 2784 "parser.cc"
    break;

  case 43: // importdecls_semi: %empty
#line 599 "parser.y"
                         { }
#line 2790 "parser.cc"
    break;

  case 44: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 601 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::LImpDecl > () = {yylhs.location, Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < Located<std::string> > (),yystack_[1].value.as < std::optional<Located<std::string>> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ())};
}
#line 2798 "parser.cc"
    break;

  case 45: // optqualified: "qualified"
#line 614 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2804 "parser.cc"
    break;

  case 46: // optqualified: %empty
#line 615 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2810 "parser.cc"
    break;

  case 47: // maybeas: "as" modid
#line 617 "parser.y"
                               { yylhs.value.as < std::optional<Located<std::string>> > () = yystack_[0].value.as < Located<std::string> > (); }
#line 2816 "parser.cc"
    break;

  case 48: // maybeas: %empty
#line 618 "parser.y"
                               { }
#line 2822 "parser.cc"
    break;

  case 49: // maybeimpspec: impspec
#line 620 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2828 "parser.cc"
    break;

  case 50: // maybeimpspec: %empty
#line 621 "parser.y"
                               { }
#line 2834 "parser.cc"
    break;

  case 51: // impspec: "(" importlist ")"
#line 625 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2840 "parser.cc"
    break;

  case 52: // impspec: "hiding" "(" importlist ")"
#line 626 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2846 "parser.cc"
    break;

  case 53: // importlist: importlist1
#line 628 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2852 "parser.cc"
    break;

  case 54: // importlist: %empty
#line 629 "parser.y"
                                      {}
#line 2858 "parser.cc"
    break;

  case 55: // importlist: importlist1 ','
#line 630 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2864 "parser.cc"
    break;

  case 56: // importlist: ','
#line 631 "parser.y"
                                      {}
#line 2870 "parser.cc"
    break;

  case 57: // importlist1: importlist1 "," import
#line 633 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2876 "parser.cc"
    break;

  case 58: // importlist1: import
#line 634 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2882 "parser.cc"
    break;

  case 59: // import: qcname export_subspec
#line 636 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2888 "parser.cc"
    break;

  case 60: // import: "module" modid
#line 637 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2894 "parser.cc"
    break;

  case 61: // prec: %empty
#line 642 "parser.y"
                   { }
#line 2900 "parser.cc"
    break;

  case 62: // prec: "INTEGER"
#line 643 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < integer > ().convert_to<int>(); }
#line 2906 "parser.cc"
    break;

  case 63: // infix: "infix"
#line 645 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2912 "parser.cc"
    break;

  case 64: // infix: "infixl"
#line 646 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2918 "parser.cc"
    break;

  case 65: // infix: "infixr"
#line 647 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2924 "parser.cc"
    break;

  case 66: // ops: ops "," op
#line 649 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 2930 "parser.cc"
    break;

  case 67: // ops: op
#line 650 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = {{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2936 "parser.cc"
    break;

  case 68: // topdecls: topdecls_semi topdecl
#line 654 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ()); }
#line 2942 "parser.cc"
    break;

  case 69: // topdecls_semi: topdecls_semi topdecl semis1
#line 656 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 2948 "parser.cc"
    break;

  case 70: // topdecls_semi: %empty
#line 657 "parser.y"
                                            { }
#line 2954 "parser.cc"
    break;

  case 71: // topdecl: cl_decl
#line 659 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2960 "parser.cc"
    break;

  case 72: // topdecl: ty_decl
#line 660 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2966 "parser.cc"
    break;

  case 73: // topdecl: standalone_kind_sig
#line 661 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2972 "parser.cc"
    break;

  case 74: // topdecl: inst_decl
#line 662 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2978 "parser.cc"
    break;

  case 75: // topdecl: "default" opt_class "(" comma_types0 ")"
#line 665 "parser.y"
                                                         {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::DefaultDecl(yystack_[3].value.as < std::optional<Located<std::string>> > (),yystack_[1].value.as < std::vector<Hs::LType> > ())}; }
#line 2984 "parser.cc"
    break;

  case 76: // topdecl: "foreign" "import" call_conv "STRING" var "::" sigtypedoc
#line 666 "parser.y"
                                                                  {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ForeignDecl(yystack_[4].value.as < Located<std::string> > (), yystack_[3].value.as < std::string > (), {yystack_[2].location,yystack_[2].value.as < std::string > ()}, yystack_[0].value.as < Hs::LType > ())};}
#line 2990 "parser.cc"
    break;

  case 77: // topdecl: decl_no_th
#line 672 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2996 "parser.cc"
    break;

  case 78: // topdecl: infixexp
#line 674 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<Hs::InfixExp> > ();}
#line 3002 "parser.cc"
    break;

  case 79: // call_conv: "bpcall"
#line 677 "parser.y"
                    {yylhs.value.as < Located<std::string> > () = {yylhs.location,"bpcall"};}
#line 3008 "parser.cc"
    break;

  case 80: // call_conv: "trcall"
#line 678 "parser.y"
                    {yylhs.value.as < Located<std::string> > () = {yylhs.location,"trcall"};}
#line 3014 "parser.cc"
    break;

  case 81: // cl_decl: "class" tycl_hdr fds where_cls
#line 680 "parser.y"
                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_class_decl(yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first,yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,yystack_[1].value.as < std::vector<Hs::FunDep> > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 3020 "parser.cc"
    break;

  case 82: // ty_decl: "type" type "=" ktype
#line 683 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_type_synonym(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3026 "parser.cc"
    break;

  case 83: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 684 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > ())};}
#line 3032 "parser.cc"
    break;

  case 84: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 686 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ())};}
#line 3038 "parser.cc"
    break;

  case 85: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 687 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[3].value.as < Hs::LType > (), yystack_[2].value.as < std::optional<Located<Hs::Kind>> > (), yystack_[0].value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ())};}
#line 3044 "parser.cc"
    break;

  case 86: // ty_decl: "data" "family" type opt_datafam_kind_sig
#line 688 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})};}
#line 3050 "parser.cc"
    break;

  case 87: // standalone_kind_sig: "type" sks_vars "::" kind
#line 690 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::KindSigDecl(yystack_[2].value.as < std::vector<Hs::LTypeCon> > (),yystack_[0].value.as < Hs::Kind > ())};}
#line 3056 "parser.cc"
    break;

  case 88: // sks_vars: sks_vars "," oqtycon
#line 692 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = yystack_[2].value.as < std::vector<Hs::LTypeCon> > (); yylhs.value.as < std::vector<Hs::LTypeCon> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}); }
#line 3062 "parser.cc"
    break;

  case 89: // sks_vars: oqtycon
#line 693 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}}; }
#line 3068 "parser.cc"
    break;

  case 90: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 696 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_instance_decl(yystack_[2].value.as < std::optional<std::string> > (),yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 3074 "parser.cc"
    break;

  case 91: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 697 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};}
#line 3080 "parser.cc"
    break;

  case 92: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 699 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), {}, {}, yystack_[1].value.as < Hs::ConstructorsDecl > ()))};
	   }
#line 3090 "parser.cc"
    break;

  case 93: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 705 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ()))};
	   }
#line 3100 "parser.cc"
    break;

  case 94: // overlap_pragma: "{-# OVERLAPPABLE" "#-}"
#line 711 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPABLE"; }
#line 3106 "parser.cc"
    break;

  case 95: // overlap_pragma: "{-# OVERLAPPING" "#-}"
#line 712 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPING"; }
#line 3112 "parser.cc"
    break;

  case 96: // overlap_pragma: "{-# OVERLAPS" "#-}"
#line 713 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPS"; }
#line 3118 "parser.cc"
    break;

  case 97: // overlap_pragma: "{-# INCOHERENT" "#-}"
#line 714 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "INCOHERENT"; }
#line 3124 "parser.cc"
    break;

  case 98: // overlap_pragma: %empty
#line 715 "parser.y"
                                               {}
#line 3130 "parser.cc"
    break;

  case 108: // where_type_family: %empty
#line 742 "parser.y"
                                                           {}
#line 3136 "parser.cc"
    break;

  case 109: // where_type_family: "where" ty_fam_inst_eqn_list
#line 743 "parser.y"
                                                           {yylhs.value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > () = yystack_[0].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3142 "parser.cc"
    break;

  case 110: // ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
#line 745 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3148 "parser.cc"
    break;

  case 111: // ty_fam_inst_eqn_list: "vocurly" ty_fam_inst_eqns close
#line 746 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3154 "parser.cc"
    break;

  case 112: // ty_fam_inst_eqn_list: "{" ".." "}"
#line 747 "parser.y"
                                                           {}
#line 3160 "parser.cc"
    break;

  case 113: // ty_fam_inst_eqn_list: "vocurly" ".." close
#line 748 "parser.y"
                                                           {}
#line 3166 "parser.cc"
    break;

  case 114: // ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
#line 750 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[2].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > (); yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ().push_back(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 3172 "parser.cc"
    break;

  case 115: // ty_fam_inst_eqns: ty_fam_inst_eqns ";"
#line 751 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3178 "parser.cc"
    break;

  case 116: // ty_fam_inst_eqns: ty_fam_inst_eqn
#line 752 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = {yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ()};}
#line 3184 "parser.cc"
    break;

  case 117: // ty_fam_inst_eqns: %empty
#line 753 "parser.y"
                                                           {}
#line 3190 "parser.cc"
    break;

  case 118: // ty_fam_inst_eqn: type "=" ctype
#line 755 "parser.y"
                                                           {yylhs.value.as < Hs::TypeFamilyInstanceEqn > () = make_type_family_instance_eqn(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ());}
#line 3196 "parser.cc"
    break;

  case 119: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 758 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3202 "parser.cc"
    break;

  case 120: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 760 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3208 "parser.cc"
    break;

  case 121: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 762 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3214 "parser.cc"
    break;

  case 122: // at_decl_cls: "type" ty_fam_inst_eqn
#line 764 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3220 "parser.cc"
    break;

  case 123: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 765 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3226 "parser.cc"
    break;

  case 128: // at_decl_inst: "type" opt_instance ty_fam_inst_eqn
#line 773 "parser.y"
                                                              { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3232 "parser.cc"
    break;

  case 129: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 776 "parser.y"
              {
		  auto& [tvs, context, type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), {}, {}, yystack_[1].value.as < Hs::ConstructorsDecl > ()))};
	      }
#line 3242 "parser.cc"
    break;

  case 130: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 783 "parser.y"
              {
		  auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ()))};
	      }
#line 3252 "parser.cc"
    break;

  case 131: // data_or_newtype: "data"
#line 789 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 3258 "parser.cc"
    break;

  case 132: // data_or_newtype: "newtype"
#line 790 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 3264 "parser.cc"
    break;

  case 133: // opt_class: %empty
#line 793 "parser.y"
                  {yylhs.value.as < std::optional<Located<std::string>> > () = {};}
#line 3270 "parser.cc"
    break;

  case 134: // opt_class: qtycon
#line 794 "parser.y"
                  {yylhs.value.as < std::optional<Located<std::string>> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()};}
#line 3276 "parser.cc"
    break;

  case 135: // opt_kind_sig: %empty
#line 798 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 3282 "parser.cc"
    break;

  case 136: // opt_kind_sig: "::" kind
#line 799 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < Hs::Kind > ();}
#line 3288 "parser.cc"
    break;

  case 137: // opt_datafam_kind_sig: %empty
#line 801 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {};}
#line 3294 "parser.cc"
    break;

  case 138: // opt_datafam_kind_sig: "::" kind
#line 802 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3300 "parser.cc"
    break;

  case 139: // opt_tyfam_kind_sig: %empty
#line 804 "parser.y"
                                      {}
#line 3306 "parser.cc"
    break;

  case 140: // opt_tyfam_kind_sig: "::" kind
#line 805 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3312 "parser.cc"
    break;

  case 141: // opt_tyfam_kind_sig: "=" tv_bndr
#line 806 "parser.y"
                                      {}
#line 3318 "parser.cc"
    break;

  case 142: // opt_at_kind_inj_sig: %empty
#line 808 "parser.y"
                                      {}
#line 3324 "parser.cc"
    break;

  case 143: // opt_at_kind_inj_sig: "::" kind
#line 809 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3330 "parser.cc"
    break;

  case 144: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 810 "parser.y"
                                                                  {}
#line 3336 "parser.cc"
    break;

  case 145: // tycl_hdr: context "=>" type
#line 813 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ()};}
#line 3342 "parser.cc"
    break;

  case 146: // tycl_hdr: type
#line 814 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {{},yystack_[0].value.as < Hs::LType > ()};}
#line 3348 "parser.cc"
    break;

  case 147: // datafam_inst_hdr: "forall" tv_bndrs "." context "=>" type
#line 817 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[4].value.as < std::vector<Hs::LTypeVar> > ()}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3354 "parser.cc"
    break;

  case 148: // datafam_inst_hdr: "forall" tv_bndrs "=>" type
#line 818 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[2].value.as < std::vector<Hs::LTypeVar> > ()}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3360 "parser.cc"
    break;

  case 149: // datafam_inst_hdr: context "=>" type
#line 819 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3366 "parser.cc"
    break;

  case 150: // datafam_inst_hdr: type
#line 820 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3372 "parser.cc"
    break;

  case 154: // decl_cls: at_decl_cls
#line 866 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3378 "parser.cc"
    break;

  case 155: // decl_cls: decl
#line 867 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3384 "parser.cc"
    break;

  case 156: // decls_cls: decls_cls ";" decl_cls
#line 869 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3390 "parser.cc"
    break;

  case 157: // decls_cls: decls_cls ";"
#line 870 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3396 "parser.cc"
    break;

  case 158: // decls_cls: decl_cls
#line 871 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3402 "parser.cc"
    break;

  case 159: // decls_cls: %empty
#line 872 "parser.y"
                                           {}
#line 3408 "parser.cc"
    break;

  case 160: // decllist_cls: "{" decls_cls "}"
#line 874 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3414 "parser.cc"
    break;

  case 161: // decllist_cls: "vocurly" decls_cls close
#line 875 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3420 "parser.cc"
    break;

  case 162: // where_cls: "where" decllist_cls
#line 877 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3426 "parser.cc"
    break;

  case 163: // where_cls: %empty
#line 878 "parser.y"
                                           {}
#line 3432 "parser.cc"
    break;

  case 164: // decl_inst: at_decl_inst
#line 880 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3438 "parser.cc"
    break;

  case 165: // decl_inst: decl
#line 881 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3444 "parser.cc"
    break;

  case 166: // decls_inst: decls_inst ";" decl_inst
#line 883 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3450 "parser.cc"
    break;

  case 167: // decls_inst: decls_inst ";"
#line 884 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3456 "parser.cc"
    break;

  case 168: // decls_inst: decl_inst
#line 885 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3462 "parser.cc"
    break;

  case 169: // decls_inst: %empty
#line 886 "parser.y"
                                           {}
#line 3468 "parser.cc"
    break;

  case 170: // decllist_inst: "{" decls_inst "}"
#line 888 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3474 "parser.cc"
    break;

  case 171: // decllist_inst: "vocurly" decls_inst close
#line 889 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3480 "parser.cc"
    break;

  case 172: // where_inst: "where" decllist_inst
#line 891 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3486 "parser.cc"
    break;

  case 173: // where_inst: %empty
#line 892 "parser.y"
                                           {}
#line 3492 "parser.cc"
    break;

  case 174: // decls: decls ";" decl
#line 895 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3498 "parser.cc"
    break;

  case 175: // decls: decls ";"
#line 896 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3504 "parser.cc"
    break;

  case 176: // decls: decl
#line 897 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3510 "parser.cc"
    break;

  case 177: // decls: %empty
#line 898 "parser.y"
                        {}
#line 3516 "parser.cc"
    break;

  case 178: // decllist: "{" decls "}"
#line 900 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3522 "parser.cc"
    break;

  case 179: // decllist: "vocurly" decls close
#line 901 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3528 "parser.cc"
    break;

  case 180: // binds: decllist
#line 903 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 3534 "parser.cc"
    break;

  case 181: // wherebinds: "where" binds
#line 905 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 3540 "parser.cc"
    break;

  case 182: // wherebinds: %empty
#line 906 "parser.y"
                                 {}
#line 3546 "parser.cc"
    break;

  case 188: // opt_tyconsig: %empty
#line 932 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {};}
#line 3552 "parser.cc"
    break;

  case 189: // opt_tyconsig: "::" gtycon
#line 933 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}};}
#line 3558 "parser.cc"
    break;

  case 190: // sigtype: ctype
#line 942 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3564 "parser.cc"
    break;

  case 191: // sigtypedoc: ctypedoc
#line 944 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3570 "parser.cc"
    break;

  case 192: // sig_vars: sig_vars "," var
#line 946 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > () = yystack_[2].value.as < std::vector<Hs::LVar> > (); yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3576 "parser.cc"
    break;

  case 193: // sig_vars: var
#line 947 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3582 "parser.cc"
    break;

  case 194: // sigtypes1: sigtype
#line 949 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3588 "parser.cc"
    break;

  case 195: // sigtypes1: sigtypes1 "," sigtype
#line 950 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3594 "parser.cc"
    break;

  case 196: // ktype: ctype
#line 959 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3600 "parser.cc"
    break;

  case 197: // ktype: ctype "::" kind
#line 960 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeOfKind(yystack_[2].value.as < Hs::LType > (), yystack_[0].value.as < Hs::Kind > ())};}
#line 3606 "parser.cc"
    break;

  case 198: // ctype: "forall" tv_bndrs "." ctype
#line 962 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ForallType(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3612 "parser.cc"
    break;

  case 199: // ctype: context "=>" ctype
#line 963 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3618 "parser.cc"
    break;

  case 200: // ctype: type
#line 965 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3624 "parser.cc"
    break;

  case 201: // ctypedoc: ctype
#line 967 "parser.y"
          { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3630 "parser.cc"
    break;

  case 202: // context: btype
#line 976 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::LType > ());}
#line 3636 "parser.cc"
    break;

  case 203: // context_no_ops: btype_no_ops
#line 978 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ()));}
#line 3642 "parser.cc"
    break;

  case 204: // type: btype
#line 980 "parser.y"
      { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3648 "parser.cc"
    break;

  case 205: // type: btype "->" ctype
#line 981 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = Hs::type_apply({{yystack_[1].location,Hs::TypeCon("->")},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3654 "parser.cc"
    break;

  case 206: // typedoc: type
#line 983 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3660 "parser.cc"
    break;

  case 207: // btype: infixtype
#line 986 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3666 "parser.cc"
    break;

  case 208: // infixtype: ftype
#line 988 "parser.y"
           { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3672 "parser.cc"
    break;

  case 209: // infixtype: btype tyop btype
#line 989 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = Hs::type_apply({{yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3678 "parser.cc"
    break;

  case 210: // btype_no_ops: atype_docs
#line 991 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3684 "parser.cc"
    break;

  case 211: // btype_no_ops: btype_no_ops atype_docs
#line 992 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3690 "parser.cc"
    break;

  case 212: // ftype: atype
#line 994 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3696 "parser.cc"
    break;

  case 213: // ftype: ftype tyarg
#line 996 "parser.y"
                                   { yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeApp(yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())}; }
#line 3702 "parser.cc"
    break;

  case 214: // ftype: ftype "@" atype
#line 997 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[2].value.as < Hs::LType > (); }
#line 3708 "parser.cc"
    break;

  case 215: // tyarg: atype
#line 999 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3714 "parser.cc"
    break;

  case 216: // tyop: qtyconop
#line 1001 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3720 "parser.cc"
    break;

  case 217: // tyop: tyvarop
#line 1002 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3726 "parser.cc"
    break;

  case 218: // atype_docs: atype
#line 1009 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3732 "parser.cc"
    break;

  case 219: // atype: ntgtycon
#line 1016 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon(yystack_[0].value.as < std::string > ())};}
#line 3738 "parser.cc"
    break;

  case 220: // atype: tyvar
#line 1017 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3744 "parser.cc"
    break;

  case 221: // atype: "*"
#line 1018 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("*")};}
#line 3750 "parser.cc"
    break;

  case 222: // atype: PREFIX_BANG atype
#line 1019 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::StrictType(yystack_[0].value.as < Hs::LType > ())};}
#line 3756 "parser.cc"
    break;

  case 223: // atype: PREFIX_TILDE atype
#line 1020 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::LazyType(yystack_[0].value.as < Hs::LType > ())};}
#line 3762 "parser.cc"
    break;

  case 224: // atype: "{" fielddecls "}"
#line 1021 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ())};}
#line 3768 "parser.cc"
    break;

  case 225: // atype: "(" ")"
#line 1022 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("()")};}
#line 3774 "parser.cc"
    break;

  case 226: // atype: "(" comma_types1 "," ktype ")"
#line 1023 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::LType> > ();ts.push_back(yystack_[1].value.as < Hs::LType > ());yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TupleType(ts)};}
#line 3780 "parser.cc"
    break;

  case 227: // atype: "[" ktype "]"
#line 1029 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ListType{yystack_[1].value.as < Hs::LType > ()}}; }
#line 3786 "parser.cc"
    break;

  case 228: // atype: "(" ktype ")"
#line 1030 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[1].value.as < Hs::LType > ();}
#line 3792 "parser.cc"
    break;

  case 229: // inst_type: sigtype
#line 1033 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3798 "parser.cc"
    break;

  case 232: // comma_types0: comma_types1
#line 1038 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > ();}
#line 3804 "parser.cc"
    break;

  case 233: // comma_types0: %empty
#line 1039 "parser.y"
                                       { /* default construction OK */ }
#line 3810 "parser.cc"
    break;

  case 234: // comma_types1: ktype
#line 1041 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3816 "parser.cc"
    break;

  case 235: // comma_types1: comma_types1 "," ktype
#line 1042 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3822 "parser.cc"
    break;

  case 236: // tv_bndrs: tv_bndrs tv_bndr
#line 1049 "parser.y"
                               {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back(yystack_[0].value.as < Hs::LTypeVar > ());}
#line 3828 "parser.cc"
    break;

  case 237: // tv_bndrs: %empty
#line 1050 "parser.y"
                               { /* default construction OK */}
#line 3834 "parser.cc"
    break;

  case 238: // tv_bndr: tv_bndr_no_braces
#line 1052 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = yystack_[0].value.as < Hs::LTypeVar > ();}
#line 3840 "parser.cc"
    break;

  case 239: // tv_bndr: "{" tyvar "}"
#line 1053 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[1].location,Hs::TypeVar(yystack_[1].value.as < std::string > ())};}
#line 3846 "parser.cc"
    break;

  case 240: // tv_bndr: "{" tyvar "::" kind "}"
#line 1054 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[3].location,Hs::TypeVar(yystack_[3].value.as < std::string > ())};}
#line 3852 "parser.cc"
    break;

  case 241: // tv_bndr_no_braces: tyvar
#line 1057 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3858 "parser.cc"
    break;

  case 242: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 1058 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[3].value.as < std::string > (),yystack_[1].value.as < Hs::Kind > ())};}
#line 3864 "parser.cc"
    break;

  case 243: // fds: %empty
#line 1062 "parser.y"
                                    { /* default to empty */ }
#line 3870 "parser.cc"
    break;

  case 244: // fds: "|" fds1
#line 1063 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > () = yystack_[0].value.as < std::vector<Hs::FunDep> > (); }
#line 3876 "parser.cc"
    break;

  case 245: // fds1: fds1 "," fd
#line 1065 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > () = yystack_[2].value.as < std::vector<Hs::FunDep> > (); yylhs.value.as < std::vector<Hs::FunDep> > ().push_back(yystack_[0].value.as < Hs::FunDep > ()); }
#line 3882 "parser.cc"
    break;

  case 246: // fds1: fd
#line 1066 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > ().push_back(yystack_[0].value.as < Hs::FunDep > ()); }
#line 3888 "parser.cc"
    break;

  case 247: // fd: varids0 "->" varids0
#line 1069 "parser.y"
                                    { yylhs.value.as < Hs::FunDep > () = Hs::FunDep(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < std::vector<Hs::LTypeVar> > ()); }
#line 3894 "parser.cc"
    break;

  case 248: // varids0: varids0 tyvar
#line 1071 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3900 "parser.cc"
    break;

  case 249: // varids0: %empty
#line 1072 "parser.y"
                                    { /* default to empty */}
#line 3906 "parser.cc"
    break;

  case 250: // kind: ctype
#line 1077 "parser.y"
             {yylhs.value.as < Hs::Kind > () = type_to_kind(yystack_[0].value.as < Hs::LType > ());}
#line 3912 "parser.cc"
    break;

  case 251: // gadt_constrlist: "where" "{" gadt_constrs "}"
#line 1083 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3918 "parser.cc"
    break;

  case 252: // gadt_constrlist: "where" "vocurly" gadt_constrs close
#line 1084 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3924 "parser.cc"
    break;

  case 253: // gadt_constrlist: %empty
#line 1085 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = {};}
#line 3930 "parser.cc"
    break;

  case 254: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1087 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3936 "parser.cc"
    break;

  case 255: // gadt_constrs: gadt_constr
#line 1088 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3942 "parser.cc"
    break;

  case 256: // gadt_constr: optSemi con_list "::" sigtype
#line 1090 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),yystack_[0].value.as < Hs::LType > ());}
#line 3948 "parser.cc"
    break;

  case 257: // constrs: "=" constrs1
#line 1092 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 3954 "parser.cc"
    break;

  case 258: // constrs1: constrs1 "|" constr
#line 1094 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3960 "parser.cc"
    break;

  case 259: // constrs1: constr
#line 1095 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3966 "parser.cc"
    break;

  case 260: // constr: forall context_no_ops "=>" constr_stuff
#line 1097 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::LTypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::LType > ());}
#line 3972 "parser.cc"
    break;

  case 261: // constr: forall constr_stuff
#line 1098 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::LTypeVar> > (),{}, yystack_[0].value.as < Hs::LType > ());}
#line 3978 "parser.cc"
    break;

  case 262: // forall: "forall" tv_bndrs "."
#line 1100 "parser.y"
                                {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > ();}
#line 3984 "parser.cc"
    break;

  case 263: // forall: %empty
#line 1101 "parser.y"
                                {}
#line 3990 "parser.cc"
    break;

  case 264: // constr_stuff: btype_no_ops
#line 1103 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ());}
#line 3996 "parser.cc"
    break;

  case 265: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1104 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::type_apply({
                                                                          {yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},
                                                                          Hs::type_apply(yystack_[2].value.as < std::vector<Hs::LType> > ()),
                                                                          Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ())
                                                                       });}
#line 4006 "parser.cc"
    break;

  case 266: // fielddecls: %empty
#line 1110 "parser.y"
                                {}
#line 4012 "parser.cc"
    break;

  case 267: // fielddecls: fielddecls1
#line 1111 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 4018 "parser.cc"
    break;

  case 268: // fielddecls1: fielddecls1 "," fielddecl
#line 1113 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 4024 "parser.cc"
    break;

  case 269: // fielddecls1: fielddecl
#line 1114 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 4030 "parser.cc"
    break;

  case 270: // fielddecl: sig_vars "::" ctype
#line 1116 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ());}
#line 4036 "parser.cc"
    break;

  case 281: // decl_no_th: sigdecl
#line 1135 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4042 "parser.cc"
    break;

  case 282: // decl_no_th: infixexp rhs
#line 1137 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ValueDecl({yystack_[1].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ())};}
#line 4048 "parser.cc"
    break;

  case 283: // decl: decl_no_th
#line 1139 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4054 "parser.cc"
    break;

  case 284: // rhs: "=" exp wherebinds
#line 1143 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4060 "parser.cc"
    break;

  case 285: // rhs: gdrhs wherebinds
#line 1144 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 4066 "parser.cc"
    break;

  case 286: // gdrhs: gdrhs gdrh
#line 1146 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4072 "parser.cc"
    break;

  case 287: // gdrhs: gdrh
#line 1147 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4078 "parser.cc"
    break;

  case 288: // gdrh: "|" guardquals "=" exp
#line 1151 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4084 "parser.cc"
    break;

  case 289: // sigdecl: sig_vars "::" sigtypedoc
#line 1161 "parser.y"
                                  { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeSigDecl{yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ()}}; }
#line 4090 "parser.cc"
    break;

  case 290: // sigdecl: infix prec ops
#line 1162 "parser.y"
                         { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<Located<std::string>> > ()}}; }
#line 4096 "parser.cc"
    break;

  case 291: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1164 "parser.y"
                                                    {}
#line 4102 "parser.cc"
    break;

  case 292: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1165 "parser.y"
                                            { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::InlinePragma({yystack_[3].location,yystack_[3].value.as < std::string > ()},{yystack_[1].location,yystack_[1].value.as < std::string > ()})}; }
#line 4108 "parser.cc"
    break;

  case 293: // sigdecl: "{-# SCC" qvar "#-}"
#line 1166 "parser.y"
                              {}
#line 4114 "parser.cc"
    break;

  case 294: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1167 "parser.y"
                                     {}
#line 4120 "parser.cc"
    break;

  case 295: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1168 "parser.y"
                                                               {}
#line 4126 "parser.cc"
    break;

  case 296: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1169 "parser.y"
                                                                      {}
#line 4132 "parser.cc"
    break;

  case 297: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1170 "parser.y"
                                                     {}
#line 4138 "parser.cc"
    break;

  case 302: // exp: infixexp "::" sigtype
#line 1182 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[2].location+yystack_[0].location, Hs::TypedExp({yystack_[2].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[2].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::LType > ())}; }
#line 4144 "parser.cc"
    break;

  case 303: // exp: infixexp
#line 1183 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}; }
#line 4150 "parser.cc"
    break;

  case 304: // infixexp: exp10
#line 1187 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location,Hs::InfixExp({yystack_[0].value.as < Located<expression_ref> > ()})};}
#line 4156 "parser.cc"
    break;

  case 305: // infixexp: infixexp qop exp10
#line 1188 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < expression_ref > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4162 "parser.cc"
    break;

  case 306: // exp10: PREFIX_MINUS fexp
#line 1190 "parser.y"
                                        {yylhs.value.as < Located<expression_ref> > () = {yystack_[1].location+yystack_[0].location,Hs::InfixExp( { {yystack_[1].location,Hs::Neg()}, yystack_[0].value.as < Located<expression_ref> > ()} )};}
#line 4168 "parser.cc"
    break;

  case 307: // exp10: fexp
#line 1191 "parser.y"
                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4174 "parser.cc"
    break;

  case 310: // fexp: fexp aexp
#line 1199 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ApplyExp(yystack_[1].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4180 "parser.cc"
    break;

  case 311: // fexp: fexp "@" atype
#line 1200 "parser.y"
                                 {}
#line 4186 "parser.cc"
    break;

  case 312: // fexp: "static" aexp
#line 1201 "parser.y"
                                 {}
#line 4192 "parser.cc"
    break;

  case 313: // fexp: aexp
#line 1202 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4198 "parser.cc"
    break;

  case 314: // aexp: qvar TIGHT_INFIX_AT aexp
#line 1205 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::AsPattern({yystack_[2].location,Hs::Var(yystack_[2].value.as < std::string > ())},yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4204 "parser.cc"
    break;

  case 315: // aexp: PREFIX_TILDE aexp
#line 1206 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LazyPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4210 "parser.cc"
    break;

  case 316: // aexp: PREFIX_BANG aexp
#line 1207 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4216 "parser.cc"
    break;

  case 317: // aexp: "\\" apats1 "->" exp
#line 1208 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LambdaExp(yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4222 "parser.cc"
    break;

  case 318: // aexp: "let" binds "in" exp
#line 1209 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4228 "parser.cc"
    break;

  case 319: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1211 "parser.y"
                                                       {yylhs.value.as < Located<expression_ref> > () = {yystack_[7].location+yystack_[0].location,Hs::IfExp(yystack_[6].value.as < Located<expression_ref> > (),yystack_[3].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4234 "parser.cc"
    break;

  case 320: // aexp: "case" exp "of" altslist
#line 1213 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::CaseExp(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::Alts > ())}; }
#line 4240 "parser.cc"
    break;

  case 321: // aexp: "do" stmtlist
#line 1214 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Do(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4246 "parser.cc"
    break;

  case 322: // aexp: "mdo" stmtlist
#line 1215 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::MDo(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4252 "parser.cc"
    break;

  case 323: // aexp: aexp1
#line 1217 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4258 "parser.cc"
    break;

  case 324: // aexp1: aexp1 "{" fbinds "}"
#line 1220 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::RecordExp{yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<Hs::FieldBindings> > ()}}; }
#line 4264 "parser.cc"
    break;

  case 325: // aexp1: aexp1 TIGHT_INFIX_DOT field
#line 1221 "parser.y"
                                     { }
#line 4270 "parser.cc"
    break;

  case 326: // aexp1: aexp2
#line 1222 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > (); }
#line 4276 "parser.cc"
    break;

  case 327: // aexp2: qvar
#line 1225 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Var(yystack_[0].value.as < std::string > ())};}
#line 4282 "parser.cc"
    break;

  case 328: // aexp2: qcon
#line 1226 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Con(yystack_[0].value.as < std::string > ())};}
#line 4288 "parser.cc"
    break;

  case 329: // aexp2: literal
#line 1227 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[0].value.as < expression_ref > ()};}
#line 4294 "parser.cc"
    break;

  case 330: // aexp2: "(" texp ")"
#line 1228 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, unloc(yystack_[1].value.as < Located<expression_ref> > ())};}
#line 4300 "parser.cc"
    break;

  case 331: // aexp2: "(" tup_exprs ")"
#line 1229 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Tuple(yystack_[1].value.as < std::vector<Located<expression_ref>> > ())};}
#line 4306 "parser.cc"
    break;

  case 332: // aexp2: "(" projection ")"
#line 1230 "parser.y"
                              {}
#line 4312 "parser.cc"
    break;

  case 333: // aexp2: "[" list "]"
#line 1235 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[1].value.as < expression_ref > ()};}
#line 4318 "parser.cc"
    break;

  case 334: // aexp2: "_"
#line 1236 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::WildcardPattern()};}
#line 4324 "parser.cc"
    break;

  case 337: // texp: exp
#line 1245 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4330 "parser.cc"
    break;

  case 338: // texp: infixexp qop
#line 1246 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LeftSection ( {yystack_[1].value.as < Located<Hs::InfixExp> > ().loc, unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())}, {yystack_[0].location,yystack_[0].value.as < expression_ref > ()} )}; }
#line 4336 "parser.cc"
    break;

  case 339: // texp: qopm infixexp
#line 1247 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RightSection( {yystack_[1].location,yystack_[1].value.as < expression_ref > ()}, {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())} )}; }
#line 4342 "parser.cc"
    break;

  case 340: // tup_exprs: tup_exprs "," texp
#line 1252 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4348 "parser.cc"
    break;

  case 341: // tup_exprs: texp "," texp
#line 1253 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4354 "parser.cc"
    break;

  case 342: // list: texp
#line 1271 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List({yystack_[0].value.as < Located<expression_ref> > ()}); }
#line 4360 "parser.cc"
    break;

  case 343: // list: lexps
#line 1272 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List(yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4366 "parser.cc"
    break;

  case 344: // list: texp ".."
#line 1273 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4372 "parser.cc"
    break;

  case 345: // list: texp "," exp ".."
#line 1274 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4378 "parser.cc"
    break;

  case 346: // list: texp ".." exp
#line 1275 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4384 "parser.cc"
    break;

  case 347: // list: texp "," exp ".." exp
#line 1276 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < Located<expression_ref> > (), yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4390 "parser.cc"
    break;

  case 348: // list: texp "|" squals
#line 1277 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4396 "parser.cc"
    break;

  case 349: // lexps: lexps "," texp
#line 1279 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4402 "parser.cc"
    break;

  case 350: // lexps: texp "," texp
#line 1280 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4408 "parser.cc"
    break;

  case 351: // squals: squals "," qual
#line 1293 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4414 "parser.cc"
    break;

  case 352: // squals: qual
#line 1295 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4420 "parser.cc"
    break;

  case 353: // guardquals: guardquals1
#line 1305 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[0].value.as < std::vector<Located<expression_ref>> > ();}
#line 4426 "parser.cc"
    break;

  case 354: // guardquals1: guardquals1 "," qual
#line 1307 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > ();yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4432 "parser.cc"
    break;

  case 355: // guardquals1: qual
#line 1308 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4438 "parser.cc"
    break;

  case 356: // altslist: "{" alts "}"
#line 1311 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4444 "parser.cc"
    break;

  case 357: // altslist: "vocurly" alts close
#line 1312 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4450 "parser.cc"
    break;

  case 358: // altslist: "{" "}"
#line 1313 "parser.y"
                                 {}
#line 4456 "parser.cc"
    break;

  case 359: // altslist: "vocurly" close
#line 1314 "parser.y"
                                 {}
#line 4462 "parser.cc"
    break;

  case 360: // alts: alts1
#line 1316 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4468 "parser.cc"
    break;

  case 361: // alts: ";" alts
#line 1317 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4474 "parser.cc"
    break;

  case 362: // alts1: alts1 ";" alt
#line 1319 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4480 "parser.cc"
    break;

  case 363: // alts1: alts1 ";"
#line 1320 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4486 "parser.cc"
    break;

  case 364: // alts1: alt
#line 1321 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4492 "parser.cc"
    break;

  case 365: // alt: pat alt_rhs
#line 1323 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 4498 "parser.cc"
    break;

  case 366: // alt_rhs: "->" exp wherebinds
#line 1325 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4504 "parser.cc"
    break;

  case 367: // alt_rhs: gdpats wherebinds
#line 1326 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4510 "parser.cc"
    break;

  case 368: // gdpats: gdpats gdpat
#line 1328 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4516 "parser.cc"
    break;

  case 369: // gdpats: gdpat
#line 1329 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4522 "parser.cc"
    break;

  case 370: // gdpat: "|" guardquals "->" exp
#line 1338 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4528 "parser.cc"
    break;

  case 371: // pat: exp
#line 1340 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4534 "parser.cc"
    break;

  case 372: // bindpat: exp
#line 1342 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4540 "parser.cc"
    break;

  case 373: // apat: aexp
#line 1344 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4546 "parser.cc"
    break;

  case 374: // apats1: apats1 apat
#line 1346 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4552 "parser.cc"
    break;

  case 375: // apats1: apat
#line 1347 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4558 "parser.cc"
    break;

  case 376: // stmtlist: "{" stmts "}"
#line 1350 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4564 "parser.cc"
    break;

  case 377: // stmtlist: "vocurly" stmts close
#line 1351 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4570 "parser.cc"
    break;

  case 378: // stmts: stmts ";" stmt
#line 1353 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4576 "parser.cc"
    break;

  case 379: // stmts: stmts ";"
#line 1354 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 4582 "parser.cc"
    break;

  case 380: // stmts: stmt
#line 1355 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4588 "parser.cc"
    break;

  case 381: // stmts: %empty
#line 1356 "parser.y"
                       {}
#line 4594 "parser.cc"
    break;

  case 382: // stmt: qual
#line 1361 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4600 "parser.cc"
    break;

  case 383: // stmt: "rec" stmtlist
#line 1362 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ())};}
#line 4606 "parser.cc"
    break;

  case 384: // qual: bindpat "<-" exp
#line 1364 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::PatQual(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4612 "parser.cc"
    break;

  case 385: // qual: exp
#line 1365 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::SimpleQual(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4618 "parser.cc"
    break;

  case 386: // qual: "let" binds
#line 1366 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ())};}
#line 4624 "parser.cc"
    break;

  case 387: // fbinds: fbinds1
#line 1371 "parser.y"
                        {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > ();}
#line 4630 "parser.cc"
    break;

  case 388: // fbinds: %empty
#line 1372 "parser.y"
                        {}
#line 4636 "parser.cc"
    break;

  case 389: // fbinds1: fbind "," fbinds1
#line 1374 "parser.y"
                            {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > (); unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).insert(unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).begin(), *yystack_[2].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4642 "parser.cc"
    break;

  case 390: // fbinds1: fbind
#line 1375 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).push_back(*yystack_[0].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4648 "parser.cc"
    break;

  case 391: // fbinds1: ".."
#line 1376 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).dotdot = true; yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4654 "parser.cc"
    break;

  case 392: // fbind: qvar "=" texp
#line 1378 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[2].value.as < std::string > ())}, yystack_[0].value.as < Located<expression_ref> > ())}};}
#line 4660 "parser.cc"
    break;

  case 393: // fbind: qvar
#line 1379 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[0].value.as < std::string > ())})}};}
#line 4666 "parser.cc"
    break;

  case 394: // fbind: field TIGHT_INFIX_DOT fieldToUpdate "=" texp
#line 1380 "parser.y"
                                                      {}
#line 4672 "parser.cc"
    break;

  case 395: // fbind: field TIGHT_INFIX_DOT fieldToUpdate
#line 1381 "parser.y"
                                                      {}
#line 4678 "parser.cc"
    break;

  case 398: // qcon: gen_qcon
#line 1417 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4684 "parser.cc"
    break;

  case 399: // qcon: sysdcon
#line 1418 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4690 "parser.cc"
    break;

  case 400: // gen_qcon: qconid
#line 1420 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4696 "parser.cc"
    break;

  case 401: // gen_qcon: "(" qconsym ")"
#line 1421 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4702 "parser.cc"
    break;

  case 402: // con: conid
#line 1423 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4708 "parser.cc"
    break;

  case 403: // con: "(" consym ")"
#line 1424 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4714 "parser.cc"
    break;

  case 404: // con: sysdcon
#line 1425 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4720 "parser.cc"
    break;

  case 405: // con_list: con_list "," con
#line 1427 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4726 "parser.cc"
    break;

  case 406: // con_list: con
#line 1428 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4732 "parser.cc"
    break;

  case 407: // sysdcon_no_list: "(" ")"
#line 1430 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 4738 "parser.cc"
    break;

  case 408: // sysdcon_no_list: "(" commas ")"
#line 1431 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4744 "parser.cc"
    break;

  case 409: // sysdcon_no_list: "(#" "#)"
#line 1432 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 4750 "parser.cc"
    break;

  case 410: // sysdcon_no_list: "(#" commas "#)"
#line 1433 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4756 "parser.cc"
    break;

  case 411: // sysdcon: sysdcon_no_list
#line 1435 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4762 "parser.cc"
    break;

  case 412: // sysdcon: "[" "]"
#line 1436 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 4768 "parser.cc"
    break;

  case 413: // conop: consym
#line 1438 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4774 "parser.cc"
    break;

  case 414: // conop: "`" conid "`"
#line 1439 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4780 "parser.cc"
    break;

  case 415: // qconop: qconsym
#line 1441 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4786 "parser.cc"
    break;

  case 416: // qconop: "`" qconid "`"
#line 1442 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4792 "parser.cc"
    break;

  case 417: // gtycon: ntgtycon
#line 1445 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4798 "parser.cc"
    break;

  case 418: // gtycon: "(" ")"
#line 1446 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 4804 "parser.cc"
    break;

  case 419: // gtycon: "(#" "#)"
#line 1447 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 4810 "parser.cc"
    break;

  case 420: // ntgtycon: oqtycon
#line 1449 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4816 "parser.cc"
    break;

  case 421: // ntgtycon: "(" commas ")"
#line 1450 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4822 "parser.cc"
    break;

  case 422: // ntgtycon: "(#" commas "#)"
#line 1451 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4828 "parser.cc"
    break;

  case 423: // ntgtycon: "(" "->" ")"
#line 1452 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 4834 "parser.cc"
    break;

  case 424: // ntgtycon: "[" "]"
#line 1453 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 4840 "parser.cc"
    break;

  case 425: // oqtycon: qtycon
#line 1455 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4846 "parser.cc"
    break;

  case 426: // oqtycon: "(" qtyconsym ")"
#line 1456 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4852 "parser.cc"
    break;

  case 427: // oqtycon_no_varcon: qtycon
#line 1458 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4858 "parser.cc"
    break;

  case 428: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1459 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4864 "parser.cc"
    break;

  case 429: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1460 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4870 "parser.cc"
    break;

  case 430: // oqtycon_no_varcon: "(" ":" ")"
#line 1461 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 4876 "parser.cc"
    break;

  case 431: // qtyconop: qtyconsym
#line 1464 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4882 "parser.cc"
    break;

  case 432: // qtyconop: "`" qtycon "`"
#line 1465 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4888 "parser.cc"
    break;

  case 433: // qtycondoc: qtycon
#line 1467 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4894 "parser.cc"
    break;

  case 434: // qtycon: "QCONID"
#line 1469 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4900 "parser.cc"
    break;

  case 435: // qtycon: tycon
#line 1470 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4906 "parser.cc"
    break;

  case 436: // tycon: "CONID"
#line 1474 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4912 "parser.cc"
    break;

  case 437: // qtyconsym: "QCONSYM"
#line 1476 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4918 "parser.cc"
    break;

  case 438: // qtyconsym: "QVARSYM"
#line 1477 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4924 "parser.cc"
    break;

  case 439: // qtyconsym: tyconsym
#line 1478 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4930 "parser.cc"
    break;

  case 440: // tyconsym: "CONSYM"
#line 1480 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4936 "parser.cc"
    break;

  case 441: // tyconsym: "VARSYM"
#line 1481 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4942 "parser.cc"
    break;

  case 442: // tyconsym: ":"
#line 1482 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4948 "parser.cc"
    break;

  case 443: // tyconsym: "-"
#line 1483 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 4954 "parser.cc"
    break;

  case 444: // op: varop
#line 1488 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4960 "parser.cc"
    break;

  case 445: // op: conop
#line 1489 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4966 "parser.cc"
    break;

  case 446: // varop: varsym
#line 1491 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4972 "parser.cc"
    break;

  case 447: // varop: "`" varid "`"
#line 1492 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4978 "parser.cc"
    break;

  case 448: // qop: qvarop
#line 1494 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4984 "parser.cc"
    break;

  case 449: // qop: qconop
#line 1495 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4990 "parser.cc"
    break;

  case 450: // qopm: qvaropm
#line 1498 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4996 "parser.cc"
    break;

  case 451: // qopm: qconop
#line 1499 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 5002 "parser.cc"
    break;

  case 452: // qvarop: qvarsym
#line 1504 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5008 "parser.cc"
    break;

  case 453: // qvarop: "`" qvarid "`"
#line 1505 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5014 "parser.cc"
    break;

  case 454: // qvaropm: qvarsym_no_minus
#line 1507 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 5020 "parser.cc"
    break;

  case 455: // qvaropm: "`" qvarid "`"
#line 1508 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5026 "parser.cc"
    break;

  case 456: // tyvar: tyvarid
#line 1512 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5032 "parser.cc"
    break;

  case 457: // tyvarop: "`" tyvarid "`"
#line 1514 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5038 "parser.cc"
    break;

  case 458: // tyvarid: "VARID"
#line 1516 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5044 "parser.cc"
    break;

  case 459: // tyvarid: special_id
#line 1517 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5050 "parser.cc"
    break;

  case 460: // tyvarid: "unsafe"
#line 1518 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 5056 "parser.cc"
    break;

  case 461: // tyvarid: "safe"
#line 1519 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 5062 "parser.cc"
    break;

  case 462: // tyvarid: "interruptible"
#line 1520 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 5068 "parser.cc"
    break;

  case 463: // var: varid
#line 1523 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5074 "parser.cc"
    break;

  case 464: // var: "(" varsym ")"
#line 1524 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5080 "parser.cc"
    break;

  case 465: // qvar: qvarid
#line 1526 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5086 "parser.cc"
    break;

  case 466: // qvar: "(" varsym ")"
#line 1527 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5092 "parser.cc"
    break;

  case 467: // qvar: "(" qvarsym1 ")"
#line 1528 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5098 "parser.cc"
    break;

  case 468: // field: varid
#line 1530 "parser.y"
       { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5104 "parser.cc"
    break;

  case 469: // qvarid: varid
#line 1532 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5110 "parser.cc"
    break;

  case 470: // qvarid: "QVARID"
#line 1533 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5116 "parser.cc"
    break;

  case 471: // varid: "VARID"
#line 1535 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5122 "parser.cc"
    break;

  case 472: // varid: special_id
#line 1536 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5128 "parser.cc"
    break;

  case 473: // varid: "unsafe"
#line 1537 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 5134 "parser.cc"
    break;

  case 474: // varid: "safe"
#line 1538 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 5140 "parser.cc"
    break;

  case 475: // varid: "interruptible"
#line 1539 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 5146 "parser.cc"
    break;

  case 476: // varid: "forall"
#line 1540 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 5152 "parser.cc"
    break;

  case 477: // varid: "family"
#line 1541 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 5158 "parser.cc"
    break;

  case 478: // varid: "role"
#line 1542 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 5164 "parser.cc"
    break;

  case 479: // qvarsym: varsym
#line 1544 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5170 "parser.cc"
    break;

  case 480: // qvarsym: qvarsym1
#line 1545 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5176 "parser.cc"
    break;

  case 481: // qvarsym_no_minus: varsym_no_minus
#line 1547 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5182 "parser.cc"
    break;

  case 482: // qvarsym_no_minus: qvarsym1
#line 1548 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5188 "parser.cc"
    break;

  case 483: // qvarsym1: "QVARSYM"
#line 1550 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5194 "parser.cc"
    break;

  case 484: // varsym: varsym_no_minus
#line 1552 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5200 "parser.cc"
    break;

  case 485: // varsym: "-"
#line 1553 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 5206 "parser.cc"
    break;

  case 486: // varsym_no_minus: "VARSYM"
#line 1555 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5212 "parser.cc"
    break;

  case 487: // varsym_no_minus: special_sym
#line 1556 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5218 "parser.cc"
    break;

  case 488: // special_id: "as"
#line 1558 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 5224 "parser.cc"
    break;

  case 489: // special_id: "qualified"
#line 1559 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 5230 "parser.cc"
    break;

  case 490: // special_id: "hiding"
#line 1560 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 5236 "parser.cc"
    break;

  case 491: // special_id: "export"
#line 1561 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 5242 "parser.cc"
    break;

  case 492: // special_id: "label"
#line 1562 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 5248 "parser.cc"
    break;

  case 493: // special_id: "dynamic"
#line 1563 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 5254 "parser.cc"
    break;

  case 494: // special_id: "stdcall"
#line 1564 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 5260 "parser.cc"
    break;

  case 495: // special_id: "ccall"
#line 1565 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 5266 "parser.cc"
    break;

  case 496: // special_id: "capi"
#line 1566 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 5272 "parser.cc"
    break;

  case 497: // special_id: "prim"
#line 1567 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 5278 "parser.cc"
    break;

  case 498: // special_id: "javascript"
#line 1568 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 5284 "parser.cc"
    break;

  case 499: // special_id: "group"
#line 1569 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 5290 "parser.cc"
    break;

  case 500: // special_id: "stock"
#line 1570 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 5296 "parser.cc"
    break;

  case 501: // special_id: "anyclass"
#line 1571 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 5302 "parser.cc"
    break;

  case 502: // special_id: "via"
#line 1572 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 5308 "parser.cc"
    break;

  case 503: // special_id: "unit"
#line 1573 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 5314 "parser.cc"
    break;

  case 504: // special_id: "dependency"
#line 1574 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 5320 "parser.cc"
    break;

  case 505: // special_id: "signature"
#line 1575 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 5326 "parser.cc"
    break;

  case 506: // special_sym: "."
#line 1577 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 5332 "parser.cc"
    break;

  case 507: // special_sym: "*"
#line 1578 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 5338 "parser.cc"
    break;

  case 508: // qconid: conid
#line 1582 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5344 "parser.cc"
    break;

  case 509: // qconid: "QCONID"
#line 1583 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5350 "parser.cc"
    break;

  case 510: // conid: "CONID"
#line 1585 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5356 "parser.cc"
    break;

  case 511: // qconsym: consym
#line 1587 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5362 "parser.cc"
    break;

  case 512: // qconsym: "QCONSYM"
#line 1588 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5368 "parser.cc"
    break;

  case 513: // consym: "CONSYM"
#line 1590 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5374 "parser.cc"
    break;

  case 514: // consym: ":"
#line 1591 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 5380 "parser.cc"
    break;

  case 515: // literal: "CHAR"
#line 1595 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 5386 "parser.cc"
    break;

  case 516: // literal: "STRING"
#line 1596 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 5392 "parser.cc"
    break;

  case 517: // literal: "INTEGER"
#line 1597 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < integer > ()});}
#line 5398 "parser.cc"
    break;

  case 518: // literal: "RATIONAL"
#line 1598 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Floating{yystack_[0].value.as < rational > ()});}
#line 5404 "parser.cc"
    break;

  case 519: // literal: "PRIMINTEGER"
#line 1599 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < integer > ()});}
#line 5410 "parser.cc"
    break;

  case 521: // close: error
#line 1607 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 5416 "parser.cc"
    break;

  case 522: // modid: "CONID"
#line 1611 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5422 "parser.cc"
    break;

  case 523: // modid: "QCONID"
#line 1612 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5428 "parser.cc"
    break;

  case 524: // commas: commas ","
#line 1614 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 5434 "parser.cc"
    break;

  case 525: // commas: ","
#line 1615 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 5440 "parser.cc"
    break;


#line 5444 "parser.cc"

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


  const short parser::yypact_ninf_ = -714;

  const short parser::yytable_ninf_ = -485;

  const short
  parser::yypact_[] =
  {
      47,   166,  -714,    69,  -714,  -714,  -714,  -714,  -714,   448,
       1,    28,  -714,    81,     2,     2,    67,  -714,  -714,  -714,
    -714,   198,  -714,  -714,  -714,   132,  -714,   208,   220,  1434,
     284,   295,   221,  -714,   877,  -714,   -10,  -714,  -714,  -714,
     166,  -714,   166,  -714,  -714,  -714,  -714,  -714,  -714,  -714,
    -714,  -714,  -714,  -714,  -714,  -714,  -714,  -714,  -714,  -714,
    -714,  -714,  -714,  -714,  -714,   154,  -714,  -714,  -714,  -714,
    -714,   231,    70,  -714,   276,  -714,  -714,  -714,  -714,  -714,
    -714,  -714,   214,  -714,   166,  -714,   283,  -714,  2558,  4547,
     369,   290,   224,  2558,  -714,  -714,  -714,   388,   347,  -714,
    3663,   395,   224,  3243,  4988,   373,  3243,  3243,  2832,  3243,
    1873,  1736,   116,   317,  -714,  -714,  -714,  -714,  -714,  -714,
    -714,    30,   317,   299,   221,  -714,  -714,  -714,  -714,   104,
     107,  -714,  -714,   282,  -714,  2969,  -714,   263,  -714,  -714,
    -714,  -714,  -714,  -714,   356,   110,  -714,  -714,  -714,  -714,
     314,  -714,  -714,   345,  -714,  -714,  -714,  -714,   352,  -714,
     365,   380,   382,  -714,  -714,  -714,  4648,  -714,  4685,  -714,
    -714,  -714,  -714,   478,  -714,  1736,   477,   376,  -714,  -714,
    -714,  4547,  4547,  -714,  5089,  3750,  3356,   402,  -714,   423,
     435,  -714,   658,  -714,  3938,  -714,  -714,  -714,  -714,  -714,
    -714,  -714,  4547,   425,  -714,  2284,  2284,  -714,   424,   466,
     467,   471,   473,  4025,  1300,  1300,  -714,   536,  4547,  4547,
     119,   474,   868,   122,   196,  -714,  -714,   254,    -7,   443,
     -12,  -714,   123,  -714,  -714,  -714,  -714,  3106,  -714,  2969,
    -714,  -714,  -714,  4850,  -714,  -714,  -714,   376,   141,   451,
     437,  -714,  2558,  -714,  -714,  -714,  -714,  -714,  -714,  5220,
    -714,  -714,   145,    98,   173,   380,   449,   460,   464,   246,
    -714,   309,    -4,  4988,  -714,  4025,  4988,  4988,  -714,   647,
     283,   488,   444,  4547,  4025,  5089,  2558,  2695,  4850,  -714,
      24,  -714,  -714,  2558,  -714,  -714,  -714,  -714,  4547,  -714,
    5220,  4951,  3243,  -714,  -714,  -714,  -714,  -714,  -714,  -714,
     472,   475,   476,  -714,   481,    81,   166,    36,   364,  4025,
    -714,  -714,   278,   126,   492,   480,  -714,  -714,  -714,  -714,
     470,   513,   510,  -714,  -714,   490,  -714,  -714,  -714,  -714,
    -714,  -714,   494,   485,   499,  -714,   267,   311,  -714,   586,
    4547,  4025,  4887,  4547,  -714,  -714,  -714,  4547,  -714,  -714,
     534,  4025,   347,   224,   530,   538,   171,  -714,  -714,    43,
    -714,   594,  -714,  -714,  -714,  -714,  -714,  -714,   600,   177,
    -714,  -714,   282,    49,  2558,  -714,   552,   436,  4025,   -24,
    4025,  -714,  -714,   500,  -714,   559,   524,   683,   373,   561,
    2558,  -714,   519,   535,  2558,  2558,  2695,  2010,  -714,  2010,
     491,  -714,  -714,  5220,  -714,  -714,  2010,  -714,  2010,   130,
    -714,  -714,  -714,  -714,   515,   545,   579,   583,   584,   588,
    5052,   550,  -714,  -714,  -714,  -714,  -714,  4112,    -6,   438,
    -714,  -714,  -714,  -714,   643,   592,   555,  -714,   564,   347,
    -714,  -714,  -714,  -714,  -714,  -714,   574,  -714,   567,   608,
     591,   593,  -714,  -714,  -714,  4786,  -714,  -714,  -714,   581,
    1471,  -714,  -714,  2147,  1599,  -714,  -714,   582,  4025,  -714,
    5089,  5257,  -714,  4025,  4025,  -714,  -714,  4025,  -714,  -714,
    -714,   577,  -714,   806,   367,  -714,  -714,  -714,   578,   580,
     446,  -714,  4025,  -714,  -714,   589,   585,   536,  -714,  2558,
    -714,  2284,  -714,  2558,   370,  -714,  -714,  1300,  -714,  -714,
    4025,  4025,  5366,   613,  -714,  -714,   291,  -714,  -714,  5089,
    -714,  -714,   590,   696,   331,  -714,  -714,  -714,  -714,  -714,
    -714,  -714,  -714,   587,  -714,   633,  -714,  -714,  -714,  -714,
    -714,   605,  -714,  -714,  -714,  4025,  4025,   595,   609,   647,
    -714,   438,   642,  -714,  -714,   659,  4025,   710,   714,   737,
    -714,  2558,  2695,  -714,  -714,  -714,  4951,  2010,  5220,  -714,
    1471,   166,  -714,   276,   634,    89,  -714,  -714,  2421,  -714,
     653,   630,  -714,   351,    81,  -714,  -714,  -714,  -714,  4025,
    5460,  5460,  -714,  -714,  -714,  -714,  -714,   646,  -714,  -714,
    -714,  1163,  1163,  -714,  -714,  -714,  -714,  -714,  4025,  -714,
    -714,   424,  1020,  1020,  -714,  -714,  -714,  -714,  -714,  5460,
     735,   684,  -714,  -714,  2695,  2558,  -714,  -714,    -3,     6,
    -714,  -714,  -714,  5126,   714,   737,  4547,  -714,  -714,  -714,
     687,  -714,  4547,   384,   737,    68,  -714,   737,  -714,  -714,
    -714,  -714,  -714,    21,  -714,   655,  -714,  -714,  -714,  4749,
    -714,  -714,  -714,  2558,  2695,  2558,  -714,    51,  -714,  -714,
    -714,    32,   694,  -714,  -714,  5460,   742,  3851,  -714,  -714,
     203,  -714,    56,  -714,   769,  -714,   762,  -714,   762,  -714,
     240,  -714,    58,  -714,   700,   385,  -714,  4025,  -714,  -714,
    -714,  4025,  -714,  4547,  4547,   737,  -714,  -714,  5291,   710,
     699,  3459,  -714,  -714,  -714,   424,   424,  -714,  -714,  -714,
    -714,  4199,   248,   740,  -714,  -714,  -714,  2010,  5220,  -714,
    -714,  -714,   712,   643,  -714,  -714,  4025,  -714,  4025,  -714,
    4547,  4547,  4547,  -714,   447,  -714,  1163,  -714,  2558,  -714,
    4547,   488,  -714,  1020,  -714,  5460,  4286,  4373,  -714,  -714,
    -714,  -714,   705,   446,  -714,  -714,  -714,  4547,   675,  -714,
    4547,   244,  -714,   373,    59,  -714,  -714,   681,   688,  -714,
    4547,  -714,  -714,  -714,  2558,  -714,   697,   690,   534,  -714,
     450,  4025,  4460,  -714,  -714,  -714,  -714,  4112,  -714,  5460,
    -714,   701,   250,  -714,    81,    63,  4547,  3560,  -714,  4547,
    -714,   424,   140,  -714,  4547,  -714,  -714,  -714,  -714,  -714,
    -714,  5400,  -714,  -714,  3356,   720,   721,   438,  -714,  -714,
    -714,  4547,  -714,  -714,  -714,  -714,  4025,  -714,   694,  5460,
     714,   737,  -714,  -714,  -714,   737,  -714,  -714
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    40,     0,     2,    40,     4,   522,   523,     8,
       0,    43,     1,     0,     0,     0,    18,    11,    39,    13,
      16,    70,   521,   520,    12,   187,   183,     0,     0,    20,
       0,    46,    41,    15,    14,   186,     0,     6,     7,   488,
       0,   490,     0,   489,   476,   491,   492,   493,   474,   475,
     473,   477,   478,   494,   495,   496,   497,   498,   499,   500,
     501,   502,   503,   505,   504,     0,   471,   436,   470,   434,
      22,     0,    19,    24,    28,    36,   427,   435,    35,   465,
     469,   472,     0,    45,     0,    38,    42,   334,     0,     0,
     131,   133,     0,     0,    63,    64,    65,    98,     0,   132,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   298,   510,   509,   515,   516,   517,   518,
     519,   298,   298,    61,    68,    71,    72,    73,    74,   153,
       0,    77,   281,    78,   304,   307,   313,   323,   326,   328,
     398,   411,   399,   193,   327,   469,   400,   508,   329,   184,
       0,    27,    26,     0,   485,   507,   506,   486,     0,   483,
       0,     0,     0,   484,   487,    17,     0,    21,    31,    25,
      40,    40,     3,    48,    37,     0,     0,   303,   461,   462,
     460,     0,     0,   221,   266,     0,     0,     0,   458,   243,
       0,   146,   204,   207,   208,   212,   219,   420,   425,   220,
     456,   459,     0,     0,   134,   381,   381,   321,   309,     0,
       0,     0,     0,     0,   177,   177,   180,     0,     0,     0,
       0,     0,   204,   420,     0,   322,   312,     0,     0,     0,
       0,   406,   188,   404,   402,   373,   375,     0,   315,   306,
     316,   514,   412,     0,   513,   512,   337,   303,   342,     0,
     343,   451,     0,   450,   454,   482,   481,   415,   511,     0,
     407,   525,     0,     0,     0,   482,     0,   481,   415,     0,
     409,     0,     0,     0,   299,     0,     0,     0,    62,     0,
      69,   153,     0,     0,     0,     0,     0,     0,     0,   282,
     182,   287,   449,     0,   448,   452,   480,   479,     0,   310,
       0,   388,     0,   185,   430,   429,   428,   467,   466,    23,
       0,     0,    32,    34,     0,     0,     0,    50,     0,     0,
     223,   222,     0,     0,     0,   267,   269,   463,   237,   424,
       0,   196,     0,   200,   442,     0,   443,   225,   441,   440,
     438,   437,   234,     0,     0,   439,     0,     0,   249,   163,
       0,     0,     0,     0,   216,   431,   217,     0,   213,   215,
     137,   233,     0,     0,   385,     0,     0,   380,   382,     0,
     308,     0,    95,    94,    96,    97,   229,   190,   173,     0,
     283,   176,     0,     0,     0,    91,     0,   139,     0,     0,
       0,    79,    80,     0,   293,     0,     0,     0,     0,     0,
       0,   374,     0,     0,   338,   344,     0,     0,   333,     0,
     339,   336,   468,     0,   332,   330,     0,   331,     0,   466,
     401,   408,   524,   410,     0,     0,     0,     0,     0,     0,
       0,   290,   445,    67,   444,   446,   413,     0,     0,   135,
     289,   201,   191,   192,   182,     0,   353,   355,     0,     0,
     285,   286,   305,   311,   325,   391,     0,   387,   390,   393,
       0,   469,   314,    30,    29,     0,     9,    10,    47,     0,
      54,    44,    49,     0,     0,   320,   302,     0,     0,   224,
       0,     0,   227,     0,     0,   423,   228,     0,   426,   421,
     422,   244,   246,     0,     0,    81,   145,   205,     0,     0,
     209,   214,     0,    86,   234,     0,   232,   386,   383,     0,
     376,   379,   377,     0,     0,    90,   178,   175,   179,   318,
       0,     0,     0,   103,   250,    87,     0,    88,    82,     0,
     294,   403,     0,     0,     0,   189,   417,   405,   291,   317,
     455,   416,   346,   348,   352,   337,   350,   349,   335,   341,
     340,     0,   300,   292,   297,     0,     0,     0,     0,     0,
     237,   135,     0,   150,   152,     0,     0,   263,   253,   271,
     284,     0,     0,   453,   181,   324,     0,     0,     0,    33,
      54,     0,    56,    28,     0,    53,    58,   358,     0,   371,
       0,   360,   364,     0,     0,   359,   464,   270,   268,     0,
       0,     0,   236,   238,   241,   197,   199,   235,   249,   249,
     248,   159,   159,   162,   432,   457,   138,    75,     0,   384,
     378,   309,   169,   169,   172,   174,   118,   140,   141,     0,
     108,     0,   418,   419,     0,   345,   301,   194,     0,     0,
     447,   414,    66,     0,   253,   271,     0,   151,   136,   237,
     257,   259,     0,     0,   271,     0,    83,   272,   274,   288,
     354,   389,   392,   395,   397,     0,    60,    59,    51,     0,
      55,   361,   356,   363,     0,     0,   365,   182,   369,   357,
     198,     0,     0,   226,   245,   247,   124,     0,   154,   158,
       0,   155,     0,   235,     0,   131,   126,   164,   126,   168,
       0,   165,     0,   104,     0,     0,    85,     0,   351,   347,
     295,     0,   296,     0,     0,   271,    92,   149,     0,   263,
       0,   264,   210,   218,   261,   309,   309,    84,   101,    99,
     100,     0,     0,   275,   278,   433,   273,     0,     0,    52,
      57,   362,     0,   182,   367,   368,     0,   239,     0,   125,
       0,     0,     0,   122,   142,   160,   157,   161,     0,   127,
       0,   153,   170,   167,   171,     0,   117,   117,   109,    76,
     195,   148,     0,   202,    93,   262,   258,     0,     0,   211,
       0,     0,   255,     0,     0,   279,   206,   230,     0,   276,
       0,   277,   394,   396,     0,   366,     0,     0,   137,   123,
     142,     0,     0,   120,   156,   319,   128,     0,   166,   105,
     107,     0,     0,   116,     0,     0,     0,   264,   260,   265,
     251,   309,     0,   252,     0,   280,   102,   370,   240,   242,
     119,     0,   121,   143,     0,     0,   220,   135,   106,   112,
     110,   115,   113,   111,   147,   254,     0,   231,   220,     0,
     253,   271,   114,   256,   144,   271,   129,   130
  };

  const short
  parser::yypgoto_[] =
  {
    -714,  -714,  -714,  -714,  -714,  -714,  -714,    53,  -714,  -714,
    -714,  -714,   651,   225,  -714,  -714,    -8,   689,  -714,  -714,
    -714,  -714,  -714,  -714,  -714,  -714,   243,  -714,   151,  -714,
    -714,  -714,  -714,  -714,  -714,  -714,  -714,  -714,  -714,  -714,
    -714,  -714,  -714,  -714,  -714,   -25,  -714,  -714,  -714,    61,
    -193,  -714,  -714,   131,  -714,   797,  -714,  -525,    34,  -714,
      33,   551,    35,  -262,    79,   235,  -714,  -714,    80,   228,
    -714,  -714,   637,  -714,  -278,  -406,   839,  -714,  -714,  -282,
     148,  -149,   302,  -154,    54,  -714,   -83,  -714,   -85,  -714,
       3,  -714,  -387,  -714,  -714,  -714,  -626,  -172,   596,    40,
    -714,   504,  -507,   344,  -713,  -714,  -714,   259,   260,  -455,
    -598,   142,    52,  -520,  -714,   153,  -714,    93,  -714,  -714,
     396,  -600,  -714,   222,   146,   843,  -181,  -714,  -714,   612,
    -714,   413,  -714,   303,    29,  -232,  -182,   778,    38,  -714,
    -714,  -714,  -102,  -714,  -714,  -714,  -714,   230,  -714,  -714,
    -419,  -714,   245,  -714,  -714,   249,  -714,  -714,   661,  -714,
     -60,   716,   412,  -247,  -714,   349,  -714,  -714,  -714,  -714,
     537,   156,  -714,  -100,  -649,  -104,  -714,   539,   -80,  -714,
    -714,  -714,   -11,  -714,  -190,  -714,   383,  -714,   698,  -714,
    -714,  -714,  -266,  -714,  -325,  -242,    50,  -235,  -156,   -31,
    -714,  -714,   -13,   -26,   -94,   -89,  -714,   -91,   -98,   -81,
    -217,  -714,  -292,   -28,  -111
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   172,     6,    10,    19,    30,
      71,    72,    73,   169,   311,   312,   583,    86,    11,    20,
      21,    32,    84,   317,   471,   472,   584,   585,   586,   279,
     123,   431,    33,    34,   124,   393,   125,   126,   127,   220,
     128,   213,   732,   791,   630,   703,   809,   706,   768,   812,
     813,   688,   750,   760,   697,   698,   203,   568,   503,   523,
     803,   189,   561,   283,   689,   690,   613,   495,   699,   700,
     624,   515,   379,   216,   217,   450,    27,    36,   399,   376,
     440,   130,   638,   342,   524,   442,   332,   720,   333,   787,
     192,   193,   721,   194,   358,   353,   722,   195,   378,   788,
     505,   343,   481,   602,   603,   349,   491,   492,   493,   525,
     654,   781,   782,   569,   650,   651,   652,   724,   324,   325,
     326,   656,   657,   658,   733,   380,   691,   289,   290,   291,
     132,   273,   274,   246,   177,   134,   783,   135,   136,   137,
     138,   262,   263,   264,   249,   250,   543,   445,   446,   475,
     590,   591,   592,   676,   677,   678,   593,   365,   236,   237,
     207,   366,   367,   368,   456,   457,   458,   663,   139,   140,
     231,   232,   141,   142,   432,   251,   535,   196,   197,    75,
     354,   734,   198,    77,   344,   345,   433,   434,   293,   252,
     294,   253,   199,   356,   200,   143,   144,   460,    79,    80,
     295,   254,   255,   297,   163,    81,   164,   146,   147,   257,
     258,   148,    24,     9,   269
  };

  const short
  parser::yytable_[] =
  {
     201,   271,   355,   145,   191,   233,   190,   234,   248,   320,
     321,   201,   151,   396,   152,   221,   256,   267,    76,   437,
     223,    74,   359,   467,   411,   385,   371,   499,   605,   292,
     268,   330,   355,   381,   381,   323,   644,   476,   570,   162,
     447,   645,   225,   443,    22,   716,   715,   616,   469,   275,
      22,   449,   161,   643,   727,   594,   173,    22,    13,    22,
      22,   452,   436,   133,    22,   454,   627,   241,     1,    12,
     394,   564,   780,   292,   710,   346,   347,   512,   449,    78,
     204,   267,    22,   712,   507,   266,   424,   402,   526,   835,
     728,   518,   201,   201,   268,   779,   201,   201,   265,   149,
      67,   260,   737,   222,    69,   201,    17,   261,   287,   150,
      25,   648,   746,   201,   244,   774,   711,   360,   835,   729,
     296,   730,   738,   281,   201,   711,   453,   395,   565,   201,
     201,   425,   448,   386,   387,   674,    26,   747,   272,   247,
     247,   226,   718,   292,   235,   238,    18,   240,   470,   162,
      23,     2,   403,   327,   228,    76,    23,    76,    74,   544,
     313,   511,   265,    23,   296,    23,    23,   517,   780,   671,
      23,   574,   452,   299,   756,   282,   763,   821,   548,    29,
     731,   841,   595,   145,   145,   501,   201,   284,    23,   166,
    -463,   779,    67,   779,   201,   201,    69,   403,   191,   388,
     190,   162,   -89,   397,   247,   222,   478,   504,   669,   201,
    -464,   415,    31,   167,   161,   604,    78,   416,    78,   405,
     846,   222,   222,   314,   315,   406,   285,   610,   412,  -463,
     201,   270,   670,   153,   296,   261,   528,   391,   389,   331,
     331,   -89,   398,   382,   382,   285,   413,   154,   392,  -464,
     155,   856,   855,   435,   327,   857,   604,   156,   414,   398,
     407,   201,   201,   201,   201,   496,    35,   377,   201,   412,
     461,   744,   201,   637,   637,   235,   510,   299,   292,   157,
     158,   410,   516,   159,   160,    37,   417,   631,   468,   511,
       7,   796,   418,   797,     8,   517,   477,    38,   233,   201,
     234,   201,   679,   508,   704,   546,   292,   547,   755,   527,
     355,    82,   850,   256,   549,   256,   550,   851,   170,    83,
     171,   756,   256,   426,   256,   660,   428,   429,   205,   377,
     206,   323,   558,   607,   681,   682,   625,   795,   441,    85,
     462,   498,   436,   664,   165,   762,   833,   154,   201,   820,
     155,   459,   563,   222,   562,   840,   500,   156,   763,   421,
     731,   241,   821,   286,   300,   422,   287,   301,   841,   296,
     334,   154,    67,   377,   155,   154,    69,   604,   155,   157,
     489,   156,   412,   159,   336,   156,   422,   708,   168,   201,
     817,   176,   201,   819,   201,   201,   208,   296,   201,   557,
     757,   174,   288,   157,   201,   497,   202,   157,   244,   224,
     764,   159,   245,   201,    67,   331,   338,   339,    69,   610,
     340,   341,   346,   347,   423,   272,   490,   447,   422,   770,
     422,   201,   201,   201,   278,   674,   247,   675,   247,   694,
     810,   701,   701,   302,   331,   247,   633,   247,   303,   327,
     261,   214,   604,   215,    76,   241,   319,   579,   304,    76,
     209,   210,   211,   212,   693,   305,   201,   201,   473,   154,
     474,   611,   155,   612,   622,   662,   623,   201,   306,   156,
     723,   229,   316,   256,   838,   230,   145,   112,   725,   766,
     726,   767,   823,   307,   753,   308,   288,   114,   327,   807,
     318,   157,   244,   793,   436,   159,   245,   348,   364,   364,
     201,   201,   201,    14,    15,    78,   521,   522,   566,   567,
      78,   261,   842,   843,   704,   334,   350,   801,   802,   201,
     801,   831,   597,   435,   276,   277,   836,   361,   606,   336,
     201,   331,   370,   372,   373,   461,   382,   412,   374,   723,
     375,   384,   242,   666,   201,   390,   409,   201,   799,   282,
     408,   717,   419,   201,   853,   604,   352,   806,   848,    76,
     241,   338,   339,  -484,   626,   340,   341,   420,   438,   482,
     145,   145,   701,   355,   154,   463,   466,   155,   464,   444,
     364,   145,   145,   483,   156,   465,   201,   479,   201,   480,
     436,   484,   754,   485,   487,   723,   247,   486,   723,   377,
     377,   288,   488,   494,   502,  -372,   157,   244,   201,   513,
     159,   245,   201,   509,   201,   201,   459,   514,   771,   201,
      78,   772,   201,   520,   529,   792,   530,   531,   538,   540,
     382,   382,   201,   256,   735,   723,   786,   723,   852,   222,
     551,   382,   382,   680,   552,   541,   553,   201,    76,   201,
     554,   201,   201,   201,   555,   798,   386,   800,   556,   559,
     449,   201,   331,   571,   572,   386,   201,   201,   201,   575,
     558,   386,   386,   233,   573,   234,   576,   519,   201,   577,
     222,   201,   578,   580,  -468,   596,   608,   629,   614,   329,
     615,   201,   617,   539,   618,   826,   634,   412,   542,   364,
     545,   635,   201,   201,   636,   640,   222,   773,   201,    78,
     201,   735,   563,   346,   562,   145,   241,   201,   201,   641,
     201,   844,   145,   646,   222,   201,   647,   334,   649,   786,
     154,   653,   201,   155,   351,   201,   655,   668,   673,  -202,
     156,   336,   201,   222,   222,   222,   386,   201,   672,   683,
     201,   441,   705,   222,   707,   377,   247,   430,   739,   222,
     222,   719,   157,   244,   748,   334,   589,   589,   352,   749,
     758,   759,   335,   338,   339,   382,   765,   340,   341,   336,
     777,   532,   382,   222,   790,   533,   816,   534,   794,   114,
     824,   825,   828,   829,   849,  -241,   839,    67,   667,   632,
      39,    69,   619,   280,   364,   261,   621,   309,    41,   222,
     740,   338,   339,   665,   854,   340,   341,   222,   815,   761,
      43,   129,   830,   832,   439,   804,    45,    46,    47,   178,
     179,   180,   837,   808,   222,    53,    54,   692,    55,    56,
      57,   702,   383,    58,    28,   769,   626,    59,   639,    60,
      61,    62,    63,    64,   847,   506,   628,   684,   784,   685,
     818,   427,   776,   845,   659,   364,   598,   131,   789,   736,
      87,    39,    88,    89,    90,    91,   239,    92,   331,    41,
      93,   589,   609,    94,    95,    96,    97,    98,   401,    99,
     377,    43,   451,   100,   742,    44,   101,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,   741,    55,
      56,    57,   369,   620,    58,   661,   745,   103,    59,   188,
      60,    61,    62,    63,    64,   537,   536,   364,   709,   822,
     104,     0,   642,     0,     0,   404,     0,   334,     0,     0,
       0,     0,     0,   105,   351,     0,     0,     0,     0,   106,
       0,   336,     0,     0,     0,     0,   107,     0,     0,   108,
       0,   109,     0,     0,     0,     0,   589,   364,   743,     0,
       0,     0,     0,     0,     0,   110,     0,     0,   352,   111,
       0,   112,     0,   338,   339,     0,     0,   340,   341,   113,
      66,   114,     0,     0,    68,   115,     0,     0,     0,     0,
     116,   117,   118,   119,     0,     0,   120,     0,     0,     0,
       0,   121,   122,    87,    39,    88,     0,   695,     0,     0,
      92,     0,    41,    93,     0,     0,    94,    95,    96,     0,
      98,     0,    99,     0,    43,     0,   696,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,   805,    55,    56,    57,     0,     0,    58,     0,     0,
     103,    59,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,   104,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   105,   827,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,   107,
       0,     0,   108,     0,   109,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   110,     0,
       0,     0,   111,     0,   112,     0,     0,     0,     0,     0,
       0,     0,   113,    66,   114,     0,     0,    68,   115,     0,
       0,     0,     0,   116,   117,   118,   119,     0,     0,   120,
       0,     0,     0,     0,   121,   122,    87,    39,    88,     0,
     686,     0,     0,    92,     0,    41,    93,     0,     0,    94,
      95,    96,     0,    98,     0,     0,     0,    43,     0,   687,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,   103,    59,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,   104,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   105,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,   107,     0,     0,   108,     0,   109,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,     0,   111,     0,   112,     0,     0,
       0,     0,     0,     0,     0,   113,    66,   114,     0,     0,
      68,   115,     0,     0,     0,     0,   116,   117,   118,   119,
       0,     0,   120,    87,    39,    88,     0,   121,   122,     0,
      92,     0,    41,    93,     0,     0,    94,    95,    96,     0,
      98,     0,     0,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
     103,    59,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,   104,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   105,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,   107,
       0,     0,   108,     0,   109,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   110,     0,
       0,     0,   111,     0,   112,     0,     0,     0,     0,     0,
       0,     0,   113,    66,   114,     0,     0,    68,   115,     0,
       0,     0,     0,   116,   117,   118,   119,     0,    39,   120,
       0,     0,    40,     0,   121,   122,    41,     0,     0,     0,
       0,     0,     0,     0,     0,    42,     0,     0,    43,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
       0,    51,    52,    53,    54,    39,    55,    56,    57,     0,
       0,    58,     0,    41,     0,    59,     0,    60,    61,    62,
      63,    64,   581,     0,     0,    43,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    65,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    66,    67,     0,
       0,    68,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    70,     0,     0,
       0,     0,     0,    65,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    66,    67,     0,     0,    68,    69,
      22,     0,    87,    39,    88,     0,     0,     0,     0,    92,
       0,    41,    93,     0,   582,     0,     0,     0,     0,    98,
       0,     0,     0,    43,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,   103,
      59,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,   107,     0,
       0,   108,     0,   109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    23,   110,     0,     0,
       0,   175,     0,   112,     0,     0,     0,   588,     0,     0,
       0,     0,    66,   114,     0,     0,    68,   115,     0,     0,
       0,     0,   116,   117,   118,   119,     0,     0,   120,    87,
      39,    88,     0,     0,     0,     0,    92,     0,    41,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,   103,    59,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   241,     0,     0,   106,     0,
       0,     0,     0,     0,     0,   107,     0,     0,   108,   154,
     109,     0,   155,     0,     0,     0,     0,     0,   259,   156,
       0,     0,     0,     0,   110,     0,     0,     0,   175,   260,
     112,     0,     0,     0,     0,   261,   243,     0,     0,    66,
     114,   157,   244,    68,   115,   159,   245,     0,     0,   116,
     117,   118,   119,     0,     0,   120,    87,    39,    88,     0,
       0,     0,     0,    92,     0,    41,    93,     0,     0,     0,
       0,     0,     0,    98,     0,     0,     0,    43,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,   103,    59,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   241,     0,     0,   106,     0,     0,     0,     0,
       0,     0,   107,     0,     0,   108,     0,   109,     0,   155,
       0,     0,     0,     0,     0,     0,   156,     0,     0,     0,
       0,   110,   242,     0,     0,   175,     0,   112,     0,     0,
       0,     0,     0,   243,     0,     0,    66,   114,   157,   244,
      68,   115,   159,   245,     0,     0,   116,   117,   118,   119,
       0,     0,   120,    87,    39,    88,     0,     0,     0,     0,
      92,     0,    41,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
     103,    59,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   241,
       0,     0,   106,     0,     0,     0,     0,     0,     0,   107,
       0,     0,   108,     0,   109,     0,   155,     0,     0,     0,
       0,     0,     0,   156,     0,     0,     0,     0,   110,     0,
       0,     0,   175,     0,   112,     0,     0,     0,     0,     0,
     243,     0,     0,    66,   114,   157,   244,    68,   115,   159,
     245,     0,     0,   116,   117,   118,   119,     0,     0,   120,
      87,    39,    88,     0,     0,     0,     0,    92,     0,    41,
      93,     0,     0,     0,     0,     0,     0,    98,     0,     0,
       0,    43,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,   103,    59,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,   107,     0,     0,   108,
       0,   109,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   587,     0,     0,   110,     0,     0,     0,   175,
       0,   112,     0,     0,     0,   588,     0,     0,     0,     0,
      66,   114,     0,     0,    68,   115,     0,     0,     0,     0,
     116,   117,   118,   119,     0,     0,   120,    87,    39,    88,
       0,     0,     0,     0,    92,     0,    41,    93,     0,     0,
       0,     0,     0,     0,   362,     0,     0,     0,    43,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,     0,    55,    56,    57,     0,
     363,    58,     0,     0,   103,    59,     0,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,   107,     0,     0,   108,     0,   109,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,   175,     0,   112,     0,
       0,     0,     0,     0,     0,     0,     0,    66,   114,     0,
       0,    68,   115,     0,     0,     0,     0,   116,   117,   118,
     119,     0,     0,   120,    87,    39,    88,     0,     0,     0,
       0,    92,     0,    41,    93,     0,     0,     0,     0,     0,
       0,    98,     0,     0,     0,    43,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,   103,    59,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
     107,     0,     0,   108,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,   175,     0,   112,     0,     0,     0,   588,
       0,     0,     0,     0,    66,   114,     0,     0,    68,   115,
       0,     0,     0,     0,   116,   117,   118,   119,     0,     0,
     120,    87,    39,    88,     0,     0,     0,     0,    92,     0,
      41,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,   103,    59,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,   107,     0,     0,
     108,     0,   109,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
     175,     0,   112,     0,     0,     0,     0,     0,     0,     0,
       0,    66,   114,     0,     0,    68,   115,     0,     0,     0,
       0,   116,   117,   118,   119,     0,     0,   120,    87,    39,
      88,     0,     0,     0,     0,    92,     0,    41,    93,     0,
       0,     0,     0,     0,     0,   362,     0,     0,     0,    43,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,   103,    59,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,   107,     0,     0,   108,     0,   109,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,     0,   175,     0,   112,
       0,     0,     0,     0,     0,     0,     0,     0,    66,   114,
       0,     0,    68,   115,     0,     0,     0,     0,   116,   117,
     118,   119,     0,     0,   120,    87,    39,    88,     0,     0,
       0,     0,    92,     0,    41,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    43,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,   103,    59,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,   107,     0,     0,     0,     0,   109,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,   175,     0,   112,     0,     0,     0,
       0,     0,     0,     0,     0,    66,   114,     0,     0,    68,
     115,     0,     0,     0,     0,   116,   117,   118,   119,     0,
       0,   120,    87,    39,    88,     0,     0,     0,     0,    92,
       0,    41,    93,     0,     0,     0,     0,     0,     0,    98,
       0,     0,     0,    43,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,   298,   107,     0,
       0,     0,     0,   109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   110,     0,     0,
       0,   175,     0,   112,     0,     0,     0,     0,     0,     0,
       0,     0,    66,   114,     0,     0,    68,   115,     0,     0,
       0,     0,   116,   117,   118,   119,     0,     0,   120,    87,
      39,    88,     0,     0,     0,     0,    92,     0,    41,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,   400,     0,     0,   107,     0,     0,     0,     0,
     109,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   110,     0,     0,     0,   175,     0,
     112,     0,     0,     0,     0,     0,     0,     0,     0,    66,
     114,     0,     0,    68,   115,     0,     0,     0,     0,   116,
     117,   118,   119,     0,     0,   120,    87,    39,    88,     0,
       0,     0,     0,    92,     0,    41,    93,     0,     0,     0,
       0,     0,     0,    98,     0,     0,     0,    43,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,   109,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,     0,   175,     0,   112,     0,     0,
      39,     0,     0,     0,     0,     0,    66,   114,    41,     0,
      68,   115,     0,     0,     0,     0,   116,   117,   118,   119,
      43,     0,   120,     0,   328,     0,    45,    46,    47,   178,
     179,   180,     0,     0,     0,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   334,     0,     0,     0,     0,
       0,     0,   335,     0,     0,   181,     0,     0,     0,   336,
     182,     0,   183,     0,     0,     0,     0,     0,     0,     0,
     184,     0,     0,    39,   185,     0,     0,     0,   186,   337,
     187,    41,     0,     0,     0,   261,     0,     0,     0,   188,
      67,   338,   339,    43,    69,   340,   341,     0,     0,    45,
      46,    47,   178,   179,   180,     0,     0,     0,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   241,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,     0,
    -203,     0,     0,   182,     0,   183,     0,     0,     0,     0,
       0,     0,     0,   184,    39,     0,     0,   185,     0,     0,
       0,   186,    41,   187,     0,     0,     0,     0,     0,   778,
       0,     0,   188,    67,    43,   244,     0,    69,     0,     0,
      45,    46,    47,   178,   179,   180,     0,     0,     0,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   241,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
       0,     0,     0,     0,   182,     0,   183,     0,     0,     0,
       0,     0,     0,     0,   184,     0,     0,    39,   185,     0,
       0,     0,   186,     0,   187,    41,     0,     0,     0,     0,
     778,     0,   218,   188,    67,     0,   244,    43,    69,     0,
       0,     0,     0,    45,    46,    47,   178,   179,   180,     0,
     219,     0,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   181,     0,    39,     0,     0,   182,     0,   183,
       0,     0,    41,     0,     0,     0,     0,   184,     0,     0,
       0,   185,     0,     0,    43,   186,     0,   187,   328,     0,
      45,    46,    47,   178,   179,   180,   188,    67,     0,    53,
      54,    69,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
       0,     0,     0,     0,   182,     0,   183,     0,     0,     0,
       0,     0,     0,     0,   184,    39,     0,     0,   185,   329,
       0,     0,   186,    41,   187,     0,     0,     0,     0,     0,
     751,     0,     0,   188,    67,    43,     0,     0,    69,     0,
       0,    45,    46,    47,   178,   179,   180,     0,   752,     0,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     181,     0,    39,     0,     0,   182,     0,   183,     0,     0,
      41,     0,     0,     0,     0,   184,     0,     0,     0,   185,
       0,     0,    43,   186,     0,   187,     0,     0,    45,    46,
      47,   178,   179,   180,   188,    67,     0,    53,    54,    69,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   357,   181,     0,    39,
       0,     0,   182,     0,   183,     0,     0,    41,     0,     0,
       0,     0,   184,     0,     0,     0,   185,     0,     0,    43,
     186,     0,   187,   328,     0,    45,    46,    47,   178,   179,
     180,   188,    67,     0,    53,    54,    69,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   181,     0,    39,     0,     0,   182,
       0,   183,     0,     0,    41,     0,     0,     0,     0,   184,
       0,     0,     0,   185,     0,     0,    43,   186,     0,   187,
     560,     0,    45,    46,    47,   178,   179,   180,   188,    67,
       0,    53,    54,    69,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   181,     0,    39,     0,     0,   182,     0,   183,     0,
       0,    41,     0,     0,     0,     0,   184,     0,     0,     0,
     185,     0,     0,    43,   186,     0,   187,     0,     0,    45,
      46,    47,   178,   179,   180,   188,    67,     0,    53,    54,
      69,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,     0,
      39,     0,     0,   182,     0,   183,     0,     0,    41,     0,
       0,     0,     0,   184,     0,     0,     0,   185,     0,     0,
      43,   186,   785,   187,     0,     0,    45,    46,    47,   178,
     179,   180,   188,    67,     0,    53,    54,    69,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   811,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,     0,    39,     0,     0,
     182,     0,   183,     0,     0,    41,     0,     0,     0,     0,
     184,     0,     0,     0,   185,     0,     0,    43,   186,     0,
     187,     0,     0,    45,    46,    47,   178,   179,   180,   188,
      67,     0,    53,    54,    69,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   814,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   181,     0,    39,     0,     0,   182,     0,   183,
       0,     0,    41,     0,     0,     0,     0,   184,     0,     0,
       0,   185,     0,     0,    43,   186,     0,   187,   328,     0,
      45,    46,    47,   178,   179,   180,   188,    67,     0,    53,
      54,    69,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
       0,    39,     0,     0,   182,     0,   183,     0,     0,    41,
       0,     0,     0,     0,   184,     0,     0,     0,   185,     0,
       0,    43,   834,     0,   187,     0,     0,    45,    46,    47,
     178,   179,   180,   188,    67,     0,    53,    54,    69,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   181,     0,     0,     0,
       0,   182,     0,   183,     0,     0,     0,     0,     0,     0,
       0,   184,    39,     0,     0,   185,    40,     0,     0,   186,
      41,   187,     0,     0,     0,     0,     0,     0,     0,    42,
     188,    67,    43,     0,     0,    69,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    39,
      55,    56,    57,     0,     0,    58,     0,    41,     0,    59,
       0,    60,    61,    62,    63,    64,     0,     0,     0,    43,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    39,     0,     0,     0,     0,     0,     0,
      65,    41,     0,   310,     0,     0,     0,     0,     0,     0,
     581,    66,    67,    43,     0,    68,    69,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      39,    55,    56,    57,     0,     0,    58,    65,    41,     0,
      59,     0,    60,    61,    62,    63,    64,     0,    66,    67,
      43,     0,    68,    69,    44,     0,    45,    46,    47,    48,
      49,    50,     0,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    39,     0,     0,     0,     0,     0,
       0,    65,    41,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    66,    67,    43,     0,    68,    69,    44,     0,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,    39,    55,    56,    57,     0,     0,    58,    65,    41,
       0,    59,     0,    60,    61,    62,    63,    64,     0,    66,
      67,    43,     0,    68,    69,     0,     0,    45,    46,    47,
     178,   179,   180,     0,     0,     0,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    39,     0,     0,     0,     0,
       0,     0,     0,    41,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    66,   114,    43,     0,    68,   115,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    39,    55,    56,    57,     0,     0,    58,     0,
      41,     0,    59,     0,    60,    61,    62,    63,    64,     0,
     188,    67,    43,     0,     0,    69,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,   455,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    39,     0,     0,     0,
       0,     0,     0,   227,    41,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    66,     0,    43,     0,    68,     0,
      44,     0,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,    39,    55,    56,    57,     0,     0,    58,
     227,    41,     0,    59,     0,    60,    61,    62,    63,    64,
       0,    66,     0,    43,     0,    68,     0,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      39,    55,    56,    57,     0,     0,    58,     0,    41,     0,
      59,     0,    60,    61,    62,    63,    64,     0,     0,     0,
      43,     0,     0,     0,     0,     0,    45,    46,    47,   178,
     179,   180,     0,     0,     0,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,    66,   114,    59,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   322,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    66,     0,     0,     0,     0,   713,     0,     0,
       0,     0,     0,     0,    39,     0,     0,     0,     0,   714,
     600,     0,    41,     0,     0,     0,     0,     0,   601,     0,
       0,     0,     0,     0,    43,     0,     0,     0,    44,   188,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,    39,    55,    56,    57,     0,     0,    58,     0,    41,
       0,    59,     0,    60,    61,    62,    63,    64,     0,     0,
       0,    43,     0,     0,     0,     0,     0,    45,    46,    47,
     178,   179,   180,     0,     0,    39,    53,    54,     0,    55,
      56,    57,     0,    41,    58,     0,     0,     0,    59,     0,
      60,    61,    62,    63,    64,    43,     0,     0,     0,     0,
       0,    45,    46,    47,   178,   179,   180,     0,     0,     0,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,    66,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     599,   600,     0,     0,     0,     0,     0,     0,     0,   601,
      39,     0,     0,     0,     0,     0,     0,     0,    41,     0,
     188,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      43,     0,     0,     0,   775,   600,    45,    46,    47,   178,
     179,   180,     0,   601,    39,    53,    54,     0,    55,    56,
      57,     0,    41,    58,   188,     0,     0,    59,     0,    60,
      61,    62,    63,    64,    43,     0,     0,     0,     0,     0,
      45,    46,    47,   178,   179,   180,     0,     0,     0,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,    39,     0,     0,     0,     0,     0,
     600,     0,    41,     0,     0,     0,     0,     0,   601,     0,
       0,     0,     0,     0,    43,     0,     0,     0,     0,   188,
      45,    46,    47,   178,   179,   180,     0,     0,     0,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,   601,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,   188,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188
  };

  const short
  parser::yycheck_[] =
  {
      89,   112,   192,    34,    89,   105,    89,   105,   110,   181,
     182,   100,    40,   230,    42,   100,   110,   111,    29,   281,
     100,    29,   194,   315,   259,   218,   208,   352,   483,   133,
     111,   185,   222,   214,   215,   184,   561,   319,   444,    65,
     287,   561,   102,   285,     1,   645,   644,   502,    12,    19,
       1,    27,    65,   560,   654,   474,    84,     1,     5,     1,
       1,   293,   279,    34,     1,   300,   521,    79,    21,     0,
      77,    77,   721,   177,    77,   186,   187,   369,    27,    29,
      91,   175,     1,    77,   362,   111,    90,   243,   112,   802,
      22,   383,   181,   182,   175,   721,   185,   186,   111,   109,
     124,   113,    81,   100,   128,   194,   105,   119,    84,   119,
     108,   566,    80,   202,   126,   715,   119,   202,   831,    51,
     133,    53,   101,    19,   213,   119,   298,   134,   134,   218,
     219,   135,   288,   218,   219,    84,   134,   105,   108,   110,
     111,   103,   649,   247,   106,   107,   118,   109,   112,   175,
     107,   104,   243,   184,   104,   166,   107,   168,   166,   406,
     168,   118,   175,   107,   177,   107,   107,   118,   817,   588,
     107,   449,   404,   135,   118,    71,   118,   118,   413,   112,
     112,   118,   474,   214,   215,   357,   275,    80,   107,   119,
      80,   817,   124,   819,   283,   284,   128,   288,   283,    80,
     283,   227,    80,    80,   175,   202,    80,   361,   119,   298,
      80,   113,    14,   143,   227,   481,   166,   119,   168,    78,
      80,   218,   219,   170,   171,    84,   119,   493,   259,   119,
     319,   115,   143,    79,   247,   119,   390,    41,   119,   185,
     186,   119,   119,   214,   215,   119,   101,    93,    52,   119,
      96,   851,   850,   279,   285,   855,   522,   103,   113,   119,
     119,   350,   351,   352,   353,   350,   134,   213,   357,   300,
     301,   677,   361,   555,   556,   237,   105,   239,   382,   125,
     126,   252,   105,   129,   130,    77,   113,   529,   316,   118,
     124,   746,   119,   748,   128,   118,   322,    77,   398,   388,
     398,   390,   594,   363,   629,   407,   410,   409,   105,   389,
     500,    27,   837,   407,   416,   409,   418,   837,   104,    24,
     106,   118,   416,   273,   418,   572,   276,   277,   104,   275,
     106,   480,   430,   487,   600,   601,   517,   743,   284,   118,
     302,   352,   559,   578,   113,   105,   801,    93,   437,   105,
      96,   301,   437,   350,   437,   105,   353,   103,   118,   113,
     112,    79,   118,    81,   101,   119,    84,   104,   118,   382,
      79,    93,   124,   319,    96,    93,   128,   643,    96,   125,
     113,   103,   413,   129,    93,   103,   119,   634,   112,   478,
     777,    88,   481,   780,   483,   484,    93,   410,   487,   430,
     692,   118,   120,   125,   493,   351,    37,   125,   126,    14,
     702,   129,   130,   502,   124,   361,   125,   126,   128,   685,
     129,   130,   533,   534,   115,   108,   115,   674,   119,   711,
     119,   520,   521,   522,   135,    84,   407,    86,   409,   621,
     765,   622,   623,    87,   390,   416,   115,   418,   134,   480,
     119,   104,   718,   106,   465,    79,    80,   465,   113,   470,
      72,    73,    74,    75,   618,   113,   555,   556,   104,    93,
     106,   104,    96,   106,   104,   577,   106,   566,   113,   103,
     652,   108,     4,   577,   809,   112,   517,   114,   104,   104,
     106,   106,   784,   113,   687,   113,   120,   124,   529,   761,
      23,   125,   126,   738,   721,   129,   130,    84,   205,   206,
     599,   600,   601,    65,    66,   465,    80,    81,    80,    81,
     470,   119,   814,   815,   849,    79,    91,    80,    81,   618,
      80,    81,   478,   559,   121,   122,   802,   112,   484,    93,
     629,   487,   118,    77,    77,   576,   517,   578,    77,   721,
      77,    15,   109,   581,   643,    81,   119,   646,   751,    71,
     109,   646,   113,   652,   846,   831,   120,   760,   834,   580,
      79,   125,   126,   113,   520,   129,   130,   113,   134,   109,
     611,   612,   763,   773,    93,   113,   105,    96,   113,   286,
     287,   622,   623,    80,   103,   119,   685,   105,   687,   119,
     817,    91,   687,   113,   119,   777,   577,   113,   780,   555,
     556,   120,   113,    27,    80,    85,   125,   126,   707,    25,
     129,   130,   711,    85,   713,   714,   576,    27,   713,   718,
     580,   714,   721,    81,   134,   737,    77,   113,    77,   120,
     611,   612,   731,   737,   655,   817,   731,   819,   841,   646,
     135,   622,   623,   599,   109,   120,    77,   746,   669,   748,
      77,   750,   751,   752,    80,   750,   751,   752,    80,   119,
      27,   760,   618,    81,   119,   760,   765,   766,   767,   105,
     778,   766,   767,   783,   120,   783,   119,   384,   777,    81,
     687,   780,   101,   112,   101,   113,   119,    84,   120,   109,
     120,   790,   113,   400,   119,   790,   119,   738,   405,   406,
     407,    78,   801,   802,   109,   120,   713,   714,   807,   669,
     809,   732,   807,   834,   807,   756,    79,   816,   817,   120,
     819,   816,   763,    91,   731,   824,    77,    79,    28,   824,
      93,    27,   831,    96,    86,   834,     9,   113,   118,    91,
     103,    93,   841,   750,   751,   752,   841,   846,   105,   113,
     849,   707,    27,   760,    80,   711,   737,   120,   113,   766,
     767,    84,   125,   126,    80,    79,   473,   474,   120,    37,
      11,    19,    86,   125,   126,   756,    86,   129,   130,    93,
      91,   108,   763,   790,    54,   112,    91,   114,    86,   124,
     119,   113,   105,   113,    84,    84,   105,   124,   583,   113,
       4,   128,   509,   124,   511,   119,   513,   166,    12,   816,
     669,   125,   126,   580,   849,   129,   130,   824,   767,   698,
      24,    34,   798,   800,   283,   756,    30,    31,    32,    33,
      34,    35,   807,   763,   841,    39,    40,   612,    42,    43,
      44,   623,   215,    47,    15,   707,   802,    51,   556,    53,
      54,    55,    56,    57,   824,   361,   522,   608,   726,   609,
     777,   275,   719,   821,   571,   572,   480,    34,   732,   657,
       3,     4,     5,     6,     7,     8,   108,    10,   834,    12,
      13,   588,    86,    16,    17,    18,    19,    20,   237,    22,
     846,    24,   290,    26,   674,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,   673,    42,
      43,    44,   206,   511,    47,   576,   677,    50,    51,   123,
      53,    54,    55,    56,    57,   398,   397,   634,   635,   783,
      63,    -1,   559,    -1,    -1,   247,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    76,    86,    -1,    -1,    -1,    -1,    82,
      -1,    93,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      -1,    94,    -1,    -1,    -1,    -1,   673,   674,   675,    -1,
      -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,   120,   112,
      -1,   114,    -1,   125,   126,    -1,    -1,   129,   130,   122,
     123,   124,    -1,    -1,   127,   128,    -1,    -1,    -1,    -1,
     133,   134,   135,   136,    -1,    -1,   139,    -1,    -1,    -1,
      -1,   144,   145,     3,     4,     5,    -1,     7,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    16,    17,    18,    -1,
      20,    -1,    22,    -1,    24,    -1,    26,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,   758,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    -1,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,   794,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,    -1,
      -1,    -1,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   123,   124,    -1,    -1,   127,   128,    -1,
      -1,    -1,    -1,   133,   134,   135,   136,    -1,    -1,   139,
      -1,    -1,    -1,    -1,   144,   145,     3,     4,     5,    -1,
       7,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    16,
      17,    18,    -1,    20,    -1,    -1,    -1,    24,    -1,    26,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    -1,    53,    54,    55,    56,
      57,    -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    -1,    94,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,    -1,    -1,    -1,   112,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122,   123,   124,    -1,    -1,
     127,   128,    -1,    -1,    -1,    -1,   133,   134,   135,   136,
      -1,    -1,   139,     3,     4,     5,    -1,   144,   145,    -1,
      10,    -1,    12,    13,    -1,    -1,    16,    17,    18,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    -1,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,    -1,
      -1,    -1,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   123,   124,    -1,    -1,   127,   128,    -1,
      -1,    -1,    -1,   133,   134,   135,   136,    -1,     4,   139,
      -1,    -1,     8,    -1,   144,   145,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    21,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,     4,    42,    43,    44,    -1,
      -1,    47,    -1,    12,    -1,    51,    -1,    53,    54,    55,
      56,    57,    21,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    -1,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   123,   124,    -1,
      -1,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,    -1,    -1,
      -1,    -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   123,   124,    -1,    -1,   127,   128,
       1,    -1,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,   143,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    -1,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,
      -1,   112,    -1,   114,    -1,    -1,    -1,   118,    -1,    -1,
      -1,    -1,   123,   124,    -1,    -1,   127,   128,    -1,    -1,
      -1,    -1,   133,   134,   135,   136,    -1,    -1,   139,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    -1,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      94,    -1,    96,    -1,    -1,    -1,    -1,    -1,   102,   103,
      -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,   112,   113,
     114,    -1,    -1,    -1,    -1,   119,   120,    -1,    -1,   123,
     124,   125,   126,   127,   128,   129,   130,    -1,    -1,   133,
     134,   135,   136,    -1,    -1,   139,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    -1,    53,    54,    55,    56,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    -1,    94,    -1,    96,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
      -1,   108,   109,    -1,    -1,   112,    -1,   114,    -1,    -1,
      -1,    -1,    -1,   120,    -1,    -1,   123,   124,   125,   126,
     127,   128,   129,   130,    -1,    -1,   133,   134,   135,   136,
      -1,    -1,   139,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    -1,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    -1,    94,    -1,    96,    -1,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,    -1,    -1,   108,    -1,
      -1,    -1,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,
     120,    -1,    -1,   123,   124,   125,   126,   127,   128,   129,
     130,    -1,    -1,   133,   134,   135,   136,    -1,    -1,   139,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    -1,
      53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   105,    -1,    -1,   108,    -1,    -1,    -1,   112,
      -1,   114,    -1,    -1,    -1,   118,    -1,    -1,    -1,    -1,
     123,   124,    -1,    -1,   127,   128,    -1,    -1,    -1,    -1,
     133,   134,   135,   136,    -1,    -1,   139,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      46,    47,    -1,    -1,    50,    51,    -1,    53,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    -1,    94,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   108,    -1,    -1,    -1,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   123,   124,    -1,
      -1,   127,   128,    -1,    -1,    -1,    -1,   133,   134,   135,
     136,    -1,    -1,   139,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    -1,    94,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
      -1,    -1,    -1,   112,    -1,   114,    -1,    -1,    -1,   118,
      -1,    -1,    -1,    -1,   123,   124,    -1,    -1,   127,   128,
      -1,    -1,    -1,    -1,   133,   134,   135,   136,    -1,    -1,
     139,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      -1,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,
     112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   123,   124,    -1,    -1,   127,   128,    -1,    -1,    -1,
      -1,   133,   134,   135,   136,    -1,    -1,   139,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    -1,    53,    54,
      55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    -1,    94,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   108,    -1,    -1,    -1,   112,    -1,   114,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   123,   124,
      -1,    -1,   127,   128,    -1,    -1,    -1,    -1,   133,   134,
     135,   136,    -1,    -1,   139,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    -1,    53,    54,    55,    56,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    -1,    -1,    94,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     108,    -1,    -1,    -1,   112,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   123,   124,    -1,    -1,   127,
     128,    -1,    -1,    -1,    -1,   133,   134,   135,   136,    -1,
      -1,   139,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    -1,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    89,    -1,
      -1,    -1,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,
      -1,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   123,   124,    -1,    -1,   127,   128,    -1,    -1,
      -1,    -1,   133,   134,   135,   136,    -1,    -1,   139,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    86,    -1,    -1,    89,    -1,    -1,    -1,    -1,
      94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,   112,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   123,
     124,    -1,    -1,   127,   128,    -1,    -1,    -1,    -1,   133,
     134,   135,   136,    -1,    -1,   139,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    -1,    53,    54,    55,    56,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    -1,    -1,    94,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,    -1,    -1,    -1,   112,    -1,   114,    -1,    -1,
       4,    -1,    -1,    -1,    -1,    -1,   123,   124,    12,    -1,
     127,   128,    -1,    -1,    -1,    -1,   133,   134,   135,   136,
      24,    -1,   139,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    86,    -1,    -1,    89,    -1,    -1,    -1,    93,
      94,    -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,    -1,     4,   108,    -1,    -1,    -1,   112,   113,
     114,    12,    -1,    -1,    -1,   119,    -1,    -1,    -1,   123,
     124,   125,   126,    24,   128,   129,   130,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    -1,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      91,    -1,    -1,    94,    -1,    96,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,     4,    -1,    -1,   108,    -1,    -1,
      -1,   112,    12,   114,    -1,    -1,    -1,    -1,    -1,   120,
      -1,    -1,   123,   124,    24,   126,    -1,   128,    -1,    -1,
      30,    31,    32,    33,    34,    35,    -1,    -1,    -1,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    -1,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    -1,    -1,    94,    -1,    96,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,     4,   108,    -1,
      -1,    -1,   112,    -1,   114,    12,    -1,    -1,    -1,    -1,
     120,    -1,    19,   123,   124,    -1,   126,    24,   128,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    -1,    53,    54,    55,    56,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,     4,    -1,    -1,    94,    -1,    96,
      -1,    -1,    12,    -1,    -1,    -1,    -1,   104,    -1,    -1,
      -1,   108,    -1,    -1,    24,   112,    -1,   114,    28,    -1,
      30,    31,    32,    33,    34,    35,   123,   124,    -1,    39,
      40,   128,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    -1,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    -1,    -1,    94,    -1,    96,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,     4,    -1,    -1,   108,   109,
      -1,    -1,   112,    12,   114,    -1,    -1,    -1,    -1,    -1,
      19,    -1,    -1,   123,   124,    24,    -1,    -1,   128,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    -1,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,     4,    -1,    -1,    94,    -1,    96,    -1,    -1,
      12,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,
      -1,    -1,    24,   112,    -1,   114,    -1,    -1,    30,    31,
      32,    33,    34,    35,   123,   124,    -1,    39,    40,   128,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      -1,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    89,    -1,     4,
      -1,    -1,    94,    -1,    96,    -1,    -1,    12,    -1,    -1,
      -1,    -1,   104,    -1,    -1,    -1,   108,    -1,    -1,    24,
     112,    -1,   114,    28,    -1,    30,    31,    32,    33,    34,
      35,   123,   124,    -1,    39,    40,   128,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    53,    54,
      55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,     4,    -1,    -1,    94,
      -1,    96,    -1,    -1,    12,    -1,    -1,    -1,    -1,   104,
      -1,    -1,    -1,   108,    -1,    -1,    24,   112,    -1,   114,
      28,    -1,    30,    31,    32,    33,    34,    35,   123,   124,
      -1,    39,    40,   128,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    -1,    53,    54,    55,    56,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,     4,    -1,    -1,    94,    -1,    96,    -1,
      -1,    12,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,
     108,    -1,    -1,    24,   112,    -1,   114,    -1,    -1,    30,
      31,    32,    33,    34,    35,   123,   124,    -1,    39,    40,
     128,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    -1,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
       4,    -1,    -1,    94,    -1,    96,    -1,    -1,    12,    -1,
      -1,    -1,    -1,   104,    -1,    -1,    -1,   108,    -1,    -1,
      24,   112,   113,   114,    -1,    -1,    30,    31,    32,    33,
      34,    35,   123,   124,    -1,    39,    40,   128,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,     4,    -1,    -1,
      94,    -1,    96,    -1,    -1,    12,    -1,    -1,    -1,    -1,
     104,    -1,    -1,    -1,   108,    -1,    -1,    24,   112,    -1,
     114,    -1,    -1,    30,    31,    32,    33,    34,    35,   123,
     124,    -1,    39,    40,   128,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    -1,    53,    54,    55,    56,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,     4,    -1,    -1,    94,    -1,    96,
      -1,    -1,    12,    -1,    -1,    -1,    -1,   104,    -1,    -1,
      -1,   108,    -1,    -1,    24,   112,    -1,   114,    28,    -1,
      30,    31,    32,    33,    34,    35,   123,   124,    -1,    39,
      40,   128,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    -1,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,     4,    -1,    -1,    94,    -1,    96,    -1,    -1,    12,
      -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,    -1,
      -1,    24,   112,    -1,   114,    -1,    -1,    30,    31,    32,
      33,    34,    35,   123,   124,    -1,    39,    40,   128,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,
      53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,
      -1,    94,    -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,     4,    -1,    -1,   108,     8,    -1,    -1,   112,
      12,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    21,
     123,   124,    24,    -1,    -1,   128,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,     4,
      42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,    51,
      -1,    53,    54,    55,    56,    57,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    53,    54,
      55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,
     112,    12,    -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,
      21,   123,   124,    24,    -1,   127,   128,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
       4,    42,    43,    44,    -1,    -1,    47,   112,    12,    -1,
      51,    -1,    53,    54,    55,    56,    57,    -1,   123,   124,
      24,    -1,   127,   128,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
      -1,   112,    12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   123,   124,    24,    -1,   127,   128,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,     4,    42,    43,    44,    -1,    -1,    47,   112,    12,
      -1,    51,    -1,    53,    54,    55,    56,    57,    -1,   123,
     124,    24,    -1,   127,   128,    -1,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,
      53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   123,   124,    24,    -1,   127,   128,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,     4,    42,    43,    44,    -1,    -1,    47,    -1,
      12,    -1,    51,    -1,    53,    54,    55,    56,    57,    -1,
     123,   124,    24,    -1,    -1,   128,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    78,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      -1,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,   112,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   123,    -1,    24,    -1,   127,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,     4,    42,    43,    44,    -1,    -1,    47,
     112,    12,    -1,    51,    -1,    53,    54,    55,    56,    57,
      -1,   123,    -1,    24,    -1,   127,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
       4,    42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,
      51,    -1,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,   123,   124,    51,    -1,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   123,    -1,    -1,    -1,    -1,    91,    -1,    -1,
      -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,   103,
     104,    -1,    12,    -1,    -1,    -1,    -1,    -1,   112,    -1,
      -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,   123,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,     4,    42,    43,    44,    -1,    -1,    47,    -1,    12,
      -1,    51,    -1,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,     4,    39,    40,    -1,    42,
      43,    44,    -1,    12,    47,    -1,    -1,    -1,    51,    -1,
      53,    54,    55,    56,    57,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,   123,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,
       4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,
     123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      24,    -1,    -1,    -1,   103,   104,    30,    31,    32,    33,
      34,    35,    -1,   112,     4,    39,    40,    -1,    42,    43,
      44,    -1,    12,    47,   123,    -1,    -1,    51,    -1,    53,
      54,    55,    56,    57,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    -1,    -1,    -1,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    -1,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
     104,    -1,    12,    -1,    -1,    -1,    -1,    -1,   112,    -1,
      -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,   123,
      30,    31,    32,    33,    34,    35,    -1,    -1,    -1,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,   112,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,   123,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   123
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   104,   147,   148,   149,   152,   124,   128,   359,
     153,   164,     0,   153,    65,    66,   150,   105,   118,   154,
     165,   166,     1,   107,   358,   108,   134,   222,   222,   112,
     155,    14,   167,   178,   179,   134,   223,    77,    77,     4,
       8,    12,    21,    24,    28,    30,    31,    32,    33,    34,
      35,    37,    38,    39,    40,    42,    43,    44,    47,    51,
      53,    54,    55,    56,    57,   112,   123,   124,   127,   128,
     143,   156,   157,   158,   162,   325,   328,   329,   342,   344,
     345,   351,    27,    24,   168,   118,   163,     3,     5,     6,
       7,     8,    10,    13,    16,    17,    18,    19,    20,    22,
      26,    29,    36,    50,    63,    76,    82,    89,    92,    94,
     108,   112,   114,   122,   124,   128,   133,   134,   135,   136,
     139,   144,   145,   176,   180,   182,   183,   184,   186,   201,
     227,   271,   276,   280,   281,   283,   284,   285,   286,   314,
     315,   318,   319,   341,   342,   345,   353,   354,   357,   109,
     119,   359,   359,    79,    93,    96,   103,   125,   126,   129,
     130,   348,   349,   350,   352,   113,   119,   143,   112,   159,
     104,   106,   151,   359,   118,   112,   279,   280,    33,    34,
      35,    89,    94,    96,   104,   108,   112,   114,   123,   207,
     232,   234,   236,   237,   239,   243,   323,   324,   328,   338,
     340,   351,    37,   202,   328,   104,   106,   306,   279,    72,
      73,    74,    75,   187,   104,   106,   219,   220,    19,    37,
     185,   234,   236,   324,    14,   306,   284,   112,   342,   108,
     112,   316,   317,   319,   354,   284,   304,   305,   284,   283,
     284,    79,   109,   120,   126,   130,   279,   280,   288,   290,
     291,   321,   335,   337,   347,   348,   350,   355,   356,   102,
     113,   119,   287,   288,   289,   348,   349,   350,   355,   360,
     115,   360,   108,   277,   278,    19,   277,   277,   135,   175,
     163,    19,    71,   209,    80,   119,    81,    84,   120,   273,
     274,   275,   321,   334,   336,   346,   348,   349,    88,   284,
     101,   104,    87,   134,   113,   113,   113,   113,   113,   158,
      78,   160,   161,   162,   153,   153,     4,   169,    23,    80,
     243,   243,   112,   227,   264,   265,   266,   345,    28,   109,
     229,   230,   232,   234,    79,    86,    93,   113,   125,   126,
     129,   130,   229,   247,   330,   331,   360,   360,    84,   251,
      91,    86,   120,   241,   326,   330,   339,    88,   240,   243,
     234,   112,    20,    46,   279,   303,   307,   308,   309,   307,
     118,   282,    77,    77,    77,    77,   225,   230,   244,   218,
     271,   272,   280,   218,    15,   196,   234,   234,    80,   119,
      81,    41,    52,   181,    77,   134,   356,    80,   119,   224,
      86,   304,   344,   353,   334,    78,    84,   119,   109,   119,
     280,   343,   345,   101,   113,   113,   119,   113,   119,   113,
     113,   113,   119,   115,    90,   135,   342,   244,   342,   342,
     120,   177,   320,   332,   333,   349,   356,   209,   134,   207,
     226,   230,   231,   341,   279,   293,   294,   309,   344,    27,
     221,   275,   281,   243,   343,    78,   310,   311,   312,   342,
     343,   345,   284,   113,   113,   119,   105,   358,   359,    12,
     112,   170,   171,   104,   106,   295,   225,   349,    80,   105,
     119,   248,   109,    80,    91,   113,   113,   119,   113,   113,
     115,   252,   253,   254,    27,   213,   234,   230,   328,   340,
     236,   243,    80,   204,   229,   246,   247,   220,   306,    85,
     105,   118,   358,    25,    27,   217,   105,   118,   358,   279,
      81,    80,    81,   205,   230,   255,   112,   324,   229,   134,
      77,   113,   108,   112,   114,   322,   323,   316,    77,   279,
     120,   120,   279,   292,   309,   279,   288,   288,   343,   288,
     288,   135,   109,    77,    77,    80,    80,   345,   354,   119,
      28,   208,   232,   234,    77,   134,    80,    81,   203,   259,
     221,    81,   119,   120,   220,   105,   119,    81,   101,   162,
     112,    21,   143,   162,   172,   173,   174,   105,   118,   279,
     296,   297,   298,   302,   296,   358,   113,   230,   266,   103,
     104,   112,   249,   250,   338,   255,   230,   229,   119,    86,
     338,   104,   106,   212,   120,   120,   255,   113,   119,   279,
     308,   279,   104,   106,   216,   272,   230,   255,   249,    84,
     190,   341,   113,   115,   119,    78,   109,   225,   228,   228,
     120,   120,   332,   248,   203,   259,    91,    77,   255,    28,
     260,   261,   262,    27,   256,     9,   267,   268,   269,   279,
     309,   311,   288,   313,   343,   172,   359,   159,   113,   119,
     143,   296,   105,   118,    84,    86,   299,   300,   301,   358,
     230,   338,   338,   113,   253,   254,     7,    26,   197,   210,
     211,   272,   211,   229,   282,     7,    26,   200,   201,   214,
     215,   272,   215,   191,   340,    27,   193,    80,   309,   279,
      77,   119,    77,    91,   103,   256,   267,   234,   248,    84,
     233,   238,   242,   243,   263,   104,   106,   267,    22,    51,
      53,   112,   188,   270,   327,   328,   269,    81,   101,   113,
     174,   298,   293,   279,   221,   301,    80,   105,    80,    37,
     198,    19,    37,   196,   234,   105,   118,   358,    11,    19,
     199,   199,   105,   118,   358,    86,   104,   106,   194,   226,
     225,   234,   232,   236,   267,   103,   261,    91,   120,   242,
     320,   257,   258,   282,   257,   113,   234,   235,   245,   270,
      54,   189,   288,   343,    86,   221,   255,   255,   234,   196,
     234,    80,    81,   206,   210,   279,   196,   209,   214,   192,
     340,    78,   195,   196,    78,   195,    91,   238,   263,   238,
     105,   118,   317,   358,   119,   113,   234,   279,   105,   113,
     204,    81,   206,   255,   112,   250,   338,   208,   340,   105,
     105,   118,   358,   358,   234,   258,    80,   245,   338,    84,
     203,   259,   196,   225,   191,   256,   267,   267
  };

  const short
  parser::yyr1_[] =
  {
       0,   146,   147,   148,   148,   149,   150,   150,   150,   151,
     151,   152,   152,   153,   154,   154,   154,   155,   155,   156,
     156,   156,   156,   157,   157,   158,   158,   158,   159,   159,
     159,   160,   160,   161,   161,   162,   162,   163,   163,   164,
     164,   165,   166,   166,   167,   168,   168,   169,   169,   170,
     170,   171,   171,   172,   172,   172,   172,   173,   173,   174,
     174,   175,   175,   176,   176,   176,   177,   177,   178,   179,
     179,   180,   180,   180,   180,   180,   180,   180,   180,   181,
     181,   182,   183,   183,   183,   183,   183,   184,   185,   185,
     186,   186,   186,   186,   187,   187,   187,   187,   187,   188,
     188,   188,   189,   190,   190,   191,   192,   192,   193,   193,
     194,   194,   194,   194,   195,   195,   195,   195,   196,   197,
     197,   197,   197,   197,   198,   198,   199,   199,   200,   200,
     200,   201,   201,   202,   202,   203,   203,   204,   204,   205,
     205,   205,   206,   206,   206,   207,   207,   208,   208,   208,
     208,   209,   209,   209,   210,   210,   211,   211,   211,   211,
     212,   212,   213,   213,   214,   214,   215,   215,   215,   215,
     216,   216,   217,   217,   218,   218,   218,   218,   219,   219,
     220,   221,   221,   222,   222,   223,   223,   223,   224,   224,
     225,   226,   227,   227,   228,   228,   229,   229,   230,   230,
     230,   231,   232,   233,   234,   234,   235,   236,   237,   237,
     238,   238,   239,   239,   239,   240,   241,   241,   242,   243,
     243,   243,   243,   243,   243,   243,   243,   243,   243,   244,
     245,   245,   246,   246,   247,   247,   248,   248,   249,   249,
     249,   250,   250,   251,   251,   252,   252,   253,   254,   254,
     255,   256,   256,   256,   257,   257,   258,   259,   260,   260,
     261,   261,   262,   262,   263,   263,   264,   264,   265,   265,
     266,   267,   267,   268,   268,   269,   269,   269,   270,   270,
     270,   271,   271,   272,   273,   273,   274,   274,   275,   276,
     276,   276,   276,   276,   276,   276,   276,   276,   277,   277,
     278,   278,   279,   279,   280,   280,   281,   281,   282,   282,
     283,   283,   283,   283,   284,   284,   284,   284,   284,   284,
     284,   284,   284,   284,   285,   285,   285,   286,   286,   286,
     286,   286,   286,   286,   286,   287,   287,   288,   288,   288,
     289,   289,   290,   290,   290,   290,   290,   290,   290,   291,
     291,   292,   292,   293,   294,   294,   295,   295,   295,   295,
     296,   296,   297,   297,   297,   298,   299,   299,   300,   300,
     301,   302,   303,   304,   305,   305,   306,   306,   307,   307,
     307,   307,   308,   308,   309,   309,   309,   310,   310,   311,
     311,   311,   312,   312,   312,   312,   313,   313,   314,   314,
     315,   315,   316,   316,   316,   317,   317,   318,   318,   318,
     318,   319,   319,   320,   320,   321,   321,   322,   322,   322,
     323,   323,   323,   323,   323,   324,   324,   325,   325,   325,
     325,   326,   326,   327,   328,   328,   329,   330,   330,   330,
     331,   331,   331,   331,   332,   332,   333,   333,   334,   334,
     335,   335,   336,   336,   337,   337,   338,   339,   340,   340,
     340,   340,   340,   341,   341,   342,   342,   342,   343,   344,
     344,   345,   345,   345,   345,   345,   345,   345,   345,   346,
     346,   347,   347,   348,   349,   349,   350,   350,   351,   351,
     351,   351,   351,   351,   351,   351,   351,   351,   351,   351,
     351,   351,   351,   351,   351,   351,   352,   352,   353,   353,
     354,   355,   355,   356,   356,   357,   357,   357,   357,   357,
     358,   358,   359,   359,   360,   360
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
       0,     1,     1,     1,     1,     5,     7,     1,     1,     1,
       1,     4,     4,     5,     6,     6,     4,     4,     3,     1,
       4,     3,     6,     7,     2,     2,     2,     2,     0,     1,
       1,     1,     2,     0,     2,     3,     2,     1,     0,     2,
       3,     3,     3,     3,     3,     2,     1,     0,     3,     4,
       3,     4,     2,     3,     0,     1,     0,     1,     3,     6,
       7,     1,     1,     0,     1,     0,     2,     0,     2,     0,
       2,     2,     0,     2,     4,     3,     1,     6,     4,     3,
       1,     4,     3,     0,     1,     1,     3,     2,     1,     0,
       3,     3,     2,     0,     1,     1,     3,     2,     1,     0,
       3,     3,     2,     0,     3,     2,     1,     0,     3,     3,
       1,     2,     0,     1,     3,     3,     1,     0,     0,     2,
       1,     1,     3,     1,     1,     3,     1,     3,     4,     3,
       1,     1,     1,     1,     1,     3,     1,     1,     1,     3,
       1,     2,     1,     2,     3,     1,     1,     1,     1,     1,
       1,     1,     2,     2,     3,     2,     5,     3,     3,     1,
       1,     3,     1,     0,     1,     3,     2,     0,     1,     3,
       5,     1,     5,     0,     2,     3,     1,     3,     2,     0,
       1,     4,     4,     0,     3,     1,     4,     2,     3,     1,
       4,     2,     3,     0,     1,     3,     0,     1,     3,     1,
       3,     0,     1,     2,     1,     2,     3,     3,     1,     2,
       3,     1,     2,     1,     3,     2,     2,     1,     4,     3,
       3,     4,     4,     3,     4,     6,     6,     4,     0,     1,
       3,     4,     3,     1,     1,     3,     2,     1,     1,     0,
       2,     3,     2,     1,     3,     2,     2,     4,     4,     8,
       4,     2,     2,     1,     4,     3,     1,     1,     1,     1,
       3,     3,     3,     3,     1,     3,     2,     1,     2,     2,
       3,     3,     1,     1,     2,     4,     3,     5,     3,     3,
       3,     3,     1,     1,     3,     1,     3,     3,     2,     2,
       1,     2,     3,     2,     1,     2,     3,     2,     2,     1,
       4,     1,     1,     1,     2,     1,     3,     3,     3,     2,
       1,     0,     1,     2,     3,     1,     2,     1,     0,     3,
       1,     1,     3,     1,     5,     3,     3,     1,     1,     1,
       1,     3,     1,     3,     1,     3,     1,     2,     3,     2,
       3,     1,     2,     1,     3,     1,     3,     1,     2,     2,
       1,     3,     3,     3,     2,     1,     3,     1,     3,     3,
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
  "\"type\"", "\"where\"", "\"forall\"", "\"foreign\"", "\"export\"",
  "\"label\"", "\"dynamic\"", "\"safe\"", "\"interruptible\"",
  "\"unsafe\"", "\"mdo\"", "\"family\"", "\"role\"", "\"stdcall\"",
  "\"ccall\"", "\"bpcall\"", "\"capi\"", "\"prim\"", "\"javascript\"",
  "\"proc\"", "\"rec\"", "\"group\"", "\"by\"", "\"using\"", "\"static\"",
  "\"stock\"", "\"trcall\"", "\"anyclass\"", "\"via\"", "\"unit\"",
  "\"signature\"", "\"dependency\"", "\"{-# SPECIALIZE\"",
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
  "deriv_strategy_via", "opt_injective_info", "injectivity_cond",
  "inj_varids", "where_type_family", "ty_fam_inst_eqn_list",
  "ty_fam_inst_eqns", "ty_fam_inst_eqn", "at_decl_cls", "opt_family",
  "opt_instance", "at_decl_inst", "data_or_newtype", "opt_class",
  "opt_kind_sig", "opt_datafam_kind_sig", "opt_tyfam_kind_sig",
  "opt_at_kind_inj_sig", "tycl_hdr", "datafam_inst_hdr", "capi_ctype",
  "decl_cls", "decls_cls", "decllist_cls", "where_cls", "decl_inst",
  "decls_inst", "decllist_inst", "where_inst", "decls", "decllist",
  "binds", "wherebinds", "strings", "stringlist", "opt_tyconsig",
  "sigtype", "sigtypedoc", "sig_vars", "sigtypes1", "ktype", "ctype",
  "ctypedoc", "context", "context_no_ops", "type", "typedoc", "btype",
  "infixtype", "btype_no_ops", "ftype", "tyarg", "tyop", "atype_docs",
  "atype", "inst_type", "deriv_types", "comma_types0", "comma_types1",
  "tv_bndrs", "tv_bndr", "tv_bndr_no_braces", "fds", "fds1", "fd",
  "varids0", "kind", "gadt_constrlist", "gadt_constrs", "gadt_constr",
  "constrs", "constrs1", "constr", "forall", "constr_stuff", "fielddecls",
  "fielddecls1", "fielddecl", "maybe_derivings", "derivings", "deriving",
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
       0,   513,   513,   530,   531,   533,   537,   538,   539,   541,
     542,   544,   545,   548,   550,   551,   552,   560,   561,   563,
     564,   565,   566,   568,   569,   571,   572,   573,   575,   576,
     577,   579,   580,   582,   583,   585,   586,   590,   591,   593,
     594,   596,   598,   599,   601,   614,   615,   617,   618,   620,
     621,   625,   626,   628,   629,   630,   631,   633,   634,   636,
     637,   642,   643,   645,   646,   647,   649,   650,   654,   656,
     657,   659,   660,   661,   662,   665,   666,   672,   674,   677,
     678,   680,   683,   684,   686,   687,   688,   690,   692,   693,
     696,   697,   698,   704,   711,   712,   713,   714,   715,   717,
     718,   719,   721,   732,   733,   735,   737,   738,   742,   743,
     745,   746,   747,   748,   750,   751,   752,   753,   755,   758,
     760,   762,   764,   765,   767,   767,   769,   769,   773,   775,
     782,   789,   790,   793,   794,   798,   799,   801,   802,   804,
     805,   806,   808,   809,   810,   813,   814,   817,   818,   819,
     820,   822,   823,   824,   866,   867,   869,   870,   871,   872,
     874,   875,   877,   878,   880,   881,   883,   884,   885,   886,
     888,   889,   891,   892,   895,   896,   897,   898,   900,   901,
     903,   905,   906,   914,   915,   917,   918,   919,   932,   933,
     942,   944,   946,   947,   949,   950,   959,   960,   962,   963,
     965,   967,   976,   978,   980,   981,   983,   986,   988,   989,
     991,   992,   994,   996,   997,   999,  1001,  1002,  1009,  1016,
    1017,  1018,  1019,  1020,  1021,  1022,  1023,  1029,  1030,  1033,
    1035,  1036,  1038,  1039,  1041,  1042,  1049,  1050,  1052,  1053,
    1054,  1057,  1058,  1062,  1063,  1065,  1066,  1069,  1071,  1072,
    1077,  1083,  1084,  1085,  1087,  1088,  1090,  1092,  1094,  1095,
    1097,  1098,  1100,  1101,  1103,  1104,  1110,  1111,  1113,  1114,
    1116,  1118,  1119,  1121,  1122,  1124,  1125,  1126,  1128,  1129,
    1130,  1135,  1137,  1139,  1143,  1144,  1146,  1147,  1151,  1161,
    1162,  1164,  1165,  1166,  1167,  1168,  1169,  1170,  1173,  1174,
    1176,  1177,  1182,  1183,  1187,  1188,  1190,  1191,  1193,  1194,
    1199,  1200,  1201,  1202,  1205,  1206,  1207,  1208,  1209,  1211,
    1213,  1214,  1215,  1217,  1220,  1221,  1222,  1225,  1226,  1227,
    1228,  1229,  1230,  1235,  1236,  1239,  1240,  1245,  1246,  1247,
    1252,  1253,  1271,  1272,  1273,  1274,  1275,  1276,  1277,  1279,
    1280,  1293,  1295,  1305,  1307,  1308,  1311,  1312,  1313,  1314,
    1316,  1317,  1319,  1320,  1321,  1323,  1325,  1326,  1328,  1329,
    1338,  1340,  1342,  1344,  1346,  1347,  1350,  1351,  1353,  1354,
    1355,  1356,  1361,  1362,  1364,  1365,  1366,  1371,  1372,  1374,
    1375,  1376,  1378,  1379,  1380,  1381,  1384,  1385,  1417,  1418,
    1420,  1421,  1423,  1424,  1425,  1427,  1428,  1430,  1431,  1432,
    1433,  1435,  1436,  1438,  1439,  1441,  1442,  1445,  1446,  1447,
    1449,  1450,  1451,  1452,  1453,  1455,  1456,  1458,  1459,  1460,
    1461,  1464,  1465,  1467,  1469,  1470,  1474,  1476,  1477,  1478,
    1480,  1481,  1482,  1483,  1488,  1489,  1491,  1492,  1494,  1495,
    1498,  1499,  1504,  1505,  1507,  1508,  1512,  1514,  1516,  1517,
    1518,  1519,  1520,  1523,  1524,  1526,  1527,  1528,  1530,  1532,
    1533,  1535,  1536,  1537,  1538,  1539,  1540,  1541,  1542,  1544,
    1545,  1547,  1548,  1550,  1552,  1553,  1555,  1556,  1558,  1559,
    1560,  1561,  1562,  1563,  1564,  1565,  1566,  1567,  1568,  1569,
    1570,  1571,  1572,  1573,  1574,  1575,  1577,  1578,  1582,  1583,
    1585,  1587,  1588,  1590,  1591,  1595,  1596,  1597,  1598,  1599,
    1604,  1607,  1611,  1612,  1614,  1615
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
#line 7541 "parser.cc"

#line 1624 "parser.y"


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
                                           const Hs::ConstructorsDecl& constrs)
{
    auto [con, type_args] = check_type_or_class_header2(header);
    if (d_or_n == Hs::DataOrNewtype::newtype and constrs.size() != 1)
        throw myexception()<<"newtype '"<<con<<"' may only have 1 constructors with 1 field";
    return {con, check_all_type_vars(type_args), Hs::DataDefn(d_or_n, context, k, constrs)};
}

Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context&  context,
                                           const Hs::LType& header, const std::optional<Hs::Kind>& k, const Hs::GADTConstructorsDecl& constrs)
{
    auto [con, type_args] = check_type_or_class_header2(header);
    if (d_or_n == Hs::DataOrNewtype::newtype)
    {
        if (constrs.size() != 1 or constrs[0].con_names.size() != 1)
            throw myexception()<<"newtype '"<<con<<"' may only have 1 constructors with 1 field";
    }

    return {con, check_all_type_vars(type_args), Hs::DataDefn(d_or_n, context, k, constrs)};
}

Hs::InstanceDecl make_instance_decl(const std::optional<std::string>& oprag, const Hs::LType& polytype, const optional<Located<Hs::Decls>>& decls)
{
    std::vector<Hs::TypeFamilyInstanceDecl> type_inst_decls;
    Hs::Decls method_decls;
    if (decls)
        for(auto& [loc,decl]: unloc(*decls))
        {
            if (auto TI = decl.to<Hs::TypeFamilyInstanceDecl>())
                type_inst_decls.push_back(*TI);
            else if (auto V = decl.to<Hs::ValueDecl>())
                method_decls.push_back({loc,*V});
            else
                throw myexception()<<"In declaration of instance "<<unloc(polytype).print()<<", I don't recognize declaration:\n   "<<decl.print();
        }
    return {oprag, polytype, type_inst_decls, method_decls};
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
