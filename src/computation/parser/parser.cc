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
#line 514 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2586 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 531 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < Located<std::string> > (),yystack_[2].value.as < std::optional<std::vector<Hs::LExport>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2592 "parser.cc"
    break;

  case 4: // module: body2
#line 532 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{{noloc,"Main"},{},yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2598 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 534 "parser.y"
                                                                 {drv.push_module_context();}
#line 2604 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 542 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2610 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 543 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2616 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 545 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2622 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 546 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2628 "parser.cc"
    break;

  case 13: // top: semis top1
#line 549 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2634 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 551 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2640 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 552 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2646 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 553 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::LImpDecl> > (),{});}
#line 2652 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 561 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::LExport>> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2658 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 562 "parser.y"
                                      {}
#line 2664 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 564 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2670 "parser.cc"
    break;

  case 20: // exportlist: %empty
#line 565 "parser.y"
                                      {}
#line 2676 "parser.cc"
    break;

  case 21: // exportlist: exportlist1 ','
#line 566 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2682 "parser.cc"
    break;

  case 22: // exportlist: ','
#line 567 "parser.y"
                                      {}
#line 2688 "parser.cc"
    break;

  case 23: // exportlist1: exportlist1 "," export
#line 569 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2694 "parser.cc"
    break;

  case 24: // exportlist1: export
#line 570 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2700 "parser.cc"
    break;

  case 25: // export: qcname export_subspec
#line 572 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2706 "parser.cc"
    break;

  case 26: // export: "module" modid
#line 573 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2712 "parser.cc"
    break;

  case 27: // export: "default" modid
#line 574 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::default_}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2718 "parser.cc"
    break;

  case 28: // export_subspec: %empty
#line 576 "parser.y"
                                      {}
#line 2724 "parser.cc"
    break;

  case 29: // export_subspec: "(" qcnames ")"
#line 577 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2730 "parser.cc"
    break;

  case 30: // export_subspec: "(" ".." ")"
#line 578 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec(); }
#line 2736 "parser.cc"
    break;

  case 31: // qcnames: %empty
#line 580 "parser.y"
                   {}
#line 2742 "parser.cc"
    break;

  case 32: // qcnames: qcnames1
#line 581 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2748 "parser.cc"
    break;

  case 33: // qcnames1: qcnames1 "," qcname
#line 583 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2754 "parser.cc"
    break;

  case 34: // qcnames1: qcname
#line 584 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2760 "parser.cc"
    break;

  case 35: // qcname: qvar
#line 586 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2766 "parser.cc"
    break;

  case 36: // qcname: oqtycon_no_varcon
#line 587 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2772 "parser.cc"
    break;

  case 41: // importdecls: importdecls_semi importdecl
#line 597 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[1].value.as < std::vector<Hs::LImpDecl> > (), yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[0].value.as < Hs::LImpDecl > ()); }
#line 2778 "parser.cc"
    break;

  case 42: // importdecls_semi: importdecls_semi importdecl semis1
#line 599 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[2].value.as < std::vector<Hs::LImpDecl> > (); yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[1].value.as < Hs::LImpDecl > ()); }
#line 2784 "parser.cc"
    break;

  case 43: // importdecls_semi: %empty
#line 600 "parser.y"
                         { }
#line 2790 "parser.cc"
    break;

  case 44: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 602 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::LImpDecl > () = {yylhs.location, Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < Located<std::string> > (),yystack_[1].value.as < std::optional<Located<std::string>> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ())};
}
#line 2798 "parser.cc"
    break;

  case 45: // optqualified: "qualified"
#line 615 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2804 "parser.cc"
    break;

  case 46: // optqualified: %empty
#line 616 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2810 "parser.cc"
    break;

  case 47: // maybeas: "as" modid
#line 618 "parser.y"
                               { yylhs.value.as < std::optional<Located<std::string>> > () = yystack_[0].value.as < Located<std::string> > (); }
#line 2816 "parser.cc"
    break;

  case 48: // maybeas: %empty
#line 619 "parser.y"
                               { }
#line 2822 "parser.cc"
    break;

  case 49: // maybeimpspec: impspec
#line 621 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2828 "parser.cc"
    break;

  case 50: // maybeimpspec: %empty
#line 622 "parser.y"
                               { }
#line 2834 "parser.cc"
    break;

  case 51: // impspec: "(" importlist ")"
#line 626 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2840 "parser.cc"
    break;

  case 52: // impspec: "hiding" "(" importlist ")"
#line 627 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2846 "parser.cc"
    break;

  case 53: // importlist: importlist1
#line 629 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2852 "parser.cc"
    break;

  case 54: // importlist: %empty
#line 630 "parser.y"
                                      {}
#line 2858 "parser.cc"
    break;

  case 55: // importlist: importlist1 ','
#line 631 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2864 "parser.cc"
    break;

  case 56: // importlist: ','
#line 632 "parser.y"
                                      {}
#line 2870 "parser.cc"
    break;

  case 57: // importlist1: importlist1 "," import
#line 634 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2876 "parser.cc"
    break;

  case 58: // importlist1: import
#line 635 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2882 "parser.cc"
    break;

  case 59: // import: qcname export_subspec
#line 637 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2888 "parser.cc"
    break;

  case 60: // import: "module" modid
#line 638 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2894 "parser.cc"
    break;

  case 61: // prec: %empty
#line 643 "parser.y"
                   { }
#line 2900 "parser.cc"
    break;

  case 62: // prec: "INTEGER"
#line 644 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < integer > ().convert_to<int>(); }
#line 2906 "parser.cc"
    break;

  case 63: // infix: "infix"
#line 646 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2912 "parser.cc"
    break;

  case 64: // infix: "infixl"
#line 647 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2918 "parser.cc"
    break;

  case 65: // infix: "infixr"
#line 648 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2924 "parser.cc"
    break;

  case 66: // ops: ops "," op
#line 650 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 2930 "parser.cc"
    break;

  case 67: // ops: op
#line 651 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = {{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2936 "parser.cc"
    break;

  case 68: // topdecls: topdecls_semi topdecl
#line 655 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ()); }
#line 2942 "parser.cc"
    break;

  case 69: // topdecls_semi: topdecls_semi topdecl semis1
#line 657 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 2948 "parser.cc"
    break;

  case 70: // topdecls_semi: %empty
#line 658 "parser.y"
                                            { }
#line 2954 "parser.cc"
    break;

  case 71: // topdecl: cl_decl
#line 660 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2960 "parser.cc"
    break;

  case 72: // topdecl: ty_decl
#line 661 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2966 "parser.cc"
    break;

  case 73: // topdecl: standalone_kind_sig
#line 662 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2972 "parser.cc"
    break;

  case 74: // topdecl: inst_decl
#line 663 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2978 "parser.cc"
    break;

  case 75: // topdecl: "default" opt_class "(" comma_types0 ")"
#line 666 "parser.y"
                                                         {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::DefaultDecl(yystack_[3].value.as < std::optional<Located<std::string>> > (),yystack_[1].value.as < std::vector<Hs::LType> > ())}; }
#line 2984 "parser.cc"
    break;

  case 76: // topdecl: "foreign" "import" call_conv "STRING" var "::" sigtypedoc
#line 667 "parser.y"
                                                                  {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ForeignDecl(yystack_[4].value.as < Located<std::string> > (), yystack_[3].value.as < std::string > (), {yystack_[2].location,yystack_[2].value.as < std::string > ()}, yystack_[0].value.as < Hs::LType > ())};}
#line 2990 "parser.cc"
    break;

  case 77: // topdecl: decl_no_th
#line 673 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2996 "parser.cc"
    break;

  case 78: // topdecl: infixexp
#line 675 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<Hs::InfixExp> > ();}
#line 3002 "parser.cc"
    break;

  case 79: // call_conv: "bpcall"
#line 678 "parser.y"
                    {yylhs.value.as < Located<std::string> > () = {yylhs.location,"bpcall"};}
#line 3008 "parser.cc"
    break;

  case 80: // call_conv: "trcall"
#line 679 "parser.y"
                    {yylhs.value.as < Located<std::string> > () = {yylhs.location,"trcall"};}
#line 3014 "parser.cc"
    break;

  case 81: // call_conv: "ecall"
#line 680 "parser.y"
                    {yylhs.value.as < Located<std::string> > () = {yylhs.location,"ecall"};}
#line 3020 "parser.cc"
    break;

  case 82: // cl_decl: "class" tycl_hdr fds where_cls
#line 682 "parser.y"
                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_class_decl(yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first,yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,yystack_[1].value.as < std::vector<Hs::FunDep> > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 3026 "parser.cc"
    break;

  case 83: // ty_decl: "type" type "=" ktype
#line 685 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_type_synonym(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3032 "parser.cc"
    break;

  case 84: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 686 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > ())};}
#line 3038 "parser.cc"
    break;

  case 85: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 688 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ())};}
#line 3044 "parser.cc"
    break;

  case 86: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 689 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[3].value.as < Hs::LType > (), yystack_[2].value.as < std::optional<Located<Hs::Kind>> > (), yystack_[0].value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ())};}
#line 3050 "parser.cc"
    break;

  case 87: // ty_decl: "data" "family" type opt_datafam_kind_sig
#line 690 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})};}
#line 3056 "parser.cc"
    break;

  case 88: // standalone_kind_sig: "type" sks_vars "::" kind
#line 692 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::KindSigDecl(yystack_[2].value.as < std::vector<Hs::LTypeCon> > (),yystack_[0].value.as < Hs::Kind > ())};}
#line 3062 "parser.cc"
    break;

  case 89: // sks_vars: sks_vars "," oqtycon
#line 694 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = yystack_[2].value.as < std::vector<Hs::LTypeCon> > (); yylhs.value.as < std::vector<Hs::LTypeCon> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}); }
#line 3068 "parser.cc"
    break;

  case 90: // sks_vars: oqtycon
#line 695 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}}; }
#line 3074 "parser.cc"
    break;

  case 91: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 698 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_instance_decl(yystack_[2].value.as < std::optional<std::string> > (),yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 3080 "parser.cc"
    break;

  case 92: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 699 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};}
#line 3086 "parser.cc"
    break;

  case 93: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 701 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), {}, {}, yystack_[1].value.as < Hs::ConstructorsDecl > ()))};
	   }
#line 3096 "parser.cc"
    break;

  case 94: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 707 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ()))};
	   }
#line 3106 "parser.cc"
    break;

  case 95: // overlap_pragma: "{-# OVERLAPPABLE" "#-}"
#line 713 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPABLE"; }
#line 3112 "parser.cc"
    break;

  case 96: // overlap_pragma: "{-# OVERLAPPING" "#-}"
#line 714 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPING"; }
#line 3118 "parser.cc"
    break;

  case 97: // overlap_pragma: "{-# OVERLAPS" "#-}"
#line 715 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPS"; }
#line 3124 "parser.cc"
    break;

  case 98: // overlap_pragma: "{-# INCOHERENT" "#-}"
#line 716 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "INCOHERENT"; }
#line 3130 "parser.cc"
    break;

  case 99: // overlap_pragma: %empty
#line 717 "parser.y"
                                               {}
#line 3136 "parser.cc"
    break;

  case 109: // where_type_family: %empty
#line 744 "parser.y"
                                                           {}
#line 3142 "parser.cc"
    break;

  case 110: // where_type_family: "where" ty_fam_inst_eqn_list
#line 745 "parser.y"
                                                           {yylhs.value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > () = yystack_[0].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3148 "parser.cc"
    break;

  case 111: // ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
#line 747 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3154 "parser.cc"
    break;

  case 112: // ty_fam_inst_eqn_list: "vocurly" ty_fam_inst_eqns close
#line 748 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3160 "parser.cc"
    break;

  case 113: // ty_fam_inst_eqn_list: "{" ".." "}"
#line 749 "parser.y"
                                                           {}
#line 3166 "parser.cc"
    break;

  case 114: // ty_fam_inst_eqn_list: "vocurly" ".." close
#line 750 "parser.y"
                                                           {}
#line 3172 "parser.cc"
    break;

  case 115: // ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
#line 752 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[2].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > (); yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ().push_back(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 3178 "parser.cc"
    break;

  case 116: // ty_fam_inst_eqns: ty_fam_inst_eqns ";"
#line 753 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3184 "parser.cc"
    break;

  case 117: // ty_fam_inst_eqns: ty_fam_inst_eqn
#line 754 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = {yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ()};}
#line 3190 "parser.cc"
    break;

  case 118: // ty_fam_inst_eqns: %empty
#line 755 "parser.y"
                                                           {}
#line 3196 "parser.cc"
    break;

  case 119: // ty_fam_inst_eqn: type "=" ctype
#line 757 "parser.y"
                                                           {yylhs.value.as < Hs::TypeFamilyInstanceEqn > () = make_type_family_instance_eqn(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ());}
#line 3202 "parser.cc"
    break;

  case 120: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 760 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3208 "parser.cc"
    break;

  case 121: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 762 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3214 "parser.cc"
    break;

  case 122: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 764 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3220 "parser.cc"
    break;

  case 123: // at_decl_cls: "type" ty_fam_inst_eqn
#line 766 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3226 "parser.cc"
    break;

  case 124: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 767 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3232 "parser.cc"
    break;

  case 129: // at_decl_inst: "type" opt_instance ty_fam_inst_eqn
#line 775 "parser.y"
                                                              { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3238 "parser.cc"
    break;

  case 130: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 778 "parser.y"
              {
		  auto& [tvs, context, type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), {}, {}, yystack_[1].value.as < Hs::ConstructorsDecl > ()))};
	      }
#line 3248 "parser.cc"
    break;

  case 131: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 785 "parser.y"
              {
		  auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ()))};
	      }
#line 3258 "parser.cc"
    break;

  case 132: // data_or_newtype: "data"
#line 791 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 3264 "parser.cc"
    break;

  case 133: // data_or_newtype: "newtype"
#line 792 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 3270 "parser.cc"
    break;

  case 134: // opt_class: %empty
#line 795 "parser.y"
                  {yylhs.value.as < std::optional<Located<std::string>> > () = {};}
#line 3276 "parser.cc"
    break;

  case 135: // opt_class: qtycon
#line 796 "parser.y"
                  {yylhs.value.as < std::optional<Located<std::string>> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()};}
#line 3282 "parser.cc"
    break;

  case 136: // opt_kind_sig: %empty
#line 800 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 3288 "parser.cc"
    break;

  case 137: // opt_kind_sig: "::" kind
#line 801 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < Hs::Kind > ();}
#line 3294 "parser.cc"
    break;

  case 138: // opt_datafam_kind_sig: %empty
#line 803 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {};}
#line 3300 "parser.cc"
    break;

  case 139: // opt_datafam_kind_sig: "::" kind
#line 804 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3306 "parser.cc"
    break;

  case 140: // opt_tyfam_kind_sig: %empty
#line 806 "parser.y"
                                      {}
#line 3312 "parser.cc"
    break;

  case 141: // opt_tyfam_kind_sig: "::" kind
#line 807 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3318 "parser.cc"
    break;

  case 142: // opt_tyfam_kind_sig: "=" tv_bndr
#line 808 "parser.y"
                                      {}
#line 3324 "parser.cc"
    break;

  case 143: // opt_at_kind_inj_sig: %empty
#line 810 "parser.y"
                                      {}
#line 3330 "parser.cc"
    break;

  case 144: // opt_at_kind_inj_sig: "::" kind
#line 811 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3336 "parser.cc"
    break;

  case 145: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 812 "parser.y"
                                                                  {}
#line 3342 "parser.cc"
    break;

  case 146: // tycl_hdr: context "=>" type
#line 815 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ()};}
#line 3348 "parser.cc"
    break;

  case 147: // tycl_hdr: type
#line 816 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {{},yystack_[0].value.as < Hs::LType > ()};}
#line 3354 "parser.cc"
    break;

  case 148: // datafam_inst_hdr: "forall" tv_bndrs "." context "=>" type
#line 819 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[4].value.as < std::vector<Hs::LTypeVar> > ()}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3360 "parser.cc"
    break;

  case 149: // datafam_inst_hdr: "forall" tv_bndrs "=>" type
#line 820 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[2].value.as < std::vector<Hs::LTypeVar> > ()}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3366 "parser.cc"
    break;

  case 150: // datafam_inst_hdr: context "=>" type
#line 821 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3372 "parser.cc"
    break;

  case 151: // datafam_inst_hdr: type
#line 822 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3378 "parser.cc"
    break;

  case 155: // decl_cls: at_decl_cls
#line 868 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3384 "parser.cc"
    break;

  case 156: // decl_cls: decl
#line 869 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3390 "parser.cc"
    break;

  case 157: // decls_cls: decls_cls ";" decl_cls
#line 871 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3396 "parser.cc"
    break;

  case 158: // decls_cls: decls_cls ";"
#line 872 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3402 "parser.cc"
    break;

  case 159: // decls_cls: decl_cls
#line 873 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3408 "parser.cc"
    break;

  case 160: // decls_cls: %empty
#line 874 "parser.y"
                                           {}
#line 3414 "parser.cc"
    break;

  case 161: // decllist_cls: "{" decls_cls "}"
#line 876 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3420 "parser.cc"
    break;

  case 162: // decllist_cls: "vocurly" decls_cls close
#line 877 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3426 "parser.cc"
    break;

  case 163: // where_cls: "where" decllist_cls
#line 879 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3432 "parser.cc"
    break;

  case 164: // where_cls: %empty
#line 880 "parser.y"
                                           {}
#line 3438 "parser.cc"
    break;

  case 165: // decl_inst: at_decl_inst
#line 882 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3444 "parser.cc"
    break;

  case 166: // decl_inst: decl
#line 883 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3450 "parser.cc"
    break;

  case 167: // decls_inst: decls_inst ";" decl_inst
#line 885 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3456 "parser.cc"
    break;

  case 168: // decls_inst: decls_inst ";"
#line 886 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3462 "parser.cc"
    break;

  case 169: // decls_inst: decl_inst
#line 887 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3468 "parser.cc"
    break;

  case 170: // decls_inst: %empty
#line 888 "parser.y"
                                           {}
#line 3474 "parser.cc"
    break;

  case 171: // decllist_inst: "{" decls_inst "}"
#line 890 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3480 "parser.cc"
    break;

  case 172: // decllist_inst: "vocurly" decls_inst close
#line 891 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3486 "parser.cc"
    break;

  case 173: // where_inst: "where" decllist_inst
#line 893 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3492 "parser.cc"
    break;

  case 174: // where_inst: %empty
#line 894 "parser.y"
                                           {}
#line 3498 "parser.cc"
    break;

  case 175: // decls: decls ";" decl
#line 897 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3504 "parser.cc"
    break;

  case 176: // decls: decls ";"
#line 898 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3510 "parser.cc"
    break;

  case 177: // decls: decl
#line 899 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3516 "parser.cc"
    break;

  case 178: // decls: %empty
#line 900 "parser.y"
                        {}
#line 3522 "parser.cc"
    break;

  case 179: // decllist: "{" decls "}"
#line 902 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3528 "parser.cc"
    break;

  case 180: // decllist: "vocurly" decls close
#line 903 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3534 "parser.cc"
    break;

  case 181: // binds: decllist
#line 905 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 3540 "parser.cc"
    break;

  case 182: // wherebinds: "where" binds
#line 907 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 3546 "parser.cc"
    break;

  case 183: // wherebinds: %empty
#line 908 "parser.y"
                                 {}
#line 3552 "parser.cc"
    break;

  case 189: // opt_tyconsig: %empty
#line 934 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {};}
#line 3558 "parser.cc"
    break;

  case 190: // opt_tyconsig: "::" gtycon
#line 935 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}};}
#line 3564 "parser.cc"
    break;

  case 191: // sigtype: ctype
#line 944 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3570 "parser.cc"
    break;

  case 192: // sigtypedoc: ctypedoc
#line 946 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3576 "parser.cc"
    break;

  case 193: // sig_vars: sig_vars "," var
#line 948 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > () = yystack_[2].value.as < std::vector<Hs::LVar> > (); yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3582 "parser.cc"
    break;

  case 194: // sig_vars: var
#line 949 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3588 "parser.cc"
    break;

  case 195: // sigtypes1: sigtype
#line 951 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3594 "parser.cc"
    break;

  case 196: // sigtypes1: sigtypes1 "," sigtype
#line 952 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3600 "parser.cc"
    break;

  case 197: // ktype: ctype
#line 961 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3606 "parser.cc"
    break;

  case 198: // ktype: ctype "::" kind
#line 962 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeOfKind(yystack_[2].value.as < Hs::LType > (), yystack_[0].value.as < Hs::Kind > ())};}
#line 3612 "parser.cc"
    break;

  case 199: // ctype: "forall" tv_bndrs "." ctype
#line 964 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ForallType(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3618 "parser.cc"
    break;

  case 200: // ctype: context "=>" ctype
#line 965 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3624 "parser.cc"
    break;

  case 201: // ctype: type
#line 967 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3630 "parser.cc"
    break;

  case 202: // ctypedoc: ctype
#line 969 "parser.y"
          { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3636 "parser.cc"
    break;

  case 203: // context: btype
#line 978 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::LType > ());}
#line 3642 "parser.cc"
    break;

  case 204: // context_no_ops: btype_no_ops
#line 980 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ()));}
#line 3648 "parser.cc"
    break;

  case 205: // type: btype
#line 982 "parser.y"
      { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3654 "parser.cc"
    break;

  case 206: // type: btype "->" ctype
#line 983 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = Hs::type_apply({{yystack_[1].location,Hs::TypeCon("->")},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3660 "parser.cc"
    break;

  case 207: // typedoc: type
#line 985 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3666 "parser.cc"
    break;

  case 208: // btype: infixtype
#line 988 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3672 "parser.cc"
    break;

  case 209: // infixtype: ftype
#line 990 "parser.y"
           { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3678 "parser.cc"
    break;

  case 210: // infixtype: btype tyop btype
#line 991 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = Hs::type_apply({{yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3684 "parser.cc"
    break;

  case 211: // btype_no_ops: atype_docs
#line 993 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3690 "parser.cc"
    break;

  case 212: // btype_no_ops: btype_no_ops atype_docs
#line 994 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3696 "parser.cc"
    break;

  case 213: // ftype: atype
#line 996 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3702 "parser.cc"
    break;

  case 214: // ftype: ftype tyarg
#line 998 "parser.y"
                                   { yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeApp(yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())}; }
#line 3708 "parser.cc"
    break;

  case 215: // ftype: ftype "@" atype
#line 999 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[2].value.as < Hs::LType > (); }
#line 3714 "parser.cc"
    break;

  case 216: // tyarg: atype
#line 1001 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3720 "parser.cc"
    break;

  case 217: // tyop: qtyconop
#line 1003 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3726 "parser.cc"
    break;

  case 218: // tyop: tyvarop
#line 1004 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3732 "parser.cc"
    break;

  case 219: // atype_docs: atype
#line 1011 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3738 "parser.cc"
    break;

  case 220: // atype: ntgtycon
#line 1018 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon(yystack_[0].value.as < std::string > ())};}
#line 3744 "parser.cc"
    break;

  case 221: // atype: tyvar
#line 1019 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3750 "parser.cc"
    break;

  case 222: // atype: "*"
#line 1020 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("*")};}
#line 3756 "parser.cc"
    break;

  case 223: // atype: PREFIX_BANG atype
#line 1021 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::StrictType(yystack_[0].value.as < Hs::LType > ())};}
#line 3762 "parser.cc"
    break;

  case 224: // atype: PREFIX_TILDE atype
#line 1022 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::LazyType(yystack_[0].value.as < Hs::LType > ())};}
#line 3768 "parser.cc"
    break;

  case 225: // atype: "{" fielddecls "}"
#line 1023 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ())};}
#line 3774 "parser.cc"
    break;

  case 226: // atype: "(" ")"
#line 1024 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("()")};}
#line 3780 "parser.cc"
    break;

  case 227: // atype: "(" comma_types1 "," ktype ")"
#line 1025 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::LType> > ();ts.push_back(yystack_[1].value.as < Hs::LType > ());yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TupleType(ts)};}
#line 3786 "parser.cc"
    break;

  case 228: // atype: "[" ktype "]"
#line 1031 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ListType{yystack_[1].value.as < Hs::LType > ()}}; }
#line 3792 "parser.cc"
    break;

  case 229: // atype: "(" ktype ")"
#line 1032 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[1].value.as < Hs::LType > ();}
#line 3798 "parser.cc"
    break;

  case 230: // inst_type: sigtype
#line 1035 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3804 "parser.cc"
    break;

  case 233: // comma_types0: comma_types1
#line 1040 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > ();}
#line 3810 "parser.cc"
    break;

  case 234: // comma_types0: %empty
#line 1041 "parser.y"
                                       { /* default construction OK */ }
#line 3816 "parser.cc"
    break;

  case 235: // comma_types1: ktype
#line 1043 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3822 "parser.cc"
    break;

  case 236: // comma_types1: comma_types1 "," ktype
#line 1044 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3828 "parser.cc"
    break;

  case 237: // tv_bndrs: tv_bndrs tv_bndr
#line 1051 "parser.y"
                               {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back(yystack_[0].value.as < Hs::LTypeVar > ());}
#line 3834 "parser.cc"
    break;

  case 238: // tv_bndrs: %empty
#line 1052 "parser.y"
                               { /* default construction OK */}
#line 3840 "parser.cc"
    break;

  case 239: // tv_bndr: tv_bndr_no_braces
#line 1054 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = yystack_[0].value.as < Hs::LTypeVar > ();}
#line 3846 "parser.cc"
    break;

  case 240: // tv_bndr: "{" tyvar "}"
#line 1055 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[1].location,Hs::TypeVar(yystack_[1].value.as < std::string > ())};}
#line 3852 "parser.cc"
    break;

  case 241: // tv_bndr: "{" tyvar "::" kind "}"
#line 1056 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[3].location,Hs::TypeVar(yystack_[3].value.as < std::string > ())};}
#line 3858 "parser.cc"
    break;

  case 242: // tv_bndr_no_braces: tyvar
#line 1059 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3864 "parser.cc"
    break;

  case 243: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 1060 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[3].value.as < std::string > (),yystack_[1].value.as < Hs::Kind > ())};}
#line 3870 "parser.cc"
    break;

  case 244: // fds: %empty
#line 1064 "parser.y"
                                    { /* default to empty */ }
#line 3876 "parser.cc"
    break;

  case 245: // fds: "|" fds1
#line 1065 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > () = yystack_[0].value.as < std::vector<Hs::FunDep> > (); }
#line 3882 "parser.cc"
    break;

  case 246: // fds1: fds1 "," fd
#line 1067 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > () = yystack_[2].value.as < std::vector<Hs::FunDep> > (); yylhs.value.as < std::vector<Hs::FunDep> > ().push_back(yystack_[0].value.as < Hs::FunDep > ()); }
#line 3888 "parser.cc"
    break;

  case 247: // fds1: fd
#line 1068 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > ().push_back(yystack_[0].value.as < Hs::FunDep > ()); }
#line 3894 "parser.cc"
    break;

  case 248: // fd: varids0 "->" varids0
#line 1071 "parser.y"
                                    { yylhs.value.as < Hs::FunDep > () = Hs::FunDep(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < std::vector<Hs::LTypeVar> > ()); }
#line 3900 "parser.cc"
    break;

  case 249: // varids0: varids0 tyvar
#line 1073 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3906 "parser.cc"
    break;

  case 250: // varids0: %empty
#line 1074 "parser.y"
                                    { /* default to empty */}
#line 3912 "parser.cc"
    break;

  case 251: // kind: ctype
#line 1079 "parser.y"
             {yylhs.value.as < Hs::Kind > () = type_to_kind(yystack_[0].value.as < Hs::LType > ());}
#line 3918 "parser.cc"
    break;

  case 252: // gadt_constrlist: "where" "{" gadt_constrs "}"
#line 1085 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3924 "parser.cc"
    break;

  case 253: // gadt_constrlist: "where" "vocurly" gadt_constrs close
#line 1086 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3930 "parser.cc"
    break;

  case 254: // gadt_constrlist: %empty
#line 1087 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = {};}
#line 3936 "parser.cc"
    break;

  case 255: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1089 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3942 "parser.cc"
    break;

  case 256: // gadt_constrs: gadt_constr
#line 1090 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3948 "parser.cc"
    break;

  case 257: // gadt_constr: optSemi con_list "::" sigtype
#line 1092 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),yystack_[0].value.as < Hs::LType > ());}
#line 3954 "parser.cc"
    break;

  case 258: // constrs: "=" constrs1
#line 1094 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 3960 "parser.cc"
    break;

  case 259: // constrs1: constrs1 "|" constr
#line 1096 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3966 "parser.cc"
    break;

  case 260: // constrs1: constr
#line 1097 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3972 "parser.cc"
    break;

  case 261: // constr: forall context_no_ops "=>" constr_stuff
#line 1099 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::LTypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::LType > ());}
#line 3978 "parser.cc"
    break;

  case 262: // constr: forall constr_stuff
#line 1100 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::LTypeVar> > (),{}, yystack_[0].value.as < Hs::LType > ());}
#line 3984 "parser.cc"
    break;

  case 263: // forall: "forall" tv_bndrs "."
#line 1102 "parser.y"
                                {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > ();}
#line 3990 "parser.cc"
    break;

  case 264: // forall: %empty
#line 1103 "parser.y"
                                {}
#line 3996 "parser.cc"
    break;

  case 265: // constr_stuff: btype_no_ops
#line 1105 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ());}
#line 4002 "parser.cc"
    break;

  case 266: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1106 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::type_apply({
                                                                          {yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},
                                                                          Hs::type_apply(yystack_[2].value.as < std::vector<Hs::LType> > ()),
                                                                          Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ())
                                                                       });}
#line 4012 "parser.cc"
    break;

  case 267: // fielddecls: %empty
#line 1112 "parser.y"
                                {}
#line 4018 "parser.cc"
    break;

  case 268: // fielddecls: fielddecls1
#line 1113 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 4024 "parser.cc"
    break;

  case 269: // fielddecls1: fielddecls1 "," fielddecl
#line 1115 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 4030 "parser.cc"
    break;

  case 270: // fielddecls1: fielddecl
#line 1116 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 4036 "parser.cc"
    break;

  case 271: // fielddecl: sig_vars "::" ctype
#line 1118 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ());}
#line 4042 "parser.cc"
    break;

  case 282: // decl_no_th: sigdecl
#line 1137 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4048 "parser.cc"
    break;

  case 283: // decl_no_th: infixexp rhs
#line 1139 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ValueDecl({yystack_[1].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ())};}
#line 4054 "parser.cc"
    break;

  case 284: // decl: decl_no_th
#line 1141 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4060 "parser.cc"
    break;

  case 285: // rhs: "=" exp wherebinds
#line 1145 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4066 "parser.cc"
    break;

  case 286: // rhs: gdrhs wherebinds
#line 1146 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 4072 "parser.cc"
    break;

  case 287: // gdrhs: gdrhs gdrh
#line 1148 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4078 "parser.cc"
    break;

  case 288: // gdrhs: gdrh
#line 1149 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4084 "parser.cc"
    break;

  case 289: // gdrh: "|" guardquals "=" exp
#line 1153 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4090 "parser.cc"
    break;

  case 290: // sigdecl: sig_vars "::" sigtypedoc
#line 1163 "parser.y"
                                  { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeSigDecl{yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ()}}; }
#line 4096 "parser.cc"
    break;

  case 291: // sigdecl: infix prec ops
#line 1164 "parser.y"
                         { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<Located<std::string>> > ()}}; }
#line 4102 "parser.cc"
    break;

  case 292: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1166 "parser.y"
                                                    {}
#line 4108 "parser.cc"
    break;

  case 293: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1167 "parser.y"
                                            { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::InlinePragma({yystack_[3].location,yystack_[3].value.as < std::string > ()},{yystack_[1].location,yystack_[1].value.as < std::string > ()})}; }
#line 4114 "parser.cc"
    break;

  case 294: // sigdecl: "{-# SCC" qvar "#-}"
#line 1168 "parser.y"
                              {}
#line 4120 "parser.cc"
    break;

  case 295: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1169 "parser.y"
                                     {}
#line 4126 "parser.cc"
    break;

  case 296: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1170 "parser.y"
                                                               {}
#line 4132 "parser.cc"
    break;

  case 297: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1171 "parser.y"
                                                                      {}
#line 4138 "parser.cc"
    break;

  case 298: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1172 "parser.y"
                                                     {}
#line 4144 "parser.cc"
    break;

  case 303: // exp: infixexp "::" sigtype
#line 1184 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[2].location+yystack_[0].location, Hs::TypedExp({yystack_[2].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[2].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::LType > ())}; }
#line 4150 "parser.cc"
    break;

  case 304: // exp: infixexp
#line 1185 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}; }
#line 4156 "parser.cc"
    break;

  case 305: // infixexp: exp10
#line 1189 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location,Hs::InfixExp({yystack_[0].value.as < Located<expression_ref> > ()})};}
#line 4162 "parser.cc"
    break;

  case 306: // infixexp: infixexp qop exp10
#line 1190 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < expression_ref > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4168 "parser.cc"
    break;

  case 307: // exp10: PREFIX_MINUS fexp
#line 1192 "parser.y"
                                        {yylhs.value.as < Located<expression_ref> > () = {yystack_[1].location+yystack_[0].location,Hs::InfixExp( { {yystack_[1].location,Hs::Neg()}, yystack_[0].value.as < Located<expression_ref> > ()} )};}
#line 4174 "parser.cc"
    break;

  case 308: // exp10: fexp
#line 1193 "parser.y"
                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4180 "parser.cc"
    break;

  case 311: // fexp: fexp aexp
#line 1201 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ApplyExp(yystack_[1].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4186 "parser.cc"
    break;

  case 312: // fexp: fexp "@" atype
#line 1202 "parser.y"
                                 {}
#line 4192 "parser.cc"
    break;

  case 313: // fexp: "static" aexp
#line 1203 "parser.y"
                                 {}
#line 4198 "parser.cc"
    break;

  case 314: // fexp: aexp
#line 1204 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4204 "parser.cc"
    break;

  case 315: // aexp: qvar TIGHT_INFIX_AT aexp
#line 1207 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::AsPattern({yystack_[2].location,Hs::Var(yystack_[2].value.as < std::string > ())},yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4210 "parser.cc"
    break;

  case 316: // aexp: PREFIX_TILDE aexp
#line 1208 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LazyPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4216 "parser.cc"
    break;

  case 317: // aexp: PREFIX_BANG aexp
#line 1209 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4222 "parser.cc"
    break;

  case 318: // aexp: "\\" apats1 "->" exp
#line 1210 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LambdaExp(yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4228 "parser.cc"
    break;

  case 319: // aexp: "let" binds "in" exp
#line 1211 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4234 "parser.cc"
    break;

  case 320: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1213 "parser.y"
                                                       {yylhs.value.as < Located<expression_ref> > () = {yystack_[7].location+yystack_[0].location,Hs::IfExp(yystack_[6].value.as < Located<expression_ref> > (),yystack_[3].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4240 "parser.cc"
    break;

  case 321: // aexp: "case" exp "of" altslist
#line 1215 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::CaseExp(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::Alts > ())}; }
#line 4246 "parser.cc"
    break;

  case 322: // aexp: "do" stmtlist
#line 1216 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Do(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4252 "parser.cc"
    break;

  case 323: // aexp: "mdo" stmtlist
#line 1217 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::MDo(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4258 "parser.cc"
    break;

  case 324: // aexp: aexp1
#line 1219 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4264 "parser.cc"
    break;

  case 325: // aexp1: aexp1 "{" fbinds "}"
#line 1222 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::RecordExp{yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<Hs::FieldBindings> > ()}}; }
#line 4270 "parser.cc"
    break;

  case 326: // aexp1: aexp1 TIGHT_INFIX_DOT field
#line 1223 "parser.y"
                                     { }
#line 4276 "parser.cc"
    break;

  case 327: // aexp1: aexp2
#line 1224 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > (); }
#line 4282 "parser.cc"
    break;

  case 328: // aexp2: qvar
#line 1227 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Var(yystack_[0].value.as < std::string > ())};}
#line 4288 "parser.cc"
    break;

  case 329: // aexp2: qcon
#line 1228 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Con(yystack_[0].value.as < std::string > ())};}
#line 4294 "parser.cc"
    break;

  case 330: // aexp2: literal
#line 1229 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[0].value.as < expression_ref > ()};}
#line 4300 "parser.cc"
    break;

  case 331: // aexp2: "(" texp ")"
#line 1230 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, unloc(yystack_[1].value.as < Located<expression_ref> > ())};}
#line 4306 "parser.cc"
    break;

  case 332: // aexp2: "(" tup_exprs ")"
#line 1231 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Tuple(yystack_[1].value.as < std::vector<Located<expression_ref>> > ())};}
#line 4312 "parser.cc"
    break;

  case 333: // aexp2: "(" projection ")"
#line 1232 "parser.y"
                              {}
#line 4318 "parser.cc"
    break;

  case 334: // aexp2: "[" list "]"
#line 1237 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[1].value.as < expression_ref > ()};}
#line 4324 "parser.cc"
    break;

  case 335: // aexp2: "_"
#line 1238 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::WildcardPattern()};}
#line 4330 "parser.cc"
    break;

  case 338: // texp: exp
#line 1247 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4336 "parser.cc"
    break;

  case 339: // texp: infixexp qop
#line 1248 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LeftSection ( {yystack_[1].value.as < Located<Hs::InfixExp> > ().loc, unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())}, {yystack_[0].location,yystack_[0].value.as < expression_ref > ()} )}; }
#line 4342 "parser.cc"
    break;

  case 340: // texp: qopm infixexp
#line 1249 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RightSection( {yystack_[1].location,yystack_[1].value.as < expression_ref > ()}, {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())} )}; }
#line 4348 "parser.cc"
    break;

  case 341: // tup_exprs: tup_exprs "," texp
#line 1254 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4354 "parser.cc"
    break;

  case 342: // tup_exprs: texp "," texp
#line 1255 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4360 "parser.cc"
    break;

  case 343: // list: texp
#line 1273 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List({yystack_[0].value.as < Located<expression_ref> > ()}); }
#line 4366 "parser.cc"
    break;

  case 344: // list: lexps
#line 1274 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List(yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4372 "parser.cc"
    break;

  case 345: // list: texp ".."
#line 1275 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4378 "parser.cc"
    break;

  case 346: // list: texp "," exp ".."
#line 1276 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4384 "parser.cc"
    break;

  case 347: // list: texp ".." exp
#line 1277 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4390 "parser.cc"
    break;

  case 348: // list: texp "," exp ".." exp
#line 1278 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < Located<expression_ref> > (), yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4396 "parser.cc"
    break;

  case 349: // list: texp "|" squals
#line 1279 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4402 "parser.cc"
    break;

  case 350: // lexps: lexps "," texp
#line 1281 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4408 "parser.cc"
    break;

  case 351: // lexps: texp "," texp
#line 1282 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4414 "parser.cc"
    break;

  case 352: // squals: squals "," qual
#line 1295 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4420 "parser.cc"
    break;

  case 353: // squals: qual
#line 1297 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4426 "parser.cc"
    break;

  case 354: // guardquals: guardquals1
#line 1307 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[0].value.as < std::vector<Located<expression_ref>> > ();}
#line 4432 "parser.cc"
    break;

  case 355: // guardquals1: guardquals1 "," qual
#line 1309 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > ();yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4438 "parser.cc"
    break;

  case 356: // guardquals1: qual
#line 1310 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4444 "parser.cc"
    break;

  case 357: // altslist: "{" alts "}"
#line 1313 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4450 "parser.cc"
    break;

  case 358: // altslist: "vocurly" alts close
#line 1314 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4456 "parser.cc"
    break;

  case 359: // altslist: "{" "}"
#line 1315 "parser.y"
                                 {}
#line 4462 "parser.cc"
    break;

  case 360: // altslist: "vocurly" close
#line 1316 "parser.y"
                                 {}
#line 4468 "parser.cc"
    break;

  case 361: // alts: alts1
#line 1318 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4474 "parser.cc"
    break;

  case 362: // alts: ";" alts
#line 1319 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4480 "parser.cc"
    break;

  case 363: // alts1: alts1 ";" alt
#line 1321 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4486 "parser.cc"
    break;

  case 364: // alts1: alts1 ";"
#line 1322 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4492 "parser.cc"
    break;

  case 365: // alts1: alt
#line 1323 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4498 "parser.cc"
    break;

  case 366: // alt: pat alt_rhs
#line 1325 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 4504 "parser.cc"
    break;

  case 367: // alt_rhs: "->" exp wherebinds
#line 1327 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4510 "parser.cc"
    break;

  case 368: // alt_rhs: gdpats wherebinds
#line 1328 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4516 "parser.cc"
    break;

  case 369: // gdpats: gdpats gdpat
#line 1330 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4522 "parser.cc"
    break;

  case 370: // gdpats: gdpat
#line 1331 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4528 "parser.cc"
    break;

  case 371: // gdpat: "|" guardquals "->" exp
#line 1340 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4534 "parser.cc"
    break;

  case 372: // pat: exp
#line 1342 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4540 "parser.cc"
    break;

  case 373: // bindpat: exp
#line 1344 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4546 "parser.cc"
    break;

  case 374: // apat: aexp
#line 1346 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4552 "parser.cc"
    break;

  case 375: // apats1: apats1 apat
#line 1348 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4558 "parser.cc"
    break;

  case 376: // apats1: apat
#line 1349 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4564 "parser.cc"
    break;

  case 377: // stmtlist: "{" stmts "}"
#line 1352 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4570 "parser.cc"
    break;

  case 378: // stmtlist: "vocurly" stmts close
#line 1353 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4576 "parser.cc"
    break;

  case 379: // stmts: stmts ";" stmt
#line 1355 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4582 "parser.cc"
    break;

  case 380: // stmts: stmts ";"
#line 1356 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 4588 "parser.cc"
    break;

  case 381: // stmts: stmt
#line 1357 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4594 "parser.cc"
    break;

  case 382: // stmts: %empty
#line 1358 "parser.y"
                       {}
#line 4600 "parser.cc"
    break;

  case 383: // stmt: qual
#line 1363 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4606 "parser.cc"
    break;

  case 384: // stmt: "rec" stmtlist
#line 1364 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ())};}
#line 4612 "parser.cc"
    break;

  case 385: // qual: bindpat "<-" exp
#line 1366 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::PatQual(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4618 "parser.cc"
    break;

  case 386: // qual: exp
#line 1367 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::SimpleQual(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4624 "parser.cc"
    break;

  case 387: // qual: "let" binds
#line 1368 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ())};}
#line 4630 "parser.cc"
    break;

  case 388: // fbinds: fbinds1
#line 1373 "parser.y"
                        {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > ();}
#line 4636 "parser.cc"
    break;

  case 389: // fbinds: %empty
#line 1374 "parser.y"
                        {}
#line 4642 "parser.cc"
    break;

  case 390: // fbinds1: fbind "," fbinds1
#line 1376 "parser.y"
                            {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > (); unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).insert(unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).begin(), *yystack_[2].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4648 "parser.cc"
    break;

  case 391: // fbinds1: fbind
#line 1377 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).push_back(*yystack_[0].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4654 "parser.cc"
    break;

  case 392: // fbinds1: ".."
#line 1378 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).dotdot = true; yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4660 "parser.cc"
    break;

  case 393: // fbind: qvar "=" texp
#line 1380 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[2].value.as < std::string > ())}, yystack_[0].value.as < Located<expression_ref> > ())}};}
#line 4666 "parser.cc"
    break;

  case 394: // fbind: qvar
#line 1381 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[0].value.as < std::string > ())})}};}
#line 4672 "parser.cc"
    break;

  case 395: // fbind: field TIGHT_INFIX_DOT fieldToUpdate "=" texp
#line 1382 "parser.y"
                                                      {}
#line 4678 "parser.cc"
    break;

  case 396: // fbind: field TIGHT_INFIX_DOT fieldToUpdate
#line 1383 "parser.y"
                                                      {}
#line 4684 "parser.cc"
    break;

  case 399: // qcon: gen_qcon
#line 1419 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4690 "parser.cc"
    break;

  case 400: // qcon: sysdcon
#line 1420 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4696 "parser.cc"
    break;

  case 401: // gen_qcon: qconid
#line 1422 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4702 "parser.cc"
    break;

  case 402: // gen_qcon: "(" qconsym ")"
#line 1423 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4708 "parser.cc"
    break;

  case 403: // con: conid
#line 1425 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4714 "parser.cc"
    break;

  case 404: // con: "(" consym ")"
#line 1426 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4720 "parser.cc"
    break;

  case 405: // con: sysdcon
#line 1427 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4726 "parser.cc"
    break;

  case 406: // con_list: con_list "," con
#line 1429 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4732 "parser.cc"
    break;

  case 407: // con_list: con
#line 1430 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4738 "parser.cc"
    break;

  case 408: // sysdcon_no_list: "(" ")"
#line 1432 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 4744 "parser.cc"
    break;

  case 409: // sysdcon_no_list: "(" commas ")"
#line 1433 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4750 "parser.cc"
    break;

  case 410: // sysdcon_no_list: "(#" "#)"
#line 1434 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 4756 "parser.cc"
    break;

  case 411: // sysdcon_no_list: "(#" commas "#)"
#line 1435 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4762 "parser.cc"
    break;

  case 412: // sysdcon: sysdcon_no_list
#line 1437 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4768 "parser.cc"
    break;

  case 413: // sysdcon: "[" "]"
#line 1438 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 4774 "parser.cc"
    break;

  case 414: // conop: consym
#line 1440 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4780 "parser.cc"
    break;

  case 415: // conop: "`" conid "`"
#line 1441 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4786 "parser.cc"
    break;

  case 416: // qconop: qconsym
#line 1443 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4792 "parser.cc"
    break;

  case 417: // qconop: "`" qconid "`"
#line 1444 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4798 "parser.cc"
    break;

  case 418: // gtycon: ntgtycon
#line 1447 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4804 "parser.cc"
    break;

  case 419: // gtycon: "(" ")"
#line 1448 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 4810 "parser.cc"
    break;

  case 420: // gtycon: "(#" "#)"
#line 1449 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 4816 "parser.cc"
    break;

  case 421: // ntgtycon: oqtycon
#line 1451 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4822 "parser.cc"
    break;

  case 422: // ntgtycon: "(" commas ")"
#line 1452 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4828 "parser.cc"
    break;

  case 423: // ntgtycon: "(#" commas "#)"
#line 1453 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4834 "parser.cc"
    break;

  case 424: // ntgtycon: "(" "->" ")"
#line 1454 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 4840 "parser.cc"
    break;

  case 425: // ntgtycon: "[" "]"
#line 1455 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 4846 "parser.cc"
    break;

  case 426: // oqtycon: qtycon
#line 1457 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4852 "parser.cc"
    break;

  case 427: // oqtycon: "(" qtyconsym ")"
#line 1458 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4858 "parser.cc"
    break;

  case 428: // oqtycon_no_varcon: qtycon
#line 1460 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4864 "parser.cc"
    break;

  case 429: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1461 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4870 "parser.cc"
    break;

  case 430: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1462 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4876 "parser.cc"
    break;

  case 431: // oqtycon_no_varcon: "(" ":" ")"
#line 1463 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 4882 "parser.cc"
    break;

  case 432: // qtyconop: qtyconsym
#line 1466 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4888 "parser.cc"
    break;

  case 433: // qtyconop: "`" qtycon "`"
#line 1467 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4894 "parser.cc"
    break;

  case 434: // qtycondoc: qtycon
#line 1469 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4900 "parser.cc"
    break;

  case 435: // qtycon: "QCONID"
#line 1471 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4906 "parser.cc"
    break;

  case 436: // qtycon: tycon
#line 1472 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4912 "parser.cc"
    break;

  case 437: // tycon: "CONID"
#line 1476 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4918 "parser.cc"
    break;

  case 438: // qtyconsym: "QCONSYM"
#line 1478 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4924 "parser.cc"
    break;

  case 439: // qtyconsym: "QVARSYM"
#line 1479 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4930 "parser.cc"
    break;

  case 440: // qtyconsym: tyconsym
#line 1480 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4936 "parser.cc"
    break;

  case 441: // tyconsym: "CONSYM"
#line 1482 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4942 "parser.cc"
    break;

  case 442: // tyconsym: "VARSYM"
#line 1483 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4948 "parser.cc"
    break;

  case 443: // tyconsym: ":"
#line 1484 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4954 "parser.cc"
    break;

  case 444: // tyconsym: "-"
#line 1485 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 4960 "parser.cc"
    break;

  case 445: // op: varop
#line 1490 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4966 "parser.cc"
    break;

  case 446: // op: conop
#line 1491 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4972 "parser.cc"
    break;

  case 447: // varop: varsym
#line 1493 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4978 "parser.cc"
    break;

  case 448: // varop: "`" varid "`"
#line 1494 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4984 "parser.cc"
    break;

  case 449: // qop: qvarop
#line 1496 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4990 "parser.cc"
    break;

  case 450: // qop: qconop
#line 1497 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4996 "parser.cc"
    break;

  case 451: // qopm: qvaropm
#line 1500 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 5002 "parser.cc"
    break;

  case 452: // qopm: qconop
#line 1501 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 5008 "parser.cc"
    break;

  case 453: // qvarop: qvarsym
#line 1506 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5014 "parser.cc"
    break;

  case 454: // qvarop: "`" qvarid "`"
#line 1507 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5020 "parser.cc"
    break;

  case 455: // qvaropm: qvarsym_no_minus
#line 1509 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 5026 "parser.cc"
    break;

  case 456: // qvaropm: "`" qvarid "`"
#line 1510 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5032 "parser.cc"
    break;

  case 457: // tyvar: tyvarid
#line 1514 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5038 "parser.cc"
    break;

  case 458: // tyvarop: "`" tyvarid "`"
#line 1516 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5044 "parser.cc"
    break;

  case 459: // tyvarid: "VARID"
#line 1518 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5050 "parser.cc"
    break;

  case 460: // tyvarid: special_id
#line 1519 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5056 "parser.cc"
    break;

  case 461: // tyvarid: "unsafe"
#line 1520 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 5062 "parser.cc"
    break;

  case 462: // tyvarid: "safe"
#line 1521 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 5068 "parser.cc"
    break;

  case 463: // tyvarid: "interruptible"
#line 1522 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 5074 "parser.cc"
    break;

  case 464: // var: varid
#line 1525 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5080 "parser.cc"
    break;

  case 465: // var: "(" varsym ")"
#line 1526 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5086 "parser.cc"
    break;

  case 466: // qvar: qvarid
#line 1528 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5092 "parser.cc"
    break;

  case 467: // qvar: "(" varsym ")"
#line 1529 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5098 "parser.cc"
    break;

  case 468: // qvar: "(" qvarsym1 ")"
#line 1530 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5104 "parser.cc"
    break;

  case 469: // field: varid
#line 1532 "parser.y"
       { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5110 "parser.cc"
    break;

  case 470: // qvarid: varid
#line 1534 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5116 "parser.cc"
    break;

  case 471: // qvarid: "QVARID"
#line 1535 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5122 "parser.cc"
    break;

  case 472: // varid: "VARID"
#line 1537 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5128 "parser.cc"
    break;

  case 473: // varid: special_id
#line 1538 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5134 "parser.cc"
    break;

  case 474: // varid: "unsafe"
#line 1539 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 5140 "parser.cc"
    break;

  case 475: // varid: "safe"
#line 1540 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 5146 "parser.cc"
    break;

  case 476: // varid: "interruptible"
#line 1541 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 5152 "parser.cc"
    break;

  case 477: // varid: "forall"
#line 1542 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 5158 "parser.cc"
    break;

  case 478: // varid: "family"
#line 1543 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 5164 "parser.cc"
    break;

  case 479: // varid: "role"
#line 1544 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 5170 "parser.cc"
    break;

  case 480: // qvarsym: varsym
#line 1546 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5176 "parser.cc"
    break;

  case 481: // qvarsym: qvarsym1
#line 1547 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5182 "parser.cc"
    break;

  case 482: // qvarsym_no_minus: varsym_no_minus
#line 1549 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5188 "parser.cc"
    break;

  case 483: // qvarsym_no_minus: qvarsym1
#line 1550 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5194 "parser.cc"
    break;

  case 484: // qvarsym1: "QVARSYM"
#line 1552 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5200 "parser.cc"
    break;

  case 485: // varsym: varsym_no_minus
#line 1554 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5206 "parser.cc"
    break;

  case 486: // varsym: "-"
#line 1555 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 5212 "parser.cc"
    break;

  case 487: // varsym_no_minus: "VARSYM"
#line 1557 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5218 "parser.cc"
    break;

  case 488: // varsym_no_minus: special_sym
#line 1558 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5224 "parser.cc"
    break;

  case 489: // special_id: "as"
#line 1560 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 5230 "parser.cc"
    break;

  case 490: // special_id: "qualified"
#line 1561 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 5236 "parser.cc"
    break;

  case 491: // special_id: "hiding"
#line 1562 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 5242 "parser.cc"
    break;

  case 492: // special_id: "export"
#line 1563 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 5248 "parser.cc"
    break;

  case 493: // special_id: "label"
#line 1564 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 5254 "parser.cc"
    break;

  case 494: // special_id: "dynamic"
#line 1565 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 5260 "parser.cc"
    break;

  case 495: // special_id: "stdcall"
#line 1566 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 5266 "parser.cc"
    break;

  case 496: // special_id: "ccall"
#line 1567 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 5272 "parser.cc"
    break;

  case 497: // special_id: "capi"
#line 1568 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 5278 "parser.cc"
    break;

  case 498: // special_id: "prim"
#line 1569 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 5284 "parser.cc"
    break;

  case 499: // special_id: "javascript"
#line 1570 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 5290 "parser.cc"
    break;

  case 500: // special_id: "group"
#line 1571 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 5296 "parser.cc"
    break;

  case 501: // special_id: "stock"
#line 1572 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 5302 "parser.cc"
    break;

  case 502: // special_id: "anyclass"
#line 1573 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 5308 "parser.cc"
    break;

  case 503: // special_id: "via"
#line 1574 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 5314 "parser.cc"
    break;

  case 504: // special_id: "unit"
#line 1575 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 5320 "parser.cc"
    break;

  case 505: // special_id: "dependency"
#line 1576 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 5326 "parser.cc"
    break;

  case 506: // special_id: "signature"
#line 1577 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 5332 "parser.cc"
    break;

  case 507: // special_sym: "."
#line 1579 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 5338 "parser.cc"
    break;

  case 508: // special_sym: "*"
#line 1580 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 5344 "parser.cc"
    break;

  case 509: // qconid: conid
#line 1584 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5350 "parser.cc"
    break;

  case 510: // qconid: "QCONID"
#line 1585 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5356 "parser.cc"
    break;

  case 511: // conid: "CONID"
#line 1587 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5362 "parser.cc"
    break;

  case 512: // qconsym: consym
#line 1589 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5368 "parser.cc"
    break;

  case 513: // qconsym: "QCONSYM"
#line 1590 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5374 "parser.cc"
    break;

  case 514: // consym: "CONSYM"
#line 1592 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5380 "parser.cc"
    break;

  case 515: // consym: ":"
#line 1593 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 5386 "parser.cc"
    break;

  case 516: // literal: "CHAR"
#line 1597 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 5392 "parser.cc"
    break;

  case 517: // literal: "STRING"
#line 1598 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 5398 "parser.cc"
    break;

  case 518: // literal: "INTEGER"
#line 1599 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < integer > ()});}
#line 5404 "parser.cc"
    break;

  case 519: // literal: "RATIONAL"
#line 1600 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Floating{yystack_[0].value.as < rational > ()});}
#line 5410 "parser.cc"
    break;

  case 520: // literal: "PRIMINTEGER"
#line 1601 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < integer > ()});}
#line 5416 "parser.cc"
    break;

  case 522: // close: error
#line 1609 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 5422 "parser.cc"
    break;

  case 523: // modid: "CONID"
#line 1613 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5428 "parser.cc"
    break;

  case 524: // modid: "QCONID"
#line 1614 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5434 "parser.cc"
    break;

  case 525: // commas: commas ","
#line 1616 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 5440 "parser.cc"
    break;

  case 526: // commas: ","
#line 1617 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 5446 "parser.cc"
    break;


#line 5450 "parser.cc"

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

  const short parser::yytable_ninf_ = -486;

  const short
  parser::yypact_[] =
  {
      46,   242,  -714,    88,  -714,  -714,  -714,  -714,  -714,   407,
      32,   -12,  -714,    40,   -15,   -15,     3,  -714,  -714,  -714,
    -714,   138,  -714,  -714,  -714,    80,  -714,   155,   177,  1528,
     252,   334,   170,  -714,   963,  -714,   222,  -714,  -714,  -714,
     242,  -714,   242,  -714,  -714,  -714,  -714,  -714,  -714,  -714,
    -714,  -714,  -714,  -714,  -714,  -714,  -714,  -714,  -714,  -714,
    -714,  -714,  -714,  -714,  -714,   753,  -714,  -714,  -714,  -714,
    -714,   280,    68,  -714,   298,  -714,  -714,  -714,  -714,  -714,
    -714,  -714,   219,  -714,   242,  -714,   308,  -714,  2664,  4672,
     398,   287,   357,  2664,  -714,  -714,  -714,   378,   364,  -714,
    3778,   429,   357,  3354,  5080,   293,  3354,  3354,  2940,  3354,
    1974,  1836,   271,   340,  -714,  -714,  -714,  -714,  -714,  -714,
    -714,    37,   340,   329,   170,  -714,  -714,  -714,  -714,    65,
      -9,  -714,  -714,   362,  -714,  3078,  -714,   200,  -714,  -714,
    -714,  -714,  -714,  -714,   409,     6,  -714,  -714,  -714,  -714,
     344,  -714,  -714,   377,  -714,  -714,  -714,  -714,   386,  -714,
     389,   396,   400,  -714,  -714,  -714,  4774,  -714,  4811,  -714,
    -714,  -714,  -714,   517,  -714,  1836,   501,   700,  -714,  -714,
    -714,  4672,  4672,  -714,  5179,  3866,  3468,   408,  -714,   442,
     439,  -714,   479,  -714,  4056,  -714,  -714,  -714,  -714,  -714,
    -714,  -714,  4672,   416,  -714,  2388,  2388,  -714,   415,   457,
     459,   461,   462,  4144,  1393,  1393,  -714,   521,  4672,  4672,
      73,   460,   458,    87,   185,  -714,  -714,   262,   -10,   433,
     109,  -714,    94,  -714,  -714,  -714,  -714,  3216,  -714,  3078,
    -714,  -714,  -714,  4978,  -714,  -714,  -714,   700,    30,   434,
     431,  -714,  2664,  -714,  -714,  -714,  -714,  -714,  -714,  5311,
    -714,  -714,   213,   231,   265,   396,   440,   446,   447,   295,
    -714,   310,   -14,  5080,  -714,  4144,  5080,  5080,  -714,   130,
     308,   477,   418,  4672,  4144,  5179,  2664,  2802,  4978,  -714,
      25,  -714,  -714,  2664,  -714,  -714,  -714,  -714,  4672,  -714,
    5311,  5015,  3354,  -714,  -714,  -714,  -714,  -714,  -714,  -714,
     449,   453,   448,  -714,   466,    40,   242,    36,   365,  4144,
    -714,  -714,   243,   101,   471,   463,  -714,  -714,  -714,  -714,
     468,   499,   494,  -714,  -714,   480,  -714,  -714,  -714,  -714,
    -714,  -714,   481,   476,   484,  -714,   305,   320,  -714,   575,
    4672,  4144,   887,  4672,  -714,  -714,  -714,  4672,  -714,  -714,
     523,  4144,   364,   357,   522,   525,   -21,  -714,  -714,    48,
    -714,   588,  -714,  -714,  -714,  -714,  -714,  -714,   587,    99,
    -714,  -714,   362,    58,  2664,  -714,   533,   376,  4144,   221,
    4144,  -714,  -714,  -714,   482,  -714,   538,   504,   288,   293,
     542,  2664,  -714,   503,   506,  2664,  2664,  2802,  2112,  -714,
    2112,   708,  -714,  -714,  5311,  -714,  -714,  2112,  -714,  2112,
     139,  -714,  -714,  -714,  -714,   486,   518,   556,   557,   555,
     558,  5117,   520,  -714,  -714,  -714,  -714,  -714,  4232,    -8,
     403,  -714,  -714,  -714,  -714,   611,   562,   526,  -714,   527,
     364,  -714,  -714,  -714,  -714,  -714,  -714,   543,  -714,   531,
     572,   559,   563,  -714,  -714,  -714,  4913,  -714,  -714,  -714,
     544,  1569,  -714,  -714,  2250,  1698,  -714,  -714,   560,  4144,
    -714,  5179,  5348,  -714,  4144,  4144,  -714,  -714,  4144,  -714,
    -714,  -714,   549,  -714,  5461,   375,  -714,  -714,  -714,   550,
     554,   621,  -714,  4144,  -714,  -714,   570,   565,   521,  -714,
    2664,  -714,  2388,  -714,  2664,   391,  -714,  -714,  1393,  -714,
    -714,  4144,  4144,  1634,   602,  -714,  -714,   119,  -714,  -714,
    5179,  -714,  -714,   583,   234,   325,  -714,  -714,  -714,  -714,
    -714,  -714,  -714,  -714,   574,  -714,   620,  -714,  -714,  -714,
    -714,  -714,   593,  -714,  -714,  -714,  4144,  4144,   584,   586,
     130,  -714,   403,   612,  -714,  -714,   631,  4144,   682,   685,
     707,  -714,  2664,  2802,  -714,  -714,  -714,  5015,  2112,  5311,
    -714,  1569,   242,  -714,   298,   603,   133,  -714,  -714,  2526,
    -714,   614,   599,  -714,   285,    40,  -714,  -714,  -714,  -714,
    4144,  5535,  5535,  -714,  -714,  -714,  -714,  -714,   616,  -714,
    -714,  -714,  1255,  1255,  -714,  -714,  -714,  -714,  -714,  4144,
    -714,  -714,   415,  1111,  1111,  -714,  -714,  -714,  -714,  -714,
    5535,   712,   653,  -714,  -714,  2802,  2664,  -714,  -714,     1,
      11,  -714,  -714,  -714,  5216,   685,   707,  4672,  -714,  -714,
    -714,   650,  -714,  4672,   401,   707,   249,  -714,   707,  -714,
    -714,  -714,  -714,  -714,     0,  -714,   627,  -714,  -714,  -714,
    4876,  -714,  -714,  -714,  2664,  2802,  2664,  -714,    33,  -714,
    -714,  -714,   125,   663,  -714,  -714,  5535,   713,  3968,  -714,
    -714,   141,  -714,    72,  -714,   734,  -714,   730,  -714,   730,
    -714,   169,  -714,    77,  -714,   667,   402,  -714,  4144,  -714,
    -714,  -714,  4144,  -714,  4672,  4672,   707,  -714,  -714,  5385,
     682,   668,  3572,  -714,  -714,  -714,   415,   415,  -714,  -714,
    -714,  -714,  4320,   255,   710,  -714,  -714,  -714,  2112,  5311,
    -714,  -714,  -714,   675,   611,  -714,  -714,  4144,  -714,  4144,
    -714,  4672,  4672,  4672,  -714,   435,  -714,  1255,  -714,  2664,
    -714,  4672,   477,  -714,  1111,  -714,  5535,  4408,  4496,  -714,
    -714,  -714,  -714,   674,   621,  -714,  -714,  -714,  4672,   643,
    -714,  4672,   174,  -714,   293,   100,  -714,  -714,   649,   656,
    -714,  4672,  -714,  -714,  -714,  2664,  -714,   665,   658,   523,
    -714,   437,  4144,  4584,  -714,  -714,  -714,  -714,  4232,  -714,
    5535,  -714,   671,   179,  -714,    40,   103,  4672,  3674,  -714,
    4672,  -714,   415,   154,  -714,  4672,  -714,  -714,  -714,  -714,
    -714,  -714,  5498,  -714,  -714,  3468,   688,   693,   403,  -714,
    -714,  -714,  4672,  -714,  -714,  -714,  -714,  4144,  -714,   663,
    5535,   685,   707,  -714,  -714,  -714,   707,  -714,  -714
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    40,     0,     2,    40,     4,   523,   524,     8,
       0,    43,     1,     0,     0,     0,    18,    11,    39,    13,
      16,    70,   522,   521,    12,   188,   184,     0,     0,    20,
       0,    46,    41,    15,    14,   187,     0,     6,     7,   489,
       0,   491,     0,   490,   477,   492,   493,   494,   475,   476,
     474,   478,   479,   495,   496,   497,   498,   499,   500,   501,
     502,   503,   504,   506,   505,     0,   472,   437,   471,   435,
      22,     0,    19,    24,    28,    36,   428,   436,    35,   466,
     470,   473,     0,    45,     0,    38,    42,   335,     0,     0,
     132,   134,     0,     0,    63,    64,    65,    99,     0,   133,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   299,   511,   510,   516,   517,   518,   519,
     520,   299,   299,    61,    68,    71,    72,    73,    74,   154,
       0,    77,   282,    78,   305,   308,   314,   324,   327,   329,
     399,   412,   400,   194,   328,   470,   401,   509,   330,   185,
       0,    27,    26,     0,   486,   508,   507,   487,     0,   484,
       0,     0,     0,   485,   488,    17,     0,    21,    31,    25,
      40,    40,     3,    48,    37,     0,     0,   304,   462,   463,
     461,     0,     0,   222,   267,     0,     0,     0,   459,   244,
       0,   147,   205,   208,   209,   213,   220,   421,   426,   221,
     457,   460,     0,     0,   135,   382,   382,   322,   310,     0,
       0,     0,     0,     0,   178,   178,   181,     0,     0,     0,
       0,     0,   205,   421,     0,   323,   313,     0,     0,     0,
       0,   407,   189,   405,   403,   374,   376,     0,   316,   307,
     317,   515,   413,     0,   514,   513,   338,   304,   343,     0,
     344,   452,     0,   451,   455,   483,   482,   416,   512,     0,
     408,   526,     0,     0,     0,   483,     0,   482,   416,     0,
     410,     0,     0,     0,   300,     0,     0,     0,    62,     0,
      69,   154,     0,     0,     0,     0,     0,     0,     0,   283,
     183,   288,   450,     0,   449,   453,   481,   480,     0,   311,
       0,   389,     0,   186,   431,   430,   429,   468,   467,    23,
       0,     0,    32,    34,     0,     0,     0,    50,     0,     0,
     224,   223,     0,     0,     0,   268,   270,   464,   238,   425,
       0,   197,     0,   201,   443,     0,   444,   226,   442,   441,
     439,   438,   235,     0,     0,   440,     0,     0,   250,   164,
       0,     0,     0,     0,   217,   432,   218,     0,   214,   216,
     138,   234,     0,     0,   386,     0,     0,   381,   383,     0,
     309,     0,    96,    95,    97,    98,   230,   191,   174,     0,
     284,   177,     0,     0,     0,    92,     0,   140,     0,     0,
       0,    79,    80,    81,     0,   294,     0,     0,     0,     0,
       0,     0,   375,     0,     0,   339,   345,     0,     0,   334,
       0,   340,   337,   469,     0,   333,   331,     0,   332,     0,
     467,   402,   409,   525,   411,     0,     0,     0,     0,     0,
       0,     0,   291,   446,    67,   445,   447,   414,     0,     0,
     136,   290,   202,   192,   193,   183,     0,   354,   356,     0,
       0,   286,   287,   306,   312,   326,   392,     0,   388,   391,
     394,     0,   470,   315,    30,    29,     0,     9,    10,    47,
       0,    54,    44,    49,     0,     0,   321,   303,     0,     0,
     225,     0,     0,   228,     0,     0,   424,   229,     0,   427,
     422,   423,   245,   247,     0,     0,    82,   146,   206,     0,
       0,   210,   215,     0,    87,   235,     0,   233,   387,   384,
       0,   377,   380,   378,     0,     0,    91,   179,   176,   180,
     319,     0,     0,     0,   104,   251,    88,     0,    89,    83,
       0,   295,   404,     0,     0,     0,   190,   418,   406,   292,
     318,   456,   417,   347,   349,   353,   338,   351,   350,   336,
     342,   341,     0,   301,   293,   298,     0,     0,     0,     0,
       0,   238,   136,     0,   151,   153,     0,     0,   264,   254,
     272,   285,     0,     0,   454,   182,   325,     0,     0,     0,
      33,    54,     0,    56,    28,     0,    53,    58,   359,     0,
     372,     0,   361,   365,     0,     0,   360,   465,   271,   269,
       0,     0,     0,   237,   239,   242,   198,   200,   236,   250,
     250,   249,   160,   160,   163,   433,   458,   139,    75,     0,
     385,   379,   310,   170,   170,   173,   175,   119,   141,   142,
       0,   109,     0,   419,   420,     0,   346,   302,   195,     0,
       0,   448,   415,    66,     0,   254,   272,     0,   152,   137,
     238,   258,   260,     0,     0,   272,     0,    84,   273,   275,
     289,   355,   390,   393,   396,   398,     0,    60,    59,    51,
       0,    55,   362,   357,   364,     0,     0,   366,   183,   370,
     358,   199,     0,     0,   227,   246,   248,   125,     0,   155,
     159,     0,   156,     0,   236,     0,   132,   127,   165,   127,
     169,     0,   166,     0,   105,     0,     0,    86,     0,   352,
     348,   296,     0,   297,     0,     0,   272,    93,   150,     0,
     264,     0,   265,   211,   219,   262,   310,   310,    85,   102,
     100,   101,     0,     0,   276,   279,   434,   274,     0,     0,
      52,    57,   363,     0,   183,   368,   369,     0,   240,     0,
     126,     0,     0,     0,   123,   143,   161,   158,   162,     0,
     128,     0,   154,   171,   168,   172,     0,   118,   118,   110,
      76,   196,   149,     0,   203,    94,   263,   259,     0,     0,
     212,     0,     0,   256,     0,     0,   280,   207,   231,     0,
     277,     0,   278,   395,   397,     0,   367,     0,     0,   138,
     124,   143,     0,     0,   121,   157,   320,   129,     0,   167,
     106,   108,     0,     0,   117,     0,     0,     0,   265,   261,
     266,   252,   310,     0,   253,     0,   281,   103,   371,   241,
     243,   120,     0,   122,   144,     0,     0,   221,   136,   107,
     113,   111,   116,   114,   112,   148,   255,     0,   232,   221,
       0,   254,   272,   115,   257,   145,   272,   130,   131
  };

  const short
  parser::yypgoto_[] =
  {
    -714,  -714,  -714,  -714,  -714,  -714,  -714,    31,  -714,  -714,
    -714,  -714,   613,   198,  -714,  -714,    -3,   659,  -714,  -714,
    -714,  -714,  -714,  -714,  -714,  -714,   204,  -714,   117,  -714,
    -714,  -714,  -714,  -714,  -714,  -714,  -714,  -714,  -714,  -714,
    -714,  -714,  -714,  -714,  -714,   -61,  -714,  -714,  -714,    24,
    -187,  -714,  -714,    96,  -714,   766,  -714,  -541,     7,  -714,
       8,   524,     2,  -276,    51,   202,  -714,  -714,    47,   189,
    -714,  -714,   604,  -714,  -309,  -405,   802,  -714,  -714,  -290,
     110,  -156,   263,  -158,   337,  -714,   -83,  -714,   -85,  -714,
      23,  -714,  -392,  -714,  -714,  -714,  -676,   -22,   548,    -1,
    -714,   467,  -518,   309,  -713,  -714,  -714,   227,   232,  -439,
    -611,   114,    15,  -525,  -714,   123,  -714,    66,  -714,  -714,
     368,  -604,  -714,   187,   113,   817,  -195,  -714,  -714,   564,
    -714,   404,  -714,   289,    29,  -224,  -201,   744,    55,  -714,
    -714,  -714,   -88,  -714,  -714,  -714,  -714,   178,  -714,  -714,
    -418,  -714,   181,  -714,  -714,   182,  -714,  -714,   619,  -714,
     -79,   657,   347,  -237,  -714,   290,  -714,  -714,  -714,  -714,
     465,    82,  -714,  -103,  -675,   -78,  -714,   470,   -70,  -714,
    -714,  -714,   -11,  -714,  -184,  -714,   311,  -714,   622,  -714,
    -714,  -714,  -428,  -714,  -335,  -252,    10,  -235,  -182,   -31,
    -714,  -714,   -30,   -40,  -101,   -89,  -714,  -152,   -93,   -67,
    -217,  -714,  -283,   -26,  -111
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   172,     6,    10,    19,    30,
      71,    72,    73,   169,   311,   312,   584,    86,    11,    20,
      21,    32,    84,   317,   472,   473,   585,   586,   587,   279,
     123,   432,    33,    34,   124,   394,   125,   126,   127,   220,
     128,   213,   733,   792,   631,   704,   810,   707,   769,   813,
     814,   689,   751,   761,   698,   699,   203,   569,   504,   524,
     804,   189,   562,   283,   690,   691,   614,   496,   700,   701,
     625,   516,   379,   216,   217,   451,    27,    36,   400,   376,
     441,   130,   639,   342,   525,   443,   332,   721,   333,   788,
     192,   193,   722,   194,   358,   353,   723,   195,   378,   789,
     506,   343,   482,   603,   604,   349,   492,   493,   494,   526,
     655,   782,   783,   570,   651,   652,   653,   725,   324,   325,
     326,   657,   658,   659,   734,   380,   692,   289,   290,   291,
     132,   273,   274,   246,   177,   134,   784,   135,   136,   137,
     138,   262,   263,   264,   249,   250,   544,   446,   447,   476,
     591,   592,   593,   677,   678,   679,   594,   365,   236,   237,
     207,   366,   367,   368,   457,   458,   459,   664,   139,   140,
     231,   232,   141,   142,   433,   251,   536,   196,   197,    75,
     354,   735,   198,    77,   344,   345,   434,   435,   293,   252,
     294,   253,   199,   356,   200,   143,   144,   461,    79,    80,
     295,   254,   255,   297,   163,    81,   164,   146,   147,   257,
     258,   148,    24,     9,   269
  };

  const short
  parser::yytable_[] =
  {
     201,   271,   233,   145,   191,   438,   190,   371,   355,   256,
     267,   201,   234,   397,   151,   221,   152,   500,    76,   381,
     381,   645,   248,   225,   412,   162,    74,   330,   323,   477,
     223,   385,   468,   444,   716,   161,    13,   646,   355,    78,
     571,    22,   717,   644,   268,   606,   780,   781,   470,    22,
     448,   728,   450,   508,   605,   292,   275,   595,   173,    22,
     450,   403,   437,   133,   617,   455,   611,     1,   395,   453,
     565,   266,   284,    22,   267,   346,   347,   425,    22,   711,
     204,   265,   738,   628,   281,   511,   513,  -464,    12,   713,
     836,   404,   201,   201,    25,   605,   201,   201,   512,   292,
     519,    22,   739,   296,    22,   201,   449,    18,   268,   406,
     287,   285,   775,   201,   228,   407,    29,   360,   675,   836,
      26,   712,   426,   222,   201,   396,  -464,   566,   649,   201,
     201,   712,   719,   386,   387,   162,   404,   282,    17,   247,
     247,   575,   780,   781,   780,   265,   272,   296,    23,   471,
     408,     2,    31,   327,   388,    76,    23,    76,   226,   320,
     321,   235,   238,    74,   240,   313,    23,   512,   -90,   292,
     545,   672,   359,   682,   683,   398,    78,   518,    78,   549,
      23,   453,   479,   145,   145,    23,   201,   162,   166,   241,
     299,   757,   596,   389,   201,   201,   764,   161,   191,   334,
     190,   314,   315,   505,   247,   517,   747,   -90,    23,   201,
     241,    23,   167,   336,   399,    35,   605,   296,   518,   822,
    -465,   285,   842,   260,   154,   222,   391,   155,   413,   261,
     201,   748,   529,    37,   156,   847,   244,   392,   393,   436,
     856,   222,   222,   382,   382,   338,   339,   756,   857,   340,
     341,   431,   858,   670,   327,    38,   157,   244,   611,  -465,
     757,   201,   201,   201,   201,   497,   638,   638,   201,   413,
     462,   729,   201,   745,   399,   763,   454,   671,   632,    82,
     821,   411,   478,   427,   509,   841,   429,   430,   764,    85,
     469,   605,   235,   822,   299,   705,   233,   851,   842,   201,
     730,   201,   300,   731,   292,   301,   234,   256,   797,   256,
     798,   460,   680,   852,   334,   414,   256,   355,   256,   528,
     547,   335,   548,   626,   170,   323,   171,   415,   336,   550,
     608,   551,   149,   292,   527,   502,   661,   154,   559,   796,
     155,   499,   150,   437,   665,   416,    67,   156,   633,   201,
      69,   417,   296,   564,   261,   563,   154,   463,    83,   155,
     338,   339,   732,   834,   340,   341,   156,     7,   732,   157,
     675,     8,   676,   222,    67,   837,   501,   176,    69,   418,
      67,   296,   208,   413,    69,   419,   818,   270,   157,   820,
     201,   261,   159,   201,   165,   201,   201,   533,   709,   201,
     558,   534,   229,   535,   605,   201,   230,   849,   112,   422,
     758,   168,    67,    67,   201,   423,    69,    69,   114,   490,
     765,   695,   771,   346,   347,   423,   424,   174,   702,   702,
     423,   811,   201,   201,   201,   202,   491,   247,   448,   247,
     423,   634,   241,   224,   286,   261,   247,   287,   247,   272,
     327,   209,   210,   211,   212,    76,   154,   522,   523,   155,
      76,   694,   205,   580,   206,   278,   156,   201,   201,   214,
     474,   215,   475,    14,    15,   839,    78,   256,   201,   303,
     612,    78,   613,   288,   567,   568,   808,   145,   157,   244,
     663,   304,   159,   245,   364,   364,   623,   302,   624,   327,
     305,   754,   824,   306,   794,   437,   726,   767,   727,   768,
     307,   201,   201,   201,   308,   705,   802,   803,   802,   832,
     436,   316,   331,   331,   318,   276,   277,   348,   261,   361,
     201,   350,   843,   844,   370,   372,   384,   373,   334,   374,
     375,   201,   390,   242,   409,   351,   462,   382,   413,   282,
     377,   410,   336,   439,   420,   201,   667,   854,   201,   334,
    -485,   421,   718,   464,   201,   800,   351,   465,   466,   702,
      76,  -203,   467,   336,   807,   445,   364,   480,   483,   352,
     484,   145,   145,   481,   338,   339,   485,   460,   340,   341,
     355,    78,   145,   145,   486,   487,   488,   201,   489,   201,
     352,   437,   495,   755,   503,   338,   339,   247,  -373,   340,
     341,   510,   377,   514,   515,   521,   531,   530,   532,   201,
     539,   442,   552,   201,   541,   201,   201,   542,   553,   772,
     201,   724,   773,   201,   554,   555,   556,   256,   450,   557,
     560,   382,   382,   201,   572,   736,   573,   787,   574,   576,
     793,   577,   382,   382,   578,   853,   377,   581,   201,    76,
     201,   579,   201,   201,   201,  -469,   799,   386,   801,   609,
     222,   615,   201,   520,   597,   616,   386,   201,   201,   201,
      78,   233,   386,   386,   618,   619,   559,   630,   498,   201,
     540,   234,   201,   329,   635,   543,   364,   546,   331,   636,
     724,   334,   201,   637,   647,   641,   827,   642,   413,   648,
     650,   222,   654,   201,   201,   336,   656,   669,   674,   201,
     673,   201,   736,   564,   346,   563,   145,   331,   201,   201,
     684,   201,   845,   145,   708,   720,   201,   222,   774,   706,
     787,   740,   352,   201,   749,   759,   201,   338,   339,   760,
     750,   340,   341,   201,   766,   222,   724,   386,   201,   724,
     778,   201,   795,   590,   590,   791,   817,   247,   114,   825,
     826,   829,   830,   850,   222,   222,   222,   840,  -242,   309,
     241,   319,   668,   280,   222,   666,   382,   741,   241,   855,
     222,   222,   816,   382,   154,   762,   724,   155,   724,   620,
     129,   364,   154,   622,   156,   155,   831,   440,   805,   833,
     838,   809,   156,   703,   222,   693,   598,    28,   770,   383,
     640,   288,   607,   428,   848,   331,   157,   244,   507,   288,
     159,   245,   629,   153,   157,   244,   685,   846,   159,   245,
     222,   785,   686,   777,   819,   737,   790,   154,   222,   599,
     155,   131,   239,   743,   452,   742,   402,   156,   627,   621,
     746,   660,   364,   369,   538,   222,   823,   662,   537,   405,
       0,   643,     0,     0,     0,     0,     0,     0,   590,   157,
     158,     0,     0,   159,   160,     0,     0,     0,     0,     0,
       0,    39,     0,   377,   377,     0,     0,     0,     0,    41,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    43,     0,     0,     0,     0,     0,    45,    46,    47,
     178,   179,   180,     0,   364,   710,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,   681,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   331,     0,     0,     0,
       0,     0,     0,   590,   364,   744,    87,    39,    88,    89,
      90,    91,     0,    92,     0,    41,    93,     0,     0,    94,
      95,    96,    97,    98,     0,    99,     0,    43,     0,   100,
       0,    44,   101,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,     0,    55,    56,    57,     0,     0,
      58,   188,    67,   103,    59,     0,    69,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,   104,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     105,     0,     0,     0,     0,   442,   106,     0,   806,   377,
       0,     0,     0,   107,     0,     0,   108,     0,   109,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,   111,     0,   112,     0,
       0,     0,     0,     0,   828,     0,   113,    66,   114,     0,
       0,    68,   115,     0,     0,     0,     0,   116,   117,   118,
     119,     0,     0,   120,     0,     0,     0,     0,   121,   122,
       0,     0,     0,     0,    87,    39,    88,     0,   696,     0,
       0,    92,     0,    41,    93,     0,     0,    94,    95,    96,
       0,    98,     0,    99,     0,    43,     0,   697,     0,    44,
     627,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,   103,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,   331,     0,     0,   104,     0,     0,     0,     0,
       0,     0,     0,     0,   377,     0,     0,     0,   105,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,   107,     0,     0,   108,     0,   109,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,   111,     0,   112,     0,     0,     0,
       0,     0,     0,     0,   113,    66,   114,     0,     0,    68,
     115,     0,     0,     0,     0,   116,   117,   118,   119,     0,
       0,   120,     0,     0,     0,     0,   121,   122,    87,    39,
      88,     0,   687,     0,     0,    92,     0,    41,    93,     0,
       0,    94,    95,    96,     0,    98,     0,     0,     0,    43,
       0,   688,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,   103,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,   104,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   105,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,   107,     0,     0,   108,     0,
     109,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   110,     0,     0,     0,   111,     0,
     112,     0,     0,     0,     0,     0,     0,     0,   113,    66,
     114,     0,     0,    68,   115,     0,     0,     0,     0,   116,
     117,   118,   119,     0,     0,   120,    87,    39,    88,     0,
     121,   122,     0,    92,     0,    41,    93,     0,     0,    94,
      95,    96,     0,    98,     0,     0,     0,    43,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,   103,    59,     0,     0,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,   104,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     105,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,   107,     0,     0,   108,     0,   109,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,   111,     0,   112,     0,
       0,     0,     0,     0,     0,     0,   113,    66,   114,     0,
       0,    68,   115,     0,     0,     0,     0,   116,   117,   118,
     119,     0,    39,   120,     0,     0,    40,     0,   121,   122,
      41,     0,     0,     0,     0,     0,     0,     0,     0,    42,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,     0,
      55,    56,    57,    39,     0,    58,     0,     0,     0,    59,
       0,    41,    60,    61,    62,    63,    64,     0,     0,     0,
     582,     0,     0,    43,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    39,     0,
       0,    65,     0,     0,     0,     0,    41,     0,     0,     0,
       0,     0,    66,    67,     0,     0,    68,    69,    43,     0,
       0,     0,     0,     0,    45,    46,    47,   178,   179,   180,
       0,     0,    70,    53,    54,     0,    55,    56,    57,     0,
       0,    58,    65,     0,     0,    59,     0,     0,    60,    61,
      62,    63,    64,    66,    67,     0,     0,    68,    69,    22,
       0,    87,    39,    88,     0,     0,     0,     0,    92,     0,
      41,    93,     0,   583,     0,     0,     0,     0,    98,     0,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,   601,
      55,    56,    57,     0,     0,    58,     0,   602,   103,    59,
       0,     0,    60,    61,    62,    63,    64,     0,   188,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,   107,     0,
       0,   108,     0,   109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    23,   110,     0,     0,
       0,   175,     0,   112,     0,     0,     0,   589,     0,     0,
       0,     0,    66,   114,     0,     0,    68,   115,     0,     0,
       0,     0,   116,   117,   118,   119,     0,     0,   120,    87,
      39,    88,     0,     0,     0,     0,    92,     0,    41,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,   103,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   241,     0,     0,   106,
       0,     0,     0,     0,     0,     0,   107,     0,     0,   108,
     154,   109,     0,   155,     0,     0,     0,     0,     0,   259,
     156,     0,     0,     0,     0,   110,     0,     0,     0,   175,
     260,   112,     0,     0,     0,     0,   261,   243,     0,     0,
      66,   114,   157,   244,    68,   115,   159,   245,     0,     0,
     116,   117,   118,   119,     0,     0,   120,    87,    39,    88,
       0,     0,     0,     0,    92,     0,    41,    93,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,    43,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,   103,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   241,     0,     0,   106,     0,     0,
       0,     0,     0,     0,   107,     0,     0,   108,     0,   109,
       0,   155,     0,     0,     0,     0,     0,     0,   156,     0,
       0,     0,     0,   110,   242,     0,     0,   175,     0,   112,
       0,     0,     0,     0,     0,   243,     0,     0,    66,   114,
     157,   244,    68,   115,   159,   245,     0,     0,   116,   117,
     118,   119,     0,     0,   120,    87,    39,    88,     0,     0,
       0,     0,    92,     0,    41,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    43,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,   103,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   241,     0,     0,   106,     0,     0,     0,     0,
       0,     0,   107,     0,     0,   108,     0,   109,     0,   155,
       0,     0,     0,     0,     0,     0,   156,     0,     0,     0,
       0,   110,     0,     0,     0,   175,     0,   112,     0,     0,
       0,     0,     0,   243,     0,     0,    66,   114,   157,   244,
      68,   115,   159,   245,     0,     0,   116,   117,   118,   119,
       0,     0,   120,    87,    39,    88,     0,     0,     0,     0,
      92,     0,    41,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
     103,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
     107,     0,     0,   108,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   588,     0,     0,   110,
       0,     0,     0,   175,     0,   112,     0,     0,     0,   589,
       0,     0,     0,     0,    66,   114,     0,     0,    68,   115,
       0,     0,     0,     0,   116,   117,   118,   119,     0,     0,
     120,    87,    39,    88,     0,     0,     0,     0,    92,     0,
      41,    93,     0,     0,     0,     0,     0,     0,   362,     0,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,     0,
      55,    56,    57,     0,   363,    58,     0,     0,   103,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,   107,     0,
       0,   108,     0,   109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   110,     0,     0,
       0,   175,     0,   112,     0,     0,     0,     0,     0,     0,
       0,     0,    66,   114,     0,     0,    68,   115,     0,     0,
       0,     0,   116,   117,   118,   119,     0,     0,   120,    87,
      39,    88,     0,     0,     0,     0,    92,     0,    41,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,   103,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,   107,     0,     0,   108,
       0,   109,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,   175,
       0,   112,     0,     0,     0,   589,     0,     0,     0,     0,
      66,   114,     0,     0,    68,   115,     0,     0,     0,     0,
     116,   117,   118,   119,     0,     0,   120,    87,    39,    88,
       0,     0,     0,     0,    92,     0,    41,    93,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,    43,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,   103,    59,     0,     0,    60,    61,
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
       0,     0,   362,     0,     0,     0,    43,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,   103,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,   107,     0,     0,   108,     0,   109,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,     0,   175,     0,   112,     0,     0,
       0,     0,     0,     0,     0,     0,    66,   114,     0,     0,
      68,   115,     0,     0,     0,     0,   116,   117,   118,   119,
       0,     0,   120,    87,    39,    88,     0,     0,     0,     0,
      92,     0,    41,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
     103,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,   175,     0,   112,     0,     0,     0,     0,
       0,     0,     0,     0,    66,   114,     0,     0,    68,   115,
       0,     0,     0,     0,   116,   117,   118,   119,     0,     0,
     120,    87,    39,    88,     0,     0,     0,     0,    92,     0,
      41,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
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
      57,     0,     0,    58,     0,     0,     0,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,   401,     0,     0,   107,     0,     0,     0,
       0,   109,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,   175,
       0,   112,     0,     0,     0,     0,     0,     0,     0,     0,
      66,   114,     0,     0,    68,   115,     0,     0,     0,     0,
     116,   117,   118,   119,     0,     0,   120,    87,    39,    88,
       0,     0,     0,     0,    92,     0,    41,    93,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,    43,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,   109,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,     0,   175,     0,   112,
       0,     0,    39,     0,     0,     0,     0,     0,    66,   114,
      41,     0,    68,   115,     0,     0,     0,     0,   116,   117,
     118,   119,    43,     0,   120,     0,   328,     0,    45,    46,
      47,   178,   179,   180,     0,     0,     0,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   334,     0,
       0,     0,     0,     0,     0,   335,     0,     0,   181,     0,
       0,     0,   336,   182,     0,   183,     0,     0,     0,     0,
       0,     0,     0,   184,     0,     0,    39,   185,     0,     0,
       0,   186,   337,   187,    41,     0,     0,     0,   261,     0,
       0,     0,   188,    67,   338,   339,    43,    69,   340,   341,
       0,     0,    45,    46,    47,   178,   179,   180,     0,     0,
       0,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   241,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   181,     0,  -204,     0,     0,   182,     0,   183,
       0,     0,     0,     0,     0,     0,     0,   184,    39,     0,
       0,   185,     0,     0,     0,   186,    41,   187,     0,     0,
       0,     0,     0,   779,     0,     0,   188,    67,    43,   244,
       0,    69,     0,     0,    45,    46,    47,   178,   179,   180,
       0,     0,     0,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   241,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   181,     0,     0,     0,     0,   182,
       0,   183,     0,     0,     0,     0,     0,     0,     0,   184,
       0,     0,    39,   185,     0,     0,     0,   186,     0,   187,
      41,     0,     0,     0,     0,   779,     0,   218,   188,    67,
       0,   244,    43,    69,     0,     0,     0,     0,    45,    46,
      47,   178,   179,   180,     0,   219,     0,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,     0,
      39,     0,     0,   182,     0,   183,     0,     0,    41,     0,
       0,     0,     0,   184,     0,     0,     0,   185,     0,     0,
      43,   186,     0,   187,   328,     0,    45,    46,    47,   178,
     179,   180,   188,    67,     0,    53,    54,    69,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   181,     0,     0,     0,
       0,   182,     0,   183,     0,     0,     0,     0,     0,     0,
       0,   184,    39,     0,     0,   185,   329,     0,     0,   186,
      41,   187,     0,     0,     0,     0,     0,   752,     0,     0,
     188,    67,    43,     0,     0,    69,     0,     0,    45,    46,
      47,   178,   179,   180,     0,   753,     0,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,     0,
      39,     0,     0,   182,     0,   183,     0,     0,    41,     0,
       0,     0,     0,   184,     0,     0,     0,   185,     0,     0,
      43,   186,     0,   187,     0,     0,    45,    46,    47,   178,
     179,   180,   188,    67,     0,    53,    54,    69,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   357,   181,     0,    39,     0,
       0,   182,     0,   183,     0,     0,    41,     0,     0,     0,
       0,   184,     0,     0,     0,   185,     0,     0,    43,   186,
       0,   187,   328,     0,    45,    46,    47,   178,   179,   180,
     188,    67,     0,    53,    54,    69,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   181,     0,    39,     0,     0,   182,
       0,   183,     0,     0,    41,     0,     0,     0,     0,   184,
       0,     0,     0,   185,     0,     0,    43,   186,     0,   187,
     561,     0,    45,    46,    47,   178,   179,   180,   188,    67,
       0,    53,    54,    69,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   181,     0,    39,     0,     0,   182,     0,   183,
       0,     0,    41,     0,     0,     0,     0,   184,     0,     0,
       0,   185,     0,     0,    43,   186,     0,   187,     0,     0,
      45,    46,    47,   178,   179,   180,   188,    67,     0,    53,
      54,    69,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     181,     0,    39,     0,     0,   182,     0,   183,     0,     0,
      41,     0,     0,     0,     0,   184,     0,     0,     0,   185,
       0,     0,    43,   186,   786,   187,     0,     0,    45,    46,
      47,   178,   179,   180,   188,    67,     0,    53,    54,    69,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   812,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,     0,
      39,     0,     0,   182,     0,   183,     0,     0,    41,     0,
       0,     0,     0,   184,     0,     0,     0,   185,     0,     0,
      43,   186,     0,   187,     0,     0,    45,    46,    47,   178,
     179,   180,   188,    67,     0,    53,    54,    69,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   815,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   181,     0,    39,     0,
       0,   182,     0,   183,     0,     0,    41,     0,     0,     0,
       0,   184,     0,     0,     0,   185,     0,     0,    43,   186,
       0,   187,   328,     0,    45,    46,    47,   178,   179,   180,
     188,    67,     0,    53,    54,    69,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   181,     0,    39,     0,     0,   182,
       0,   183,     0,     0,    41,     0,     0,     0,     0,   184,
       0,     0,     0,   185,     0,     0,    43,   835,     0,   187,
       0,     0,    45,    46,    47,   178,   179,   180,   188,    67,
       0,    53,    54,    69,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   181,     0,     0,     0,     0,   182,     0,   183,
       0,     0,     0,     0,     0,     0,     0,   184,    39,     0,
       0,   185,    40,     0,     0,   186,    41,   187,     0,     0,
       0,     0,     0,     0,     0,    42,   188,    67,    43,     0,
       0,    69,    44,     0,    45,    46,    47,    48,    49,    50,
       0,    51,    52,    53,    54,    39,    55,    56,    57,     0,
       0,    58,     0,    41,     0,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,    43,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      39,     0,     0,     0,     0,     0,     0,    65,    41,     0,
     310,     0,     0,     0,     0,     0,     0,   582,    66,    67,
      43,     0,    68,    69,    44,     0,    45,    46,    47,    48,
      49,    50,     0,    51,    52,    53,    54,    39,    55,    56,
      57,     0,     0,    58,    65,    41,     0,    59,     0,     0,
      60,    61,    62,    63,    64,    66,    67,    43,     0,    68,
      69,    44,     0,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,     0,     0,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    39,     0,     0,     0,     0,     0,     0,    65,
      41,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      66,    67,    43,     0,    68,    69,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    39,
      55,    56,    57,     0,     0,    58,    65,    41,     0,    59,
       0,     0,    60,    61,    62,    63,    64,    66,    67,    43,
       0,    68,    69,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    39,     0,     0,     0,     0,     0,
       0,     0,    41,     0,   456,     0,     0,     0,     0,     0,
       0,     0,    66,   114,    43,     0,    68,   115,    44,     0,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,    39,    55,    56,    57,     0,     0,    58,   227,    41,
       0,    59,     0,     0,    60,    61,    62,    63,    64,    66,
       0,    43,     0,    68,     0,    44,     0,    45,    46,    47,
      48,    49,    50,     0,    51,    52,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,    39,     0,     0,     0,     0,     0,     0,
       0,    41,     0,   227,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    43,    66,     0,     0,    44,    68,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      39,    55,    56,    57,     0,     0,    58,     0,    41,     0,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
      43,    66,   114,     0,     0,     0,    45,    46,    47,   178,
     179,   180,     0,     0,     0,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   322,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    66,     0,     0,     0,     0,   714,     0,
       0,     0,     0,     0,     0,    39,     0,     0,     0,     0,
     715,   601,     0,    41,     0,     0,     0,     0,     0,   602,
       0,     0,     0,     0,     0,    43,     0,     0,     0,    44,
     188,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    39,    55,    56,    57,     0,     0,    58,     0,
      41,     0,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,    43,     0,     0,     0,     0,     0,    45,    46,
      47,   178,   179,   180,     0,     0,     0,    53,    54,    39,
      55,    56,    57,     0,     0,    58,     0,    41,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,    43,
       0,     0,     0,     0,     0,    45,    46,    47,   178,   179,
     180,     0,     0,     0,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,    66,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,   600,   601,     0,     0,     0,     0,     0,     0,
       0,   602,     0,     0,     0,    39,     0,     0,     0,     0,
       0,     0,   188,    41,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    43,     0,     0,     0,   776,
     601,    45,    46,    47,   178,   179,   180,     0,   602,     0,
      53,    54,    39,    55,    56,    57,     0,     0,    58,   188,
      41,     0,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,    43,     0,     0,     0,     0,     0,    45,    46,
      47,   178,   179,   180,     0,     0,     0,    53,    54,    39,
      55,    56,    57,     0,     0,    58,     0,    41,   610,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,    43,
       0,     0,     0,     0,     0,    45,    46,    47,   178,   179,
     180,     0,     0,     0,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,   188,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   602,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188
  };

  const short
  parser::yycheck_[] =
  {
      89,   112,   105,    34,    89,   281,    89,   208,   192,   110,
     111,   100,   105,   230,    40,   100,    42,   352,    29,   214,
     215,   562,   110,   102,   259,    65,    29,   185,   184,   319,
     100,   218,   315,   285,   645,    65,     5,   562,   222,    29,
     445,     1,   646,   561,   111,   484,   722,   722,    12,     1,
     287,   655,    27,   362,   482,   133,    19,   475,    84,     1,
      27,   243,   279,    34,   503,   300,   494,    21,    78,   293,
      78,   111,    81,     1,   175,   186,   187,    91,     1,    78,
      91,   111,    82,   522,    19,   106,   369,    81,     0,    78,
     803,   243,   181,   182,   109,   523,   185,   186,   119,   177,
     383,     1,   102,   133,     1,   194,   288,   119,   175,    79,
      85,   120,   716,   202,   104,    85,   113,   202,    85,   832,
     135,   120,   136,   100,   213,   135,   120,   135,   567,   218,
     219,   120,   650,   218,   219,   175,   288,    72,   106,   110,
     111,   450,   818,   818,   820,   175,   109,   177,   108,   113,
     120,   105,    14,   184,    81,   166,   108,   168,   103,   181,
     182,   106,   107,   166,   109,   168,   108,   119,    81,   247,
     407,   589,   194,   601,   602,    81,   166,   119,   168,   414,
     108,   405,    81,   214,   215,   108,   275,   227,   120,    80,
     135,   119,   475,   120,   283,   284,   119,   227,   283,    80,
     283,   170,   171,   361,   175,   106,    81,   120,   108,   298,
      80,   108,   144,    94,   120,   135,   644,   247,   119,   119,
      81,   120,   119,   114,    94,   202,    41,    97,   259,   120,
     319,   106,   390,    78,   104,    81,   127,    52,    53,   279,
     851,   218,   219,   214,   215,   126,   127,   106,   852,   130,
     131,   121,   856,   120,   285,    78,   126,   127,   686,   120,
     119,   350,   351,   352,   353,   350,   556,   557,   357,   300,
     301,    22,   361,   678,   120,   106,   298,   144,   530,    27,
     106,   252,   322,   273,   363,   106,   276,   277,   119,   119,
     316,   719,   237,   119,   239,   630,   399,   838,   119,   388,
      51,   390,   102,    54,   382,   105,   399,   408,   747,   410,
     749,   301,   595,   838,    80,   102,   417,   501,   419,   389,
     408,    87,   410,   518,   105,   481,   107,   114,    94,   417,
     488,   419,   110,   411,   113,   357,   573,    94,   431,   744,
      97,   352,   120,   560,   579,   114,   125,   104,   114,   438,
     129,   120,   382,   438,   120,   438,    94,   302,    24,    97,
     126,   127,   113,   802,   130,   131,   104,   125,   113,   126,
      85,   129,    87,   350,   125,   803,   353,    88,   129,   114,
     125,   411,    93,   414,   129,   120,   778,   116,   126,   781,
     479,   120,   130,   482,   114,   484,   485,   109,   635,   488,
     431,   113,   109,   115,   832,   494,   113,   835,   115,   114,
     693,   113,   125,   125,   503,   120,   129,   129,   125,   114,
     703,   622,   712,   534,   535,   120,   116,   119,   623,   624,
     120,   766,   521,   522,   523,    37,   116,   408,   675,   410,
     120,   116,    80,    14,    82,   120,   417,    85,   419,   109,
     481,    73,    74,    75,    76,   466,    94,    81,    82,    97,
     471,   619,   105,   466,   107,   136,   104,   556,   557,   105,
     105,   107,   107,    66,    67,   810,   466,   578,   567,   135,
     105,   471,   107,   121,    81,    82,   762,   518,   126,   127,
     578,   114,   130,   131,   205,   206,   105,    88,   107,   530,
     114,   688,   785,   114,   739,   722,   105,   105,   107,   107,
     114,   600,   601,   602,   114,   850,    81,    82,    81,    82,
     560,     4,   185,   186,    23,   121,   122,    85,   120,   113,
     619,    92,   815,   816,   119,    78,    15,    78,    80,    78,
      78,   630,    82,   110,   110,    87,   577,   518,   579,    72,
     213,   120,    94,   135,   114,   644,   582,   847,   647,    80,
     114,   114,   647,   114,   653,   752,    87,   114,   120,   764,
     581,    92,   106,    94,   761,   286,   287,   106,   110,   121,
      81,   612,   613,   120,   126,   127,    92,   577,   130,   131,
     774,   581,   623,   624,   114,   114,   120,   686,   114,   688,
     121,   818,    27,   688,    81,   126,   127,   578,    86,   130,
     131,    86,   275,    25,    27,    82,    78,   135,   114,   708,
      78,   284,   136,   712,   121,   714,   715,   121,   110,   714,
     719,   653,   715,   722,    78,    78,    81,   738,    27,    81,
     120,   612,   613,   732,    82,   656,   120,   732,   121,   106,
     738,   120,   623,   624,    82,   842,   319,   113,   747,   670,
     749,   102,   751,   752,   753,   102,   751,   752,   753,   120,
     647,   121,   761,   384,   114,   121,   761,   766,   767,   768,
     670,   784,   767,   768,   114,   120,   779,    85,   351,   778,
     401,   784,   781,   110,   120,   406,   407,   408,   361,    79,
     722,    80,   791,   110,    92,   121,   791,   121,   739,    78,
      28,   688,    27,   802,   803,    94,     9,   114,   119,   808,
     106,   810,   733,   808,   835,   808,   757,   390,   817,   818,
     114,   820,   817,   764,    81,    85,   825,   714,   715,    27,
     825,   114,   121,   832,    81,    11,   835,   126,   127,    19,
      37,   130,   131,   842,    87,   732,   778,   842,   847,   781,
      92,   850,    87,   474,   475,    55,    92,   738,   125,   120,
     114,   106,   114,    85,   751,   752,   753,   106,    85,   166,
      80,    81,   584,   124,   761,   581,   757,   670,    80,   850,
     767,   768,   768,   764,    94,   699,   818,    97,   820,   510,
      34,   512,    94,   514,   104,    97,   799,   283,   757,   801,
     808,   764,   104,   624,   791,   613,   479,    15,   708,   215,
     557,   121,   485,   275,   825,   488,   126,   127,   361,   121,
     130,   131,   523,    80,   126,   127,   609,   822,   130,   131,
     817,   727,   610,   720,   778,   658,   733,    94,   825,   481,
      97,    34,   108,   675,   290,   674,   237,   104,   521,   512,
     678,   572,   573,   206,   399,   842,   784,   577,   398,   247,
      -1,   560,    -1,    -1,    -1,    -1,    -1,    -1,   589,   126,
     127,    -1,    -1,   130,   131,    -1,    -1,    -1,    -1,    -1,
      -1,     4,    -1,   556,   557,    -1,    -1,    -1,    -1,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    -1,   635,   636,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,   600,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   619,    -1,    -1,    -1,
      -1,    -1,    -1,   674,   675,   676,     3,     4,     5,     6,
       7,     8,    -1,    10,    -1,    12,    13,    -1,    -1,    16,
      17,    18,    19,    20,    -1,    22,    -1,    24,    -1,    26,
      -1,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,   124,   125,    50,    51,    -1,   129,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    -1,    -1,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      77,    -1,    -1,    -1,    -1,   708,    83,    -1,   759,   712,
      -1,    -1,    -1,    90,    -1,    -1,    93,    -1,    95,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,    -1,    -1,    -1,   113,    -1,   115,    -1,
      -1,    -1,    -1,    -1,   795,    -1,   123,   124,   125,    -1,
      -1,   128,   129,    -1,    -1,    -1,    -1,   134,   135,   136,
     137,    -1,    -1,   140,    -1,    -1,    -1,    -1,   145,   146,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,     7,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    16,    17,    18,
      -1,    20,    -1,    22,    -1,    24,    -1,    26,    -1,    28,
     803,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,   835,    -1,    -1,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   847,    -1,    -1,    -1,    77,    -1,
      -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    -1,    95,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,    -1,    -1,    -1,   113,    -1,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   123,   124,   125,    -1,    -1,   128,
     129,    -1,    -1,    -1,    -1,   134,   135,   136,   137,    -1,
      -1,   140,    -1,    -1,    -1,    -1,   145,   146,     3,     4,
       5,    -1,     7,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    16,    17,    18,    -1,    20,    -1,    -1,    -1,    24,
      -1,    26,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    77,    -1,    -1,    -1,    -1,    -1,    83,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    -1,
      95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,    -1,    -1,    -1,   113,    -1,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   123,   124,
     125,    -1,    -1,   128,   129,    -1,    -1,    -1,    -1,   134,
     135,   136,   137,    -1,    -1,   140,     3,     4,     5,    -1,
     145,   146,    -1,    10,    -1,    12,    13,    -1,    -1,    16,
      17,    18,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    -1,    -1,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    -1,    -1,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      77,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    -1,    95,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,    -1,    -1,    -1,   113,    -1,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   123,   124,   125,    -1,
      -1,   128,   129,    -1,    -1,    -1,    -1,   134,   135,   136,
     137,    -1,     4,   140,    -1,    -1,     8,    -1,   145,   146,
      12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    21,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    -1,
      42,    43,    44,     4,    -1,    47,    -1,    -1,    -1,    51,
      -1,    12,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      21,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,
      -1,   113,    -1,    -1,    -1,    -1,    12,    -1,    -1,    -1,
      -1,    -1,   124,   125,    -1,    -1,   128,   129,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,   144,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,   113,    -1,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,   124,   125,    -1,    -1,   128,   129,     1,
      -1,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,   144,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,   105,
      42,    43,    44,    -1,    -1,    47,    -1,   113,    50,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,   124,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,
      -1,   113,    -1,   115,    -1,    -1,    -1,   119,    -1,    -1,
      -1,    -1,   124,   125,    -1,    -1,   128,   129,    -1,    -1,
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
      94,    95,    -1,    97,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,    -1,    -1,    -1,   109,    -1,    -1,    -1,   113,
     114,   115,    -1,    -1,    -1,    -1,   120,   121,    -1,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,    -1,    -1,
     134,   135,   136,   137,    -1,    -1,   140,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    83,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    -1,    95,
      -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
      -1,    -1,    -1,   109,   110,    -1,    -1,   113,    -1,   115,
      -1,    -1,    -1,    -1,    -1,   121,    -1,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,    -1,    -1,   134,   135,
     136,   137,    -1,    -1,   140,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    83,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    -1,    93,    -1,    95,    -1,    97,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,
      -1,   109,    -1,    -1,    -1,   113,    -1,   115,    -1,    -1,
      -1,    -1,    -1,   121,    -1,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,    -1,    -1,   134,   135,   136,   137,
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
      -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,    -1,   109,
      -1,    -1,    -1,   113,    -1,   115,    -1,    -1,    -1,   119,
      -1,    -1,    -1,    -1,   124,   125,    -1,    -1,   128,   129,
      -1,    -1,    -1,    -1,   134,   135,   136,   137,    -1,    -1,
     140,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    46,    47,    -1,    -1,    50,    51,
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
      -1,   115,    -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    -1,    95,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,    -1,    -1,    -1,   113,    -1,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
      -1,    -1,   128,   129,    -1,    -1,    -1,    -1,   134,   135,
     136,   137,    -1,    -1,   140,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
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
      -1,    83,    -1,    -1,    -1,    -1,    -1,    89,    90,    -1,
      -1,    -1,    -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,    -1,
      -1,   113,    -1,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,    -1,    -1,   128,   129,    -1,    -1,
      -1,    -1,   134,   135,   136,   137,    -1,    -1,   140,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,
      54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,
      -1,    -1,    -1,    87,    -1,    -1,    90,    -1,    -1,    -1,
      -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   109,    -1,    -1,    -1,   113,
      -1,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,    -1,    -1,   128,   129,    -1,    -1,    -1,    -1,
     134,   135,   136,   137,    -1,    -1,   140,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    -1,    -1,    95,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,    -1,    -1,    -1,   113,    -1,   115,
      -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,   124,   125,
      12,    -1,   128,   129,    -1,    -1,    -1,    -1,   134,   135,
     136,   137,    24,    -1,   140,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    90,    -1,
      -1,    -1,    94,    95,    -1,    97,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,    -1,    -1,     4,   109,    -1,    -1,
      -1,   113,   114,   115,    12,    -1,    -1,    -1,   120,    -1,
      -1,    -1,   124,   125,   126,   127,    24,   129,   130,   131,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    92,    -1,    -1,    95,    -1,    97,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,     4,    -1,
      -1,   109,    -1,    -1,    -1,   113,    12,   115,    -1,    -1,
      -1,    -1,    -1,   121,    -1,    -1,   124,   125,    24,   127,
      -1,   129,    -1,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    -1,    -1,    95,
      -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,
      -1,    -1,     4,   109,    -1,    -1,    -1,   113,    -1,   115,
      12,    -1,    -1,    -1,    -1,   121,    -1,    19,   124,   125,
      -1,   127,    24,   129,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    -1,    39,    40,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    -1,
      -1,    95,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   105,     4,    -1,    -1,   109,   110,    -1,    -1,   113,
      12,   115,    -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,
     124,   125,    24,    -1,    -1,   129,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    -1,    39,    40,    -1,
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
      -1,    -1,    -1,    -1,    -1,    89,    90,    -1,     4,    -1,
      -1,    95,    -1,    97,    -1,    -1,    12,    -1,    -1,    -1,
      -1,   105,    -1,    -1,    -1,   109,    -1,    -1,    24,   113,
      -1,   115,    28,    -1,    30,    31,    32,    33,    34,    35,
     124,   125,    -1,    39,    40,   129,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,     4,    -1,    -1,    95,
      -1,    97,    -1,    -1,    12,    -1,    -1,    -1,    -1,   105,
      -1,    -1,    -1,   109,    -1,    -1,    24,   113,    -1,   115,
      28,    -1,    30,    31,    32,    33,    34,    35,   124,   125,
      -1,    39,    40,   129,    42,    43,    44,    -1,    -1,    47,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,     4,    -1,    -1,    95,    -1,    97,    -1,    -1,
      12,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,
      -1,    -1,    24,   113,   114,   115,    -1,    -1,    30,    31,
      32,    33,    34,    35,   124,   125,    -1,    39,    40,   129,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
       4,    -1,    -1,    95,    -1,    97,    -1,    -1,    12,    -1,
      -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,    -1,
      24,   113,    -1,   115,    -1,    -1,    30,    31,    32,    33,
      34,    35,   124,   125,    -1,    39,    40,   129,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,
      54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,     4,    -1,
      -1,    95,    -1,    97,    -1,    -1,    12,    -1,    -1,    -1,
      -1,   105,    -1,    -1,    -1,   109,    -1,    -1,    24,   113,
      -1,   115,    28,    -1,    30,    31,    32,    33,    34,    35,
     124,   125,    -1,    39,    40,   129,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,     4,    -1,    -1,    95,
      -1,    97,    -1,    -1,    12,    -1,    -1,    -1,    -1,   105,
      -1,    -1,    -1,   109,    -1,    -1,    24,   113,    -1,   115,
      -1,    -1,    30,    31,    32,    33,    34,    35,   124,   125,
      -1,    39,    40,   129,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    -1,    -1,    -1,    95,    -1,    97,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,     4,    -1,
      -1,   109,     8,    -1,    -1,   113,    12,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    21,   124,   125,    24,    -1,
      -1,   129,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,     4,    42,    43,    44,    -1,
      -1,    47,    -1,    12,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       4,    -1,    -1,    -1,    -1,    -1,    -1,   113,    12,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    21,   124,   125,
      24,    -1,   128,   129,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,     4,    42,    43,
      44,    -1,    -1,    47,   113,    12,    -1,    51,    -1,    -1,
      54,    55,    56,    57,    58,   124,   125,    24,    -1,   128,
     129,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,   113,
      12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,    24,    -1,   128,   129,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,     4,
      42,    43,    44,    -1,    -1,    47,   113,    12,    -1,    51,
      -1,    -1,    54,    55,    56,    57,    58,   124,   125,    24,
      -1,   128,   129,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    12,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,    24,    -1,   128,   129,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,     4,    42,    43,    44,    -1,    -1,    47,   113,    12,
      -1,    51,    -1,    -1,    54,    55,    56,    57,    58,   124,
      -1,    24,    -1,   128,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    -1,   113,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    24,   124,    -1,    -1,    28,   128,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
       4,    42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      24,   124,   125,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,
      54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,    -1,    -1,    -1,    -1,    92,    -1,
      -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
     104,   105,    -1,    12,    -1,    -1,    -1,    -1,    -1,   113,
      -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
     124,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,     4,    42,    43,    44,    -1,    -1,    47,    -1,
      12,    -1,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,     4,
      42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,   124,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   113,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,   124,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,   104,
     105,    30,    31,    32,    33,    34,    35,    -1,   113,    -1,
      39,    40,     4,    42,    43,    44,    -1,    -1,    47,   124,
      12,    -1,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,     4,
      42,    43,    44,    -1,    -1,    47,    -1,    12,    87,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,   124,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   105,   148,   149,   150,   153,   125,   129,   360,
     154,   165,     0,   154,    66,    67,   151,   106,   119,   155,
     166,   167,     1,   108,   359,   109,   135,   223,   223,   113,
     156,    14,   168,   179,   180,   135,   224,    78,    78,     4,
       8,    12,    21,    24,    28,    30,    31,    32,    33,    34,
      35,    37,    38,    39,    40,    42,    43,    44,    47,    51,
      54,    55,    56,    57,    58,   113,   124,   125,   128,   129,
     144,   157,   158,   159,   163,   326,   329,   330,   343,   345,
     346,   352,    27,    24,   169,   119,   164,     3,     5,     6,
       7,     8,    10,    13,    16,    17,    18,    19,    20,    22,
      26,    29,    36,    50,    64,    77,    83,    90,    93,    95,
     109,   113,   115,   123,   125,   129,   134,   135,   136,   137,
     140,   145,   146,   177,   181,   183,   184,   185,   187,   202,
     228,   272,   277,   281,   282,   284,   285,   286,   287,   315,
     316,   319,   320,   342,   343,   346,   354,   355,   358,   110,
     120,   360,   360,    80,    94,    97,   104,   126,   127,   130,
     131,   349,   350,   351,   353,   114,   120,   144,   113,   160,
     105,   107,   152,   360,   119,   113,   280,   281,    33,    34,
      35,    90,    95,    97,   105,   109,   113,   115,   124,   208,
     233,   235,   237,   238,   240,   244,   324,   325,   329,   339,
     341,   352,    37,   203,   329,   105,   107,   307,   280,    73,
      74,    75,    76,   188,   105,   107,   220,   221,    19,    37,
     186,   235,   237,   325,    14,   307,   285,   113,   343,   109,
     113,   317,   318,   320,   355,   285,   305,   306,   285,   284,
     285,    80,   110,   121,   127,   131,   280,   281,   289,   291,
     292,   322,   336,   338,   348,   349,   351,   356,   357,   103,
     114,   120,   288,   289,   290,   349,   350,   351,   356,   361,
     116,   361,   109,   278,   279,    19,   278,   278,   136,   176,
     164,    19,    72,   210,    81,   120,    82,    85,   121,   274,
     275,   276,   322,   335,   337,   347,   349,   350,    89,   285,
     102,   105,    88,   135,   114,   114,   114,   114,   114,   159,
      79,   161,   162,   163,   154,   154,     4,   170,    23,    81,
     244,   244,   113,   228,   265,   266,   267,   346,    28,   110,
     230,   231,   233,   235,    80,    87,    94,   114,   126,   127,
     130,   131,   230,   248,   331,   332,   361,   361,    85,   252,
      92,    87,   121,   242,   327,   331,   340,    89,   241,   244,
     235,   113,    20,    46,   280,   304,   308,   309,   310,   308,
     119,   283,    78,    78,    78,    78,   226,   231,   245,   219,
     272,   273,   281,   219,    15,   197,   235,   235,    81,   120,
      82,    41,    52,    53,   182,    78,   135,   357,    81,   120,
     225,    87,   305,   345,   354,   335,    79,    85,   120,   110,
     120,   281,   344,   346,   102,   114,   114,   120,   114,   120,
     114,   114,   114,   120,   116,    91,   136,   343,   245,   343,
     343,   121,   178,   321,   333,   334,   350,   357,   210,   135,
     208,   227,   231,   232,   342,   280,   294,   295,   310,   345,
      27,   222,   276,   282,   244,   344,    79,   311,   312,   313,
     343,   344,   346,   285,   114,   114,   120,   106,   359,   360,
      12,   113,   171,   172,   105,   107,   296,   226,   350,    81,
     106,   120,   249,   110,    81,    92,   114,   114,   120,   114,
     114,   116,   253,   254,   255,    27,   214,   235,   231,   329,
     341,   237,   244,    81,   205,   230,   247,   248,   221,   307,
      86,   106,   119,   359,    25,    27,   218,   106,   119,   359,
     280,    82,    81,    82,   206,   231,   256,   113,   325,   230,
     135,    78,   114,   109,   113,   115,   323,   324,   317,    78,
     280,   121,   121,   280,   293,   310,   280,   289,   289,   344,
     289,   289,   136,   110,    78,    78,    81,    81,   346,   355,
     120,    28,   209,   233,   235,    78,   135,    81,    82,   204,
     260,   222,    82,   120,   121,   221,   106,   120,    82,   102,
     163,   113,    21,   144,   163,   173,   174,   175,   106,   119,
     280,   297,   298,   299,   303,   297,   359,   114,   231,   267,
     104,   105,   113,   250,   251,   339,   256,   231,   230,   120,
      87,   339,   105,   107,   213,   121,   121,   256,   114,   120,
     280,   309,   280,   105,   107,   217,   273,   231,   256,   250,
      85,   191,   342,   114,   116,   120,    79,   110,   226,   229,
     229,   121,   121,   333,   249,   204,   260,    92,    78,   256,
      28,   261,   262,   263,    27,   257,     9,   268,   269,   270,
     280,   310,   312,   289,   314,   344,   173,   360,   160,   114,
     120,   144,   297,   106,   119,    85,    87,   300,   301,   302,
     359,   231,   339,   339,   114,   254,   255,     7,    26,   198,
     211,   212,   273,   212,   230,   283,     7,    26,   201,   202,
     215,   216,   273,   216,   192,   341,    27,   194,    81,   310,
     280,    78,   120,    78,    92,   104,   257,   268,   235,   249,
      85,   234,   239,   243,   244,   264,   105,   107,   268,    22,
      51,    54,   113,   189,   271,   328,   329,   270,    82,   102,
     114,   175,   299,   294,   280,   222,   302,    81,   106,    81,
      37,   199,    19,    37,   197,   235,   106,   119,   359,    11,
      19,   200,   200,   106,   119,   359,    87,   105,   107,   195,
     227,   226,   235,   233,   237,   268,   104,   262,    92,   121,
     243,   321,   258,   259,   283,   258,   114,   235,   236,   246,
     271,    55,   190,   289,   344,    87,   222,   256,   256,   235,
     197,   235,    81,    82,   207,   211,   280,   197,   210,   215,
     193,   341,    79,   196,   197,    79,   196,    92,   239,   264,
     239,   106,   119,   318,   359,   120,   114,   235,   280,   106,
     114,   205,    82,   207,   256,   113,   251,   339,   209,   341,
     106,   106,   119,   359,   359,   235,   259,    81,   246,   339,
      85,   204,   260,   197,   226,   192,   257,   268,   268
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
     180,   181,   181,   181,   181,   181,   181,   181,   181,   182,
     182,   182,   183,   184,   184,   184,   184,   184,   185,   186,
     186,   187,   187,   187,   187,   188,   188,   188,   188,   188,
     189,   189,   189,   190,   191,   191,   192,   193,   193,   194,
     194,   195,   195,   195,   195,   196,   196,   196,   196,   197,
     198,   198,   198,   198,   198,   199,   199,   200,   200,   201,
     201,   201,   202,   202,   203,   203,   204,   204,   205,   205,
     206,   206,   206,   207,   207,   207,   208,   208,   209,   209,
     209,   209,   210,   210,   210,   211,   211,   212,   212,   212,
     212,   213,   213,   214,   214,   215,   215,   216,   216,   216,
     216,   217,   217,   218,   218,   219,   219,   219,   219,   220,
     220,   221,   222,   222,   223,   223,   224,   224,   224,   225,
     225,   226,   227,   228,   228,   229,   229,   230,   230,   231,
     231,   231,   232,   233,   234,   235,   235,   236,   237,   238,
     238,   239,   239,   240,   240,   240,   241,   242,   242,   243,
     244,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     245,   246,   246,   247,   247,   248,   248,   249,   249,   250,
     250,   250,   251,   251,   252,   252,   253,   253,   254,   255,
     255,   256,   257,   257,   257,   258,   258,   259,   260,   261,
     261,   262,   262,   263,   263,   264,   264,   265,   265,   266,
     266,   267,   268,   268,   269,   269,   270,   270,   270,   271,
     271,   271,   272,   272,   273,   274,   274,   275,   275,   276,
     277,   277,   277,   277,   277,   277,   277,   277,   277,   278,
     278,   279,   279,   280,   280,   281,   281,   282,   282,   283,
     283,   284,   284,   284,   284,   285,   285,   285,   285,   285,
     285,   285,   285,   285,   285,   286,   286,   286,   287,   287,
     287,   287,   287,   287,   287,   287,   288,   288,   289,   289,
     289,   290,   290,   291,   291,   291,   291,   291,   291,   291,
     292,   292,   293,   293,   294,   295,   295,   296,   296,   296,
     296,   297,   297,   298,   298,   298,   299,   300,   300,   301,
     301,   302,   303,   304,   305,   306,   306,   307,   307,   308,
     308,   308,   308,   309,   309,   310,   310,   310,   311,   311,
     312,   312,   312,   313,   313,   313,   313,   314,   314,   315,
     315,   316,   316,   317,   317,   317,   318,   318,   319,   319,
     319,   319,   320,   320,   321,   321,   322,   322,   323,   323,
     323,   324,   324,   324,   324,   324,   325,   325,   326,   326,
     326,   326,   327,   327,   328,   329,   329,   330,   331,   331,
     331,   332,   332,   332,   332,   333,   333,   334,   334,   335,
     335,   336,   336,   337,   337,   338,   338,   339,   340,   341,
     341,   341,   341,   341,   342,   342,   343,   343,   343,   344,
     345,   345,   346,   346,   346,   346,   346,   346,   346,   346,
     347,   347,   348,   348,   349,   350,   350,   351,   351,   352,
     352,   352,   352,   352,   352,   352,   352,   352,   352,   352,
     352,   352,   352,   352,   352,   352,   352,   353,   353,   354,
     354,   355,   356,   356,   357,   357,   358,   358,   358,   358,
     358,   359,   359,   360,   360,   361,   361
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
       1,     1,     4,     4,     5,     6,     6,     4,     4,     3,
       1,     4,     3,     6,     7,     2,     2,     2,     2,     0,
       1,     1,     1,     2,     0,     2,     3,     2,     1,     0,
       2,     3,     3,     3,     3,     3,     2,     1,     0,     3,
       4,     3,     4,     2,     3,     0,     1,     0,     1,     3,
       6,     7,     1,     1,     0,     1,     0,     2,     0,     2,
       0,     2,     2,     0,     2,     4,     3,     1,     6,     4,
       3,     1,     4,     3,     0,     1,     1,     3,     2,     1,
       0,     3,     3,     2,     0,     1,     1,     3,     2,     1,
       0,     3,     3,     2,     0,     3,     2,     1,     0,     3,
       3,     1,     2,     0,     1,     3,     3,     1,     0,     0,
       2,     1,     1,     3,     1,     1,     3,     1,     3,     4,
       3,     1,     1,     1,     1,     1,     3,     1,     1,     1,
       3,     1,     2,     1,     2,     3,     1,     1,     1,     1,
       1,     1,     1,     2,     2,     3,     2,     5,     3,     3,
       1,     1,     3,     1,     0,     1,     3,     2,     0,     1,
       3,     5,     1,     5,     0,     2,     3,     1,     3,     2,
       0,     1,     4,     4,     0,     3,     1,     4,     2,     3,
       1,     4,     2,     3,     0,     1,     3,     0,     1,     3,
       1,     3,     0,     1,     2,     1,     2,     3,     3,     1,
       2,     3,     1,     2,     1,     3,     2,     2,     1,     4,
       3,     3,     4,     4,     3,     4,     6,     6,     4,     0,
       1,     3,     4,     3,     1,     1,     3,     2,     1,     1,
       0,     2,     3,     2,     1,     3,     2,     2,     4,     4,
       8,     4,     2,     2,     1,     4,     3,     1,     1,     1,
       1,     3,     3,     3,     3,     1,     3,     2,     1,     2,
       2,     3,     3,     1,     1,     2,     4,     3,     5,     3,
       3,     3,     3,     1,     1,     3,     1,     3,     3,     2,
       2,     1,     2,     3,     2,     1,     2,     3,     2,     2,
       1,     4,     1,     1,     1,     2,     1,     3,     3,     3,
       2,     1,     0,     1,     2,     3,     1,     2,     1,     0,
       3,     1,     1,     3,     1,     5,     3,     3,     1,     1,
       1,     1,     3,     1,     3,     1,     3,     1,     2,     3,
       2,     3,     1,     2,     1,     3,     1,     3,     1,     2,
       2,     1,     3,     3,     3,     2,     1,     3,     1,     3,
       3,     3,     1,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     1,
       1,     1,     1,     1,     3,     1,     3,     1,     3,     1,
       1,     1,     1,     1,     1,     3,     1,     3,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     1
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
       0,   514,   514,   531,   532,   534,   538,   539,   540,   542,
     543,   545,   546,   549,   551,   552,   553,   561,   562,   564,
     565,   566,   567,   569,   570,   572,   573,   574,   576,   577,
     578,   580,   581,   583,   584,   586,   587,   591,   592,   594,
     595,   597,   599,   600,   602,   615,   616,   618,   619,   621,
     622,   626,   627,   629,   630,   631,   632,   634,   635,   637,
     638,   643,   644,   646,   647,   648,   650,   651,   655,   657,
     658,   660,   661,   662,   663,   666,   667,   673,   675,   678,
     679,   680,   682,   685,   686,   688,   689,   690,   692,   694,
     695,   698,   699,   700,   706,   713,   714,   715,   716,   717,
     719,   720,   721,   723,   734,   735,   737,   739,   740,   744,
     745,   747,   748,   749,   750,   752,   753,   754,   755,   757,
     760,   762,   764,   766,   767,   769,   769,   771,   771,   775,
     777,   784,   791,   792,   795,   796,   800,   801,   803,   804,
     806,   807,   808,   810,   811,   812,   815,   816,   819,   820,
     821,   822,   824,   825,   826,   868,   869,   871,   872,   873,
     874,   876,   877,   879,   880,   882,   883,   885,   886,   887,
     888,   890,   891,   893,   894,   897,   898,   899,   900,   902,
     903,   905,   907,   908,   916,   917,   919,   920,   921,   934,
     935,   944,   946,   948,   949,   951,   952,   961,   962,   964,
     965,   967,   969,   978,   980,   982,   983,   985,   988,   990,
     991,   993,   994,   996,   998,   999,  1001,  1003,  1004,  1011,
    1018,  1019,  1020,  1021,  1022,  1023,  1024,  1025,  1031,  1032,
    1035,  1037,  1038,  1040,  1041,  1043,  1044,  1051,  1052,  1054,
    1055,  1056,  1059,  1060,  1064,  1065,  1067,  1068,  1071,  1073,
    1074,  1079,  1085,  1086,  1087,  1089,  1090,  1092,  1094,  1096,
    1097,  1099,  1100,  1102,  1103,  1105,  1106,  1112,  1113,  1115,
    1116,  1118,  1120,  1121,  1123,  1124,  1126,  1127,  1128,  1130,
    1131,  1132,  1137,  1139,  1141,  1145,  1146,  1148,  1149,  1153,
    1163,  1164,  1166,  1167,  1168,  1169,  1170,  1171,  1172,  1175,
    1176,  1178,  1179,  1184,  1185,  1189,  1190,  1192,  1193,  1195,
    1196,  1201,  1202,  1203,  1204,  1207,  1208,  1209,  1210,  1211,
    1213,  1215,  1216,  1217,  1219,  1222,  1223,  1224,  1227,  1228,
    1229,  1230,  1231,  1232,  1237,  1238,  1241,  1242,  1247,  1248,
    1249,  1254,  1255,  1273,  1274,  1275,  1276,  1277,  1278,  1279,
    1281,  1282,  1295,  1297,  1307,  1309,  1310,  1313,  1314,  1315,
    1316,  1318,  1319,  1321,  1322,  1323,  1325,  1327,  1328,  1330,
    1331,  1340,  1342,  1344,  1346,  1348,  1349,  1352,  1353,  1355,
    1356,  1357,  1358,  1363,  1364,  1366,  1367,  1368,  1373,  1374,
    1376,  1377,  1378,  1380,  1381,  1382,  1383,  1386,  1387,  1419,
    1420,  1422,  1423,  1425,  1426,  1427,  1429,  1430,  1432,  1433,
    1434,  1435,  1437,  1438,  1440,  1441,  1443,  1444,  1447,  1448,
    1449,  1451,  1452,  1453,  1454,  1455,  1457,  1458,  1460,  1461,
    1462,  1463,  1466,  1467,  1469,  1471,  1472,  1476,  1478,  1479,
    1480,  1482,  1483,  1484,  1485,  1490,  1491,  1493,  1494,  1496,
    1497,  1500,  1501,  1506,  1507,  1509,  1510,  1514,  1516,  1518,
    1519,  1520,  1521,  1522,  1525,  1526,  1528,  1529,  1530,  1532,
    1534,  1535,  1537,  1538,  1539,  1540,  1541,  1542,  1543,  1544,
    1546,  1547,  1549,  1550,  1552,  1554,  1555,  1557,  1558,  1560,
    1561,  1562,  1563,  1564,  1565,  1566,  1567,  1568,  1569,  1570,
    1571,  1572,  1573,  1574,  1575,  1576,  1577,  1579,  1580,  1584,
    1585,  1587,  1589,  1590,  1592,  1593,  1597,  1598,  1599,  1600,
    1601,  1606,  1609,  1613,  1614,  1616,  1617
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
#line 7561 "parser.cc"

#line 1626 "parser.y"


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
